#include <config.h>

#include <string.h>
#include <strings.h>
#include <stdint.h>
#include <defer.h>
#include <tclstuff.h>
#include <tip445.h>
#include <tdom.h>
#include <obstack.h>
#include <obstack_pool.h>

#define NS "::" PACKAGE_NAME

#define SET_UINT_ATTR(node, attr, value) \
	do { \
		char		tmp[21];	\
		const int	len = u64toa((value), (tmp)); \
		tmp[len] = 0; \
		domSetAttributeEx((node), (attr), sizeof(attr)-1, tmp, len); \
	} while(0)

#define SET_CONST_ATTR(node, attr, value) \
	domSetAttributeEx(node, (attr), sizeof((attr))-1, (value), sizeof((value))-1)

#define EMIT(type, parent, from, length) \
	do { \
		if (full && (length)>0) { \
			domNode*	node = domNewElementNode(doc, (type)); \
			domAppendNewTextNode(node, (char*)(from), (length), TEXT_NODE, 0); \
			SET_UINT_ATTR(node, "idx", (from)-text+ofs); \
			SET_UINT_ATTR(node, "len", (length)); \
			domAppendChild((parent), node); \
		} \
	} while(0)

#define SET_VALUE_ATTRIB(node, value) \
	do { \
		const char*	valuestr = Tcl_DStringValue(value); \
		const Tcl_Size	valuelen = Tcl_DStringLength(value); \
		domSetAttributeEx(node, "value", sizeof("value")-1, valuestr, valuelen); \
	} while(0)

// Per-interp / per-thread local data <<<
#define LITSTRS \
	X( LIT_CMD_PARSERS,		NS "::cmd_parsers"	) \
	X( LIT_APPLY,			"apply"				) \
	X( LIT_DOMNODE,			"domNode"			) \
	X( LIT_ASXML,			"asXML"				) \
	/* line intentionally left blank */
enum {
#define X(k, v) k,
	LITSTRS
#undef X
	LIT_END
};
static const char* lit_strings[LIT_END] = {
#define X(k, v) v,
	LITSTRS
#undef X
};

struct pidata {
	Tcl_Obj*		lit[LIT_END];
};

Tcl_ThreadDataKey tsd_ptdata;
struct ptdata {
	bool			initialized;
	uintptr_t		next_cookie;	// Per-thread monotonic; 0 reserved for "invalid"
	Tcl_HashTable	scriptdoms;		// cookie -> scriptdom Tcl_Obj*
};

static void thread_exit_ptdata(ClientData cd) //<<<
{
	struct ptdata*	td = cd;
	if (td->initialized) {
		// By the time we run, all interps in this thread have been deleted and
		// their Tcl_Objs released — which means free_parsetree has already
		// removed every entry it added.  The hash should be empty; just release
		// the bucket storage.
		Tcl_DeleteHashTable(&td->scriptdoms);
		td->initialized = false;
	}
}

//>>>
static struct ptdata* get_ptdata() //<<<
{
	struct ptdata* td = Tcl_GetThreadData(&tsd_ptdata, sizeof(*td));
	if (!td->initialized) {
		Tcl_InitHashTable(&td->scriptdoms, TCL_ONE_WORD_KEYS);
		td->next_cookie = 1;
		td->initialized = true;
		Tcl_CreateThreadExitHandler(thread_exit_ptdata, td);
	}

	return td;
}

//>>>
//>>>

// Types for line index <<<
struct encskip {
	uint32_t	bytestart;	// From the start of the buf
	uint32_t	adj;		// cumulative adjustment for bytes -> chars from the start of the line
};
struct line {
	uint32_t		bytestart;
	uint32_t		lineadjs;	// Number of elements in skips[]
	struct encskip	skips[];
};
struct lineidx {
	uint32_t		bytestart;
	struct line*	line;
};
//>>>

// Prototypes <<<
static int subparse_script(
		Tcl_Interp*				interp,
		struct pidata*			l,
		domNode*				parent,
		const char* restrict	text,
		const int				textlen,
		const int				ofs,
		struct lineidx*			lineindex,
		uint32_t				lines,
		const int				incmdsubst,
		const char** restrict	end);
int u64toa(uint64_t value, char* restrict dst);
static void byte2line(struct lineidx*const lineindex, uint32_t lines, const uint32_t byteofs, uint32_t* linePtr, uint32_t* cPtr);
#ifdef USE_TDOM_STUBS
// tdom 0.9.6.1's public headers no longer expose this — declared in
// tdomStubLib.c only.  Provide the prototype so we can link against
// libtdomstub.a.
extern const char* Tdom_InitStubs(Tcl_Interp* interp, char* version, int exact);
#endif
// Prototypes >>>

static void free_parsetree(Tcl_Obj* obj);
static void free_ast(Tcl_Obj* obj);
static void dup_parsetree(Tcl_Obj* src, Tcl_Obj* dest);
static void dup_ast(Tcl_Obj* src, Tcl_Obj* dest);
static void update_string_rep_parsetree(Tcl_Obj* obj);
static void free_lineidx(Tcl_Obj* obj);
static void dup_lineidx(Tcl_Obj* src, Tcl_Obj* dest);
static void update_string_rep_lineidx(Tcl_Obj* obj);
static void free_noderef(Tcl_Obj* obj);
static void dup_noderef(Tcl_Obj* src, Tcl_Obj* dst);
static void update_string_rep_noderef(Tcl_Obj* obj);

Tcl_ObjType parsetree = {
	.name				= "parsetcl::parsetree",
	.freeIntRepProc		= free_parsetree,
	.dupIntRepProc		= dup_parsetree,
	.updateStringProc	= update_string_rep_parsetree,
};

Tcl_ObjType astObjtype = {
	.name				= "parsetcl::ast",
	.freeIntRepProc		= free_ast,
	.dupIntRepProc		= dup_ast,
	.updateStringProc	= update_string_rep_parsetree,
};

Tcl_ObjType lineidxtype = {
	.name				= "parsetcl::lineidx",
	.freeIntRepProc		= free_lineidx,
	.dupIntRepProc		= dup_lineidx,
	.updateStringProc	= update_string_rep_lineidx,
};

Tcl_ObjType nodereftype = {
	.name				= "parsetcl::noderef",
	.freeIntRepProc		= free_noderef,
	.dupIntRepProc		= dup_noderef,
	.updateStringProc	= update_string_rep_noderef,
};

struct lineidx_intrep {
	int					refcount;
	struct obstack*		ob;
	struct obstack*		linestarts;
	uint32_t			lines;
	struct lineidx		lineidx[];
};

// twoPtrValue.ptr1 = domDocument*, twoPtrValue.ptr2 = (void*)(uintptr_t)cookie
// The cookie keys this scriptdom in the per-thread scriptdoms hash so that
// noderefs serialized to strings can be safely revived without leaking heap
// pointers into the script and without risking Tcl_Obj-slot reuse aliasing.

static void free_parsetree_intrep(Tcl_Obj* obj, const Tcl_ObjType* type) //<<<
{
	Tcl_ObjInternalRep*	ir = Tcl_FetchInternalRep(obj, type);
	if (!ir) return;

	const uintptr_t	cookie = (uintptr_t)ir->twoPtrValue.ptr2;
	if (cookie) {
		struct ptdata*	td = get_ptdata();
		Tcl_HashEntry*	he = Tcl_FindHashEntry(&td->scriptdoms, (void*)cookie);
		if (he) Tcl_DeleteHashEntry(he);
	}

	if (ir->twoPtrValue.ptr1) {
		domFreeDocument((domDocument*)ir->twoPtrValue.ptr1, NULL, NULL);
		ir->twoPtrValue.ptr1 = NULL;
	}
}

//>>>
static void free_parsetree(Tcl_Obj* obj) { free_parsetree_intrep(obj, &parsetree); }
static void free_ast      (Tcl_Obj* obj) { free_parsetree_intrep(obj, &astObjtype); }

//>>>
static void dup_parsetree_intrep(Tcl_Obj* src, Tcl_Obj* dest, const Tcl_ObjType* type) //<<<
{
	Tcl_ObjInternalRep*	srcir = Tcl_FetchInternalRep(src, type);
	if (!srcir) return;

	domDocument*	srcdoc = srcir->twoPtrValue.ptr1;
	domDocument*	destdoc = domCreateDoc(NULL, 0);
	domNode*		destroot = domCloneNode(srcdoc->documentElement, 1);

	destdoc->rootNode->firstChild = destdoc->rootNode->lastChild = destroot;
	domSetDocumentElement(destdoc);

	// The dup is a fresh scriptdom: assign a new cookie and register it.
	struct ptdata*	td = get_ptdata();
	const uintptr_t	cookie = td->next_cookie++;
	int				isnew;
	Tcl_HashEntry*	he = Tcl_CreateHashEntry(&td->scriptdoms, (void*)cookie, &isnew);
	Tcl_SetHashValue(he, dest);

	Tcl_StoreInternalRep(dest, type, &(Tcl_ObjInternalRep){
		.twoPtrValue.ptr1 = destdoc,
		.twoPtrValue.ptr2 = (void*)cookie,
	});
}

//>>>
static void dup_parsetree(Tcl_Obj* src, Tcl_Obj* dest) { dup_parsetree_intrep(src, dest, &parsetree); }
static void dup_ast      (Tcl_Obj* src, Tcl_Obj* dest) { dup_parsetree_intrep(src, dest, &astObjtype); }

//>>>
static void update_string_rep_parsetree(Tcl_Obj* obj) //<<<
{
	Tcl_ObjInternalRep*	ir = Tcl_FetchInternalRep(obj, &parsetree);
	if (!ir) ir = Tcl_FetchInternalRep(obj, &astObjtype);
	domNode*			node = ((domDocument*)ir->twoPtrValue.ptr1)->documentElement;

	// To make an XML Tcl_ObjType (which would be useful), serialize to XML here
	Tcl_DString	res;
	Tcl_DStringInit(&res);
	defer { Tcl_DStringFree(&res); }

	domNode* child = node->firstChild;
	while (child) {
		if (
				child->nodeType == TEXT_NODE ||
				child->nodeType == CDATA_SECTION_NODE
		) {
			Tcl_DStringAppend(&res,
					((domTextNode*)child)->nodeValue,
					((domTextNode*)child)->valueLength);
		}
		child = child->nextSibling;
	}

	// TODO: Are we sure this char* is valid UTF-8?
	Tcl_InitStringRep(obj, Tcl_DStringValue(&res), Tcl_DStringLength(&res));
}

//>>>
static void free_lineidx(Tcl_Obj* obj) //<<<
{
	Tcl_ObjInternalRep*	ir = Tcl_FetchInternalRep(obj, &lineidxtype);

	struct lineidx_intrep*	l = ir->twoPtrValue.ptr1;

	if (--l->refcount <= 0) {
		if (l->ob) {
			obstack_pool_release(l->ob);
			l->ob = NULL;
		}

		if (l->linestarts) {
			obstack_pool_release(l->linestarts);
			l->linestarts = NULL;
		}

		ckfree((char*)l);
	}
}

//>>>
static void dup_lineidx(Tcl_Obj* src, Tcl_Obj* dest) //<<<
{
	Tcl_ObjInternalRep*		ir = Tcl_FetchInternalRep(src, &lineidxtype);
	struct lineidx_intrep*	l = ir->twoPtrValue.ptr1;

	l->refcount++;

	Tcl_StoreInternalRep(dest, &lineidxtype, ir);
}

//>>>
static void update_string_rep_lineidx(Tcl_Obj* obj) //<<<
{
	char					tmp[21];		// Max decimal digits in 2**64: 20 +1 for \0
	Tcl_ObjInternalRep*		ir = Tcl_FetchInternalRep(obj, &lineidxtype);
	struct lineidx_intrep*	l = ir->twoPtrValue.ptr1;

	Tcl_DString	ds;
	Tcl_DStringInit(&ds);
	defer { Tcl_DStringFree(&ds); }

	for (uint32_t i=0; i<l->lines; i++) {
		const struct lineidx*const	lineidx = &l->lineidx[i];
		const struct line*const		line = lineidx->line;
		tmp[u64toa(line->bytestart, tmp)] = 0;
		Tcl_DStringStartSublist(&ds);
		Tcl_DStringAppendElement(&ds, tmp);
		Tcl_DStringStartSublist(&ds);
		for (uint32_t j=0; j<line->lineadjs; j++) {
			const struct encskip*const	adj = &line->skips[j];
			tmp[u64toa(adj->bytestart, tmp)] = 0; Tcl_DStringAppendElement(&ds, tmp);
			tmp[u64toa(adj->adj, tmp)] = 0;       Tcl_DStringAppendElement(&ds, tmp);
		}
		Tcl_DStringEndSublist(&ds);
		Tcl_DStringEndSublist(&ds);
	}

	Tcl_InitStringRep(obj, Tcl_DStringValue(&ds), Tcl_DStringLength(&ds));
}

//>>>

static int GetLineIdxFromObj(Tcl_Interp* interp, Tcl_Obj* scriptObj, struct lineidx** lineindex, uint32_t* lines) //<<<
{
	Tcl_ObjInternalRep*	ir = Tcl_FetchInternalRep(scriptObj, &lineidxtype);
	if (!ir) THROW_ERROR("Expected lineidx internal representation");

	struct lineidx_intrep*	l = ir->twoPtrValue.ptr1;
	*lineindex	= l->lineidx;
	*lines		= l->lines;

	return TCL_OK;
}

//>>>
static Tcl_Obj* NewLineIdxObj(Tcl_Obj* scriptObj) //<<<
{
	struct obstack* ob			= obstack_pool_get(OBSTACK_POOL_SMALL);
	struct obstack* linestarts	= obstack_pool_get(OBSTACK_POOL_SMALL);
	obstack_blank(ob, sizeof(struct line));

	Tcl_Size		scriptlen;
	const char*		script = Tcl_GetStringFromObj(scriptObj, &scriptlen);
	const uint8_t*	start = (const uint8_t*)script;
	const uint8_t*	p = start;
	uint32_t		adj = 0;
	uint32_t		linestart = 0;
	uint32_t		lines = 0;
	uint32_t		lineadjs = 0;

nextchar:
	{
		const uint8_t	c = *p;

		switch (c) {
			case '\n':
				{
					// glibc obstack.h's __PTR_ALIGN deliberately does (P) - (char*)0
					// when sizeof(PTR_INT_TYPE) >= sizeof(void*) — UB-by-strict-C
					// but the fast-path glibc has shipped for decades.
					[[clang::suppress]]
					struct line*const	line_final = obstack_finish(ob);
					line_final->bytestart	= linestart;
					line_final->lineadjs	= lineadjs;
					struct lineidx idx = {linestart, line_final};
					obstack_grow(linestarts, &idx, sizeof(struct lineidx));
					lines++;
					lineadjs = 0;
					adj = 0;
					p++;
					linestart = p-start;
					obstack_blank(ob, sizeof(struct line));
					goto nextchar;
				}

			case 0x00:
			case 0x05:	// EOF
				if (p > start + linestart) {
					[[clang::suppress]]
					struct line*const	line_final = obstack_finish(ob);
					line_final->bytestart	= linestart;
					line_final->lineadjs	= lineadjs;
					struct lineidx idx = {linestart, line_final};
					obstack_grow(linestarts, &idx, sizeof(struct lineidx));
					lines++;
				}
				goto eof;

			default:
				if (c >= 0x80) {
					const uint8_t	enclen = __builtin_clz(~(c<<((sizeof(int)-1)*8)));
					adj += enclen - 1;
					p += enclen;
					struct encskip skip = {p-start, adj};
					obstack_grow(ob, &skip, sizeof(struct encskip));
					lineadjs++;
				} else {
					p++;
				}
				goto nextchar;
		}
	}
eof:
	{
		[[clang::suppress]]
		struct lineidx*const	lineindex = obstack_finish(linestarts);

		// Create the Tcl_Obj with lineidx internal rep
		struct lineidx_intrep*	l = (struct lineidx_intrep*)ckalloc(sizeof(struct lineidx_intrep) + lines * sizeof(struct lineidx));

		*l = (struct lineidx_intrep){
			.refcount	= 1,
			.ob			= ob,
			.linestarts	= linestarts,
			.lines		= lines,
		};
		memcpy(l->lineidx, lineindex, lines * sizeof(struct lineidx));

		Tcl_Obj*	obj = Tcl_NewObj();
		Tcl_StoreInternalRep(obj, &lineidxtype, &(Tcl_ObjInternalRep){.twoPtrValue.ptr1 = l});
		return obj;
	}
}

//>>>

// Noderef objtype <<<
struct noderef {
	Tcl_Obj*	scriptdom;		// Keeps the doc alive (via parsetree intrep refcount)
	Tcl_Obj*	tdomnodeobj;	// tdom command name string ("domNodeNN")
	domNode*	node;
	uintptr_t	cookie;			// Looks up scriptdom in scriptdoms hash on revive
};

static void free_noderef(Tcl_Obj* obj) //<<<
{
	Tcl_ObjInternalRep*	ir = Tcl_FetchInternalRep(obj, &nodereftype);
	struct noderef*		ref = ir->otherValuePtr;

	replace_tclobj(&ref->scriptdom,		NULL);
	replace_tclobj(&ref->tdomnodeobj,	NULL);
	ref->node = NULL;
	ckfree(ref);
}

//>>>
static void dup_noderef(Tcl_Obj* src, Tcl_Obj* dst) //<<<
{
	Tcl_ObjInternalRep*	ir = Tcl_FetchInternalRep(src, &nodereftype);
	struct noderef*		srcIr = ir->otherValuePtr;
	struct noderef*		dstIr = ckalloc(sizeof *dstIr);

	*dstIr = (struct noderef){ .node = srcIr->node, .cookie = srcIr->cookie };
	replace_tclobj(&dstIr->scriptdom,	srcIr->scriptdom);
	replace_tclobj(&dstIr->tdomnodeobj,	srcIr->tdomnodeobj);

	Tcl_StoreInternalRep(dst, &nodereftype, &(Tcl_ObjInternalRep){.otherValuePtr = dstIr});
	Tcl_InvalidateStringRep(dst);
}

//>>>
static void update_string_rep_noderef(Tcl_Obj* obj) //<<<
{
	Tcl_ObjInternalRep*	ir = Tcl_FetchInternalRep(obj, &nodereftype);
	struct noderef*		ref = ir->otherValuePtr;

	Tcl_DString	ds;
	Tcl_DStringInit(&ds);
	defer { Tcl_DStringFree(&ds); }

	char		tmp[21];		// Max decimal digits in 2**64: 20 +1 for \0
	const int	len = u64toa(ref->cookie, tmp);
	tmp[len] = 0;

	Tcl_DStringAppendElement(&ds, "parsetcl_noderef");
	Tcl_DStringAppendElement(&ds, Tcl_GetString(ref->tdomnodeobj));
	Tcl_DStringAppendElement(&ds, tmp);

	Tcl_InitStringRep(obj, Tcl_DStringValue(&ds), Tcl_DStringLength(&ds));
}

//>>>
static int GetNoderefFromObj(Tcl_Interp* interp, Tcl_Obj* obj, struct noderef** ref) //<<<
{
	Tcl_ObjInternalRep*	ir = Tcl_FetchInternalRep(obj, &nodereftype);
	if (!ir) {
		Tcl_Obj**	ov;
		Tcl_Size	oc;
		TEST_OK(Tcl_ListObjGetElements(interp, obj, &oc, &ov));
		if (
			oc != 3 ||
			strcmp("parsetcl_noderef", Tcl_GetString(ov[0]))
		) THROW_ERROR("Not a node ref");

		Tcl_WideInt	cookie_wide;
		TEST_OK(Tcl_GetWideIntFromObj(interp, ov[2], &cookie_wide));
		if (cookie_wide <= 0) THROW_ERROR("The script dom the node refers to does not exist in this thread");
		const uintptr_t	cookie = (uintptr_t)cookie_wide;

		struct ptdata*	td = get_ptdata();
		Tcl_HashEntry*	he = Tcl_FindHashEntry(&td->scriptdoms, (void*)cookie);
		if (!he) THROW_ERROR("The script dom the node refers to does not exist in this thread");
		Tcl_Obj*	scriptdom = Tcl_GetHashValue(he);

		char*		errmsg = NULL;
		domNode*	node = tcldom_getNodeFromName(interp, Tcl_GetString(ov[1]), &errmsg);
		if (!node) {
			Tcl_SetObjResult(interp, errmsg ?
					Tcl_ObjPrintf("tDOM error getting node: \"%s\"", errmsg) :
					Tcl_ObjPrintf("Could not retrieve node from \"%s\"", Tcl_GetString(ov[1])));
			return TCL_ERROR;
		}

		struct noderef* ref = ckalloc(sizeof *ref);
		*ref = (struct noderef){ .node = node, .cookie = cookie };
		replace_tclobj(&ref->scriptdom,		scriptdom);
		replace_tclobj(&ref->tdomnodeobj,	ov[1]);

		Tcl_StoreInternalRep(obj, &nodereftype, &(Tcl_ObjInternalRep){.otherValuePtr = ref});
		ir = Tcl_FetchInternalRep(obj, &nodereftype);
	}

	*ref = ir->otherValuePtr;

	return TCL_OK;
}

//>>>
static Tcl_Obj* NewNoderefObj(Tcl_Interp* interp, domNode* node, Tcl_Obj* scriptdom, uintptr_t cookie) //<<<
{
	char	nodecmd[80];
	tcldom_createNodeObj(interp, node, nodecmd);

	struct noderef*	ref = ckalloc(sizeof(*ref));
	*ref = (struct noderef){ .node = node, .cookie = cookie };
	replace_tclobj(&ref->scriptdom,		scriptdom);
	replace_tclobj(&ref->tdomnodeobj,	Tcl_NewStringObj(nodecmd, -1));

	Tcl_Obj*	res = Tcl_NewObj();
	Tcl_StoreInternalRep(res, &nodereftype, &(Tcl_ObjInternalRep){.otherValuePtr = ref});
	Tcl_InvalidateStringRep(res);
	return res;
}

//>>>
// Noderef objtype >>>

const char* toktype_string(int type) //<<<
{
	switch (type) {
		case TCL_TOKEN_WORD:		return "TCL_TOKEN_WORD";
		case TCL_TOKEN_SIMPLE_WORD:	return "TCL_TOKEN_SIMPLE_WORD";
		case TCL_TOKEN_EXPAND_WORD:	return "TCL_TOKEN_EXPAND_WORD";
		case TCL_TOKEN_TEXT:		return "TCL_TOKEN_TEXT";
		case TCL_TOKEN_BS:			return "TCL_TOKEN_BS";
		case TCL_TOKEN_COMMAND:		return "TCL_TOKEN_COMMAND";
		case TCL_TOKEN_VARIABLE:	return "TCL_TOKEN_VARIABLE";
		case TCL_TOKEN_SUB_EXPR:	return "TCL_TOKEN_SUB_EXPR";
		case TCL_TOKEN_OPERATOR:	return "TCL_TOKEN_OPERATOR";
		default:					return "Unknown";
	}
}

//>>>
static int get_attr(Tcl_Interp* interp, domNode* node, const char* attr, const char** value) //<<<
{
	domAttrNode*	attrnode = node->firstAttr;

	while (attrnode) {
		if (strcmp(attrnode->nodeName, attr) == 0) {
			*value = attrnode->nodeValue;
			return TCL_OK;
		}
		attrnode = attrnode->nextSibling;
	}

	if (interp)
		Tcl_SetObjResult(interp, Tcl_ObjPrintf("No such attribute \"%s\"", attr));

	return TCL_ERROR;
}

//>>>
// Fast unsigned int to string conversion from the talk by Alexandrescu: "Three Optimization Tips for C++" <<<
uint32_t digits10(uint64_t v) //<<<
{
#define P01	10
#define P02	100
#define P03	1000
#define P04	10000
#define P05	100000
#define P06	1000000
#define P07	10000000
#define P08	100000000
#define P09	1000000000
#define P10	10000000000
#define P11	100000000000
#define P12	1000000000000
	if (v < P01) return 1;
	if (v < P02) return 2;
	if (v < P03) return 3;
	if (v < P12) {
		if (v < P08) {
			if (v < P06) {
				if (v < P04) return 4;
				return 5 + (v >= P05);
			}
			return 7 + (v >= P07);
		}
		if (v < P10) {
			return 9 + (v >= P09);
		}
		return 11 + (v >= P11);
	}
	return 12 + digits10(v / P12);
}

//>>>
int u64toa(uint64_t value, char* restrict dst) //<<<
{
	// TODO: benchmark this against TclFormatInt and replace the latter with this if it's faster
	static const char digits[201] =
		"0001020304050607080910111213141516171819"
		"2021222324252627282930313233343536373839"
		"4041424344454647484950515253545556575859"
		"6061626364656667686970717273747576777879"
		"8081828384858687888990919293949596979899";
	const uint32_t length = digits10(value);
	uint32_t next = length-1;

	while (value >= 100) {
		const int i = (value % 100) * 2;
		value /= 100;
		memcpy(dst+next-1, digits+i, 2);
		//dst[next] = digits[i+1];
		//dst[next-1] = digits[i];
		next -= 2;
	}
	if (value < 10) {
		dst[next] = '0' + (uint32_t)value;
	} else {
		const int i = (uint32_t)value * 2;
		memcpy(dst+next-1, digits+i, 2);
		//dst[next] = digits[i + 1];
		//dst[next-1] = digits[i];
	}

	return length;
}

//>>>
//>>>
static int append_sub_tokens( //<<<
		Tcl_Interp*			interp,
		struct pidata*		l,
		domNode*			parent,
		const char*restrict	text,
		const Tcl_Token*	subtokens,
		int					numComponents,
		int					ofs,
		bool*				dynamic,
		Tcl_DString*		value,
		int					raw,
		struct lineidx*		lineindex,
		uint32_t			lines,
		const int			full)
{
	domDocument*	doc = parent->ownerDocument;

	for (int t=0; t<numComponents; t++) {
		/*
		fprintf(stderr, "\tsubtoken %d: type: %s(%d), start: %ld, length %d, numComponents: %d\n",
				t, toktype_string(subtokens[t].type), subtokens[t].type, subtokens[t].start-text,
				subtokens[t].size, subtokens[t].numComponents);
		*/

		int	expand = 0;
		switch (subtokens[t].type) {
			case TCL_TOKEN_EXPAND_WORD: expand = 1; // Falls through
			case TCL_TOKEN_WORD:
			case TCL_TOKEN_SIMPLE_WORD: //<<<
				{
					Tcl_DString	value;				// Shadows argument!
					int			raw = 0;			// Shadows argument!
					bool		dynamic = false;	// Shadows argument!
					const int	syntax_len = subtokens[t+1].start - subtokens[t].start;
					const char	c = subtokens[t].start[expand*3];
					domNode*	wordnode = NULL;

					wordnode = domNewElementNode(doc, "word");
					if (full) {
						SET_UINT_ATTR(wordnode, "idx", subtokens[t].start -text +ofs);
						SET_UINT_ATTR(wordnode, "len", subtokens[t].size);
					}
					domAppendChild(parent, wordnode);

					if (syntax_len) {
						//EMIT("syntax", wordnode, token->start, syntax_len);
						switch (c) {
							case '"': SET_CONST_ATTR(wordnode, "quoted", "quote"); break;
							case '{': SET_CONST_ATTR(wordnode, "quoted", "brace"); raw = 1; break;
						}
					} else {
						//SET_CONST_ATTR(wordnode, "quoted", "none");
					}

					Tcl_DStringInit(&value);
					if (subtokens[t].numComponents) {
						TEST_OK(append_sub_tokens(
								interp,
								l,
								wordnode,
								text,
								subtokens+t+1,
								subtokens[t].numComponents,
								ofs,
								&dynamic,
								&value,
								raw,
								lineindex,
								lines,
								full));

						t += 1 + subtokens[t].numComponents;
					}

					if (!dynamic) 
						SET_VALUE_ATTRIB(wordnode, &value);

					Tcl_DStringFree(&value);
				}
				break;
				//>>>
			case TCL_TOKEN_TEXT: //<<<
				{
					domNode* toknode = domNewElementNode(doc, "text");
					if (full) {
						SET_UINT_ATTR(toknode, "idx", subtokens[t].start-text+ofs);
						SET_UINT_ATTR(toknode, "len", subtokens[t].size);
					}
					domAppendNewTextNode(toknode, (char*)subtokens[t].start, subtokens[t].size, TEXT_NODE, 0);
					domAppendChild(parent, toknode);
					if (!*dynamic)
						Tcl_DStringAppend(value, subtokens[t].start, subtokens[t].size);
				}
				break;
				//>>>
			case TCL_TOKEN_BS: //<<<
				{
					domNode* toknode = domNewElementNode(doc, "escape");
					if (full) {
						/*
						char idxstr[22];
						u64toa(subtokens[t].start-text+ofs, idxstr);
						fprintf(stderr, "idx: %ld, str: (%s)\n", subtokens[t].start-text+ofs, idxstr);
						*/
						SET_UINT_ATTR(toknode, "idx", subtokens[t].start-text+ofs);
						SET_UINT_ATTR(toknode, "len", subtokens[t].size);
					}
					//if (subtokens[t].size > 1 && subtokens[t].start[1] == '\n') { // Line folding
					//	fprintf(stderr, "Line folding case, adjusting by %d\n", subtokens[t].size-1);
					//	ofs -= subtokens[t].size-1;
					//}
					domAppendNewTextNode(toknode, (char*)subtokens[t].start, subtokens[t].size, TEXT_NODE, 0);
					domAppendChild(parent, toknode);
					if (!*dynamic) {
						if (raw) {
							Tcl_DStringAppend(value, subtokens[t].start, subtokens[t].size);
						} else {
							Tcl_Obj*	rawval	= NULL;
							Tcl_Obj*	escape	= NULL;
							defer {
								replace_tclobj(&rawval,	NULL);
								replace_tclobj(&escape,	NULL);
							}

							replace_tclobj(&rawval, Tcl_NewStringObj(subtokens[t].start, subtokens[t].size));
							replace_tclobj(&escape, Tcl_SubstObj(interp, rawval, TCL_SUBST_BACKSLASHES));
							if (!escape) return TCL_ERROR;

							Tcl_Size	len;
							const char*	bytes = Tcl_GetStringFromObj(escape, &len);
							Tcl_DStringAppend(value, bytes, len);
						}
					}
				}
				break;
				//>>>
			case TCL_TOKEN_VARIABLE: //<<<
				{
					*dynamic = true;
					domNode* toknode = domNewElementNode(doc, "var");
					if (subtokens[t].numComponents == 1) {
						SET_CONST_ATTR(toknode, "type", "scalar");
					} else {
						SET_CONST_ATTR(toknode, "type", "array");
					}
					if (full) {
						SET_UINT_ATTR(toknode, "idx", subtokens[t].start-text+ofs);
						SET_UINT_ATTR(toknode, "len", subtokens[t].size);
					}
					//domAppendNewTextNode(toknode, subtokens[t].start, subtokens[t].size, TEXT_NODE, 0);
					domAppendChild(parent, toknode);

					if (subtokens[t].numComponents) {
						TEST_OK(append_sub_tokens(
								interp,
								l,
								toknode,
								text,
								subtokens+t+1,
								subtokens[t].numComponents,
								ofs,
								dynamic,
								value,
								raw,
								lineindex,
								lines,
								full));

						domSetAttributeEx(toknode, "name", sizeof("name")-1, subtokens[t+1].start, subtokens[t+1].size);
						// TODO: if this is an array and the index tokens are static literals, store that in the "index" attribute
						t += subtokens[t].numComponents;
					}
				}
				break;
				//>>>
			case TCL_TOKEN_COMMAND: //<<<
				{
					if (full) {
						domNode*	node = domNewElementNode(doc, "syntax");
						domAppendNewTextNode(node, (char*)subtokens[t].start, 1, TEXT_NODE, 0);
						SET_UINT_ATTR(node, "idx", subtokens[t].start-text+ofs);
						SET_UINT_ATTR(node, "len", 1);
						domAppendChild(parent, node);
					}

					*dynamic = true;
					TEST_OK(subparse_script(
							interp,
							l,
							parent,
							subtokens[t].start+1,
							subtokens[t].size-1,
							subtokens[t].start+1 - text + ofs,
							lineindex,
							lines,
							1,
							NULL));
				}
				break;
				//>>>
			case TCL_TOKEN_SUB_EXPR: //<<<
				{
					Tcl_DString	value;				// Shadows argument!
					bool		dynamic = false;	// Shadows argument!

					domNode* toknode = domNewElementNode(doc, "subexpr");
					if (full) {
						SET_UINT_ATTR(toknode, "idx", subtokens[t].start-text+ofs);
						SET_UINT_ATTR(toknode, "len", subtokens[t].size);
					}
					//domAppendNewTextNode(toknode, subtokens[t].start, subtokens[t].size, TEXT_NODE, 0);
					domSetAttributeEx(toknode, "orig", sizeof("orig")-1, subtokens[t].start, subtokens[t].size);
					/*
					{
						Tcl_Obj* tmp = NULL;

						replace_tclobj(&tmp, Tcl_NewStringObj(subtokens[t].start, subtokens[t].size));
						fprintf(stderr, "subexpr: ->%s<-\n", Tcl_GetString(tmp));
						replace_tclobj(&tmp, NULL);
					}
					*/
					if (subtokens[t].size) {
						switch (subtokens[t].start[0]) {
							case '"': SET_CONST_ATTR(toknode, "quoted", "quote"); break;
							case '{': SET_CONST_ATTR(toknode, "quoted", "brace"); break;
						}
					}
					domAppendChild(parent, toknode);

					Tcl_DStringInit(&value);
					if (subtokens[t].numComponents) {
						TEST_OK(append_sub_tokens(
								interp,
								l,
								toknode,
								text,
								subtokens+t+1,
								subtokens[t].numComponents,
								ofs,
								&dynamic,
								&value,
								0,
								lineindex,
								lines,
								full));

						t += subtokens[t].numComponents;
					}
					if (!dynamic)
						SET_VALUE_ATTRIB(toknode, &value);

					Tcl_DStringFree(&value);
				}
				break;
				//>>>
			case TCL_TOKEN_OPERATOR: //<<<
				{
					*dynamic = true;
					domNode* toknode = domNewElementNode(doc, "operator");
					if (full) {
						SET_UINT_ATTR(toknode, "idx", subtokens[t].start-text+ofs);
						SET_UINT_ATTR(toknode, "len", subtokens[t].size);
					}
					domSetAttributeEx(toknode, "name", sizeof("name")-1, subtokens[t].start, subtokens[t].size);
					//domAppendNewTextNode(toknode, subtokens[t].start, subtokens[t].size, TEXT_NODE, 0);
					domAppendChild(parent, toknode);

					if (t < numComponents-1) {
						TEST_OK(append_sub_tokens(
								interp,
								l,
								toknode,
								text,
								subtokens+t+1,
								numComponents-t-1,
								ofs,
								dynamic,
								value,
								raw,
								lineindex,
								lines,
								full));
					}
				}
				return TCL_OK;
				//>>>
			default: THROW_PRINTF("Unexpected token type: %d", subtokens[t].type);
		}
	}

	return TCL_OK;
}

//>>>
static int subparse_script( //<<<
		Tcl_Interp*				interp,
		struct pidata*			l,
		domNode*				parent,
		const char* restrict	text,
		const int				textlen,
		const int				ofs,
		struct lineidx*			lineindex,
		uint32_t				lines,
		const int				incmdsubst,
		const char**restrict	endPtr)
{
	//fprintf(stderr, "subparse_script (%s), ofs: %d\n", text, ofs);

	domDocument*		doc = parent->ownerDocument;
	domNode*			cmdnode = NULL;
	domNode*			wordnode = NULL;
	const char*			last_wordend = NULL;
	const char*			cur = text;
	const char*const	end = text + textlen;
	const bool			full = strcmp(doc->documentElement->nodeName, "tcl") == 0;

	Tcl_Obj*			linestarts	= NULL;
	Tcl_Obj*			cmd_parsers	= NULL;
	Tcl_Obj*			cmd_parser	= NULL;	// lambda in ::parsetcl::cmd_parsers dict for this command
	defer {
		replace_tclobj(&linestarts,		NULL);
		replace_tclobj(&cmd_parsers,	NULL);
		replace_tclobj(&cmd_parser,		NULL);
	}

	replace_tclobj(&cmd_parsers,
			Tcl_ObjGetVar2(interp, l->lit[LIT_CMD_PARSERS], NULL, TCL_GLOBAL_ONLY | TCL_LEAVE_ERR_MSG));
	if (!cmd_parsers) return TCL_ERROR;

	if (full)
		replace_tclobj(&linestarts, Tcl_NewListObj(0, NULL));

	domNode*	scriptnode = domNewElementNode(doc, "script");
	domAppendChild(parent, scriptnode);
	if (full) SET_UINT_ATTR(scriptnode, "idx", ofs);

	bool	done = false;
	while (cur < end && !done) {
		int			word = 0;
		const int	remain = end - cur;
		const char* commandEnd = NULL;

		Tcl_Parse	parse;
		TEST_OK(Tcl_ParseCommand(interp, cur, remain, incmdsubst, &parse));
		defer { Tcl_FreeParse(&parse); }

		commandEnd = parse.commandStart + parse.commandSize;

		//fprintf(stderr, "commentStart: %ld, commentSize: %d text: %s\n", parse.commentStart == NULL ? 0 : parse.commentStart-text+ofs, parse.commentSize, text);
		//fprintf(stderr, "commandStart: %ld, commandSize: %d\n", parse.commandStart-text+ofs, parse.commandSize);
		//fprintf(stderr, "numwords: %d, numTokens: %d\n", parse.numWords, parse.numTokens);
		//fprintf(stderr, "commandEnd: %ld\n", commandEnd-text);

		/* scan the chunk of text covered by Tcl_Parse for newlines while
		   it's pretty much guaranteed to be in cache, and add the offsets to
		   linestarts */
		if (full)
			for (int j=0; j<parse.commandSize; j++)
				if (cur[j] == '\n')
					TEST_OK(Tcl_ListObjAppendElement(interp, linestarts, Tcl_NewWideIntObj(cur-text+ofs+j)));

		if (parse.commentSize > 0) {
			const int			spacelen = parse.commentStart - cur;
			const char*const	commentEnd = parse.commentStart + parse.commentSize;

			if (spacelen)
				EMIT("space", scriptnode, cur, spacelen);

			EMIT("comment", scriptnode, parse.commentStart, parse.commentSize);

			if (commentEnd < parse.commandStart)
				EMIT("space", scriptnode, commentEnd, parse.commandStart - commentEnd);
		} else if (parse.commandStart > cur) {
			EMIT("space", scriptnode, cur, parse.commandStart - cur);
		}

		if (parse.numTokens) {
			cmdnode = domNewElementNode(doc, "command");
			if (full) {
				SET_UINT_ATTR(cmdnode, "idx", parse.commandStart-text+ofs);
				SET_UINT_ATTR(cmdnode, "len", parse.commandSize);
			}
			domAppendChild(scriptnode, cmdnode);
		}

		last_wordend = parse.commandStart;

		for (int t=0; t<parse.numTokens;) {
			const Tcl_Token*	token = &parse.tokenPtr[t];
			const int			spacelen = token->start - last_wordend;
			//const char*			wordend = token->start + token->size;
			bool				dynamic=false, raw=false;

			word++;

			/*
			fprintf(stderr, "token, t: %d, last_wordend-text: %ld\n", t, last_wordend-text);
			fprintf(stderr, "\ttoken %d: type: %s(%d), start: %ld, length %d, numComponents: %d\n",
					t, toktype_string(token->type), token->type, token->start-text+ofs,
					token->size, token->numComponents);
					*/

			if (spacelen)
				EMIT("space", cmdnode, last_wordend, spacelen);

			last_wordend = token->start + token->size;

			wordnode = domNewElementNode(doc, "word");
			if (full) {
				SET_UINT_ATTR(wordnode, "idx", token->start -text +ofs);
				SET_UINT_ATTR(wordnode, "len", token->size);
			}
			domAppendChild(cmdnode, wordnode);

			Tcl_DString	value;
			Tcl_DStringInit(&value);
			defer { Tcl_DStringFree(&value); }

			int	expand = 0;
			switch (token->type) {
				case TCL_TOKEN_EXPAND_WORD:
					SET_CONST_ATTR(wordnode, "expand", "");
					expand = 1;
					// Falls through
				case TCL_TOKEN_WORD:
				case TCL_TOKEN_SIMPLE_WORD:
					break;

				default: THROW_PRINTF("Unexpected token type: %d %s", token->type, toktype_string(token->type));
			}

			{
				const int	syntax_len = parse.tokenPtr[t+1].start - token->start;
				const char	c = token->start[expand*3];

				if (syntax_len) {
					//EMIT("syntax", wordnode, token->start, syntax_len);
					switch (c) {
						case '"': SET_CONST_ATTR(wordnode, "quoted", "quote"); break;
						case '{': SET_CONST_ATTR(wordnode, "quoted", "brace"); raw = true; break;
					}
				} else {
					//SET_CONST_ATTR(wordnode, "quoted", "none");
				}
			}

			if (token->numComponents) {
				TEST_OK(append_sub_tokens(
						interp,
						l,
						wordnode,
						text,
						token+1,
						token->numComponents,
						ofs,
						&dynamic,
						&value,
						raw,
						lineindex,
						lines,
						full));

				t += 1 + token->numComponents;
			}

			/*
			{
				const char*	aftertok = parse.tokenPtr[t-1].start + parse.tokenPtr[t-1].size;
				EMIT("syntax", wordnode, aftertok, wordend - aftertok);
			}
			*/

			/*
			 * If the word has a static literal value, loft it into
			 * the value= attrib on the word, and look for a deep
			 * parser in $::parsetcl::cmd_parsers
			 */
			if (!dynamic) {
				const char*		valuestr = Tcl_DStringValue(&value);
				const Tcl_Size	valuelen = Tcl_DStringLength(&value);
				domSetAttributeEx(wordnode, "value", sizeof("value")-1, valuestr, valuelen);

				if (word == 1) {
					Tcl_Obj*	cmdname	= NULL;		defer { replace_tclobj(&cmdname, NULL); }

					// If this is the first word, record the value as the name of this command
					domSetAttributeEx(cmdnode, "name", sizeof("name")-1, valuestr, valuelen);

					// Look up a command parser for this command
					replace_tclobj(&cmdname, Tcl_NewStringObj(valuestr, valuelen));
					Tcl_Obj*	lambda;	// on loan from dict
					if (TCL_OK == Tcl_DictObjGet(NULL, cmd_parsers, cmdname, &lambda)) {
						replace_tclobj(&cmd_parser, lambda);
					} else {
						//fprintf(stderr, "No cmd_parser found for (%s)\n", Tcl_GetString(cmdname));
					}
				}
			} else {
				if (word == 1) {
					//fprintf(stderr, "command name word is not static\n");
					replace_tclobj(&cmd_parser, NULL);
				}
			}
		}

		//fprintf(stderr, "cmd tail, last_wordend: %ld, commandEnd: %ld\n", last_wordend-text, commandEnd-text);
		if (last_wordend < commandEnd) {
			switch (commandEnd[-1]) {
				case ']':
					if (incmdsubst) done = true;
					// Falls through
				case '\n':
				case ';':
					if (0 && cmdnode) {
						EMIT("space", cmdnode, last_wordend, commandEnd-last_wordend-1);
						EMIT("end",   cmdnode, commandEnd-1, 1);
					} else {
						// TODO: Should these always go onto scriptnode?
						EMIT("space", scriptnode, last_wordend, commandEnd-last_wordend-1);
						EMIT("end",   scriptnode, commandEnd-1, 1);
					}
					break;
				default:
					if (0 && cmdnode) {
						EMIT("space", cmdnode, last_wordend, commandEnd-last_wordend);
					} else {
						// TODO: Should these always go onto scriptnode?
						EMIT("space", scriptnode, last_wordend, commandEnd-last_wordend);
					}
			}
		} else {
			/*
			// This is the one zero-length token we want to allow
			domNode*	node = domNewElementNode(doc, "end");

			SET_UINT_ATTR(node, "idx", commandEnd-text);
			SET_UINT_ATTR(node, "len", 0);
			domAppendChild(cmdnode, node);
			*/
		}

		// Attempt to deep parse this command
		if (cmd_parser && parse.numTokens) {
			char	nodecmd[80];
			tcldom_createNodeObj(interp, cmdnode, (char*)&nodecmd);
			//fprintf(stderr, "Created dom command: (%s) for cmdnode: %p\n", nodecmd, cmdnode);

			Tcl_Obj*	cmd = NULL;		defer { replace_tclobj(&cmd, NULL); }
			replace_tclobj(&cmd, Tcl_NewListObj(3, NULL));
			TEST_OK(Tcl_ListObjAppendElement(interp, cmd, l->lit[LIT_APPLY]));
			TEST_OK(Tcl_ListObjAppendElement(interp, cmd, cmd_parser));
			TEST_OK(Tcl_ListObjAppendElement(interp, cmd, Tcl_NewStringObj(nodecmd, -1)));

			if (TCL_OK != Tcl_EvalObjEx(interp, cmd, TCL_EVAL_DIRECT | TCL_EVAL_GLOBAL)) {
				Tcl_Obj*	res		= NULL;
				Tcl_Obj*	options	= NULL;
				defer {
					replace_tclobj(&res,		NULL);
					replace_tclobj(&options,	NULL);
				}

				replace_tclobj(&options,	Tcl_GetReturnOptions(interp, TCL_ERROR));
				replace_tclobj(&res,		Tcl_GetObjResult(interp));

				const char*	cmdname = NULL;
#if 0
				Tcl_Obj*	getXML	= NULL;		defer { replace_tclobj(&getXML, NULL); }
				replace_tclobj(&getXML, Tcl_NewListObj(3, (Tcl_Obj**){
					l->lit[LIT_DOMNODE],
					Tcl_NewStringObj(nodecmd, -1),
					l->lit[LIT_ASXML],
				}));
				TEST_OK(Tcl_EvalObjEx(interp, getXML, TCL_EVAL_DIRECT | TCL_EVAL_GLOBAL));
				const char*	asXML = Tcl_GetString(Tcl_GetObjResult(interp));

				if (TCL_OK == get_attr(NULL, cmdnode, "name", &cmdname)) {
					fprintf(stderr, "Could not parse command (%s): %s\n%s\n", cmdname, Tcl_GetString(cmd), asXML);
				} else {
					fprintf(stderr, "Could not parse command: %s\n%s\n", Tcl_GetString(cmd), asXML);
				}
#else
				if (TCL_OK == get_attr(NULL, cmdnode, "name", &cmdname)) {
					fprintf(stderr, "Could not parse command (%s): %s\n", cmdname, Tcl_GetString(cmd));
				} else {
					fprintf(stderr, "Could not parse command: %s\n", Tcl_GetString(cmd));
				}
#endif

				Tcl_SetReturnOptions(interp, options);
				Tcl_SetObjResult(interp, res);

				return TCL_ERROR;
			}
			Tcl_ResetResult(interp);
		}

		cur = parse.commandStart + parse.commandSize;
	}

	if (endPtr) *endPtr = cur;

	if (full) {
		Tcl_Size	linestarts_len;
		const char*	linestarts_str = Tcl_GetStringFromObj(linestarts, &linestarts_len);
		uint32_t	startline = 1;
		uint32_t	startcol  = 1;

		if (lineindex)
			byte2line(lineindex, lines, ofs, &startline, &startcol);

		SET_UINT_ATTR(scriptnode, "len",     textlen);
		SET_UINT_ATTR(scriptnode, "lineofs", startcol - 1);
		domSetAttributeEx(scriptnode, "linestarts", sizeof("linestarts")-1, linestarts_str, linestarts_len);
	}

	return TCL_OK;
}

//>>>
static int subparse_expr( //<<<
		Tcl_Interp*			interp,
		struct pidata*		l,
		domNode*			parent,
		const char*restrict	text,
		const int			textlen,
		const int			ofs,
		struct lineidx*		lineindex,
		uint32_t			lines)
{
	//fprintf(stderr, "subparse_expr (%s), ofs: %d\n", text, ofs);

	domDocument*	doc = parent->ownerDocument;
	domNode*		exprnode = domNewElementNode(doc, "expr");
	domAppendChild(parent, exprnode);
	SET_UINT_ATTR(exprnode, "idx", ofs);

	Tcl_Parse	parse;
	TEST_OK(Tcl_ParseExpr(interp, text, textlen, &parse));
	defer { Tcl_FreeParse(&parse); }

	const bool	full = strcmp(doc->documentElement->nodeName, "tcl") == 0;
	const char*	last_wordend = text;
	for (int t=0; t<parse.numTokens;) {
		const Tcl_Token*	token = &parse.tokenPtr[t];

		/*
		fprintf(stderr, "token, t: %d, last_wordend-text: %ld\n", t, last_wordend-text);
		fprintf(stderr, "\ttoken %d: type: %s(%d), start: %ld, length %d, numComponents: %d\n",
				t, toktype_string(token->type), token->type, token->start-text+ofs,
				token->size, token->numComponents);
				*/

		if (token->type != TCL_TOKEN_SUB_EXPR)
			THROW_PRINTF("Unexpected token type: %d %s", token->type, toktype_string(token->type));

		const int	spacelen = token->start - last_wordend;
		if (spacelen) EMIT("space", exprnode, last_wordend, spacelen);
		last_wordend = token->start + token->size;

		if (token->numComponents) {
			bool	dynamic = false;
			TEST_OK(append_sub_tokens(
					interp,
					l,
					exprnode,
					text,
					token,
					parse.numTokens,
					ofs,
					&dynamic,
					NULL,
					0,
					lineindex,
					lines,
					full));

			t += 1 + token->numComponents;
		}
	}

	if (last_wordend < text+textlen)
		EMIT("space", exprnode, last_wordend, text+textlen-last_wordend);

	return TCL_OK;
}

//>>>
static int min(const int a, const int b) //<<<
{
	return a < b ? a : b;
}

//>>>
static int parse_hex(const char* text, const int maxchars) //<<<
{
	int		i = 0;
	while (i < maxchars) {
		const char	c = text[i];

		if (
				(c >= 'a' && c <= 'f') ||
				(c >= 'A' && c <= 'F') ||
				(c >= '0' && c <= '9')
		) {
			i++;
			continue;
		}

		break;
	}
	return i;
}

//>>>
static int parse_octal(const char*restrict text, const int maxchars) //<<<
{
	int		i = 0;
	while (i < maxchars) {
		const char	c = text[i];

		if (c >= '0' && c <= '7') {
			i++;
			continue;
		}

		break;
	}
	return i;
}

//>>>
static int parse_combined(Tcl_Interp* interp, struct pidata* l, const bool braced, domNode* parent, const char* text, const char*const end, const char** parent_cur, const int ofs, struct lineidx* lineindex, uint32_t lines) //<<<
{
	const char*restrict	cur = *parent_cur;
	defer { *parent_cur = cur; }

	domDocument*	doc = parent->ownerDocument;
	const bool	full = strcmp(doc->documentElement->nodeName, "tcl") == 0;
	domNode*	wordnode = domNewElementNode(doc, "word");
	domAppendChild(parent, wordnode);
	EMIT("syntax", wordnode, cur, 1);

	const char*	qend = NULL;
	Tcl_Parse	parse;
	if (braced) {
		SET_CONST_ATTR(wordnode, "quoted", "brace");
		TEST_OK(Tcl_ParseBraces(interp, cur, end-cur, &parse, 0, &qend));
	} else {
		SET_CONST_ATTR(wordnode, "quoted", "quote");
		TEST_OK(Tcl_ParseQuotedString(interp, cur, end-cur, &parse, 0, &qend));
	}
	defer { Tcl_FreeParse(&parse); }

	Tcl_DString		value;
	Tcl_DStringInit(&value);
	defer { Tcl_DStringFree(&value); }

	if (parse.numTokens) {
		bool	dynamic = false;

		TEST_OK(append_sub_tokens(
				interp,
				l,
				wordnode,
				text,
				parse.tokenPtr,
				parse.numTokens,
				ofs,
				&dynamic,
				&value,
				braced,
				lineindex,
				lines,
				full));

		if (!dynamic)
			SET_VALUE_ATTRIB(wordnode, &value);
	}

	if (full) {
		SET_UINT_ATTR(wordnode, "idx", cur-text+ofs);
		SET_UINT_ATTR(wordnode, "len", qend - cur);
	}

	cur = qend;
	EMIT("syntax", wordnode, qend-1, 1);

	return TCL_OK;
}

//>>>
static int escape_value(Tcl_Interp* interp, const char* text, const int len, Tcl_DString* value) //<<<
{
	if (len == 1) {
		Tcl_DStringAppend(value, text, 1);
		return TCL_OK;
	}

	Tcl_Obj*	raw = NULL;		defer { replace_tclobj(&raw,	NULL); }
	Tcl_Obj*	escape = NULL;	defer { replace_tclobj(&escape,	NULL); }

	replace_tclobj(&raw,	Tcl_NewStringObj(text, len));
	replace_tclobj(&escape,	Tcl_SubstObj(interp, raw, TCL_SUBST_BACKSLASHES));
	if (!escape) return TCL_ERROR;

	Tcl_Size	elen;
	const char*	bytes = Tcl_GetStringFromObj(escape, &elen);
	Tcl_DStringAppend(value, bytes, elen);

	return TCL_OK;
}

//>>>
static int subparse_list( //<<<
		Tcl_Interp*				interp,
		struct pidata*			l,
		domNode*				parent,
		const char* restrict	text,
		const int				textlen,
		const int				ofs,
		struct lineidx*			lineindex,
		uint32_t				lines)
{
	domDocument*	doc = parent->ownerDocument;
	const bool		full = strcmp(doc->documentElement->nodeName, "tcl") == 0;
	domNode*		listnode = NULL;

	Tcl_DString	value;
	Tcl_DStringInit(&value);
	defer { Tcl_DStringFree(&value); }

	listnode = domNewElementNode(doc, "list");
	domAppendChild(parent, listnode);
	if (full)
		SET_UINT_ATTR(listnode, "idx", ofs);

	const char*			cur = text;
	const char*const	end = text + textlen;
	while (cur < end) { // Each word
		const char*const	tok = cur;

		while (cur < end) {
			switch (cur[0]) {
				case '\t':
				case '\n':
				case '\v':
				case '\f':
				case '\r':
				case ' ':
					cur++;
					continue;
			}
			break;
		}
		if (cur > tok)
			EMIT("space", listnode, tok, cur-tok);

		if (cur >= end) return TCL_OK;

		switch (cur[0]) {
			case '{': TEST_OK(parse_combined(interp, l, 1, listnode, text, end, &cur, ofs, lineindex, lines)); break;
			case '"': TEST_OK(parse_combined(interp, l, 0, listnode, text, end, &cur, ofs, lineindex, lines)); break;

			default: // Parse unquoted word <<<
			{
				const char*const	wordstart = cur;
				const char*			tok = wordstart;

				domNode*	wordnode = domNewElementNode(doc, "word");
				domAppendChild(listnode, wordnode);

				Tcl_DStringInit(&value);

				while (cur < end) {
					switch (cur[0]) {
						case '\t':
						case '\n':
						case '\v':
						case '\f':
						case '\r':
						case ' ':
							break;

						case '\\':
							if (cur == end-1) {
								// Trailing backslash as the last char before EOF, in bare word mode this is just itself
								cur++;
								continue;
							}

							if (cur > tok) {
								Tcl_DStringAppend(&value, tok, cur-tok);
								EMIT("text", wordnode, tok, cur-tok);
							}

							cur++;
							switch (cur[0]) {
								case 'a': Tcl_DStringAppend(&value, "\a", 1); break;
								case 'b': Tcl_DStringAppend(&value, "\b", 1); break;
								case 'f': Tcl_DStringAppend(&value, "\f", 1); break;
								case 'n': Tcl_DStringAppend(&value, "\n", 1); break;
								case 'r': Tcl_DStringAppend(&value, "\r", 1); break;
								case 't': Tcl_DStringAppend(&value, "\t", 1); break;
								case 'v': Tcl_DStringAppend(&value, "\v", 1); break;

								case '\n':
									// Line folding - effectively a whitespace that ends this bare word
									continue;

								case 'x':
									{
										const int consumed = parse_hex(cur+1, end-cur-1);
										TEST_OK(escape_value(interp, cur-1, consumed+2, &value));
										EMIT("escape", wordnode, cur-1, consumed+2);
										cur += consumed+1;
									}
									break;

								case 'u':
									{
										const int consumed = parse_hex(cur+1, min(4, end-cur-1));
										TEST_OK(escape_value(interp, cur-1, consumed+2, &value));
										EMIT("escape", wordnode, cur-1, consumed+2);
										cur += consumed+1;
									}
									break;

								default:
									{
										const int consumed = parse_octal(cur+1, min(3, end-cur-1));
										TEST_OK(escape_value(interp, cur-1, consumed+1, &value));
										EMIT("escape", wordnode, cur-1, consumed+1);
										cur += consumed;
									}
									break;
							}
							cur++;
							tok = cur;
							continue;

						default:
							cur++;
							continue;
					}
					break;
				}

				if (cur > tok) {
					Tcl_DStringAppend(&value, tok, cur-tok);
					EMIT("text", wordnode, tok, cur-tok);
				}


				if (full) {
					SET_UINT_ATTR(wordnode, "idx", wordstart-text+ofs);
					SET_UINT_ATTR(wordnode, "len", cur-wordstart);
				}

				SET_VALUE_ATTRIB(wordnode, &value);
			}
			//>>>
		}
	}

	return TCL_OK;
}

//>>>
static int subparse_subst( //<<<
		Tcl_Interp*				interp,
		struct pidata*			l,
		const int				flags,		// some combination of TCL_SUBST_COMMANDS, TCL_SUBST_VARIABLES, TCL_SUBST_BACKSLASHES
		domNode*				parent,
		const char* restrict	text,
		const int				textlen,
		const int				ofs,
		struct lineidx*			lineindex,
		uint32_t				lines)
{
	const char*restrict	cur = text;
	const char*restrict	tok = cur;
	const char*const	end = text+textlen;

	domDocument*	doc = parent->ownerDocument;
	domNode*		substnode = domNewElementNode(doc, "subst");
	domAppendChild(parent, substnode);

	const bool	full = strcmp(doc->documentElement->nodeName, "tcl") == 0;
	if (full) SET_UINT_ATTR(substnode, "idx", ofs);

	if (!(flags & TCL_SUBST_VARIABLES))		SET_CONST_ATTR(substnode, "novariables",	"");
	if (!(flags & TCL_SUBST_COMMANDS))		SET_CONST_ATTR(substnode, "nocommands",		"");
	if (!(flags & TCL_SUBST_BACKSLASHES))	SET_CONST_ATTR(substnode, "nobackslashes",	"");

	bool	dynamic = false;	// true if word contains command or variable substitutions

	Tcl_DString	value;		// accumulates the static literal value if the word has one (!dynamic)
	Tcl_DStringInit(&value);
	defer { Tcl_DStringFree(&value); }

	while (cur < end) {
		switch (cur[0]) {
			case '$': //<<<
				if (flags & TCL_SUBST_VARIABLES) {
					dynamic = true;

					if (cur > tok)
						EMIT("text", substnode, tok, cur-tok);

					Tcl_Parse	parse;
					TEST_OK(Tcl_ParseVarName(interp, cur, end-cur, &parse, 0));
					defer { Tcl_FreeParse(&parse); }

					if (parse.numTokens) {
						TEST_OK(append_sub_tokens(
								interp,
								l,
								substnode,
								text,
								parse.tokenPtr,
								parse.numTokens,
								ofs,
								&dynamic,
								&value,
								0,
								lineindex,
								lines,
								full));

						tok = cur = parse.tokenPtr[0].start + parse.tokenPtr[0].size;
					}
				} else {
					cur++;
				}
				break;
				//>>>
			case '[': //<<<
				if (flags & TCL_SUBST_COMMANDS) {
					const char*	send = NULL;

					dynamic = true;

					if (cur > tok)
						EMIT("text", substnode, tok, cur-tok);

					EMIT("syntax", substnode, cur, 1);
					cur++;

					TEST_OK(subparse_script(
							interp,
							l,
							substnode,
							cur,
							end-cur,
							ofs+(cur-text),
							lineindex,
							lines,
							1,
							&send));

					tok = cur = send;
				} else {
					cur++;
				}
				break;
				//>>>
			case '\\': //<<<
				if (!(flags & TCL_SUBST_BACKSLASHES)) {
					cur++;
					break;
				}
				if (cur > tok) {
					EMIT("text", substnode, tok, cur-tok);
					if (!dynamic) Tcl_DStringAppend(&value, tok, cur-tok);
					tok = cur;
				}

				if (cur == end-1) {
					// Trailing backslash as the last char before EOF, in bare word mode this is just itself
					cur++;
					continue;
				}

#define LETTER_ESCAPE(c) \
	do { \
		Tcl_DStringAppend(&value, c, 1); \
		EMIT("escape", substnode, cur-1, 2); \
		tok = ++cur; \
	} while(0);

				cur++;
				switch (cur[0]) {
					case 'a': LETTER_ESCAPE("\a"); break;
					case 'b': LETTER_ESCAPE("\b"); break;
					case 'f': LETTER_ESCAPE("\f"); break;
					case 'n': LETTER_ESCAPE("\n"); break;
					case 'r': LETTER_ESCAPE("\r"); break;
					case 't': LETTER_ESCAPE("\t"); break;
					case 'v': LETTER_ESCAPE("\v"); break;
#undef LETTER_ESCAPE

					case '\n':
						// Line folding: \\\n[ \t]+ seq is equivalent to a single space
						if (cur > tok) {
							EMIT("text", substnode, tok, cur-tok);
							EMIT("text", substnode, " ", 1);
							if (!dynamic) {
								Tcl_DStringAppend(&value, tok, cur-tok);
								Tcl_DStringAppend(&value, " ", 1);
							}
						}
						cur++;
						while (cur < end) {
							switch (cur[0]) {
								case ' ':
								case '\t':
									cur++;
									continue;
							}
							break;
						}
						tok = cur;
						continue;

#define CODEPOINT_ESCAPE(f, digits_start, max_digits) \
	do { \
		const char*const	digits_s = digits_start; \
		const char*const	escapestart = cur-1; \
		const int			consumed = (f)(digits_s, min(max_digits, end-cur-1)); \
		const int			escapelen = digits_s +consumed -escapestart; \
		if (consumed == 0) { /* Not a valid codepoint escape sequence, the value is just the backquoted character */ \
			EMIT("escape", substnode, cur-1, 2); \
			if (!dynamic) Tcl_DStringAppend(&value, cur, 1); \
			cur++; \
		} else { \
			EMIT("escape", substnode, escapestart, escapelen); \
			if (!dynamic) TEST_OK(escape_value(interp, escapestart, escapelen, &value)); \
			cur = digits_s + consumed; \
		} \
		tok = cur; \
	} while(0);
					case 'x': CODEPOINT_ESCAPE(parse_hex,   cur+1, 2); break;
					case 'u': CODEPOINT_ESCAPE(parse_hex,   cur+1, 4); break;
					default:  CODEPOINT_ESCAPE(parse_octal, cur,   3); break;
#undef CODEPOINT_ESCAPE
				}
				break;
				//>>>
			default: cur++; break;
		}
	}

	if (cur > tok) {
		EMIT("text", substnode, tok, cur-tok);
		if (!dynamic) Tcl_DStringAppend(&value, tok, cur-tok);
	}

	if (!dynamic)
		SET_VALUE_ATTRIB(substnode, &value);

	return TCL_OK;
}

//>>>
static int subparse(ClientData cdata, Tcl_Interp* interp, int objc, Tcl_Obj* const objv[]) //<<<
{
	struct pidata*	l = cdata;

	enum {A_cmd, A_MODE, A_WORD, A_args};
	CHECK_MIN_ARGS("mode word");

	static const char* modes[] = {
		"script",
		"expr",
		"list",
		"subst",
		"sql",
		"json",
		"javascript",
		NULL
	};
	enum {
		MODE_SCRIPT,
		MODE_EXPR,
		MODE_LIST,
		MODE_SUBST,
		MODE_SQL,
		MODE_JSON,
		MODE_JAVASCRIPT
	} mode;
	int	modeidx;
	TEST_OK(Tcl_GetIndexFromObj(interp, objv[A_MODE], modes, "mode", TCL_EXACT, &modeidx));
	mode = modeidx;

	const char*		text = NULL;
	domDocument*	doc = NULL;
	domNode*		asnode = NULL;
	int				ofs;
	bool			full;
	{ // Get doc context and literal word value <<<
		char*		errmsg = NULL;
		domNode*	wordnode = tcldom_getNodeFromName(interp, Tcl_GetString(objv[A_WORD]), &errmsg);
		if (!wordnode) {
			Tcl_SetObjResult(interp, errmsg ?
					Tcl_ObjPrintf("tDOM error getting node: \"%s\"", errmsg) :
					Tcl_ObjPrintf("Could not retrieve node from \"%s\"", Tcl_GetString(objv[A_WORD])));
			return TCL_ERROR;
		}

		doc = wordnode->ownerDocument;
		full = strcmp(doc->documentElement->nodeName, "tcl") == 0;

		for (domNode* node = wordnode->firstChild; node; node = node->nextSibling) {
			if (node->nodeType != ELEMENT_NODE) continue;
			if (strcmp("as", node->nodeName) != 0) continue;
			asnode = node;
			break;
		}
		if (asnode == NULL) {
			asnode = domNewElementNode(doc, "as");
			domAppendChild(wordnode, asnode);
		}

		// Extract text from word <<<
		//TEST_OK(get_attr(interp, wordnode, "value", &text));
		if (TCL_OK != get_attr(NULL, wordnode, "value", &text)) return TCL_OK; // TODO: warn about this (word to subparse doesn't have a static literal value)

		if (full) {
			const char*	idx = NULL;
			TEST_OK(get_attr(interp, wordnode, "idx", &idx));

			int	scanned;
			sscanf(idx, "%d%n", &ofs, &scanned);
			if (scanned < (int)strlen(idx))
				THROW_PRINTF("Invalid idx value: \"%s\", must be an integer, scanned: %d, strlen(idx): %ld", idx, scanned, strlen(idx));

			const char*	quoted = NULL;
			if (TCL_OK == get_attr(NULL, wordnode, "quoted", &quoted))
				ofs++; // Word being parsed is quoted, adjust the offset for the quote char
		} else {
			ofs = 0;
		}
		// Extract text from word >>>
	}
	//>>>

	switch (mode) {
		case MODE_SCRIPT: //<<<
		{
			enum {A_cmd, A_MODE, A_WORD, A_objc};
			CHECK_ARGS("mode word");

			if (TCL_OK != subparse_script(
					interp,
					l,
					asnode,
					text,
					strlen(text),
					ofs,
					NULL,
					0,
					0,
					NULL)
			) THROW_PRINTF("Error parsing \"%s\" as Tcl script: %s", text, Tcl_GetString(Tcl_GetObjResult(interp)));

			break;
		}
		//>>>
		case MODE_EXPR: //<<<
		{
			enum {A_cmd, A_MODE, A_WORD, A_objc};
			CHECK_ARGS("mode word");

			TEST_OK(subparse_expr(
					interp,
					l,
					asnode,
					text,
					strlen(text),
					ofs,
					NULL,
					0));

			break;
		}
		//>>>
		case MODE_LIST: //<<<
		{
			enum {A_cmd, A_MODE, A_WORD, A_objc};
			CHECK_ARGS("mode word");

			TEST_OK(subparse_list(
					interp,
					l,
					asnode,
					text,
					strlen(text),
					ofs,
					NULL,
					0));
			break;
		}
		//>>>
		case MODE_SUBST: //<<<
			{
				int flags = TCL_SUBST_ALL;
				for (int i=A_args; i<objc; i++) {
					static struct {
						const char*	switchname;
						int			flagmask;
					} switches[] = {
						{ "-nocommands",		~TCL_SUBST_COMMANDS		},
						{ "-novariables",		~TCL_SUBST_VARIABLES	},
						{ "-nobackslashes",		~TCL_SUBST_BACKSLASHES	},
						{}
					};
					int sw;
					TEST_OK(Tcl_GetIndexFromObjStruct(interp, objv[i], switches, sizeof(switches[0]), "switch", TCL_EXACT, &sw));
					flags &= switches[sw].flagmask;
				}
				
				TEST_OK(subparse_subst(
						interp,
						l,
						flags,
						asnode,
						text,
						strlen(text),
						ofs,
						NULL,
						0));
			}
			break;
			//>>>

		case MODE_SQL:
		case MODE_JSON:
		case MODE_JAVASCRIPT: //<<<
			{
				enum {A_cmd, A_MODE, A_WORD, A_objc};
				CHECK_ARGS("mode word");

				domNode*	typenode = domNewElementNode(doc, modes[mode]);

				domAppendNewTextNode(typenode, (char*)text, strlen(text), TEXT_NODE, 0);
				if (full) {
					SET_UINT_ATTR(typenode, "idx", ofs);
					SET_UINT_ATTR(typenode, "len", strlen(text));
				}
				SET_CONST_ATTR(typenode, "unparsed", "");
				domAppendChild(asnode, typenode);
			}
			break;
			//>>>

		default: THROW_PRINTF("Unexpected mode index: %d", mode);
	}

	return TCL_OK;
}

//>>>
static void byte2line(struct lineidx*const lineindex, uint32_t lines, const uint32_t byteofs, uint32_t* linePtr, uint32_t* cPtr) //<<<
{
	if (lines == 0) {
		*linePtr = 1;
		*cPtr = byteofs + 1;
		return;
	}

	// Find largest line index L where lineindex[L].bytestart <= byteofs
	uint32_t	lo = 0;
	uint32_t	hi = lines;
	while (hi - lo > 1) {
		const uint32_t	mid = lo + ((hi - lo) >> 1);
		if (byteofs < lineindex[mid].bytestart) {
			hi = mid;
		} else {
			lo = mid;
		}
	}
	const uint32_t			line = lo;
	const uint32_t			linestart = lineindex[line].bytestart;
	const uint32_t			lineadjs  = lineindex[line].line->lineadjs;
	struct encskip*const	skips     = lineindex[line].line->skips;

	uint32_t	adj = 0;
	if (lineadjs > 0 && byteofs >= skips[0].bytestart) {
		uint32_t	slo = 0;
		uint32_t	shi = lineadjs;
		while (shi - slo > 1) {
			const uint32_t	mid = slo + ((shi - slo) >> 1);
			if (byteofs < skips[mid].bytestart) {
				shi = mid;
			} else {
				slo = mid;
			}
		}
		adj = skips[slo].adj;
	}

	*linePtr = line + 1;					// 1-based line numbers
	*cPtr = byteofs - linestart - adj + 1;	// 1-based character numbers
}

//>>>
static int parse_tcl_script(Tcl_Interp* interp, struct pidata* l, Tcl_Obj* script, domDocument** res, const int full) //<<<
{
	domDocument*	doc = domCreateDocument(NULL, full ? "tcl" : "ast");	defer { if (doc) domFreeDocument(doc, NULL, NULL); }
	domNode*		root = doc->documentElement;

	Tcl_Size	textlen;
	const char*	text = Tcl_GetStringFromObj(script, &textlen);

	// Index the line and character offsets <<<
	Tcl_Obj*	idxobj = NULL;	defer { replace_tclobj(&idxobj, NULL); }
	//TIME("Index line and char offsets",
	idxobj = NewLineIdxObj(script);
	//)

	struct lineidx*	lineindex = NULL;
	uint32_t		lines = 0;
	TEST_OK(GetLineIdxFromObj(interp, idxobj, &lineindex, &lines));
	//>>>

	//TIME("parse script",
	TEST_OK(subparse_script(interp, l, root, text, textlen, 0, lineindex, lines, 0, NULL));
	//);

	if (*res) domFreeDocument(*res, NULL, NULL);
	*res = doc;
	doc = NULL;

	return TCL_OK;
}

//>>>
static int parse_and_register(Tcl_Interp* interp, struct pidata* l, Tcl_Obj* obj, const Tcl_ObjType* type, int full, uintptr_t* cookie_out) //<<<
{
	domDocument*	doc = NULL;		defer { if (doc) domFreeDocument(doc, NULL, NULL); }
	TEST_OK(parse_tcl_script(interp, l, obj, &doc, full));

	struct ptdata*	td = get_ptdata();
	const uintptr_t	cookie = td->next_cookie++;
	int				isnew;
	Tcl_HashEntry*	he = Tcl_CreateHashEntry(&td->scriptdoms, (void*)cookie, &isnew);
	Tcl_SetHashValue(he, obj);

	Tcl_StoreInternalRep(obj, type, &(Tcl_ObjInternalRep){
		.twoPtrValue.ptr1 = doc,
		.twoPtrValue.ptr2 = (void*)cookie,
	});
	doc = NULL;	// Hand ownership to the intrep

	if (cookie_out) *cookie_out = cookie;
	return TCL_OK;
}

//>>>
static int get_parsetree_from_obj(Tcl_Interp* interp, struct pidata* l, Tcl_Obj* obj, domDocument** doc, uintptr_t* cookie) //<<<
{
	Tcl_ObjInternalRep*	ir = Tcl_FetchInternalRep(obj, &parsetree);
	if (!ir) {
		TEST_OK(parse_and_register(interp, l, obj, &parsetree, 1, NULL));
		ir = Tcl_FetchInternalRep(obj, &parsetree);
	}
	*doc = (domDocument*)ir->twoPtrValue.ptr1;
	if (cookie) *cookie = (uintptr_t)ir->twoPtrValue.ptr2;
	return TCL_OK;
}

//>>>
static int get_ast_from_obj(Tcl_Interp* interp, struct pidata* l, Tcl_Obj* obj, domDocument** doc, uintptr_t* cookie) //<<<
{
	Tcl_ObjInternalRep*	ir = Tcl_FetchInternalRep(obj, &astObjtype);
	if (!ir) {
		TEST_OK(parse_and_register(interp, l, obj, &astObjtype, 0, NULL));
		ir = Tcl_FetchInternalRep(obj, &astObjtype);
	}
	*doc = (domDocument*)ir->twoPtrValue.ptr1;
	if (cookie) *cookie = (uintptr_t)ir->twoPtrValue.ptr2;
	return TCL_OK;
}

//>>>
static int get_parsetree(ClientData cdata, Tcl_Interp* interp, int objc, Tcl_Obj *const objv[]) //<<<
{
	struct pidata*	l = cdata;

	enum {A_cmd, A_SCRIPT, A_objc};
	CHECK_ARGS("script");

	// Have to dup the script arg (and hide it from general script access) to
	// prevent it from converting away from our intrep (and thereby freeing the
	// tdom doc), leaving our node refs dangling.  Each node ref holds a reference
	// to the hidden scriptdom object
	Tcl_Obj*	scriptdom = NULL;		defer { replace_tclobj(&scriptdom, NULL); }
	replace_tclobj(&scriptdom, Tcl_DuplicateObj(objv[A_SCRIPT]));

	domDocument*	doc = NULL;
	uintptr_t		cookie = 0;
	TEST_OK(get_parsetree_from_obj(interp, l, scriptdom, &doc, &cookie));

	Tcl_SetObjResult(interp, NewNoderefObj(interp, doc->documentElement, scriptdom, cookie));

	return TCL_OK;
}

//>>>
static int get_ast(ClientData cdata, Tcl_Interp* interp, int objc, Tcl_Obj *const objv[]) //<<<
{
	struct pidata*	l = cdata;

	enum {A_cmd, A_SCRIPT, A_objc};
	CHECK_ARGS("script");

	// Have to dup the script arg (and hide it from general script access) to
	// prevent it from converting away from our intrep (and thereby freeing the
	// tdom doc), leaving our node refs dangling.  Each node ref holds a reference
	// to the hidden scriptdom object
	Tcl_Obj*	scriptdom = NULL;		defer { replace_tclobj(&scriptdom, NULL); }
	replace_tclobj(&scriptdom, Tcl_DuplicateObj(objv[A_SCRIPT]));

	domDocument*	doc = NULL;
	uintptr_t		cookie = 0;
	TEST_OK(get_ast_from_obj(interp, l, scriptdom, &doc, &cookie));

	Tcl_SetObjResult(interp, NewNoderefObj(interp, doc->documentElement, scriptdom, cookie));

	return TCL_OK;
}

//>>>
static int get_node(ClientData /*cdata*/, Tcl_Interp* interp, int objc, Tcl_Obj*const objv[]) //<<<
{
	enum {A_cmd, A_NODE, A_objc};
	CHECK_ARGS("node");

	struct noderef*	ref = NULL;
	TEST_OK(GetNoderefFromObj(interp, objv[A_NODE], &ref));

	Tcl_SetObjResult(interp, ref->tdomnodeobj);

	return TCL_OK;
}

//>>>
static void free_pidata(ClientData cdata, Tcl_Interp* /*interp*/) //<<<
{
	struct pidata*	l = cdata;

	if (!l) return;
	for (int i=0; i<LIT_END; i++) replace_tclobj(&l->lit[i], NULL);
	ckfree(l);
}

//>>>

struct cmd {
	char*			name;
	Tcl_ObjCmdProc*	proc;
} cmds[] = {
	{NS "::parsetree",	get_parsetree},
	{NS "::ast",		get_ast},
	{NS "::subparse",	subparse},
	{NS "::node",		get_node},
	{}
};

DLLEXPORT int Parsetcl_Init(Tcl_Interp* interp) //<<<
{
#ifdef USE_TCL_STUBS
	if (!Tcl_InitStubs(interp, TCL_VERSION, 0)) return TCL_ERROR;
#endif

#ifdef USE_TDOM_STUBS
	if (!Tdom_InitStubs(interp, "0.9.2", 0)) return TCL_ERROR;
#endif

	struct pidata*	l = ckalloc(sizeof(*l));
	*l = (struct pidata){};

	for (int i=0; i<LIT_END; i++)
		replace_tclobj(&l->lit[i], Tcl_NewStringObj(lit_strings[i], -1));

	Tcl_SetAssocData(interp, "parsetcl", free_pidata, l);

	Tcl_Namespace*	ns = Tcl_CreateNamespace(interp, NS, NULL, NULL);
	TEST_OK(Tcl_Export(interp, ns, "*", 0));
	Tcl_CreateEnsemble(interp, NS, ns, 0);

	for (struct cmd* c = cmds; c->name; c++)
		Tcl_CreateObjCommand(interp, c->name, c->proc, l, NULL);

	TEST_OK(Tcl_PkgProvide(interp, PACKAGE_NAME, PACKAGE_VERSION));

	return TCL_OK;
}

//>>>

/* Local Variables: */
/* tab-width: 4 */
/* c-basic-offset: 4 */
/* End: */
// vim: foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4
