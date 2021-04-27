#include "tclstuff.h"
#include <tdom.h>
#include <string.h>
#include <stdint.h>

#define NS "::parsetcl"

#define SET_INT_ATTR(node, attr, value) \
	do { \
		char		tmp[21];	\
		const int	len = u64toa((value), (tmp)); \
		tmp[len] = 0; \
		domSetAttributeEx((node), (attr), sizeof(attr)-1, tmp, len); \
	} while (0)

#define SET_CONST_ATTR(node, attr, value) \
	domSetAttributeEx(node, (attr), sizeof((attr))-1, (value), sizeof((value))-1)

#define EMIT(type, parent, from, length) \
	if (full && (length)>0) { \
		domNode*	node = domNewElementNode(doc, (type)); \
		domAppendNewTextNode(node, (from), (length), TEXT_NODE, 0); \
		if (full) { \
			SET_INT_ATTR(node, "idx", (from)-text+ofs); \
			SET_INT_ATTR(node, "len", (length)); \
		} \
		domAppendChild((parent), node); \
	}

#define SET_VALUE_ATTRIB(node, value) \
	do { \
		const char*	valuestr = Tcl_DStringValue(value); \
		const int	valuelen = Tcl_DStringLength(value); \
		domSetAttributeEx(node, "value", sizeof("value")-1, valuestr, valuelen); \
	} while(0)

// Per-interp local data {{{
enum {
	LIT_CMD_PARSERS,
	LIT_APPLY,
	LIT_END
};

static const char* lit_strings[] = {
	NS "::cmd_parsers",
	"apply",
	NULL
};

struct pidata {
	Tcl_Obj*		lit[LIT_END];
};
//}}}

// Prototypes {{{
static int subparse_script(
		Tcl_Interp*				interp,
		struct pidata*			l,
		domNode*				parent,
		const char* restrict	text,
		const int				textlen,
		const int				ofs,
		const int				lineofs,
		const int				incmdsubst,
		const char** restrict	end);
// Prototypes }}}

static void free_parsetree(Tcl_Obj* obj);
static void dup_parsetree(Tcl_Obj* src, Tcl_Obj* dest);
static void update_string_rep_parsetree(Tcl_Obj* obj);

Tcl_ObjType parsetree = {
	"parsetree",
	free_parsetree,
	dup_parsetree,
	update_string_rep_parsetree,
	NULL,
};

Tcl_ObjType ast = {
	"ast",
	free_parsetree,
	dup_parsetree,
	update_string_rep_parsetree,
	NULL,
};


static void free_parsetree(Tcl_Obj* obj) //{{{
{
	Tcl_ObjIntRep*	ir = NULL;

	//fprintf(stderr, "free_parsetree\n");
	ir = Tcl_FetchIntRep(obj, &parsetree);
	if (ir && ir->twoPtrValue.ptr1) {
		//fprintf(stderr, "freeing doc: %p\n", ir->twoPtrValue.ptr1);
		domFreeDocument((domDocument*)ir->twoPtrValue.ptr1, NULL, NULL);
		ir->twoPtrValue.ptr1 = NULL;
	}
}

//}}}
static void dup_parsetree(Tcl_Obj* src, Tcl_Obj* dest) //{{{
{
	Tcl_ObjIntRep*		srcir = NULL;
	Tcl_ObjIntRep		destir;
	domNode*			srcroot = NULL;
	domNode*			destroot = NULL;
	domDocument*		destdoc = NULL;

	fprintf(stderr, "dup_parsetree\n");

	srcir = Tcl_FetchIntRep(src, &parsetree);
	if (srcir == NULL)
		Tcl_Panic("dup_internal_rep asked to duplicate for type, but that type wasn't available on the src object");

	srcroot = srcir->twoPtrValue.ptr2;

	destdoc = domCreateDoc(NULL, 0);
	destroot = domCloneNode(srcroot, 1);
	destdoc->rootNode->firstChild = destdoc->rootNode->lastChild = destdoc->documentElement;
	domSetDocumentElement(destdoc);

	destir.twoPtrValue.ptr1 = destdoc;
	destir.twoPtrValue.ptr2 = destroot;

	Tcl_StoreIntRep(dest, &parsetree, &destir);
}

//}}}
static void update_string_rep_parsetree(Tcl_Obj* obj) //{{{
{
	Tcl_ObjIntRep*		ir = Tcl_FetchIntRep(obj, &parsetree);
	Tcl_DString			res;
	domNode*			child = NULL;
	domNode*			node = NULL;

	fprintf(stderr, "update_string_rep_parsetree\n");

	if (ir == NULL)
		Tcl_Panic("dup_internal_rep asked to duplicate for type, but that type wasn't available on the src object");

	node = ir->twoPtrValue.ptr2;

	// To make an XML Tcl_ObjType (which would be useful), serialize to XML here
	Tcl_DStringInit(&res);

	child = node->firstChild;
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

	Tcl_DStringFree(&res);
}

//}}}

const char* toktype_string(int type) //{{{
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

//}}}
static int get_attr(Tcl_Interp* interp, domNode* node, const char* attr, const char** value) //{{{
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

//}}}
// Fast unsigned int to string conversion from the talk by Alexandrescu: "Three Optimization Tips for C++" {{{
uint32_t digits10(uint64_t v) //{{{
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

//}}}
int u64toa(uint64_t value, char* restrict dst) //{{{
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

//}}}
//}}}
void set_int_attr(domNode* restrict node, const char* restrict attr, uint64_t v) //{{{
{
	char		tmp[21];		// Max decimal digits in 2**64: 20 +1 for \0
	const int	len = u64toa(v, tmp);

	tmp[len] = 0;
	//fprintf(stderr, "u64toa(%ld): \"%s\"\n", v, tmp);

	domSetAttributeEx(node, attr, strlen(attr), tmp, len);
}

//}}}
static int append_sub_tokens(Tcl_Interp* interp, struct pidata* l, domNode* parent, const char* restrict text, const Tcl_Token* subtokens, int numComponents, int ofs, int* dynamic, Tcl_DString* value, int raw, int lineofs, const int full) //{{{
{
	int				t;
	domNode*		toknode = NULL;
	domDocument*	doc = parent->ownerDocument;
	int				code = TCL_OK;
	int				expand = 0;

	for (t=0; t<numComponents; t++) {
		/*
		fprintf(stderr, "\tsubtoken %d: type: %s(%d), start: %ld, length %d, numComponents: %d\n",
				t, toktype_string(subtokens[t].type), subtokens[t].type, subtokens[t].start-text,
				subtokens[t].size, subtokens[t].numComponents);
		*/

		switch (subtokens[t].type) {
			case TCL_TOKEN_EXPAND_WORD: expand = 1; // Falls through
			case TCL_TOKEN_WORD:
			case TCL_TOKEN_SIMPLE_WORD:
				{
					Tcl_DString	value;			// Shadows argument!
					int			raw = 0;		// Shadows argument!
					int			dynamic = 0;	// Shadows argument!
					const int	syntax_len = subtokens[t+1].start - subtokens[t].start;
					const char	c = subtokens[t].start[expand*3];
					domNode*	wordnode = NULL;

					wordnode = domNewElementNode(doc, "word");
					if (full) {
						SET_INT_ATTR(wordnode, "idx", subtokens[t].start -text +ofs);
						SET_INT_ATTR(wordnode, "len", subtokens[t].size);
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
						code = append_sub_tokens(
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
								lineofs,
								full);

						if (code != TCL_OK) goto finally;
						t += 1 + subtokens[t].numComponents;
					}

					if (!dynamic) 
						SET_VALUE_ATTRIB(wordnode, &value);

					Tcl_DStringFree(&value);
				}
				break;


			case TCL_TOKEN_TEXT:
				toknode = domNewElementNode(doc, "text");
				if (full) {
					SET_INT_ATTR(toknode, "idx", subtokens[t].start-text+ofs);
					SET_INT_ATTR(toknode, "len", subtokens[t].size);
				}
				domAppendNewTextNode(toknode, subtokens[t].start, subtokens[t].size, TEXT_NODE, 0);
				domAppendChild(parent, toknode);
				if (!*dynamic)
					Tcl_DStringAppend(value, subtokens[t].start, subtokens[t].size);
				break;

			case TCL_TOKEN_BS:
				toknode = domNewElementNode(doc, "escape");
				if (full) {
					/*
					char idxstr[22];
					u64toa(subtokens[t].start-text+ofs, idxstr);
					fprintf(stderr, "idx: %ld, str: (%s)\n", subtokens[t].start-text+ofs, idxstr);
					*/
					SET_INT_ATTR(toknode, "idx", subtokens[t].start-text+ofs);
					SET_INT_ATTR(toknode, "len", subtokens[t].size);
				}
				//if (subtokens[t].size > 1 && subtokens[t].start[1] == '\n') { // Line folding
				//	fprintf(stderr, "Line folding case, adjusting by %d\n", subtokens[t].size-1);
				//	ofs -= subtokens[t].size-1;
				//}
				domAppendNewTextNode(toknode, subtokens[t].start, subtokens[t].size, TEXT_NODE, 0);
				domAppendChild(parent, toknode);
				if (!*dynamic) {
					if (raw) {
						Tcl_DStringAppend(value, subtokens[t].start, subtokens[t].size);
					} else {
						Tcl_Obj*	raw = NULL;
						Tcl_Obj*	escape = NULL;

						replace_tclobj(&raw, Tcl_NewStringObj(subtokens[t].start, subtokens[t].size));
						replace_tclobj(&escape, Tcl_SubstObj(interp, raw, TCL_SUBST_BACKSLASHES));
						if (escape) {
							int len;
							const char* bytes;

							bytes = Tcl_GetStringFromObj(escape, &len);
							Tcl_DStringAppend(value, bytes, len);
						} else {
							code = TCL_ERROR;
						}
						release_tclobj(&escape);
						release_tclobj(&raw);
						if (code != TCL_OK) goto finally;
					}
				}
				break;

			case TCL_TOKEN_VARIABLE:
				*dynamic = 1;
				toknode = domNewElementNode(doc, "var");
				if (subtokens[t].numComponents == 1) {
					SET_CONST_ATTR(toknode, "type", "scalar");
				} else {
					SET_CONST_ATTR(toknode, "type", "array");
				}
				if (full) {
					SET_INT_ATTR(toknode, "idx", subtokens[t].start-text+ofs);
					SET_INT_ATTR(toknode, "len", subtokens[t].size);
				}
				//domAppendNewTextNode(toknode, subtokens[t].start, subtokens[t].size, TEXT_NODE, 0);
				domAppendChild(parent, toknode);

				if (subtokens[t].numComponents) {
					code = append_sub_tokens(
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
							lineofs,
							full);

					if (code != TCL_OK) goto finally;
					domSetAttributeEx(toknode, "name", sizeof("name")-1, subtokens[t+1].start, subtokens[t+1].size);
					// TODO: if this is an array and the index tokens are static literals, store that in the "index" attribute
					t += subtokens[t].numComponents;
				}
				break;

			case TCL_TOKEN_COMMAND:
				{
					/*domNode*	node = domNewElementNode(doc, "syntax");
					domAppendNewTextNode(node, subtokens[t].start, 1, TEXT_NODE, 0);
					SET_INT_ATTR(node, "idx", subtokens[t].start-text+ofs);
					SET_INT_ATTR(node, "len", 1);
					domAppendChild(parent, node);
					*/

					*dynamic = 1;
					code = subparse_script(
							interp,
							l,
							parent,
							subtokens[t].start+1,
							subtokens[t].size-1,
							subtokens[t].start+1 - text + ofs,
							lineofs,
							1,
							NULL);

					if (code != TCL_OK) goto finally;
				}
				break;

			case TCL_TOKEN_SUB_EXPR:
				{
					Tcl_DString	value;			// Shadows argument!
					int			dynamic = 0;	// Shadows argument!

					toknode = domNewElementNode(doc, "subexpr");
					if (full) {
						SET_INT_ATTR(toknode, "idx", subtokens[t].start-text+ofs);
						SET_INT_ATTR(toknode, "len", subtokens[t].size);
					}
					//domAppendNewTextNode(toknode, subtokens[t].start, subtokens[t].size, TEXT_NODE, 0);
					domSetAttributeEx(toknode, "orig", sizeof("orig")-1, subtokens[t].start, subtokens[t].size);
					/*
					{
						Tcl_Obj* tmp = NULL;

						replace_tclobj(&tmp, Tcl_NewStringObj(subtokens[t].start, subtokens[t].size));
						fprintf(stderr, "subexpr: ->%s<-\n", Tcl_GetString(tmp));
						release_tclobj(&tmp);
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
						code = append_sub_tokens(
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
								lineofs,
								full);

						if (code != TCL_OK) goto finally;
						t += subtokens[t].numComponents;
					}
					if (!dynamic)
						SET_VALUE_ATTRIB(toknode, &value);

					Tcl_DStringFree(&value);
				}
				break;

			case TCL_TOKEN_OPERATOR:
				*dynamic = 1;
				toknode = domNewElementNode(doc, "operator");
				if (full) {
					SET_INT_ATTR(toknode, "idx", subtokens[t].start-text+ofs);
					SET_INT_ATTR(toknode, "len", subtokens[t].size);
				}
				domSetAttributeEx(toknode, "name", sizeof("name")-1, subtokens[t].start, subtokens[t].size);
				//domAppendNewTextNode(toknode, subtokens[t].start, subtokens[t].size, TEXT_NODE, 0);
				domAppendChild(parent, toknode);

				if (t < numComponents-1) {
					code = append_sub_tokens(
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
							lineofs,
							full);

					if (code != TCL_OK) goto finally;
				}
				goto finally;

			default:
				Tcl_SetObjResult(interp, Tcl_ObjPrintf("Unexpected token type: %d", subtokens[t].type));
				code = TCL_ERROR;
				goto finally;
		}
	}

finally:
	return code;
}

//}}}
static int subparse_script( //{{{
		Tcl_Interp*				interp,
		struct pidata*			l,
		domNode*				parent,
		const char* restrict	text,
		const int				textlen,
		const int				ofs,
		const int				lineofs,
		const int				incmdsubst,
		const char** restrict	end)
{
	domDocument*		doc = parent->ownerDocument;
	domNode*			scriptnode = NULL;
	domNode*			cmdnode = NULL;
	domNode*			wordnode = NULL;
	int					code = TCL_OK;
	Tcl_Parse			parse;
	int					j, t;
	Tcl_Obj*			linestarts = NULL;
	const char*			last_wordend = NULL;
	Tcl_Obj*			cmd_parsers = NULL;
	Tcl_Obj*			cmd_parser = NULL;	// lambda in ::parsetcl::cmd_parsers dict for this command
	const char*			base = text;
	const char*	const	textend = text + textlen;
	const int			full = strcmp(doc->documentElement->nodeName, "tcl") == 0;
	int					done = 0;

	//fprintf(stderr, "subparse_script (%s), ofs: %d\n", text, ofs);

	replace_tclobj(&cmd_parsers,
			Tcl_ObjGetVar2(interp, l->lit[LIT_CMD_PARSERS], NULL, TCL_GLOBAL_ONLY | TCL_LEAVE_ERR_MSG));
	if (cmd_parsers == NULL) {
		code = TCL_ERROR;
		goto finally;
	}

	if (full)
		replace_tclobj(&linestarts, Tcl_NewListObj(0, NULL));

	scriptnode = domNewElementNode(doc, "script");
	domAppendChild(parent, scriptnode);
	if (full)
		SET_INT_ATTR(scriptnode, "idx", ofs);

	while (base < textend && !done) {
		int			word = 0;
		const int	remain = textend - base;
		const char* commandEnd = NULL;

		code = Tcl_ParseCommand(interp, base, remain, incmdsubst, &parse);
		if (code != TCL_OK) goto finally;

		commandEnd = parse.commandStart + parse.commandSize;

		//fprintf(stderr, "commentStart: %ld, commentSize: %d text: %s\n", parse.commentStart == NULL ? 0 : parse.commentStart-text+ofs, parse.commentSize, text);
		//fprintf(stderr, "commandStart: %ld, commandSize: %d\n", parse.commandStart-text+ofs, parse.commandSize);
		//fprintf(stderr, "numwords: %d, numTokens: %d\n", parse.numWords, parse.numTokens);
		//fprintf(stderr, "commandEnd: %ld\n", commandEnd-text);

		/* scan the chunk of text covered by Tcl_Parse for newlines while
		   it's pretty much guaranteed to be in cache, and add the offsets to
		   linestarts */
		if (full)
			for (j=0; j<parse.commandSize; j++)
				if (base[j] == '\n')
					if (TCL_OK != (code = Tcl_ListObjAppendElement(interp, linestarts, Tcl_NewIntObj(base-text+ofs+j))))
						goto finally;

		if (parse.commentSize > 0) {
			const int			spacelen = parse.commentStart - base;
			const char*	const	commentEnd = parse.commentStart + parse.commentSize;

			if (spacelen > 0)
				EMIT("space", scriptnode, base, spacelen);

			EMIT("comment", scriptnode, parse.commentStart, parse.commentSize);

			if (commentEnd < parse.commandStart)
				EMIT("space", scriptnode, commentEnd, parse.commandStart - commentEnd);
		} else if (parse.commandStart > base) {
			EMIT("space", scriptnode, base, parse.commandStart - base);
		}

		if (parse.numTokens) {
			cmdnode = domNewElementNode(doc, "command");
			if (full) {
				SET_INT_ATTR(cmdnode, "idx", parse.commandStart-text+ofs);
				SET_INT_ATTR(cmdnode, "len", parse.commandSize);
			}
			domAppendChild(scriptnode, cmdnode);
		}

		last_wordend = parse.commandStart;

		for (t=0; t<parse.numTokens;) {
			const Tcl_Token*	token = &parse.tokenPtr[t];
			const int			spacelen = token->start - last_wordend;
			//const char*			wordend = token->start + token->size;
			int					dynamic=0, raw=0, expand=0;
			Tcl_DString			value;

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
				SET_INT_ATTR(wordnode, "idx", token->start -text +ofs);
				SET_INT_ATTR(wordnode, "len", token->size);
			}
			domAppendChild(cmdnode, wordnode);

			Tcl_DStringInit(&value);

			expand = 0;
			switch (token->type) {
				case TCL_TOKEN_EXPAND_WORD:
					SET_CONST_ATTR(wordnode, "expand", "");
					expand = 1;
					// Falls through
				case TCL_TOKEN_WORD:
				case TCL_TOKEN_SIMPLE_WORD:
					break;

				default:
					Tcl_SetObjResult(interp, Tcl_ObjPrintf("Unexpected token type: %d %s",
								token->type, toktype_string(token->type)));
					code = TCL_ERROR;
					goto finally;
			}

			{
				const int	syntax_len = parse.tokenPtr[t+1].start - token->start;
				const char	c = token->start[expand*3];

				if (syntax_len) {
					//EMIT("syntax", wordnode, token->start, syntax_len);
					switch (c) {
						case '"': SET_CONST_ATTR(wordnode, "quoted", "quote"); break;
						case '{': SET_CONST_ATTR(wordnode, "quoted", "brace"); raw = 1; break;
					}
				} else {
					//SET_CONST_ATTR(wordnode, "quoted", "none");
				}
			}

			if (token->numComponents) {
				code = append_sub_tokens(
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
						lineofs,
						full);

				if (code != TCL_OK) goto finally;
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
				const char*	valuestr = Tcl_DStringValue(&value);
				const int	valuelen = Tcl_DStringLength(&value);
				domSetAttributeEx(wordnode, "value", sizeof("value")-1, valuestr, valuelen);

				if (word == 1) {
					Tcl_Obj*	cmdname = NULL;
					Tcl_Obj*	lambda = NULL;

					// If this is the first word, record the value as the name of this command
					domSetAttributeEx(cmdnode, "name", sizeof("name")-1, valuestr, valuelen);

					// Look up a command parser for this command
					replace_tclobj(&cmdname, Tcl_NewStringObj(valuestr, valuelen));
					if (TCL_OK == Tcl_DictObjGet(NULL, cmd_parsers, cmdname, &lambda)) {
						if (lambda) Tcl_IncrRefCount(lambda);
						replace_tclobj(&cmd_parser, lambda);
					} else {
						//fprintf(stderr, "No cmd_parser found for (%s)\n", Tcl_GetString(cmdname));
						release_tclobj(&cmd_parser);
					}
					release_tclobj(&cmdname);
					release_tclobj(&lambda);
				}
			} else {
				if (word == 1) {
					//fprintf(stderr, "command name word is not static\n");
					release_tclobj(&cmd_parser);
				}
			}
			Tcl_DStringFree(&value);
		}

		//fprintf(stderr, "cmd tail, last_wordend: %ld, commandEnd: %ld\n", last_wordend-text, commandEnd-text);
		if (last_wordend < commandEnd) {
			switch (commandEnd[-1]) {
				case ']':
					if (incmdsubst) done = 1;
					// Falls through
				case '\n':
				case ';':
					if (1 && cmdnode) {
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

			SET_INT_ATTR(node, "idx", commandEnd-text);
			SET_INT_ATTR(node, "len", 0);
			domAppendChild(cmdnode, node);
			*/
		}

		Tcl_FreeParse(&parse);

		// Attempt to deep parse this command
		if (cmd_parser && parse.numTokens) {
			Tcl_Obj*	cmd = NULL;
			char		nodecmd[80];

			tcldom_createNodeObj(interp, cmdnode, (char*)&nodecmd);
			//fprintf(stderr, "Created dom command: (%s) for cmdnode: %p\n", nodecmd, cmdnode);
			replace_tclobj(&cmd, Tcl_NewListObj(0, NULL));
			code = Tcl_ListObjAppendElement(interp, cmd, l->lit[LIT_APPLY]);
			if (code == TCL_OK) code = Tcl_ListObjAppendElement(interp, cmd, cmd_parser);
			if (code == TCL_OK) code = Tcl_ListObjAppendElement(interp, cmd, Tcl_NewStringObj(nodecmd, -1));
			if (code == TCL_OK) code = Tcl_EvalObjEx(interp, cmd, TCL_EVAL_DIRECT | TCL_EVAL_GLOBAL);
			if (code != TCL_OK) {
				Tcl_Obj*	getXML = NULL;
				Tcl_Obj*	res = NULL;
				Tcl_Obj*	options = NULL;
				int			oldcode = code;

				code = TCL_OK;

				replace_tclobj(&options, Tcl_GetReturnOptions(interp, code));
				replace_tclobj(&getXML, Tcl_NewListObj(0, NULL));
				replace_tclobj(&res, Tcl_GetObjResult(interp));

				if (code == TCL_OK) code = Tcl_ListObjAppendElement(interp, getXML, Tcl_NewStringObj("domNode", -1));
				if (code == TCL_OK) code = Tcl_ListObjAppendElement(interp, getXML, Tcl_NewStringObj(nodecmd, -1));
				if (code == TCL_OK) code = Tcl_ListObjAppendElement(interp, getXML, Tcl_NewStringObj("asXML", -1));
				if (code == TCL_OK) code = Tcl_EvalObjEx(interp, getXML, TCL_EVAL_DIRECT | TCL_EVAL_GLOBAL);
				if (code == TCL_OK) {
					const char*	asXML = Tcl_GetString(Tcl_GetObjResult(interp));
					const char*	cmdname = NULL;

					if (TCL_OK == get_attr(NULL, cmdnode, "name", &cmdname)) {
						//fprintf(stderr, "Could not parse command (%s): %s\n%s\n", cmdname, Tcl_GetString(cmd), asXML);
						fprintf(stderr, "Could not parse command (%s): %s\n", cmdname, Tcl_GetString(cmd));
					} else {
						//fprintf(stderr, "Could not parse command: %s\n%s\n", Tcl_GetString(cmd), asXML);
						fprintf(stderr, "Could not parse command: %s\n", Tcl_GetString(cmd));
					}
				}
				Tcl_SetReturnOptions(interp, options);
				Tcl_SetObjResult(interp, res);
				release_tclobj(&getXML);
				release_tclobj(&res);
				release_tclobj(&options);
				code = oldcode;
				goto finally;
			}
			release_tclobj(&cmd);
			Tcl_ResetResult(interp);
		}

		base = parse.commandStart + parse.commandSize;
	}

	if (end != NULL)
		*end = base;

	if (full) {
		int linestarts_len;
		const char*	linestarts_str = Tcl_GetStringFromObj(linestarts, &linestarts_len);

		SET_INT_ATTR(scriptnode, "len",     textlen);
		SET_INT_ATTR(scriptnode, "lineofs", lineofs);
		domSetAttributeEx(scriptnode, "linestarts", sizeof("linestarts")-1, linestarts_str, linestarts_len);
	}

finally:
	release_tclobj(&linestarts);
	release_tclobj(&cmd_parsers);
	release_tclobj(&cmd_parser);

	return code;
}

//}}}
static int subparse_expr( //{{{
		Tcl_Interp*				interp,
		struct pidata*			l,
		domNode*				parent,
		const char* restrict	text,
		const int				textlen,
		const int				ofs,
		const int				lineofs,
		const int				incmdsubst)
{
	domDocument*		doc = parent->ownerDocument;
	domNode*			exprnode = NULL;
	int					code = TCL_OK;
	Tcl_Parse			parse;
	int					t;
	Tcl_Obj*			linestarts = NULL;
	const char*			last_wordend = NULL;
	Tcl_Obj*			cmd_parsers = NULL;
	Tcl_Obj*			cmd_parser = NULL;	// lambda in ::parsetcl::cmd_parsers dict for this command
	const char*			base = text;
	const char*	const	textend = text + textlen;
	const int			remain = textend - base;
	const int			full = strcmp(doc->documentElement->nodeName, "tcl") == 0;

	//fprintf(stderr, "subparse_expr (%s), ofs: %d\n", text, ofs);

	exprnode = domNewElementNode(doc, "expr");
	domAppendChild(parent, exprnode);
	SET_INT_ATTR(exprnode, "idx", ofs);

	code = Tcl_ParseExpr(interp, base, remain, &parse);
	if (code != TCL_OK) goto finally;

	last_wordend = base;

	for (t=0; t<parse.numTokens;) {
		const Tcl_Token*	token = &parse.tokenPtr[t];
		const int			spacelen = token->start - last_wordend;
		int					dynamic=0;

		/*
		fprintf(stderr, "token, t: %d, last_wordend-text: %ld\n", t, last_wordend-text);
		fprintf(stderr, "\ttoken %d: type: %s(%d), start: %ld, length %d, numComponents: %d\n",
				t, toktype_string(token->type), token->type, token->start-text+ofs,
				token->size, token->numComponents);
				*/

		if (spacelen)
			EMIT("space", exprnode, last_wordend, spacelen);

		last_wordend = token->start + token->size;

		if (token->type != TCL_TOKEN_SUB_EXPR) {
			Tcl_SetObjResult(interp, Tcl_ObjPrintf("Unexpected token type: %d %s",
						token->type, toktype_string(token->type)));
			code = TCL_ERROR;
			goto finally;
		}

		if (token->numComponents) {
			code = append_sub_tokens(
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
					lineofs,
					full);

			if (code != TCL_OK) goto finally;
			t += 1 + token->numComponents;
		}
	}

	if (last_wordend < text+textlen)
		EMIT("space", exprnode, last_wordend, text+textlen-last_wordend);

	Tcl_FreeParse(&parse);

finally:
	release_tclobj(&linestarts);
	release_tclobj(&cmd_parsers);
	release_tclobj(&cmd_parser);

	return code;
}

//}}}
static int min(const int a, const int b) //{{{
{
	if (a < b) {
		return a;
	} else {
		return b;
	}
}

//}}}
static int parse_hex(const char* text, const int maxchars) //{{{
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

//}}}
static int parse_octal(const char* text, const int maxchars) //{{{
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

//}}}
static int parse_combined(Tcl_Interp* interp, struct pidata* l, const int braced, domNode* parent, const char* text, const int textlen, int *parent_i, const int ofs, const int lineofs) //{{{
{
	domDocument*	doc = parent->ownerDocument;
	domNode*		wordnode = domNewElementNode(doc, "word");
	const int		full = strcmp(doc->documentElement->nodeName, "tcl") == 0;
	const char*		end = NULL;
	int				dynamic = 0;
	int				code = TCL_OK;
	int				i = *parent_i;
	int				t;
	Tcl_Parse		parse;
	Tcl_DString		value;

	Tcl_DStringInit(&value);

	domAppendChild(parent, wordnode);
	EMIT("syntax", wordnode, text+i, 1);

	if (braced) {
		SET_CONST_ATTR(wordnode, "quoted", "brace");
		code = Tcl_ParseBraces(interp, text+i, textlen-i, &parse, 0, &end);
	} else {
		SET_CONST_ATTR(wordnode, "quoted", "quote");
		code = Tcl_ParseQuotedString(interp, text+i, textlen-i, &parse, 0, &end);
	}
	if (code != TCL_OK) goto finally;

	if (parse.numTokens) {
		code = append_sub_tokens(
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
				lineofs,
				full);

		if (code != TCL_OK) goto finally;
		t += 1 + parse.numTokens;
	}

	if (full) {
		const int idx = i+ofs;
		SET_INT_ATTR(wordnode, "idx", idx);
		SET_INT_ATTR(wordnode, "len", end - (text+i));
	}

	i = end-text;
	EMIT("syntax", wordnode, end-1, 1);

	SET_VALUE_ATTRIB(wordnode, &value);

finally:
	Tcl_FreeParse(&parse);
	Tcl_DStringFree(&value);

	*parent_i = i;
	return code;
}

//}}}
static int escape_value(Tcl_Interp* interp, const char* text, const int len, Tcl_DString* value) //{{{
{
	int				code = TCL_OK;

	if (len == 1) {
		Tcl_DStringAppend(value, text, 1);
	} else {
		Tcl_Obj*	raw = NULL;
		Tcl_Obj*	escape = NULL;

		replace_tclobj(&raw, Tcl_NewStringObj(text, len));
		replace_tclobj(&escape, Tcl_SubstObj(interp, raw, TCL_SUBST_BACKSLASHES));
		if (escape) {
			int len;
			const char* bytes;

			bytes = Tcl_GetStringFromObj(escape, &len);
			Tcl_DStringAppend(value, bytes, len);
		} else {
			code = TCL_ERROR;
		}
		release_tclobj(&escape);
		release_tclobj(&raw);
		if (code != TCL_OK) goto finally;
	}

finally:
	return code;
}

//}}}
static int subparse_list( //{{{
		Tcl_Interp*				interp,
		struct pidata*			l,
		domNode*				parent,
		const char* restrict	text,
		const int				textlen,
		const int				ofs,
		const int				lineofs,
		const int				incmdsubst)
{
	domDocument*		doc = parent->ownerDocument;
	domNode*			listnode = NULL;
	int					code = TCL_OK;
	int					i = 0;
	const int			full = strcmp(doc->documentElement->nodeName, "tcl") == 0;
	int					value_used = 0;
	Tcl_DString			value;

	//fprintf(stderr, "subparse_expr (%s), ofs: %d\n", text, ofs);

	listnode = domNewElementNode(doc, "list");
	domAppendChild(parent, listnode);
	if (full)
		SET_INT_ATTR(listnode, "idx", ofs);

	while (i < textlen) { // Each word
		const int	start = i;

		while (i < textlen) {
			switch (text[i]) {
				case '\t':
				case '\n':
				case '\v':
				case '\f':
				case '\r':
				case ' ':
					i++;
					continue;
			}
			break;
		}
		if (i > start)
			EMIT("space", listnode, text+start, i-start);

		if (i >= textlen) goto finally;

		switch (text[i]) {
			case '{':
				code = parse_combined(interp, l, 1, listnode, text, textlen, &i, ofs, lineofs);
				if (code != TCL_OK) goto finally;
				break;

			case '"':
				code = parse_combined(interp, l, 0, listnode, text, textlen, &i, ofs, lineofs);
				if (code != TCL_OK) goto finally;
				break;

			default:
				{
					domNode*	wordnode = domNewElementNode(doc, "word");
					const int	wordstart = i;
					int			chunkstart = wordstart;

					Tcl_DStringInit(&value);
					value_used = 1;

					domAppendChild(listnode, wordnode);

					while (i < textlen) {
						switch (text[i]) {
							case '\t':
							case '\n':
							case '\v':
							case '\f':
							case '\r':
							case ' ':
								break;

							case '\\':
								if (i == textlen-1) {
									// Trailing backslash as the last char before EOF, in bare word mode this is just itself
									i++;
									continue;
								}

								if (i > chunkstart) {
									Tcl_DStringAppend(&value, text+chunkstart, i-chunkstart);
									EMIT("text", wordnode, text+chunkstart, i-chunkstart);
								}

								i++;
								switch (text[i]) {
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
											const int consumed = parse_hex(text+i+1, textlen-i-1);
											code = escape_value(interp, text+i-1, consumed+2, &value);
											if (code != TCL_OK) goto finally;
											EMIT("escape", wordnode, text+i-1, consumed+2);
											i += consumed+1;
										}
										break;

									case 'u':
										{
											const int consumed = parse_hex(text+i+1, min(4, textlen-i-1));
											code = escape_value(interp, text+i-1, consumed+2, &value);
											EMIT("escape", wordnode, text+i-1, consumed+2);
											if (code != TCL_OK) goto finally;
											i += consumed+1;
										}
										break;

									default:
										{
											const int consumed = parse_octal(text+i+1, min(3, textlen-i-1));
											code = escape_value(interp, text+i-1, consumed+1, &value);
											EMIT("escape", wordnode, text+i-1, consumed+1);
											if (code != TCL_OK) goto finally;
											i += consumed;
										}
										break;
								}
								i++;
								chunkstart = i;
								continue;

							default:
								i++;
								continue;
						}
						break;
					}

					if (i > chunkstart) {
						Tcl_DStringAppend(&value, text+chunkstart, i-chunkstart);
						EMIT("text", wordnode, text+chunkstart, i-chunkstart);
					}


					if (full) {
						SET_INT_ATTR(wordnode, "idx", wordstart+ofs);
						SET_INT_ATTR(wordnode, "len", i-wordstart);
					}

					SET_VALUE_ATTRIB(wordnode, &value);
				}
				break;
		}
	}

finally:
	if (value_used) {
		Tcl_DStringFree(&value);
		value_used = 0;
	}

	return code;
}

//}}}
static int subparse_subst( //{{{
		Tcl_Interp*				interp,
		struct pidata*			l,
		const int				flags,		// some combination of TCL_SUBST_COMMANDS, TCL_SUBST_VARIABLES, TCL_SUBST_BACKSLASHES
		domNode*				parent,
		const char* restrict	text,
		const int				textlen,
		const int				ofs,
		const int				lineofs,
		const int				incmdsubst)
{
	domDocument*		doc = parent->ownerDocument;
	domNode*			substnode = NULL;
	int					code = TCL_OK;
	int					i = 0;
	int					start = i;
	const int			full = strcmp(doc->documentElement->nodeName, "tcl") == 0;
	int					parse_used=0, dynamic=0;
	Tcl_Parse			parse;
	Tcl_DString			value;

	//fprintf(stderr, "subparse_expr (%s), ofs: %d\n", text, ofs);

	Tcl_DStringInit(&value);

	substnode = domNewElementNode(doc, "subst");
	domAppendChild(parent, substnode);
	if (full)
		SET_INT_ATTR(substnode, "idx", ofs);

	if (!(flags & TCL_SUBST_VARIABLES))
		SET_CONST_ATTR(substnode, "novariables", "");
	if (!(flags & TCL_SUBST_COMMANDS))
		SET_CONST_ATTR(substnode, "nocommands", "");
	if (!(flags & TCL_SUBST_BACKSLASHES))
		SET_CONST_ATTR(substnode, "nobackslashes", "");

	while (i < textlen) {
		switch (text[i]) {
			case '$':
				if (flags & TCL_SUBST_VARIABLES) {
					dynamic = 1;

					if (i > start)
						EMIT("text", substnode, text+start, i-start);

					if (parse_used) Tcl_FreeParse(&parse);
					code = Tcl_ParseVarName(interp, text+i, textlen-i, &parse, 0);
					parse_used = 1;
					if (code != TCL_OK) goto finally;

					if (parse.numTokens) {
						code = append_sub_tokens(
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
								lineofs,
								full);

						if (code != TCL_OK) goto finally;
						start = i = parse.tokenPtr[0].start + parse.tokenPtr[0].size - text;
					}
				} else {
					i++;
				}
				break;

			case '[':
				if (flags & TCL_SUBST_COMMANDS) {
					const char*	end = NULL;

					dynamic = 1;

					if (i > start)
						EMIT("text", substnode, text+start, i-start);

					EMIT("syntax", substnode, text+i, 1);
					i++;

					code = subparse_script(
							interp,
							l,
							substnode,
							text+i,
							textlen-i,
							ofs+i,
							lineofs,
							1,
							&end);

					if (code != TCL_OK) goto finally;

					start = i = end - text;
				} else {
					i++;
				}
				break;

			case '\\':
				if (!(flags & TCL_SUBST_BACKSLASHES)) {
					i++;
					break;
				}
				if (i > start) {
					EMIT("text", substnode, text+start, i-start);
					if (!dynamic)
						Tcl_DStringAppend(&value, text+start, i-start);
					start = i;
				}

				if (i == textlen-1) {
					// Trailing backslash as the last char before EOF, in bare word mode this is just itself
					i++;
					continue;
				}

#define LETTER_ESCAPE(c) \
	do { \
		Tcl_DStringAppend(&value, c, 1); \
		EMIT("escape", substnode, text+i-1, 2); \
		i++; \
		start = i; \
	} while(0);

				i++;
				switch (text[i]) {
					case 'a': LETTER_ESCAPE("\a"); break;
					case 'b': LETTER_ESCAPE("\b"); break;
					case 'f': LETTER_ESCAPE("\f"); break;
					case 'n': LETTER_ESCAPE("\n"); break;
					case 'r': LETTER_ESCAPE("\r"); break;
					case 't': LETTER_ESCAPE("\t"); break;
					case 'v': LETTER_ESCAPE("\v"); break;

					case '\n':
						// Line folding: \\\n[ \t]+ seq is equivalent to a single space
						if (i > start) {
							EMIT("text", substnode, text+start, i-start);
							EMIT("text", substnode," ", 1);
							if (!dynamic) {
								Tcl_DStringAppend(&value, text+start, i-start);
								Tcl_DStringAppend(&value, " ", 1);
							}
						}
						i++;
						while (i < textlen) {
							switch (text[i]) {
								case ' ':
								case '\t':
									i++;
									continue;
							}
							break;
						}
						start = i;
						continue;

#define CODEPOINT_ESCAPE(f, digits_start, max_digits) \
	do { \
		const int consumed = (f)(text+digits_start, min(max_digits, textlen-i-1)); \
		const int escapelen = (digits_start)+consumed-(i-1); \
		const char* escapestart = text+i-1; \
		if (consumed == 0) { /* Not a valid codepoint escape sequence, the value is just the backquoted character */ \
			EMIT("escape", substnode, text+i-1, 2); \
			if (!dynamic) Tcl_DStringAppend(&value, text+i, 1); \
			i++; \
		} else { \
			EMIT("escape", substnode, escapestart, escapelen); \
			if (!dynamic) { \
				code = escape_value(interp, escapestart, escapelen, &value); \
				if (code != TCL_OK) goto finally; \
			} \
			i = (digits_start) + consumed; \
		} \
		start = i; \
	} while(0);
					case 'x': CODEPOINT_ESCAPE(parse_hex,   i+1, 2); break;
					case 'u': CODEPOINT_ESCAPE(parse_hex,   i+1, 4); break;
					default:  CODEPOINT_ESCAPE(parse_octal, i,   3); break;
				}
				break;

			default:
				i++;
				break;
		}
	}

	if (i > start) {
		EMIT("text", substnode, text+start, i-start);
		if (!dynamic)
			Tcl_DStringAppend(&value, text+start, i-start);
	}

	if (!dynamic)
		SET_VALUE_ATTRIB(substnode, &value);

finally:
	if (parse_used) {
		Tcl_FreeParse(&parse);
		parse_used = 0;
	}

	Tcl_DStringFree(&value);

	return code;
}

//}}}
static int subparse(ClientData cdata, Tcl_Interp* interp, int objc, Tcl_Obj* const objv[]) //{{{
{
	struct pidata*	l = cdata;
	int		code = TCL_OK;
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
	};
	int	mode;
	domDocument*	doc = NULL;
	domNode*		wordnode = NULL;
	domNode*		node = NULL;
	domNode*		asnode = NULL;
	char*			errmsg = NULL;
	const char*		text = NULL;
	const char*		idx  = NULL;
	int				ofs, scanned, full;

	if (objc < 3) {
		Tcl_WrongNumArgs(interp, 1, objv, "mode word");
		code = TCL_ERROR;
		goto finally;
	}

	code = Tcl_GetIndexFromObj(interp, objv[1], modes, "mode", TCL_EXACT, &mode);
	if (code != TCL_OK) goto finally;

	//fprintf(stderr, "subparse %s %s\n", modes[mode], Tcl_GetString(objv[2]));
	wordnode = tcldom_getNodeFromName(interp, Tcl_GetString(objv[2]), &errmsg);
	if (wordnode == NULL) {
		Tcl_SetObjResult(interp, errmsg ?
				Tcl_ObjPrintf("tDOM error getting node: \"%s\"", errmsg) :
				Tcl_ObjPrintf("Could not retrieve node from \"%s\"", Tcl_GetString(objv[2])));
		code = TCL_ERROR;
		goto finally;
	}

	doc = wordnode->ownerDocument;
	full = strcmp(doc->documentElement->nodeName, "tcl") == 0;

	node = wordnode->firstChild;
	for (node = wordnode->firstChild; node; node = node->nextSibling) {
		if (node->nodeType != ELEMENT_NODE) continue;
		if (strcmp("as", node->nodeName) != 0) continue;
		asnode = node;
		break;
	}
	if (asnode == NULL) {
		asnode = domNewElementNode(doc, "as");
		domAppendChild(wordnode, asnode);
	}

	// Extract text from word {{{
	//code = get_attr(interp, wordnode, "value", &text);
	//if (code != TCL_OK) goto finally;
	code = get_attr(NULL, wordnode, "value", &text);
	if (code != TCL_OK) {
		// TODO: warn about this (word to subparse doesn't have a static literal value)
		code = TCL_OK;
		goto finally;
	}

	if (full) {
		const char* quoted = NULL;
		code = get_attr(interp, wordnode, "idx",   &idx);
		if (code != TCL_OK) goto finally;
		sscanf(idx, "%d%n", &ofs, &scanned);
		if (scanned < strlen(idx)) {
			Tcl_SetObjResult(interp, Tcl_ObjPrintf("Invalid idx value: \"%s\", must be an integer, scanned: %d, strlen(idx): %ld", idx, scanned, strlen(idx)));
			code = TCL_ERROR;
			goto finally;
		}

		code = get_attr(NULL, wordnode, "quoted",  &quoted);
		if (code == TCL_OK) {
			// Word being parsed is quoted, adjust the offset for the quote char
			ofs++;
		}
	} else {
		ofs = 0;
	}
	// Extract text from word }}}

	switch (mode) {
		case MODE_SCRIPT:
			if (objc != 3) {
				Tcl_WrongNumArgs(interp, 1, objv, "mode word");
				code = TCL_ERROR;
				goto finally;
			}

			code = subparse_script(
					interp,
					l,
					asnode,
					text,
					strlen(text),
					ofs,
					0,
					0,
					NULL);

			if (code != TCL_OK) {
				Tcl_SetObjResult(interp, Tcl_ObjPrintf("Error parsing \"%s\" as Tcl script: %s", text, Tcl_GetString(Tcl_GetObjResult(interp))));
				goto finally;
			}
			break;

		case MODE_EXPR:
			if (objc != 3) {
				Tcl_WrongNumArgs(interp, 1, objv, "mode word");
				code = TCL_ERROR;
				goto finally;
			}

			code = subparse_expr(
					interp,
					l,
					asnode,
					text,
					strlen(text),
					ofs,
					0,
					0);

			if (code != TCL_OK) goto finally;
			break;

		case MODE_LIST:
			if (objc != 3) {
				Tcl_WrongNumArgs(interp, 1, objv, "mode word");
				code = TCL_ERROR;
				goto finally;
			}

			code = subparse_list(
					interp,
					l,
					asnode,
					text,
					strlen(text),
					ofs,
					0,
					0);

			if (code != TCL_OK) goto finally;
			break;

		case MODE_SUBST:
			{
				static const char* switches[] = {
					"-nocommands",
					"-novariables",
					"-nobackslashes",
					NULL
				};
				enum {
					NO_COMMANDS,
					NO_VARIABLES,
					NO_BACKSLASHES
				};
				int sw;
				int i;
				int flags = TCL_SUBST_ALL;

				for (i=3; i<objc; i++) {
					code = Tcl_GetIndexFromObj(interp, objv[i], switches, "switch", TCL_EXACT, &sw);
					if (code != TCL_OK) goto finally;

					switch (sw) {
						case NO_COMMANDS:		flags &= ~TCL_SUBST_COMMANDS;		break;
						case NO_VARIABLES:		flags &= ~TCL_SUBST_VARIABLES;		break;
						case NO_BACKSLASHES:	flags &= ~TCL_SUBST_BACKSLASHES;	break;
						default:
							Tcl_SetObjResult(interp, Tcl_ObjPrintf("Unhandled switch case: %d", sw));
							code = TCL_ERROR;
							goto finally;
					}
				}
				
				code = subparse_subst(
						interp,
						l,
						flags,
						asnode,
						text,
						strlen(text),
						ofs,
						0,
						0);

				if (code != TCL_OK) goto finally;
			}
			break;

		case MODE_SQL:
		case MODE_JSON:
		case MODE_JAVASCRIPT:
			{
				domNode*	typenode = domNewElementNode(doc, modes[mode]);

				domAppendNewTextNode(typenode, text, strlen(text), TEXT_NODE, 0);
				if (full) {
					SET_INT_ATTR(typenode, "idx", ofs);
					SET_INT_ATTR(typenode, "len", strlen(text));
				}
				SET_CONST_ATTR(typenode, "unparsed", "");
				domAppendChild(asnode, typenode);
			}
			break;

		default:
			Tcl_SetObjResult(interp, Tcl_ObjPrintf("Unexpected mode index: %d", mode));
			code = TCL_ERROR;
			goto finally;
	}

finally:
	return code;
}

//}}}
static int parse_tcl_script(Tcl_Interp* interp, struct pidata* l, Tcl_Obj* script, domDocument** res, const int full) //{{{
{
	domDocument*		doc = NULL;
	domNode*			root = NULL;
	int					textlen;
	const char*			text = NULL;
	int					code = TCL_OK;

	if (full) {
		doc = domCreateDocument(NULL, "tcl");
	} else {
		doc = domCreateDocument(NULL, "ast");
	}
	root = doc->documentElement;

	text = Tcl_GetStringFromObj(script, &textlen);

	//TIME("parse script",
	if (TCL_OK != (code = subparse_script(interp, l, root, text, textlen, 0, 0, 0, NULL)))
		goto finally;
	//);

	if (*res) domFreeDocument(*res, NULL, NULL);
	*res = doc;
	doc = NULL;

finally:
	if (doc) {
		domFreeDocument(doc, NULL, NULL);
		doc = NULL;
	}

	return code;
}

//}}}
static int get_parsetree_from_obj(Tcl_Interp* interp, struct pidata* l, Tcl_Obj* obj, domDocument** doc) //{{{
{
	Tcl_ObjIntRep*		ir = NULL;
	int					code = TCL_OK;

	ir = Tcl_FetchIntRep(obj, &parsetree);
	if (ir == NULL) {
		Tcl_ObjIntRep	newir;

		newir.twoPtrValue.ptr1 = NULL;
		newir.twoPtrValue.ptr2 = NULL;

		if (TCL_OK != (code = parse_tcl_script(interp, l, obj, (domDocument**)&newir.twoPtrValue.ptr1, 1)))
			goto finally;

		Tcl_StoreIntRep(obj, &parsetree, &newir);
		ir = Tcl_FetchIntRep(obj, &parsetree);
	}

	*doc = (domDocument*)ir->twoPtrValue.ptr1;

finally:
	return code;
}

//}}}
static int get_ast_from_obj(Tcl_Interp* interp, struct pidata* l, Tcl_Obj* obj, domDocument** doc) //{{{
{
	Tcl_ObjIntRep*		ir = NULL;
	int					code = TCL_OK;

	ir = Tcl_FetchIntRep(obj, &ast);
	if (ir == NULL) {
		Tcl_ObjIntRep	newir;

		newir.twoPtrValue.ptr1 = NULL;
		newir.twoPtrValue.ptr2 = NULL;

		if (TCL_OK != (code = parse_tcl_script(interp, l, obj, (domDocument**)&newir.twoPtrValue.ptr1, 0)))
			goto finally;

		Tcl_StoreIntRep(obj, &ast, &newir);
		ir = Tcl_FetchIntRep(obj, &ast);
	}

	*doc = (domDocument*)ir->twoPtrValue.ptr1;

finally:
	return code;
}

//}}}
static int get_parsetree(ClientData cdata, Tcl_Interp* interp, int objc, Tcl_Obj *const objv[]) //{{{
{
	int				code = TCL_OK;
	domDocument*	doc = NULL;
	char			nodecmd[80];
	struct pidata*	l = cdata;

	if (objc != 2) {
		Tcl_WrongNumArgs(interp, 1, objv, "script");
		code = TCL_ERROR;
		goto finally;
	}

	if (TCL_OK != (code = get_parsetree_from_obj(interp, l, objv[1], &doc)))
		goto finally;

	tcldom_createNodeObj(interp, doc->documentElement, (char*)&nodecmd);

	Tcl_SetObjResult(interp, Tcl_NewStringObj(nodecmd, -1));

finally:
	return code;
}

//}}}
static int get_ast(ClientData cdata, Tcl_Interp* interp, int objc, Tcl_Obj *const objv[]) //{{{
{
	int				code = TCL_OK;
	domDocument*	doc = NULL;
	char			nodecmd[80];
	struct pidata*	l = cdata;

	if (objc != 2) {
		Tcl_WrongNumArgs(interp, 1, objv, "script");
		code = TCL_ERROR;
		goto finally;
	}

	if (TCL_OK != (code = get_ast_from_obj(interp, l, objv[1], &doc)))
		goto finally;

	tcldom_createNodeObj(interp, doc->documentElement, (char*)&nodecmd);

	Tcl_SetObjResult(interp, Tcl_NewStringObj(nodecmd, -1));

finally:
	return code;
}

//}}}
static void free_pidata(ClientData cdata, Tcl_Interp* interp) //{{{
{
	struct pidata*	l = cdata;
	int				i;

	if (l) {
		for (i=0; i<LIT_END; i++) release_tclobj(&l->lit[i]);
		ckfree(l);
		l = NULL;
	}
}

//}}}

struct cmd {
	char*			name;
	Tcl_ObjCmdProc*	proc;
} cmds[] = {
	{NS "::parsetree",	get_parsetree},
	{NS "::ast",		get_ast},
	{NS "::subparse",	subparse},
	{NULL,				NULL}
};

#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus */
DLLEXPORT int Parsetcl_Init(Tcl_Interp* interp) //{{{
{
	int				code = TCL_OK;
	Tcl_Namespace*	ns = NULL;
	struct pidata*	l = NULL;
	int				i;

#ifdef USE_TCL_STUBS
	if (Tcl_InitStubs(interp, "8.6", 0) == NULL)
		return TCL_ERROR;
#endif // USE_TCL_STUBS

#ifdef USE_TDOM_STUBS
	if (Tdom_InitStubs(interp, "0.9.2", 0) == NULL)
		return TCL_ERROR;
#endif

	l = ckalloc(sizeof(*l));
	memset(l, 0, sizeof(*l));

	for (i=0; i<LIT_END; i++)
		replace_tclobj(&l->lit[i], Tcl_NewStringObj(lit_strings[i], -1));

	Tcl_SetAssocData(interp, "parsetcl", free_pidata, l);

	ns = Tcl_CreateNamespace(interp, NS, NULL, NULL);
	code = Tcl_Export(interp, ns, "*", 0);
	if (code != TCL_OK) goto finally;

	{
		struct cmd*	c = cmds;

		Tcl_CreateEnsemble(interp, NS, ns, 0);

		while (c->name != NULL) {
			if (NULL == Tcl_CreateObjCommand(interp, c->name, c->proc, l, NULL)) {
				Tcl_SetObjResult(interp, Tcl_ObjPrintf("Could not create command %s", c->name));
				code = TCL_ERROR;
				goto finally;
			}
			c++;
		}
	}

	code = Tcl_PkgProvide(interp, PACKAGE_NAME, PACKAGE_VERSION);

finally:
	return code;
}

//}}}
#ifdef __cplusplus
}
#endif  /* __cplusplus */

/* Local Variables: */
/* tab-width: 4 */
/* c-basic-offset: 4 */
/* End: */
// vim: foldmethod=marker foldmarker={{{,}}} ts=4 shiftwidth=4
