#include "tclstuff.h"
#include <tdom.h>
#include <tcldom.h>

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


static void free_parsetree(Tcl_Obj* obj) //{{{
{
	Tcl_ObjIntRep*	ir = NULL;

	ir = Tcl_FetchIntRep(obj, &parsetree);
	if (ir && ir->twoPtrValue.ptr1) {
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

static int parse_tcl_script(Tcl_Interp* interp, Tcl_Obj* script, domDocument** res) //{{{
{
	domDocument*		doc = domCreateDocument(NULL, "tcl");
	domNode*			node = NULL;
	int					code = TCL_OK;

	node = doc->documentElement;

	// TODO: Parse obj as Tcl, populate parsetree doc

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
static int get_parsetree_from_obj(Tcl_Interp* interp, Tcl_Obj* obj, domDocument** doc) //{{{
{
	Tcl_ObjIntRep*		ir = NULL;
	int					code = TCL_OK;

	ir = Tcl_FetchIntRep(obj, &parsetree);
	if (ir == NULL) {
		Tcl_ObjIntRep	newir;

		if (TCL_OK != (code = parse_tcl_script(interp, obj, (domDocument**)&newir.twoPtrValue.ptr1)))
			goto finally;

		Tcl_StoreIntRep(obj, &parsetree, &newir);
		ir = Tcl_FetchIntRep(obj, &parsetree);
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
	//Tcl_Obj*		res = NULL;
	//Tcl_ObjIntRep	ir;
	char			nodecmd[80];

	if (objc != 2) {
		Tcl_WrongNumArgs(interp, 1, objv, "script");
		code = TCL_ERROR;
		goto finally;
	}

	if (TCL_OK != (code = get_parsetree_from_obj(interp, objv[1], &doc)))
		goto finally;

	tcldom_createNodeObj(interp, doc->documentElement, (char*)&nodecmd);
	/*
	res = Tcl_NewObj();
	ir.otherValuePtr = doc->documentElement;
	Tcl_StoreIntRep(res, &tdomNodeType, &ir);
	Tcl_SetObjResult(interp, res);
	*/

	Tcl_SetObjResult(interp, Tcl_NewStringObj(nodecmd, -1));

finally:
	return code;
}

//}}}

#define NS "::parsetcl"

struct cmd {
	char*			name;
	Tcl_ObjCmdProc*	proc;
} cmds[] = {
	{NS "::parsetree",	get_parsetree},
	{NULL,				NULL}
};

#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus */
DLLEXPORT int Parsetcl_Init(Tcl_Interp* interp) //{{{
{
	int				code = TCL_OK;
	Tcl_Namespace*	ns = NULL;

#ifdef USE_TCL_STUBS
	if (Tcl_InitStubs(interp, "8.6", 0) == NULL)
		return TCL_ERROR;
#endif // USE_TCL_STUBS

	domModuleInitialize();		// Don't know if this is required

	ns = Tcl_CreateNamespace(interp, NS, NULL, NULL);
	code = Tcl_Export(interp, ns, "*", 0);
	if (code != TCL_OK) goto finally;

	{
		struct cmd*	c = cmds;

		Tcl_CreateEnsemble(interp, NS, ns, 0);

		while (c->name != NULL) {
			if (NULL == Tcl_CreateObjCommand(interp, c->name, c->proc, NULL, NULL)) {
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
