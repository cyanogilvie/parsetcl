#include "tclstuff.h"
#include <tdom.h>

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
	if (ir != NULL) {
		// TODO: release the ref on the parsetree dom
	}
}

//}}}
static void dup_parsetree(Tcl_Obj* src, Tcl_Obj* dest) //{{{
{
	Tcl_ObjIntRep*		srcir = NULL;
	Tcl_ObjIntRep		destir;

	srcir = Tcl_FetchIntRep(src, &parsetree);
	if (srcir == NULL)
		Tcl_Panic("dup_internal_rep asked to duplicate for type, but that type wasn't available on the src object");

	destir.twoPtrValue.ptr1 = srcir->twoPtrValue.ptr1;
	destir.twoPtrValue.ptr2 = NULL;

	// TODO: take a ref on the parsetree dom for the new copy of ptr1

	Tcl_StoreIntRep(dest, &parsetree, &destir);
}

//}}}
static void update_string_rep_parsetree(Tcl_Obj* obj) //{{{
{
	Tcl_ObjIntRep*				ir = Tcl_FetchIntRep(obj, &parsetree);

	if (ir == NULL)
		Tcl_Panic("dup_internal_rep asked to duplicate for type, but that type wasn't available on the src object");

	// TODO: reconstitute the script fragment from the parsetree fragment $obj
}

//}}}

#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus */
DLLEXPORT int Parsetcl_Init(Tcl_Interp* interp) //{{{
{
#ifdef USE_TCL_STUBS
	if (Tcl_InitStubs(interp, "8.6", 0) == NULL)
		return TCL_ERROR;
#endif // USE_TCL_STUBS

	TEST_OK(Tcl_PkgProvide(interp, PACKAGE_NAME, PACKAGE_VERSION));

	return TCL_OK;
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
