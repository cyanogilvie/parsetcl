#include "tclstuff.h"
#include <tdom.h>

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
