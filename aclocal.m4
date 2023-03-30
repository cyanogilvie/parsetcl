#
# Include the TEA standard macro set
#

builtin(include,tclconfig/tcl.m4)

#
# Add here whatever m4 macros you want to define for your package
#

builtin(include,teabase/teabase.m4)

AC_DEFUN([TDOM_STUBS], [
	if test "${STUBS_BUILD}" = "1"; then
		AC_DEFINE(USE_TDOM_STUBS, 1, [Use TDOM Stubs])
	fi
])
