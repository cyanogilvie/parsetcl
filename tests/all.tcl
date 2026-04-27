# all.tcl --
#
# Top-level script that runs all *.test files in this directory under tcltest.
# Sourced by the meson `tcltest` runner via teabase's test_preamble.tcl.

package require tcltest
namespace import ::tcltest::*

singleProcess 1
configure {*}$argv -testdir [file dirname [info script]]
set failed [runAllTests]

# Signal failure via TCL_ERROR instead of [exit $code] so meson's test
# runner sees a non-zero exit code without leaking temps on the
# Tcl_FSEvalFileEx stack.  See tdom/tests/all.tcl for the full rationale.
if {$failed} {
    puts stderr "[file tail [info script]]: $failed test(s) failed"
    close stderr
    error "test run failed"
}
