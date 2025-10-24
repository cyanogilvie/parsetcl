echo 'Loading autoload parsetcl.vim'
if !has('tcl') || exists('g:autoloaded_parsetcl')
	finish
endif

let s:path = resolve(expand('<sfile>:p'))
tcl source [file join [file dirname [file normalize [vim::expr s:path]]] parsetcl.tcl]
function parsetcl#ParseTcl()
	tcl ::parsetcl::vim_highlight
endfunction

let g:autoloaded_parsetcl = 1
