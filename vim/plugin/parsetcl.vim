if exists('g:loaded_parsetcl') || !has('tcl') || &cp || version < 700
	finish
endif

command! ParseTcl call parsetcl#ParseTcl()

nnoremap <Leader>p	:ParseTcl<CR>

let g:loaded_parsetcl = 1
