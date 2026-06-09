if exists("b:current_syntax")
	finish
endif

" One-time global setup (highlight groups, prop types, callback functions)
if !exists("g:parsetcl_loaded")
	hi def parsetclSyntax ctermfg=grey

	hi def link parsetclCommand		Statement
	hi def link parsetclCommandEnd	parsetclSyntax
	hi def link parsetclEscape		Special
	hi def link	parsetclComment		Comment
	hi def link	parsetclOperator	Operator
	hi def link	parsetclString		String
	hi def link	parsetclVariable	Identifier

	call prop_type_add("parsetclSyntax",		{"highlight": "parsetclSyntax"})
	call prop_type_add("parsetclCommand",		{"highlight": "parsetclCommand",	"start_incl": 1, "end_incl": 1})
	call prop_type_add("parsetclCommandEnd",	{"highlight": "parsetclCommandEnd"})
	call prop_type_add("parsetclEscape",		{"highlight": "parsetclEscape"})
	call prop_type_add("parsetclComment",		{"highlight": "parsetclComment",	"end_incl": 1})
	call prop_type_add("parsetclOperator",		{"highlight": "parsetclOperator"})
	call prop_type_add("parsetclString",		{"highlight": "parsetclString",		"start_incl": 1, "end_incl": 1})
	call prop_type_add("parsetclVariable",		{"highlight": "parsetclVariable",	"start_incl": 1, "end_incl": 1})

	let g:parsetcl_path = resolve(expand('<sfile>:p:h'))

	func ParsetclHandleJobMsg(channel, msg)
		echo "Got ParsetclHandleJobMsg: ".a:msg
	endfunc

	" Per-buffer listener: dispatch changes to that buffer's own job.
	func ParsetclListener(bufnr, start, end, added, changes)
		let l:job = getbufvar(a:bufnr, "parsetcl_job", v:null)
		if l:job is v:null
			return
		endif
		let l:changes = []
		for c in a:changes
			" Per :help listener_add, lines c.lnum..c.end-1 (before) became
			" c.lnum..c.end-1+c.added (after). Snapshot the post-change content so
			" the worker can splice it into its mirror without calling back.
			let l:newend = c.end + c.added
			let l:lines = (l:newend > c.lnum) ? getbufline(a:bufnr, c.lnum, l:newend - 1) : []
			call add(l:changes, extend(copy(c), {"lines": l:lines}))
		endfor
		call ch_sendexpr(job_getchannel(l:job), ["bufchanged", {"bufnr": a:bufnr, "start": a:start, "end": a:end, "added": a:added, "changes": l:changes}])
	endfunc

	" Tear down the per-buffer job when its buffer goes away.
	func ParsetclBufCleanup(bufnr)
		let l:job = getbufvar(a:bufnr, "parsetcl_job", v:null)
		if l:job isnot v:null
			call job_stop(l:job)
			call setbufvar(a:bufnr, "parsetcl_job", v:null)
		endif
	endfunc

	augroup parsetcl
		autocmd!
		autocmd BufUnload * call ParsetclBufCleanup(str2nr(expand("<abuf>")))
	augroup END

	let g:parsetcl_loaded = 1
endif

" Per-buffer setup: start a dedicated job for this buffer (the job's internal
" state is bound to a single buffer, so each buffer needs its own).
if !exists("b:parsetcl_job")
	"echo "Starting parsetcl_job.tcl: ".g:parsetcl_path."/parsetcl_job.tcl"
	let b:parsetcl_job = job_start(["/opt/tcl9/bin/tclsh", g:parsetcl_path."/parsetcl_job.tcl"], {"mode": "json", "callback": "ParsetclHandleJobMsg", "err_io":"out"})
	"echo b:parsetcl_job

	call listener_add("ParsetclListener", bufnr("%"))
	call ch_sendexpr(job_getchannel(b:parsetcl_job), ["bufchanged", {"bufnr": bufnr("%")}])
endif

let b:current_syntax = "tcl"
