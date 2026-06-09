close stderr
open /tmp/parsetcl_job.stderr a
chan configure stderr -blocking 0 -buffering none
puts stderr starting

try {
package require parsetcl
package require rl_json
package require parse_args
package require aio
package require chantricks
package require tty

namespace import ::rl_json::json
namespace import ::parse_args::parse_args
namespace import ::parsetcl::*

source [file join $::env(HOME) git/rl/helpers/nsadmin_cmd_parsers.tcl]

#proc log msg {chantricks appendfile /tmp/parsetcl_job.log $msg}
proc log msg {puts stderr $msg}

# linebuf accelerator <<<
package require jitc
set linebuf_cdef {
	options	{-Wall -Werror -std=c23}
	filter	{jitc::re2c --case-ranges}
	code {
		#include <stdint.h>

		OBJCMD(linestarts)
		{
			Tcl_Obj*	starts = NULL;

			int		code = TCL_OK;
			enum {A_cmd, A_LINEBUF, A_objc};
			CHECK_ARGS_LABEL(finally, code, "linebuf");

			replace_tclobj(&starts, Tcl_NewListObj(2000, NULL));
			TEST_OK_LABEL(finally, code, Tcl_ListObjAppendElement(interp, starts, Tcl_NewWideIntObj(0)));

			Tcl_Size	len;
			const uint8_t*	str = (const uint8_t*)Tcl_GetString(objv[A_LINEBUF]);
			const uint8_t	*cur, *tok, *mar;
			cur = str;

			/*!types:re2c*/
			/*!stags:re2c format = "const uint8_t* @@;\n"; */
			for (;;) {
				tok = cur;
				const uint8_t	*e;
				/*!re2c
					re2c:yyfill:enable		= 0;
					re2c:define:YYCTYPE		= "uint8_t";
					re2c:define:YYCURSOR	= "cur";
					re2c:define:YYMARKER	= "mar";
					re2c:encoding:utf8		= 1;
					re2c:tags				= 1;

					end		= "\x00";
					eol
						= "\n"
						| "\r\n"
						| "\r"
						;
					c		= [^\r\n\x00];

					c* @e eol	{
						TEST_OK_LABEL(finally, code, Tcl_ListObjAppendElement(interp, starts, Tcl_NewWideIntObj(cur-str)));
						continue;
					}

					c* @e end	{
						TEST_OK_LABEL(finally, code, Tcl_ListObjAppendElement(interp, starts, Tcl_NewWideIntObj(cur-str)));
						break;
					}

					* {
						if (cur[-1] == 0xC0 && cur[0] == 0x80) {
							cur++;
							continue;
						}
						THROW_PRINTF_LABEL(finally, code, "scan error at %d: codeunit: %02x", tok-str, tok[0]);
					}
				*/
			}

			Tcl_SetObjResult(interp, starts);

		finally:
			replace_tclobj(&starts, NULL);
			return code;
		}
	}
}

jitc::bind linestarts	$linebuf_cdef linestarts
#>>>

namespace eval vim {
	namespace export *
	namespace ensemble create -prefixes no -map {
		flush		_flush
		redraw		redraw
		ex			ex
		normal		normal
		expr		_expr
		call		call
	}

	variable seq	-2
	variable seq_cb	{}

	proc _flush {} {flush stdout}

	proc redraw args { #<<<
		parse_args $args {
			-force	{-boolean}
		}
		set forcearg	[if {$force} {return -level 0 force}]
		puts [json template {
			["redraw","~S:forcearg"]
		}]
		#yield
	}

	#>>>
	proc ex cmd { #<<<
		puts [json template {
			["ex", "~S:cmd"]
		}]
	}

	#>>>
	proc normal cmd { #<<<
		puts [json template {
			["normal", "~S:cmd"]
		}]
	}

	#>>>
	proc _expr args { #<<<
		parse_args $args {
			-noresponse	{-boolean}
			expression	{-required}
		}
		set msg	[json template {
			["expr", "~S:expression"]
		}]
		if {!$noresponse} {
			variable seq
			variable seq_cb
			set myseq	[incr seq -1]
			dict set seq_cb $myseq [info coroutine]
			json set msg end+1 $myseq
		}
		puts $msg
		if {!$noresponse} {
			_flush
			try yield finally {dict unset seq_cb $myseq}
		}
	}

	#>>>
	proc call args { #<<<
		parse_args $args {
			-noresponse	{-boolean}
			func		{-required}
			args		{-name funcargs}
		}

		set argarr {[]}
		foreach a $funcargs {
			json set argarr end+1 $a
		}

		set msg	[json template {["call","~S:func","~J:argarr"]}]
		if {!$noresponse} {
			variable seq
			variable seq_cb
			set myseq	[incr seq -1]
			dict set seq_cb $myseq [info coroutine]
			json set msg end+1 $myseq
		}
		#set before	[clock microseconds]
		puts $msg
		if {!$noresponse} {
			_flush
			#try yield finally {dict unset seq_cb $myseq; log "call $myseq got result: [format {%.6f s} [expr {([clock microseconds]-$before)/1e6}]]"}
			try yield finally {dict unset seq_cb $myseq}
		}
	}

	#>>>
}

proc idx2lc_vim idx { #<<<
	set line		[vim call byte2line [expr {$idx+1}]]
	set linestart	[vim call line2byte $line]
	list $line [expr {$idx-$linestart+2}]	;# TODO: account for multibyte chars in the byterange [linestart, $idx)
}

#>>>
proc idx2lc idx { #<<<
	global linestarts lines
	set line	[lsearch -bisect -increasing -integer $linestarts $idx]
	if {$line == -1} {
		set line		0
		set linestart	0
	} else {
		set linestart	[lindex $linestarts $line]
	}
	set col	[expr {$idx - $linestart}]
	incr line
	incr col
	#log "idx2lc($idx): $line $col"
	list $line $col
}

#>>>
proc printval str { #<<<
	global emap
	if {![info exists emap]} {
		set emap	[dict create \
			\a		\\a \
			\b		\\b \
			\e		\\e \
			\f		\\f \
			\n		\\n \
			\r		\\r \
			\t		\\t \
			\v		\\v \
		]
	}
	set v	{}
	foreach c [split $str {}] {
		if {[string is print -strict $c]} {
			append v $c
		} else {
			append v [tty colour green {
				if {$c eq "\\"} {
					return -level 0 \\\\
				} elseif {[set e [dict getdef $emap $c {}]] ne ""} {
					set e
				} else {
					scan $c %c codepoint
					if {$codepoint <= 0xff} {
						return -level 0 \\x[format %02x $codepoint]
					} elseif {$codepoint <= 0xFFFF} {
						return -level 0 \\u[format %04x $codepoint]
					} else {
						return -level 0 \\U[format %x $codepoint]
					}
				}
			}]
		}
	}
	tty colour inverse {set v}
}

#>>>
proc changed_chunks {old new} { #<<<
	# Strip the longest common prefix and suffix from $old and $new to find
	# the minimal contiguous chunks that were deleted and inserted. Returns
	# [list deleted inserted].
	set oldlen	[string length $old]
	set newlen	[string length $new]
	set common	[expr {min($oldlen, $newlen)}]

	set pre	0
	while {$pre < $common && [string index $old $pre] eq [string index $new $pre]} {
		incr pre
	}

	set maxsuf	[expr {$common - $pre}]
	set suf	0
	while {$suf < $maxsuf && [string index $old end-$suf] eq [string index $new end-$suf]} {
		incr suf
	}

	list \
		[string range $old $pre [expr {$oldlen - 1 - $suf}]] \
		[string range $new $pre [expr {$newlen - 1 - $suf}]]
}

#>>>
proc chunk_fingerprint chunk { #<<<
	# Reduce a chunk to the features that affect the semantics of surrounding
	# lines: the literal sequence of brace/quote characters, and the parity of
	# the trailing backslash run (an odd count is a line-continuation). A
	# deletion and insertion with matching fingerprints leave the parser state
	# of unchanged lines untouched.
	return [regsub -all {[^{}"\[\]]+} $chunk {}][expr {
		[string length [regexp -inline {\\+$} $chunk]] & 1
			? "\\"
			: ""
	}]
}

#>>>

namespace eval req {
	namespace export *
	namespace ensemble create -prefixes no

	proc bufchanged desc { #<<<
		global lines linestarts
		set start	[clock microseconds]
		set tlog {msg {
			upvar 1 start start
			log [format {%.6f %s} [expr {([clock microseconds]-$start)/1e6}] $msg]
		}}
		apply $tlog "desc: [json pretty $desc]"
		set buf			[json get $desc bufnr]
		if {![info exists lines] || ![json exists $desc changes]} {
			set lines		[vim call getbufline $buf 1 {$}]
			#set lines		[vim call getline 1 {$}]
			apply $tlog "got [json length $lines] lines"
			set semantic_change	1
		} else {
			set linelist		[json get $lines]
			apply $tlog "Computing blast radius"
			set semantic_change	0
			json foreach c [json extract $desc changes] {
				set lnum		[json get $c lnum]
				set cend		[json get $c end]
				set new_lines	[json get $c lines]
				set new_block	[join $new_lines \n]
				set old_block	[join [lrange $linelist [expr {$lnum-1}] [expr {$cend-2}]] \n]
				#lassign [changed_chunks $old_block $new_block] deleted inserted
				set deleted		$old_block
				set inserted	$new_block
				set old_fingerprint	[chunk_fingerprint $deleted]
				set new_fingerprint	[chunk_fingerprint $inserted]
				if {$old_fingerprint ne $new_fingerprint} { set semantic_change 1 }
				apply $tlog "change at line $lnum: semantic_change: $semantic_change (deleted=[string length $deleted]B [printval $deleted], inserted=[string length $inserted]B [printval $inserted])"
				apply $tlog "old_fingerprint: [printval $old_fingerprint]\nnew_fingerprint: [printval $new_fingerprint]"
				apply $tlog "old_block: [printval $old_block], new_block: [printval $new_block]"
				set linelist	[lreplace $linelist[unset linelist] [expr {$lnum-1}] [expr {$cend-2}] {*}$new_lines]
				set zlnum	[expr {$lnum-1}]
				if {!$semantic_change} {
					if {![info exists minline] || $minline > $zlnum} {set minline $zlnum}
					if {![info exists maxline] || $maxline > $zlnum} {set maxline $zlnum}
				}
			}
			set lines	{[]}
			foreach line $linelist {json set lines end+1 [json string $line]}
		}
		#log "got lines: [json pretty $lines]"
		set buftext		[join [json get $lines] \n]
		set parsetree	[parsetree $buftext]
		set root		[node $parsetree]
		apply $tlog "parsed script"

		#set linestarts	[xpath $root string(/tcl/script/@linestarts)]
		apply $tlog "build linestarts: [timerate {
		set linestarts	[linestarts $buftext]
		} 1 1]"
		if 0 {
		#apply $tlog "parsed script, linestarts: $linestarts"
		set line_ranges [lmap e $linestarts {
			try {
				if {![info exists last]} continue
				puts stderr "last: $last, e: $e"
				list $last [expr {$e-2}]
			} finally {
				set last $e
			}
		}]
		foreach line [json get $lines] range $line_ranges {
			puts stderr "range: $range"
			puts stderr [printval $line]\n[printval [if {[llength $range]} {string range $buftext {*}$range} {format <none>}]]
		}
		}

		set props	1

		if {$props} {
			#set last_line	[json length $lines]
			#set existing [vim call prop_list 1 [json template {
			#	{"end_lnum": "~N:last_line", "bufnr": "~N:buf"}
			#}]]
			#apply $tlog "Fetch existing text props: [string length $existing] chars"
			if {$semantic_change} {
				set minline	0
				set maxline	[json length $lines]
			} else {
				incr maxline
			}
			apply $tlog "Clearing props from $minline to $maxline"
			vim call -noresponse prop_clear [expr {$minline+1}] [expr {$maxline+0}] [json template {
				{"bufnr": "~N:buf"}
			}]
		} else {
			vim call -noresponse clearmatches
		}
		set from_idx	[lindex $linestarts $minline]
		set to_idx		[lindex $linestarts $maxline]
		if {$to_idx eq {}} {
			set to_idx	[expr {[lindex $linestarts end] + [string length [json get $lines end]]}]
		}
		apply $tlog "from_idx: $from_idx, to_idx: $to_idx"
		foreach {group xpath} {
			parsetclSyntax			{//syntax[@idx >= $from_idx and @idx < $to_idx]}
			parsetclCommandEnd		{//end[@idx >= $from_idx and @idx < $to_idx]}
			parsetclCommand {
				//command[not(
					ancestor::word[as/script][@valuetransformed]
				)]/word[1][@idx >= $from_idx and @idx < $to_idx]
			}
			parsetclEscape			{//escape[@idx >= $from_idx and @idx < $to_idx]}
			parsetclComment			{//comment[@idx >= $from_idx and @idx < $to_idx]}
			parsetclOperator		{//operator/syntax[@idx >= $from_idx and @idx < $to_idx]}
			parsetclString {
				//word[@quoted="quote" and not(as and not(@valuetransformed))]/text[@idx >= $from_idx and @idx < $to_idx]
			}
			parsetclVariable {
				//var/text[1][@idx >= $from_idx and @idx < $to_idx] |
				//command[@name="set"]/word[position()=2 and not(@dynamic)][@idx >= $from_idx and @idx < $to_idx] |
				//command[@name="proc"]/word[3]/as/list/word[@idx >= $from_idx and @idx < $to_idx] |
				//command[@name="lassign"]/word[position()>2][@idx >= $from_idx and @idx < $to_idx]
			}
		} {
			set locs	{[]}
			foreach node [xpath $root $xpath] {
				set idx	[domNode $node getAttribute idx]
				set len	[domNode $node getAttribute len]
				if {$len == 0} continue
				lassign [idx2lc $idx] line col

				if {$props} {
					#lassign [idx2lc [expr {$end+1}]] eline ecol
					lassign [idx2lc [expr {$idx + $len}]] eline ecol
					json set locs end+1 [json template {
						["~N:line", "~N:col", "~N:eline", "~N:ecol"]
					}]
				} else {
					set end	[expr {$idx + $len - 1}]
					# TODO: maybe trim len to the end of the first line?
					json set locs end+1 [json template {
						["~N:line", "~N:col", "~N:len"]
					}]
					if {[string first \n $buftext $idx] < $end} {
						lassign [idx2lc $end] endline endcol
						if {$endline > $line} {
							for {set l [expr {$line+1}]} {$l < $endline} {incr l} {
								json set locs end+1 [json template {
									["~N:l"]
								}]
							}
							json set locs end+1 [json template {
								["~N:l", 1, "~N:endcol"]
							}]
						}
					}
				}
			}
			#apply $tlog "locs: $locs"
			apply $tlog "found [json length $locs] $group ranges"
			if {[json length $locs]} {
				if {$props} {
					vim call -noresponse prop_add_list [json template {
						{"type":"~S:group", "bufnr":"~N:buf"}
					}] $locs
				} else {
					vim call -noresponse matchaddpos $group $locs
				}
			}
		}

		#vim redraw
		vim flush
		apply $tlog "redraw and flushed"
	}

	#>>>
}


chan configure stdin  -buffering line -blocking 0
chan configure stdout -buffering full -blocking 0
#chan configure stdin  -buffering none -blocking 0 -translation binary
#chan configure stdout -buffering none -blocking 0

proc stdin_readable {} { #<<<
	variable ::vim::seq_cb

	try {
		while 1 {
			set line	[aio gets stdin]
			set id		[json get $line 0]
			if {[dict exists $seq_cb $id]} {
				after 0 [list [dict get $seq_cb $id] [json extract $line 1]]
			} else {
				coroutine req_$id apply {{id name info} {
					try {
						req $name $info
					} on ok r {
						if {![json valid $r]} { set r [json string $r] }
						puts [json template {["~N:id","~J:r"]}]
					} on error {errmsg options} {
						log "Unhandled error in req processing ($name) [dict get $options -errorcode]: [dict get $options -errorinfo]"
						puts [json template {["~N:id","~S:errmsg"]}]
					} finally {
						log "[info coroutine] $id req $name done"
					}
				}} $id [json get $line 1 0] [json extract $line 1 1]
			}
		}
	} on error {errmsg options} {
		log "coro_stdin_readable error: [dict get $options -errorinfo]"
	} finally {
		log "=========== LEAVING [info coroutine] ================="
		set ::exit 1
	}
}

#>>>
coroutine coro_stdin_readable stdin_readable

#chantricks tap_chan stdin
#chantricks tap_chan stdout

if {![info exists exit]} {vwait exit}
} on error {errmsg options} {
	puts stderr "handled error: [dict get $options -errorinfo]"
	exit 1
} on ok {} {
	puts stderr "exiting normally"
}

# vim: foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4 noexpandtab
