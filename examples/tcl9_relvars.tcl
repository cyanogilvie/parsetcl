# Survey Tcl source for variable names that Tcl 9 resolves differently.
#
# Tcl 8 resolved a relative qualified variable name (crypto::rsa::sha1, not
# starting with ::) in the current namespace first and then fell back to the
# global namespace.  Tcl 9 dropped the fallback (TIP 278), so code running in
# any namespace other than :: that relies on it now fails ("can't read
# crypto::rsa::sha1: no such variable"), or silently creates a new variable.
#
# This walks parsetcl's deep parse tree of each file, tracks the namespace
# each script will run in, and reports relative qualified names in variable
# positions ($substitutions and the varName arguments of set, info exists,
# upvar, ...) used where the current namespace isn't the global one.
#
#   tclsh tcl9_relvars.tcl ?-coverage? ?-all? ?-parsers file? ?-nseval cmd=ns? ?-proc cmds? ?-where cmds? path ...
#
# Two kinds of report:
#   - relative qualified names (crypto::rsa::sha1) outside the global namespace
#   - in namespace-level code (not a proc frame) outside ::, unqualified names
#     the corpus declares global elsewhere (global / upvar #0) - Tcl 8 found
#     those through the global fallback - and [global] statements, which do
#     nothing outside a proc ("global" / "global-noop" kinds)
#
# Paths are files or directories (searched for *.tcl, *.tm and scripts with a
# tclsh shebang).  -all also reports names that look like references to a
# child namespace of the current one (likely fine).  -nseval cmd=ns declares
# a command whose last argument runs as namespace-level code in ns (RL's
# page_local=::page_local), -proc cmd,cmd commands that define procs (their
# last argument is a proc body: RL's rpc_proc, rl_formproc, ...).  -parsers
# sources a file
# that adds a codebase's own commands to ::parsetcl::cmd_parsers (repeatable).
# -coverage lists braced
# words that weren't deep-parsed, by command, to find cmd_parsers gaps.
#
# Blind spots: scripts parsetcl's cmd_parsers don't know are scripts (see
# -coverage), scripts held in variables and run later, and computed names.

package require parsetcl

# A cmd_parsers entry that subparses a word that isn't valid in the requested
# language (a C or SQL block taken for a script) throws out of the whole
# parse.  Make that local: leave the word without its <as> parse, and count it
rename ::parsetcl::subparse ::parsetcl::_subparse_strict
proc ::parsetcl::subparse {mode word args} {
	if {[catch {::parsetcl::_subparse_strict $mode $word {*}$args} r o]} {
		dict incr ::relvars::subparse_failed $mode
		return
	}
	set r
}

namespace eval relvars {
	variable subparse_failed	{}
	variable globals			{}		;# names the corpus uses as global variables
	variable nsvars				{}		;# ns -> names declared with [variable] there
	variable nseval				{}
	variable proclike			{}		;# commands that define a proc: name ... body (body runs in a proc frame)		;# command -> namespace its last arg runs in, at namespace level
	variable where				{}
	variable known_ns	{:: 1}		;# namespaces created by namespace eval / qualified procs, anywhere in the corpus
	variable hits		{}
	variable unparsed	{}			;# command name -> count of brace-quoted words with newlines that weren't deep-parsed
	variable dynamic	0

	# Commands whose arguments name variables: cmd -> positions (1-based word
	# indices; "end" for the last; "rest:N" for every word from N on; a
	# leading subcommand-dispatch is handled in var_words below)
	variable var_args {
		set			{2}
		unset		{rest:2}
		append		{2}
		lappend		{2}
		lset		{2}
		incr		{2}
		vwait		{2}
		lassign		{rest:3}
		gets		{3}
		upvar		{upvar}
		variable	{variable}
		global		{}
		catch		{3 4}
		scan		{rest:4}
		regexp		{regexp}
		regsub		{regsub}
	}

	variable collecting	0

	proc known ns { #<<<
		# Record $ns (and its parents) as a namespace the corpus creates
		variable known_ns
		if {[string match <* $ns]} return
		set acc	""
		foreach p [split [string map {:: \x1f} [string trimleft $ns :]] \x1f] {
			append acc ::$p
			dict set known_ns $acc 1
		}
	}

	#>>>
	proc ns_join {ctx name} { #<<<
		if {[string match ::* $name]} {return [string trimright $name :]}
		if {$ctx eq "::" || $ctx eq ""} {return ::$name}
		return ${ctx}::$name
	}

	#>>>
	proc literal {word} { #<<<
		if {[domNode $word hasAttribute value]} {
			return [list 1 [domNode $word getAttribute value]]
		}
		return {0 {}}
	}

	#>>>
	proc words cmd {parsetcl xpath $cmd word}
	proc line_of {idx} { #<<<
		variable nl_offsets
		# binary search for the number of newlines before idx
		set lo 0; set hi [llength $nl_offsets]
		while {$lo < $hi} {
			set mid [expr {($lo + $hi) / 2}]
			if {[lindex $nl_offsets $mid] < $idx} {set lo [expr {$mid+1}]} else {set hi $mid}
		}
		expr {$lo + 1}
	}

	#>>>
	proc check_name {name kind node ctx {inproc 1}} { #<<<
		variable hits
		variable known_ns
		variable file
		variable collecting
		variable globals
		variable nsvars
		if {$collecting} return
		if {$name eq "" || [string match ::* $name]} return
		if {[string first :: $name] < 0} {
			# Unqualified: only namespace-level code (not a proc frame) outside ::
			# had the global fallback.  Report names the corpus uses as globals
			# that this namespace doesn't declare itself.
			set base	[regsub {\(.*$} $name {}]
			if {
				!$inproc && $ctx ni {:: <object> <unknown>} &&
				[dict exists $globals $base] &&
				!([dict exists $nsvars $ctx] && $base in [dict get $nsvars $ctx])
			} {
				lappend hits [list $file [line_of [domNode $node getAttribute idx]] $ctx global $base 0]
			}
			return
		}
		if {$ctx eq "::"} return
		set first	[lindex [split [string map {:: \x1f} $name] \x1f] 0]
		# A namespace eval'd/defined anywhere as <ctx>::<first> makes this a
		# genuine child reference, resolved the same on Tcl 8 and 9
		set likely_ok	[expr {$ctx ne "<object>" && [dict exists $known_ns [ns_join $ctx $first]]}]
		lappend hits [list $file [line_of [domNode $node getAttribute idx]] $ctx $kind $name $likely_ok]
	}

	#>>>
	proc check_word {word kind ctx {inproc 1}} { #<<<
		variable dynamic
		lassign [literal $word] static value
		if {$static} {
			check_name $value $kind $word $ctx $inproc
		} elseif {[string first :: [domNode $word asText]] >= 0} {
			incr dynamic
		}
	}

	#>>>
	proc var_words {cmd name ws} { #<<<
		# The words of $cmd (a command node named $name) that name variables
		variable var_args
		set n	[llength $ws]
		set out	{}
		switch -exact -- $name {
			info {
				lassign [literal [lindex $ws 1]] s sub
				if {$s && $sub eq "exists" && $n >= 3} {lappend out [lindex $ws 2]}
				return $out
			}
			array {
				if {$n >= 3} {lappend out [lindex $ws 2]}
				return $out
			}
			dict {
				lassign [literal [lindex $ws 1]] s sub
				if {$s && $sub in {set unset append lappend incr update with getwithdefault}} {
					if {$sub ne "getwithdefault" && $n >= 3} {lappend out [lindex $ws 2]}
				}
				return $out
			}
			trace {
				lassign [literal [lindex $ws 1]] s sub
				lassign [literal [lindex $ws 2]] s2 type
				if {$s && $sub in {add remove info} && $s2 && $type eq "variable" && $n >= 4} {lappend out [lindex $ws 3]}
				if {$s && $sub in {variable vdelete vinfo} && $n >= 3} {lappend out [lindex $ws 2]}
				return $out
			}
			namespace {
				# namespace upvar ns otherVar myVar ...: otherVar is relative to
				# ns, not the current namespace - skip; nothing else names vars
				return $out
			}
		}
		if {![dict exists $var_args $name]} {return $out}
		foreach spec [dict get $var_args $name] {
			switch -glob -- $spec {
				upvar {
					set i 1
					lassign [literal [lindex $ws 1]] s lvl
					if {$s && [regexp {^#?[0-9]+$} $lvl]} {incr i}
					# pairs: otherVar myVar.  otherVar resolves in the target
					# frame's namespace: global for #0, unknown otherwise (flag it)
					if {!($s && $lvl eq "#0")} {
						for {} {$i < $n} {incr i 2} {lappend out [lindex $ws $i]}
					}
				}
				variable {
					# variable name ?value name value ...?
					for {set i 1} {$i < $n} {incr i 2} {lappend out [lindex $ws $i]}
				}
				regexp - regsub {
					# skip switches, then: exp string ?matchVar ...?  (regsub: exp string subSpec ?varName?)
					set i 1
					while {$i < $n} {
						lassign [literal [lindex $ws $i]] s v
						if {!$s || ![string match -* $v]} break
						if {$v eq "--"} {incr i; break}
						if {$v in {-start -indices}} {if {$v eq "-start"} {incr i}}
						incr i
					}
					set first	[expr {$i + ($spec eq "regexp" ? 2 : 3)}]
					for {set j $first} {$j < $n} {incr j} {lappend out [lindex $ws $j]}
				}
				rest:* {
					set from	[expr {[string range $spec 5 end] - 1}]
					for {set j $from} {$j < $n} {incr j} {
						lassign [literal [lindex $ws $j]] s v
						if {$name eq "unset" && $s && $v in {-nocomplain --}} continue
						lappend out [lindex $ws $j]
					}
				}
				default {
					set j	[expr {$spec - 1}]
					if {$j < $n} {lappend out [lindex $ws $j]}
				}
			}
		}
		set out
	}

	#>>>
	proc walk {node ctx {inproc 0}} { #<<<
		# Walk $node's subtree, $ctx is the namespace scripts in it run in
		# (an absolute namespace, or <object> for oo method bodies), $inproc
		# whether they run in a proc frame rather than at namespace level
		foreach child [domNode $node childNodes] {
			if {[domNode $child nodeType] ne "ELEMENT_NODE"} continue
			switch -exact -- [domNode $child nodeName] {
				var {
					check_name [domNode $child getAttribute name ""] var $child $ctx $inproc
					walk $child $ctx $inproc
				}
				command {
					walk_command $child $ctx $inproc
				}
				default {
					walk $child $ctx $inproc
				}
			}
		}
	}

	#>>>
	proc body_ctx {cmd name ws ctx} { #<<<
		# The namespace context for scripts (as/script children) in the words of
		# this command: a dict word-index -> ctx for words that change it.  Also
		# sets frames in the caller: word-index -> 1 for proc-frame bodies, 0 for
		# namespace-level scripts
		upvar 1 frames frames
		set frames	{}
		set out	{}
		variable nseval
		variable proclike
		if {$name in $proclike && [llength $ws] > 2} {
			# name ... body ?...?: the body (the last word the cmd_parsers parsed
			# as a script) is a proc body in the current namespace
			for {set j [expr {[llength $ws]-1}]} {$j > 1} {incr j -1} {
				if {[llength [parsetcl xpath [lindex $ws $j] as/script]]} break
			}
			dict set out $j $ctx
			dict set frames $j 1
			return $out
		}
		if {[dict exists $nseval $name] && [llength $ws] > 1} {
			set last	[expr {[llength $ws]-1}]
			dict set out $last [dict get $nseval $name]
			dict set frames $last 0
			return $out
		}
		switch -exact -- $name {
			"namespace" {
				lassign [literal [lindex $ws 1]] s sub
				if {$s && $sub eq "inscope" && [llength $ws] == 4} {
					lassign [literal [lindex $ws 2]] s2 nsname
					dict set out 3 [expr {$s2 ? [ns_join $ctx $nsname] : "<dynamic>"}]
					dict set frames 3 0
				}
				if {$s && $sub eq "eval" && [llength $ws] == 4} {
					lassign [literal [lindex $ws 2]] s2 nsname
					if {$s2} {
						dict set out 3 [ns_join $ctx $nsname]
						known [ns_join $ctx $nsname]
					} else {
						dict set out 3 <dynamic>
					}
					dict set frames 3 0
				}
			}
			"proc" {
				lassign [literal [lindex $ws 1]] s pname
				if {$s} {
					set q	[namespace qualifiers $pname]
					if {$q eq ""} {
						dict set out 3 $ctx
					} else {
						dict set out 3 [ns_join $ctx $q]
						known [ns_join $ctx $q]
					}
				} else {
					dict set out 3 <dynamic>
				}
				dict set frames 3 1
			}
			"method" - "constructor" - "destructor" {
				# Bodies of methods run in the object's namespace, in a proc frame
				dict set out [expr {[llength $ws]-1}] <object>
				dict set frames [expr {[llength $ws]-1}] 1
			}
			"apply" {
				# apply {args body ?ns?}: body runs in ns, default global
				dict set out 1 <apply>
				dict set frames 1 1
			}
			"list" {
				# [list apply {args body ?ns?} ...]: a lambda built as a command
				# prefix (deep-parsed by e.g. RL's cmd_parsers)
				if {[llength $ws] < 3} return
				lassign [literal [lindex $ws 1]] s w1
				if {$s && $w1 eq "apply"} {
					dict set out 2 <apply>
					dict set frames 2 1
				}
			}
			"oo::define" - "oo::objdefine" {
				# oo::define cls method name args body | constructor args body | destructor body
				lassign [literal [lindex $ws 2]] s kind
				if {$s && $kind in {method constructor destructor} && [llength $ws] > 3} {
					dict set out [expr {[llength $ws]-1}] <object>
					dict set frames [expr {[llength $ws]-1}] 1
				}
			}
			"after" - "thread::send" - "interp" {
				# Scripts run at global level: the event loop, another thread's
				# or another interpreter's global namespace
				for {set j 1} {$j < [llength $ws]} {incr j} {dict set out $j ::}
			}
		}
		set out
	}

	#>>>
	proc walk_command {cmd ctx {inproc 0}} { #<<<
		variable unparsed
		variable collecting
		variable globals
		variable nsvars
		variable hits
		variable file
		set ws		[words $cmd]
		set name	[domNode $cmd getAttribute name ""]
		if {$name eq ""} {lassign [literal [lindex $ws 0]] - name}
		set bare	[namespace tail $name]
		if {[string match ::* $name] && [namespace qualifiers $name] in {"" "::tcl"}} {set name $bare}

		if {$collecting} {
			# Names code declares as globals: [global] and upvar #0 targets.
			# (Not everything set at global level: page scripts run there, so
			# that would include every request variable.)
			switch -exact -- $name {
				global {
					foreach w [lrange $ws 1 end] {
						lassign [literal $w] s v
						if {$s} {dict set globals [string trimleft $v :] 1}
					}
				}
				upvar {
					lassign [literal [lindex $ws 1]] s lvl
					if {$s && $lvl eq "#0"} {
						foreach {o m} [lrange $ws 2 end] {
							lassign [literal $o] s v
							if {$s} {dict set globals [regsub {\(.*$} [string trimleft $v :] {}] 1}
						}
					}
				}
				variable {
					if {$ctx ni {:: <object> <unknown>}} {
						foreach {w -} [lrange $ws 1 end] {
							lassign [literal $w] s v
							if {$s} {dict lappend nsvars $ctx [regsub {\(.*$} $v {}]}
						}
					}
				}
			}
		} elseif {$name eq "global" && !$inproc && $ctx ni {:: <object> <unknown>}} {
			# [global] outside a proc does nothing: the names it lists resolved
			# through the Tcl 8 global fallback
			foreach w [lrange $ws 1 end] {
				lassign [literal $w] s v
				lappend hits [list $file [line_of [domNode $w getAttribute idx]] $ctx global-noop $v 0]
			}
		}

		foreach w [var_words $cmd $name $ws] {
			if {$w ne ""} {check_word $w varname $ctx $inproc}
		}

		set ctxs	[body_ctx $cmd $name $ws $ctx]
		set i	0
		foreach w $ws {
			set wctx	$ctx
			set winproc	$inproc
			if {[dict exists $frames $i]} {set winproc [dict get $frames $i]}
			if {[dict exists $ctxs $i]} {set wctx [dict get $ctxs $i]}
			if {$wctx eq "<apply>"} {
				# lambda list {args body ?ns?}
				set lw	[parsetcl xpath $w {as/list/word}]
				set wctx	"::"
				if {[llength $lw] == 3} {
					lassign [literal [lindex $lw 2]] s lns
					set wctx [expr {$s ? [ns_join :: $lns] : "<dynamic>"}]
				}
			}
			if {$wctx eq "<dynamic>"} {set wctx "<unknown>"}
			# Coverage: a brace-quoted multi-line word with no deep parse
			if {
				[domNode $w getAttribute quoted ""] eq "brace" &&
				[llength [parsetcl xpath $w as]] == 0 &&
				[string first \n [domNode $w getAttribute value ""]] >= 0
			} {
				dict incr unparsed [expr {$name eq "" ? "<dynamic>" : $name}]
				variable where
				variable file
				variable collecting
				if {!$collecting && ([expr {$name eq "" ? "<dynamic>" : $name}] in $where)} {
					puts "  unparsed: $file:[line_of [domNode $w getAttribute idx]] $name word $i: [string range [string trim [domNode $w getAttribute value]] 0 60]"
				}
			}
			walk $w $wctx $winproc
			incr i
		}
	}

	#>>>
	proc files {paths} { #<<<
		set out	{}
		foreach p $paths {
			if {[file isdirectory $p]} {
				foreach f [lsort [glob -nocomplain -directory $p *]] {
					if {[file isdirectory $f]} {
						if {[file tail $f] in {.git node_modules}} continue
						if {[file type $f] eq "link"} continue	;# avoid link loops and double counting
						lappend out {*}[files [list $f]]
					} elseif {[string match *.tcl $f] || [string match *.tm $f]} {
						lappend out $f
					} elseif {![file isfile $f]} {
						continue		;# dangling links, sockets, ...
					} elseif {[file extension $f] eq "" && [file size $f] < 4000000} {
						if {![catch {set h [open $f rb]; set head [read $h 64]; close $h}]} {
							if {[regexp {^#!.*(tclsh|wish)} $head]} {lappend out $f}
						}
					}
				}
			} elseif {[file exists $p]} {
				lappend out $p
			}
		}
		set out
	}

	#>>>
	proc read_file fn { #<<<
		set h	[open $fn rb]
		try {set raw [read $h]} finally {close $h}
		# alpine-tcl / cftcl compressed modules: a loader stub, ^Z, then the
		# brotli-compressed source.  Scan the source.
		set eof	[string first \x1A $raw]
		set stub	"apply \{\{\} \{set h \[open \[info script\] rb\]"
		if {$eof > 0 && [string equal -length [string length $stub] $stub $raw] && [string first brotli [string range $raw 0 $eof]] >= 0} {
			package require brotli
			return [encoding convertfrom utf-8 [brotli::decompress [string range $raw $eof+1 end]]]
		}
		# Tcl source is normally utf-8; fall back to latin1 for odd bytes
		if {[catch {encoding convertfrom -profile strict utf-8 $raw} text]} {
			set text [encoding convertfrom iso8859-1 $raw]
		}
		# source stops at ^Z: anything after it (a C payload, a zip) isn't script
		set eof	[string first \x1A $text]
		if {$eof >= 0} {set text [string range $text 0 $eof-1]}
		set text
	}

	#>>>
	proc main argv { #<<<
		variable hits
		variable unparsed
		variable dynamic
		variable file
		variable nl_offsets
		set coverage	0
		set all			0
		set paths		{}
		for {set i 0} {$i < [llength $argv]} {incr i} {
			set a	[lindex $argv $i]
			switch -exact -- $a {
				-coverage	{set coverage 1}
				-all		{set all 1}
				-nseval		{
					# cmd=ns: cmd's last argument runs as namespace-level code in ns
					# (e.g. page_local=::page_local)
					variable nseval
					lassign [split [lindex $argv [incr i]] =] c n
					dict set nseval $c $n
				}
				-proc		{
					# cmd: a command that defines a proc (name ... body), so its
					# last argument runs in a proc frame (RL's rpc_proc, ...)
					variable proclike
					lappend proclike {*}[split [lindex $argv [incr i]] ,]
				}
				-where		{
					# List each unparsed multi-line braced word of these commands
					variable where	[split [lindex $argv [incr i]] ,]
				}
				-parsers	{
					# Extra cmd_parsers for a codebase's own script-taking
					# commands (a file that adds to ::parsetcl::cmd_parsers)
					uplevel #0 [list source [lindex $argv [incr i]]]
				}
				default		{lappend paths $a}
			}
		}
		set fns		[files $paths]
		set trees	{}
		set failed	{}
		foreach fn $fns {
			set text	[read_file $fn]
			# A .tm module's source is a script; skip any appended binary payload
			if {[catch {parsetcl parsetree $text} pt]} {
				lappend failed [list $fn [lindex [split $pt \n] 0]]
				continue
			}
			lappend trees $fn $text $pt
		}
		# Pass 1: the namespaces the corpus creates (to tell child references
		# from global-fallback ones); pass 2: the survey
		variable collecting
		set collecting	1
		foreach {fn text pt} $trees {
			set file		$fn
			set nl_offsets	{}
			walk [parsetcl node $pt] ::
		}
		set collecting	0
		set unparsed	{}
		set dynamic		0
		foreach {fn text pt} $trees {
			set file		$fn
			set nl_offsets	[lmap m [regexp -all -indices -inline \n $text] {lindex $m 0}]
			walk [parsetcl node $pt] ::
		}

		set problems	[lsearch -all -inline -index 5 -exact $hits 0]
		set likely		[lsearch -all -inline -index 5 -exact $hits 1]
		puts "Scanned [llength $fns] files ([llength $failed] didn't parse)"
		puts "Relative qualified variable names in non-global contexts: [llength $problems] (+[llength $likely] likely child-namespace references)"
		foreach h [lsort -index 0 -dictionary $problems] {
			lassign $h fn line ctx kind name
			puts [format "  %s:%d  %-9s %-28s in %s" $fn $line $kind $name $ctx]
		}
		if {$all && [llength $likely]} {
			puts "Likely child-namespace references:"
			foreach h [lsort -index 0 -dictionary $likely] {
				lassign $h fn line ctx kind name
				puts [format "  %s:%d  %-9s %-28s in %s" $fn $line $kind $name $ctx]
			}
		}
		puts "Variable-name words built from substitutions and containing :: (not analysed): $dynamic"
		variable subparse_failed
		if {[dict size $subparse_failed]} {
			puts "Words a cmd_parser took for script/expr/... that didn't parse as such (left unanalysed): $subparse_failed"
		}
		if {[llength $failed]} {
			puts "Didn't parse:"
			foreach f $failed {puts "  [join $f {: }]"}
		}
		if {$coverage} {
			puts "Multi-line braced words that weren't deep-parsed, by command:"
			foreach {cmd n} [lsort -stride 2 -index 1 -integer -decreasing $unparsed] {
				puts [format "  %6d  %s" $n $cmd]
			}
		}
	}

	#>>>
}

relvars::main $argv

# vim: ft=tcl foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4
