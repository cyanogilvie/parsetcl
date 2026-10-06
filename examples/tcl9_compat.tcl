# Survey Tcl source for constructs that break or change meaning on Tcl 9.
#
#   tclsh tcl9_compat.tcl ?-parsers file? ?-kinds k,k? ?-skip k,k? ?-summary? ?-requires file? path ...
#
# Walks parsetcl's deep parse of each file (so code inside proc bodies, if,
# namespace eval, and any script-taking command a cmd_parser knows about is
# seen) and reports, per kind:
#
#   errors on Tcl 9:
#     case            the case command
#     puts-nonewline  puts chan string nonewline
#     read-nonewline  read chan nonewline
#     trace-old       trace variable/vdelete/vinfo
#     bytelength      string bytelength
#     enc-binary      -encoding binary / {}, encoding convert* binary
#     enc-unknown     a literal encoding name this Tcl doesn't have (identity, ...)
#     eofchar-pair    -eofchar {in out} (write side gone)
#     inject          ::tcl::unsupported::inject
#     pkg-tcl8        package require Tcl 8.x without a range (8.5 means 8.5-<9)
#     load-prefix     load with a literal init prefix (now case sensitive)
#   behaviour changes, to review:
#     octal           0-prefixed integer literal >= 8 where a number is parsed
#     tilde           ~ in a path argument (no longer expanded)
#     glob-catch      catch {glob ...} without -nocomplain (glob no longer errors on no match)
#     bytes           data argument of a byte-oriented command (binary scan,
#                     binary encode, zlib, md5/sha*) not visibly bytes: throws on
#                     chars > U+00FF, Tcl 8 silently used the low byte
#     convert         encoding convertfrom/convertto without -profile (strict now)
#     chan-encoding   -encoding <non-utf-8> on a channel without -profile (strict now)
#     clock-free      free-form clock scan with relative words (validated now:
#                     a weekday is checked against the date after the offset)
#     int-wrap        int()/wide() mixed with bit operations (no 32/64-bit truncation)
#     string-is-int   string is integer/int/wide (accepts 1_000 and any size now)
#     tcl-vars        tcl_precision, tcl_platform(threaded)
#     astral          \uD800-\uDFFF surrogate escapes / \uFFFF ranges
#     varname-nest    ${..{..}..} / nested () in variable names (TIP 465)
#
# -requires file writes every literal "package require" (name + requirements)
# found, one Tcl list per line, for checking against a runtime.
#
# -screen dir writes what the scan can't decide, as batches of JSON items
# (dir/batch-NNN.json) for a cheaper model to triage: each item has its kind,
# location, the flagged source, the enclosing proc's source (capped) and the
# question to answer.  Items are the review kinds above, plus:
#     dyn-script      a script-taking position given a non-literal value (eval $x,
#                     uplevel $body, thread::send $t $script, after $ms $cb, ...)
#                     whose code the scan never sees
#     unparsed-code   a multi-line braced literal that looks like Tcl code but
#                     wasn't deep-parsed (likely a cmd_parsers gap)
# -batch N sets the items per batch (default 40).

package require parsetcl

rename ::parsetcl::subparse ::parsetcl::_subparse_strict
proc ::parsetcl::subparse {mode word args} {
	if {[catch {::parsetcl::_subparse_strict $mode $word {*}$args} r o]} {
		dict incr ::compat::subparse_failed $mode
		return
	}
	set r
}

namespace eval compat {
	variable subparse_failed	{}
	variable hits				{}
	variable requires			{}
	variable file
	variable nl_offsets

	variable path_cmds {
		open 1  source end  cd 1  load 1
	}
	variable relative_words {day days week weeks month months year years hour hours minute minutes min mins second seconds sec secs fortnight fortnights ago tomorrow yesterday today now next last this}

	proc literal word { #<<<
		if {[domNode $word hasAttribute value]} {
			return [list 1 [domNode $word getAttribute value]]
		}
		return {0 {}}
	}

	#>>>
	proc lit word {lindex [literal $word] 1}
	proc static word {lindex [literal $word] 0}
	proc words cmd {parsetcl xpath $cmd word}
	proc line_of idx { #<<<
		variable nl_offsets
		set lo 0; set hi [llength $nl_offsets]
		while {$lo < $hi} {
			set mid [expr {($lo + $hi) / 2}]
			if {[lindex $nl_offsets $mid] < $idx} {set lo [expr {$mid+1}]} else {set hi $mid}
		}
		expr {$lo + 1}
	}

	#>>>
	proc text node { #<<<
		# Source text of a node
		# idx/len are utf-8 byte offsets
		variable srcbytes
		set idx	[domNode $node getAttribute idx]
		set len	[domNode $node getAttribute len 0]
		encoding convertfrom -profile replace utf-8 [string range $srcbytes $idx [expr {$idx + $len - 1}]]
	}

	#>>>
	proc hit {kind node detail} { #<<<
		variable hits
		variable file
		variable screen_kinds
		if {[speculative $node]} {
			# Inside a literal the scan only guessed is code: confirm both
			append detail " \[speculative: in a braced literal guessed to be code\]"
			lappend hits [list $kind $file [line_of [domNode $node getAttribute idx]] $detail]
			screen $kind $node $detail
			return
		}
		lappend hits [list $kind $file [line_of [domNode $node getAttribute idx]] $detail]
		if {$kind in $screen_kinds} {screen $kind $node $detail}
	}

	#>>>
	proc snippet node { #<<<
		if {[domNode $node nodeName] eq "expr"} {
			set t	[domNode $node selectNodes {string(subexpr/@orig)}]
		} else {
			set t	[text $node]
		}
		set t	[string map {\n " " \t " "} $t]
		regsub -all { +} $t { } t
		if {[string length $t] > 100} {set t "[string range $t 0 96]..."}
		set t
	}

	#>>>
	proc cmdname cmd { #<<<
		set n	[domNode $cmd getAttribute name ""]
		if {$n eq ""} {return ""}
		if {[string match ::* $n]} {set n [string range $n 2 end]}
		set n
	}

	#>>>
	proc octal_differs v { #<<<
		# A 0-prefixed decimal-looking integer whose Tcl 8 (octal / invalid)
		# and Tcl 9 (decimal) readings differ
		if {![regexp {^[-+]?0+([0-9]+)$} $v - digits]} {return 0}
		expr {[scan $digits %d] >= 8}
	}

	#>>>
	proc made_of_bytes {word scope {seen {}}} { #<<<
		# 1 if $word visibly holds bytes (or only ASCII): a literal without
		# chars > 0xFF, a [command] that produces bytes, or a variable set from
		# one of those within $scope (the enclosing proc body / file)
		lassign [literal $word] s v
		if {$s} {return [regexp {^[\x00-\xff]*$} $v]}
		set parts	[parsetcl xpath $word {*[not(self::syntax)]}]
		if {[llength $parts] != 1} {return 0}
		set p	[lindex $parts 0]
		switch -exact -- [domNode $p nodeName] {
			script {
				set c	[lindex [parsetcl xpath $p command] 0]
				if {$c eq ""} {return 0}
				return [bytes_cmd $c $scope $seen]
			}
			var {
				set name	[domNode $p getAttribute name ""]
				if {$name eq ""} {return 0}
				# Self-referencing assignments (set x [string range $x ..]) are
				# decided by the variable's other assignments
				if {$name in $seen} {return 1}
				lappend seen $name
				foreach set [parsetcl xpath $scope [format {.//command[@name='set' and word[2][@value='%s'] and count(word)=3]} $name]] {
					set w3	[lindex [words $set] 2]
					if {![made_of_bytes $w3 $scope $seen]} {return 0}
					set found 1
				}
				# Assigned only by commands that produce bytes into a variable
				foreach c [parsetcl xpath $scope [format {.//command[@name='binary' or @name='zlib' or @name='read'][word/@value='%s']} $name]] {
					set found 1
				}
				return [info exists found]
			}
		}
		return 0
	}

	#>>>
	proc bytes_cmd {c scope {seen {}}} { #<<<
		set n	[cmdname $c]
		set ws	[words $c]
		set sub	[lit [lindex $ws 1]]
		switch -glob -- $n {
			encoding	{return [expr {$sub eq "convertto"}]}
			binary		{return [expr {$sub in {format decode}}]}
			zlib		{return 1}
			read		{return 1}
			string {
				if {$sub eq "range" || $sub eq "repeat"} {return [made_of_bytes [lindex $ws 2] $scope $seen]}
				return 0
			}
			md5::md5 - sha1::sha1 - sha2::sha256 - sha2::sha224 - *::hmac {
				return [expr {"-hex" ni [lmap w $ws {lit $w}]}]
			}
			default {
				# hashes, ciphers and decoders conventionally return bytes
				return [regexp {(?i)(decode|encrypt|decrypt|hmac|hash|digest|sign|random|bytes|_bin)} $n]
			}
		}
	}

	#>>>
	proc scope_of node { #<<<
		# The proc / method / lambda body script enclosing $node, else the file
		set s	[parsetcl xpath $node {ancestor::script[parent::as/parent::word/preceding-sibling::word and (parent::as/parent::word/parent::command[@name='proc' or @name='method' or @name='apply' or @name='constructor'])][1]}]
		if {[llength $s]} {return [lindex $s 0]}
		lindex [parsetcl xpath $node {ancestor::script[last()]}] 0
	}

	#>>>
	proc check_bytes {c data what} { #<<<
		if {$data eq ""} return
		if {[made_of_bytes $data [scope_of $c]]} return
		hit bytes $data "$what: [snippet $data]"
	}

	#>>>
	proc check_command c { #<<<
		variable path_cmds
		variable relative_words
		variable requires
		set n	[cmdname $c]
		set ws	[words $c]
		set nw	[llength $ws]
		set v	[lmap w $ws {lit $w}]
		set sub	[lindex $v 1]

		# Options anywhere: -encoding binary, -eofchar pairs, -profile
		set has_profile	[expr {"-profile" in $v}]
		if {$n in {fconfigure open source socket} || ($n eq "chan" && $sub eq "configure")} {
			for {set i 1} {$i < $nw-1} {incr i} {
				set o	[lindex $v $i]
				set w	[lindex $ws $i+1]
				if {$o eq "-encoding" && [static $w]} {
					set e	[lit $w]
					if {$e ni {binary {}} && [string tolower $e] ni [encoding names]} {hit enc-unknown $c [snippet $c]}
					if {$e in {binary {}}} {
						hit enc-binary $c [snippet $c]
					} elseif {$n ne "source" && ![string match -nocase utf-8 $e] && $e ni {utf8 unicode} && !$has_profile} {
						hit chan-encoding $c [snippet $c]
					}
				}
				# Tcl 9 dropped the write side: {in out} with a non-empty out throws
				if {$o eq "-eofchar" && [static $w] && [string is list [lit $w]] && [llength [lit $w]] == 2 && [lindex [lit $w] 1] ne ""} {
					hit eofchar-pair $c [snippet $c]
				}
			}
		}

		switch -exact -- $n {
			case {
				if {$nw >= 3} {hit case $c [snippet $c]}
			}
			puts {
				if {$nw == 4 && [lindex $v 3] eq "nonewline"} {hit puts-nonewline $c [snippet $c]}
			}
			read {
				if {$nw == 3 && [lindex $v 2] eq "nonewline"} {hit read-nonewline $c [snippet $c]}
			}
			trace {
				if {$sub in {variable vdelete vinfo}} {hit trace-old $c [snippet $c]}
			}
			string {
				if {$sub eq "bytelength"} {hit bytelength $c [snippet $c]}
				if {$sub eq "is" && [lindex $v 2] in {integer int wide}} {hit string-is-int $c [snippet $c]}
			}
			encoding {
				if {$sub in {convertfrom convertto}} {
					if {[lindex $v 2] eq "binary"} {
						hit enc-binary $c [snippet $c]
					} elseif {[static [lindex $ws 2]] && ![string match -* [lindex $v 2]] && [lindex $v 2] ni [encoding names]} {
						# Encodings Tcl 9 dropped (identity, ...)
						hit enc-unknown $c [snippet $c]
					}
					# utf-8 encodes every code point but lone surrogates
					if {!$has_profile && !($sub eq "convertto" && [string match -nocase utf-8 [lindex $v 2]])} {hit convert $c [snippet $c]}
				}
			}
			binary {
				if {$sub eq "scan" && $nw >= 3} {check_bytes $c [lindex $ws 2] "binary scan"}
				if {$sub eq "encode" && $nw >= 4} {check_bytes $c [lindex $ws end] "binary encode [lindex $v 2]"}
			}
			zlib {
				if {$sub in {compress deflate gzip crc32 adler32} && $nw >= 3} {check_bytes $c [lindex $ws 2] "zlib $sub"}
			}
			md5::md5 - sha1::sha1 - sha2::sha256 - sha2::sha224 {
				if {"-filename" ni $v && "-channel" ni $v && "-file" ni $v} {check_bytes $c [lindex $ws end] $n}
			}
			md5::hmac - sha1::hmac - sha2::hmac {
				check_bytes $c [lindex $ws end] $n
			}
			package {
				if {$sub eq "require"} {
					set args	[lrange $ws 2 end]
					set av		[lrange $v 2 end]
					set exact	0
					while {[string match -* [lindex $av 0]]} {
						if {[lindex $av 0] eq "-exact"} {set exact 1}
						set av [lrange $av 1 end]; set args [lrange $args 1 end]
					}
					if {[llength $args] && [static [lindex $args 0]] && [string match -nocase tcl [lindex $av 0]]} {
						foreach r [lrange $av 1 end] {
							if {[string match 8* $r] && ![string match *-* $r]} {hit pkg-tcl8 $c [snippet $c]}
						}
					}
					if {[llength $args] && [static [lindex $args 0]] && [lsearch -not -exact [lmap a $args {static $a}] 1] < 0} {
						dict lappend requires [list [lindex $av 0] {*}[expr {$exact ? [list [lindex $av 1]-[lindex $av 1]] : [lrange $av 1 end]}]] "[set ::compat::file]:[line_of [domNode $c getAttribute idx]]"
					}
				}
			}
			load {
				if {$nw >= 3} {hit load-prefix $c [snippet $c]}
			}
			catch {
				set body	[lindex $ws 1]
				set first	[lindex [parsetcl xpath $body {as/script/command[1]}] 0]
				if {$first ne "" && [cmdname $first] eq "glob" && "-nocomplain" ni [lmap w [words $first] {lit $w}]} {
					hit glob-catch $c [snippet $c]
				}
			}
			clock {
				if {$sub eq "scan" && $nw >= 3 && "-format" ni $v} {
					set arg	[lindex $ws 2]
					set t	[string tolower [text $arg]]
					set rel	0
					foreach tok [regexp -all -inline {[a-z]+} [regsub -all {\$[a-z_:()]+|\[[^]]*\]} $t {}]] {
						if {$tok in $relative_words} {set rel 1; break}
					}
					if {$rel && ![static $arg]} {hit clock-free $c [snippet $c]}
				}
			}
			file {
				if {$sub eq "join" && $nw >= 3 && [string match ~* [lindex $v 2]]} {hit tilde $c [snippet $c]}
				if {$sub ni {join split tail extension rootname dirname} && $nw >= 3} {
					foreach w [lrange $ws 2 end] {
						if {[string match ~* [text $w]] || [string match \"~* [text $w]]} {hit tilde $c [snippet $c]; break}
					}
				}
			}
			glob {
				foreach w [lrange $ws 1 end] {
					if {[string match ~* [text $w]] || [string match \"~* [text $w]]} {hit tilde $c [snippet $c]; break}
				}
			}
			incr {
				if {$nw == 3 && [octal_differs [lindex $v 2]]} {hit octal $c [snippet $c]}
			}
			lindex - lrange - lreplace - lset - after - format {
				foreach w [lrange $ws 1 end] x [lrange $v 1 end] {
					if {[octal_differs $x]} {hit octal $c [snippet $c]; break}
				}
			}
			"::tcl::unsupported::inject" - "tcl::unsupported::inject" {
				hit inject $c [snippet $c]
			}
		}
		if {[dict exists $path_cmds $n]} {
			set w	[lindex $ws [dict get $path_cmds $n]]
			if {$w ne "" && ([string match ~* [text $w]] || [string match \"~* [text $w]])} {hit tilde $c [snippet $c]}
		}
	}

	#>>>
	proc check_tree root { #<<<
		variable screening
		speculate $root
		foreach c [parsetcl xpath $root {//command}] {
			if {[catch {check_command $c} e o]} {
				hit internal $c [lindex [split [dict get $o -errorinfo] \n] 0]
			}
			if {$screening} {
				set n	[cmdname $c]
				set ws	[words $c]
				set v	[lmap w $ws {lit $w}]
				foreach w [dyn_script_words $c $n $ws $v] {
					if {[visible_value $w]} continue
					screen dyn-script $w "$n: [snippet $c]"
				}
				# Code-looking literals that didn't even parse speculatively
				foreach w [lrange $ws 1 end] {
					if {
						[domNode $w getAttribute quoted ""] eq "brace" &&
						[llength [parsetcl xpath $w as]] == 0 && ![dead_word $w] &&
						[string first \n [domNode $w getAttribute value ""]] >= 0 &&
						[looks_like_code [domNode $w getAttribute value]]
					} {
						screen unparsed-code $w "[expr {$n eq "" ? [snippet [lindex $ws 0]] : $n}] word [lsearch -exact $ws $w]"
					}
				}
			}
		}
		# Expressions: octal literals, int()/wide() with bit ops
		foreach se [parsetcl xpath $root {//as/expr//subexpr[@value]}] {
			# String comparisons don't parse numbers
			if {[domNode $se selectNodes {string(parent::operator/@name)}] in {eq ne in ni lt gt le ge}} continue
			if {[octal_differs [domNode $se getAttribute value]]} {hit octal $se "expr: [snippet [lindex [parsetcl xpath $se {ancestor::expr[1]}] 0]]"}
		}
		foreach ex [parsetcl xpath $root {//as/expr[.//operator[@name='int' or @name='wide']][.//operator[@name='<<' or @name='&' or @name='|' or @name='^' or @name='>>' or @name='~']]}] {
			hit int-wrap $ex "expr: [snippet $ex]"
		}
		foreach var [parsetcl xpath $root {//var}] {
			set name	[domNode $var getAttribute name ""]
			if {$name in {tcl_precision ::tcl_precision} || [string match *tcl_platform(threaded)* $name]} {
				hit tcl-vars $var [snippet $var]
			}
			set t	[text $var]
			# Nested substitutions ($a($b(c))) parse the same on 8 and 9: only
			# literal nested braces / parens changed (TIP 465)
			set t0	$t
			set t	"\$[string range $t 1 end]"
			while {[regsub -all {(.)\$(?:::)?[A-Za-z0-9_:]+(?:\([^()]*\))?} $t {\1X} t2] && $t2 ne $t} {set t $t2}
			set t	[string map {X {}} $t]
			if {[regexp {^\$\{[^\}]*\{} $t] || [regexp {^\$[^(]*\([^)]*\(} $t]} {hit varname-nest $var $t0}
		}
		foreach w [parsetcl xpath $root {//word}] {
			set v	[domNode $w getAttribute value ""]
			if {$v in {tcl_precision ::tcl_precision tcl_platform(threaded) ::tcl_platform(threaded)}} {
				hit tcl-vars $w $v
			}
		}
	}

	#>>>
	proc check_text {} { #<<<
		# Source-text checks: escapes are gone from parsed values
		variable src
		variable srcbytes
		variable file
		variable hits
		foreach m [regexp -all -indices -inline {\\u[dD][89abAB][0-9a-fA-F]{2}|\\u[dD][c-fC-F][0-9a-fA-F]{2}|\\u[fF]{4}} $src] {
			lappend hits [list astral $file [line_of [string length [encoding convertto utf-8 [string range $src 0 [lindex $m 0]-1]]]] [string range $src [lindex $m 0] [lindex $m 1]]]
		}
	}

	#>>>
	variable screen_kinds	{bytes convert chan-encoding clock-free int-wrap string-is-int}
	variable screen_items	{}
	variable questions {
		bytes			"Tcl 9 byte commands (binary scan/encode, zlib, md5/sha*) throw on strings containing characters above U+00FF (Tcl 8 silently used the low byte). Can the flagged data argument hold such characters, or is it always bytes (encoding convertto / binary format / crypto output / binary-mode channel read) or plain ASCII?"
		convert			"Tcl 9 encoding convertfrom/convertto default to the strict profile: invalid input bytes (convertfrom) or characters the target encoding can't represent (convertto) now throw instead of being silently mapped. Can the input here be invalid / unencodable (external input: HTTP, files, sockets, user data), and if so does a failure matter (is it caught)?"
		chan-encoding	"Tcl 9 channels default to the strict profile: reading invalid bytes for the channel's encoding, or writing characters it can't represent, throws. Can that happen on this channel?"
		clock-free		"Tcl 9 free-form clock scan validates the result by default, and checks a weekday name against the date after any relative offset (\"Tue ... 30 days\" fails). Can the scanned string contain a weekday name, an impossible date, or other input Tcl 9 rejects?"
		int-wrap		"Tcl 9 int()/wide() no longer truncate to 32/64 bits. Does this expression rely on truncation or wraparound (hashes, checksums, bit manipulation)?"
		string-is-int	"Tcl 9 string is integer/int/wide accepts digit separators (1_000) and integers of any size. Does a value that passes this check flow somewhere that needs a bounded integer or plain digits (SQL int columns, array sizes, format, external APIs)?"
		dyn-script		"This script-taking position gets a non-literal value, so the static scan never saw the code that runs here. Trace where the value comes from within the context. If it's built from literal code visible in the context, does that code use any Tcl 9 breaking construct (see the checklist)? If the code comes from elsewhere (a caller, a variable set outside the context), say so."
		unparsed-code	"This multi-line braced literal wasn't parsed as code. Is it Tcl code (vs data: SQL, JSON, HTML, C, parse_args spec, text)? If it's code, which command runs it, and does it use any Tcl 9 breaking construct (see the checklist)?"
	}
	variable checklist "Tcl 9 breaking constructs: case; puts chan str nonewline / read chan nonewline; trace variable|vdelete|vinfo; string bytelength; -encoding binary; -eofchar {in out}; tcl_precision; tcl_platform(threaded); ~ in paths (no tilde expansion); 0-prefixed integers are decimal (010 == 10, 08 valid); relative namespace variable names (a::b inside a namespace) and unqualified names at namespace level no longer fall back to the global namespace; glob no longer errors on no match; byte commands throw on chars > U+00FF; strict encoding errors on channels/encoding convert*; clock scan validation; int() no truncation; load prefix case-sensitive."

	proc context node { #<<<
		# Source of the enclosing proc/method/lambda body (or +-25 lines),
		# with line numbers, capped at 80 lines centred on $node
		variable srcbytes
		variable nl_offsets
		set line	[line_of [domNode $node getAttribute idx]]
		set scope	[parsetcl xpath $node {ancestor::command[@name='proc' or @name='method' or @name='constructor' or @name='rpc_proc' or @name='rl_formproc' or @name='oo::define' or @name='apply'][1]}]
		if {[llength $scope]} {
			set sc		[lindex $scope 0]
			set from	[line_of [domNode $sc getAttribute idx]]
			set to		[line_of [expr {[domNode $sc getAttribute idx] + [domNode $sc getAttribute len]}]]
		} else {
			set from	[expr {$line - 25}]
			set to		[expr {$line + 25}]
		}
		if {$to - $from > 80} {
			set from	[expr {max($from, $line - 40)}]
			set to		[expr {$from + 80}]
		}
		set from	[expr {max(1, $from)}]
		set lines	[split [encoding convertfrom -profile replace utf-8 $srcbytes] \n]
		set to		[expr {min($to, [llength $lines])}]
		set out		{}
		for {set i $from} {$i <= $to} {incr i} {
			append out [format "%5d%s %s\n" $i [expr {$i == $line ? ">" : ":"}] [lindex $lines $i-1]]
		}
		set out
	}

	#>>>
	proc screen {kind node detail} { #<<<
		variable screen_items
		variable file
		variable screening
		if {!$screening} return
		lappend screen_items [list $kind $file [line_of [domNode $node getAttribute idx]] $detail [context $node]]
	}

	#>>>
	proc dyn_script_words {c n ws v} { #<<<
		# Words in script positions of well-known commands that aren't literal
		set sub	[lindex $v 1]
		set nw	[llength $ws]
		set pos	{}
		switch -exact -- $n {
			eval - uplevel - subst {
				set first	1
				if {$n eq "uplevel" && [regexp {^#?[0-9]+$} [lindex $v 1]]} {set first 2}
				for {set i $first} {$i < $nw} {incr i} {lappend pos $i}
			}
			namespace {
				if {$sub in {eval inscope} && $nw >= 4} {for {set i 3} {$i < $nw} {incr i} {lappend pos $i}}
			}
			interp {if {$sub eq "eval" && $nw >= 4} {lappend pos 3}}
			after {if {$nw >= 3 && ([lindex $v 1] eq "idle" || [string is entier -strict [lindex $v 1]])} {lappend pos 2}}
			thread::send {
				set i 1
				while {[lindex $v $i] in {-async -head}} {incr i}
				if {$nw > $i+1} {lappend pos [expr {$i+1}]}
			}
			apply {lappend pos 1}
			if - while - for - foreach - lmap - catch - try - time - proc {
				# bodies: the last word (and catch's first), by convention braced
				switch -- $n {
					catch {if {$nw >= 2} {lappend pos 1}}
					proc {if {$nw == 4} {lappend pos 3}}
					default {lappend pos [expr {$nw-1}]}
				}
			}
		}
		set out {}
		foreach i $pos {
			set w	[lindex $ws $i]
			if {$w eq "" || [static $w]} continue
			# [list ...] built commands are visible to the cmd_parsers when
			# their pieces are literal; skip pure [list literal ...]
			if {[llength [parsetcl xpath $w {.//as/script}]]} continue
			lappend out $w
		}
		set out
	}

	#>>>
	variable spec_ranges	{}		;# idx/len of literals parsed speculatively
	variable spec_count		0
	proc speculative node { #<<<
		variable spec_ranges
		set idx	[domNode $node getAttribute idx 0]
		foreach {from to} $spec_ranges {
			if {$idx >= $from && $idx < $to} {return 1}
		}
		return 0
	}

	#>>>
	proc dead_word w { #<<<
		# The body of an [if 0 {...}] (commented-out code)
		set c	[domNode $w parentNode]
		expr {
			[domNode $c nodeName] eq "command" && [cmdname $c] eq "if" &&
			[lit [lindex [words $c] 1]] in {0 false}
		}
	}

	#>>>
	proc speculate root { #<<<
		# Parse multi-line braced literals that look like code but that no
		# cmd_parser parsed (scripts kept in variables, templates, arguments of
		# commands the deep parse doesn't know), so the checks see them.
		# Repeat for literals newly exposed inside those.
		variable spec_ranges
		variable spec_count
		set done	{}
		while 1 {
			set new	0
			foreach w [parsetcl xpath $root {//word[@quoted='brace'][not(as)]}] {
				set v	[domNode $w getAttribute value ""]
				if {[string first \n $v] < 0 || [dict exists $done $w]} continue
				dict set done $w 1
				if {[dead_word $w]} continue
				if {
					[string is list $v] && [llength $v] in {2 3} &&
					[string first \n [lindex $v 0]] < 0 && [looks_like_code [lindex $v 1]]
				} {
					::parsetcl::lambda_word $w		;# {args body ?ns?}
				} elseif {[looks_like_code $v]} {
					::parsetcl::subparse script $w
				} else continue
				if {[llength [parsetcl xpath $w {.//as/script}]]} {
					set idx	[domNode $w getAttribute idx]
					lappend spec_ranges $idx [expr {$idx + [domNode $w getAttribute len]}]
					incr spec_count
					set new 1
				}
			}
			if {!$new} break
		}
	}

	#>>>
	proc scope_params node { #<<<
		# Formal parameter names of the proc / method / lambda enclosing $node
		set sc	[lindex [parsetcl xpath $node {ancestor::command[@name='proc' or @name='method' or @name='constructor' or @name='rpc_proc' or @name='rl_formproc'][1]}] 0]
		set out	{}
		if {$sc ne ""} {
			set ws	[words $sc]
			set aw	[lindex $ws [expr {[cmdname $sc] eq "constructor" ? 1 : [llength $ws] - 2}]]
			if {[static $aw] && [string is list [lit $aw]]} {
				foreach a [lit $aw] {lappend out [lindex $a 0]}
			}
		}
		# parse_args $args {name spec -opt spec ...}: names declared there
		if {$sc ne ""} {
			foreach spec [parsetcl xpath $sc {.//command[@name='parse_args' or @name='parse_args::parse_args' or @name='::parse_args::parse_args']/word[3][@value]}] {
				set sv	[lit $spec]
				if {[string is list $sv] && [llength $sv] % 2 == 0} {
					foreach {n -} $sv {lappend out [string trimleft $n -]}
				}
			}
		}
		# Enclosing lambdas: {args body} words parsed as lambdas
		foreach l [parsetcl xpath $node {ancestor::word[as/list/word[2]/as/script]}] {
			set a	[lindex [parsetcl xpath $l {as/list/word[1]}] 0]
			if {$a ne "" && [static $a] && [string is list [lit $a]]} {
				foreach p [lit $a] {lappend out [lindex $p 0]}
			}
		}
		set out
	}

	#>>>
	proc visible_value w { #<<<
		# 1 if dynamic script word $w is a pass-through of a parameter (its code
		# is at the call sites), or a variable the scope sets from a literal
		# (parsed directly or speculatively)
		set parts	[parsetcl xpath $w {*[not(self::syntax)]}]
		if {[llength $parts] != 1} {return 0}
		# [list cmd arg ...] with a literal command name runs that command:
		# nothing hidden (unless cmd takes scripts, which the parse covers)
		if {[domNode [lindex $parts 0] nodeName] eq "script"} {
			return [llength [parsetcl xpath [lindex $parts 0] {self::script[count(command)=1]/command[@name='list' and word[2]/@value]}]]
		}
		if {[domNode [lindex $parts 0] nodeName] ne "var"} {return 0}
		set name	[domNode [lindex $parts 0] getAttribute name ""]
		if {$name in [scope_params $w]} {return 1}
		set scope	[scope_of $w]
		set sets	[parsetcl xpath $scope [format {.//command[@name='set' and count(word)=3 and word[2]/@value='%s']/word[3]} $name]]
		if {![llength $sets]} {return 0}
		foreach v $sets {
			if {![static $v] && ![llength [parsetcl xpath $v {.//as/script}]]} {return 0}
		}
		return 1
	}

	#>>>
	proc looks_like_code body { #<<<
		# Heuristic: several lines starting with common Tcl commands
		set n	0
		foreach line [split $body \n] {
			if {[regexp {^\s*(set|if|foreach|proc|return|lappend|dict|incr|puts|while|switch|try|catch|expr|my|variable|upvar|global|namespace|append|unset|lassign|json|db|rl_[a-z_]+)\s} $line]} {incr n}
		}
		expr {$n >= 2}
	}

	#>>>
	proc files paths { #<<<
		set out	{}
		foreach p $paths {
			if {[file isdirectory $p]} {
				foreach f [lsort [glob -nocomplain -directory $p *]] {
					if {[file isdirectory $f]} {
						if {[file tail $f] in {.git node_modules}} continue
						if {[file type $f] eq "link"} continue
						lappend out {*}[files [list $f]]
					} elseif {[string match *.tcl $f] || [string match *.tm $f]} {
						if {[file isfile $f]} {lappend out $f}
					} elseif {![file isfile $f]} {
						continue
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
		set eof	[string first \x1A $raw]
		set stub	"apply \{\{\} \{set h \[open \[info script\] rb\]"
		if {$eof > 0 && [string equal -length [string length $stub] $stub $raw] && [string first brotli [string range $raw 0 $eof]] >= 0} {
			package require brotli
			return [encoding convertfrom utf-8 [brotli::decompress [string range $raw $eof+1 end]]]
		}
		if {$eof >= 0} {set raw [string range $raw 0 $eof-1]}
		if {[catch {encoding convertfrom -profile strict utf-8 $raw} text]} {
			# source fails on this under Tcl 9 (utf-8, strict)
			variable hits
			lappend hits [list not-utf8 $fn 0 "source would fail: not valid utf-8"]
			set text [encoding convertfrom iso8859-1 $raw]
		}
		set text
	}

	#>>>
	proc write_screen {dir batch} { #<<<
		package require rl_json
		variable screen_items
		variable questions
		variable checklist
		file mkdir $dir
		# One item per distinct (kind, flagged source): the first context, and
		# every location
		set groups	{}
		foreach it [lsort -unique $screen_items] {
			lassign $it kind fn line detail ctx
			set key	[list $kind [regsub -all {\s+} $detail { }]]
			if {![dict exists $groups $key]} {dict set groups $key [list $kind $fn $line $detail $ctx {}]}
			dict set groups $key [lreplace [dict get $groups $key] 5 5 [list {*}[lindex [dict get $groups $key] 5] $fn:$line]]
		}
		set items	[dict values $groups]
		set n		0
		set counts	{}
		for {set i 0} {$i < [llength $items]} {incr i $batch} {
			set arr	{[]}
			foreach it [lrange $items $i [expr {$i + $batch - 1}]] {
				lassign $it kind fn line detail ctx locs
				dict incr counts $kind
				set more	[lmap l [lrange $locs 1 end] {::rl_json::json string $l}]
				::rl_json::json set arr end+1 [::rl_json::json template {
					{
						"id":		"~S:id",
						"kind":		"~S:kind",
						"file":		"~S:fn",
						"line":		"~N:line",
						"flagged":	"~S:detail",
						"context":	"~S:ctx",
						"same_code_also_at":	"~J:more"
					}
				} [dict create id [incr n] kind $kind fn $fn line $line detail $detail ctx $ctx more "\[[join $more ,]\]"]]
			}
			set qs	{{}}
			foreach k [lsort -unique [lmap it [lrange $items $i [expr {$i + $batch - 1}]] {lindex $it 0}]] {
				set q	[expr {[dict exists $questions $k] ? [dict get $questions $k] :
					"The scan found a Tcl 9 breaking construct ($k, see the checklist) inside a braced literal it only guessed is code. Is the literal really Tcl code that runs (vs data or a comment), and is the construct a real Tcl 9 problem there?"}]
				::rl_json::json set qs $k [::rl_json::json string $q]
			}
			set doc	[::rl_json::json template {
				{
					"instructions":	"For each item, answer the question for its kind using the context (source lines, '>' marks the flagged line). Give a verdict: safe (no Tcl 9 problem), bug (a Tcl 9 problem: explain the failing case), or human (can't tell from the context: say what would need checking). Be concrete and brief.",
					"checklist":	"~S:checklist",
					"questions":	"~J:qs",
					"items":		"~J:arr"
				}
			} [dict create checklist $checklist qs $qs arr $arr]]
			set h	[open [file join $dir [format batch-%03d.json [expr {$i / $batch + 1}]]] w]
			puts $h [::rl_json::json pretty $doc]
			close $h
		}
		puts "Screening items: [llength $items] in [expr {([llength $items] + $batch - 1) / $batch}] batches in $dir ($counts)"
	}

	#>>>
	proc main argv { #<<<
		variable hits
		variable file
		variable nl_offsets
		variable src
		variable requires
		set kinds	{}
		set skip	{}
		set summary	0
		set reqfile	""
		set screendir	""
		set batch	40
		set paths	{}
		for {set i 0} {$i < [llength $argv]} {incr i} {
			set a	[lindex $argv $i]
			switch -exact -- $a {
				-parsers	{uplevel #0 [list source [lindex $argv [incr i]]]}
				-kinds		{set kinds [split [lindex $argv [incr i]] ,]}
				-skip		{set skip [split [lindex $argv [incr i]] ,]}
				-summary	{set summary 1}
				-requires	{set reqfile [lindex $argv [incr i]]}
				-screen		{set screendir [lindex $argv [incr i]]}
				-batch		{set batch [lindex $argv [incr i]]}
				default		{lappend paths $a}
			}
		}
		variable screening	[expr {$screendir ne ""}]
		set fns		[files $paths]
		set failed	{}
		foreach fn $fns {
			set file	$fn
			set src		[read_file $fn]
			variable srcbytes	[encoding convertto utf-8 $src]
			set nl_offsets	[lmap m [regexp -all -indices -inline \n $srcbytes] {lindex $m 0}]
			if {[catch {parsetcl parsetree $src} pt]} {
				lappend failed [list $fn [lindex [split $pt \n] 0]]
				continue
			}
			variable spec_ranges	{}
			check_tree [parsetcl node $pt]
			check_text
			unset pt
		}
		if {$reqfile ne ""} {
			set h	[open $reqfile w]
			dict for {r locs} $requires {puts $h [list $r $locs]}
			close $h
		}
		if {$screendir ne ""} {write_screen $screendir $batch}
		puts "Scanned [llength $fns] files ([llength $failed] didn't parse)"
		variable spec_count
		puts "Braced literals parsed speculatively as code: $spec_count"
		set by	{}
		foreach h $hits {dict lappend by [lindex $h 0] $h}
		set order {not-utf8 case puts-nonewline read-nonewline trace-old bytelength enc-binary enc-unknown eofchar-pair inject pkg-tcl8 load-prefix octal tilde glob-catch bytes convert chan-encoding clock-free int-wrap string-is-int tcl-vars astral varname-nest internal}
		foreach k [concat $order [lmap k [dict keys $by] {if {$k in $order} continue; set k}]] {
			if {![dict exists $by $k]} continue
			if {[llength $kinds] && $k ni $kinds} continue
			if {$k in $skip} continue
			set hs	[lsort -unique [dict get $by $k]]
			puts "\n== $k: [llength $hs]"
			if {$summary} {
				set perfile	{}
				foreach h $hs {dict incr perfile [lindex $h 1]}
				foreach {f c} [lsort -stride 2 -index 1 -integer -decreasing $perfile] {puts [format "  %4d  %s" $c $f]}
				continue
			}
			foreach h [lsort -dictionary -index 1 [lsort -integer -index 2 $hs]] {
				lassign $h - fn line detail
				puts "  $fn:$line  $detail"
			}
		}
		variable subparse_failed
		if {[dict size $subparse_failed]} {puts "\nWords a cmd_parser took for code that didn't parse as such: $subparse_failed"}
		if {[llength $failed]} {
			puts "\nDidn't parse:"
			foreach f $failed {puts "  [join $f {: }]"}
		}
	}

	#>>>
}

compat::main $argv

# vim: ft=tcl foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4
