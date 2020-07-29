#!/usr/bin/env tclsh8.7
# vim: ft=tcl foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4

if {[file system [info script]] eq "native"} {
	package require platform

	foreach platform [platform::patterns [platform::identify]] {
		set tm_path		[file join $env(HOME) .tbuild repo tm $platform]
		set pkg_path	[file join $env(HOME) .tbuild repo pkg $platform]
		if {[file exists $tm_path]} {
			tcl::tm::path add $tm_path
		}
		if {[file exists $pkg_path]} {
			lappend auto_path $pkg_path
		}
	}
}

package require rl_json
interp alias {} json {} ::rl_json::json


namespace eval ::parsetcl {
	variable emitstack	{}

	namespace path {
		::tcl::mathop
	}

	# Helpers <<<
	proc ++post v { # Post-increment $v.  Works like v++ in c / js <<<
		upvar 1 $v $v
		set pre	$v
		incr v
		set pre
	}

	#>>>
	# Helpers >>>

	proc closure {name args script} { # Fake closure for parse state variables <<<
		tailcall proc $name $args "upvar 1 i i text text tokstart tokstart token token tokens tokens; $script"
	}

	#>>>
	proc closurelambda {l args} { # Fake closure lambda generator <<<
		set script	[lindex $l 1]
		lset l 1 "upvar 1 i i text text tokstart tokstart token token tokens tokens; $script"
		list apply $l {*}$args
	}

	#>>>

	proc toklength tok { #<<<
		switch -exact -- [json get $tok 0] {
			INDEX {
				set len	0
				foreach t [json get $tok 1] {
					incr len	[toklength $t]
				}
				set len
			}

			SCRIPT {
				set tokens	[all_script_tokens [json get $tok 1]]
				set len		0
				foreach t $tokens {
					incr len	[toklength $t]
				}
				set len
			}

			default {
				string length [json get $tok 1]
			}
		}
	}

	#>>>
	closure emit args { #<<<
		set tok	[switch -- [llength $args] {
			2 {
				lassign $args toktype detail
				json template {
					{
						"0":	"~S:toktype",
						"3":	"~N:tokstart",
						"meta":	{}
					}
				}
			}

			3 {
				lassign $args toktype detail crep
				json template {
					{
						"0":	"~S:toktype",
						"2":	"~S:crep",
						"3":	"~N:tokstart",
						"meta":	{}
					}
				}
			}

			default {
				error "wrong # of args, must be 2 or 3"
			}
		}]
		json set tok 1 $detail	;# $detail may be a string or JSON
		# tokstart += (tok.meta.toklength = toklength(tok));  Always 0 (tok.meta.toklength is a const null here)
		json set tokens end+1 $tok
	}

	#>>>
	closure emit_waiting type { #<<<
		if {$token ne ""} {
			emit $type [json string $token]
		}
	}

	#>>>
	closure literal {crep {len {}}} { # Only called within parse_escape <<<
		upvar 1 first first
		set last	[expr {$first + $len eq "" ? 1 : $len}]
		emit_waiting TEXT
		set subtext	[string range $text $first $last]
		emit ESCAPE [json string $subtext] $crep
		set i	[+ $last 1]
	}

	#>>>
	closure charcode {code len} { # Only called within parse_escape <<<
		upvar 1 first first
		literal [format %c $code] $len
	}

	#>>>
	closure parse_escape {} { #<<<
		set first	[+ $i 1]

		switch -exact -- [string index $text [++post i]] {
			{}  {append token "\\"}
			a   {charcode 0x7}
			b   {charcode 0x8}
			f   {charcode 0xc}
			n   {charcode 0xa}
			r   {charcode 0xd}
			t   {charcode 0x9}
			v   {charcode 0xb}

			x {
				if {[regexp -start $i {^[0-9A-Fa-f]+/} $text escapechars]} {
					charcode [& 0x$escapechars 0xff] [+ 1 [string length $escapechars]]
				} else {
					literal x
				}
			}

			u {
				if {[regexp -start $i {^[0-9A-Fa-f]{1,4}/} $text escapechars]} {
					charcode 0x$escapechars [+ 1 [string length $escapechars]]
				} else {
					literal u
				}
			}

			"\n" {
				# Line folding
				if {[regexp -start i {^[ \t]*} $text match]} {
					literal { } [+ 1 [string length $match]]
				} else {
					literal { }
				}
			}

			default {
				incr i -1

				if {[regexp -start $i {^[0-7]{1,3}} $text escapechars]} {
					set acc	0
					foreach char [split $escapechars {}] {
						set acc		[expr {($acc << 3) + $char}]
					}
					charcode $acc [string length $escapechars]
				} else {
					literal [string index $text $i]
				}
			}
		}
	}

	#>>>
	closure parse_commands {} { #<<<
		set cmd		{[]}
		set cmds	{[]}

		emit_waiting TEXT
		set c	[string index $text [++post i]]
		emit SYNTAX [json string $c]
		set savetokstart	$tokstart
		while 1 {
			set savetokens	$tokens
			set word		[get_word [eq $cmd ""] true]
			set tokens		$savetokens
			json set cmd end+1 $word
			set lasttoken	[json extract $word end]
			if {[json isnull $lasttoken]} {
				throw [list PARSETCL PARSE COMMAND_NOT_TERMINATED $i $text $ofs] "Cannot find end of command"
			}
			if {[json get $lasttoken 0] eq "END"} {
				json set cmds end+1 $cmd
				set cmd	{[]}
				if {[json get $lasttoken 1] eq "\]" || [json get $lasttoken 1] eq ""} break
			}
		}
		set tokstart	$savetokstart
		emit SCRIPT $cmds
	}

	#>>>
	closure parse_index {} { # Only called from within parse_variable <<<
		# escape, variable and command substs apply here
		set c	[string index $text [++post i]]
		emit SYNTAX [json string $c]
		set saved_tokens	$tokens
		set saved_tokstart	$tokstart
		set tokens			{[]}
		while 1 {
			set c	[string index $text $i]
			switch -exact -- $c {
				{} {
					throw [list PARSETCL PARSE MISSING_END_ARRAY $i $text $ofs] "missing )"
				}

				) {
					emit_waiting TEXT
					set indextokens		$tokens
					set tokens			$saved_tokens
					set tokstart		$saved_tokstart
					emit INDEX $indextokens
					set c	[string index $text [++post i]]
					emit SYNTAX [json string $c]
					return
				}

				"\\"   parse_escape
				"\$"   parse_variable
				"\["   parse_commands

				default {
					append token $c
					incr i
				}
			}
		}
	}

	#>>>
	closure parse_variable {} { #<<<
		if {![regexp -start [+ $i 1] {^(?:[a-zA-Z0-9_{(]|::)} $text]} {
			append token	[string index $text [++post i]]
			return
		}

		emit_waiting TEXT
		set c	[string index $text [++post i]]
		emit SYNTAX [json string $c]

		if {[string index $text $i] eq "\{"} {
			set c	[string index $text [++post i]]
			emit SYNTAX [json string $c]
			set idx	[string first "\}" $text $i]
			if {$idx == -1} {
				throw [list PARSETCL PARSE MISSING_VARIABLE_CLOSE_BRACE $i $text $ofs] "missing close-brace for variable name"
			}
			set token	[string range $text $i $idx-1]
			set i		$idx
			if {[string index $token end] eq ")" && [set idx [string last "(" $token]] != -1} {
				set save_i	$i
				set i		[- $i [string length $token]]
				set token	[string range $token 0 $idx-1]
				incr i		[string length $token]
				emit ARRAY [json string $token]
				parse_index
				set i	$save_i
			} else {
				emit VAR [json string $token]
			}
			set c	[string index $text [++post i]]
			emit SYNTAX [json string $c]
		} else {
			regexp -start $i {^[a-zA-Z_0-9:]*} $text token
			# : alone is a name terminator
			set idx		[string first : [string map {:: __} $token]]
			if {$idx > 0} {
				set token	[string range $token 0 $idx-1]
			}
			incr i	[string length $token]
			if {[string index $text $i] ne "("} {
				emit VAR [json string $token]
			} else {
				emit ARRAY [json string $token]
				parse_index
			}
		}
	}

	#>>>
	closure emit_fold len { # Only called from parse_braced <<<
		upvar 1 from from
		set subtext		[string range $text $i [+ $i $len -1]]
		emit ESCAPE [json string $subtext] { }
		incr i $len
		set from	$i
	}

	#>>>
	closure parse_braced {} { #<<<
		set depth	1
		set emitted	false

		set c	[string index $text [++post i]]
		emit SYNTAX [json string $c]
		set from	$i

		while {$depth} {
			if {![regexp -start $i -indices {(\\*?)?(?:(\{|\})|(\\\n[ \t]*))} $text m0 m1 m2 m3]} {
				throw [list PARSETCL PARSE MISSING_CLOSE_BRACE [+ $i -1] $text $ofs] "missing close-brace"
			}
			set m1len	[- {*}$m1 -1]
			if {$m1len % 2 == 1} {
				# The text we found was backquoted, move along
				set m0len	[- {*}$m0 -1]
				incr i		[+ [lindex $m0 0] $m0len]
				continue
			}
			incr i	[+ [lindex $m0 0] $m1len]
			if {$m3 ne ""} {
				# line fold
				if {$i > $from} {
					set subtext		[string range $text $from $i-1]
					emit TEXT [json string $subtext]
				}
				set m3len	[- {*}$m3 -1]
				emit_fold $m4len
				set	emitted	true
			} else {
				if {[string index [lindex $m2 0]] eq "\{"} {
					incr depth
				} else {
					incr depth -1
				}
				incr i
			}
		}
		incr i -1
		if {!$emitted || $i > $from} {
			set subtext		[string range $text $from $i-1]
			emit TEXT [json string $subtext]
		}
		set c	[string index $text [++post i]]
		emit SYNTAX [json string $c]

		set tokens
	}

	#>>>
	closure parse_combined {quoted incmdsubst ignore_trailing} { #<<<
		set start	$i

		if {$quoted} {
			set c	[string index $text [++post i]]
			emit SYNTAX [json string $c]
		}

		while 1 {
			set matched		true

			if {$quoted} {
				switch -exact -- [string index $text $i] {
					{} {
						throw [list PARSETCL PARSE MISSING_QUOTE $start $text $ofs] "missing \""
					}

					"\"" {
						if {$incmdsubst} {
							set re	{[\s;\]]}
						} else {
							set re	{[\s;]}
						}
						set starti	[+ $i 1]
						if {
							!$ignore_trailing &&
							[string index $text $start_i] ne "" &&
							![regexp -start $start_i {^\\\n} $text] &&
							![regexp -start $start_i $re]
						} {
							set lineno	[string length [regsub -all {[^\n]+} [string range $text 0 $i-1] {}]]
							#puts "i: $i, ([string range $text 0 99]) line: $lineno: [string range $text $i-5 $i+4]"
							throw [list PARSETCL PARSE EXTRA_CHARS_AFTER_CLOSE_QUOTE $starti $text $ofs] "extra characters after close-quote"
						}
						if {$i == $start + 1} {
							# Need to manually emit rather than using
							# emit_waiting because we still need it if
							# token eq {}
							emit TEXT [json string $token]
						} else {
							emit_waiting TEXT
						}
						emit SYNTAX [json string [string index $text [++post i]]]
						return $tokens
					}

					default {
						set matched		false
					}
				}
			} else {
				switch -exact -- [string index $text $i] {
					{} {
						emit_waiting TEXT
						emit END {""}
						return $tokens
					}

					"\n" - ";" {
						emit_waiting TEXT
						set token	[string index $text [++post i]]
						emit END [json string $token]
						return $tokens
					}

					"\\" {
						if {[string index $text $i+1] ne "\n"} {
							set matched		false
						} else {
							# Line fold - falls through
							emit_waiting TEXT
							return $tokens
						}
					}

					{ } - "\t" {
						emit_waiting TEXT
						return $tokens
					}

					default {
						set matched		false
					}
				}
			}

			if {!$matched} {
				switch -exact -- [string index $text $i] {
					"\\"   parse_escape
					"\$"   parse_variable
					"\["   parse_commands
					"\]" {
						set c	[string index $text [++post i]]
						if {$incmdsubst && !$quoted} {
							emit_waiting TEXT
							set token	$c
							emit END [json string $token]
							return $tokens
						}
						append token $c
					}

					default {
						append token	[string index $text [++post i]]
					}
				}
			}
		}
	}

	#>>>
	closure get_word {first incmdsubst} { #<<<
		set tokens	{[]}
		set token	{}
		if {$first} {
			set re	{^(?:[\t \n]*\\\n[\t \n]*)|^[\t \n]+}
		} else {
			set re	{^(?:[\t ]*\\\n[\t ]*)|^[\t ]+}
		}

		# Consume any leading whitespace / comments if first word
		set m	{}
		set c	[string index $text $i]
		while {
			($first && [set c [string index $text $i]] eq "#") ||
			([regexp -start $i $re $text m])
		} {
			if {$m ne ""} {
				append token $m
				incr i	[string length $m]
				set c	[string index $text $i]
			}
			emit_waiting SPACE
			if {$first && $c eq "#"} {
				set textlen	[string length $text]
				while ($i < $textlen) {
					set c	[string index $text $i]
					if {$c eq "\\" && $i < $textlen-1} {
						append token	$c
						incr i
						set c	[string index $text $i]
					}
					append token $c
					incr i
					set c	[string index $text $i]
					if {$c eq "\n"} {
						append token	$c
						incr i
						set c	[string index $text $i]
						break
					}
				}
				emit COMMENT [json string $token]
			}
			set m	{}
		}

		# handle {*}
		if {$c eq "\{" && [regexp -start $i {^{\*}} $text]} {
			emit EXPAND {"{*}"}
			incr i 3
			set c	[string index $text $i]
		}

		switch -exact -- $c {
			{}    {return $tokens}
			"\{"  {return [parse_braced]}
			"\""  {return [parse_combined true $incmdsubst]}
			"\]" {
				if {$incmdsubst} {
					emit END [json string [string index $text [++post i]]]
					return $tokens
				}
				return [parse_combined false $incmdsubst]
			}
			default {
				return [parse_combined false $incmdsubst]
			}
		}
	}

	#>>>
	closure emit_token {type value subtype crep) { # Only called from parse_subexpr <<<
		if {$value eq "" && $type ne "END"} {
			throw [list PARSETCL PARSE EMPTY_TOKEN $i $text $ofs] "Refusing to emit a token of length 0"
		}
		json set tokens end+1 [json template {
			["~S:type", "~S:subtype", "~S:crep", "~S:value"]
		}]
		incr tokstart	[string length $value]
		incr i			[string length $value]
	}

	#>>>
	closure parse_quoted {} { # Only called from parse_subexpr <<<
		parse_combined true false true
	}

	#>>>
	closure sub_parse {subtoken func {make_crep {}}} { # Only called from parse_subexpr <<<
		set s_tokens	$tokens
		set s_tokstart	$tokstart
		set s_i			$i
		set tokens	{[]}
		{*}$func
		set subtokens	$tokens
		set e_i			$i
		set tokens		$s_tokens
		set tokstart	$s_tokstart
		set i			$s_i
		if {$make_crep ne ""} {
			set crep	[{*}make_crep $subtokens]
		} else {
			set crep	$subtokens
		}
		emit_token OPERAND [string range $text $i $e_i-1] $subtoken $crep
	}

	#>>>
	closure sub_parse_arg {} { # Only called from parse_subexpr <<<
		set s_tokens	$tokens
		set s_i			$i
		set tokens		{[]}
		parse_subexpr true
		set subtokens	$tokens
		set tokens		$s_tokens;
		set e_i			$i
		set	i			$s_i
		emit_token ARG [string range $text $i $e_i-1] EXPR $subtokens
		json extract $subtokens end 3
	}

	#>>>
	closure parse_mathfunc {funcname space} { # Only called from parse_subexpr <<<
		set s_i			$i
		set s_tokens	$tokens
		set tokens		{[]}
		emit_token MATHFUNC $funcname
		if {$space} {emit_token SPACE $space}
		emit_token SYNTAX (;
		set term		,
		while {$term eq ","} {
			set term	[sub_parse_arg]
		}
		set subtokens	$tokens
		set tokens		$s_tokens
		set e_i			$i
		set i			$s_i
		emit_token OPERAND [string range $text $i $e_i-1] MATHFUNC $subtokens
	}

	#>>>
	closure parse_subexpr funcargs { #<<<
		#var here, m, found, j
		set expecting_operator	false

		# <|> are technically the same precedence as <=|>=, <=|>= needs to be matched against first
		set operators	{
			{^(?:~|!(?=[^=]))} 1
			{^\*\*}            2
			{^[*\/%]}          2
			{^[\-+]}           2
			{^(?:<<|>>)}       2
			{^(?:<=|>=)}       2
			{^(?:<|>)}         2
			{^(?:==|!=)}       2
			{^(?:ne|eq)}       2
			{^(?:in|ni)}       2
			{^&(?!&)}          2
			{^\^}              2
			{^\|(?!\|)}        2
			{^&&}              2
			{^\|\|}            2
			{^[?:]}            3
		}

		set textlen	[string length $text]
		while {$i < $textlen} {
			#set here	[string range $text $i end]		;# TODO: replace with relative indexing later

			# line folds
			if {[regexp -start $i {^\\\n\s+} $text m]} {
				emit_token SPACE $m
				continue
			}

			# whitespace
			if {[regexp -start $i {^\s+} $text m]} {
				emit_token SPACE $m
				continue
			}

			if {!$expecting_operator} {
				# Unitary + and -
				if {[regexp -start $i {^[\-+]} $text m]} {
					emit_token OPERATOR $m 0 1
					continue
				}
			}

			# operators, in decreasing precedence
			set found	false
			set j		-1
			foreach {re precedence} $operators {
				incr j
				if {[regexp -start $i $re $text m]} {
					emit_token OPERATOR $m $j $precedence
					set found				true
					set expecting_operator	false
					break
				}
			}
			if {$found} continue

			set expecting_operator	true

			# number
			if {[regexp -start $i -nocase {^(?:([\-+]?)(Inf(?:inity)?)|(NaN))\M} $text m m1]} {
				if {[string match -nocase ?n* $m]} {
					emit_token OPERAND $m FLOAT [expr {$m}]
				} else {
					emit_token OPERAND $m FLOAT [expr {$m}]
				}
				continue
			}

			if {
				[regexp -start $i -nocase {^([\-+])?(0x)([\dA-F]+)} $text m] ||
				[regexp -start $i -nocase {^([\-+])?(0b)([01]+)} $text m] ||
				[regexp -start $i -nocase {^([\-+])?(0o)([0-7]+)} $text m]
			} {
				# TODO: Bignum support
				emit_token OPERAND $m INTEGER [expr {$m+0}]
				continue
			}
			if {
				[regexp -start $i -nocase {^[\-+]?\d+(?:(\.)(?:\d+)?)?(e[\-+]?\d+)?} $text m m1 m2] ||
				[regexp -start $i -nocase {^[\-+]?(\.)\d+(e[\-+]?\d+)?} $text m m1 m2]
			} {
				if {$m1 eq "" && $m eq ""} {
					emit_token OPERAND $m INTEGER [expr {$m}]
				} else {
					emit_token OPERAND $m FLOAT   [expr {$m}]
				}
				continue
			}

			set c	[string index $text $i]
			switch -exact -- $c {
				{} {
					throw [list PARSETCL PARSE MISSING_OPERAND $i $text $ofs] "missing operand"
				}

				"\""  {sub_parse QUOTED parse_quoted;		continue}
				"\{"  {sub_parse BRACED parse_braced;		continue}
				"\$" {
					sub_parse VAR parse_variable [closurelambda {tokens {
						json foreach t $tokens {
							switch -exact -- [json get $t 0] {
								VAR {
									set d	[json extract $t 1]
									return [json template {["~J:d"]}]
								}
								ARRAY {
									set array	[json extract $t 1]
								}
								INDEX {
									set index	[json extract $t 1]
									if {[json length $index] == 1 && [json get $index 0 0] eq "TEXT"} {
										# Optimize the common case where the
										# index is a simple string
										set indexstr	[json extract $index 0 1]
										return [json template {
											["~J:array", "~J:indexstr"]
										}]
									} else {
										# Index needs runtime resolution
										set indextoks	[json extract $t 1]
										return [json template {
											["~J:array", "~J:indextoks"]
										}]
									}
								}
							}
						}
						throw [list PARSETCL PARSE NO_VARIABLE_FOUND $i $text $ofs] "No variable found"
					}}]
					continue
				}
				"\[" {
					sub_parse SCRIPT parse_commands [closurelambda {tokens {
						json foreach t $tokens {
							if {[json get $t 0] eq "SCRIPT"} {
								return $t
							} elseif {[json get $t 0] eq "SYNTAX"} {
								# Dirty hack to inject the [ syntax token
								emit_token SYNTAX [json get $t 1]
							}
						}
						throw [list PARSETCL PARSE NO_SCRIPT_FOUND $i $text $ofs] "No script found"
					}}]
					continue
				}
				"(" {emit_token LPAREN $c;			continue}
				")" {
					if {$funcargs} {
						emit_token SYNTAX $c
						return
					}
					emit_token RPAREN $c
					continue
				}
			}
			if {$funcargs} {
				if {$c eq ","} {
					emit_token SYNTAX $c
					return
				}
			}
			# mathfunc
			if {[regexp -start $i {^(\w+)(\s*)?\(} $text m m1 m2]} {
				parse_mathfunc $m1 $m2
				continue
			}
			# boolean
			if {[regexp -start $i -nocase {^(?:tr(?:ue?)?|yes?|on)\M} $text m]} {
				emit_token OPERAND $m BOOL true
				continue
			}
			if {[regexp -start $i -nocase {^(?:fa(?:l(?:se?)?)?|no|off?)\M} $text m]} {
				emit_token OPERAND $m BOOL false
				continue
			}
			# invalid bareword
			if {[regexp -start $i {^\w+\M} $text m]} {
				throw [list PARSETCL PARSE EXPR_BAREWORD $i $text $ofs] "invalid bareword \"$m\""
			}
			#puts stderr "Cannot parse expression portion: \"[string range $text $i end]]\""
			throw [list PARSETCL PARSE EXPR_GIVEUP $i $text $ofs] "Cannot parse expression portion: \"[string range $text $i $i+5]...\""
		}
	}

	#>>>
	proc is_whitespace c { #<<<
		# Definitive whitespace list from http://tip.tcl.tk/407
		switch -exact -- $c {
			{} -
			"\t" -
			"\n" -
			"\v" -
			"\f" -
			"\r" -
			{ } {
				return true
			}

			default {
				return false
			}
		}
	}

	#>>>
	closure parse_list_element cx { #<<<
		set start	$i
		set depth	1

		if {$cx ne ""} {
			emit SYNTAX [string index $text [++post i]]
		}

		while 1 {
			if {$cx eq "\""} {
				switch -exact [string index $text $i] {
					{} {
						throw [list PARSETCL PARSE MISSING_QUOTE $start $text $ofs] "missing \""
					}

					"\"" {
						if {![is_whitespace [string index $text $i+1]]} {
							throw [list PARSETCL PARSE LIST_ELEMENT_TRAILING_GARBAGE [+ $i 1] $text $ofs] "list element in quotes followed by \"[string index $text $i+1]\" instead of space"
						}
						emit_waiting TEXT
						emit SYNTAX [json string [string index $text [++post i]]]
						return $tokens
					}
				}
			} elseif {$cx eq "\{"} {
				switch -exact -- [string index $text $i] {
					{} {
						throw [list PARSETCL PARSE MISSING_CLOSE_BRACE $start $text $ofs] "missing \}"
					}

					"\{" {
						incr depth
					}

					"\}" {
						if ([incr depth -1] == 0} {
							if {![is_whitespace [string index $text $i+1]]} {
								throw [list PARSETCL PARSE LIST_ELEMENT_TRAILING_GARBAGE [+ $i 1] $text $ofs] "list element in braces followed by \"[string index $text $i+1]\" instead of space"
							}
							emit_waiting TEXT
							emit SYNTAX [json string [string index $text [++post i]]]
							return $tokens
						}
					}
				}
			} elseif {[is_whitespace [string index $text $i]]} {
				emit_waiting TEXT
				return $tokens
			}

			if {[string index $text $i] eq "\\"} {
				if {$cx eq "\{"} {
					append token	[string index $text [++post i]]
					if {[string index $text $i] eq ""} {
						throw [list PARSETCL PARSE MISSING_CLOSE_BRACE $start $text $ofs] "missing \}"
					}
					append token	[string index $text [++post i]]
				} else {
					parse_escape
				}
			} else {
				append token	[string index $text [++post i]]
			}
		}
	}

	#>>>
	closure tokenize_list { #<<<
		while 1 {
			if {[regexp -start $i {^[\t\n\v\f\r ]+} $text m]} {
				emit SPACE $m
				incr i	[string length $m]
			}

			switch -exact -- [string index $text $i] {
				{} {
					return $tokens
				}

				"\{" -
				"\"" {
					set cx	[string index $text $i]
				}

				default {
					set cx	{}
				}
			}

			parse_list_element $cx
		}
	}

	#>>>
	proc find_line_no {source ofs} { #<<<
		set line	[string length [regsub -all [string range $source 0 $ofs-1] {[^\n]+} {}]]
		+ $line 1
	}

	#>>>
	proc find_line_ofs {source ofs} { #<<<
		- $ofs [string first \n $source $ofs]
	}

	#>>>
	proc visualize_whitespace str { #<<<
		string map {\n \u23ce  \t \u21e5} $str
	}

	#>>>
	proc all_script_tokens commands { #<<<
		set tokens	{[]}

		json foreach command $commands {
			json foreach word $command {
				json foreach token $word {
					json set tokens end+1 $token
				}
			}
		}
	}

	#>>>
	proc word_empty tokens { #<<<
		json foreach token $tokens {
			switch -exact -- [json get $token 0] {
				TEXT - ESCAPE - VAR - ARRAY - SCRIPT - EXPAND {
					return false
				}
			}
		}

		return true
	}

	#>>>
	proc parse {text mode ofs} { #<<<
		set i			0
		set token		{}
		set tokens		{[]}
		set command		{[]}
		set commands	{[]}
		#set matches		{}
		set tokstart	[expr {$ofs eq "" ? 0 : $ofs}]
		set textlen		[string length $text]

		switch -exact -- $mode {
			script {
				while {$i < $textlen} {
					set word	[get_word [== 0 [llength $command]] false]
					if {$i >= $textlen && [json length $word] && [json get $word end 0] ne "END"} {
						json set word end+1 [json template {
							["END", "", null, "~N:i"]
						}]
					}
					if {[json length $command] > 1 && [word_empty $word]} {
						# Prevent a fake word being added to the command only
						# containing non-word tokens
						json set command end end+1 $word
					} else {
						json set command end+1 $word
					}
					set lasttoken	[json extract $word end]
					if {[json get $lasttoken 0] eq "END"} {
						json set commands end+1 $command
						set command {[]}
					}
				}
				return [json template {
					["SCRIPT", "~J:commands", null, 0]
				}]
			}
			expr {
				parse_subexpr
				return $tokens
			}
			list {
				tokenize_list
				return $tokens
			}
			subst {
				while {$i < $textlen} {
					set c	[string index $text $i]
					switch -exact -- $c {
						"\\"   parse_escape
						"\$"   parse_variable
						"\["   parse_commands

						default {
							append token $c
							incr i
						}
					}
				}
				emit_waiting TEXT
				return $tokens
			}
			default {
				throw {TCL WRONGARGS} "Invalid parse mode: \"$mode\""
			}
		}
	}

	#>>>
	proc parse_script {text ofs} { #<<<
		# First unfold - happens even in brace quoted words
		# This has been pushed down to parse_escape, parse_braced and parse_subexpr
		#regsub -all {\\\n\s*} $text { } text
		tailcall parse $text script $ofs
	}

	#>>>
	proc parse_expr {text ofs} { #<<<
		tailcall parse $text expr $ofs
	}

	#>>>
	proc parse_list {text ofs} { #<<<
		tailcall parse $text list $ofs
	}

	#>>>
	proc parse_subst {text ofs} { #<<<
		tailcall parse $text subst $ofs
	}

	#>>>
	proc expr2stack expr { #<<<
		# Algorithm from Harry Hutchins http://faculty.cs.niu.edu/~hutchins/csci241/eval.htm
		set P		{[]}
		set stack	{[]}

		json foreach e $expr {
			switch -exact -- [json get $e 0] {
				OPERAND		{json set P end+1 $e}
				LPAREN      {json set stack end+1 $e}
				RPAREN {
					if {[json length $stack] == 0} {
						error "Unbalanced close parenthesis in expression"
					}
					while {[json length $stack]} {
						set item	[json extract $stack end]
						json unset stack end
						if {[json get $item 0] eq "LPAREN"} {
							break
						}
						json set P end+1 $item
					}
				}
				OPERATOR {
					if {[json length $stack] == 0 || [json get $stack end 0] eq "LPAREN"} {
						json set stack end+1 $e
					} else {
						while {
							[json length $stack] &&
							[json get [set item [json extract $stack end]] 0] ne "LPAREN" &&
							[json get $e 1] > [json get $item 1]
						} {
							json set P end+1 [json extract $stack end]
							json unset $stack end
						}
						json set stack end+1 $e
					}
				}

				SYNTAX -
				SPACE {}

				default {
					# puts stderr "Ignoring expr item: [json pretty $e]"
					error "Ignoring expr item: $e"
				}
			}
		}
		if {[json length $stack] && [json get $stack end 0] eq "LPAREN"} {
			error "Unbalanced open parenthesis in expression"
		}
		while {[json length $stack]} {
			json set P end+1 [json extract $stack end]
			json unset stack end
		}

		set P
	}

	#>>>
}

# vim: ts=4 shiftwidth=4 foldmethod=marker foldmarker=<<<,>>>
