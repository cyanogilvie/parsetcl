namespace eval ::parsetcl {
	namespace eval dom {
		foreach tag {
			script
			command
			word
			arg
			expr
			list
			subst

			text
			space
			var
			array
			index
			escape
			end
			comment
			expand
			syntax
			operand
			operator
			lparen
			rparen
			float
			integer
			mathfunc
			bool
			expr
			arg
			quoted
			braced
			scriptarg
			exprarg
			substarg
			listarg
		} {
			dom createNodeCmd -returnNodeCmd elementNode $tag
			interp alias {} ::parsetcl::[string toupper $tag] {} [namespace current]::$tag
		}
		dom createNodeCmd textNode txt
		dom createNodeCmd commentNode <!--
	}

	proc closure {name args script} { # Fake closure for parse state variables <<<
		tailcall proc $name $args "upvar 1 i i text text textlen textlen tokstart tokstart token token tokens tokens linestarts linestarts ofsline ofsline; $script"
	}

	#>>>
	proc closurelambda {l args} { # Fake closure lambda generator <<<
		set script	[lindex $l 1]
		lset l 1 "upvar 1 i i text text textlen textlen tokstart tokstart token token tokens tokens linestarts linestarts ofsline ofsline; $script"
		list apply $l {*}$args
	}

	#>>>

	proc char++ {} { # Returns the character at [string index $text $i] and post-increments i <<<
		upvar 1 i i text text
		try {
			string index $text $i
		} finally {
			incr i
		}
	}

	#>>>
	closure idx2line idx { # Fast lookup of the line number for $idx into $text <<<
		set line	[lsearch -sorted -increasing -bisect -integer $linestarts $idx]
		if {$line == -1} {error "Invalid idx \"$idx\", before the start of the first line"}
		set linestart	[lindex $linestarts $line]
		list [+ $line $ofsline] [::tcl::mathfunc::max 0 [- $idx $linestart -1]]
	}

	#>>>
	proc toklength tok { #<<<
		switch -exact -- [tok get $tok type] {
			INDEX {
				set len	0
				if {[tok type $tok parts] eq "string"} {
					incr len	[string length [tok get $parts str]]
				} else {
					foreach part [tok get $tok parts] {
						incr len	[toklength $part]
					}
				}
				set len
			}

			SCRIPT {
				set tokens	[all_script_tokens [tok get $tok commands]]
				set len		0
				json foreach t $tokens {
					incr len	[toklength $t]
				}
				set len
			}

			default {
				if {![tok exists $tok str]} {return 0}
				string length [tok get $tok str]
			}
		}
	}

	#>>>
	proc tok {verb tok args} { #<<<
		set pathargs	[lrange $args 0 end-1]
		set part		[lindex $args end]
		if {[llength $pathargs] > 0} {
			# Resolve the path
			set path		[join [lmap e $pathargs {
				if {[regexp {^end([+-].*)?$} $e - endofs]} {
					return -level 0 "*\[last()$endofs\]"
				} else {
					incr e	;# 1-based numbering
					return -level 0 "*\[$e\]"
				}
			}] /]
			set matches	[$tok selectNodes $path]
			switch -exact -- [llength $matches] {
				1		{}
				0		{error "Path does not exist: $path"}
				default	{error "Path names more than one node: $path"}
			}
		} else {
			set path	.
		}

		switch -exact -- $part {
			str {
				set type	[$tok selectNodes name($path)]
				if {[$tok nodeName] in {SCRIPT INDEX}} {
					error "Invalid token part \"$part\""
				}
				set xpath	string($path)
			}
			type {
				set xpath	name($path)
			}
			norm - idx - length - line - char {
				set xpath	string($path/@$part)
			}
			commands -
			parts {
				switch -exact -- [$tok selectNodes name($path)] {
					SCRIPT	{set part	command}
					INDEX	{set part	part}
					default	{error "invalid token part \"$part\""}
				}
				set xpath	$path/$part
			}
			default {
				error "Invalid token part \"$part\""
			}
		}

		switch -exact -- $verb {
			get {
				if {[$tok selectNodes not($xpath)]} {
					error "Part does not exist"
				}
				$tok selectNodes $xpath
			}

			exists  { $tok selectNodes boolean($xpath) }
			type    { $tok selectNodes name($xpath)    }
			default { error "Unsupported tok verb: \"$verb\"" }
		}
	}

	#>>>
	proc all_script_tokens commands { #<<<
		set res	{}
		foreach command $commands {
			lappend res {*}[$command selectNodes command/word/*]
		}
		set res
	}

	#>>>
	closure emit {type args} { #<<<
		parse_args $args {
			-idx	{-# {Index into the source text for the first character of this token}}
			-str	{-# {The source characters of the token}}
			-length	{-# {The length of characters this token spans in the source}}
			-norm	{-# {The normalized form of the token}}
			script	{-# {If specified, run this script beneath this node context}}
		}

		#if {$type eq "SPACE"} {puts stderr "emitting SPACE: $args, i: $i, tokstart: $tokstart"}

		if {![info exists idx]} {
			set idx		$tokstart
		}

		lassign [idx2line $idx] line char

		set params	[list idx $idx line $line char $char]

		if {[info exists norm]}   { lappend params norm	$norm }
		if {[info exists script]} { lappend params $script    }

		set my_tokstart	$tokstart
		set tokstart	$i
		set token		{}

		set node	[$type {*}$params]

		if {[info exists str] && $str ne ""} {
			$node appendChild [[$node ownerDocument] createTextNode $str]
		}

		if {![info exists length]} {
			#set length	[toklength $node]
			set length	[- $i $my_tokstart]
			#puts stderr "emitting $type: i: $i, tokstart: $tokstart, calculated length $length"

			$node setAttribute length $length
		}

		set node
	}

	#>>>
	closure emit_waiting type { #<<<
		if {$token ne ""} {
			emit $type -str $token
		}
	}

	#>>>
	closure literal {crep {len {}}} { # Only called within parse_escape <<<
		upvar 1 first first
		set last	[expr {$first + $len eq "" ? 1 : $len}]
		emit_waiting TEXT
		set subtext	[string range $text $first $last]
		emit ESCAPE -str $subtext -norm $crep
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

		switch -exact -- [char++] {
			{}  {append token "\\"}
			a   {charcode 0x7}
			b   {charcode 0x8}
			f   {charcode 0xc}
			n   {charcode 0xa}
			r   {charcode 0xd}
			t   {charcode 0x9}
			v   {charcode 0xb}

			x {
				if {[regexp -start $i {\A[0-9A-Fa-f]+/} $text escapechars]} {
					charcode [& 0x$escapechars 0xff] [+ 1 [string length $escapechars]]
				} else {
					literal x
				}
			}

			u {
				if {[regexp -start $i {\A[0-9A-Fa-f]{1,4}/} $text escapechars]} {
					charcode 0x$escapechars [+ 1 [string length $escapechars]]
				} else {
					literal u
				}
			}

			"\n" {
				# Line folding
				if {[regexp -start i {\A[ \t]*} $text match]} {
					literal { } [+ 1 [string length $match]]
				} else {
					literal { }
				}
			}

			default {
				incr i -1

				if {[regexp -start $i {\A[0-7]{1,3}} $text escapechars]} {
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
	closure parse_whitespace_or_comments {} { #<<<
		set re	{\A(?:[\t \n]*\\\n[\t \n]*)|\A[\t \n]+}

		set m	{}
		while {
			[set c [string index $text $i]] eq "#" ||
			[regexp -start $i $re $text m]
		} {
			if {$m ne ""} {
				append token $m
				incr i	[string length $m]
				set c	[string index $text $i]
				emit_waiting SPACE
				set m	{}
			}

			if {$c eq "#"} {
				append token $c
				incr i
				set c	[string index $text $i]
				while {$i < $textlen} {
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
				emit_waiting COMMENT
			}
		}
	}

	#>>>
	closure parse_whitespace {} { #<<<
		while {[regexp -start $i {\A(?:[\t ]*\\\n[\t ]*)|\A[\t ]+} $text m]} {
			if {$m ne ""} {
				append token $m
				incr i	[string length $m]
			}
			emit_waiting SPACE
		}
	}

	#>>>
	closure parse_commands {} { #<<<
		emit_waiting TEXT

		emit SCRIPT {
			emit SYNTAX -str [char++]

			while {$i < $textlen} {
				parse_whitespace_or_comments

				emit COMMAND -idx $i {
					while {$i < $textlen} {
						parse_whitespace

						if {$i < $textlen} {
							emit WORD {
								parse_word true
							}
						}

						if {[tok get [dom currentNode] end type] eq "end"} break

						if {$i >= $textlen} {
							throw [list PARSETCL PARSE COMMAND_NOT_TERMINATED $i $text $ofs] "Cannot find end of command"
						}
					}
					if {[tok get [dom currentNode] end str] in {"\]" ""}} break
				}
			}
		}
	}

	#>>>
	proc get_text {varname tok {raw false}} { # Store the constant text value of $tok in variable $varname if it is a static literal, return true if static <<<
		upvar 1 $varname literal
		set text	""
		foreach child [$tok selectNodes *] {
			switch -exact -- [$child nodeName] {
				text	{append text [$child text]}
				escape	{
					if {$raw} {
						append text [$child text]
					} else {
						append text [$child getAttribute norm]
					}
				}
				default	{
					# Not a constant value
					return false
				}
			}
		}
		if {[info exists text]} {
			set literal	$text
			return true
		}
		return false
	}

	#>>>
	closure parse_index {} { # Only called from within parse_variable <<<
		# escape, variable and command substs apply here
		emit SYNTAX -str [char++]
		emit INDEX {
			while 1 {
				set c	[string index $text $i]
				switch -exact -- $c {
					{} {
						throw [list PARSETCL PARSE MISSING_END_ARRAY $i $text $ofs] "missing )"
					}

					) {
						emit_waiting TEXT

						set parent		[[dom currentNode] parent]
						set isliteral	[get_text literal_value $parent]
						if {$isliteral} {
							# All child tokens of this index are static literals, compile them into value= attrib
							$parent setAttribute value $literal_value
						}
						break
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
		emit SYNTAX -str [char++]
	}

	#>>>
	closure parse_variable {} { #<<<
		if {![regexp -start [+ $i 1] {\A(?:[a-zA-Z0-9_\{(]|::)} $text]} {
			append token	[char++]
			return
		}

		emit_waiting TEXT
		emit SYNTAX -str [char++]

		if {[string index $text $i] eq "\{"} {
			emit SYNTAX -str [char++]
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
				emit ARRAY -str $token
				parse_index
				set i	$save_i
			} else {
				emit VAR -str $token
			}
			emit SYNTAX -str [char++]
		} else {
			regexp -start $i {\A[a-zA-Z_0-9:]*} $text token
			# : alone is a name terminator
			set idx		[string first : [string map {:: __} $token]]
			if {$idx > 0} {
				set token	[string range $token 0 $idx-1]
			}
			incr i	[string length $token]
			if {[string index $text $i] ne "("} {
				emit VAR -str $token
			} else {
				emit ARRAY -str $token
				parse_index
			}
		}
	}

	#>>>
	closure emit_fold len { # Only called from parse_braced <<<
		upvar 1 from from
		set subtext		[string range $text $i [+ $i $len -1]]
		emit ESCAPE -str $subtext -crep { }
		incr i $len
		set from	$i
	}

	#>>>
	closure parse_braced {} { #<<<
		set depth	1
		set emitted	false

		emit SYNTAX -str [char++]
		[dom currentNode] setAttribute quoted brace
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
					emit TEXT -str $subtext
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
			emit TEXT -str $subtext
		}
		emit SYNTAX -str [char++]
	}

	#>>>
	closure parse_combined {quoted incmdsubst {ignore_trailing false}} { #<<<
		set start	$i

		if {$quoted} {
			emit SYNTAX -str [char++]
			[dom currentNode] setAttribute quoted quote
		} else {
			[dom currentNode] setAttribute quoted none
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
							set re	{\A(?:[\s;\]]|\\\n)}
						} else {
							set re	{\A(?:[\s;]|\\\n)}
						}
						set next_i	[+ $i 1]

						if {
							!$ignore_trailing &&
							$next_i < $textlen &&
							![regexp -start $next_i $re $text]
						} {
							set lineno	[string length [regsub -all {[\A\n]+} [string range $text 0 $i-1] {}]]
							#puts "i: $i, ([string range $text 0 99]) line: $lineno: [string range $text $i-5 $i+4]"
							throw [list PARSETCL PARSE EXTRA_CHARS_AFTER_CLOSE_QUOTE $next_i $text $ofs] "extra characters after close-quote"
						}
						if {$i == $start + 1} {
							# Need to manually emit rather than using
							# emit_waiting because we still need it if
							# token eq {}
							emit TEXT -str $token
						} else {
							emit_waiting TEXT
						}
						emit SYNTAX -str [char++]
						return
					}

					default {
						set matched		false
					}
				}
			} else {
				switch -exact -- [string index $text $i] {
					{} {
						emit_waiting TEXT
						[[dom currentNode] parentNode] appendFromScript {
							emit END
						}
						return
					}

					"\n" - ";" {
						emit_waiting TEXT
						set token	[char++]
						[[dom currentNode] parentNode] appendFromScript {
							emit END -str $token
						}
						return
					}

					"\\" {
						if {[string index $text $i+1] ne "\n"} {
							set matched		false
						} else {
							# Line fold - falls through
							emit_waiting TEXT
							return
						}
					}

					{ } - "\t" {
						# TODO: actually - any whitespace other than \n, not just space and tab?
						emit_waiting TEXT
						return
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
						set c	[char++]
						if {$incmdsubst && !$quoted} {
							emit_waiting TEXT
							set token	$c
							[[dom currentNode] parentNode] appendFromScript {
								emit END -str $token
							}
							return
						}
						append token $c
					}

					default {
						append token	[char++]
					}
				}
			}
		}
	}

	#>>>
	closure parse_word incmdsubst { #<<<
		# handle {*}
		set c	[string index $text $i]
		if {$c eq "\{" && [regexp -start $i {\A{\*}} $text]} {
			emit EXPAND -str "{*}"
			incr i 3
			set c	[string index $text $i]
			[dom currentNode] setAttribute expand true
		}

		switch -exact -- $c {
			{}    {}
			"\{"  {parse_braced}
			"\""  {parse_combined true $incmdsubst}
			"\]" {
				if {$incmdsubst} {
					[[dom currentNode] parentNode] appendFromScript {
						# Arrange for the END node to be inserted as a sibling of this word node, just after it
						emit END -str [char++]
					}
				}
				parse_combined false $incmdsubst
			}
			default {
				parse_combined false $incmdsubst
			}
		}
	}

	#>>>
	closure emit_token {type value subtype crep} { # Only called from parse_subexpr <<<
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
			{\A(?:~|!(?=[^=]))} 1
			{\A\*\*}            2
			{\A[*\/%]}          2
			{\A[\-+]}           2
			{\A(?:<<|>>)}       2
			{\A(?:<=|>=)}       2
			{\A(?:<|>)}         2
			{\A(?:==|!=)}       2
			{\A(?:ne|eq)}       2
			{\A(?:in|ni)}       2
			{\A&(?!&)}          2
			{\A\^}              2
			{\A\|(?!\|)}        2
			{\A&&}              2
			{\A\|\|}            2
			{\A[?:]}            3
		}

		set textlen	[string length $text]
		while {$i < $textlen} {
			#set here	[string range $text $i end]		;# TODO: replace with relative indexing later

			# line folds
			if {[regexp -start $i {\A\\\n\s+} $text m]} {
				emit_token SPACE $m
				continue
			}

			# whitespace
			if {[regexp -start $i {\A\s+} $text m]} {
				emit_token SPACE $m
				continue
			}

			if {!$expecting_operator} {
				# Unitary + and -
				if {[regexp -start $i {\A[\-+]} $text m]} {
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
			if {[regexp -start $i -nocase {\A(?:([\-+]?)(Inf(?:inity)?)|(NaN))\M} $text m m1]} {
				if {[string match -nocase ?n* $m]} {
					emit_token OPERAND $m FLOAT [expr {$m}]
				} else {
					emit_token OPERAND $m FLOAT [expr {$m}]
				}
				continue
			}

			if {
				[regexp -start $i -nocase {\A([\-+])?(0x)([\dA-F]+)} $text m] ||
				[regexp -start $i -nocase {\A([\-+])?(0b)([01]+)} $text m] ||
				[regexp -start $i -nocase {\A([\-+])?(0o)([0-7]+)} $text m]
			} {
				# TODO: Bignum support
				emit_token OPERAND $m INTEGER [expr {$m+0}]
				continue
			}
			if {
				[regexp -start $i -nocase {\A[\-+]?\d+(?:(\.)(?:\d+)?)?(e[\-+]?\d+)?} $text m m1 m2] ||
				[regexp -start $i -nocase {\A[\-+]?(\.)\d+(e[\-+]?\d+)?} $text m m1 m2]
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
							switch -exact -- [tok get $t type] {
								VAR {
									set d	[json extract $t 1]
									return [json template {["~J:d"]}]
								}
								ARRAY {
									set array	[json extract $t 1]
								}
								INDEX {
									set index	[json extract $t 1]
									if {[json length $index] == 1 && [tok get $index 0 type] eq "TEXT"} {
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
							if {[tok get $t type] eq "SCRIPT"} {
								return $t
							} elseif {[tok get $t type] eq "SYNTAX"} {
								# Dirty hack to inject the [ syntax token
								emit_token SYNTAX [tok get $t str]
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
			if {[regexp -start $i {\A(\w+)(\s*)?\(} $text m m1 m2]} {
				parse_mathfunc $m1 $m2
				continue
			}
			# boolean
			if {[regexp -start $i -nocase {\A(?:tr(?:ue?)?|yes?|on)\M} $text m]} {
				emit_token OPERAND $m BOOL true
				continue
			}
			if {[regexp -start $i -nocase {\A(?:fa(?:l(?:se?)?)?|no|off?)\M} $text m]} {
				emit_token OPERAND $m BOOL false
				continue
			}
			# invalid bareword
			if {[regexp -start $i {\A\w+\M} $text m]} {
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
			emit SYNTAX -str [char++]
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
						emit SYNTAX -str [char++]
						return
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
						if {[incr depth -1] == 0} {
							if {![is_whitespace [string index $text $i+1]]} {
								throw [list PARSETCL PARSE LIST_ELEMENT_TRAILING_GARBAGE [+ $i 1] $text $ofs] "list element in braces followed by \"[string index $text $i+1]\" instead of space"
							}
							emit_waiting TEXT
							emit SYNTAX -str [char++]
							return
						}
					}
				}
			} elseif {[is_whitespace [string index $text $i]]} {
				emit_waiting TEXT
				return
			}

			if {[string index $text $i] eq "\\"} {
				if {$cx eq "\{"} {
					append token	[char++]
					if {[string index $text $i] eq ""} {
						throw [list PARSETCL PARSE MISSING_CLOSE_BRACE $start $text $ofs] "missing \}"
					}
					append token	[char++]
				} else {
					parse_escape
				}
			} else {
				append token	[char++]
			}
		}
	}

	#>>>
	closure tokenize_list {} { #<<<
		while 1 {
			if {[regexp -start $i {\A[\t\n\v\f\r ]+} $text m]} {
				emit SPACE -str $m
				incr i	[string length $m]
			}

			switch -exact -- [string index $text $i] {
				{} {
					return
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
	proc visualize_whitespace str { #<<<
		string map {\n \u23ce  \t \u21e5} $str
	}

	#>>>
	proc word_empty tokens { #<<<
		json foreach token $tokens {
			switch -exact -- [tok get $token type] {
				TEXT - ESCAPE - VAR - ARRAY - SCRIPT - EXPAND {
					return false
				}
			}
		}

		return true
	}

	#>>>
	proc parse {text mode {ofs 0} {a_ofsline 1}} { #<<<
		variable doc
		set i			0
		set token		{}
		set tokens		{[]}		;# Only used by expr parser
		set tokstart	[expr {$ofs eq "" ? 0 : $ofs}]
		set textlen		[string length $text]
		set linestarts	[list $tokstart {*}[lmap m [regexp -indices -all -inline {\n} $text] {+ [lindex $m 0] $ofs 1}]]
		set ofsline		$a_ofsline

		if {![info exists doc]} {
			set doc		[dom createDocument tcl]
			set node	[$doc documentElement]
			set docowned	true
		} else {
			set docowned	false
			set node		[$mode]
		}
		try {
			$node appendFromScript {
				switch -exact -- $mode {
					script {
						emit SCRIPT {
							while {$i < $textlen} {
								parse_whitespace_or_comments

								emit COMMAND -idx $i {
									while {$i < $textlen} {
										parse_whitespace

										if {$i < $textlen} {
											emit WORD {
												parse_word false
											}
											set lastword	[[dom currentNode] selectNodes {word[last()]}]
											if {[$lastword selectNodes count(*)] == 0} {
												$lastword delete
											}
										}

										if {[tok get [dom currentNode] end type] eq "end"} break

										if {$i >= $textlen} {
											# Hit EOF before finding the end of this command, produce an empty, synthetic END
											emit END -idx $i
											break
										}
									}

									# If the first word of this command is a literal, store the name in the name= attrib
									set thiscommand	[dom currentNode]
									set isliteral	[get_text literal_value [$thiscommand selectNodes {word[1]}]]
									if {$isliteral} {
										# All child tokens of this index are static literals, compile them into value= attrib
										$thiscommand setAttribute name $literal_value
									}
								}
							}
						}
					}
					expr {
						emit EXPR {
							parse_subexpr
						}
					}
					list {
						emit LIST {
							tokenize_list
						}
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
					}
					default {
						throw {TCL WRONGARGS} "Invalid parse mode: \"$mode\""
					}
				}
			}
		} on error {errmsg options} {
			if {$docowned} {
				catch {$doc delete}
			} else {
				$node delete
			}
			return -options $options $errmsg
		} finally {
			if {$docowned} {
				unset -nocomplain doc
			}
		}
		#puts stderr "parsetree:\n[$node asXML]"
		set node
	}

	#>>>
	proc parse_script {text {ofs 0} {ofsline 1}} { #<<<
		# First unfold - happens even in brace quoted words
		# This has been pushed down to parse_escape, parse_braced and parse_subexpr
		#regsub -all {\\\n\s*} $text { } text
		tailcall parse $text script $ofs $ofsline
	}

	#>>>
	proc parse_expr {text {ofs 0} {ofsline 1}} { #<<<
		tailcall parse $text expr $ofs $ofsline
	}

	#>>>
	proc parse_list {text {ofs 0} {ofsline 1}} { #<<<
		tailcall parse $text list $ofs $ofsline
	}

	#>>>
	proc parse_subst {text {ofs 0} {ofsline 1}} { #<<<
		tailcall parse $text subst $ofs $ofsline
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
