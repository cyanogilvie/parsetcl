namespace eval ::parsetcl {
	proc word_braced word { #<<<
		#$word selectNodes {boolean(*[1][name()='syntax']/text() = "\{")}
		eq brace [domNode $word getAttribute quoted]
	}

	#>>>
	proc is_unbraced {cmd_text command} { #<<<
		switch -exact -- $cmd_text {
			while - expr {
				return [xpath $command {boolean(word[2][@quoted='brace'])}]
			}
			if {
				set words	[xpath $command {word[position() > 1]}]
				if {![word_braced [lindex $words 0]]} {return true}
				for {set i 0} {$i < [llength $words]} {incr i} {
					set word	[lindex $words $i]
					if {[domNode $word nodeValue] eq "elseif"} {
						incr i
						set word	[lindex $words $i]
						if {![word_braced $word]} {return true}
					}
				}
				return false
			}
			for {
				return [xpath $command {boolean(word[3][@quoted='brace'])}]
			}
		}
	}

	#>>>
	proc parse_command {cmd name} { #<<<
		variable cmd_parsers

		if {![dict exists $cmd_parsers $name]} return
		apply [dict get $cmd_parsers $name] $cmd
	}

	#>>>
	proc indent cmd { #<<<
		set newline	"\n"	;# Can't escape a literal newline in xpath

		# We have a valid indent when one of the following is true:
		#	- The space token immediately preceding our command token contains a newline
		#	- The previous command ended with a newline
		#	- There are no previous commands (we're the first command in the script, and the script isn't a [] expansion in a word)
		set nodes	[xpath $cmd {
			preceding-sibling::*[position()=1 and name()='space' and (
				contains(string(), $newline) or
				string(preceding::end[1]) = $newline or
				count(preceding-sibling::command) = 0 and name(../..)!='word'
			)]
		}]

		if {[llength $nodes] > 0} {
			set spacenode	[lindex $nodes 0]
			set spacetext	[domNode $spacenode asText]
			if {[regexp {([ \t]*)$} $spacetext - indent]} {
				return $indent
			}
		}

		return ""
	}

	#>>>

	proc commandwords command { #<<<
		set words	[xpath $command word]
		if {[info coroutine] eq ""} {
			return $words
		}
		yield
		foreach word $words {
			yield $word
		}
		throw {PARSETCL DONE} "No more words"
	}

	#>>>

	namespace eval context {
		namespace export *
		namespace ensemble create -prefixes no

		variable stack	{}
		proc push {name value} { #<<<
			variable stack
			dict lappend stack $name $value
		}

		#>>>
		proc pop name { #<<<
			variable stack
			if {[dict exists $stack $name]} {
				dict set stack $name	[lrange [dict get $stack $name] 0 end-1]
			}
		}

		#>>>
		proc get {name args} { #<<<
			variable stack

			switch -exact -- [llength $args] {
				0 - 1 {}
				default {
					throw {TCL WRONGARGS} "Wrong number of args, should be: context get name ?default?"
				}
			}

			if {[dict exists $stack $name]} {
				lindex [dict get $stack $name] end
			} else {
				if {[llength $args] == 1} {
					return [lindex $args 0]
				}
				throw [list PARSETCL NO_CONTEXT $name] "No current context for $name"
			}
		}

		#>>>
		proc get_all name { #<<<
			variable stack

			if {[dict exists $stack $name]} {
				dict get $stack $name
			} else {
				return {}
			}
		}

		#>>>
	}

	proc resolve_name {namespace_context name} { #<<<
		if {![domNode $name hasAttribute value]} {
			# TODO: what?
		}

		set name_value	[domNode $name getAttribute value]
		if {[string match ::* $name]} {
			# First check if $name is fully qualified already
			return $name
		}

		set qualifiers	[namespace qualifiers $name_value]
		set tail		[namespace tail $name_value]
		if {[string match ::* $qualifiers]} {
			return ${qualifiers}::$name
		}

		foreach ns_context [lreverse $namespace_context] {
			set ns	[type::get namespace_word $ns_context]
			if {[json isnull $ns]} {
				# ??
			}
		}
	}

	#>>>

	variable cmd_parsers {
		"if" {cmd { #<<<
			set nextword	nextword_[incr ::_parsetcl_nextword_seq]
			coroutine $nextword	commandwords $cmd

			try {
				$nextword	;# pop "if"
				set if_expr	[$nextword]
				subparse expr $if_expr
				set parse_body	1
				if {[domNode $if_expr hasAttribute value]} {
					if {[domNode $if_expr getAttribute value] eq "0"} {
						# Support if 0 {} style comments containing non-tcl
						set parse_body	0
					}
				}

				set word	[$nextword]
				if {[domNode $word hasAttribute value] && [domNode $word getAttribute value] eq "then"} {set word [$nextword]}
				if {$parse_body} {
					subparse script $word
				}

				while 1 {
					set word	[$nextword]
					switch -exact -- [domNode $word getAttribute value] {
						"elseif" {
							set parse_body	1
							set elseif_expr	[$nextword]
							subparse expr   $elseif_expr
							if {[domNode $elseif_expr hasAttribute value] && [domNode $elseif_expr getAttribute value] eq "0"} {
								# if 0 {} style comment - don't parse the body (which may not be Tcl)
								$nextword
							} else {
								subparse script [$nextword]
							}
						}
						"else" {
							subparse script [$nextword]
							break
						}
						default {
							subparse script $word
							break
						}
					}
				}
			} trap {PARSETCL DONE} {} {
			} finally {
				catch {rename $nextword {}}
			}
			#>>>
		}}
		"expr"		{cmd { #<<<
			set words	[xpath $cmd word]
			if {[llength $words] > 2} {
				# TODO: warn about this
			}
			subparse expr [lindex $words 1]
			#>>>
		}}
		"foreach"	{cmd { #<<<
			set words		[xpath $cmd word]
			set iterators	[lrange $words 1 end-1]
			foreach {varlist elements} $iterators {
				#puts stderr "varlist: ($varlist), elements: ($elements)"
				if {[domNode $varlist hasAttribute value]} {
					subparse list $varlist
				} else {
					# TODO: warn about this
				}
				if {[domNode $elements hasAttribute value]} {
					subparse list $elements
				} else {
					# TODO: warn about this
				}
			}
			set body		[lindex $words end]
			if {[domNode $body hasAttribute value]} {
				subparse script $body
			} else {
				# TODO: warn about this
			}
			#puts stderr "<- foreach cmd parser $cmd"
			#>>>
		}}
		"lmap"			{cmd { ::parsetcl::parse_command $cmd foreach			}}
		"for" {cmd { #<<<
			lassign [xpath $cmd word] - setup condition next body
			subparse script $setup
			subparse expr   $condition
			subparse script $next
			subparse script $body
			#>>>
		}}
		"while" {cmd { #<<<
			lassign [lrange [xpath $cmd word] 1 end] condition body
			subparse expr   $condition
			subparse script $body
			#>>>
		}}
		"subst" {cmd { #<<<
			set words	[xpath $cmd word]
			set switches	[lmap word [lrange $words 1 end-1] {
				if {![domNode $word hasAttribute value]} {
					# TODO warn about this - an option doesn't have a static
					# value, so we don't know how the subst arg will be
					# interpreted
					continue
				}
				domNode $word getAttribute value
			}]
			subparse subst [lindex $words end] {*}$switches
			#>>>
		}}
		"proc"			{cmd {
			#puts stderr "-> proc subcommand parse"
			set name	[xpath $cmd {word[2]}]
			set fqname	[resolve_name [context get_all namespace] $name]
			#if {[domNode $name hasAttribute value]} {
			#	puts stderr "parsing proc \"[domNode $name getAttribute value]\""
			#} else {
			#	puts stderr "parsing proc \"[domNode $name asText]\""
			#}
			subparse list [xpath $cmd {word[3]}]
			subparse script [xpath $cmd { word[last()] }]
			#puts stderr "<- proc subcommand parse"
		}}
		"oo::class"		{cmd { subparse script [xpath $cmd { word[last()] }]	}}
		"method"		{cmd { subparse script [xpath $cmd { word[last()] }]	}}
		"constructor"	{cmd { subparse script [xpath $cmd { word[last()] }]	}}
		"destructor"	{cmd { subparse script [xpath $cmd { word[last()] }]	}}
		"catch"			{cmd { subparse script [xpath $cmd { word[2] }]			}}
		"time"			{cmd { subparse script [xpath $cmd { word[2] }]			}}
		"timerate"	{cmd { #<<<
			# First arg that isn't an option (starting with "-")
			foreach word [xpath $cmd word] {
				if {[string index [domNode $word getAttribute value] 0] eq "-"} continue
				subparse script $word
			}
			#>>>
		}}
		"namespace" {cmd { #<<<
			set words		[xpath $cmd word]
			set subcommand	[lindex $words 1]
			if {![domNode $subcommand hasAttribute value]} return
			switch -exact -- [domNode $subcommand getAttribute value] {
				eval {
					context push namespace [lindex $words 2]
					subparse script [lindex $words 3]
					context pop namespace
				}
			}
			# TODO: namespace code, namespace inscope, etc
			#>>>
		}}
		"oo::define" {cmd { #<<<
			set words		[xpath $cmd word]
			if {[llength $words] == 3} {
				subparse script [lindex $words end]
			}
			#>>>
		}}
		"oo::objdefine" {cmd { #<<<
			set words		[xpath $cmd word]
			if {[llength $words] == 3} {
				subparse script [lindex $words end]
			}
			#>>>
		}}
		"try" {cmd { #<<<
			set next		{}
			foreach word [xpath $cmd word] {
				if {[llength $next] > 0} {
					set next	[lassign $next type]
					if {$type ne ""} {
						#puts stderr "Interpreting word as $type and subparsing: [$word asXML]"
						if {[domNode $word hasAttribute value]} {
							#puts stderr "-> try parsing $type $word"
							subparse $type $word
							#puts stderr "<- try parsing $type $word"
						} else {
							# TODO: warn about this
						}
					}
					continue
				}
				if {![get_text literal $word] && [string index $literal 0]} continue
				switch -exact -- $literal {
					try		{set next	script}
					on		{set next	{list list script}}
					trap	{set next	{list list script}}
					finally	{set next	script}
				}
			}
			#>>>
		}}
		"eval" {cmd { #<<<
			set words	[xpath $cmd word]
			if {[llength $words] == 2} {subparse script [lindex $words 1]} else {
				# TODO: we may still be able to resolve this case, if all the
				# args that will be concatenated are static literals, but where
				# to put them in the tree?
			}
			#>>>
		}}
		"uplevel" {cmd { #<<<
			set i	1
			set words	[xpath $cmd word]
			if {
				[domNode [lindex $words $i] hasAttribute value] &&
				[regexp {^[0-9#]} [domNode [lindex $words $i] getAttribute value]]
			} {
				incr i
			}

			if {[llength $words] - $i > 1} {
				# TODO: warn about this
				return
			}

			set script	[lindex $words end]
			#puts "------------------ uplevel script word: ($script) ---------------------------"
			if {[domNode $script hasAttribute value]} {
				subparse script $script
			} elseif {[xpath $script {
				boolean(
					count(*) = 1 and
					script[
						count(command) = 1
					]/command[1][
						@name = 'list' and
						count(word) > 1
					]
				)
			}]} {
				# Uplevel script arg is not a static literal, but is a list, attempt a deep-parse recursion
				set script_cmd_words	[lrange [xpath $script {script/command[1]/word}] 1 end]
				#puts stderr "script_cmd_words: ($script_cmd_words)"
				if {![domNode [lindex $script_cmd_words 0] hasAttribute value]} {
					# TODO: warn about this
					return
				}
				set cmdname	[domNode [lindex $script_cmd_words 0] getAttribute value]
				# TODO: call the cmd_parser for $cmdname on the args of the
				# [list] command.  Difficulty is that the cmd_parsers expect to
				# be passed a command domnode, where the word children are the
				# words of the command, but what we have is a list command,
				# with the cmdname being the 2nd word of the [list] command
				#::parsetcl::parse_command [domNode $script_cmd_words parentNode] $cmdname
				puts stderr "Would chain to parsing $cmdname from \[list\] arg to uplevel"
			}
			#>>>
		}}
		"switch" {cmd { #<<<
			set i				1
			set skipping_args	true

			set words	[xpath $cmd word]
			while {$skipping_args && $i < [llength $words]} {
				set word	[lindex $words $i]
				if {![domNode $word hasAttribute value]} break

				switch -exact -- [domNode $word getAttribute value] {
					-exact - -glob - -regexp - -nocase {
						incr i	1
						continue
					}
					-matchvar - -indexar {
						incr i	2
						continue
					}
					-- {
						set skipping_args	false
						incr i
						continue
					}
					default {
						set skipping_args	false
					}
				}
			}

			incr i
			if {$i == [llength $words] - 1} { # String argument
				set subspecials		{}
				set handler_word	[lindex $words $i]
				if {[domNode $handler_word hasAttribute value]} {
					subparse list $handler_word
					set handlers	[xpath $handler_word as/list/word]
				} else {
					# Switch handlers arg is not a static literal
					if {[xpath $handler_word {
						count(*) = 1 and
						script[
							count(command) = 1 and
							command[1][@name = 'list']
						]
					}]} {
						# This is a special case of dynamic handlers that we might be able to deal with, if the handler scripts are static literals
						set handlers	[lrange [xpath $handler_word {script/command[1]/word}] 1 end]
					} else {
						# TODO: warn about this
						set handlers	{}
					}
				}
			} else { # List arguments
				set handlers	[lrange $words $i end]
			}

			foreach {match handler} $handlers {
				if {![domNode $handler hasAttribute value]} {
					# TODO: warn about this
					puts stderr "handler for [domNode $match asText] has no static value"
					continue
				}
				if {[domNode $handler getAttribute value] eq "-"} continue
				subparse script $handler
			}
			#>>>
		}}
		"dict" {cmd { #<<<
			set words	[xpath $cmd word]
			set subcmd	[lindex $words 1]
			if {![domNode $subcmd hasAttribute value]} {
				# TODO: warn about this
				continue
			}
			switch -exact -- [domNode $subcmd getAttribute value] {
				filter {
					if {[domNode [lindex $words 3] getAttribute value] eq "script"} {
						subparse script [lindex $words 5]
					}
				}
				for - map - update - with {
					subparse script [lindex $words end]
				}
			}
			#>>>
		}}
		"apply" {cmd { #<<<
			#puts stderr "cmd_parse apply"
			set lambda_word	[lindex [xpath $cmd {word[2]}] 0]
			if {[domNode $lambda_word hasAttribute value]} {
				#puts stderr "apply literal, string case"
				subparse list $lambda_word
				set lambda_words	[xpath $lambda_word as/list/word]
			} else {
				#puts stderr "apply literal, dynamic case"
				set lambda_words	[xpath $lambda_word {
					count(*)=1 and
					script[count(command)=1]/command[@name='list']/word[position()>1]
				}]
			}
			#puts stderr "lambda_words: $lambda_words\n[domNode $lambda_word asXML]"
			#puts stderr "lambda_words: $lambda_words"
			if {[llength $lambda_words] < 2 || [llength $lambda_words] > 3} return

			set body	[lindex $lambda_words 1]
			#puts stderr "subparsing: [domNode $body asXML]"
			subparse script $body
			#puts stderr "apply cmd: [domNode $cmd asXML]"
			#>>>
		}}
	}
}

# vim: ts=4 shiftwidth=4 foldmethod=marker foldmarker=<<<,>>>
