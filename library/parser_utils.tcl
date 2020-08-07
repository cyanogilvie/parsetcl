namespace eval ::parsetcl {
	proc word_braced word { #<<<
		#$word selectNodes {boolean(*[1][name()='syntax']/text() == "\{")}
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

	variable cmd_parsers {
		"if" {cmd { #<<<
			coroutine nextword	commandwords $cmd
			try {
				nextword	;# pop "if"
				subparse expr [nextword]

				set word	[nextword]
				if {[domNode $word getAttribute value] eq "then"} {set word [nextword]}
				subparse script $word

				while 1 {
					set word	[nextword]
					switch -exact -- [domNode $word getAttribute value] {
						"elseif" {
							subparse expr   $word
							subparse script [nextword]
						}
						"else" {
							subparse script $word
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
				catch {rename nextword {}}
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
				subparse list $varlist
				subparse list $elements
			}
			set body		[lindex $words end]
			subparse script $body
			#>>>
		}}
		"lmap"			{cmd { ::parsetcl::parse_command $cmd foreach			}}
		"for" {cmd { #<<<
			lassign [xpath $cmd word] setup condition next body
			subparse script $setup
			subparse expr   $condition
			subparse script $next
			subparse script $body
			#>>>
		}}
		"while" {cmd { #<<<
			lassign [xpath $cmd word] condition body
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
		"proc"			{cmd { subparse script [xpath $cmd { word[last()] }]	}}
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
				eval { subparse script [lindex $words 2] }
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
			#puts stderr "try cmd parser, [$cmd asXML]"
			set next		{}
			foreach word [xpath $cmd word] {
				#puts stderr "word: ([$word asXML])"
				if {[llength $next] > 0} {
					set next	[lassign $next type]
					if {$type ne ""} {
						#puts stderr "Interpreting word as $type and subparsing: [$word asXML]"
						subparse $type $word
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
			#puts stderr "try cmd_parse result: [$cmd asXML]"
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
				[regexp {^[0-9#]} [domNode [lindex $words $i] getAttribute value]
			} {
				incr i
			}

			if {[llength $words] - $i > 1} {
				# TODO: warn about this
				return
			}

			subparse script [lindex $words end]
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
						incr i	2
						continue
					}
					-matchvar - -indexar {
						incr i	3
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
				subparse script $handler
			}
			#>>>
		}}
		"dict" {cmd { #<<<
			set words	[xpath $cmd word]
			switch -exact -- [lindex $words 1] {
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
	}
}

# vim: ts=4 shiftwidth=4 foldmethod=marker foldmarker=<<<,>>>
