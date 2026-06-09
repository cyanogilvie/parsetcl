package require parsetcl

namespace eval ::parsetcl {
	proc idx2lc idx {
		set line		[vim::expr "byte2line([expr {$idx+1}])"]
		set linestart	[vim::expr "line2byte($line)"]
		list $line [expr {$idx-$linestart+2}]	;# TODO: account for multibyte chars in the byterange [linestart, $idx)
	}

	proc vim_highlight {} {
		puts "highlight: [timerate {
		vim::command {call clearmatches()}
		set buf	$::vim::current(buffer)
		#puts "cursor: [$::vim::current(window) cursor]"
		#set buftext	[vim::expr {join(getline(1, '$'), "\n")}]
		set buftext	[vim::expr {getline(1, '$')}]
		#set a	$buftext
		#puts "get buftext: [timerate {
		#	set buftext	[join [$buf get 1 [$buf count]] \n]
		#} 1 1]"
		#set b	$buftext
		#puts "matches: [expr {$a eq $b}], a: [string length $a], b: [string length $b]"
		set parsetree	[parsetree $buftext]
		set root		[node $parsetree]
		#puts [domNode $root asXML]
		set groups	{}

		if 0 {
		foreach node [xpath $root {
			//command[not(ancestor::word[as/script][@valuetransformed])]/word[1]
		}] {
			set byteofs	[domNode $node getAttribute idx]
			set bytelen	[domNode $node getAttribute len]
			#set line	[vim::expr "byte2line([expr {$byteofs+1}])"]
			lassign [idx2lc $byteofs] line col
			#lassign [idx2lc $buftext [expr {$byteofs+$bytelen-1}]] tline tcol
			#puts "command: byteofs: $byteofs, bytelen: $bytelen, line: ($line), col: ($col)"
			dict lappend groups Statement	\[$line,$col,$bytelen\]
			if 0 {
			puts "byte2line bench: [timerate {
				vim::command "call byte2line(1)"
			}]"
			puts "getAttr bench: [timerate {
				domNode $node getAttribute idx
			}]"
			}
			# TODO: matchaddpos.  research :help channel, :help textprop, :help listener_add()
			# TODO: example plugin using technique: https://github.com/mattn/vim-treesitter
		}

		foreach node [xpath $root //comment] {
			lassign [idx2lc [domNode $node getAttribute idx]] line col
			dict lappend groups Comment \[$line,$col,[domNode $node getAttribute len]\]
		}

		foreach node [xpath $root //operator/syntax] {
			lassign [idx2lc [domNode $node getAttribute idx]] line col
			dict lappend groups Operator \[$line,$col,[domNode $node getAttribute len]\]
		}

		foreach node [xpath $root {
			//word[@quoted="quote" and not(as and not(@valuetransformed))]/text
		}] {
			lassign [idx2lc [domNode $node getAttribute idx]] line col
			dict lappend groups String \[$line,$col,[domNode $node getAttribute len]\]
		}

		foreach node [xpath $root //escape] {
			lassign [idx2lc [domNode $node getAttribute idx]] line col
			dict lappend groups Special \[$line,$col,[domNode $node getAttribute len]\]
		}

		foreach node [xpath $root {
			//var/text[1] |
			//command[@name="set"]/word[2][not(@dynamic)] |
			//command[@name="proc"]/word[3]/as/list/word
		}] {
			lassign [idx2lc [domNode $node getAttribute idx]] line col
			dict lappend groups Identifier \[$line,$col,[domNode $node getAttribute len]\]
		}

		foreach node [xpath $root {
			//var/syntax
		}] {
			lassign [idx2lc [domNode $node getAttribute idx]] line col
			dict lappend groups Ignore \[$line,$col,[domNode $node getAttribute len]\]
		}

		}

		foreach {group xpath} {
			parsetclSyntax	//syntax|//end
			parsetclCommand {
				//command[not(ancestor::word[as/script][@valuetransformed])]/word[1]
			}
			parsetclEscape			//escape
			parsetclComment			//comment
			parsetclOperator		//operator/syntax
			parsetclString {
				//word[@quoted="quote" and not(as and not(@valuetransformed))]/text
			}
			parsetclVariable {
				//var/text[1] |
				//command[@name="set"]/word[2][not(@dynamic)] |
				//command[@name="proc"]/word[3]/as/list/word
			}
		} {
			foreach node [xpath $root $xpath] {
				set idx	[domNode $node getAttribute idx]
				set len	[domNode $node getAttribute len]
				set end	[expr {$idx + $len - 1}]
				lassign [idx2lc $idx] line col
				dict lappend groups $group \[$line,$col,$len\]	;# TODO: maybe trim len to the end of the first line?
				if {[string first \n $buftext $idx] < $end} {
					lassign [idx2lc $end] endline endcol
					if {$endline > $line} {
						for {set l [expr {$line+1}]} {$l < $endline} {incr l} {
							dict lappend groups $group \[$l\]
						}
						dict lappend groups $group \[$l,1,$endcol\]
					}
				}
			}
		}

		dict for {group locs} $groups {
			vim::command "call matchaddpos('$group', \[[join $locs ,]\])"
		}
		} 1 1]"
	}
}
