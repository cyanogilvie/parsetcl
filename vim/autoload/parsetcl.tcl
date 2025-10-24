package require parsetcl

namespace eval ::parsetcl {
	proc vim_highlight {} {
		vim::command {call clearmatches()}
		set buf	$::vim::current(buffer)
		puts "cursor: [$::vim::current(window) cursor]"
		puts "get buftext: [timerate {
			set buftext	[join [$buf get 1 [$buf count]] \n]
		} 1 1]"
		puts "parse: [timerate {
			set parsetree	[parsetcl::parsetree $buftext]
		} 1 1]"
		puts [domNode $parsetree asXML]
		foreach node [xpath $parsetree {//command/word[1]}] {
			set byteofs	[domNode $node getAttribute idx]
			set bytelen	[domNode $node getAttribute len]
			puts "byte2line: [timerate {
				set line	[vim::command "call byte2line([expr {$byteofs+1}])"]
			} 1 1]"
			puts "command: byteofs: $byteofs, bytelen: $bytelen, line: ($line)"
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
	}
}
