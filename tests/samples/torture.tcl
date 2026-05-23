package require tdom
#change

array set a	{}
set x	et
s$x foo bar
s$a(y$x) bar baz
puts ${foo bar}
set a	[foo bar]
set b	[
	foo bar; quux
	baz
  ]
set c "foo is ([foo x y z])"
set other	[list {*}{first second
					third} {*}"fourth fifth" {*}"sixth $footh" {*}$a]	;# TODO: fix

# Import xpath command
namespace import \
	parsetcl::xpath

proc out {x str} {
	if {
		( $x > min(4, $y) || $str eq "foo" && $bar <= int(rand()*10) ) && "foo$x" in {"bar" baz} && [
		# sneaky comment \
		foo bar; list
		baz quux; list
		x
		] ne ${foo baz(y$x)}
	} {
		puts -nonewline "$str update from installed ${test}PWA"	;# TODO: fix
	} else {
		puts {blah}
	}
}

proc test "foo bar" b\u0061z\ xyz\;set\ foo\ bar\nputs\ "x"
proc t2 {} "set x y"
proc t3 {} "set x\ y z"
proc t4 {} "set x\\ y z"

set d	[{*}{dict create
	foo		bar
	baz		quux
	static	$notvar
}	dynamic $butthisis]
