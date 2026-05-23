proc loop {bytes chunklen strbytes strlen enc} {
	set res {}
	set idx	0
	set buf	{}
	set cutpoints	{}
	while {$idx < [string length $bytes]} {
		set chunk	[string range $bytes $idx [expr {$idx+$chunklen-1}]]
		dict incr cutpoints [expr {$idx % $strlen}] 1
		#puts stderr "[string repeat - 80]\nbuf: [regexp -all -inline .. [binary encode hex $buf]], chunk first 4: [regexp -all -inline .. [binary encode hex [string range $chunk 0 3]]], last 4: [regexp -all -inline .. [binary encode hex [string range $chunk end-3 end]]], idx: $idx, % [string length $strbytes]: [expr {$idx % [string length $strbytes]}]"
		append buf $chunk
		incr idx	[string length $chunk]
		set last	[expr {[string length $buf]-1}]
		set start	$last	;# start code unit of the last complete character
		while {
			$start >= 0 &&
			$last-$start+1 <= 4 &&
			([binary scan [string index $buf $start] cu b; set b] & 0b11000000) == 0b10000000
		} {
			# continuation byte
			incr start -1
		}
		#puts stderr "start: $start, len: [expr {$last-$start+1}], last: $last, b: [if {[info exists b]} {format 0b%08b $b} else {format {<not set>}}], last character (possibly incomplete): [regexp -all -inline .. [binary encode hex [string range $buf $start $last]]]"
		set surrogate_rewind	0
		while {$start >= 0} {
			set len		[expr {$last-$start+1}]
			if {
				($len == 1 && ($b & 0b10000000) == 0b00000000) ||
				($len == 2 && ($b & 0b11100000) == 0b11000000) ||
				($len == 3 && ($b & 0b11110000) == 0b11100000) ||
				($len == 4 && ($b & 0b11111000) == 0b11110000)
			} {
				# Codepoint encoding is complete
				#puts stderr "codepoint is complete"
				if {$enc eq "cesu-8" && !$surrogate_rewind} {
					if {$len == 3} {
						# Encoding is complete, check for high surrogate
						set codepoint	[expr {$b & (0b11111111 >> ($len+1))}]
						binary scan [string range $buf $start+1 $last] cu* cb
						foreach b $cb {
							set codepoint	[expr {($codepoint << 6) | ($b & 0b00111111)}]
						}

						if {$codepoint >= 0xD800 && $codepoint <= 0xD8FF} {
							#puts stderr "Is a high surrogate: [format 0x%04x $codepoint], talking further"
							# High surrogate, step back to the previous char
							set last	[expr {$start-1}]
							set start	$last
							while {$start >= 0 && ([binary scan [string index $buf $start] cu b; set b] & 0b11000000) == 0b10000000} {
								#puts stderr "is continuation: [format 0b%08b $b]"
								incr start -1
							}
							#puts stderr "high surrogate rewind walked back to $start: [regexp -all -inline .. [binary encode hex [string range $buf $start [expr {$start+3}]]]]"
							set surrogate_rewind	1	;# Prevent rewinding more than one high surrogate - that situation would represent an invalid encoding and we want to fail early rather than accumulating an unbounded number of high surrogates
							continue
						}
					}
				}
			} elseif {$start > 0} {
				# Last character encoding isn't complete, step back to the end of the previous one
				#puts stderr "last character incomplete, start: $start, last: $last"
				set last	[expr {$start-1}]
				set start	$last
				while {$start >= 0 && ([binary scan [string index $buf $start] cu b; set b] & 0b11000000) == 0b10000000} {
					#puts stderr "is continuation: [format 0b%08b $b]"
					incr start -1
				}
				#puts stderr "  walked back to start: $start, last: $last, rechecking"
				continue
			} else {
				set last	-1
			}
			break
		}

		if {$last >= 0} {
			#puts stderr "head 4 bytes: [regexp -all -inline .. [binary encode hex [string range $buf 0 3]]], start of chunk: [regexp -all -inline .. [binary encode hex [string range $chunk 0 3]]], end of output [regexp -all -inline .. [binary encode hex [string range $buf [expr {$last-3}] $last]]]"
			append res	[encoding convertfrom $enc [string range $buf 0 $last]]
			set buf	[string range $buf [expr {$last+1}] end]
			#puts stderr "Wrote [expr {$last+1}] complete code unit bytes, holding back [string length $buf] bytes: [regexp -all -inline .. [binary encode hex $buf]]"
		} else {
			#puts stderr "No complete codepoints in the buffer saved [string length $buf] bytes: [regexp -all -inline .. [binary encode hex $buf]]"
		}
	}
	for {set i 0} {$i < $strlen} {incr i} {
		#puts [format {cutpoint count %02d: %04d} $i [if {[dict exists $cutpoints $i]} {dict get $cutpoints $i} {expr 0}]]
		if {![dict exists $cutpoints $i]} {error "strbytes cutpoint $i not exercised"}
	}
	set res
}

set str			"\xe9\0\nfoo \x7e\x7f\u306f\U1f680"		;# 17 bytes (prime), has 1, 2, 3 and 4 byte encodings (or 6 if mutf-8 encoded \U1f680, in which case len is 19, also prime), null, non-null C0, graph ascii range
foreach enc {utf-8 cesu-8} {
	puts stderr "enc: $enc"
	set strbytes	[encoding convertto $enc $str]
	set strlen		[string length $strbytes]
	puts stderr "str encoding: [regexp -all -inline .. [binary encode hex $strbytes]]"
	proc print str {
		set res	{}
		foreach c [split $str {}] {
			if {[string is graph -strict $c]} {append res $c} else {
				scan $c %c codepoint
				append res	\x1b\[7m<U+[format %04x $codepoint]>\x1b\[0m\x1b\[4m
			}
		}
		string cat \x1b\[4m $res \x1b\[0m
	}
	puts stderr "charlen: [string length $strbytes] strbytes: [print $str]"
	foreach chunklen {65537 23 1} {
		puts stderr "chunklen: $chunklen"
		#set chunklen	65537	;# 65537: prime (and so also relatively prime with the length of $str)
		#set chunklen	23		;# 23: prime (and so also relatively prime with the length of $str)
		#set chunklen	1		;# 1: prime (and so also relatively prime with the length of $str)
		set bytes		[string repeat $strbytes [expr {1 + ($strlen*$chunklen)/$strlen}]]
		puts stderr "bytes len: [string length $bytes]"
		puts "$enc/$chunklen loop clean: [expr {$bytes eq [encoding convertto $enc [loop $bytes $chunklen $strbytes $strlen $enc]]}]"
	}
}


