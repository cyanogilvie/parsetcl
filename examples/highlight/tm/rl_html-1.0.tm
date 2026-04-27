package require tdom
package require parse_args

namespace eval ::rl_html {
	variable custom_tags
	if {![info exists custom_tags]} {
		set custom_tags	{}
	}
}

set make_nodecmds {
	namespace eval ::h {
		foreach tag {
			div span pre code
			table tbody thead td th tr tfoot caption
			ul ol li dl dt dd
			h1 h2 h3 h4 h5 h6
			a p br i b u em strong img hr small sup sub strike blockquote
			nobr
			figure figcaption
			form button input textarea select optgroup option label fieldset legend
			style script noscript
			html head body dialog meta link title
			fb:login-button
			nav aside article section header footer time main
			video picture source iframe canvas audio
			progress template output dialog
			details summary

			svg defs radialGradient linearGradient stop rect path g use circle image

			OpenSearchDescription xmlns:referrer Image ShortName LongName
			Description Tags Attribution SyndicationRight AdultContent Language
			InputEncoding OutputEncoding Contact Url Query moz:SearchForm
		} {
			dom createNodeCmd -returnNodeCmd elementNode $tag
			#dom createNodeCmd -returnNodeCmd elementNode [string toupper $tag]
			interp alias {} ::<$tag {} [namespace current]::$tag
			#interp alias {} ::<$tag> {} [namespace current]::$tag
		}

		foreach {source tags} $::rl_html::custom_tags {
			foreach tag $tags {
				dom createNodeCmd -returnNodeCmd elementNode $tag
				#dom createNodeCmd -returnNodeCmd elementNode [string toupper $tag]
				interp alias {} ::<$tag {} [namespace current]::$tag
				#interp alias {} ::<$tag> {} [namespace current]::$tag
			}
		}
	}

	dom createNodeCmd textNode _txt
	dom createNodeCmd commentNode <!--

	proc txt args {
		try {_txt {*}$args} on error {errmsg options} {
			_txt {*}[lrange $args 0 end-1] [string map {
				\x00 "" \x01 "" \x02 "" \x03 ""
				\x04 "" \x05 "" \x06 "" \x07 ""
				\x08 "" \x0b "" \x0c "" \x0e ""
				\x0f "" \x10 "" \x11 "" \x12 ""
				\x13 "" \x14 "" \x15 "" \x16 ""
				\x17 "" \x18 "" \x19 "" \x1a ""
				\x1b "" \x1c "" \x1d "" \x1e ""
				\x1f ""
			} [lindex $args end]]
		}
	}
}

if {[llength [info commands ns_itcl]] != 0} {
	ns_ictl trace create $make_nodecmds
} else {
	eval $make_nodecmds
}
unset make_nodecmds

interp alias {} < {} txt -disableOutputEscaping
proc &nbsp {} {txt -disableOutputEscaping "&nbsp;"}


proc html args { #<<<
	global _docstack

	parse_args::parse_args $args {
		-breakLines	{-boolean}
		script		{-required}
	}

	dom createDocument rl doc
	lappend _docstack $doc
	try {
		$doc documentElement root
		uplevel 1 [list $root appendFromScript $script]
		if {$breakLines} {
			$root asHTML -onlyContents -breakLines
		} else {
			$root asHTML -onlyContents
		}
	} finally {
		set _docstack	[lrange $_docstack 0 end-1]
		if {[info exists doc]} {
			$doc delete
		}
	}
}

#>>>
if {[info commands ns_cache_eval] ne ""} {
	ns_runonce {
		ns_cache_create html_component_cache [expr {100 * 1048576}]
	}
	proc cache_component {seconds cachekey script} { #<<<
		global _docstack

		set xml	[ns_cache_eval -expires $seconds html_component_cache $cachekey {
			dom createDocument span doc
			lappend _docstack $doc
			try {
				$doc documentElement root
				$root setAttribute data-cached $cachekey
				uplevel 1 [list $root appendFromScript $script]
				$root asHTML
			} on ok html {
				# Ugh - have to re-parse this as html to fudge the brokenness
				# introduced by abusing < to pull in invalid markup
				try {
					dom parse -html $html htmldoc
					$htmldoc asXML -indent none
				} on error {errmsg options} {
					# The parse could fail if the string is invalid, as a last
					# ditch effort, fall back to just passing the garbage on
					set rand	[expr {int(rand()*10000)}]
					rl_log error "cache_component: Error parsing HTML from script (wrote /tmp/bad_$rand.html): $errmsg"
					writefile /tmp/bad_$rand.html $html
					set html
				} finally {
					if {[info exists htmldoc]} {
						$htmldoc delete
					}
				}
			} finally {
				set _docstack	[lrange $_docstack 0 end-1]
				if {[info exists doc]} {
					$doc delete
				}
			}
		}]

		set node	[<span]
		try {
			set parent	[$node parentNode]
			try {
				$node appendXML $xml
			} on error {errmsg options} {
				set rand	[expr {int(rand()*10000)}]
				rl_log error "cache_component: Error reconstructing nodes from cached XML (wrote /tmp/bad_$rand.xml): $errmsg"
				writefile /tmp/bad_$rand.xml $xml
				$node appendFromScript {
					< $xml
				}
			}
			set cachenode	[lindex [$node childNodes] 0]
			foreach child [$cachenode childNodes] {
				$cachenode removeChild $child
				$parent appendChild $child
			}
		} finally {
			$node delete
		}
	}

	#>>>
}

# Expose the inner-most tdom document object, to allow the dom to be directly
# manipulated, or queried with selectNodes
proc html_doc args { #<<<
	global _docstack

	if {![info exists _docstack] || [llength $_docstack] == 0} {
		throw {RL HTML NO_DOM_CONTEXT} "There is no document context (no calls to \[html\] on the call stack)"
	}

	tailcall [lindex $_docstack end] {*}$args
}

#>>>

# Transform some (reasonably well formed) HTML string by parsing it with tdom,
# looping over each node matching some xpath expression and reflowing to an
# HTML string.  The loop bodies receive the node command in the loop variable
# they specify.  Any changes to the document by the loop body are reflected in
# the resulting string.
proc foreach_node {var xpath html_str body} { #<<<
	upvar 1 $var node
	try {
		dom parse -keepEmpties -html <html>$html_str</html> doc
		$doc documentElement root
		# The uplevel on the selectNodes call is to allow the tdom xpath Tcl
		# var interpolation magic to work
		foreach node [uplevel 1 [list $root selectNodes $xpath]] {
			uplevel 1 $body
		}
		$root asHTML -onlyContents
	} finally {
		if {[info exists doc]} {
			catch {$doc delete}
		}
	}
}

#>>>

# vim: ft=tcl foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4
