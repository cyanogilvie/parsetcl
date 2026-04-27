package require parse_args

proc req_init {conn proto reqline headers body} { #<<<
	global _req
	set _req [dict create \
		conn	$conn \
		proto	$proto \
		reqline	$reqline \
		headers	$headers \
		body	$body \
	]
	set_form
}

#>>>
namespace eval ::req {
	namespace export *
	namespace ensemble create -prefixes no

	proc conn {} { #<<<
		global _req
		dict get $_req conn
	}

	#>>>
	proc method {} { #<<<
		global _req
		dict get $_req reqline method
	}

	#>>>
	proc path {} { #<<<
		global _req
		dict get $_req reqline path
	}

	#>>>
	proc query {} { #<<<
		global _req
		reuri::query decode [dict get $_req reqline query]
	}

	#>>>
	proc body {} { #<<<
		global _req
		dict get $_req body
	}

	#>>>
	proc headers args { #<<<
		global _req
		parse_args::parse_args $args {
			-last	{-multi which -default last}
			-first	{-multi which}
			-all	{-multi which}
			header	{}
			default	{}
		}

		if {![info exists header]} {
			set res
			foreach {lk vals} [dict get $_req headers] {
				foreach v $vals {
					lappend res $lk $v
				}
			}
			return $res
		}
		if {![dict exists $_req headers $header]} {
			if {[info exists default]} {
				return $default
			}
			error "Header not set: \"$header\""
		}

		switch -exact -- $which {
			last	{lindex [dict get $_req headers $header] end}
			first	{lindex [dict get $_req headers $header] 0}
			all		{dict get $_req headers $header}
		}
	}

	#>>>
	proc http_response args { #<<<
		global _req
		tailcall [dict get $_req conn] http_response {*}$args
	}

	#>>>
}

proc parse_multipart_form-data {content_type body} { #<<<
	set parse_params {param_txt { #<<<
		# RFC 2045 Section 5.1 for parameter syntax
		set token					{[^[:cntrl:]()<>@,;:\\"/[\]?= ]+}
		set qchar_or_quoted_pair	{(?:[^\n"\\]|\\.)}
		set re						[string cat {;\s*(} $token {)=(?:(} $token {)|"(} $qchar_or_quoted_pair {*)")\s*}]

		set params	{}
		foreach {- param pval qpval} [regexp -all -inline $re $param_txt] {
			lappend params [string tolower $param]
			if {$pval ne ""} {
				lappend params $pval
			} else {
				lappend params $qpval
			}
		}
		set params
	}}
	#>>>
	set quote_regexp {str { #<<<
		regsub -all {[^a-zA-Z0-9_-]} $str {\\\0} str
		set str
	}}
	#>>>

	if {![regexp {^([^;]*)(;.*)?$} $content_type - mimetype param_txt]} {
		error "Can't parse content_type: ($content_type)"
	}
	if {$mimetype ne "multipart/form-data"} {
		error "Mimetype must be multipart/form-data, got \"$mimetype\""
	}
	set ct_params	[apply $parse_params $param_txt]
	if {![dict exists $ct_params boundary]} {
		error "No boundary specified: ($content_type)"
	}
	set boundary	[dict get $ct_params boundary]

	set qboundary	[quote_regexp $boundary]
	set lineend		{(?:\r\n|\n|\r)}
	if {![regexp ^.*?--$qboundary-- [encoding convertto utf-8 $body] all]} {
		error "Cannot find parts for boundary $boundary"
	}
	set part_re		[string cat {--} $qboundary $lineend {{1,1}?(.*?} $lineend ) $lineend {(.*?(?=} $lineend {--} $qboundary {))}]
	set form_charset	utf-8
	set parsed_parts	{}
	foreach {- header_txt subbody} [regexp -all -inline -- $part_re $all] {
		set header_txt	[string map {\r\n \n \r \n} $header_txt]
		regsub -all {\n\s+} $header_txt { } header_txt

		unset -nocomplain name
		set charset			_default_
		foreach line [split [string trim $header_txt] \n] {
			if {![regexp {^([^:]+):\s*(.*)$} $line - k v]} {
				error "Unable to parse header line: \"$line\""
			}
			switch -nocase -- $k {
				content-type {
					lassign [regexp -inline {^([^;]*)(;.*)?$} $v] - content_type param_txt
					set params	[apply $parse_params $param_txt]
					if {[dict exists $params charset]} {
						set charset	[dict get $params charset]
					}
				}

				content-disposition {
					lassign [regexp -inline {^([^;]*)(;.*)?$} $v] - disposition_type param_txt

					if {[string tolower $disposition_type] ne "form-data"} {
						puts "Unsupported disposition_type: ($disposition_type)"
					}

					set params	[apply $parse_params $param_txt]

					# RFC 2045 requires that each part contain a content-disposition form-data, which must contain a "name" param
					if {![dict exists $params name]} {
						puts "Content-disposition doesn't contain a name param: ($v)"
						continue
					}
					set name	[dict get $params name]
					if {$name eq "_charset_"} {
						set form_charset	$subbody
					}
				}

				content-transfer-encoding {
					# TODO: implement
				}

				default {
					# RFC 7578 Section 4.8 mandates that headers other than these must be ignored
					puts "ignoring $k"
					continue
				}
			}
		}

		if {[info exists name]} {
			lappend parsed_parts	$name $charset $subbody
		}
	}

	set res {}
	foreach {name charset subbody} $parsed_parts {
		if {$name eq "_charset_"} continue
		lappend res $name
		if {$charset eq "_default_"} {
			set charset	$form_charset
		}
		set charset	[string tolower $charset]
		set charset	[regsub {^iso-} $charset iso]
		set charset	[regsub {^windows-} $charset cp]
		lappend res [encoding convertfrom [string tolower $charset] $subbody]
	}
	set res
}

#>>>
proc set_form {} { #<<<
	global _form

	set method	[req method]
	set _form	[req query]
	if {$method eq "POST"} {
		set content_type	[req headers content-type]
		set body			[req body]
		switch -- $content_type {
			application/x-www-form-urlencoded {
				lappend _form	{*}[reuri::query decode $body]
			}
			multipart/form-data {
				lappend _form	{*}[parse_multipart_form-data $content_type $body]
			}
		}
	}
}

#>>>
proc form_val {name args} { #<<<
	global _form

	parse_args::parse_args $args {
		default	{-default {}}
	}

	foreach {k v} $_form {
		if {$k eq $name} {return $v}
	}

	set default
}

#>>>
proc form_val_exists name { #<<<
	global _form

	foreach {k v} $_form {
		if {$k eq $name} {return true}
	}
	return false
}

#>>>
proc form_all name { #<<<
	global _form
	lmap {k v} $_form {
		if {$k ne $name} continue
		set v
	}
}

#>>>

# vim: foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4
