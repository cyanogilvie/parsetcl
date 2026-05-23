#!/usr/bin/env cftcl

set here	[file dirname [file normalize [info script]]]
tcl::tm::path add [file join $here tm]
package require rl_httpd
package require chantricks
package require reuri
package require brotli
package require parsetcl

source [file join $here req.tcl]
source [file join $here syntax.tcl]

set nsadmin_cmd_parsers_fn	[file join $::env(HOME) git/rl/helpers/nsadmin_cmd_parsers.tcl]
if {[file readable $nsadmin_cmd_parsers_fn]} {
	source $nsadmin_cmd_parsers_fn
}

proc serve_asset {} { #<<<
	global here
	set path		[req path]
	set filepath	[file join $here [string trimleft $path /]]
	if {[string match */..* $path] || ![file readable $filepath]} {
		req http_response -status 404 -type text/plain -body {Not found}
		return keepalive
	}

	set binary	true
	switch -nocase -glob -- $path {
		*.html			{set contentType	text/html; set binary false}
		*.css			{set contentType	text/css; set binary false}
		*.js			{set contentType	application/javascript; set binary false}
		*.pdf			{set contentType	application/pdf}
		*.jpg			{set contentType	image/jpeg}
		*.jpeg			{set contentType	image/jpeg}
		*.png			{set contentType	image/png}
		*.webp			{set contentType	image/webp}
		*.svg			{set contentType	image/svg+xml; set binary false}
		*/manifest.json	{set contentType	application/manifest+json; set binary false}
		default			{set contentType	application/octet-stream}
	}

	set extra	{}
	if {$binary} {
		set body	[chantricks::readbin $filepath]
	} else {
		set body	[chantricks::readfile $filepath]
		lappend extra -compress
	}

	req http_response -status 200 -type $contentType -body $body {*}$extra
	return keepalive
}

#>>>
proc fetch_image name { #<<<
	global here
	package require Pixel_webp
	pixel::pmap_to_pmapf [pixel::webp::decode [chantricks::readbin [file join $here images $name.webp]]]
}

#>>>
proc pod_lite {} { #<<<
	package require Pixel
	package require Pixel_webp
	package require Pixel_jpeg

	set pathlist	[lrange [reuri::path split [req path]] 1 end]
	set size		[lindex $pathlist 1]
	set name		[lindex $pathlist 2]

	if {![regexp {^([0-9]+)(?:x([0-9]+))?$} $size - px density]} {
		puts "Can't parse size: ($size)"
		throw {http_status 404} "Not found"
	}

	if {$density eq ""} {set density 1}

	if {0 && $px ni {150 300 400 700}} {
		# Limit the dimensions we're willing to generate
		puts "px isn't valid: ($px)"
		throw {http_status 404} "Not found"
	}

	set src	[fetch_image [file rootname $name]]

	lassign [pixel::pmapf_info $src] width height

	set dim [expr {int(round($px * $density))}]

	set new_w	$px

	# Compute the missing dimension from the fixed one, preserving aspect ratio
	if {![info exists new_h]} {
		set new_h   [expr {int(round($height * double($new_w)/$width))}]
	} elseif {![info exists new_w]} {
		set new_w   [expr {int(round($width * double($new_h)/$height))}]
	}

	if {$width == $new_w && $height == $new_h} {
		set scaled	$src
	} else {
		set scaled	[pixel::scale_pmapf_lanczos $src $new_w $new_h]
	}

	switch -exact -- [file extension $name] {
		.webp {
			set content_type	image/webp
			if {$density == 1} {
				set q	82
			} elseif {$density <= 2} {
				set q	75
			} else {
				set q	60
			}
			set bytes		[pixel::webp::encode [pixel::pmapf_to_pmap $scaled] $q]
		}
		.jpg {
			set content_type	image/jpeg
			if {$density == 1} {
				set q	82
			} elseif {$density <= 2} {
				set q	80
			} else {
				set q	75
			}
			set bytes		[pixel::jpeg::encodejpeg [pixel::pmapf_to_pmap $scaled] $q]
		}

		default {
			throw {http_status 404} "Not found"
		}
	}

	req http_response -type $content_type -headers {cache-control {Cache-Control max-age=31536000}} -body $bytes
	return keepalive
}

#>>>
proc page_syntax {} { #<<<
	set script	[if {[req method] eq "PUT"} {
		req body
	} else {
		chantricks::readfile [file join $::here [form_val script t.tcl]]
	}]
	set body	"<!DOCTYPE html>\n"
	append body [html {
		<html {
			<head {
				<title {txt "Tcl Syntax View"}
				#<link rel "stylesheet" href "/assets/core.css"
				#<link rel "stylesheet" href "/assets/colour.css"
				<link rel "stylesheet" href "/assets/core_alt.css"
				<link rel "stylesheet" href "/assets/colour_alt.css"
				<link rel "manifest" href "/assets/manifest.json" crossorigin "use-credentials"
			}
			<body {
				set before	[clock microseconds]
				set pt	[parsetcl parsetree $script]
				puts "parse: [format %.3f [expr {([clock microseconds]-$before)/1e3}]] ms"
				highlight_script [xpath [parsetcl node $pt] /tcl/script] main ""
				<script type "module" {txt {
					if ('serviceWorker' in navigator)
						navigator.serviceWorker.register('/assets/serviceworker.js?9');

					document.addEventListener('beforeinstallprompt', ev => {
						console.log('beforeinstallprompt');
						ev.prompt();
					});
				}}
			}
		}
	}]

	req http_response -status 200 -type text/html -body $body
	return keepalive
}

#>>>
proc page_404 {} { #<<<
	req http_response -status 404 -type text/plain -body "Not found"
	return keepalive
}

#>>>

rl_httpd instvar httpd -onrequest [list apply {{conn proto reqline headers body} {
	puts "HIT: [dict get $reqline raw]"
	try {
		req_init $conn $proto $reqline $headers $body
		switch -glob -- [dict get $reqline method],[dict get $reqline path] {
			GET,/assets/*	serve_asset
			GET,/image/*	pod_lite

			GET,/ -
			GET,/syntax	-
			PUT,/syntax		page_syntax

			default			page_404
		}
	} on error {errmsg options} {
		$conn http_response -status 500 -type text/plain -body "[dict get $options -errorcode]: $errmsg\n[dict get $options -errorinfo]"
	}
	return keepalive
}}]

$httpd listen_http -port 18086

if {![info exists exit]} {
	vwait exit
}
exit $exit

# vim: foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4
