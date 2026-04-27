package require parse_args
package require sockopt
package require aio
package require gc_class

namespace eval ::rl_httpd {
	namespace path	[list {*}[namespace path] {*}{
		::parse_args
	}]

	proc serialize_headers headers { #<<<
		set res	{}
		foreach {k vals} $headers {
			foreach {origcase val} $vals {
				append res	[format "%s: %s\n" $origcase [string map [list \n { } \r { }] $val]]
			}
		}
		set res
	}

	#>>>
	proc http_response args { #<<<
		parse_args $args {
			-cur_reqline		{-required}
			-cur_reqhdrs		{-required}
			-type				{-default text/plain}
			-servername			{}
			-status				{-name http_status -default 200}
			-headers			{-default {}}
			-compress			{-boolean}
			-msg				{}
			-body				{-default {}}
			-binary				{-boolean}
			-contentsentlength	{-alias}
		}

		if {![string is integer -strict $http_status] || $http_status < 100 || $http_status >= 600} {
			error "Invalid HTTP status \"$http_status\""
		}

		if {![info exists msg]} {
			switch -glob -- $http_status {
				1* {set msg Informational}
				2* {set msg OK}
				3* {set msg Redirection}
				4* {set msg "Client Error"}
				5* {set msg "Server Error"}
			}
		}

		if {
			[dict get $cur_reqline http_ver] >= 1.1 &&
			[dict exists $cur_reqhdrs connection] &&
			"close" ni [lmap {k v} {dict get $cur_reqhdrs connection} {set v}]
		} {
			set connection	keep-alive
		} else {
			set connection	close
		}

		# TODO: parse this properly
		set content_type	$type
		set encoding		binary
		if {![string match *charset=* $content_type]} {
			switch -glob -- $content_type {
				application/json - application/ld+json -
				application/javascript -
				text/* {
					append content_type "; charset=utf-8"
					set encoding	utf-8
				}
			}
		}

		if {!$binary && $encoding ne "binary"} {
			set body	[encoding convertto $encoding $body]
		}

		if {$compress && [string length $body] > 860} {
			if {[dict exists $cur_reqhdrs accept-encoding]} {
				set acceptible	[lmap {k v_raw} [dict get $cur_reqhdrs accept-encoding] {
					if {![regexp {(?x) ^([^;]*) (?:;q=([0-9.]+))? $} $v_raw - v q]} {
						ns_log warning "Failed to parse accept-encoding value \"$v\""
						continue
					}
					if {$q == 0} continue	;# Otherwise ignore quality factor for now
					if {$v eq "compressed"} {set v compress}	;# ELB-HealthChecker bug
					set v
				}]

				set origsize	[string length $body]
				unset -nocomplain applied_encoding
				if {"br" in $acceptible} {
					set body	[brotli::compress -quality 2 $body]
					set applied_encoding	br
				} elseif {"deflate" in $acceptible} {
					set body	[zlib deflate $body 1]
					set applied_encoding	deflate
				} elseif {"gzip" in $acceptible} {
					set body	[zlib gzip $body -level 1]
					set applied_encoding	gzip
				} elseif {"compress" in $acceptible} {
					set body	[zlib compress $body 1]
					set applied_encoding	compress
				}
				if {[info exists applied_encoding]} {
					dict set headers content-encoding [list Content-Encoding $applied_encoding]
				}
			}
		}

		set content_length		[string length $body]
		set contentsentlength	$content_length

		dict set headers content-type	[list Content-Type		$content_type]
		dict set headers content-length	[list Content-Length	$content_length]
		if {![dict exists headers date]} {
			set http_date	[clock format [clock seconds] -format "%a, %d %b %Y %T GMT" -timezone :UTC]
			dict set headers date		[list Date				$http_date]
		}
		if {![dict exists headers server]} {
			if {![info exists servername]} {
				set servername	"rl_httpd [package require rl_httpd]"
			}
			dict set headers server		[list Server			$servername]
		}

		set resp	[encoding convertto ascii [string map {\n \r\n} "HTTP/1.1 $http_status $msg\n[serialize_headers $headers]\n"]]$body
		set resp
	}

	#>>>
}

gc_class create ::rl_httpd {
	variable {*}{
		listen
		server_status
		onrequest
		conns
		inflight
		servername
	}

	constructor args { #<<<
		if {[self next] ne ""} next

		if {[llength [info commands ::log]] == 0} {
			proc log {lvl msg} {puts stderr $msg}
		}

		if {"::parse_args" ni [namespace path]} {
			namespace path [list {*}[namespace path] {*}{
				::parse_args
			}]
		}

		set inflight		0
		set listen			{}
		set conns			{status {} new {} busy {} reading {} keepalive {} websocket {}}
		my server_status startup

		parse_args $args {
			-servername		{}
			-onrequest		{-required}
		}
		if {![info exists servername]} {
			set servername	"rl_httpd [package require rl_httpd]"
		}
	}

	#>>>
	destructor { #<<<
		my stop_listening
		if {[self next] ne ""} next
	}

	#>>>
	method listen_http args { #<<<
		parse_args $args {
			-port		{-default 80}
			-reuseport	{-boolean}
		}
		log notice "Listing on http port $port"
		dict set listen http	[socket -server [namespace code {my _accept http}] -reuseport $reuseport $port]
	}

	#>>>
	method listen_https args { #<<<
		parse_args $args {
			-cadir		{-default /etc/ssl/certs}
			-certfile	{-required}
			-port		{-default 443}
			-reuseport	{-boolean}
		}

		package require tls
		if {[lsearch -index 1 $::tls::socketOptionRules -reuseport] == -1} {
			lappend ::tls::socketOptionRules {1 -reuseport sopts 1}		;# Patch in wiring for -reuseport
			unset -nocomplain ::tls::socketOptionsSwitchBody			;# Force the init to happen again
		}
		log notice "Listing on httpd port $port"
		dict set listen https [tls::socket \
			-server		[namespace code {my _accept https}] \
			-reuseport	$reuseport \
			-cadir		$cadir \
			-certfile	$certfile \
			-request	false \
			-require	false $port]
	}

	#>>>
	method _accept {proto sock peer_ip peer_port} { #<<<
		set conn	[rl_httpd_conn new accept [self] $proto $sock $peer_ip $peer_port]
		after 0 [list $conn start_handle]
	}

	#>>>
	method stop_listening {} { #<<<
		dict for {proto sock} $listen {
			close $sock
		}
		set listen	{}
		foreach conn [my conns keepalive] {
			$conn destroy
		}
		# TODO: close websockets
	}

	#>>>
	method drain args { # Wait for all currently processing requests to finish <<<
		parse_args $args {
			-timeout	{-# {Max time in fractional seconds to wait for requests to finish}}
		}
		set horizon	[expr {[clock microseconds] + $timeout*1e6}]

		try {
			while {$inflight > 0} {aio coro_vwait inflight [expr {($horizon - [clock microseconds])/1e6}]}
		} trap {AIO TIMEOUT CORO_VWAIT inflight} {} {
			throw {RL_HTTPD TIMEOUT DRAIN} "Timeout waiting for active connections to finish, $inflight remain"
		}
	}

	#>>>
	method handle_http_req {conn proto reqline headers body} { #<<<
		#log notice "[self] handle_http_req $proto [dict get $reqline raw]"
		try {
			{*}$onrequest $conn $proto $reqline $headers $body
		} on error {errmsg options} {
			log error "HTTPD onrequest error ([dict get $options -errorcode]): [dict get $options -errorinfo]"
			try {
				$conn http_response -status 500 -msg {Server error} -body "Server error\n"
			} trap {RL HTTPD ALREADY_RESPONDED} {} {
			}
			return close
		}
	}

	#>>>
	method server_status args { #<<<
		parse_args $args {
			newstatus	{-enum {startup running stopping stopped}}
		}
		if {[info exists newstatus]} {
			set server_status $newstatus
		}
		set server_status
	}

	#>>>
	method conn_status args { #<<<
		parse_args $args {
			conn	{-required}
			status	{-enum {new busy reading keepalive websocket closed}}
		}
		if {[dict exists $conns status $conn]} {
			dict unset conns [dict get $conns status $conn] $conn
		}
		if {$status eq "closed"} {
			dict unset conns status $conn
		} else {
			dict set conns status $conn $status
			dict set conns $status $conn [clock microseconds]
		}
	}

	#>>>
	method conns args { #<<<
		parse_args $args {
			status	{-enum {new busy reading keepalive websocket} -default status}
		}

		dict keys [dict get $conns $status]
	}

	#>>>
	method inflight args { #<<<
		parse_args $args {
			inc	{-enum {inc dec}}
		}

		if {[info exists inc]} {
			incr inflight [expr {$inc eq "inc" ? 1 : -1}]
		} else {
			set inflight
		}
	}

	#>>>
	method servername {} { #<<<
		set servername
	}

	#>>>
	method thaw state { #<<<
		set conn	[rl_httpd_conn new thaw [self] $state]
	}

	#>>>
}

gc_class create ::rl_httpd_conn {
	variable {*}{
		proto
		sock
		accepttime
		peer_ip
		peer_port
		status
		timeout_ref
		processing
		httpd
		cur_reqline
		cur_reqhdrs
		responded
		sls
	}

	constructor {mode args} { #<<<
		if {"::parse_args" ni [namespace path]} {
			namespace path [list {*}[namespace path] {*}{
				::parse_args
			}]
		}

		set processing	0
		set responded	0

		if {[llength [info commands ::log]] == 0} {
			proc log {lvl msg} {puts stderr $msg}
		}

		switch -exact -- $mode {
			accept	{ my accept {*}$args }
			thaw	{ my thaw {*}$args   }
			default	{ error "Invalid rl_httpd_conn constructor mode: \"$mode\"" }
		}

		if {[self next] ne ""} next
	}

	#>>>
	destructor { #<<<
		if {$processing && [info exists httpd] && [info object isa object $httpd]} {$httpd inflight dec}
		if {[info exists sock] && $sock in [chan names]} {
			close $sock
		}
		unset -nocomplain sock

		if {[llength [info commands [namespace current]::coro_handle]] > 0} {
			rename coro_handle {}
		}
		if {[info exists status] && $status ne "frozen"} {
			set status	closed
			$httpd conn_status [self] $status
		}

		if {[self next] ne {}} next
	}

	#>>>
	method start_handle {} { #<<<
		if {[llength [info commands [namespace current]::coro_handle]] > 0} {
			error "Handler coro already running"
		}
		coroutine coro_handle my handle
	}

	#>>>
	method freeze {} { #<<<
		if {[my status] ne {new keepalive}} {
			return {}
		}

		set state	[list \
			status		[my status] \
			proto		$proto \
			sock		$sock \
			accepttime	$accepttime \
			peer_ip		$peer_ip \
			peer_port	$peer_port \
			timeout_ref	$timeout_ref \
			sls			$sls \
		]
		thread::detach $sock
		unset -nocomplain sock
		my status frozen
		if {[llength [info commands [namespace current]::coro_handle]] > 0} {
			rename coro_handle {}
		}
		my destroy
	}

	#>>>
	method thaw {a_httpd state} { #<<<
		set httpd	$a_httpd
		dict with state {}
		thread::attach $sock
		my start_handle
	}

	#>>>
	method accept args { #<<<
		set accepttime	[clock microseconds]
		parse_args $args {
			httpd		{-required}
			proto		{-required}
			sock		{-required}
			peer_ip		{-required}
			peer_port	{-required}
		}

		set status		{}
		set sls			{}
		my status new

		sockopt::setsockopt $sock IPPROTO_TCP TCP_NODELAY 1
	}

	#>>>
	method handle {} { #<<<
		set cleanup	[list apply {{obj old new op} {
			if {[info object isa object $obj]} {
				if {[$obj status] ne "frozen"} {
					$obj destroy
				}
			}
		}} [self]]
		trace add command [info coroutine] delete $cleanup
		if {![info exists timeout_ref]} {
			set timeout_ref	[clock microseconds]
		}
		try {
			while 1 {
				chan configure $sock -blocking 0 -buffering line -encoding ascii
				unset -nocomplain cur_reqline cur_reqhdrs
				lassign [my _read_headers true] reqline headers
				set body	[my _read_body $reqline headers]

				my status busy
				set cur_reqline	$reqline
				set cur_reqhdrs	$headers
				set responded	0
				set handle_status	[$httpd handle_http_req [self] $proto $reqline $headers $body]
				set processing	0
				$httpd inflight dec
				switch -exact -- $handle_status {
					close		return
					closed		{unset sock; return}
					detached	break
					keepalive {
						if {!$responded} {
							log warning "No response sent for reqest [dict get $reqline raw]"
							throw {RL HTTPD CLOSE_WITH 500} "No response"
						}
						if {[$httpd server_status] ni {startup running}} return
					}
					default		{error "Unhandled handle_http_req status: \"$handle_status\""}
				}

				my status keepalive
			}

			my status websocket
			# TODO: websocket readable loop
			chan configure $sock -blocking 0 -buffering none -translation binary
			while 1 {
				set msg	[read_websocket_msg $sock]
				handle_websocket_msg $sock $msg
			}
		} trap {RL HTTPD READ_HEADERS closed} {} {
		} trap {RL HTTPD CLOSE_WITH} {msg options} {
			set http_status	[lindex [dict get $options -errorcode] 3]
			my http_response -status $http_status -msg $msg -headers {connection {Connection close}} -body $msg
		} on error {errmsg options} {
			log error "Error handling http sock $sock: [dict get $options -errorinfo]"
		} finally {
			if {[info coroutine] ne {}} {
				trace remove command [info coroutine] delete $cleanup
			}
			my destroy
		}
	}

	#>>>
	method _read_headers {{consume_reqline false}} { #<<<
		set firstline	1
		set resp_headers_buf	""
		chan configure $sock -buffering line -translation {auto crlf} -encoding ascii
		while 1 {
			set line	[gets $sock]
			#log notice "[info coroutine] _read_headers got line ($line), blocked: ([chan blocked $sock]), eof: ([chan eof $sock])"

			if {$firstline && $line ne {}} {
				set timeout_ref	[clock microseconds]
				set firstline	0
				if {!$processing} {
					$httpd inflight inc
					set processing	1
				}
				my status reading
			}

			if {[eof $sock]} {
				if {$consume_reqline && ![info exists reqline]} {
					# Client closed the connection before sending another request
					set headers_status	closed
				} else {
					set headers_status	dropped
				}
				break
			}

			if {![chan blocked $sock]} {
				if {$consume_reqline && ![info exists reqline]} {
					if {$line eq ""} {
						# This is expressly forbidden in the HTTP RFC, but for some
						# reason I'm getting these from the sugarcrm rest api
						continue
					}
					set reqline	$line
					continue
				}

				if {$line eq ""} {
					set headers_status	ok
					break
				}

				append resp_headers_buf $line \n
			} else {
				try {
					aio waitfor readable $sock [my _remaining_timeout]
				} trap {AIO TIMEOUT READ} {errmsg options} {
					if {$consume_reqline && ![info exists reqline]} {
						# Timeout waiting for a new request on a keepalive channel
						set header_status	closed
					} else {
						return -options $options $errmsg
					}
				}
			}
		}

		if {$headers_status ne "ok"} {
			throw [list RL HTTPD READ_HEADERS $headers_status] "Error reading HTTP headers: $headers_status"
		}

		if {$consume_reqline} {
			list [my _parse_reqline $reqline] [my _parse_headers $resp_headers_buf]
		} else {
			my _parse_headers $resp_headers_buf
		}
	}

	#>>>
	method _parse_reqline reqline { #<<<
		if {![regexp {^([-!#$%&'*+.^_`|~0-9a-zA-Z]*) (.*?) HTTP/([0-9].[0-9])$} $reqline - method request_target http_ver]} {
			throw [list RL HTTPD PARSE_HEADERS $reqline] "Invalid HTTP request line: \"$reqline\""
		}

		foreach v {scheme userinfo host port path query} {set $v {}}

		set r_sub_delims	{[!$&'()*+,;=]}
		set r_pct_encoded	{%[0-9a-fA-F]{2}}
		set r_alpha			{[a-zA-Z]}
		set r_digit			{[0-9]}
		set r_hexdigit		{[0-9a-fA-F]}
		set r_unreserved	"(?: $r_alpha | $r_digit | \[-._~\] )"
		set r_userinfo		"(?: $r_unreserved | $r_pct_encoded | $r_sub_delims | : )*"
		set r_dec_octet		"(?: $r_digit | \[1-9\] $r_digit | 1 $r_digit{2} | 2 \[0-4\] $r_digit | 25 \[0-5\] )"
		set r_ipv4address	"$r_dec_octet \. $r_dec_octet \. $r_dec_octet \. $r_dec_octet"
		set r_ipvfuture		"v $r_hexdigit+ \. (?: $r_unreserved | $r_sub_delims | : )+"
		set r_h16			"$r_hexdigit{1,4}"
		set r_ls32			"(?: $r_h16 : $r_h16 | $r_ipv4address )"
		set r_ipv6address	"(?:
											($r_h16 :){6} $r_ls32
			|                           :: ($r_h16 :){5} $r_ls32
			| (                $r_h16)? :: ($r_h16 :){4} $r_ls32
			| (($r_h16 :){0,1} $r_h16)? :: ($r_h16 :){3} $r_ls32
			| (($r_h16 :){0,2} $r_h16)? :: ($r_h16 :){2} $r_ls32
			| (($r_h16 :){0,3} $r_h16)? ::  $r_h16 :     $r_ls32
			| (($r_h16 :){0,4} $r_h16)? ::               $r_ls32
			| (($r_h16 :){0,5} $r_h16)? ::               $r_h16
			| (($r_h16 :){0,6} $r_h16)? ::
		)
		"
		set r_ip_literal	"\\\[ (?: $r_ipv6address | $r_ipvfuture ) \\\]"
		set r_reg_name		"(?: $r_unreserved | $r_pct_encoded | $r_sub_delims )*"
		set r_host			"(?: $r_ip_literal | $r_ipv4address | $r_reg_name )"
		set r_port			"$r_digit*"
		set r_scheme		"$r_alpha (?: $r_alpha | $r_digit | \[-+.\])*"
		set r_pchar			"(?: $r_unreserved | $r_pct_encoded | $r_sub_delims | \[:@\] )"
		set r_path_abempty	"(?: / $r_pchar* )*"
		set r_path_absolute	"/ (?: $r_pchar+ (?: / $r_pchar* )* )?"
		set r_path_empty	""
		set r_path_rootless	"$r_pchar+ (?: / $r_pchar* )*"
		set r_query			"(?: $r_pchar | \[/?\] )*"

		set patterns	{}

		lappend patterns "(?x)^(?: ($r_userinfo) @)? ($r_host) (?: : ($r_port))?\$" {
			# authority
			lassign $m - userinfo host port
			set reqline_type	authority
		}

		lappend patterns "(?x)^($r_scheme):
			(?:
					(?: (//) ($r_userinfo) @ ($r_host) (?: : ($r_port))? ($r_path_abempty) )
				| (?: ($r_path_absolute | $r_path_rootless | $r_path_empty) )
			)
			(?: \\? ($r_query) )?
			\$" {
			# absolute_uri
			lassign $m - scheme form1 userinfo host port path1 path2 query
			set reqline_type	absolute_uri
			if {$form1 eq "//"} {
				set path	$path1
			} else {
				set path	$path2
			}
		}

		lappend patterns "(?x)^ ($r_path_abempty) (?: \\? ($r_query) )? \$" {
			# origin_form
			set reqline_type	origin_form
			lassign $m - path query
		}

		lappend patterns {(?x)^\*$} {
			set reqline_type	*
			# TODO: what does "*" mean?
		}

		switch -regexp -matchvar m -- $request_target {*}$patterns default {
			throw [list RL HTTPD PARSE_HEADERS $reqline"] "Invalid HTTP request line: \"$reqline\" - request_target malformed"
		}

		dict create \
			raw				$reqline \
			method			$method \
			request_target	$request_target \
			http_ver		$http_ver \
			reqline_type	$reqline_type \
			scheme			$scheme \
			userinfo		$userinfo \
			host			$host \
			port			$port \
			path			$path \
			query			$query
	}

	#>>>
	method _parse_headers header_txt { #<<<
		set headers	{}

		# Unfold headers
		regsub -all {\n\s+} $header_txt { } header_txt

		foreach line [split [string trim $header_txt] \n] {
			if {![regexp {^([^:]+):\s*(.*)$} $line - k v]} {
				throw [list RL HTTPD PARSE_HEADERS $line] "Unable to parse HTTP response header line: \"$line\""
			}
			my _append_headers headers $k $v
		}

		set headers
	}

	#>>>
	method _append_headers {headervar k raw_value} { #<<<
		upvar 1 $headervar headers
		set lk	[string tolower $k]
		# TODO: properly research the set of standard headers that don't follow the , separated values format
		switch -- $lk {
			set-cookie - cookie - user-agent	{ set vlist	[list $raw_value] }
			default				{ set vlist	[lmap v [split $raw_value ,] {string trim $v}] }
		}

		foreach v $vlist {
			dict lappend headers $lk $k $v
		}
	}

	#>>>
	method _read_body {reqline headersvar} { #<<<
		upvar 1 $headersvar headers

		# Determine message body length as per RFC7230 section 3.3.3 <<<
		# Only applies to responses to these methods
		#if {[dict get $reqline method] in {HEAD CONNECT}} {
		#	# No body, regardless of any header fields that might indicate a length
		#	return
		#}

		if {[dict exists $headers transfer-encoding]} {
			if {[dict exists $headers content-length]} {
				# MUST strip content-length if it was present
				dict unset headers content-length
			}

			set last_te	[lindex [dict get $headers transfer-encoding] end 1]
			if {[string tolower $last_te] eq "chunked"} {
				# Message length is determined by chunked transfer coding
				set body_len_mode	chunked
			} else {
				# Message length cannot be determined, MUST reject with 400 Bad Request and close the connection
				throw [list RL HTTPD CLOSE_WITH 400] "Bad Request"
			}
		} elseif {[dict exists $headers content-length]} {
			set body_len_mode	content_length
		} else {
			# Length is 0
			set body_len_mode	none
		}
		# Determine message body length as per RFC7230 section 3.3.3 >>>

		switch -exact -- $body_len_mode {
			content_length {
				set content_lengths	[lsort -unique [lmap {k v} [dict get $headers content-length] {set v}]]
				if {[llength $content_lengths] > 1} {
					# Invalid to have multiple Content-Length headers with different values
					# MUST close with 400 Bad Request
					throw [list RL HTTPD CLOSE_WITH 400] "Bad Request"
				}
				set content_length	[lindex $content_lengths 0]
				set expecting	$content_length
				chan configure $sock -buffersize [expr {min(1000000, $expecting)}] -buffering full -translation binary

				set body	""
				while 1 {
					set body_status	[my _readable_body body $expecting]
					if {$body_status ne ""} break
					aio waitfor readable $sock [my _remaining_timeout]
				}

				if {$body_status ne "ok"} {
					throw [list RL HTTPD READ_BODY $body_status] "Error reading HTTP request body: $body_status"
				}

				# Check content-length (if provided) to ensure we got the whole response body
				if {[string length $body] != $content_length} {
					throw [list RL HTTPD READ_BODY truncated] "Expecting $content_length bytes in HTTP request body, got [string length $body]"
				}
			}

			chunked {
				set total_expecting	0
				while 1 {
					lassign [my _read_chunk_control] length chunk_extensions
					if {$length == 0} break
					incr total_expecting $length
					my _read_chunk_data body $total_expecting
				}
				dict for {lk v} [my _read_headers $sock] {
					# Ignore trailer forbidden headers
					if {$k in {
						content-type
						transfer-encoding
						host
						www-authenticate
						authorization
						proxy-authenticate
						proxy-authorization
						cookie
						set-cookie
						age
						cache-control
						expires
						date
						location
						retry-after
						vary
						warning
						content-encoding
						content-type
						content-range
						trailer
					} || [string match if-* $lk]} continue

					dict lappend headers $lk {*}$v
				}
			}

			none {
				set body	""
			}

			default {
				error "Invalid body_len_mode: ($body_len_mode)"
			}
		}

		# Decode transfer-encoding and content-encoding <<<
		foreach header {transfer-encoding content-encoding} {
			if {[dict exists $headers $header]} {
				foreach enc [lreverse [lmap {k v} [dict get $headers $header] {set v}]] {
					switch -nocase -- $enc {
						chunked {
							# Handled during read
						}
						base64                { set body	[binary decode base64 $body] }
						gzip - x-gzip         { set body	[zlib gunzip $body] }
						deflate               { set body	[zlib inflate $body] }
						compress - x-compress { set body	[zlib decompress $body] }
						identity - 8bit - 7bit - binary {}
						default {
							throw [list RL HTTPD READ_BODY unhandled_encoding $enc] "Unhandled HTTP request body $header: \"$enc\""
						}
					}
				}
			}
		}
		# Decode transfer-encoding and content-encoding >>>
		# Convert from the specified charset encoding (if supplied) <<<
		if {[dict exists $headers content-type]} {
			set content_type	[lindex [dict get $headers content-type] end 1]
			if {[regexp -nocase {^((?:text|application)/[^ ]+)(?:\scharset=\"?([^\"]+)\"?)?$} $content_type - mimetype charset]} {
				if {$charset eq ""} {
					# Some mimetypes have default charsets
					switch -- $mimetype {
						application/json -
						text/json {
							set charset		utf-8
						}

						application/xml -
						text/xml {
							# According to the RFC, text/xml should default to
							# US-ASCII, but this is widely regarded as stupid,
							# and US-ASCII is a subset of UTF-8 anyway.  Any
							# documents that fail because of an invalid UTF-8
							# encoding were broken anyway (they contained bytes
							# not legal for US-ASCII either)
							set charset		utf-8
						}

						default {
							set charset		identity
						}
					}
				}

				switch -nocase -- $charset {
					utf-8        { set body [encoding convertfrom utf-8     $body] }
					iso-8859-1   { set body [encoding convertfrom iso8859-1 $body] }
					windows-1252 { set body [encoding convertfrom cp1252    $body] }
					identity     {}
					default {
						# Only broken servers will land here - we specified the set of encodings we support in the
						# request Accept-Encoding header
						throw [list RL HTTPD READ_BODY UNHANDLED_CHARSET $charset] "Unhandled HTTP request body charset: \"$charset\""
					}
				}
			}
		}
		# Convert from the specified charset encoding (if supplied) >>>

		set body
	}

	#>>>
	method _readable_body {bufvar {expecting ""}} { #<<<
		upvar 1 $bufvar resp_body_buf
		if {$expecting ne ""} {
			set chunk	[read $sock [expr {$expecting - [string length $resp_body_buf]}]]
		} else {
			set chunk	[read $sock]
		}
		append resp_body_buf	$chunk

		if {[eof $sock]} {
			close $sock
			return ok
		}
		if {$expecting ne ""} {
			set remain		[expr {$expecting - [string length $resp_body_buf]}]
			if {$remain <= 0} {
				return ok
			}
			chan configure $sock -buffersize [expr {min(1000000, $remain)}]
		}
		return
	}

	#>>>
	method _read_chunk_data {bodyvar length} { #<<<
		upvar 1 $bodyvar body

		set expecting	[expr {$length + 2}]		;# +2: trailing \r\n
		chan configure $sock -buffersize [expr {min(1000000, $expecting)}] -buffering full -translation binary

		while 1 {
			unset -nocomplain wait
			set body_status	[my _readable_body body $expecting]
			if {$body_status ne ""} break
			aio waitfor readable $sock [my _remaining_timeout]
		}

		if {$body_status ne "ok"} {
			throw [list RL HTTPD READ_BODY $body_status] "Error reading HTTP request chunk: $body_status"
		}

		if {[string range $body end-1 end] ne "\r\n"} {
			throw [list RL HTTPD READ_BODY CORRUPT_CHUNKED] "Corrupt HTTP Transfer-Encoding: chunked body"
		}
		set body	[string range [try {set body} finally {unset body}] 0 end-2]
	}

	#>>>
	method _read_chunk_control {} { #<<<
		chan configure $sock -translation {auto crlf} -encoding ascii -buffering line

		while 1 {
			set chunk_buf	[gets $sock]
			tsv::incr server_stats bytes_in [expr {[string length $chunk_buf]+2}]

			if {[eof $sock]} {
				set body_status	dropped
				break
			}

			if {![chan blocked $sock]} {
				set body_status	ok
				break
			}

			aio waitfor readable $sock [my _remaining_timeout]
		}

		if {$body_status ne "ok"} {
			throw [list RL HTTP READ_BODY $body_status] "Error reading HTTP chunk control line: $body_status"
		}

		if {![regexp {^([0-9a-fA-F]+)(?:;(.+))?$} $chunk_buf - octets chunk_extensions_enc]} {
			throw [list RL HTTP READ_BODY CORRUPT_CHUNKED] "Corrupt HTTP Transfer-Encoding: chunked body"
		}

		# Convert chunk_extensions to a dict
		set chunk_extensions	[concat {*}[lmap e [split $chunk_extensions_enc ";"] {
			regexp {^([^=]+)(?:=(.*))?$} $e - name value
			list $name $value
		}]]

		set octets	0x$octets

		list $octets $chunk_extensions
	}

	#>>>
	method _remaining_timeout {} { #<<<
		switch -exact -- $status {
			new			{return 25}
			keepalive	{return 120}
			reading		-
			busy		{
				set elapsed			[expr {([clock microseconds] - $timeout_ref)/1e6}]
				return [expr {max(0, 10.0 - $elapsed)}]
			}
			default {
				log error "Invalid sock_status for $sock: \"$status\""
			}
		}
	}

	#>>>
	method http_response args { #<<<
		if {$responded} {
			throw {RL HTTPD ALREADY_RESPONDED} "A response was already sent for this request"
		}

		set resp	[::rl_httpd::http_response \
			-cur_reqline		$cur_reqline \
			-cur_reqhdrs		$cur_reqhdrs \
			-servername			[$httpd servername] \
			{*}$args \
			-contentsentlength	contentsentlength \
		]

		dict set res contentsentlength	$contentsentlength
		dict set res bytes_out	[string length $resp]

		my http_response_raw $resp

		set res
	}

	#>>>
	method http_response_raw bytes { #<<<
		# With this write in non-blocking mode, and only when going through the AWS ALB, large responses are truncated for some reason
		chan configure $sock -blocking 1 -buffering none -translation binary
		try {
			puts -nonewline $sock $bytes
			flush $sock
			set responded	1
		} finally {
			chan configure $sock -blocking 0
		}
	}

	#>>>
	method peer_ip {} {set peer_ip}
	method peer_port {} {set peer_port}
	method status args { #<<<
		parse_args $args {
			newstatus	{-enum {new busy reading keepalive websocket closed frozen}}
		}
		if {[info exists newstatus]} {
			if {$newstatus ne $status} {
				#log notice "httpd conn [self] in [thread::id] status transition: $status -> $newstatus"
				set status	$newstatus
				$httpd conn_status [self] $status
			}
		}
		set status
	}

	#>>>
	method sls args { #<<<
		parse_args $args {
			op		{-required -enum {set get unset replace}}
			args	{-name rest}
		}

		switch -exact -- $op {
			set {
				parse_args $rest {
					key		{-required}
					value	{-required}
				}
				dict set sls $key $value
				set value
			}

			get {
				parse_args $rest {
					key		{}
					default	{}
				}

				if {![info exists key]} {
					return $sls
				}

				if {[dict exists $sls $key]} {
					dict get $sls $key
				} else {
					if {[info exists default]} {
						return $default
					}
					throw {RL HTTPD SLS NOTFOUND} "HTTPD SLS key \"$key\" not found"
				}
			}

			unset {
				parse_args $rest {
					key		{-required}
				}
				dict unset sls $key
			}

			replace {
				parse_args $rest {
					replacement	{-required -validate {dict size}}
				}
				set sls	$replacement
			}
		}
	}

	#>>>
}

# vim: ft=tcl foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4
