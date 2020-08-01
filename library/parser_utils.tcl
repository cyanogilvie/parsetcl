namespace eval ::parsertcl {
	proc real_word word { #<<<
		expr {[tok get $word type] eq "word"}
	}

	#>>>
	proc real_words words { #<<<
		set words
	}

	#>>>
	proc last_real_word_number words { #<<<
		- [llength $words] 1
	}

	#>>>
	proc word_start word { #<<<
		$word selectNodes {*[1]}
	}

	#>>>
	proc replace_static {tokens replacement} { #<<<
		error TODO

		set replaced	false
		json foreach tok $tokens {
			if {[tok get $tok type] in {TEXT ESCAPE}} {
				json set out end+1 $replacement
				set replaced	true
			} else {
				json set out end+1 $tok
			}
		}

		if {!$replaced} {
			error "Couldn't find static tokens to replace"
		}

		set out
	}

	#>>>
	proc deep_parse_tokens {tokens params} { #<<<
		foreach token $tokens {
			switch -exact -- [$token nodeName] {
				script {
					deep_parse $token $params
				}
				index {
					deep_parse_tokens [$token selectNodes *]
				}
			}
		}
	}

	#>>>
	proc toklength token { #<<<
		$token getAttribute length
	}

	#>>>
	proc command_range command { #<<<
		list [$command getAttribute idx] [- [$command getAttribute length] 1]
	}

	#>>>
	proc tokname type { #<<<
		set type
	}

	#>>>
	proc word_braced word { #<<<
		#$word selectNodes {boolean(*[1][name()='syntax']/text() == "\{")}
		eq brace [$word getAttribute quoted]
	}

	#>>>
	proc is_unbraced {cmd_text command} { #<<<
		switch -exact -- $cmd_text {
			while - expr {
				return [$command selectNodes {boolean(word[2][@quoted='brace'])}]
			}
			if {
				set words	[$command selectNodes {word[position() > 1]}]
				if {![word_braced [lindex $words 0]]} {return true}
				for {set i 0} {$i < [llength $words]} {incr i} {
					set word	[lindex $words $i]
					if {[$word nodeValue] eq "elseif"} {
						incr i
						set word	[lindex $words $i]
						if {![word_braced $word]} {return true}
					}
				}
				return false
			}
			for {
				return [$command selectNodes {boolean(word[3][@quoted='brace'])}]
			}
		}
	}

	#>>>
	proc deep_parse {script_tok params} { #<<<
		set nop	{apply {args {}}}
		foreach cb {oncommand descend ascend} {
			if {![dict exists $params $cb]} {
				dict set params $cb $nop
			}
		}

		# Scan for script tokens to recurse into
		foreach tok [$script_tok selectNodes {command/word/(script | index/script)}] {
			deep_parse $tok $params
		}

		foreach command [$script_tok selectNodes command] {
			if {![$command hasAttribute name]} continue	;# command name is dynamic

			parse_command $command [$command getAttribute name]
		}
	}

	#>>>
	proc parse_command {cmd name} { #<<<
		variable cmd_parsers

		if {![dict exists $cmd_parsers $name]} return
		apply [dict get $cmd_parsers $name] $cmd
	}

	#>>>
	proc reconstitute_expr tok { #<<<
		$tok text
	}

	#>>>
	proc reconstitute_word word { #<<<
		$word text
	}

	#>>>
	proc reconstitute tok { #<<<
		$tok text
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
				parse expr [nextword]

				set word	[nextword]
				if {[$word getAttribute value] eq "then"} {set word [nextword]}
				parse script $word

				while 1 {
					set word	[nextword]
					switch -exact -- [$word getAttribute value] {
						"elseif" {
							parse expr $word
							parse script [nextword]
						}
						"else" {
							parse script $word
							break
						}
						default {
							parse script $word
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
		"expr"		{cmd {
			set words	[$cmd selectNodes word]
			if {[llength $words] > 2} {
				# TODO: warn about this
			}
			subparse expr [lindex $words 1]
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
		"lmap"		{cmd { ::parsetcl::parse_command $cmd foreach			}}
		"for"		{cmd { #<<<
			lassign [xpath $cmd word] setup condition next body
			subparse script $setup
			subparse expr   $condition
			subparse script $next
			subparse script $body
			#>>>
		}}
		"while"		{cmd { #<<<
			lassign [xpath $cmd word] condition body
			subparse expr   $condition
			subparse script $body
			#>>>
		}}
		"proc"		{cmd { subparse script [xpath $cmd { word[last()] }]	}}
		"catch"		{cmd { subparse script [xpath $cmd { word[2] }]			}}
		"subst"		{cmd { subparse subst  [xpath $cmd { word[last()] }]	}}
		"time"		{cmd { subparse script [xpath $cmd { word[2] }]			}}
		"timerate"	{cmd { #<<<
			# First arg that isn't an option (starting with "-")
			foreach word [xpath $cmd word] {
				if {[string index [$word getAttribute value] 0] eq "-"} continue
				subparse script $word
			}
			#>>>
		}}
		"namespace"	{cmd { #<<<
			set words		[xpath $cmd word]
			set subcommand	[lindex $words 1]
			if {![$subcommand hasAttribute value]} return
			switch -exact -- [$subcommand getAttribute value] {
				eval { subparse script [lindex $words 2] }
			}
			# TODO: namespace code, namespace inscope, etc
			#>>>
		}}
		"try" {cmd { #<<<
			set next		{}
			foreach word [xpath $cmd word] {
				if {[llength $next] > 0} {
					set next	[lassign $next type]
					if {$type ne ""} {
						subparse $type $word
					}
					continue
				}
				if {![get_text literal [lindex $words $p]] && [string index $literal 0]} continue
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
			set words	[$cmd selectNodes word]
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
				[[lindex $words $i] hasAttribute value] &&
				[regexp {^[0-9#]} [[lindex $words $i] getAttribute value]
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
			set i				-1
			set skipping_args	true

			set words	[xpath $cmd word]
			while {$skipping_args && $i < [llength $words]} {
				set word	[lindex $words $i]
				if {![$word hasAttribute value]} break

				switch -exact -- [$word getAttribute value] {
					-exact - -glob - -regexp - -nocase {
						incr i
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
				if {[$handler_word hasAttribute value]} {
					subparse list $handler_word
					set handlers	[xpath $handler_word as/list/word]
				} else {
					# Switch handlers arg is not a static literal
					if {[xpath $handler_word {
						name() = 'script' and
						count(command) = 1 and
						command[1][@name = 'list']
					}]} {
						# This is a special case of dynamic handlers that we might be able to deal with, if the handler scripts are static literals
						set handlers	[xpath $handler_word word]
					} else {
						# TODO: warn about this
						set handlers	{}
					}
				}
			} else { # List arguments
				set handlers	[lrange $words $i end]
			}

			foreach {match handler} $handlers {
				subparse script $handler
			}
			#>>>
		}}
		"dict" {cmd { #<<<
			set words	[xpath $cmd word]
			switch -exact -- [lindex $words 1] {
				filter {
					if {[[lindex $words 3] getAttribute value] eq "script"} {
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

function deep_parse_expr_tokens(tokens, params) {
	var i, token;
	for (i=0; i<tokens.length; i++) {
		token = tokens[i];
		if (token[0] === parser.OPERAND && token[1] === parser.SCRIPT) {
			deep_parse(token[2], params);
		} else if (token[0] === parser.VAR && typeof token[2][1] !== 'string') {
			deep_parse_tokens(token[2][1], params);
		} else if (token[0] === parser.QUOTED) {
			deep_parse_tokens(token[2], params);
		}
	}
	return tokens;
}

function list_elements(listtokens) {
	var i, tok, out=[], ofs, token;

	function emit_elem() {
		if (tok != null) {
			out.push([tok, ofs]);
			tok = null;
			ofs = null;
		}
	}

	for (i=0; i<listtokens.length; i++) {
		token = listtokens[i];
		switch (token[0]) {
			case parser.SPACE:
				emit_elem();
				break;

			case parser.TEXT:
			case parser.ESCAPE:
				if (ofs == null) {ofs = token[3];}
				if (tok == null) {tok = '';}
				tok += token[2] || token[1];
				break;

			case parser.SYNTAX:
				break;

			case parser.END:
				emit_elem();
				break;

			default:
				throw new Error('Unexpected list token: '+JSON.stringify(listtokens[i]));
		}
	}

	return out;
}

function list_words(listtokens) { // Convert to command-style list of words
	var i, token, word=[], words=[];

	for (i=0; i<listtokens.length; i++) {
		token = listtokens[i];

		// Leading space tokens are part of the first word
		if (word.length > 0 && token[0] == parser.SPACE) {
			words.push(word);
			word = [];
		}

		word.push(token);
	}

	if (word.length > 0)
		words.push(word);

	return words;
}

function deep_parse_list_arg(list_arg, special, ofs, params) {
	var words;

	words = list_words(parser.parse_list(list_arg, ofs));
	apply_specials(words, special, params);

	return words;
}

function get_cmd_parse_info(params, cmd_text) {
	if (cmd_parse_info[cmd_text] !== undefined)
		return cmd_parse_info[cmd_text];

	return params.get_cmd_parse_info == null ? null : params.get_cmd_parse_info(params, cmd_text);
}

function apply_specials(command, special, params) {
	var j, k, ofs, txt;

	if (special == null) return;

	for (j=0; j<special.length; j+=2) {
		k = special[j];
		if (command[k] == null) continue;

		//console.warn(cmd_text+' k: '+k+', command.length: '+command.length);
		txt = get_text(command[k], true);
		if (txt == null) {
			// word text is dynamic - comes from a variable or
			// result of a command, so we can't statically parse it
			continue;
		}

		if (Array.isArray(special[j+1])) {
			// Word j is a list, with specials given by the array special[j+1]
			ofs = word_start(command[k]);
			params.descend(command, k, LISTARG);
			command[k] = replace_static(command[k], [
				LISTARG,
				command[k].slice(),
				deep_parse_list_arg(get_text(command[k]), special[j+1], ofs, params),
				ofs
			]);
			params.ascend(command, k, LISTARG);

		} else {
			switch (special[j+1]) {
				case SCRIPTARG:
					ofs = word_start(command[k]);
					params.descend(command, k, SCRIPTARG);
					command[k] = replace_static(command[k], [
						SCRIPTARG,
						command[k].slice(),
						deep_parse(parser.parse_script(txt, ofs), params),
						ofs
					]);
					params.ascend(command, k, SCRIPTARG);
					break;

				case EXPRARG:
					ofs = word_start(command[k]);
					params.descend(command, k, EXPRARG);
					command[k] = replace_static(command[k], [
						EXPRARG,
						command[k].slice(),
						deep_parse_expr_tokens(parser.parse_expr(txt, ofs), params),
						ofs
					]);
					params.ascend(command, k, EXPRARG);
					break;

				case SUBSTARG:
					ofs = word_start(command[k]);
					params.descend(command, k, SUBSTARG);
					command[k] = replace_static(command[k], [
						SUBSTARG,
						command[k].slice(),
						parser.parse_subst(txt, ofs),
						ofs
					]);
					params.ascend(command, k, SUBSTARG);
					break;
			}
		}
	}
}

function process_flags(str, flags) {
	var parts = [], i, m;
	parts = str.split(',');
	for (i=0; i<parts.length; i++) {
		if (!(m = /^\s*(.*?)\s*(?::\s*(.*?)\s*)?$/.exec(parts[i]))) {
			console.warn('Cannot parse flag: "'+parts[i]+'"');
			continue;
		}
		flags[m[1]] = m[2] == null ? true : m[2];
	}
}

export {
	deep_parse,
	word_braced,
	is_unbraced,
	command_range,
	process_flags,
	word_start,
	get_text,
	tokname,
	visualize_space,
	real_words,
	last_real_word_number,
	toklength,
	reconstitute_expr,
	reconstitute_word,
	reconstitute
};

# vim: ts=4 shiftwidth=4 foldmethod=marker foldmarker=<<<,>>>
