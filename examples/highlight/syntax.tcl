set here	[file dirname [file normalize [info script]]]
tcl::tm::path add [file join $here tm]
namespace eval ::rl_html {}
dict set ::rl_html::custom_tags tcl_syntax {
	tcl-script
	tcl-space
	tcl-comment
	tcl-command
	tcl-word
	tcl-tokens
	tcl-end
	tcl-var
	tcl-scalar
	tcl-array
	tcl-array-name
	tcl-array-index
	tcl-escape
	tcl-list
	tcl-quoted
	tcl-braced
	tcl-expr
	tcl-subexpr
	tcl-op
	tcl-as
	tcl-txt
	tcl-syntax
}
package require parsetcl
package require chantricks
package require tdom
package require jitc
package require rl_html

namespace import parsetcl::xpath

# valid_varname <<<
interp alias {} valid_varname {} ::jitc::capply {
	filter	{jitc::re2c -W --case-ranges}
	options	{-Wall -Werror -gdwarf-5}
	code {
		//@begin=c@
		static Tcl_Obj*	g_true = NULL;
		static Tcl_Obj*	g_false = NULL;

		INIT {
			replace_tclobj(&g_true,  Tcl_NewBooleanObj(1));
			replace_tclobj(&g_false, Tcl_NewBooleanObj(0));
			return TCL_OK;
		}

		RELEASE {
			replace_tclobj(&g_true,  NULL);
			replace_tclobj(&g_false, NULL);
		}

		OBJCMD(valid) {
			enum {A_cmd, A_STR, A_objc};
			CHECK_ARGS("str");
			const char*	str = Tcl_GetString(objv[A_STR]);
			const char* s = str;
			const char* YYMARKER;

			/*!re2c
				re2c:yyfill:enable		= 0;
				re2c:define:YYCTYPE		= "char";
				re2c:define:YYCURSOR	= "s";
				
				end			= [\x00];
				namechar	= [_a-zA-Z0-9] | "::";
				name		= namechar+;

				name end	{Tcl_SetObjResult(interp, g_true);  return TCL_OK;}
				*			{Tcl_SetObjResult(interp, g_false); return TCL_OK;}
			*/
		}
		//@end=c@
	}
} valid
# valid_varname >>>
# whitespace_sections <<<
interp alias {} whitespace_sections {} ::jitc::capply {
	filter	{jitc::re2c -W --case-ranges}
	options	{-Wall -Werror -gdwarf-5}
	code {
		//@begin=c@
		OBJCMD(whitespace_sections) {
			int			code = TCL_OK;
			enum {A_cmd, A_STR, A_OFS, A_objc};
			CHECK_ARGS("str ofs");
			const char*	str = Tcl_GetString(objv[A_STR]);
			const char* s = str;
			const char* YYMARKER;
			const char* tok;
			Tcl_Obj*	res = NULL;
			int			ofs;

			replace_tclobj(&res, Tcl_NewListObj(0, NULL));

			TEST_OK_LABEL(finally, code, Tcl_GetIntFromObj(interp, objv[A_OFS], &ofs));

		loop:
			tok = s;
			/*!re2c
				re2c:yyfill:enable		= 0;
				re2c:define:YYCTYPE		= "char";
				re2c:define:YYCURSOR	= "s";

				end			= [\x00];
				whitespace	= [\t\n\v\f\r \n];
				non_ws		= [^] \ end \ whitespace;

				whitespace+	{
					TEST_OK_LABEL(finally, code, Tcl_ListObjAppendElement(interp, res, Tcl_NewIntObj(s-str+ofs)));
					TEST_OK_LABEL(finally, code, Tcl_ListObjAppendElement(interp, res, Tcl_NewStringObj(tok, s-tok)));
					goto loop;
				}
				non_ws+		{goto loop;}
				end			{goto finally;}
				*			{THROW_ERROR_LABEL(finally, code, "Failed to parse");}
			*/

		finally:
			if (code == TCL_OK) Tcl_SetObjResult(interp, res);
			replace_tclobj(&res, NULL);
			return code;
		}
		//@end=c@
	}
} whitespace_sections
# whitespace_sections >>>
# parens <<<
interp alias {} parens {} ::jitc::capply {
	filter	{jitc::re2c -W --case-ranges}
	options	{-Wall -Werror -gdwarf-5}
	code {
		//@begin=c@
		Tcl_Obj*	g_open = NULL;
		Tcl_Obj*	g_empty = NULL;

		INIT {
			replace_tclobj(&g_open,  Tcl_NewStringObj("(", 1));
			replace_tclobj(&g_empty, Tcl_NewObj());
			return TCL_OK;
		}

		//@end=c@@begin=c@
		RELEASE {
			replace_tclobj(&g_open,  NULL);
			replace_tclobj(&g_empty, NULL);
		}

		//@end=c@@begin=c@
		OBJCMD(parens) {
			int			code = TCL_OK;
			enum {A_cmd, A_STR, A_OFS, A_objc};
			CHECK_ARGS("str ofs");
			const char*	str = Tcl_GetString(objv[A_STR]);
			const char* s = str;
			const char* YYMARKER;
			const char* tok;
			Tcl_Obj*	res = NULL;
			int			ofs;

			replace_tclobj(&res, Tcl_NewListObj(0, NULL));

			TEST_OK_LABEL(finally, code, Tcl_GetIntFromObj(interp, objv[A_OFS], &ofs));

		loop:
			tok = s;
			/*!re2c
				re2c:yyfill:enable		= 0;
				re2c:define:YYCTYPE		= "char";
				re2c:define:YYCURSOR	= "s";

				end			= [\x00];
				open		= "(";
				close		= ")";
				whitespace	= [\t\n\v\f\r \n];
				skip		= [^] \ end \ open \ close \ whitespace;

				open whitespace* {
					TEST_OK_LABEL(finally, code, Tcl_ListObjAppendElement(interp, res, Tcl_NewIntObj(s-str+ofs)));
					TEST_OK_LABEL(finally, code, Tcl_ListObjAppendElement(interp, res, g_open));
					goto loop;
				}
				whitespace* close {
					TEST_OK_LABEL(finally, code, Tcl_ListObjAppendElement(interp, res, Tcl_NewIntObj(tok-str+ofs)));
					const int	wslen = s-tok-1;
					if (wslen > 0) {
						TEST_OK_LABEL(finally, code, Tcl_ListObjAppendElement(interp, res, Tcl_NewStringObj(tok, wslen)));
					} else {
						TEST_OK_LABEL(finally, code, Tcl_ListObjAppendElement(interp, res, g_empty));
					}
					goto loop;
				}
				whitespace+	{goto loop;}
				skip+		{goto loop;}
				end			{goto finally;}
				*			{THROW_ERROR_LABEL(finally, code, "Failed to parse");}
			*/

		finally:
			if (code == TCL_OK) Tcl_SetObjResult(interp, res);
			replace_tclobj(&res, NULL);
			return code;
		}
		//@end=c@
	}
} parens
# parens >>>
# varinfo <<<
interp alias {} varinfo {} ::jitc::capply {
	filter	{jitc::re2c -W --case-ranges --tags}
	options	{-Wall -Werror -gdwarf-5}
	code {
		//@begin=c@
		Tcl_Obj*	g_scalar = NULL;
		Tcl_Obj*	g_array  = NULL;

		INIT {
			replace_tclobj(&g_scalar, Tcl_NewStringObj("scalar", 6));
			replace_tclobj(&g_array,  Tcl_NewStringObj("array",  5));
			return TCL_OK;
		}

		//@end=c@@begin=c@
		RELEASE {
			replace_tclobj(&g_scalar, NULL);
			replace_tclobj(&g_array,  NULL);
		}

		//@end=c@@begin=c@
		OBJCMD(varinfo) {
			int			code = TCL_OK;
			enum {A_cmd, A_STR, A_objc};
			CHECK_ARGS("str");
			const char*	str = Tcl_GetString(objv[A_STR]);
			const char* s = str;
			const char* YYMARKER;
			const char	*a1, *a2, *i1, *i2;
			Tcl_Obj*	res = NULL;
			int			ofs;
			/*!types:re2c*/
			/*!stags:re2c format = "const char* @@;\n"; */

			replace_tclobj(&res, Tcl_NewListObj(0, NULL));

		loop:
			/*!re2c
				re2c:yyfill:enable		= 0;
				re2c:define:YYCTYPE		= "char";
				re2c:define:YYCURSOR	= "s";

				end			= [\x00];
				open_paren	= "(";
				close_paren	= ")";
				close_brace	= [\x7d];
				arrayname	= ([^] \ end \ open_paren \ close_brace)*;
				index		= ([^] \ end \ close_brace)*;

				@a1 arrayname @a2 open_paren @i1 index @i2 close_paren end {
					TEST_OK_LABEL(finally, code, Tcl_ListObjAppendElement(interp, res, g_array));
					TEST_OK_LABEL(finally, code, Tcl_ListObjAppendElement(interp, res, Tcl_NewStringObj(a1, a2-a1)));
					TEST_OK_LABEL(finally, code, Tcl_ListObjAppendElement(interp, res, Tcl_NewStringObj(i1, i2-i1)));
					goto finally;
				}

				* {
					TEST_OK_LABEL(finally, code, Tcl_ListObjAppendElement(interp, res, g_scalar));
					TEST_OK_LABEL(finally, code, Tcl_ListObjAppendElement(interp, res, objv[1]));
					goto finally;
				}
			*/

		finally:
			if (code == TCL_OK) Tcl_SetObjResult(interp, res);
			replace_tclobj(&res, NULL);
			return code;
		}
		//@end=c@
	}
} varinfo
# varinfo >>>

proc highlight_expr_ws idx { #<<<
	upvar 1 ws ws
	if {[dict exists $ws $idx]} {<tcl-space idx $idx {txt [dict get $ws $idx]}}
}

#>>>
proc highlight_operator n { #<<<
	upvar 1 ws ws orig orig ofs ofs parens parens

	set op		[xpath $n string(@name)]
	set opidx	[xpath $n string(@idx)]
	set arity	[xpath $n count(subexpr)]

	if {[jitc::capply {
		filter	{jitc::re2c -W --case-ranges}
		code {//@begin=c@
			Tcl_Obj*	g_true	= NULL;
			Tcl_Obj*	g_false	= NULL;

			INIT {
				replace_tclobj(&g_true,  Tcl_NewBooleanObj(1));
				replace_tclobj(&g_false, Tcl_NewBooleanObj(0));
				return TCL_OK;
			}

			RELEASE {
				replace_tclobj(&g_true,  NULL);
				replace_tclobj(&g_false, NULL);
			}

			OBJCMD(is_op) {
				enum {A_cmd, A_STR, A_objc};
				CHECK_ARGS("str");
				const char*	str = Tcl_GetString(objv[A_STR]);
				const char*	s = str;

				/*!re2c
					re2c:yyfill:enable		= 0;
					re2c:define:YYCURSOR	= "s";
					re2c:define:YYCTYPE		= "char";

					op	= [-+~!*%/<>&^|?]
						| "**" | "<<" | ">>" | "<=" | ">=" | "==" | "!="
						| "eq" | "ne" | "in" | "ni" | "&&" | "||";

					op	{Tcl_SetObjResult(interp, g_true);  return TCL_OK;}
					*	{Tcl_SetObjResult(interp, g_false); return TCL_OK;}
				*/
			}
			//@end=c@
		}
	} is_op $op]} {
		if {$arity > 1} {
			highlight_subexpr [xpath $n {subexpr[1]}]
		}
		highlight_expr_ws $opidx
		<tcl-op {txt $op}
		if {$arity >= 2} {
			highlight_subexpr [xpath $n {subexpr[2]}]
		}
		if {$arity >= 3} {
			# TODO: How to get the prefix whitespace for the ':' ?
			<tcl-op {txt :}
			highlight_subexpr [xpath $n {subexpr[3]}]
		}
		if {$arity > 3} {
			error "expr op with arity > 3: $arity"
		}
	} else {
		#puts stderr [domNode $n asXML]
		highlight_expr_ws $opidx
		<tcl-op {txt $op}
		if {$arity > 0} {
			highlight_subexpr [xpath $n {subexpr[1]}]
		}
		for {set i 2} {$i <= $arity} {incr i} {
			<tcl-space {txt ","}
			highlight_subexpr [xpath $n "subexpr\[$i\]"]
		}
	}
}

#>>>
proc highlight_subexpr subexpr { #<<<
	upvar 1 ws ws orig orig ofs ofs parens parens
	set idx		[domNode $subexpr getAttribute idx]
	set len		[domNode $subexpr getAttribute len]
	set to		[expr {$idx + $len}]
	set extra	{}
	if {
		[dict exists $parens $idx] && [dict get $parens $idx] eq "(" &&
		[dict exists $parens $to]  && [dict get $parens $to]  ne "("
	} {
		lappend extra parens {}
		set tail_ws	[dict get $parens $to]
	}
	<tcl-subexpr {*}$extra {
		if {
			[dict exists $ws $idx] &&
			![xpath $subexpr {boolean(operator/subexpr[@idx=$idx])}]
		} {
			<tcl-space {txt [dict get $ws $idx]}
		}
		foreach n [xpath $subexpr *] {
			switch -exact -- [domNode $n nodeName] {
				operator	{ highlight_operator $n }
				var			{ highlight_var $n }
				text		{ highlight_word $subexpr }
				word		{ highlight_word $n }
				script		{ highlight_script $n }
				syntax		{ highlight_syntax $n }
				default		{ error "Unhandled subexpr child: \"[domNode $n nodeName]\":\n[domNode $n asXML]" }
			}
		}
		if {[info exists tail_ws] && $tail_ws ne ""} {
			<tcl-space {txt $tail_ws}
		}
	}
}

#>>>
proc highlight_expr exprnode { #<<<
	set top_subexpr	[xpath $exprnode {subexpr[1]}]
	set orig	[xpath $top_subexpr {string(@orig)}]
	set ofs		[xpath $top_subexpr {string(@idx)}]
	set ws		[whitespace_sections $orig $ofs]
	set parens	[parens $orig $ofs]
	<tcl-expr ws $ws {
		foreach n [xpath $exprnode *] {
			switch -exact -- [domNode $n nodeName] {
				space	{ <tcl-space {txt [domNode $n asText]} }
				subexpr	{ highlight_subexpr $top_subexpr }
				default { error "Unhandled child of expr node: \"[domNode $n nodeName]\"" }
			}
		}
	}
}

#>>>
proc highlight_list listnode { #<<<
	<tcl-list {
		foreach n [xpath $listnode *] {
			switch -exact -- [domNode $n nodeName] {
				word {
					highlight_word $n
				}
				space {
					<tcl-space {
						txt [domNode $n asText]
					}
				}
				default {
					error "Unhandled list child: \"[domNode $n nodeName]\""
				}
			}
		}
	}
}

#>>>
proc highlight_var varnode { #<<<
	set varname	[xpath $varnode {string(text[1])}]
	<tcl-var {*}[if {![valid_varname $varname]} {list braced ""}] {
		switch -exact -- [xpath $varnode string(@type)] {
			scalar {
				lassign [varinfo $varname] vartype n1 n2
				switch -exact -- $vartype {
					scalar {
						<tcl-scalar {txt $varname}
					}
					array  {
						<tcl-array {
							<tcl-array-name		{txt $n1}
							<tcl-array-index	{<tcl-txt {txt $n2}}
						}
					}
					default {
						error "Unexpected vartype: \"$vartype\""
					}
				}
			}

			array {
				<tcl-array {
					<tcl-array-name		{txt $varname}
					<tcl-array-index	{highlight_tokens $varnode 1}
				}
			}
			default {
				error "Unhandled var type: \"[xpath $varnode string(@type)]\""
			}
		}
	}
}

#>>>
proc highlight_tokens {parent {skip 0}} { #<<<
	foreach n [xpath $parent *] {
		if {$skip > 0} {
			incr skip -1
			continue
		}
		switch -exact -- [domNode $n nodeName] {
			text {
				<tcl-txt {txt [domNode $n asText]}
			}
			var {
				highlight_var $n
			}
			escape {
				<tcl-escape {txt [domNode $n asText]}
			}
			script {
				highlight_script $n
			}
			as {
				#puts stderr "Unhandled as: [domNode $n asXML]"
			}
			syntax {
				puts stderr "Syntax token: [domNode $n asXML]"
			}
			default {
				error "Unhandled word token: \"[domNode $n nodeName]\""
			}
		}
	}
}

#>>>
proc highlight_word wordnode { #<<<
	set quoted	[xpath $wordnode {string(@quoted)}]
	<tcl-word {*}[if {$quoted ne ""} {list quoted $quoted}] {*}[if {[domNode $wordnode hasAttribute expand]} {list expand ""}] {
		<tcl-tokens {highlight_tokens $wordnode}
		if {[xpath $wordnode {boolean(as/script)}]} {
			<tcl-as type "script" {highlight_script [xpath $wordnode as/script]}
		} elseif {[xpath $wordnode {boolean(as/expr)}]} {
			<tcl-as type "expr" {highlight_expr [xpath $wordnode as/expr]}
		} elseif {[xpath $wordnode {boolean(as/list)}]} {
			<tcl-as type "list" {highlight_list [xpath $wordnode as/list]}
		}
	}
}

#>>>
proc highlight_syntax syntaxnode { #<<<
	<tcl-syntax {
		txt [domNode $syntaxnode asText]
	}
}

#>>>
proc highlight_command commandnode { #<<<
	<tcl-command {
		foreach n [xpath $commandnode *] {
			switch -exact -- [domNode $n nodeName] {
				word {
					highlight_word $n
				}
				space {
					<tcl-space {
						txt [domNode $n asText]
					}
				}
				end {
					<tcl-end {
						txt [domNode $n asText]
					}
				}
				default {
					error "Unhandled command node: [domNode $n nodeName]"
				}
			}
		}
	}
}

#>>>
proc highlight_script {scriptnode args} { #<<<
	<tcl-script {*}$args {
		foreach n [xpath $scriptnode *] {
			switch -exact -- [domNode $n nodeName] {
				command {
					highlight_command $n
				}
				space {
					<tcl-space {
						txt [domNode $n asText]
					}
				}
				comment {
					<tcl-comment {
						txt [domNode $n asText]
					}
				}
				end {
					<tcl-end {txt [domNode $n asText]}
				}
				default {
					error "Unhandled script child: \"[domNode $n nodeName]\""
				}
			}
		}
	}
}

#>>>

return

set fn	[lindex $argv 0]
if {$fn eq ""} {
	set script	[read stdin]
} else {
	set script	[chantricks readfile $fn]
}

#puts stderr [domNode [parsetcl::parsetree $script] asXML]

puts "<!DOCTYPE html>\n[html {
	<html {
		<head {
			<style {txt {
				TCL-SCRIPT, TCL-SPACE, TCL-COMMENT, TCL-COMMAND, TCL-WORD, TCL-END, TCL-VAR, TCL-SCALAR, TCL-ARRAY, TCL-ARRAY-NAME, TCL-ARRAY-INDEX, TCL-LIST, TCL-QUOTED, TCL-BRACED, TCL-EXPR, TCL-OP {
					display: inline;
				}

				TCL-SCRIPT {
					white-space: pre;
					font-family: monospace;
				}
				TCL-VAR::before			{ content: '$'; }
				TCL-VAR[braced]::before	{ content: '$\7B'; }
				TCL-VAR[braced]::after	{ content: '\7D'; }

				TCL-WORD[quoted="brace"]::before	{ content: '\7B'; }
				TCL-WORD[quoted="brace"]::after		{ content: '\7D'; }
				TCL-WORD[quoted="quote"]::before	{ content: '"'; }
				TCL-WORD[quoted="quote"]::after		{ content: '"'; }
				TCL-WORD[expand]::before			{ content: '{*}'; }

				TCL-ARRAY-INDEX::before	{ content: '('; }
				TCL-ARRAY-INDEX::after	{ content: ')'; }

				TCL-COMMAND>TCL-WORD:first-of-type {
					color: #d9c009;
				}

				TCL-AS		{ display: none; }

				TCL-SUBEXPR[parens]::before	{ content: '('; }
				TCL-SUBEXPR[parens]::after	{ content: ')'; }
				TCL-SUBEXPR>TCL-SCRIPT::before,
				TCL-TOKENS>TCL-SCRIPT::before	{ content: '['; }
			}}

			<style {txt {
				TCL-SCRIPT[main] {
					--default-fg: #d8d8d8;
					--default-bg: #202020;
					display: block;
					background: var(--default-bg);
					color: var(--default-fg);
					padding: .25em;
					tab-size: 4;
				}

				TCL-COMMENT			{color: #7191c1;}
				TCL-ESCAPE			{color: #ff00ff; background: rgba(255 0 255 / .2);}
				TCL-SCALAR			{color: #10c1df;}
				TCL-VAR::before,
				TCL-VAR::after		{color: #006678;}
				TCL-ARRAY-NAME		{color: #10c1df;}
				TCL-ARRAY-INDEX		{color: #a41fce;}
				TCL-ARRAY-INDEX::before,
				TCL-ARRAY-INDEX::after	{color: #cc64ed;}
				TCL-WORD[quoted="quote"]			{color: #dd6c5f;}
				TCL-WORD[quoted="quote"]::before,
				TCL-WORD[quoted="quote"]::after		{color: #b73222;}
				TCL-WORD[quoted="brace"]::before,
				TCL-WORD[quoted="brace"]::after		{color: #878787;}
				TCL-WORD[expand]::before			{color: #00b100;}
				TCL-SPACE							{color: #4a4a4a;}
				TCL-SCRIPT							{color: var(--default-fg);}

				TCL-WORD	{border-radius: 7px;}
				TCL-COMMAND:hover:not(:has(TCL-COMMAND:hover))>TCL-WORD {
					background: rgba(255 255 255 / .1);
				}

				TCL-WORD:hover:not(:has(TCL-WORD:hover, TCL-AS:hover)) {
					/*filter: drop-shadow(0 0 .2em rgba(255 255 128 / .9));*/
					/*background: #525252 !important;*/
					outline: 1px solid #a9a92d;
					outline-offset: .2em;
				}

				TCL-WORD[expand]:hover:not(:has(TCL-WORD:hover, TCL-AS:hover)) {
					/*filter: drop-shadow(0 0 .2em rgba(255 255 128 / .9));*/
					/*background: #525252 !important;*/
					outline-style: dashed;
				}

				TCL-WORD:hover:has(TCL-AS) > TCL-TOKENS	{ display: none; }
				TCL-WORD:hover > TCL-AS					{ display: inline; }

				TCL-AS:hover:not(:has(TCL-WORD>TCL-AS:hover)) {
					outline: 1px solid #196619;
					/* outline-offset: .25em; */
					/*background: rgba(0 255 0 / .10);*/
					background: black;
				}

				TCL-SCRIPT[main]:has(TCL-AS:hover) TCL-SCALAR,
				TCL-SCRIPT[main]:has(TCL-AS:hover) TCL-ARRAY,
				TCL-SCRIPT[main]:has(TCL-AS:hover) TCL-ESCAPE,
				TCL-SCRIPT[main]:has(TCL-AS:hover) TCL-SPACE,
				TCL-SCRIPT[main]:has(TCL-AS:hover) TCL-COMMENT,
				TCL-SCRIPT[main]:has(TCL-AS:hover) TCL-END,
				TCL-SCRIPT[main]:has(TCL-AS:hover) TCL-TXT {
					filter: saturate(18%);
				}
				TCL-AS:hover * {
					filter: saturate(100%) !important;
				}
				TCL-AS:hover:not(:has(TCL-WORD>TCL-AS:hover)) * {
					filter: saturate(100%) !important;
				}
			}}
		}
		<body {
			set pt	[parsetcl::parsetree $script]
			highlight_script [xpath $pt /tcl/script] main ""
		}
	}
}]"

# vim: foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4
