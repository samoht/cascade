(** Tests for the CSS Syntax Level 3 section 5 parser algorithms. *)

open Cascade

(* Shorthand constructors mirroring Parser.component_value so test data is
   readable. *)

let pv t = Component.Preserved t

let block op vs : Component.t =
  let body : Component.block = { opening = op; value = vs; closed = true } in
  Component.Block { node = body; loc = Loc.dummy }

let func name args : Component.t =
  let body : Component.func = { name; arguments = args; terminated = true } in
  Component.Func { node = body; loc = Loc.dummy }

(* Pretty-print a component value for assertion diffing. *)

let rec pp_cv : Component.t Css.Pp.t =
 fun ctx cv ->
  match cv with
  | Component.Preserved t -> Token.pp ctx t
  | Component.Block { node = { opening; value; _ }; _ } ->
      let open_c, close_c =
        match opening with
        | Token.Curly -> ('{', '}')
        | Token.Paren -> ('(', ')')
        | Token.Square -> ('[', ']')
      in
      Css.Pp.char ctx open_c;
      Css.Pp.list ~sep:Css.Pp.sp pp_cv ctx value;
      Css.Pp.char ctx close_c
  | Component.Func { node = { name; arguments; _ }; _ } ->
      Css.Pp.string ctx name;
      Css.Pp.char ctx '(';
      Css.Pp.list ~sep:Css.Pp.sp pp_cv ctx arguments;
      Css.Pp.char ctx ')'

let pp_cvs ctx cvs = Css.Pp.list ~sep:Css.Pp.sp pp_cv ctx cvs

let pp_rule : Component.rule Css.Pp.t =
 fun ctx -> function
  | Component.Qualified
      { node = { prelude; block = { node = { opening = _; value; _ }; _ } }; _ }
    ->
      Css.Pp.string ctx "qualified{prelude=";
      pp_cvs ctx prelude;
      Css.Pp.string ctx "; block=";
      pp_cvs ctx value;
      Css.Pp.char ctx '}'
  | Component.At { node = { name; prelude; block }; _ } ->
      Css.Pp.string ctx "at[";
      Css.Pp.string ctx name;
      Css.Pp.string ctx "]{prelude=";
      pp_cvs ctx prelude;
      Css.Pp.string ctx "; block=";
      (match block with
      | None -> Css.Pp.string ctx "<none>"
      | Some { node = { value; _ }; _ } -> pp_cvs ctx value);
      Css.Pp.char ctx '}'

let pp_rules ctx rules = Css.Pp.list ~sep:Css.Pp.sp pp_rule ctx rules

let parse_ss css =
  let r = Reader.of_string css in
  (Parser.stylesheet r).value

let parse_rules css =
  let r = Reader.of_string css in
  (Parser.list_of_rules r).value

let parse_cvs css =
  let p = Parser.of_string css in
  let rec loop acc =
    match Parser.next p with
    | Component.Preserved { kind = Token.Eof; _ } -> List.rev acc
    | cv -> loop (cv :: acc)
  in
  loop []

(* ----- Basic shape tests ----- *)

let simple_rule () =
  let rs = parse_ss ".a { color: red }" in
  Alcotest.(check int) "one rule" 1 (List.length rs);
  match rs with
  | [
   Component.Qualified
     { node = { prelude = _; block = { node = { value; _ }; _ } }; _ };
  ] ->
      (* block contains: ws, ident color, colon, ws, ident red, ws *)
      Alcotest.(check bool) "block non-empty" true (value <> [])
  | _ -> Alcotest.fail "expected one qualified rule"

let multiple_rules () =
  let rs = parse_ss ".a { } .b { } .c { }" in
  Alcotest.(check int) "three rules" 3 (List.length rs)

let at_rule_with_block () =
  let rs = parse_ss "@media screen { .btn { color: red } }" in
  match rs with
  | [ Component.At { node = { name; block = Some _; _ }; _ } ] ->
      Alcotest.(check string) "name" "media" name
  | _ -> Alcotest.fail "expected one @media at-rule with block"

let at_rule_semi_terminated () =
  let rs = parse_ss "@charset \"utf-8\";" in
  match rs with
  | [ Component.At { node = { name; block = None; _ }; _ } ] ->
      Alcotest.(check string) "name" "charset" name
  | _ -> Alcotest.fail "expected @charset without block"

let bad_selector_drops_rule () =
  (* The prelude <!--.. parses as component values; the rule still forms because
     it has a block. This test just checks we don't crash. *)
  let rs = parse_ss "..bad { } .good { color: red }" in
  Alcotest.(check int) "two rules survive structurally" 2 (List.length rs)

let unterminated_qualified_rule_dropped () =
  (* EOF before the opening brace drops the rule per section 5.5.3. *)
  let rs = parse_ss "h1" in
  Alcotest.(check int) "zero rules" 0 (List.length rs)

let spec_syntax_description_examples () =
  (* CSS Syntax Level 3 section 2 uses these as representative qualified-rule
     and at-rule shapes. They exercise the section 5.4 stylesheet entry point
     and the section 5.5.1/5.5.2/5.5.3 rule consumers. *)
  let rs =
    parse_ss
      "p > a { color: blue; text-decoration: underline } @import \
       \"my-styles.css\"; @page :left { margin-left: 4cm; margin-right: 3cm } \
       @media print { body { font-size: 10pt } }"
  in
  Alcotest.(check int)
    "one qualified rule and three at-rules" 4 (List.length rs);
  match rs with
  | Component.Qualified _
    :: Component.At { node = { name = "import"; _ }; _ }
    :: Component.At { node = { name = "page"; block = Some _; _ }; _ }
    :: [ Component.At { node = { name = "media"; block = Some _; _ }; _ } ] ->
      ()
  | _ -> Alcotest.fail "unexpected section 2 example rule shapes"

let spec_error_handling_eof_closes () =
  (* CSS Syntax Level 3 section 2.2: EOF auto-closes open rules, declarations,
     functions, and strings at the syntax layer. Grammar validation may reject
     them later, but the component parser still builds a rule. *)
  let rs = parse_ss ".foo { transform: translate(50px" in
  Alcotest.(check int) "one auto-closed qualified rule" 1 (List.length rs);
  match rs with
  | [
   Component.Qualified { node = { block = { node = { value; _ }; _ }; _ }; _ };
  ] ->
      let serialized = Parser.string_of_components value in
      Alcotest.(check string)
        "auto-closed declaration value" " transform: translate(50px)" serialized
  | _ -> Alcotest.fail "expected one auto-closed qualified rule"

let spec_parse_grammar_entry_points () =
  (* CSS Syntax Level 3 sections 5.4.1 and 5.4.2: parse component values first,
     then match either the whole value or each comma-separated group against the
     supplied grammar. *)
  let single_ident = function
    | [ Component.Preserved { kind = Token.Ident _; _ } ] -> true
    | _ -> false
  in
  let parse_one css =
    (Parser.matches_grammar (Reader.of_string css) single_ident).value
  in
  Alcotest.(check bool)
    "single ident matches grammar" true
    (match parse_one "foo" with
    | Some cvs -> Parser.string_of_components cvs = "foo"
    | _ -> false);
  Alcotest.(check bool)
    "multiple components do not match grammar" true
    (parse_one "foo bar" = None);
  let parse_groups css =
    (Parser.csv_by_grammar (Reader.of_string css) single_ident).value
  in
  let show_group = function
    | None -> "<no-match>"
    | Some cvs -> Parser.to_string_minified cvs
  in
  Alcotest.(check string)
    "comma groups matched independently" "foo|<no-match>|bar"
    (parse_groups "foo, 1px, bar" |> List.map show_group |> String.concat "|");
  Alcotest.(check int)
    "whitespace-only comma grammar input is empty" 0
    (List.length (parse_groups "   "))

let spec_grammar_empty_commas () =
  (* CSS Syntax Level 3 sections 5.4.1 and 5.4.2: the caller's grammar decides
     whether an empty component-value list is valid, and comma splitting keeps
     empty interior groups while not synthesizing a group after a trailing
     comma. *)
  let empty_only = function [] -> true | _ -> false in
  let parse_empty css =
    (Parser.matches_grammar (Reader.of_string css) empty_only).value
  in
  Alcotest.(check bool)
    "empty grammar accepts whitespace-only input" true
    (parse_empty " \n\t " = Some []);
  let single_ident = function
    | [ Component.Preserved { kind = Token.Ident _; _ } ] -> true
    | _ -> false
  in
  let parse_groups css =
    (Parser.csv_by_grammar (Reader.of_string css) single_ident).value
  in
  let show_group = function
    | None -> "<no-match>"
    | Some cvs -> Parser.to_string_minified cvs
  in
  Alcotest.(check string)
    "empty interior comma group is matched independently" "a|<no-match>|b"
    (parse_groups " a, , b " |> List.map show_group |> String.concat "|");
  let parse_empty_groups css =
    (Parser.csv_by_grammar (Reader.of_string css) empty_only).value
  in
  Alcotest.(check bool)
    "single comma produces one empty group" true
    (parse_empty_groups "," = [ Some [] ]);
  Alcotest.(check bool)
    "whitespace-only input is still no groups" true
    (parse_empty_groups "   " = [])

let spec_parse_stylesheet_entry_point () =
  (* CSS Syntax Level 3 section 5.4.3: parse a stylesheet by normalizing input
     and consuming stylesheet contents. *)
  Alcotest.(check int) "empty stylesheet" 0 (List.length (parse_ss " \t\n "));
  let rs = parse_ss "<!-- @layer base; .a{} -->" in
  Alcotest.(check int) "CDO/CDC skipped around rules" 2 (List.length rs);
  match rs with
  | [ Component.At { node = { name = "layer"; _ }; _ }; Component.Qualified _ ]
    ->
      ()
  | _ -> Alcotest.fail "expected @layer and .a rules"

let spec_parse_rule_entry_point () =
  (* CSS Syntax Level 3 section 5.4.6: a rule entry point trims surrounding
     whitespace, requires one rule, and rejects empty input or trailing
     rules. *)
  let parse_one_rule css = (Parser.rule (Reader.of_string css)).value in
  Alcotest.(check bool)
    "empty input is syntax error" true
    (parse_one_rule "  " = None);
  Alcotest.(check bool)
    "one qualified rule accepted" true
    (match parse_one_rule " .a { color: red } " with
    | Some (Component.Qualified _) -> true
    | _ -> false);
  Alcotest.(check bool)
    "one at-rule accepted" true
    (match parse_one_rule " @layer base; " with
    | Some (Component.At { node = { name = "layer"; block = None; _ }; _ }) ->
        true
    | _ -> false);
  Alcotest.(check bool)
    "one block at-rule accepted" true
    (match parse_one_rule " @media screen { .a { color: red } } " with
    | Some (Component.At { node = { name = "media"; block = Some _; _ }; _ }) ->
        true
    | _ -> false);
  Alcotest.(check bool)
    "trailing rule is syntax error" true
    (parse_one_rule ".a{} .b{}" = None);
  Alcotest.(check bool)
    "trailing semicolon after at-rule is syntax error" true
    (parse_one_rule "@layer base; ;" = None);
  Alcotest.(check bool)
    "trailing component after block rule is syntax error" true
    (parse_one_rule ".a{} ident" = None);
  Alcotest.(check bool)
    "unterminated qualified rule is syntax error" true
    (parse_one_rule "h1" = None)

let spec_block_mixed_items () =
  (* CSS Syntax Level 3 section 5.4.5 / 5.5.5: block contents return runs of
     declarations interleaved with nested rules, preserving order. *)
  let out =
    Parser.block_contents
      (Reader.of_string "color: red; @media screen {}; & .x {}; width: 1px")
  in
  match out.value with
  | [
   `Decls [ { node = { name = "color"; _ }; _ } ];
   `Rule (Component.At { node = { name = "media"; _ }; _ });
   `Rule (Component.Qualified _);
   `Decls [ { node = { name = "width"; _ }; _ } ];
  ] ->
      ()
  | _ ->
      Alcotest.fail
        "expected declaration run, nested at-rule, nested qualified rule, \
         declaration run"

let spec_block_discard_branches () =
  (* CSS Syntax Level 3 section 5.5.5: whitespace and semicolons are discarded;
     EOF and right brace terminate the block contents. *)
  let parse css = (Parser.block_contents (Reader.of_string css)).value in
  Alcotest.(check int)
    "whitespace and semicolons discarded" 0
    (List.length (parse "  ; \n ;"));
  Alcotest.(check int)
    "right brace terminates contents" 1
    (List.length (parse "color: red; } width: 1px"))

let spec_block_nested_errors () =
  (* CSS Syntax Level 3 section 5.5.5 delegates failed declaration attempts to
     nested qualified-rule parsing with semicolon as a stop token. *)
  let parse css = (Parser.block_contents (Reader.of_string css)).value in
  Alcotest.(check int)
    "invalid rule does not poison following declaration" 1
    (List.length (parse ".bad ; color: red"));
  Alcotest.(check int)
    "nested custom-property-shaped rule is consumed as bad declaration" 0
    (List.length (parse "--x: y { z } tail;"))

let spec_block_nested_boundaries () =
  (* CSS Syntax Level 3 section 5.5.2 and 5.5.3 nested consumers: a right brace
     terminates nested at-rules/qualified rules, and a semicolon terminates a
     nested qualified-rule attempt without consuming following content. *)
  let parse css = (Parser.block_contents (Reader.of_string css)).value in
  (match parse "@media screen } width: 1px" with
  | [ `Rule (Component.At { node = { name = "media"; prelude; _ }; _ }) ] ->
      Alcotest.(check string)
        "right brace stops nested at-rule" "screen"
        (Parser.to_string_minified prelude)
  | _ -> Alcotest.fail "expected one nested at-rule before right brace");
  match parse "& .x ; color: red" with
  | [ `Decls [ { node = { name = "color"; _ }; _ } ] ] -> ()
  | _ -> Alcotest.fail "expected nested qualified rule to stop at semicolon"

let spec_block_reparse_examples () =
  (* CSS Syntax Level 3 section 5.5.5 first tries a declaration, then restores
     the input and tries a nested qualified rule when the declaration does not
     parse. The implementation note calls out these declaration/rule boundary
     shapes. *)
  let parse css = (Parser.block_contents (Reader.of_string css)).value in
  (match parse "foo: bar; baz {} qux: 1px" with
  | [
   `Decls [ { node = { name = "foo"; _ }; _ } ];
   `Rule (Component.Qualified { node = { prelude = baz_prelude; _ }; _ });
   `Decls [ { node = { name = "qux"; _ }; _ } ];
  ] ->
      Alcotest.(check string)
        "rule after declaration" "baz"
        (Parser.to_string_minified baz_prelude)
  | _ -> Alcotest.fail "expected declaration, rule, declaration");
  (match parse "foo:hover { color: red } width: 1px" with
  | [
   `Rule (Component.Qualified { node = { prelude = hover_prelude; _ }; _ });
   `Decls [ { node = { name = "width"; _ }; _ } ];
  ] ->
      Alcotest.(check string)
        "pseudo-class rule prelude" "foo:hover"
        (Parser.to_string_minified hover_prelude)
  | _ -> Alcotest.fail "expected pseudo-class-shaped qualified rule");
  (match parse "font+ { color: red } width: 1px" with
  | [
   `Rule (Component.Qualified { node = { prelude = plus_prelude; _ }; _ });
   `Decls [ { node = { name = "width"; _ }; _ } ];
  ] ->
      Alcotest.(check string)
        "font plus rule prelude" "font+"
        (Parser.to_string_minified plus_prelude)
  | _ -> Alcotest.fail "expected non-declaration ident rule");
  match parse "font:bar { color: red } width: 1px" with
  | [
   `Rule (Component.Qualified { node = { prelude = font_prelude; _ }; _ });
   `Decls [ { node = { name = "width"; _ }; _ } ];
  ] ->
      Alcotest.(check string)
        "font colon rule prelude" "font:bar"
        (Parser.to_string_minified font_prelude)
  | _ -> Alcotest.fail "expected mixed block declaration to reparse as rule"

let spec_block_flush_stop () =
  (* CSS Syntax Level 3 section 5.5.5 flushes pending declaration lists before
     nested rules, flushes before invalid nested-rule errors, and stops at a
     right brace without consuming later input. *)
  let parse css = (Parser.block_contents (Reader.of_string css)).value in
  (match parse "color: red; .bad ; width: 1px" with
  | [
   `Decls [ { node = { name = "color"; _ }; _ } ];
   `Decls [ { node = { name = "width"; _ }; _ } ];
  ] ->
      ()
  | _ -> Alcotest.fail "expected invalid nested rule to split declaration lists");
  (match parse "color: red; @media screen } width: 1px" with
  | [
   `Decls [ { node = { name = "color"; _ }; _ } ];
   `Rule (Component.At { node = { name = "media"; prelude; _ }; _ });
  ] ->
      Alcotest.(check string)
        "nested at-rule prelude" "screen"
        (Parser.to_string_minified prelude)
  | _ ->
      Alcotest.fail
        "expected declaration list and nested at-rule before right brace");
  match parse "color: red; } width: 1px" with
  | [ `Decls [ { node = { name = "color"; _ }; _ } ] ] -> ()
  | _ -> Alcotest.fail "expected right brace to stop block contents"

let spec_block_custom_props () =
  (* CSS Syntax Level 3 section 5.5.5 treats custom-property-shaped input as a
     declaration attempt, so it is not reparsed as a qualified rule. *)
  let parse css = (Parser.block_contents (Reader.of_string css)).value in
  match parse "--foo:hover { color: blue; }; width: 1px" with
  | [
   `Decls
     [
       { node = { name = "--foo"; value = custom_value; _ }; _ };
       { node = { name = "width"; _ }; _ };
     ];
  ] ->
      Alcotest.(check string)
        "custom property value" "hover{color:blue;}"
        (Parser.to_string_minified custom_value)
  | _ ->
      Alcotest.fail
        "expected custom-property-shaped block input to stay a declaration"

let spec_parse_declaration_entry_point () =
  (* CSS Syntax Level 3 section 5.4.7: parse one declaration. Unlike the rule
     and component-value entry points, this algorithm does not require EOF after
     the declaration. *)
  let parse_one_decl css = (Parser.declaration (Reader.of_string css)).value in
  Alcotest.(check bool)
    "basic declaration accepted" true
    (match parse_one_decl " color: red " with
    | Some { node = { name = "color"; _ }; _ } -> true
    | _ -> false);
  Alcotest.(check bool)
    "at-rule rejected" true
    (parse_one_decl "@media screen {}" = None);
  Alcotest.(check bool)
    "declaration list returns first declaration" true
    (match parse_one_decl "color: red; width: 1px" with
    | Some { node = { name = "color"; _ }; _ } -> true
    | _ -> false);
  Alcotest.(check bool)
    "bad first declaration attempt is rejected" true
    (parse_one_decl ": bad; color: red" = None);
  Alcotest.(check bool)
    "missing colon rejected" true
    (parse_one_decl "color red" = None)

let spec_component_entry_point () =
  (* CSS Syntax Level 3 section 5.4.8: surrounding whitespace is ignored, but
     the input must contain exactly one component value. *)
  let parse_one_cv css =
    (Parser.component_value (Reader.of_string css)).value
  in
  let check_some input expected =
    match parse_one_cv input with
    | Some cv ->
        Alcotest.(check string)
          (Fmt.str "component %S" input)
          expected
          (Parser.string_of_components [ cv ])
    | None -> Alcotest.failf "expected one component value for %S" input
  in
  check_some "  [a b]  " "[a b]";
  check_some "rgb(1, 2)" "rgb(1, 2)";
  check_some "," ",";
  check_some "/*x*/[a]" "[a]";
  Alcotest.(check bool) "empty input rejected" true (parse_one_cv "  " = None);
  Alcotest.(check bool)
    "extra component rejected" true
    (parse_one_cv "a b" = None);
  Alcotest.(check bool)
    "comments do not join two components for this entry" true
    (parse_one_cv "a/*x*/b" = None)

let spec_list_components_entry () =
  (* CSS Syntax Level 3 section 5.4.9: consume component values until EOF,
     preserving whitespace and grouping blocks/functions. *)
  let parse_list css =
    (Parser.list_of_component_values (Reader.of_string css)).value
  in
  Alcotest.(check string)
    "component value list" "a [b c] rgb(1, 2) }"
    (Parser.string_of_components (parse_list "a [b c] rgb(1, 2) }"));
  Alcotest.(check string)
    "unmatched closing tokens are preserved" ")]}"
    (Parser.string_of_components (parse_list ")]}"));
  Alcotest.(check string)
    "comments preserve token boundary" "a b"
    (Parser.string_of_components (parse_list "a/*x*/b"));
  Alcotest.(check string)
    "EOF closes nested blocks and functions" "[a f(b)]"
    (Parser.string_of_components (parse_list "[a f(b"));
  Alcotest.(check int) "empty list" 0 (List.length (parse_list ""))

let spec_csv_components () =
  (* CSS Syntax Level 3 section 5.4.10: split only on top-level commas; a
     trailing comma is consumed and does not synthesize an empty final group. *)
  let parse_groups css =
    (Parser.csv_component_values (Reader.of_string css)).value
  in
  let show groups =
    List.map Parser.to_string_minified groups |> String.concat "|"
  in
  Alcotest.(check string)
    "top-level commas split groups" "a|rgb(1,2)|[x,y]"
    (show (parse_groups "a, rgb(1, 2), [x,y],"));
  Alcotest.(check string)
    "nested commas stay in their component values" "a|(b,c)|f(x,y)|d"
    (show (parse_groups "a,(b,c),f(x,y),d"));
  Alcotest.(check string)
    "leading comma yields empty first group" "|a"
    (show (parse_groups ",a"));
  Alcotest.(check string)
    "trailing comma has no final empty group" "a"
    (show (parse_groups "a,"));
  Alcotest.(check string)
    "empty middle group is preserved" "a||b"
    (show (parse_groups "a,,b,"));
  Alcotest.(check string)
    "single comma produces one empty group" ""
    (show (parse_groups ","));
  Alcotest.(check string)
    "empty input yields empty group list" ""
    (show (parse_groups ""))

let spec_arbitrary_value_productions () =
  (* CSS Syntax Level 3 section 7.2: <declaration-value> and <any-value> both
     require at least one token and reject bad strings, bad URLs, and unmatched
     closing tokens. <declaration-value> additionally rejects top-level
     semicolons and top-level "!" delimiters. *)
  let parse_decl css =
    (Parser.declaration_value (Reader.of_string css)).value
  in
  let parse_any css = (Parser.any_value (Reader.of_string css)).value in
  let accepted = function Some _ -> true | None -> false in
  Alcotest.(check bool)
    "declaration value accepts ordinary values" true
    (accepted (parse_decl "red 10px var(--x)"));
  Alcotest.(check bool)
    "declaration value accepts nested semicolons" true
    (accepted (parse_decl "{ a; b } f(!important)"));
  Alcotest.(check bool)
    "declaration value accepts top-level comma" true
    (accepted (parse_decl "red, blue"));
  Alcotest.(check bool)
    "declaration value rejects empty input" true
    (parse_decl "  " = None);
  Alcotest.(check bool)
    "declaration value rejects top-level semicolon" true
    (parse_decl "red; blue" = None);
  Alcotest.(check bool)
    "declaration value rejects top-level bang delimiter" true
    (parse_decl "red ! important" = None);
  Alcotest.(check bool)
    "declaration value rejects unmatched right paren" true
    (parse_decl ")" = None);
  Alcotest.(check bool)
    "declaration value rejects unmatched right bracket" true
    (parse_decl "]" = None);
  Alcotest.(check bool)
    "declaration value rejects unmatched right brace" true
    (parse_decl "}" = None);
  Alcotest.(check bool)
    "declaration value rejects bad string token" true
    (parse_decl "\"bad\nnext" = None);
  Alcotest.(check bool)
    "declaration value rejects bad url token" true
    (parse_decl "url(foo\"bar)" = None);
  Alcotest.(check bool)
    "any value accepts top-level semicolon and bang" true
    (accepted (parse_any "red ! important; blue"));
  Alcotest.(check bool)
    "any value rejects empty input" true
    (parse_any " \n\t " = None);
  Alcotest.(check bool)
    "any value accepts top-level comma" true
    (accepted (parse_any "red, blue"));
  Alcotest.(check bool)
    "any value still rejects unmatched closers" true
    (parse_any "}" = None);
  Alcotest.(check bool)
    "any value still rejects bad urls" true
    (parse_any "url(foo\"bar)" = None)

let spec_serialization_string_escaping () =
  (* CSS Syntax Level 3 section 9.2: strings are serialized in a form that
     round-trips, escaping quote and backslash characters and not emitting raw
     newlines inside the string token. *)
  let parse_one css = (Parser.component_value (Reader.of_string css)).value in
  let string_value = function
    | Some (Component.Preserved { kind = Token.String { value; _ }; _ }) ->
        Some value
    | _ -> None
  in
  let roundtrip_string input =
    match parse_one input with
    | Some cv ->
        let serialized = Parser.string_of_components [ cv ] in
        (serialized, string_value (parse_one serialized))
    | None -> Alcotest.failf "expected string component for %S" input
  in
  let serialized, value = roundtrip_string "\"a\\\"b\\\\c\"" in
  Alcotest.(check string)
    "escaped quote and backslash value" "a\"b\\c"
    (Option.value ~default:"<none>" value);
  Alcotest.(check bool)
    "serialized string contains escaped quote" true
    (String.contains serialized '\\');
  let newline_serialized, newline_value = roundtrip_string "\"a\\A b\"" in
  Alcotest.(check string)
    "escaped newline value" "a\nb"
    (Option.value ~default:"<none>" newline_value);
  Alcotest.(check bool)
    "serialized string does not contain raw newline" false
    (String.contains newline_serialized '\n')

let spec_serialization_roundtrip_boundaries () =
  (* CSS Syntax Level 3 section 9: serialization must round-trip through
     tokenization. These pairs are from the consecutive-token separation table:
     dropping all separation would merge or reinterpret the pair. *)
  let parse_list css =
    (Parser.list_of_component_values (Reader.of_string css)).value
  in
  let non_ws cvs =
    List.filter
      (function
        | Component.Preserved { kind = Token.Whitespace; _ } -> false
        | _ -> true)
      cvs
  in
  let check_pair a b =
    let input = a ^ " " ^ b in
    let serialized = Parser.to_string_minified (parse_list input) in
    let reparsed = parse_list serialized in
    Alcotest.(check int)
      (Fmt.str "%S and %S remain separate as %S" a b serialized)
      (List.length (non_ws (parse_list input)))
      (List.length (non_ws reparsed))
  in
  List.iter
    (fun (a, b) -> check_pair a b)
    [
      ("foo", "bar");
      ("foo", "bar()");
      ("foo", "url(a)");
      ("@foo", "bar");
      ("#foo", "bar");
      ("1", "em");
      ("1", "%");
      ("-", "bar");
      ("+", "1");
      (".", "1");
      ("/", "*");
    ];
  Alcotest.(check string)
    "backslash delim serialization" "\\\n"
    (Parser.string_of_components (parse_list "\\"))

let spec_wpt_unclosed_construct_edges () =
  (* WPT unclosed-constructs vectors at the component parser layer. *)
  let list css =
    (Parser.list_of_component_values (Reader.of_string css)).value
  in
  Alcotest.(check string)
    "unclosed function and block auto-close" "calc(1px + [2em])"
    (Parser.string_of_components (list "calc(1px + [2em"));
  Alcotest.(check string)
    "unclosed nested function auto-closes" "f(g(h))"
    (Parser.to_string_minified (list "f(g(h"))

let spec_wpt_trailing_brace_edges () =
  (* WPT trailing-braces vectors: component-value list parsing preserves
     unmatched closing tokens instead of swallowing later input. *)
  let list css =
    (Parser.list_of_component_values (Reader.of_string css)).value
  in
  Alcotest.(check string)
    "trailing right braces are preserved in component lists" "a}}"
    (Parser.to_string_minified (list "a}}"));
  Alcotest.(check string)
    "unmatched mixed closers are preserved" ")]}"
    (Parser.to_string_minified (list ")]}"))

let wpt_at_rule_boundary_edges () =
  (* WPT at-rule recovery shape: semicolon-terminated at-rules and unclosed
     block at-rules must not consume following sibling rules. *)
  let rules = parse_ss "@media screen { .a { color: red } .b { color: blue }" in
  Alcotest.(check int)
    "unclosed at-rule block still forms one at-rule" 1 (List.length rules);
  (match parse_ss "@bad ; .ok { color: green }" with
  | [
   Component.At { node = { name = "bad"; block = None; _ }; _ };
   Component.Qualified _;
  ] ->
      ()
  | _ -> Alcotest.fail "semicolon at-rule should not consume following rule");
  match Parser.rule (Reader.of_string "@bad ; .ok{}") with
  | { value = None; _ } -> ()
  | _ -> Alcotest.fail "parse-rule rejects at-rule plus trailing qualified rule"

let spec_wpt_parser_branch_matrix () =
  (* Focused WPT-style branch matrix for section 5 parser algorithms: nested
     blocks/functions, invalid declarations, CDO/CDC contexts, and list/rule
     entry-point boundaries. *)
  let list css =
    (Parser.list_of_component_values (Reader.of_string css)).value
  in
  let block_items css = (Parser.block_contents (Reader.of_string css)).value in
  Alcotest.(check string)
    "mixed unmatched closers preserved at component layer" "a)]}b"
    (Parser.to_string_minified (list "a)]}b"));
  (match list "a url(foo\"bar) next" with
  | [
   Component.Preserved { kind = Token.Ident "a"; _ };
   Component.Preserved { kind = Token.Whitespace; _ };
   Component.Preserved { kind = Token.Bad_url; _ };
   Component.Preserved { kind = Token.Whitespace; _ };
   Component.Preserved { kind = Token.Ident "next"; _ };
  ] ->
      ()
  | _ -> Alcotest.fail "expected bad-url token and following ident to survive");
  Alcotest.(check string)
    "nested block EOF recovery" "[a {b (c)}]"
    (Parser.string_of_components (list "[a {b (c"));
  (match parse_ss "@a; @b{} .c{}" with
  | [
   Component.At { node = { name = "a"; block = None; _ }; _ };
   Component.At { node = { name = "b"; block = Some _; _ }; _ };
   Component.Qualified _;
  ] ->
      ()
  | _ -> Alcotest.fail "expected semicolon at-rule, block at-rule, rule");
  (match
     block_items "color: red; @supports (display: grid) { &{} } .x{} bad;"
   with
  | [
   `Decls [ { node = { name = "color"; _ }; _ } ];
   `Rule (Component.At { node = { name = "supports"; _ }; _ });
   `Rule (Component.Qualified _);
  ] ->
      ()
  | _ -> Alcotest.fail "expected block declaration/rule recovery branches");
  Alcotest.(check bool)
    "parse-rule rejects declaration-looking input" true
    ((Parser.rule (Reader.of_string "color: red")).value = None);
  Alcotest.(check bool)
    "parse component rejects two comma-separated values" true
    ((Parser.component_value (Reader.of_string "a,b")).value = None)

let spec_wpt_declaration_recovery_matrix () =
  let decls css = (Parser.list_of_declarations (Reader.of_string css)).value in
  let names css =
    decls css
    |> List.filter_map (function
      | `Decl ({ node = { name; _ }; _ } : Component.declaration) -> Some name
      | `At _ -> None)
  in
  Alcotest.(check (list string))
    "bad declarations recover at following semicolon" [ "color"; "width" ]
    (names ": bad; color: red; width: 1px");
  Alcotest.(check (list string))
    "bad string declaration is dropped" [ "color" ]
    (names "background: \"bad\n; color: red");
  Alcotest.(check (list string))
    "bad url declaration is dropped" [ "color" ]
    (names "background: url(foo\"bar); color: red");
  Alcotest.(check (list string))
    "missing colon declaration is dropped" [ "height" ]
    (names "width 10px; height: 20px");
  match decls "@page :left { margin: 1cm } color: red" with
  | [
   `At { node = { name = "page"; block = Some _; _ }; _ };
   `Decl { node = { name = "color"; _ }; _ };
  ] ->
      ()
  | _ -> Alcotest.fail "expected at-rule and declaration in declaration list"

let spec_security_resource_exhaustion_regressions () =
  (* CSS Syntax Level 3 section 11: the spec's security value is that parsing is
     unambiguous for hostile inputs. Keep regression vectors for common parser
     DoS classes: unterminated nesting, bad-url remnants, and many discarded
     comments. *)
  let parse_list css =
    (Parser.list_of_component_values (Reader.of_string css)).value
  in
  let check_completes name f =
    let exception Timeout in
    let previous =
      Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout))
    in
    Fun.protect
      ~finally:(fun () ->
        ignore (Unix.alarm 0);
        ignore (Sys.signal Sys.sigalrm previous))
      (fun () ->
        ignore (Unix.alarm 1);
        try f ()
        with Timeout -> Alcotest.failf "%s did not complete within 1s" name)
  in
  let deep_parens = String.make 4096 '(' ^ "x" in
  check_completes "unterminated paren nesting" (fun () ->
      ignore (parse_list deep_parens));
  let deep_squares = String.make 4096 '[' ^ "x" in
  check_completes "unterminated square nesting" (fun () ->
      ignore (parse_list deep_squares));
  let bad_urls = List.init 64 (fun _ -> "url(foo\"bar) ") |> String.concat "" in
  ignore (parse_list bad_urls);
  let comments =
    List.init 128 (fun i -> Fmt.str "a%d/* ignored { : ; } */" i)
    |> String.concat ""
  in
  let serialized = Parser.to_string_minified (parse_list comments) in
  Alcotest.(check bool)
    "discarded comments do not surface declarations" false
    (String.contains serialized '{')

let spec12_parsing_checklist () =
  (* CSS Syntax Level 3 section 12 is non-normative. These vectors assert the
     current normative parser behavior it lists as changed or clarified. *)
  let block_items css = (Parser.block_contents (Reader.of_string css)).value in
  let parse_decls css =
    (Parser.list_of_declarations (Reader.of_string css)).value
  in
  (match
     block_items
       "color: red; & .child { width: 1px } background: blue; @media screen { \
        & { display: block } } opacity: .5"
   with
  | [
   `Decls [ { node = { name = "color"; _ }; _ } ];
   `Rule (Component.Qualified { node = { prelude = child; _ }; _ });
   `Decls [ { node = { name = "background"; _ }; _ } ];
   `Rule (Component.At { node = { name = "media"; _ }; _ });
   `Decls [ { node = { name = "opacity"; _ }; _ } ];
  ] ->
      Alcotest.(check string)
        "nested style rule prelude" "&.child"
        (Parser.to_string_minified child)
  | _ ->
      Alcotest.fail
        "expected declarations and nested rules to preserve relative order");
  (match parse_decls "color: red; @page :left { margin: 1cm } width: 10px" with
  | [
   `Decl { node = { name = "color"; _ }; _ };
   `At { node = { name = "page"; block = Some _; _ }; _ };
   `Decl { node = { name = "width"; _ }; _ };
  ] ->
      ()
  | _ -> Alcotest.fail "expected at-rule inside declaration list");
  (match parse_decls "unicode-range: U+26, U+4??, auto" with
  | [ `Decl { node = { name = "unicode-range"; value; _ }; _ } ] ->
      Alcotest.(check string)
        "unicode-range descriptor reparsed from source" "U+26,U+400-4FF,auto"
        (Parser.to_string_minified value)
  | _ -> Alcotest.fail "expected unicode-range descriptor declaration");
  Alcotest.(check string)
    "url string is a function component" "url(\"a.png\") url(a.png)"
    (Parser.string_of_components (parse_cvs "url(\"a.png\") url(a.png)"));
  Alcotest.(check string)
    "semicolon in qualified-rule prelude" "a;b"
    (match parse_ss "a; b { color: red }" with
    | [ Component.Qualified { node = { prelude; _ }; _ } ] ->
        Parser.to_string_minified prelude
    | _ -> Alcotest.fail "expected qualified rule with semicolon prelude")

let spec_stylesheet_contents_cdo_cdc () =
  (* CSS Syntax Level 3 section 5.5.1: CDO/CDC are discarded only by the
     stylesheet/top-level rule-list algorithm. *)
  let top =
    (Parser.stylesheet_contents (Reader.of_string "<!-- .a{} --> .b{}")).value
  in
  Alcotest.(check int) "top-level rules" 2 (List.length top);
  (match top with
  | [
   Component.Qualified { node = { prelude = prelude_a; _ }; _ };
   Component.Qualified { node = { prelude = prelude_b; _ }; _ };
  ] ->
      Alcotest.(check string)
        "first prelude" ".a"
        (Parser.to_string_minified prelude_a);
      Alcotest.(check string)
        "second prelude" ".b"
        (Parser.to_string_minified prelude_b)
  | _ -> Alcotest.fail "expected two qualified rules");
  let nested = parse_rules "<!-- .a{} --> .b{}" in
  Alcotest.(check int) "nested/non-top-level rules" 2 (List.length nested);
  match nested with
  | [
   Component.Qualified { node = { prelude = prelude_a; _ }; _ };
   Component.Qualified { node = { prelude = prelude_b; _ }; _ };
  ] ->
      Alcotest.(check string)
        "nested first prelude keeps CDO" "<!--.a"
        (Parser.to_string_minified prelude_a);
      Alcotest.(check string)
        "nested second prelude keeps CDC" "-->.b"
        (Parser.to_string_minified prelude_b)
  | _ -> Alcotest.fail "expected two non-top-level qualified rules"

let spec_at_rule_branches () =
  (* CSS Syntax Level 3 section 5.5.2: semicolon, EOF, and block terminate an
     at-rule; other component values accumulate in the prelude. *)
  let check_import input =
    match parse_ss input with
    | [ Component.At { node = { name; prelude; block = None }; _ } ] ->
        Alcotest.(check string) "name" "import" name;
        Alcotest.(check string)
          "prelude" "url(a.css)"
          (Parser.to_string_minified prelude)
    | _ -> Alcotest.failf "expected semicolon/EOF @import for %S" input
  in
  check_import "@import url(a.css);";
  check_import "@import url(a.css)";
  match parse_ss "@media screen { .a { color: red } }" with
  | [ Component.At { node = { name = "media"; block = Some _; _ }; _ } ] -> ()
  | _ -> Alcotest.fail "expected block at-rule"

let spec_atrule_right_brace () =
  (* CSS Syntax Level 3 section 5.5.2: when not nested, a right brace in an
     at-rule prelude is just another component value. *)
  match parse_ss "@x } y;" with
  | [ Component.At { node = { name = "x"; prelude; block = None }; _ } ] ->
      Alcotest.(check string)
        "right brace preserved in prelude" "}y"
        (Parser.to_string_minified prelude)
  | _ -> Alcotest.fail "expected one @x at-rule"

let spec_qualified_rule_branches () =
  (* CSS Syntax Level 3 section 5.5.3: EOF before a block drops the rule; a
     non-nested stray right brace is preserved in the prelude before the
     block. *)
  Alcotest.(check int)
    "EOF before block drops rule" 0
    (List.length (parse_ss "h1"));
  match parse_ss "} .a {}" with
  | [ Component.Qualified { node = { prelude; _ }; _ } ] ->
      Alcotest.(check string)
        "right brace remains in prelude" "}.a"
        (Parser.to_string_minified prelude)
  | _ -> Alcotest.fail "expected one qualified rule with right brace prelude"

let spec_qualified_custom_prop () =
  (* CSS Syntax Level 3 section 5.5.3: a top-level qualified rule whose first
     two non-whitespace prelude values look like a custom property declaration
     is consumed and discarded, not returned as a rule. *)
  match parse_ss "--x: y { z } .a {}" with
  | [ Component.Qualified { node = { prelude; _ }; _ } ] ->
      Alcotest.(check string)
        "surviving rule prelude" ".a"
        (Parser.to_string_minified prelude)
  | _ ->
      Alcotest.fail
        "expected custom-property-shaped qualified rule to be discarded"

(* ----- Component values ----- *)

let component_value_block () =
  let rs = parse_ss "@x (a b c);" in
  match rs with
  | [ Component.At { node = { prelude; _ }; _ } ] ->
      let has_paren =
        List.exists
          (function
            | Component.Block { node = { opening = Token.Paren; _ }; _ } -> true
            | _ -> false)
          prelude
      in
      Alcotest.(check bool) "prelude contains a (...) block" true has_paren
  | _ -> Alcotest.fail "expected @x at-rule"

let component_value_function () =
  let rs = parse_ss "@x rgb(1, 2, 3);" in
  match rs with
  | [ Component.At { node = { prelude; _ }; _ } ] ->
      let has_func =
        List.exists
          (function
            | Component.Func { node = { name = "rgb"; _ }; _ } -> true
            | _ -> false)
          prelude
      in
      Alcotest.(check bool) "prelude contains rgb(...)" true has_func
  | _ -> Alcotest.fail "expected @x at-rule"

let spec_component_value_algorithms () =
  (* CSS Syntax Level 3 sections 5.5.7 through 5.5.10: lists contain preserved
     tokens, simple blocks, and functions; EOF closes an open function. *)
  let cvs = parse_cvs "[a b] rgb(1, 2)" in
  Alcotest.(check string)
    "simple block and function" "[a b] rgb(1, 2)"
    (Parser.string_of_components cvs);
  let cvs = parse_cvs "rgb(1, 2" in
  Alcotest.(check string)
    "function auto-closed at EOF" "rgb(1, 2)"
    (Parser.string_of_components cvs)

let spec_simple_block_branches () =
  (* CSS Syntax Level 3 section 5.5.9: simple blocks end only at their mirror
     closing token or EOF; mismatched closers are preserved inside the block. *)
  Alcotest.(check string)
    "square block mirror close" "[a) b]"
    (Parser.string_of_components (parse_cvs "[a) b]"));
  Alcotest.(check string)
    "paren block mirror close" "(a] b)"
    (Parser.string_of_components (parse_cvs "(a] b)"));
  Alcotest.(check string)
    "curly block mirror close" "{a] b}"
    (Parser.string_of_components (parse_cvs "{a] b}"));
  Alcotest.(check string)
    "EOF closes simple block" "[a]"
    (Parser.string_of_components (parse_cvs "[a"))

let spec_function_branches () =
  (* CSS Syntax Level 3 section 5.5.10: functions end at ")" or EOF; other
     tokens, including unmatched right braces, are arguments. *)
  Alcotest.(check string)
    "nested function arguments" "calc([a] rgb(1))"
    (Parser.string_of_components (parse_cvs "calc([a] rgb(1))"));
  Alcotest.(check string)
    "right brace argument preserved" "f(})"
    (Parser.string_of_components (parse_cvs "f(})"));
  Alcotest.(check string)
    "EOF closes function" "f(a)"
    (Parser.string_of_components (parse_cvs "f(a"))

(* ----- Declaration list parsing (5.3.6 / 5.3.7) ----- *)

let parse_decls input =
  let r = Reader.of_string input in
  (Parser.list_of_declarations r).value

let basic_declaration () =
  match parse_decls "color: red" with
  | [ `Decl { node = { name; value = _; important }; _ } ] ->
      Alcotest.(check string) "name" "color" name;
      Alcotest.(check bool) "not important" false important
  | _ -> Alcotest.fail "expected one declaration"

let declaration_important () =
  match parse_decls "color: red !important" with
  | [ `Decl { node = { name; important; _ }; _ } ] ->
      Alcotest.(check string) "name" "color" name;
      Alcotest.(check bool) "important" true important
  | _ -> Alcotest.fail "expected one important declaration"

let spec_declaration_important () =
  (* CSS Syntax Level 3 section 5.5.6 recognizes !important with optional
     whitespace between "!" and the ident. *)
  match parse_decls "color: red ! important" with
  | [ `Decl { node = { name; important; _ }; _ } ] ->
      Alcotest.(check string) "name" "color" name;
      Alcotest.(check bool) "important" true important
  | _ -> Alcotest.fail "expected one important declaration"

let spec_declaration_important_edges () =
  (* CSS Syntax Level 3 section 5.5.6 only strips !important when the last two
     non-whitespace component values are "!" and an ASCII-case-insensitive
     "important"; otherwise the tokens remain in the declaration value. *)
  (match parse_decls "color: red ! IMPORTANT  ;" with
  | [ `Decl { node = { important = true; value; _ }; _ } ] ->
      Alcotest.(check string)
        "important value trimmed" "red"
        (Parser.to_string_minified value)
  | _ -> Alcotest.fail "expected important declaration");
  (match parse_decls "color: red ! important extra;" with
  | [ `Decl { node = { important = false; value; _ }; _ } ] ->
      Alcotest.(check string)
        "non-final important left in value" "red!important extra"
        (Parser.to_string_minified value)
  | _ -> Alcotest.fail "expected non-important declaration");
  match parse_decls "background: f(!important) !important;" with
  | [ `Decl { node = { important = true; value; _ }; _ } ] ->
      Alcotest.(check string)
        "only final top-level important stripped" "f(!important)"
        (Parser.to_string_minified value)
  | _ -> Alcotest.fail "expected nested !important to remain in value"

let declaration_multiple () =
  let ds = parse_decls "a: 1; b: 2; c: 3" in
  Alcotest.(check int) "three declarations" 3 (List.length ds)

let declaration_missing_colon_dropped () =
  (* 5.3.7: if ident is not followed by ':', drop. *)
  let ds = parse_decls "color red; width: 10px" in
  Alcotest.(check int) "one declaration" 1 (List.length ds);
  match ds with
  | [ `Decl { node = { name; _ }; _ } ] ->
      Alcotest.(check string) "survivor" "width" name
  | _ -> Alcotest.fail "expected 1 decl"

let declaration_bad_token_dropped () =
  (* 5.3.6: unexpected token triggers skip-to-; *)
  let ds = parse_decls ": red; color: blue" in
  Alcotest.(check int) "one declaration" 1 (List.length ds);
  match ds with
  | [ `Decl { node = { name; _ }; _ } ] ->
      Alcotest.(check string) "survivor" "color" name
  | _ -> Alcotest.fail "expected 1 decl"

(* ----- Warnings (Error.t emitted during recovery) ----- *)

let unterminated_rule_warns () =
  let r = Reader.of_string "h1" in
  let out = Parser.stylesheet r in
  Alcotest.(check int) "no rules" 0 (List.length out.value);
  Alcotest.(check int) "one warning" 1 (List.length out.warnings);
  match out.warnings with
  | [ { sort = Sort.Qualified_rule; kind = Error.Unterminated _; _ } ] -> ()
  | _ -> Alcotest.fail "expected an Unterminated qualified-rule warning"

let missing_colon_warns () =
  let r = Reader.of_string "color red" in
  let out = Parser.list_of_declarations r in
  Alcotest.(check int) "no decls" 0 (List.length out.value);
  match out.warnings with
  | [ { sort = Sort.Declaration; kind = Error.Missing_token "':'"; _ } ] -> ()
  | _ -> Alcotest.fail "expected a Missing_token ':' warning"

let unexpected_token_warns () =
  let r = Reader.of_string ": red; color: blue" in
  let out = Parser.list_of_declarations r in
  Alcotest.(check int) "one survivor" 1 (List.length out.value);
  match out.warnings with
  | [ { sort = Sort.Declaration; kind = Error.Unexpected_token _; _ } ] -> ()
  | _ -> Alcotest.fail "expected an Unexpected_token warning"

let warning_carries_snippet () =
  (* Unterminated [@media] with an unclosed qualified rule inside triggers a
     recovery warning; the warning should carry a source snippet with a caret
     pointing at the EOF position, just like a Cursor-raised error would. *)
  let input = "h1" in
  let r = Reader.of_string input in
  let out = Parser.stylesheet r in
  match out.warnings with
  | [ w ] -> (
      match Error.snippet w with
      | None -> Alcotest.fail "warning missing source snippet"
      | Some { text; _ } ->
          Alcotest.(check string)
            "snippet text is the whole short input" input text)
  | _ -> Alcotest.fail "expected exactly one warning"

let declaration_at_rule_mixed () =
  (* 5.3.6 permits at-rules in declaration lists. *)
  let ds = parse_decls "color: red; @media screen { } ; width: 10px" in
  let decl_count =
    List.length (List.filter (function `Decl _ -> true | _ -> false) ds)
  in
  let at_count =
    List.length (List.filter (function `At _ -> true | _ -> false) ds)
  in
  Alcotest.(check int) "two declarations" 2 decl_count;
  Alcotest.(check int) "one at-rule" 1 at_count

let spec_bad_declaration_remnants () =
  (* CSS Syntax Level 3 section 5.5.6: a bad declaration consumes component
     values, including nested blocks with semicolons, until its own
     semicolon. *)
  match parse_decls "color { bad: block; }; width: 10px" with
  | [ `Decl { node = { name = "width"; _ }; _ } ] -> ()
  | _ -> Alcotest.fail "expected bad block-shaped declaration to be discarded"

let spec_declaration_block_value_rules () =
  (* CSS Syntax Level 3 section 5.5.6: top-level {} blocks are allowed as part
     of custom property values, but for non-custom properties they are only
     allowed when they are the entire non-whitespace value. *)
  (match parse_decls "color: { red }; width: 1px" with
  | [
   `Decl { node = { name = "color"; value; _ }; _ };
   `Decl { node = { name = "width"; _ }; _ };
  ] ->
      Alcotest.(check string)
        "entire block value survives" "{red}"
        (Parser.to_string_minified value)
  | _ -> Alcotest.fail "expected whole-block non-custom value to survive");
  (match parse_decls "--x: { a: b; } tail; width: 1px" with
  | [
   `Decl { node = { name = "--x"; value = custom_value; _ }; _ };
   `Decl { node = { name = "width"; _ }; _ };
  ] ->
      Alcotest.(check string)
        "custom property value" "{a:b;}tail"
        (Parser.to_string_minified custom_value)
  | _ -> Alcotest.fail "expected custom property block value to survive");
  match parse_decls "color: { red } blue; width: 1px" with
  | [ `Decl { node = { name = "width"; _ }; _ } ] -> ()
  | _ ->
      Alcotest.fail
        "expected non-custom mixed top-level {} declaration to be discarded"

let spec_decl_empty_recovery () =
  (* CSS Syntax Level 3 sections 5.5.4 and 5.5.6: declarations can have empty
     values after the colon, and bad declarations consume their own remnants
     without losing the following declaration. *)
  (match parse_decls "margin: ; --empty: ; width: 1px" with
  | [
   `Decl { node = { name = "margin"; value = margin; _ }; _ };
   `Decl { node = { name = "--empty"; value = custom; _ }; _ };
   `Decl { node = { name = "width"; _ }; _ };
  ] ->
      Alcotest.(check int) "empty normal value" 0 (List.length margin);
      Alcotest.(check int) "empty custom value" 0 (List.length custom)
  | _ -> Alcotest.fail "expected empty declarations followed by width");
  match parse_decls "color red; } width: 1px; height: 2px" with
  | [
   `Decl { node = { name = "width"; _ }; _ };
   `Decl { node = { name = "height"; _ }; _ };
  ] ->
      ()
  | _ ->
      Alcotest.fail "expected declarations after missing colon and right brace"

let spec_declaration_unicode_range_descriptor () =
  (* CSS Syntax Level 3 section 5.5.11: unicode-range descriptor values are
     represented as unicode-range component values, not ident/number
     fragments. *)
  match parse_decls "unicode-range: U+26, U+4??" with
  | [ `Decl { node = { name = "unicode-range"; value; _ }; _ } ] ->
      Alcotest.(check string)
        "unicode-range descriptor value" "U+26,U+400-4FF"
        (Parser.to_string_minified value)
  | _ -> Alcotest.fail "expected one unicode-range declaration"

let spec_decl_urange_descriptor () =
  (* CSS Syntax Level 3 section 5.5.11 retokenizes with unicode ranges allowed
     but still returns a normal component-value list for non-range fragments. *)
  (match parse_decls "unicode-range: u+0-7f, auto" with
  | [ `Decl { node = { name = "unicode-range"; value; _ }; _ } ] ->
      Alcotest.(check string)
        "range plus ident" "U+0-7F,auto"
        (Parser.to_string_minified value)
  | _ -> Alcotest.fail "expected unicode-range plus ident declaration");
  match parse_decls "unicode-range: u+" with
  | [ `Decl { node = { name = "unicode-range"; value; _ }; _ } ] ->
      Alcotest.(check string)
        "false range start remains components" "u+"
        (Parser.to_string_minified value)
  | _ -> Alcotest.fail "expected false unicode-range start declaration"

let spec_csv_nested_edges () =
  (* CSS Syntax Level 3 section 5.4.10: top-level commas split groups, but
     commas inside blocks and functions remain part of that group's component
     value list. Empty interior groups are preserved for grammar matching. *)
  let groups = Parser.csv_component_values (Reader.of_string "a,,b") in
  Alcotest.(check int) "a,,b has three groups" 3 (List.length groups.value);
  Alcotest.(check string)
    "empty interior group" "a||b"
    (groups.value |> List.map Parser.to_string_minified |> String.concat "|");
  let groups =
    Parser.csv_component_values
      (Reader.of_string "rgb(1, 2, 3), [a,b], {c:d,e:f}")
  in
  Alcotest.(check int) "nested commas not split" 3 (List.length groups.value);
  Alcotest.(check string)
    "nested comma groups" "rgb(1,2,3)|[a,b]|{c:d,e:f}"
    (groups.value |> List.map Parser.to_string_minified |> String.concat "|")

let spec_deep_nesting_edges () =
  (* CSS Syntax Level 3 section 5.5.9 closes simple blocks at EOF. This is a
     bounded regression vector for stack/resource behavior and auto-closing. *)
  let input =
    String.make 64 '(' ^ "color(red" ^ String.make 32 ')' ^ String.make 16 ']'
  in
  let cvs = parse_cvs input in
  Alcotest.(check bool) "deep mixed nesting parsed" true (cvs <> []);
  let serialized = Parser.to_string_minified cvs in
  let reparsed = parse_cvs serialized in
  Alcotest.(check string)
    "deep mixed nesting reserializes" serialized
    (Parser.to_string_minified reparsed)

let spec_comment_recovery_edges () =
  (* CSS Syntax Level 3 sections 4.3.2 and 5.5: comments disappear before
     component parsing, but serialization must preserve identifier token
     boundaries so comments cannot merge two identifiers into a new token. *)
  Alcotest.(check string)
    "comment between decl tokens" "color red"
    (parse_cvs "color/*x*/red" |> Parser.to_string_minified);
  Alcotest.(check string)
    "unterminated comment hides braces" "safe"
    (parse_cvs "safe/* .evil { color:red }" |> Parser.to_string_minified);
  let rs = parse_ss ".a { color:red /* hidden } .b { color:blue }" in
  Alcotest.(check int) "one rule after hidden brace" 1 (List.length rs)

let suite =
  ( "parser",
    [
      Alcotest.test_case "simple rule" `Quick simple_rule;
      Alcotest.test_case "multiple rules" `Quick multiple_rules;
      Alcotest.test_case "at-rule with block" `Quick at_rule_with_block;
      Alcotest.test_case "at-rule semicolon-terminated" `Quick
        at_rule_semi_terminated;
      Alcotest.test_case "bad selector kept structurally" `Quick
        bad_selector_drops_rule;
      Alcotest.test_case "unterminated qualified rule dropped" `Quick
        unterminated_qualified_rule_dropped;
      Alcotest.test_case "spec sections 2 and 5 rule shapes" `Quick
        spec_syntax_description_examples;
      Alcotest.test_case "spec section 2.2 EOF closes constructs" `Quick
        spec_error_handling_eof_closes;
      Alcotest.test_case "spec sections 5.4.1-5.4.2 grammar entry points" `Quick
        spec_parse_grammar_entry_points;
      Alcotest.test_case "spec sections 5.4.1-5.4.2 grammar empty/comma edges"
        `Quick spec_grammar_empty_commas;
      Alcotest.test_case "spec section 5.4.3 parse stylesheet entry point"
        `Quick spec_parse_stylesheet_entry_point;
      Alcotest.test_case "spec section 5.4.6 parse rule entry point" `Quick
        spec_parse_rule_entry_point;
      Alcotest.test_case "spec section 5.4.5 block contents mixed items" `Quick
        spec_block_mixed_items;
      Alcotest.test_case "spec section 5.5.5 block contents discard branches"
        `Quick spec_block_discard_branches;
      Alcotest.test_case
        "spec section 5.5.5 block contents nested error branches" `Quick
        spec_block_nested_errors;
      Alcotest.test_case "spec section 5.5.5 block contents nested boundaries"
        `Quick spec_block_nested_boundaries;
      Alcotest.test_case "spec section 5.5.5 block contents reparse examples"
        `Quick spec_block_reparse_examples;
      Alcotest.test_case "spec section 5.5.5 block contents flush/stop edges"
        `Quick spec_block_flush_stop;
      Alcotest.test_case
        "spec section 5.5.5 block contents custom property edges" `Quick
        spec_block_custom_props;
      Alcotest.test_case "spec section 5.4.7 parse declaration entry point"
        `Quick spec_parse_declaration_entry_point;
      Alcotest.test_case "spec section 5.4.8 parse component value entry point"
        `Quick spec_component_entry_point;
      Alcotest.test_case
        "spec section 5.4.9 parse list of component values entry point" `Quick
        spec_list_components_entry;
      Alcotest.test_case
        "spec section 5.4.10 parse comma-separated component values" `Quick
        spec_csv_components;
      Alcotest.test_case
        "spec section 7.2 declaration-value and any-value productions" `Quick
        spec_arbitrary_value_productions;
      Alcotest.test_case "spec section 9.2 string serialization" `Quick
        spec_serialization_string_escaping;
      Alcotest.test_case "spec section 9 serialization boundaries" `Quick
        spec_serialization_roundtrip_boundaries;
      Alcotest.test_case "spec WPT unclosed construct edges" `Quick
        spec_wpt_unclosed_construct_edges;
      Alcotest.test_case "spec WPT trailing brace edges" `Quick
        spec_wpt_trailing_brace_edges;
      Alcotest.test_case "spec WPT at-rule boundary edges" `Quick
        wpt_at_rule_boundary_edges;
      Alcotest.test_case "spec WPT parser branch matrix" `Quick
        spec_wpt_parser_branch_matrix;
      Alcotest.test_case "spec WPT declaration recovery matrix" `Quick
        spec_wpt_declaration_recovery_matrix;
      Alcotest.test_case "spec section 11 parser security regressions" `Quick
        spec_security_resource_exhaustion_regressions;
      Alcotest.test_case "spec section 12 parsing change checklist" `Quick
        spec12_parsing_checklist;
      Alcotest.test_case "spec section 5.5.1 CDO/CDC rule-list handling" `Quick
        spec_stylesheet_contents_cdo_cdc;
      Alcotest.test_case "spec section 5.5.2 at-rule branches" `Quick
        spec_at_rule_branches;
      Alcotest.test_case "spec section 5.5.2 non-nested right brace at-rule"
        `Quick spec_atrule_right_brace;
      Alcotest.test_case "spec section 5.5.3 qualified-rule branches" `Quick
        spec_qualified_rule_branches;
      Alcotest.test_case "spec section 5.5.3 custom property rule ambiguity"
        `Quick spec_qualified_custom_prop;
      Alcotest.test_case "component value: block" `Quick component_value_block;
      Alcotest.test_case "component value: function" `Quick
        component_value_function;
      Alcotest.test_case "spec section 5.5 component values" `Quick
        spec_component_value_algorithms;
      Alcotest.test_case "spec section 5.5.9 simple block branches" `Quick
        spec_simple_block_branches;
      Alcotest.test_case "spec section 5.5.10 function branches" `Quick
        spec_function_branches;
      Alcotest.test_case "declaration basic" `Quick basic_declaration;
      Alcotest.test_case "declaration important" `Quick declaration_important;
      Alcotest.test_case "spec section 5.5.6 declaration important" `Quick
        spec_declaration_important;
      Alcotest.test_case "spec section 5.5.6 declaration important edges" `Quick
        spec_declaration_important_edges;
      Alcotest.test_case "declaration multiple" `Quick declaration_multiple;
      Alcotest.test_case "declaration missing colon dropped" `Quick
        declaration_missing_colon_dropped;
      Alcotest.test_case "declaration bad token dropped" `Quick
        declaration_bad_token_dropped;
      Alcotest.test_case "warning: unterminated qualified rule" `Quick
        unterminated_rule_warns;
      Alcotest.test_case "warning: missing colon" `Quick missing_colon_warns;
      Alcotest.test_case "warning: unexpected token" `Quick
        unexpected_token_warns;
      Alcotest.test_case "warning: carries source snippet" `Quick
        warning_carries_snippet;
      Alcotest.test_case "declaration at-rule in list" `Quick
        declaration_at_rule_mixed;
      Alcotest.test_case "spec section 5.5.6 bad declaration remnants" `Quick
        spec_bad_declaration_remnants;
      Alcotest.test_case "spec section 5.5.6 declaration block value rules"
        `Quick spec_declaration_block_value_rules;
      Alcotest.test_case "spec section 5.5.6 declaration recovery edges" `Quick
        spec_decl_empty_recovery;
      Alcotest.test_case "spec section 5.5.11 unicode-range descriptor" `Quick
        spec_declaration_unicode_range_descriptor;
      Alcotest.test_case "spec section 5.5.11 unicode-range descriptor mixed"
        `Quick spec_decl_urange_descriptor;
      Alcotest.test_case "spec section 5.4.10 nested comma edges" `Quick
        spec_csv_nested_edges;
      Alcotest.test_case "spec section 5.5 deep nesting edges" `Quick
        spec_deep_nesting_edges;
      Alcotest.test_case "spec comment recovery edges" `Quick
        spec_comment_recovery_edges;
    ] )

(* Keep helper constructors referenced. *)
let _ = pv
let _ = block
let _ = func
let _ = pp_rules
