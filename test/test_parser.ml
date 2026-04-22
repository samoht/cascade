(** Tests for the CSS Syntax section 5.3 parser algorithms. *)

open Cascade

(* Shorthand constructors mirroring Parser.component_value so test data is
   readable. *)

let pv t = Css.Parser.Preserved t
let block op vs = Css.Parser.Block { opening = op; value = vs }
let func name args = Css.Parser.Func { name; arguments = args }

(* Pretty-print a component value for assertion diffing. *)

let rec pp_cv : Css.Parser.component_value Css.Pp.t =
 fun ctx cv ->
  match cv with
  | Css.Parser.Preserved t -> Css.Token.pp ctx t
  | Css.Parser.Block { opening; value } ->
      let open_c, close_c =
        match opening with
        | Css.Token.Curly -> ('{', '}')
        | Css.Token.Paren -> ('(', ')')
        | Css.Token.Square -> ('[', ']')
      in
      Css.Pp.char ctx open_c;
      Css.Pp.list ~sep:Css.Pp.sp pp_cv ctx value;
      Css.Pp.char ctx close_c
  | Css.Parser.Func { name; arguments } ->
      Css.Pp.string ctx name;
      Css.Pp.char ctx '(';
      Css.Pp.list ~sep:Css.Pp.sp pp_cv ctx arguments;
      Css.Pp.char ctx ')'

let pp_cvs ctx cvs = Css.Pp.list ~sep:Css.Pp.sp pp_cv ctx cvs

let pp_rule : Css.Parser.rule Css.Pp.t =
 fun ctx -> function
  | Css.Parser.Qualified { prelude; block = { opening = _; value } } ->
      Css.Pp.string ctx "qualified{prelude=";
      pp_cvs ctx prelude;
      Css.Pp.string ctx "; block=";
      pp_cvs ctx value;
      Css.Pp.char ctx '}'
  | Css.Parser.At { name; prelude; block } ->
      Css.Pp.string ctx "at[";
      Css.Pp.string ctx name;
      Css.Pp.string ctx "]{prelude=";
      pp_cvs ctx prelude;
      Css.Pp.string ctx "; block=";
      (match block with
      | None -> Css.Pp.string ctx "<none>"
      | Some { value; _ } -> pp_cvs ctx value);
      Css.Pp.char ctx '}'

let pp_rules ctx rules = Css.Pp.list ~sep:Css.Pp.sp pp_rule ctx rules

let parse_ss css =
  let r = Css.Reader.of_string css in
  Css.Parser.parse_stylesheet r

(* ----- Basic shape tests ----- *)

let simple_rule () =
  let rs = parse_ss ".a { color: red }" in
  Alcotest.(check int) "one rule" 1 (List.length rs);
  match rs with
  | [ Css.Parser.Qualified { prelude = _; block = { value; _ } } ] ->
      (* block contains: ws, ident color, colon, ws, ident red, ws *)
      Alcotest.(check bool) "block non-empty" true (value <> [])
  | _ -> Alcotest.fail "expected one qualified rule"

let multiple_rules () =
  let rs = parse_ss ".a { } .b { } .c { }" in
  Alcotest.(check int) "three rules" 3 (List.length rs)

let at_rule_with_block () =
  let rs = parse_ss "@media screen { .btn { color: red } }" in
  match rs with
  | [ Css.Parser.At { name; block = Some _; _ } ] ->
      Alcotest.(check string) "name" "media" name
  | _ -> Alcotest.fail "expected one @media at-rule with block"

let at_rule_semi_terminated () =
  let rs = parse_ss "@charset \"utf-8\";" in
  match rs with
  | [ Css.Parser.At { name; block = None; _ } ] ->
      Alcotest.(check string) "name" "charset" name
  | _ -> Alcotest.fail "expected @charset without block"

let bad_selector_drops_rule () =
  (* The prelude <!--.. parses as component values; the rule still forms because
     it has a block. This test just checks we don't crash. *)
  let rs = parse_ss "..bad { } .good { color: red }" in
  Alcotest.(check int) "two rules survive structurally" 2 (List.length rs)

let unterminated_qualified_rule_dropped () =
  (* EOF before the opening brace drops the rule per 5.3.4. *)
  let rs = parse_ss "h1" in
  Alcotest.(check int) "zero rules" 0 (List.length rs)

(* ----- Component values ----- *)

let component_value_block () =
  let rs = parse_ss "@x (a b c);" in
  match rs with
  | [ Css.Parser.At { prelude; _ } ] ->
      let has_paren =
        List.exists
          (function
            | Css.Parser.Block { opening = Css.Token.Paren; _ } -> true
            | _ -> false)
          prelude
      in
      Alcotest.(check bool) "prelude contains a (...) block" true has_paren
  | _ -> Alcotest.fail "expected @x at-rule"

let component_value_function () =
  let rs = parse_ss "@x rgb(1, 2, 3);" in
  match rs with
  | [ Css.Parser.At { prelude; _ } ] ->
      let has_func =
        List.exists
          (function Css.Parser.Func { name = "rgb"; _ } -> true | _ -> false)
          prelude
      in
      Alcotest.(check bool) "prelude contains rgb(...)" true has_func
  | _ -> Alcotest.fail "expected @x at-rule"

(* ----- Declaration list parsing (5.3.6 / 5.3.7) ----- *)

let parse_decls input =
  let r = Css.Reader.of_string input in
  Css.Parser.parse_list_of_declarations r

let basic_declaration () =
  match parse_decls "color: red" with
  | [ `Decl { name; value = _; important } ] ->
      Alcotest.(check string) "name" "color" name;
      Alcotest.(check bool) "not important" false important
  | _ -> Alcotest.fail "expected one declaration"

let declaration_important () =
  match parse_decls "color: red !important" with
  | [ `Decl { name; important; _ } ] ->
      Alcotest.(check string) "name" "color" name;
      Alcotest.(check bool) "important" true important
  | _ -> Alcotest.fail "expected one important declaration"

let declaration_multiple () =
  let ds = parse_decls "a: 1; b: 2; c: 3" in
  Alcotest.(check int) "three declarations" 3 (List.length ds)

let declaration_missing_colon_dropped () =
  (* 5.3.7: if ident is not followed by ':', drop. *)
  let ds = parse_decls "color red; width: 10px" in
  Alcotest.(check int) "one declaration" 1 (List.length ds);
  match ds with
  | [ `Decl { name; _ } ] -> Alcotest.(check string) "survivor" "width" name
  | _ -> Alcotest.fail "expected 1 decl"

let declaration_bad_token_dropped () =
  (* 5.3.6: unexpected token triggers skip-to-; *)
  let ds = parse_decls ": red; color: blue" in
  Alcotest.(check int) "one declaration" 1 (List.length ds);
  match ds with
  | [ `Decl { name; _ } ] -> Alcotest.(check string) "survivor" "color" name
  | _ -> Alcotest.fail "expected 1 decl"

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
      Alcotest.test_case "component value: block" `Quick component_value_block;
      Alcotest.test_case "component value: function" `Quick
        component_value_function;
      Alcotest.test_case "declaration basic" `Quick basic_declaration;
      Alcotest.test_case "declaration important" `Quick declaration_important;
      Alcotest.test_case "declaration multiple" `Quick declaration_multiple;
      Alcotest.test_case "declaration missing colon dropped" `Quick
        declaration_missing_colon_dropped;
      Alcotest.test_case "declaration bad token dropped" `Quick
        declaration_bad_token_dropped;
      Alcotest.test_case "declaration at-rule in list" `Quick
        declaration_at_rule_mixed;
    ] )

(* Silence unused warnings for the helpers exposed for future tests. *)
let _ = pv
let _ = block
let _ = func
let _ = pp_rules
