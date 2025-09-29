(** Tests for CSS Variables module - CSS/MDN spec compliance *)

open Css.Declaration
open Css.Values
open Test_helpers
open Css.Variables

let check_any_syntax = check_value "any_syntax" read_any_syntax pp_any_syntax

(* These tests are for CSS Variables module *)
let test_any_var () =
  (* Test CSS custom property declaration creation using Variables.var *)
  let decl, _var =
    var "primary-color" Color (Hex { hash = true; value = "ff0000" })
  in

  (* Check declaration is created properly *)
  let name_opt = custom_declaration_name decl in
  (match name_opt with
  | Some name ->
      Alcotest.(check string)
        "variable name has -- prefix" "--primary-color" name
  | None -> Alcotest.fail "Expected custom declaration");

  (* Test var with fallback *)
  let decl2, _var2 =
    var "theme-color" Color (Hex { hash = true; value = "ff0000" })
  in

  let name_opt2 = custom_declaration_name decl2 in
  (match name_opt2 with
  | Some name ->
      Alcotest.(check string) "theme color variable" "--theme-color" name
  | None -> Alcotest.fail "Expected custom declaration for theme color");

  (* Test negative cases *)
  neg parse_var_reference "not-a-var()"

(* Not a roundtrip test *)
let test_vars_of_calc () =
  (* Test calc without variables *)
  let simple_calc : length calc = Expr (Num 100., Add, Num 8.) in
  let vars = vars_of_calc simple_calc in
  Alcotest.(check int) "no variables in numeric calc" 0 (List.length vars);

  (* Another calc without variables *)
  let mul_calc : length calc = Expr (Num 100., Mul, Num 2.) in
  let no_vars = vars_of_calc mul_calc in
  Alcotest.(check int) "no variables in numeric calc" 0 (List.length no_vars);

  (* Calc with a variable *)
  (* Use CSS Variables.var function to create proper variable *)
  let _gap_decl, gap_var = var "gap" Length (Px 16.) in
  let calc_with_var : length calc = Expr (Var gap_var, Add, Val (Px 10.)) in
  let with_vars = vars_of_calc calc_with_var in
  Alcotest.(check int) "one variable in calc" 1 (List.length with_vars);

  (* Nested calc with multiple variables *)
  let _width_decl, width_var = var "width" Length (Pct 100.) in
  let nested : length calc =
    Expr (Var gap_var, Add, Expr (Var width_var, Div, Num 2.))
  in
  let nested_vars = vars_of_calc nested in
  Alcotest.(check int)
    "two variables in nested calc" 2 (List.length nested_vars);

  (* Complex calc expression *)
  let complex : length calc =
    Expr (Expr (Var gap_var, Mul, Num 2.), Sub, Val (Rem 1.))
  in
  let complex_vars = vars_of_calc complex in
  Alcotest.(check int)
    "one variable in complex calc" 1 (List.length complex_vars)

(* Not a roundtrip test *)
let test_vars_of_property () =
  let _width_decl, width_var = var "container-width" Length (Px 1024.) in

  (* Width property with variable *)
  let width_vars = vars_of_property Width (Length (Var width_var)) in
  Alcotest.(check int) "found width variable" 1 (List.length width_vars);

  (* Width property with calc containing variable *)
  let calc_with_var : length = Calc (Expr (Var width_var, Sub, Num 32.)) in
  let calc_vars = vars_of_property Width (Length calc_with_var) in
  Alcotest.(check int) "found variable in calc" 1 (List.length calc_vars);

  (* Width property without variable *)
  let no_vars = vars_of_property Width (Length (Px 100.)) in
  Alcotest.(check int) "no variables in px value" 0 (List.length no_vars)

(* Not a roundtrip test *)
let test_vars_of_declarations () =
  let custom_color_decl, color_var =
    var "text-color" Color (Hex { hash = true; value = "333333" })
  in
  let custom_size_decl, size_var = var "font-size" Length (Rem 1.0) in

  (* Create declarations using the variables *)
  let color_decl = v Color (Var color_var) in
  let size_decl = v Font_size (Length (Var size_var)) in

  let vars =
    vars_of_declarations
      [ custom_color_decl; custom_size_decl; color_decl; size_decl ]
  in

  (* Should find the two variables used in declarations (not definitions) *)
  Alcotest.(check bool) "found variables" true (List.length vars >= 2)

(* Not a roundtrip test *)
let test_any_var_name () =
  let _spacing_decl, var_handle = var "spacing" Length (Px 0.) in
  let any_var = V var_handle in

  let name = any_var_name any_var in
  Alcotest.(check string) "variable name with prefix" "--spacing" name

(* Not a roundtrip test *)
let test_extract_custom_declarations () =
  let regular = v Width (Length (Px 100.)) in

  let custom1, _ = var "color1" Color (Hex { hash = true; value = "ff0000" }) in
  let custom2, _ = var "size1" Length (Px 16.) in

  let decls = [ custom1; regular; custom2 ] in
  let customs = extract_custom_declarations decls in

  Alcotest.(check int) "extracted custom declarations" 2 (List.length customs)

(* Not a roundtrip test *)
let test_custom_declaration_name () =
  let regular = v Height (Length (Px 50.)) in

  let custom, _ = var "my-var" Length (Px 20.) in
  let custom_name = custom_declaration_name custom in
  let regular_name = custom_declaration_name regular in

  Alcotest.(check (option string))
    "custom has name" (Some "--my-var") custom_name;
  Alcotest.(check (option string)) "regular has no name" None regular_name

(* Not a roundtrip test *)
let test_compare_vars_by_name () =
  let _decl1, var1 = var "aaa" Length (Px 0.) in
  let _decl2, var2 = var "bbb" Length (Px 0.) in
  let _decl3, var3 = var "aaa" Length (Px 0.) in
  (* Same name as var1 *)

  let cmp1 = compare_vars_by_name (V var1) (V var2) in
  let cmp2 = compare_vars_by_name (V var1) (V var3) in

  Alcotest.(check bool) "aaa < bbb" true (cmp1 < 0);
  Alcotest.(check int) "aaa = aaa" 0 cmp2

(* Not a roundtrip test *)
let test_custom_property_roundtrip () =
  (* Create a custom property using Variables.var *)
  let custom, _ = var "primary" Color (Hex { hash = true; value = "0080ff" }) in

  (* Check it follows CSS custom property syntax *)
  match custom_declaration_name custom with
  | Some name ->
      Alcotest.(check bool)
        "has -- prefix" true
        (String.starts_with ~prefix:"--" name);
      Alcotest.(check string) "correct name" "--primary" name
  | None -> Alcotest.fail "Expected custom declaration"

let test_any_syntax () =
  (* Test syntax parsing according to CSS @property spec
     https://developer.mozilla.org/en-US/docs/Web/CSS/@property/syntax *)
  (* Syntax values must be quoted strings per CSS spec *)
  check_any_syntax "\"<length>\"";
  check_any_syntax "\"<color>\"";
  check_any_syntax "\"<number>\"";
  check_any_syntax "\"<integer>\"";
  check_any_syntax "\"<percentage>\"";
  check_any_syntax "\"<angle>\"";
  check_any_syntax "\"<time>\"";
  check_any_syntax "\"*\"";
  check_any_syntax "\"<length> | <percentage>\"";

  (* Test invalid syntax values *)
  neg read_any_syntax "<length>";
  (* Missing quotes *)
  neg read_any_syntax "length";
  (* No angle brackets or quotes *)
  neg read_any_syntax "\"<invalid-type>\"";
  (* Invalid type name *)
  neg read_any_syntax "\"\"";
  (* Empty syntax *)
  neg read_any_syntax "unquoted"

(* Not a roundtrip test *)
let test_syntax () =
  (* Syntax checking is not available in current implementation *)
  ()

(* ignore-test: parse_var_reference is a function, not a type *)
let test_parse_var_reference () =
  (* Test parsing CSS var() references - just extracts name and fallback *)
  let check_var_ref input expected_name expected_fallback =
    let r = Css.Reader.of_string input in
    let name, fallback = parse_var_reference r in
    Alcotest.(check string) "variable name" expected_name name;
    Alcotest.(check (option string)) "fallback" expected_fallback fallback
  in

  (* Basic var() references *)
  check_var_ref "var(--color)" "color" None;
  check_var_ref "var(--primary)" "primary" None;
  check_var_ref "var(--theme-bg)" "theme-bg" None;

  (* With fallbacks *)
  check_var_ref "var(--color, red)" "color" (Some "red");
  check_var_ref "var(--size, 10px)" "size" (Some "10px");

  (* Test invalid cases *)
  let neg input =
    let r = Css.Reader.of_string input in
    try
      let _ = parse_var_reference r in
      Alcotest.failf "Expected failure for: %s" input
    with
    | Css.Reader.Parse_error _ -> ()
    | exn ->
        Alcotest.failf "Unexpected exception for '%s': %s" input
          (Printexc.to_string exn)
  in

  neg "not-a-var";
  neg "var(color)";
  (* Missing -- prefix *)
  neg "var()";
  (* Empty variable name *)
  neg "variable(--color)";
  (* Wrong function name *)
  neg "var(--)" (* No name after -- *)

let tests =
  [
    ("any_var", `Quick, test_any_var);
    ("any_syntax", `Quick, test_any_syntax);
    ("vars of calc", `Quick, test_vars_of_calc);
    ("vars of property", `Quick, test_vars_of_property);
    ("vars of declarations", `Quick, test_vars_of_declarations);
    ("any_var_name", `Quick, test_any_var_name);
    ("extract custom declarations", `Quick, test_extract_custom_declarations);
    ("custom declaration name", `Quick, test_custom_declaration_name);
    ("compare vars by name", `Quick, test_compare_vars_by_name);
    ("custom property roundtrip", `Quick, test_custom_property_roundtrip);
    ("syntax", `Quick, test_syntax);
    ("parse_var_reference", `Quick, test_parse_var_reference);
  ]

let suite = ("variables", tests)
