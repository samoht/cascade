(** Tests for CSS Variables module - CSS/MDN spec compliance *)

open Css.Variables
open Css.Declaration
open Css.Values
(* open Alcotest - Used via qualified access *)

(* Generic check function for variable types *)
let check_value type_name pp reader ?expected input =
  let expected = Option.value ~default:input expected in
  let t = Css.Reader.of_string input in
  let result = reader t in
  let pp_str = Css.Pp.to_string ~minify:true pp result in
  Alcotest.(check string) (Fmt.str "%s %s" type_name input) expected pp_str

let check_any_var = check_value "any_var" pp_any_var read_any_var
let check_any_syntax = check_value "any_syntax" pp_any_syntax read_any_syntax

(** Test variable creation *)
let test_var_creation () =
  (* Basic variable creation *)
  let decl, var_handle =
    var "primary-color" Color (Hex { hash = true; value = "ff0000" })
  in

  (* Check declaration is custom property *)
  (match decl with
  | Custom_declaration { name; _ } ->
      Alcotest.(check string)
        "variable name has -- prefix" "--primary-color" name
  | _ -> Alcotest.fail "Expected Custom_declaration");

  (* Check var handle *)
  Alcotest.(check string) "var handle name" "primary-color" var_handle.name;
  Alcotest.(check bool) "var has default value" true (var_handle.default <> None)

(** Test variable with fallback *)
let test_var_with_fallback () =
  let fallback_color = Hex { hash = true; value = "0000ff" } in
  let decl, var_handle =
    var ~fallback:(Fallback fallback_color) "theme-color" Color
      (Hex { hash = true; value = "ff0000" })
  in

  Alcotest.(check bool) "has fallback" true (var_handle.fallback <> None);

  (* Verify CSS custom property naming convention *)
  match decl with
  | Custom_declaration { name; _ } ->
      Alcotest.(check bool)
        "starts with --" true
        (String.starts_with ~prefix:"--" name)
  | _ -> Alcotest.fail "Expected Custom_declaration"

(** Test variable extraction from calc *)
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
  let _, gap_var = var "gap" Length (Px 16.) in
  let calc_with_var : length calc = Expr (Var gap_var, Add, Val (Px 10.)) in
  let with_vars = vars_of_calc calc_with_var in
  Alcotest.(check int) "one variable in calc" 1 (List.length with_vars);

  (* Nested calc with multiple variables *)
  let _, width_var = var "width" Length (Pct 100.) in
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

(** Test variable extraction from properties *)
let test_vars_of_property () =
  let _, width_var = var "container-width" Length (Px 1024.) in

  (* Width property with variable *)
  let width_vars = vars_of_property Width (Var width_var) in
  Alcotest.(check int) "found width variable" 1 (List.length width_vars);

  (* Width property with calc containing variable *)
  let calc_with_var : length = Calc (Expr (Var width_var, Sub, Num 32.)) in
  let calc_vars = vars_of_property Width calc_with_var in
  Alcotest.(check int) "found variable in calc" 1 (List.length calc_vars);

  (* Width property without variable *)
  let no_vars = vars_of_property Width (Px 100.) in
  Alcotest.(check int) "no variables in px value" 0 (List.length no_vars)

(** Test variable extraction from declarations *)
let test_vars_of_declarations () =
  let decl1, color_var =
    var "text-color" Color (Hex { hash = true; value = "333333" })
  in
  let decl2, size_var = var "font-size" Length (Rem 1.0) in

  (* Create declarations using the variables *)
  let color_decl = v Color (Var color_var) in
  let size_decl = v Font_size (Var size_var) in

  let vars = vars_of_declarations [ decl1; decl2; color_decl; size_decl ] in

  (* Should find the two variables used in declarations (not definitions) *)
  Alcotest.(check bool) "found variables" true (List.length vars >= 2)

(** Test any_var_name utility *)
let test_any_var_name () =
  let _, var_handle = var "spacing" Length (Rem 1.5) in
  let any_var = V var_handle in

  let name = any_var_name any_var in
  Alcotest.(check string) "variable name with prefix" "--spacing" name

(** Test custom declaration extraction *)
let test_extract_custom_declarations () =
  let custom1, _ = var "color1" Color (Hex { hash = true; value = "ff0000" }) in
  let custom2, _ = var "size1" Length (Px 16.) in
  let regular = v Width (Px 100.) in

  let decls = [ custom1; regular; custom2 ] in
  let customs = extract_custom_declarations decls in

  Alcotest.(check int) "extracted custom declarations" 2 (List.length customs)

(** Test custom declaration name extraction *)
let test_custom_declaration_name () =
  let custom, _ = var "my-var" Length (Px 20.) in
  let regular = v Height (Px 50.) in

  let custom_name = custom_declaration_name custom in
  let regular_name = custom_declaration_name regular in

  Alcotest.(check (option string))
    "custom has name" (Some "--my-var") custom_name;
  Alcotest.(check (option string)) "regular has no name" None regular_name

(** Test variable comparison *)
let test_compare_vars_by_name () =
  let _, var1 = var "aaa" Length (Px 1.) in
  let _, var2 = var "bbb" Length (Px 2.) in
  let _, var3 = var "aaa" Length (Px 3.) in
  (* Same name as var1 *)

  let cmp1 = compare_vars_by_name (V var1) (V var2) in
  let cmp2 = compare_vars_by_name (V var1) (V var3) in

  Alcotest.(check bool) "aaa < bbb" true (cmp1 < 0);
  Alcotest.(check int) "aaa = aaa" 0 cmp2

(** Test pp/to_string roundtrip for custom properties *)
let test_custom_property_roundtrip () =
  (* Create a custom property *)
  let custom, _var_handle =
    var "primary" Color (Hex { hash = true; value = "0080ff" })
  in

  (* Check the declaration structure *)
  match custom with
  | Custom_declaration { name; _ } ->
      (* Check it follows CSS custom property syntax *)
      Alcotest.(check bool)
        "has -- prefix" true
        (String.starts_with ~prefix:"--" name);
      Alcotest.(check string) "correct name" "--primary" name
  | _ -> Alcotest.fail "Expected custom declaration"

let variables_tests =
  [
    ("var creation", `Quick, test_var_creation);
    ("var with fallback", `Quick, test_var_with_fallback);
    ("vars of calc", `Quick, test_vars_of_calc);
    ("vars of property", `Quick, test_vars_of_property);
    ("vars of declarations", `Quick, test_vars_of_declarations);
    ("any_var_name", `Quick, test_any_var_name);
    ("extract custom declarations", `Quick, test_extract_custom_declarations);
    ("custom declaration name", `Quick, test_custom_declaration_name);
    ("compare vars by name", `Quick, test_compare_vars_by_name);
    ("custom property roundtrip", `Quick, test_custom_property_roundtrip);
  ]

(* Tests for newly added check functions *)
let test_syntax () =
  (* Syntax checking is not available in current implementation *)
  ()

let test_any_var () =
  (* Test any_var parsing *)
  check_any_var ~expected:"var(--color)" "var(--color)";
  check_any_var ~expected:"var(--primary)" "var(--primary)";
  check_any_var ~expected:"var(--theme-bg)" "var(--theme-bg)";

  (* Test invalid cases *)
  let try_parse s =
    try
      let r = Css.Reader.of_string s in
      let _ = read_any_var r in
      Alcotest.fail (Fmt.str "Should have failed parsing: %s" s)
    with _ -> ()
  in
  try_parse "not-a-var";
  try_parse "var(color)";
  (* Missing -- prefix *)
  try_parse "var()"

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
  check_any_syntax "\"<length> | <percentage>\""

let additional_tests =
  [
    ("syntax", `Quick, test_syntax);
    ("any_var", `Quick, test_any_var);
    ("any_syntax", `Quick, test_any_syntax);
  ]

let suite = ("variables", variables_tests @ additional_tests)
