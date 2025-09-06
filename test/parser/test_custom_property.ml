(** Tests for CSS Custom Property parsing *)

open Alcotest

let test_is_custom_property () =
  let test name expected =
    let result = Css.Declaration.is_custom_property name in
    check bool (Fmt.str "is_custom_property %s" name) expected result
  in

  test "--my-var" true;
  test "--color" true;
  test "--primary-color" true;
  test "--" false;
  (* Just -- without a name is invalid *)
  test "-my-var" false;
  (* Single dash *)
  test "my-var" false;
  (* No dashes *)
  test "color" false;
  (* Regular property *)
  test "background-color" false (* Regular property with dash *)

let test_read_var () =
  let test input expected_name expected_fallback =
    let t = Css.Reader.of_string input in
    try
      let name, fallback = Css_parser.Custom_property.read_var t in
      check string "var name" expected_name name;
      check (option string) "var fallback" expected_fallback fallback
    with Css.Reader.Parse_error (msg, _) ->
      fail (Fmt.str "Failed to parse %s: %s" input msg)
  in

  test "var(--color)" "--color" None;
  test "var(--primary-color)" "--primary-color" None;
  test "var(--spacing, 10px)" "--spacing" (Some "10px");
  test "var(--font, sans-serif)" "--font" (Some "sans-serif");
  test "var(--color, rgb(0, 0, 0))" "--color" (Some "rgb(0, 0, 0)")

let test_read_custom_property_name () =
  let test input expected =
    let t = Css.Reader.of_string input in
    try
      let result = Css_parser.Custom_property.read_custom_property_name t in
      check string "custom property name" expected result
    with Css.Reader.Parse_error (msg, _) ->
      fail (Fmt.str "Failed to parse %s: %s" input msg)
  in

  test "--my-var" "--my-var";
  test "--primary-color" "--primary-color";
  test "--spacing-1" "--spacing-1";
  test "--_private" "--_private"

let test_read_custom_property_value () =
  let test input desc =
    let t = Css.Reader.of_string input in
    try
      let result = Css_parser.Custom_property.read_custom_property_value t in
      check string (Fmt.str "custom property value: %s" desc) input result
    with Css.Reader.Parse_error (msg, _) ->
      fail (Fmt.str "Failed to parse %s: %s" input msg)
  in

  (* Simple values *)
  test "red" "color keyword";
  test "#ff0000" "hex color";
  test "10px" "length";
  test "1.5rem" "rem unit";
  test "100%" "percentage";

  (* Complex values *)
  test "10px 20px 30px" "multiple values";
  test "rgba(255, 0, 0, 0.5)" "function call";
  test "var(--other-var)" "variable reference";
  test "calc(100% - 20px)" "calc expression";
  test "linear-gradient(to right, red, blue)" "gradient";

  (* Quoted strings *)
  test "\"Hello World\"" "double quoted string";
  test "'Hello World'" "single quoted string";
  test "url('image.png')" "url with quotes";
  test "url(image.png)" "url without quotes"

let test_read_complex_values () =
  let test input desc =
    let t = Css.Reader.of_string input in
    let result = Css_parser.Custom_property.read_custom_property_value t in
    (* Just verify it parses without error and preserves the general
       structure *)
    check bool (Fmt.str "parses %s" desc) true (String.length result > 0)
  in

  (* Nested functions *)
  test "calc(var(--spacing) * 2)" "calc with var";
  test "rgb(calc(255 * 0.5), 0, 0)" "rgb with calc";

  (* Multiple parentheses levels *)
  test "calc((100% - 20px) / 2)" "nested calc";
  test "matrix(1, 0, 0, 1, 0, 0)" "matrix transform";

  (* Mixed content *)
  test "1px solid var(--border-color)" "mixed values with var";
  test "0 0 10px rgba(0, 0, 0, 0.5)" "shadow value"

let test_whitespace_trimming () =
  let test input expected desc =
    let t = Css.Reader.of_string input in
    let result = Css_parser.Custom_property.read_custom_property_value t in
    check string desc expected result
  in

  (* Leading/trailing whitespace should be trimmed, inner whitespace
     preserved *)
  test "  red  " "red" "trims leading/trailing spaces";
  test "10px  20px" "10px  20px" "preserves multiple inner spaces";
  test "10px\n20px" "10px\n20px" "preserves newlines";
  test "10px\t20px" "10px\t20px" "preserves tabs"

let tests =
  [
    ("is_custom_property", `Quick, test_is_custom_property);
    ("read_var", `Quick, test_read_var);
    ("read_custom_property_name", `Quick, test_read_custom_property_name);
    ("read_custom_property_value", `Quick, test_read_custom_property_value);
    ("read_complex_values", `Quick, test_read_complex_values);
    ("whitespace_handling", `Quick, test_whitespace_trimming);
  ]
