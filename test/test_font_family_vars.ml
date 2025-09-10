open Alcotest
open Tw

let test_font_family_custom_declaration () =
  let fonts =
    [
      Css.Properties.Ui_sans_serif;
      Css.Properties.System_ui;
      Css.Properties.Sans_serif;
      Css.Properties.Apple_color_emoji;
    ]
  in
  let decl =
    Css.Variables.var "--test-fonts" Css.Declaration.Font_family fonts
  in
  let css_output = Css.Declaration.string_of_value (fst decl) in
  let expected =
    "ui-sans-serif, system-ui, sans-serif, \"Apple Color Emoji\""
  in
  check string "font family list in custom declaration" expected css_output

let test_theme_font_sans_definition () =
  (* Test that font-sans theme variable has proper values *)
  let _decl, _var =
    Var.theme Var.Font_sans
      [
        Css.Properties.Ui_sans_serif;
        Css.Properties.System_ui;
        Css.Properties.Sans_serif;
      ]
  in
  let css_output = Css.Declaration.string_of_value _decl in
  let expected = "ui-sans-serif, system-ui, sans-serif" in
  check string "theme font-sans variable" expected css_output

let test_theme_font_mono_definition () =
  (* Test that font-mono theme variable has proper values *)
  let _decl, _var =
    Var.theme Var.Font_mono
      [ Css.Properties.Ui_monospace; Css.Properties.Monospace ]
  in
  let css_output = Css.Declaration.string_of_value _decl in
  let expected = "ui-monospace, monospace" in
  check string "theme font-mono variable" expected css_output

let suite =
  [
    test_case "font family custom declaration" `Quick
      test_font_family_custom_declaration;
    test_case "theme font-sans definition" `Quick
      test_theme_font_sans_definition;
    test_case "theme font-mono definition" `Quick
      test_theme_font_mono_definition;
  ]
