open Cascade

let decl_t : Css.Declaration.declaration Alcotest.testable =
  Alcotest.testable
    (fun fmt d -> Format.pp_print_string fmt (Css.Declaration.to_string d))
    ( = )

let stylesheet_t : Css.Stylesheet.t Alcotest.testable =
  Alcotest.testable
    (fun fmt sheet ->
      Format.pp_print_string fmt (Css.Stylesheet.to_string ~minify:true sheet))
    ( = )

let matches pattern text =
  let re = Re.Perl.compile_pat pattern in
  Re.execp re text

let px value = Css.Media.Length (Css.Values.Px value)
let rem value = Css.Media.Length (Css.Values.Rem value)
let ident value = Css.Media.Ident (Css.Media.ident_of_string value)
let feature = Css.Media.feature
let container_feature = Css.Container.feature

let declaration_with_value decl value =
  Css.Declaration.of_string
    (Css.Declaration.property_name decl
    ^ ": " ^ value
    ^ if Css.Declaration.is_important decl then " !important" else "")

let check_eval_preserves name ~ctx input =
  let decl = Css.Declaration.of_string input in
  Alcotest.(check decl_t) name decl (Css.eval_declaration ctx decl)

let check_eval name ~ctx ?layer_order ?layer ~expected input =
  let decl = Css.Declaration.of_string input in
  let expected = Css.Declaration.of_string expected in
  let actual = Css.eval_declaration ctx ?layer_order ?layer decl in
  Alcotest.(check decl_t) name expected actual

let check_eval_idempotent name ~ctx ?layer_order ?layer input =
  let decl = Css.Declaration.of_string input in
  let once = Css.eval_declaration ctx ?layer_order ?layer decl in
  let twice = Css.eval_declaration ctx ?layer_order ?layer once in
  Alcotest.(check decl_t) name once twice

let check_eval_context_extension name ?layer_order ?layer ~weak_ctx ~strong_ctx
    ~expected input =
  let decl = Css.Declaration.of_string input in
  let expected = Css.Declaration.of_string expected in
  let direct = Css.eval_declaration strong_ctx ?layer_order ?layer decl in
  let staged =
    Css.eval_declaration strong_ctx ?layer_order ?layer
      (Css.eval_declaration weak_ctx ?layer_order ?layer decl)
  in
  Alcotest.(check decl_t) (name ^ " direct") expected direct;
  Alcotest.(check decl_t) (name ^ " staged") direct staged

let stylesheet_of_string input =
  let cursor = Cursor.of_string input in
  try Css.Stylesheet.read cursor
  with Cursor.Parse_error err ->
    Alcotest.failf "expected stylesheet to parse: %s" (Error.to_string err)

let check_eval_stylesheet name ~ctx ?layer_order ?layer ~expected input =
  let expected = stylesheet_of_string expected in
  let actual =
    Css.eval_stylesheet ctx ?layer_order ?layer (stylesheet_of_string input)
  in
  Alcotest.(check stylesheet_t) name expected actual
