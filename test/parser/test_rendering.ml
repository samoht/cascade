(** Tests for CSS Rendering & Optimization parsing *)

open Alcotest

let test_unimplemented_features () =
  let expect_error test_name f =
    try
      f ();
      fail (test_name ^ " should have failed with not implemented error")
    with
    | Css_parser.Reader.Parse_error _ -> ()
    | exn ->
        fail
          (test_name ^ " failed with unexpected exception: "
         ^ Printexc.to_string exn)
  in

  (* Test that unimplemented rendering features fail clearly *)
  expect_error "analyze_declarations" (fun () ->
      let t = Css_parser.Reader.of_string "/* test */" in
      ignore (Css_parser.Rendering.analyze_declarations t));

  expect_error "extract_custom_declarations" (fun () ->
      let t = Css_parser.Reader.of_string ".test { --var: value; }" in
      ignore (Css_parser.Rendering.extract_custom_declarations t));

  expect_error "optimize" (fun () ->
      let t = Css_parser.Reader.of_string ".a{color:red} .b{color:red}" in
      ignore (Css_parser.Rendering.optimize t));

  expect_error "stylesheet_rules" (fun () ->
      let t = Css_parser.Reader.of_string ".test { color: red; }" in
      ignore (Css_parser.Rendering.stylesheet_rules t));

  expect_error "deduplicate_declarations" (fun () ->
      let t = Css_parser.Reader.of_string "color: red; color: blue;" in
      ignore (Css_parser.Rendering.deduplicate_declarations t));

  expect_error "inline_style_of_declarations" (fun () ->
      let t = Css_parser.Reader.of_string "color: red; margin: 10px;" in
      ignore (Css_parser.Rendering.inline_style_of_declarations t))

let tests =
  [
    test_case "unimplemented rendering features fail clearly" `Quick
      test_unimplemented_features;
  ]
