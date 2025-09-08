(** CSS library tests *)

let () =
  Alcotest.run "css"
    (Test_css.suite @ Test_pp.suite @ Test_reader.suite @ Test_selector.suite
   @ Test_values.suite @ Test_declaration.suite @ Test_properties.suite
   @ Test_stylesheet.suite)
