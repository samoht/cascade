(** CSS library tests *)

let () =
  Alcotest.run "css"
    (Test_css.suite @ Test_pp.suite @ Test_reader.suite @ Test_values.suite)
