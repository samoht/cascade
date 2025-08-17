(** CSS library tests *)

let () = Alcotest.run "css" (Test_css.suite @ [ Test_version.suite ])
