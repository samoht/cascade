(** CSS library tests *)

(* Register exception printer for better Parse_error messages *)
let () =
  Printexc.register_printer (function
    | Css.Reader.Parse_error { message; position; context_window; callstack; _ }
      ->
        let callstack_str = String.concat " -> " callstack in
        Some
          (Printf.sprintf
             "Parse_error: %s at position %d\nContext: %s\nCallstack: %s"
             message position context_window callstack_str)
    | _ -> None)

let () =
  Alcotest.run "css"
    (Test_css.suite @ Test_pp.suite @ Test_reader.suite
    @ [
        Test_selector.suite;
        Test_values.suite;
        Test_declaration.suite;
        Test_properties.suite;
        Test_stylesheet.suite;
        Test_variables.suite;
        Test_optimize.suite;
      ])
