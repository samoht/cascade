(** CSS parser fuzz tests — main entry point.

    Registers all fuzz test modules and runs them with Alcobar. *)

let () =
  Alcobar.run "css"
    [
      Fuzz_reader.suite;
      Fuzz_selector.suite;
      Fuzz_values.suite;
      Fuzz_stylesheet.suite;
      Fuzz_supports.suite;
      Fuzz_font_face.suite;
      Fuzz_keyframe.suite;
    ]
