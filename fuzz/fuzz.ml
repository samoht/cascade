(** CSS parser fuzz tests — main entry point.

    Registers all fuzz test modules and runs them with Alcobar. *)

let () =
  Alcobar.run "css"
    [
      Fuzz_reader.suite;
      Fuzz_lexer.suite;
      Fuzz_parser.suite;
      Fuzz_cursor.suite;
      Fuzz_selector.suite;
      Fuzz_media.suite;
      Fuzz_container.suite;
      Fuzz_context.suite;
      Fuzz_values.suite;
      Fuzz_color_space.suite;
      Fuzz_properties.suite;
      Fuzz_declaration.suite;
      Fuzz_variables.suite;
      Fuzz_stylesheet.suite;
      Fuzz_optimize.suite;
      Fuzz_css.suite;
      Fuzz_supports.suite;
      Fuzz_font_face.suite;
      Fuzz_keyframe.suite;
    ]
