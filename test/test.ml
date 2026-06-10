(** CSS library tests *)

open Cascade

(* Register exception printer for better Parse_error messages *)
let () =
  Printexc.register_printer (function
    | Reader.Parse_error { message; position; context_window; callstack; _ } ->
        let callstack_str = String.concat " -> " callstack in
        Fmt.kstr
          (fun s -> Some s)
          "Parse_error: %s at position %d\nContext: %s\nCallstack: %s" message
          position context_window callstack_str
    | _ -> None)

let () =
  Alcotest.run "css"
    [
      Test_css.suite;
      Test_loc.suite;
      Test_token.suite;
      Test_lexer.suite;
      Test_component.suite;
      Test_parser.suite;
      Test_cursor.suite;
      Test_error.suite;
      Test_sort.suite;
      Test_order_maintenance.suite;
      Test_rule_pool.suite;
      Test_weighted_interval.suite;
      Test_css_graph.suite;
      Test_rule_merge.suite;
      Test_pp.suite;
      Test_syntax.suite;
      Test_context.suite;
      Test_reader.suite;
      Test_selector.suite;
      Test_selector_summary.suite;
      Test_aria.suite;
      Test_values.suite;
      Test_color_space.suite;
      Test_declaration.suite;
      Test_properties.suite;
      Test_stylesheet.suite;
      Test_variables.suite;
      Test_inline.suite;
      Test_optimize.suite;
      Test_font_face.suite;
      Test_keyframe.suite;
      Test_media.suite;
      Test_supports.suite;
      Test_container.suite;
      Test_string_diff.suite;
      Test_tree_diff.suite;
      Test_css_compare.suite;
    ]
