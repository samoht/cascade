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
      Test_baseline.suite;
      Test_loc.suite;
      Test_token.suite;
      Test_lexer.suite;
      Test_component.suite;
      Test_parser.suite;
      Test_cursor.suite;
      Test_error.suite;
      Test_resolve.suite;
      Test_sort.suite;
      Test_rule_graph.suite;
      Test_rule_rewrite.suite;
      Test_rule_candidate.suite;
      Test_rule_scheduler.suite;
      Test_rule_order.suite;
      Test_order_maintenance.suite;
      Test_common.suite;
      Test_ctx.suite;
      Test_pool.suite;
      Test_loop.suite;
      Test_stats.suite;
      Test_gzip_size.suite;
      Test_edge.suite;
      Test_shorthand.suite;
      Test_merge.suite;
      Test_nest.suite;
      Test_size.suite;
      Test_rule.suite;
      Test_rule_index.suite;
      Test_apply.suite;
      Test_factor.suite;
      Test_block.suite;
      Test_cover.suite;
      Test_flatten.suite;
      Test_preflight.suite;
      Test_index.suite;
      Test_summary.suite;
      Test_factor_safe.suite;
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
