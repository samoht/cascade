type baseline =
  | Snapshot_2024
  | Snapshot_2025
  | Snapshot_2026
  | Current_work
  | Experimental
  | Legacy
  | External

type row = {
  module_name : string;
  level : string;
  baseline : baseline;
  css_text_scope : bool;
  tests : string list;
  fuzzers : string list;
}

let row module_name level baseline tests fuzzers =
  { module_name; level; baseline; css_text_scope = true; tests; fuzzers }

let rows =
  [
    row "CSS Syntax" "3" Snapshot_2026
      [
        "test/test_lexer.ml";
        "test/test_parser.ml";
        "test/test_reader.ml";
        "test/test_component.ml";
        "test/spec/test.ml";
        "test/interop/wpt/test.ml";
      ]
      [ "fuzz/fuzz_lexer.ml"; "fuzz/fuzz_parser.ml"; "fuzz/fuzz_reader.ml" ];
    row "Selectors" "4" Snapshot_2026
      [ "test/test_selector.ml" ]
      [ "fuzz/fuzz_selector.ml" ];
    row "Values and Units" "3" Snapshot_2026
      [ "test/test_values.ml"; "test/test_properties.ml" ]
      [ "fuzz/fuzz_values.ml"; "fuzz/fuzz_properties.ml" ];
    row "Values and Units" "4" Current_work
      [ "test/test_values.ml"; "test/test_declaration.ml" ]
      [ "fuzz/fuzz_values.ml"; "fuzz/fuzz_properties.ml" ];
    row "Values and Units" "5" Experimental
      [ "test/test_values.ml"; "test/test_declaration.ml" ]
      [ "fuzz/fuzz_values.ml" ];
    row "CSS Color" "3" Snapshot_2026
      [ "test/test_values.ml"; "test/test_declaration.ml" ]
      [ "fuzz/fuzz_values.ml"; "fuzz/fuzz_properties.ml" ];
    row "CSS Color" "4" Snapshot_2026
      [ "test/test_values.ml"; "test/test_declaration.ml" ]
      [ "fuzz/fuzz_values.ml"; "fuzz/fuzz_properties.ml" ];
    row "CSS Color" "5" Experimental
      [ "test/test_values.ml"; "test/test_declaration.ml" ]
      [ "fuzz/fuzz_values.ml" ];
    row "Media Queries" "4" Snapshot_2026
      [ "test/test_media.ml"; "test/test_stylesheet.ml" ]
      [ "fuzz/fuzz_media.ml"; "fuzz/fuzz_stylesheet.ml" ];
    row "Media Queries" "5" Current_work
      [ "test/test_media.ml"; "test/test_stylesheet.ml" ]
      [ "fuzz/fuzz_media.ml"; "fuzz/fuzz_stylesheet.ml" ];
    row "Conditional Rules" "3" Snapshot_2026
      [ "test/test_supports.ml"; "test/test_stylesheet.ml" ]
      [ "fuzz/fuzz_supports.ml"; "fuzz/fuzz_stylesheet.ml" ];
    row "Conditional Rules" "4" Current_work
      [ "test/test_supports.ml"; "test/test_stylesheet.ml" ]
      [ "fuzz/fuzz_supports.ml"; "fuzz/fuzz_stylesheet.ml" ];
    row "CSS Nesting" "1" Current_work
      [ "test/test_stylesheet.ml" ]
      [ "fuzz/fuzz_stylesheet.ml" ];
    row "CSS Cascading and Inheritance" "5" Snapshot_2026
      [ "test/test_context.ml"; "test/test_stylesheet.ml" ]
      [ "fuzz/fuzz_context.ml"; "fuzz/fuzz_stylesheet.ml" ];
    row "CSS Cascading and Inheritance" "6" Current_work
      [ "test/test_context.ml"; "test/test_stylesheet.ml" ]
      [ "fuzz/fuzz_context.ml"; "fuzz/fuzz_stylesheet.ml" ];
    row "CSS Custom Properties" "1" Snapshot_2026
      [
        "test/test_variables.ml";
        "test/test_declaration.ml";
        "test/test_context.ml";
      ]
      [ "fuzz/fuzz_variables.ml"; "fuzz/fuzz_context.ml" ];
    row "CSS Properties and Values API" "1" Current_work
      [ "test/test_stylesheet.ml"; "test/test_variables.ml" ]
      [ "fuzz/fuzz_stylesheet.ml" ];
    row "CSS Containment" "3" Snapshot_2026
      [ "test/test_container.ml"; "test/test_stylesheet.ml" ]
      [ "fuzz/fuzz_container.ml"; "fuzz/fuzz_stylesheet.ml" ];
    row "CSS Fonts" "4" Snapshot_2026
      [ "test/test_declaration.ml"; "test/test_stylesheet.ml" ]
      [ "fuzz/fuzz_properties.ml"; "fuzz/fuzz_font_face.ml" ];
    row "CSS Fonts" "5" Current_work
      [ "test/test_declaration.ml"; "test/test_stylesheet.ml" ]
      [ "fuzz/fuzz_properties.ml"; "fuzz/fuzz_font_face.ml" ];
    row "CSS Backgrounds and Borders" "3" Snapshot_2026
      [ "test/test_declaration.ml"; "test/test_properties.ml" ]
      [ "fuzz/fuzz_properties.ml" ];
    row "CSS Box Model" "3" Snapshot_2026
      [ "test/test_declaration.ml"; "test/test_properties.ml" ]
      [ "fuzz/fuzz_properties.ml" ];
    row "CSS Sizing" "3" Snapshot_2026
      [ "test/test_declaration.ml"; "test/test_properties.ml" ]
      [ "fuzz/fuzz_properties.ml" ];
    row "CSS Display" "3" Snapshot_2026
      [ "test/test_declaration.ml"; "test/test_properties.ml" ]
      [ "fuzz/fuzz_properties.ml" ];
    row "CSS Positioned Layout" "3" Snapshot_2026
      [ "test/test_declaration.ml"; "test/test_properties.ml" ]
      [ "fuzz/fuzz_properties.ml" ];
    row "CSS Flexible Box Layout" "1" Snapshot_2026
      [ "test/test_declaration.ml"; "test/test_properties.ml" ]
      [ "fuzz/fuzz_properties.ml" ];
    row "CSS Grid Layout" "1/2" Snapshot_2026
      [ "test/test_declaration.ml"; "test/test_properties.ml" ]
      [ "fuzz/fuzz_properties.ml" ];
    row "CSS Box Alignment" "3" Snapshot_2026
      [ "test/test_declaration.ml"; "test/test_properties.ml" ]
      [ "fuzz/fuzz_properties.ml" ];
    row "CSS Logical Properties" "1" Snapshot_2026
      [ "test/test_declaration.ml"; "test/test_properties.ml" ]
      [ "fuzz/fuzz_properties.ml" ];
    row "CSS Transforms" "1/2" Snapshot_2026
      [ "test/test_declaration.ml"; "test/test_properties.ml" ]
      [ "fuzz/fuzz_properties.ml" ];
    row "CSS Transitions" "1/2" Snapshot_2026
      [ "test/test_declaration.ml"; "test/test_properties.ml" ]
      [ "fuzz/fuzz_properties.ml" ];
    row "CSS Animations" "1/2" Snapshot_2026
      [ "test/test_declaration.ml"; "test/test_stylesheet.ml" ]
      [ "fuzz/fuzz_properties.ml"; "fuzz/fuzz_keyframe.ml" ];
    row "CSS Scroll Snap" "1" Snapshot_2026
      [ "test/test_declaration.ml"; "test/test_properties.ml" ]
      [ "fuzz/fuzz_properties.ml" ];
    row "CSS Overflow" "3/4" Snapshot_2026
      [ "test/test_declaration.ml"; "test/test_properties.ml" ]
      [ "fuzz/fuzz_properties.ml" ];
    row "CSS Pseudo-Elements" "4" Snapshot_2026
      [ "test/test_selector.ml"; "test/test_stylesheet.ml" ]
      [ "fuzz/fuzz_selector.ml" ];
    row "CSS 2" "2.1/2.2" Legacy
      [ "test/spec/test.ml"; "test/test_declaration.ml" ]
      [ "fuzz/fuzz_css.ml"; "fuzz/fuzz_properties.ml" ];
  ]

let key row = row.module_name ^ "@" ^ row.level
let by_baseline baseline = List.filter (fun row -> row.baseline = baseline) rows
