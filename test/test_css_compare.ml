(** Tests for Cascade_diff.Css_compare module *)

open Css_compare_helpers

let contains_substring s needle =
  let len = String.length s in
  let needle_len = String.length needle in
  let rec loop i =
    if needle_len = 0 then true
    else if i + needle_len > len then false
    else if String.sub s i needle_len = needle then true
    else loop (i + 1)
  in
  loop 0

(* ===== equal tests ===== *)

let equal_identical () =
  let css = ".a { color: red }" in
  Alcotest.(check bool)
    "identical CSS compares equal" true
    (Cascade_diff.Css_compare.equal css css)

let equal_different () =
  Alcotest.(check bool)
    "different CSS not equal" false
    (Cascade_diff.Css_compare.equal ".a { color: red }" ".a { color: blue }")

let equal_empty () =
  Alcotest.(check bool)
    "empty strings equal" true
    (Cascade_diff.Css_compare.equal "" "")

let equal_prune_unused_custom_props () =
  let with_bind = ":root{--spacing:.25rem}.top-0{top:0}" in
  let without = ".top-0{top:0}" in
  Alcotest.(check bool)
    "default: a dead binding is a real difference" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical with_bind without);
  Alcotest.(check bool)
    "opt-in: a dead binding compares equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ~prune_unused_custom_props:true with_bind without);
  Alcotest.(check bool)
    "opt-in: a referenced binding still differs" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ~prune_unused_custom_props:true
       ":root{--spacing:.25rem}.gap{gap:var(--spacing)}"
       ".gap{gap:var(--spacing)}")

let equal_whitespace_difference () =
  (* Structurally same but different formatting - default mode does not collapse
     formatting, so this returns false. *)
  let css1 = ".a { color: red }" in
  let css2 = ".a{color:red}" in
  Alcotest.(check bool)
    "whitespace-different CSS not equal under default mode" false
    (Cascade_diff.Css_compare.equal css1 css2)

(* ===== diff returning No_diff ===== *)

let diff_no_diff () =
  let css = ".a { color: red }" in
  let result = Cascade_diff.Css_compare.diff css css in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.No_diff -> ()
  | _ -> Alcotest.fail "expected No_diff"

let diff_no_diff_empty () =
  let result = Cascade_diff.Css_compare.diff "" "" in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.No_diff -> ()
  | _ -> Alcotest.fail "expected No_diff for empty strings"

(* ===== diff returning Tree_diff ===== *)

let diff_tree_diff () =
  let expected = ".a { color: red }" in
  let actual = ".a { color: blue }" in
  let result = Cascade_diff.Css_compare.diff expected actual in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.Tree_diff d ->
      Alcotest.(check bool)
        "tree diff is not empty" false
        (Cascade_diff.Tree_diff.is_empty d)
  | _ -> Alcotest.fail "expected Tree_diff"

let diff_tree_diff_added_rule () =
  let expected = ".a { color: red }" in
  let actual = ".a { color: red } .b { margin: 0 }" in
  let result = Cascade_diff.Css_compare.diff expected actual in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.Tree_diff _ -> ()
  | _ -> Alcotest.fail "expected Tree_diff for added rule"

(* ===== diff returning String_diff ===== *)

let diff_string_diff () =
  (* To trigger String_diff, we need structurally identical CSS that differs
     only in formatting. This is tricky because the parser normalizes things. We
     use diff with `String to force it. *)
  let expected = ".a { color: red }" in
  let actual = ".a  { color: red }" in
  let result = Cascade_diff.Css_compare.diff ~mode:`String expected actual in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.String_diff d ->
      Alcotest.(check bool) "string diff has position" true (d.position >= 0)
  | Cascade_diff.Css_compare.No_diff ->
      (* Strings might be considered equal after strip_tool_header/trim *)
      ()
  | _ -> Alcotest.fail "expected String_diff or No_diff"

(* ===== diff returning parse errors ===== *)

let diff_actual_error () =
  let expected = ".a { color: red }" in
  let actual = ".a { color: }" in
  let result = Cascade_diff.Css_compare.diff expected actual in
  (* The parser may or may not error on this - just check it doesn't crash *)
  ignore result

let diff_expected_error () =
  let expected = ".a { color: }" in
  let actual = ".a { color: red }" in
  let result = Cascade_diff.Css_compare.diff expected actual in
  ignore result

(* ===== parse warnings surfaced in diff reports ===== *)

(* When one side's declaration is rejected by the value parser, the declaration
   is dropped from the AST and the structural diff alone reads as a phantom
   addition on the side that parsed. The rendered report must surface the parse
   warning so the reader sees the real cause. *)
let diff_surfaces_parse_warnings () =
  let expected = ".a { color: red; scroll-snap-align: corner }" in
  let actual = ".a { color: red; scroll-snap-align: start }" in
  let result = Cascade_diff.Css_compare.diff expected actual in
  let buf = Buffer.create 256 in
  Cascade_diff.Css_compare.pp buf result;
  let rendered = Buffer.contents buf in
  Alcotest.(check bool)
    "report mentions the rejected value" true
    (contains_substring rendered "corner");
  Alcotest.(check bool)
    "report labels the parse warning" true
    (contains_substring rendered "parse warning")

let diff_surfaces_parse_warnings_same_ast () =
  (* Both sides parse to the same AST once the bad declaration is dropped; the
     report must still surface the warning instead of presenting the difference
     as purely textual. *)
  let expected = ".a { scroll-snap-align: corner }" in
  let actual = ".a { }" in
  let result = Cascade_diff.Css_compare.diff expected actual in
  let buf = Buffer.create 256 in
  Cascade_diff.Css_compare.pp buf result;
  let rendered = Buffer.contents buf in
  Alcotest.(check bool)
    "report labels the parse warning" true
    (contains_substring rendered "parse warning")

(* ===== strip_tool_header tests ===== *)

let strip_tool_header_no_header () =
  let css = ".a { color: red }" in
  Alcotest.(check string)
    "no header unchanged" css
    (Cascade_diff.Css_compare.strip_tool_header css)

let strip_tool_header_with_header () =
  let css = "/*! Generated by tool v1.0 */\n.a { color: red }" in
  let result = Cascade_diff.Css_compare.strip_tool_header css in
  Alcotest.(check string) "header stripped" ".a { color: red }" result

let strip_tool_header_regular_comment () =
  let css = "/* regular comment */\n.a { color: red }" in
  let result = Cascade_diff.Css_compare.strip_tool_header css in
  (* Regular comments (not /*!) should NOT be stripped *)
  Alcotest.(check string)
    "regular comment preserved" "/* regular comment */\n.a { color: red }"
    result

let strip_tool_header_empty () =
  Alcotest.(check string)
    "empty string" ""
    (Cascade_diff.Css_compare.strip_tool_header "")

let strip_tool_header_only_header () =
  let css = "/*! header only */" in
  let result = Cascade_diff.Css_compare.strip_tool_header css in
  Alcotest.(check string) "only header stripped" "" result

(* A move the report names has to be one the cascade can see. Two rules that
   both write [display:none] read the same in either order, and the projection
   finds them equal on their own; a rule only one sheet has, writing [content]
   before them, must not turn their order into a reported move. *)
let stats_neutral_move_beside_a_difference () =
  let neutral_pair =
    ".h:not(:where(.d,.d *)):not(:where(.s,.s *)){display:none}"
  and media_twin =
    "@media not (prefers-color-scheme:dark){.h:not(:where(.d,.d \
     *)){display:none}}"
  and content_rule = ".c:after{content:var(--t);--t:none;content:none}" in
  let expected =
    ".p:after{content:var(--t)}" ^ content_rule ^ neutral_pair ^ media_twin
  and actual = content_rule ^ media_twin ^ neutral_pair in
  let result = Cascade_diff.Css_compare.diff ~mode:`Canonical expected actual in
  let s =
    Cascade_diff.Css_compare.stats ~expected_str:expected ~actual_str:actual
      result
  in
  Alcotest.(check bool)
    "the extra rule is a difference" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual);
  Alcotest.(check int) "no neutral move is reported" 0 s.reordered_rules

(* ===== stats tests ===== *)

let stats_no_diff () =
  let css = ".a { color: red }" in
  let result = Cascade_diff.Css_compare.diff css css in
  let s =
    Cascade_diff.Css_compare.stats ~expected_str:css ~actual_str:css result
  in
  Alcotest.(check int) "expected chars" (String.length css) s.expected_chars;
  Alcotest.(check int) "actual chars" (String.length css) s.actual_chars;
  Alcotest.(check int) "added rules" 0 s.added_rules;
  Alcotest.(check int) "removed rules" 0 s.removed_rules;
  Alcotest.(check int) "modified rules" 0 s.modified_rules;
  Alcotest.(check int) "reordered rules" 0 s.reordered_rules;
  Alcotest.(check int) "container changes" 0 s.container_changes

let stats_with_tree_diff () =
  let expected = ".a { color: red }" in
  let actual = ".a { color: red } .b { margin: 0 }" in
  let result = Cascade_diff.Css_compare.diff expected actual in
  let s =
    Cascade_diff.Css_compare.stats ~expected_str:expected ~actual_str:actual
      result
  in
  Alcotest.(check int)
    "expected chars" (String.length expected) s.expected_chars;
  Alcotest.(check int) "actual chars" (String.length actual) s.actual_chars;
  (* With a tree diff, at least one of added/removed/modified should be > 0 *)
  let total = s.added_rules + s.removed_rules + s.modified_rules in
  Alcotest.(check bool) "has rule changes" true (total > 0)

let pp_stats_does_not_crash () =
  let css = ".a { color: red }" in
  let result = Cascade_diff.Css_compare.diff css css in
  let s =
    Cascade_diff.Css_compare.stats ~expected_str:css ~actual_str:css result
  in
  let buf = Buffer.create 256 in
  Cascade_diff.Css_compare.pp_stats buf s;
  let output = Buffer.contents buf in
  Alcotest.(check bool)
    "pp_stats produces output" true
    (String.length output > 0)

(* The counters only ever count a tree diff, so they read zero on every result
   that never reached one: a comparison that fell through to strings, and a side
   whose content the parser discarded. Announcing that as an absence of
   structural differences states a conclusion the comparator never drew. *)

let rendered_stats ?mode expected actual =
  let result = Cascade_diff.Css_compare.diff ?mode expected actual in
  let s =
    Cascade_diff.Css_compare.stats ~expected_str:expected ~actual_str:actual
      result
  in
  let buf = Buffer.create 256 in
  Cascade_diff.Css_compare.pp_stats buf s;
  Buffer.contents buf

let zero_counters_do_not_claim_equivalence () =
  (* The stray braces are dropped with a parse warning, so both sides reach the
     same AST and the report falls through to a string diff. *)
  let output = rendered_stats "a{color:red}" "a{color:red}}}}" in
  Alcotest.(check bool)
    "does not claim the two sheets match structurally" false
    (contains_substring output "No structural differences");
  Alcotest.(check bool)
    "says the changes were not classified" true
    (contains_substring output "none classified structurally")

let zero_counters_unparsed_do_not_claim_equivalence () =
  (* Mode [`String] never parses, so the counters read zero over two sheets that
     paint differently. *)
  let output = rendered_stats ~mode:`String "a{color:red}" "a{color:blue}" in
  Alcotest.(check bool)
    "a comparison that never parsed is not an absence of differences" false
    (contains_substring output "No structural differences")

(* The canonical minified form is the verdict in [`Canonical] mode: two sheets
   whose canonical bytes differ differ, whether or not the tree diff reached the
   divergence. Here a layer-order pin that is the only declaration of its layer,
   so CSS Cascade 5 sec. 6.4.3 has it order [a] before [b] and the projection
   keeps it; the tree diff walks past a statement that carries no rules. *)
let canonical_byte_residual_is_a_difference () =
  let pinned = "@layer a;@layer b{x{top:0}}" in
  let unpinned = "@layer b{x{top:0}}" in
  let result = Cascade_diff.Css_compare.diff ~mode:`Canonical pinned unpinned in
  (match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.String_diff _ -> ()
  | _ ->
      Alcotest.fail
        "expected a string diff of the two differing canonical forms");
  Alcotest.(check bool)
    "the equality answer follows the bytes" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical pinned unpinned)

let canonical_byte_equal_is_no_diff () =
  (* The converse: once the two sheets reach one canonical form, the spelling
     they started from is immaterial and the verdict is equality. [equal] is
     [true] only on {!No_diff}, so it pins the constructor too. *)
  let spaced = ".x { color: red }" in
  let tight = ".x{color:red}" in
  Alcotest.(check bool)
    "the equality answer follows the bytes" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical spaced tight);
  Alcotest.(check bool)
    "no tree diff is reported over one canonical form" true
    (Option.is_none
       (Cascade_diff.Css_compare.as_tree_diff
          (Cascade_diff.Css_compare.diff ~mode:`Canonical spaced tight)))

(* ===== diff tests ===== *)

let diff_auto () =
  let expected = ".a { color: red }" in
  let actual = ".a { color: blue }" in
  let result = Cascade_diff.Css_compare.diff ~mode:`Auto expected actual in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.Tree_diff _ -> ()
  | _ -> Alcotest.fail "expected Tree_diff in auto mode"

let diff_tree () =
  let expected = ".a { color: red }" in
  let actual = ".a { color: blue }" in
  let result = Cascade_diff.Css_compare.diff ~mode:`Tree expected actual in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.Tree_diff _ -> ()
  | _ -> Alcotest.fail "expected Tree_diff in tree mode"

let diff_string () =
  let expected = ".a { color: red }" in
  let actual = ".a { color: blue }" in
  let result = Cascade_diff.Css_compare.diff ~mode:`String expected actual in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.String_diff _ -> ()
  | Cascade_diff.Css_compare.No_diff -> ()
  | _ -> Alcotest.fail "expected String_diff or No_diff in string mode"

let diff_string_identical () =
  let css = ".a { color: red }" in
  let result = Cascade_diff.Css_compare.diff ~mode:`String css css in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.No_diff -> ()
  | _ -> Alcotest.fail "expected No_diff for identical in string mode"

let semantic_color_mix_mode () =
  let expected = ".a { color: " ^ color_mix_transparent ^ " }" in
  let actual = ".a { color: " ^ color_mix_transparent_hex ^ " }" in
  let result = Cascade_diff.Css_compare.diff ~mode:`Canonical expected actual in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.No_diff -> ()
  | _ -> Alcotest.fail "expected No_diff for semantically equivalent CSS"

let diff_canonical_uses_outputs () =
  let expected =
    "@unknown { color: red } .a { -webkit-tap-highlight-color: transparent; \
     color: red }"
  in
  let actual =
    "@unknown{color:red}.a{-webkit-tap-highlight-color:#0000;color:blue}"
  in
  let result = Cascade_diff.Css_compare.diff ~mode:`Canonical expected actual in
  match result.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.Tree_diff _ ->
      let buf = Buffer.create 256 in
      Cascade_diff.Css_compare.pp buf result;
      let rendered = Buffer.contents buf in
      Alcotest.(check bool)
        "normalized diff does not report transparent alias" false
        (contains_substring rendered "-webkit-tap-highlight-color");
      Alcotest.(check bool)
        "normalized diff reports remaining color change" true
        (contains_substring rendered "color:")
  | _ -> Alcotest.fail "expected canonical diff on normalized outputs"

(* ===== as_tree_diff tests ===== *)

let as_tree_diff_with_tree () =
  let expected = ".a { color: red }" in
  let actual = ".a { color: blue }" in
  let result = Cascade_diff.Css_compare.diff expected actual in
  Alcotest.(check bool)
    "as_tree_diff returns Some for Tree_diff" true
    (Option.is_some (Cascade_diff.Css_compare.as_tree_diff result))

let as_tree_diff_no_diff () =
  let css = ".a { color: red }" in
  let result = Cascade_diff.Css_compare.diff css css in
  Alcotest.(check bool)
    "as_tree_diff returns None for No_diff" true
    (Option.is_none (Cascade_diff.Css_compare.as_tree_diff result))

(* ===== pp tests ===== *)

let pp_does_not_crash () =
  let expected = ".a { color: red }" in
  let actual = ".a { color: blue }" in
  let result = Cascade_diff.Css_compare.diff expected actual in
  let buf = Buffer.create 256 in
  Cascade_diff.Css_compare.pp buf result;
  (* Just verify it doesn't crash *)
  ignore (Buffer.contents buf)

(* ===== @import diffs ===== *)

(* Two @import rules that differ in target, media, or order are
   cascade-significant differences, not equal, in both the structural Tree mode
   and the semantic Canonical mode. *)
let import_tree_url () =
  Alcotest.(check bool)
    "different @import targets differ (tree)" false
    (equal ~mode:`Tree "@import url(a.css);" "@import url(b.css);");
  Alcotest.(check bool)
    "identical @import equal (tree)" true
    (equal ~mode:`Tree "@import url(a.css);" "@import url(a.css);")

let import_tree_media () =
  Alcotest.(check bool)
    "different @import media differ (tree)" false
    (equal ~mode:`Tree "@import url(a.css) screen;" "@import url(a.css) print;")

let import_tree_order () =
  Alcotest.(check bool)
    "swapped @import order differs (tree)" false
    (equal ~mode:`Tree "@import url(a.css);@import url(b.css);"
       "@import url(b.css);@import url(a.css);")

let import_canonical_media () =
  Alcotest.(check bool)
    "different @import media differ (canonical)" false
    (equal ~mode:`Canonical "@import url(a.css) screen;"
       "@import url(a.css) print;");
  Alcotest.(check bool)
    "identical @import equal (canonical)" true
    (equal ~mode:`Canonical "@import url(a.css);" "@import url(a.css);")

(* Two same-selector rules with conflicting declarations cascade last-wins, so
   swapping their order flips the winner: a real difference the structural Tree
   mode must report, not only Auto/Canonical. *)
let tree_same_selector_reorder () =
  Alcotest.(check bool)
    "same-selector reorder flips the cascade winner (tree)" false
    (equal ~mode:`Tree ".a{color:red}.a{color:blue}"
       ".a{color:blue}.a{color:red}");
  Alcotest.(check bool)
    "same order stays equal (tree)" true
    (equal ~mode:`Tree ".a{color:red}.a{color:blue}"
       ".a{color:red}.a{color:blue}")

(* ===== Suite ===== *)

(* ===== Warning locations ===== *)

(* A warning names the input it came from and its place in that file, not in the
   text left once the tool banner and leading blank lines are stripped. CSS 2
   sec. 9.5.1 gives [float] [left], [right] or [none], so [center] is invalid;
   CSS Syntax 3 (ED) sec. 3.3 counts each LF as one line, so after a one-line
   banner and a blank line the rule opens line 3, and [center] starts at its
   tenth code point. *)
let warning_located_in_the_file () =
  let expected = "/*! tool v1 */\n\n.x{float:center}" in
  let actual = ".x{float:left}" in
  let d =
    Cascade_diff.Css_compare.diff ~expected_filename:"a.css"
      ~actual_filename:"b.css" expected actual
  in
  match d.expected_warnings with
  | [ w ] ->
      Alcotest.(check (option string))
        "names the input" (Some "a.css") w.Cascade.Error.filename;
      Alcotest.(check (option (pair int int)))
        "place in the file"
        (Some (3, 10))
        (Cascade.Error.position w)
  | ws ->
      Alcotest.failf "expected one warning on the expected side, got %d"
        (List.length ws)

let suite =
  ( "css_compare",
    [
      Alcotest.test_case "import tree url" `Quick import_tree_url;
      Alcotest.test_case "warning located in the file" `Quick
        warning_located_in_the_file;
      Alcotest.test_case "import tree media" `Quick import_tree_media;
      Alcotest.test_case "import tree order" `Quick import_tree_order;
      Alcotest.test_case "import canonical media" `Quick import_canonical_media;
      Alcotest.test_case "tree same-selector reorder" `Quick
        tree_same_selector_reorder;
      Alcotest.test_case "equal identical" `Quick equal_identical;
      Alcotest.test_case "equal different" `Quick equal_different;
      Alcotest.test_case "equal empty" `Quick equal_empty;
      Alcotest.test_case "equal whitespace difference" `Quick
        equal_whitespace_difference;
      Alcotest.test_case "diff No_diff" `Quick diff_no_diff;
      Alcotest.test_case "diff No_diff empty" `Quick diff_no_diff_empty;
      Alcotest.test_case "diff Tree_diff" `Quick diff_tree_diff;
      Alcotest.test_case "diff Tree_diff added rule" `Quick
        diff_tree_diff_added_rule;
      Alcotest.test_case "diff String_diff" `Quick diff_string_diff;
      Alcotest.test_case "diff actual error" `Quick diff_actual_error;
      Alcotest.test_case "diff expected error" `Quick diff_expected_error;
      Alcotest.test_case "diff surfaces parse warnings" `Quick
        diff_surfaces_parse_warnings;
      Alcotest.test_case "diff surfaces parse warnings on same AST" `Quick
        diff_surfaces_parse_warnings_same_ast;
      Alcotest.test_case "strip_tool_header no header" `Quick
        strip_tool_header_no_header;
      Alcotest.test_case "strip_tool_header with header" `Quick
        strip_tool_header_with_header;
      Alcotest.test_case "strip_tool_header regular comment" `Quick
        strip_tool_header_regular_comment;
      Alcotest.test_case "strip_tool_header empty" `Quick
        strip_tool_header_empty;
      Alcotest.test_case "strip_tool_header only header" `Quick
        strip_tool_header_only_header;
      Alcotest.test_case "stats no diff" `Quick stats_no_diff;
      Alcotest.test_case "stats neutral move beside a difference" `Quick
        stats_neutral_move_beside_a_difference;
      Alcotest.test_case "stats with tree diff" `Quick stats_with_tree_diff;
      Alcotest.test_case "pp_stats does not crash" `Quick
        pp_stats_does_not_crash;
      Alcotest.test_case "zero counters do not claim equivalence" `Quick
        zero_counters_do_not_claim_equivalence;
      Alcotest.test_case "zero counters unparsed do not claim equivalence"
        `Quick zero_counters_unparsed_do_not_claim_equivalence;
      Alcotest.test_case "canonical byte residual is a difference" `Quick
        canonical_byte_residual_is_a_difference;
      Alcotest.test_case "canonical byte equality is no diff" `Quick
        canonical_byte_equal_is_no_diff;
      Alcotest.test_case "diff auto" `Quick diff_auto;
      Alcotest.test_case "diff tree" `Quick diff_tree;
      Alcotest.test_case "diff string" `Quick diff_string;
      Alcotest.test_case "diff string identical" `Quick diff_string_identical;
      Alcotest.test_case "diff semantic color-mix" `Quick
        semantic_color_mix_mode;
      Alcotest.test_case "diff canonical uses outputs" `Quick
        diff_canonical_uses_outputs;
      Alcotest.test_case "as_tree_diff with tree" `Quick as_tree_diff_with_tree;
      Alcotest.test_case "as_tree_diff with no diff" `Quick as_tree_diff_no_diff;
      Alcotest.test_case "pp does not crash" `Quick pp_does_not_crash;
      Alcotest.test_case "opt-in prune-unused-custom-props" `Quick
        equal_prune_unused_custom_props;
    ] )
