open Cmdliner

type mode = Auto | Tree | String | Canonical

let err_read path msg = Error (`Msg (Fmt.str "Error reading %s: %s" path msg))

let err_limit s =
  Error
    (`Msg
       (String.concat ""
          [
            "invalid limit \"";
            String.escaped s;
            "\": expected auto, none, or a positive integer";
          ]))

let read_source path =
  try Ok (Cli_io.read_source path) with Sys_error msg -> err_read path msg

let no_color_var = "NO_COLOR"

let no_color_env =
  Cmd.Env.info no_color_var
    ~doc:
      "When set to a non-empty value, disable colour output (see \
       https://no-color.org/). Overrides $(b,--color) and $(b,CASCADE_COLOR)."

let resolve_style_renderer style_renderer =
  match Sys.getenv_opt no_color_var with
  | Some s when s <> "" -> Some `None
  | _ -> style_renderer

let run_diff mode ~lossless ~prune_unused_custom_props ~css1 ~css2 =
  let mode =
    match mode with
    | Auto -> `Auto
    | Tree -> `Tree
    | String -> `String
    | Canonical -> `Canonical
  in
  Cascade_diff.Css_compare.diff ~mode ~lossless ~prune_unused_custom_props css1
    css2

(* How many top-level differences the report names. [Fit_entries] lets the
   automatic shaping decide, [All_entries] names every one, [Count n] names
   exactly [n]. *)
type limit = Fit_entries | All_entries | Count of int

(* A report past this many lines has stopped summarising and started dumping, so
   [Fit_entries] narrows it to the most it can show and still be read. *)
let auto_line_budget = 40

(* Every entry costs at least the line naming it, so no report fits more entries
   than the budget has lines. *)
let max_probe_entries = auto_line_budget

let count_lines s =
  let n = ref 0 in
  String.iter (fun c -> if c = '\n' then incr n) s;
  !n

(* Parse warnings shown before the rest is counted, across both sides. *)
let auto_warning_budget = 3

(* [none] bounds nothing, warnings included. Every other setting keeps the
   report short, which a wall of warnings ahead of it would undo. *)
let warning_budget = function
  | All_entries -> None
  | Fit_entries | Count _ -> Some auto_warning_budget

(* A declaration the reader refuses is dropped from that side's AST, so nothing
   it holds reaches the verdict. The typed value readers raise [Bad_value] at
   [Sort.Component] for exactly that refusal. The other kinds name material the
   parse kept - an unknown at-rule and its block reach the output, an
   unterminated block is closed with its contents - or a failure above the
   declaration. *)
let unreadable_declaration (w : Cascade.Error.t) =
  match w.kind with
  | Cascade.Error.Bad_value _ ->
      Cascade.Sort.equal w.sort Cascade.Sort.Component
  | Sort_mismatch _ | Unexpected_token _ | Missing_token _ | Bad_selector _
  | Bad_condition _ | Unknown_at_rule _ | Unterminated _ ->
      false

(* A rule the reader refuses goes the same way, and takes every declaration and
   nested rule it holds. Each kind below was checked by running [cascade fmt] on
   an input that raises it and looking for the construct in the output. An
   unterminated qualified rule, a prelude read as a declaration, an at-rule
   condition the grammar refuses and a lexeme the reader needed and did not find
   all leave nothing behind. An unterminated block is closed with its contents.
   [Bad_value] at [Sort.Component] is the declaration counted above; elsewhere
   it, [Bad_selector] and [Unknown_at_rule] report on material the parse
   kept. *)
let unreadable_rule (w : Cascade.Error.t) =
  match w.kind with
  | Cascade.Error.Unterminated _ ->
      not (Cascade.Sort.equal w.sort Cascade.Sort.Block)
  | Sort_mismatch _ | Unexpected_token _ | Missing_token _ | Bad_condition _ ->
      true
  | Bad_value _ | Bad_selector _ | Unknown_at_rule _ -> false

(* Counted per side, because the question is whether either input hid something
   from the comparison, not how many the pair hid between them. *)
type sides = { expected : int; actual : int }

(* Declarations and rules are counted apart, because a consumer reads the two to
   tell which of them its sheet lost. *)
type unread = { declarations : sides; rules : sides }

let count_sides p (result : Cascade_diff.Css_compare.t) =
  let count ws = List.length (List.filter p ws) in
  {
    expected = count result.expected_warnings;
    actual = count result.actual_warnings;
  }

let unread_of result =
  {
    declarations = count_sides unreadable_declaration result;
    rules = count_sides unreadable_rule result;
  }

let has_sides s = s.expected > 0 || s.actual > 0
let has_unread u = has_sides u.declarations || has_sides u.rules

let render_sides ~what ~file1 ~file2 s =
  if not (has_sides s) then ""
  else
    String.concat ""
      [
        "Unreadable ";
        what;
        ": ";
        file1;
        " ";
        string_of_int s.expected;
        ", ";
        file2;
        " ";
        string_of_int s.actual;
        "\n";
      ]

let render_unread ~file1 ~file2 u =
  String.concat ""
    [
      render_sides ~what:"declarations" ~file1 ~file2 u.declarations;
      render_sides ~what:"rules" ~file1 ~file2 u.rules;
    ]

let render_diff ~color ~file1 ~file2 ~entries result =
  let buf = Buffer.create 1024 in
  Cascade_diff.Css_compare.pp_diff ~expected:file1 ~actual:file2 ~color ?entries
    buf result;
  Buffer.contents buf

let render_warnings ~file1 ~file2 ~max result =
  let buf = Buffer.create 256 in
  Cascade_diff.Css_compare.pp_warnings ~expected:file1 ~actual:file2 ?max buf
    result;
  if Cascade_diff.Css_compare.has_warnings result then Buffer.add_char buf '\n';
  Buffer.contents buf

let fits body = count_lines body <= auto_line_budget

(* Most entries whose report still fits, by bisection: an entry only ever adds
   lines, so a count that overflows bounds every larger one. The floor is one
   entry, however tall: one difference shown whole and a count of the rest is
   the worst case worth printing, not every difference with its body cut. *)
let fit_entries render =
  let rec go low high best =
    if low > high then best
    else
      let mid = low + ((high - low) / 2) in
      let body = render mid in
      if fits body then go (mid + 1) high (mid, body) else go low (mid - 1) best
  in
  go 2 max_probe_entries (1, render 1)

let render_report ~color ~file1 ~file2 ~limit result =
  let render entries = render_diff ~color ~file1 ~file2 ~entries result in
  match limit with
  | All_entries -> (render None, None)
  | Count n -> (render (Some n), None)
  | Fit_entries ->
      let full = render None in
      if fits full then (full, None)
      else
        let n, body = fit_entries (fun n -> render (Some n)) in
        (body, Some n)

(* Canonical mode compares the two canonical minified forms, so the text under a
   string diff there is those forms and not the files as written. Say which. *)
let canonical_forms_note mode result =
  match (mode, result) with
  | Canonical, Cascade_diff.Css_compare.String_diff _ ->
      "Canonical forms differ:\n"
  | _ -> ""

let print_diff_report ~color ~file1 ~file2 ~css1 ~css2 ~limit ~mode result =
  let stats =
    Cascade_diff.Css_compare.stats ~expected_str:css1 ~actual_str:css2 result
  in
  let max_warnings = warning_budget limit in
  let buf = Buffer.create 1024 in
  Cascade_diff.Css_compare.pp_stats buf stats;
  Buffer.add_string buf
    (canonical_forms_note mode result.Cascade_diff.Css_compare.result);
  Buffer.add_char buf '\n';
  Buffer.add_string buf (render_warnings ~file1 ~file2 ~max:max_warnings result);
  let body, elided = render_report ~color ~file1 ~file2 ~limit result in
  Buffer.add_string buf body;
  Buffer.add_char buf '\n';
  (* A shortened report says how far it was cut and how to release it. *)
  (match elided with
  | None -> ()
  | Some n ->
      List.iter (Buffer.add_string buf)
        [
          "(limit ";
          string_of_int n;
          "; use --limit=none for the full report)\n";
        ]);
  print_string (Buffer.contents buf)

(* The JSON document. It reports the whole comparison, so the shaping the human
   report needs (a line budget, colour) has nothing to bound here: a consumer
   reads the field it wants and ignores the rest. *)

module J = Cli_json
module D = Cascade_diff.Tree_diff

let json_string s = J.String s
let json_strings ss = J.List (List.map json_string ss)

(* A member the constructor carries only sometimes is written only when it is
   there, rather than as a null a reader would have to skip. *)
let json_opt name f = function None -> [] | Some v -> [ (name, f v) ]

let json_mode = function
  | Auto -> "auto"
  | Tree -> "tree"
  | String -> "string"
  | Canonical -> "canonical"

let json_container_type = function
  | `Media -> "media"
  | `Layer -> "layer"
  | `Supports -> "supports"
  | `Container -> "container"
  | `Property -> "property"
  | `Nesting -> "nesting"
  | `At_rule -> "at_rule"

(* The author's own spelling, with the flag the human report also shows: a
   declaration and the one it outranks read alike without it. *)
let json_declaration decl =
  let value = Cascade.Css.declaration_value ~minify:false decl in
  let value =
    if Cascade.Css.declaration_is_important decl then
      String.concat "" [ value; " !important" ]
    else value
  in
  J.Obj
    [
      ("property", json_string (Cascade.Css.declaration_name decl));
      ("value", json_string value);
    ]

let json_declarations decls = J.List (List.map json_declaration decls)

let json_property (property, value) =
  J.Obj [ ("property", json_string property); ("value", json_string value) ]

let json_property_change { D.property_name; expected_value; actual_value } =
  J.Obj
    [
      ("property_name", json_string property_name);
      ("expected_value", json_string expected_value);
      ("actual_value", json_string actual_value);
    ]

(* A container's contents are CSS, not a difference, so they are written as the
   minified text of each statement. *)
let json_statements stmts =
  J.List
    (List.map
       (fun stmt ->
         json_string
           (Cascade.Css.to_string ~minify:true (Cascade.Css.v [ stmt ])))
       stmts)

(* The three kinds that carry a selector and the declarations under it. *)
let json_selector_rule selector declarations =
  [
    ("selector", json_string selector);
    ("declarations", json_declarations declarations);
  ]

let json_content_changed ~selector ~old_declarations ~new_declarations
    ~property_changes ~added_properties ~removed_properties =
  [
    ("selector", json_string selector);
    ("old_declarations", json_declarations old_declarations);
    ("new_declarations", json_declarations new_declarations);
    ("property_changes", J.List (List.map json_property_change property_changes));
    ("added_properties", J.List (List.map json_property added_properties));
    ("removed_properties", J.List (List.map json_property removed_properties));
  ]

let json_reordered ~selector ~expected_pos ~actual_pos ~swapped_with
    ~old_declarations ~new_declarations =
  [
    ("selector", json_string selector);
    ("expected_pos", J.Int expected_pos);
    ("actual_pos", J.Int actual_pos);
  ]
  @ json_opt "swapped_with" json_string swapped_with
  @ json_opt "old_declarations" json_declarations old_declarations
  @ json_opt "new_declarations" json_declarations new_declarations

let json_rule_diff (rule : D.rule_diff) =
  let entry kind fields = J.Obj (("kind", json_string kind) :: fields) in
  match rule with
  | Added { selector; declarations } ->
      entry "added" (json_selector_rule selector declarations)
  | Removed { selector; declarations } ->
      entry "removed" (json_selector_rule selector declarations)
  | Content_changed
      {
        selector;
        old_declarations;
        new_declarations;
        property_changes;
        added_properties;
        removed_properties;
      } ->
      entry "modified"
        (json_content_changed ~selector ~old_declarations ~new_declarations
           ~property_changes ~added_properties ~removed_properties)
  | Selector_changed { old_selector; new_selector; declarations } ->
      entry "selector_changed"
        [
          ("old_selector", json_string old_selector);
          ("new_selector", json_string new_selector);
          ("declarations", json_declarations declarations);
        ]
  | Reordered
      {
        selector;
        expected_pos;
        actual_pos;
        swapped_with;
        old_declarations;
        new_declarations;
      } ->
      entry "reordered"
        (json_reordered ~selector ~expected_pos ~actual_pos ~swapped_with
           ~old_declarations ~new_declarations)
  | Rearranged { selector; declarations } ->
      entry "rearranged" (json_selector_rule selector declarations)
  | Regrouped { from_selectors; to_selectors } ->
      entry "regrouped"
        [
          ("from_selectors", json_strings from_selectors);
          ("to_selectors", json_strings to_selectors);
        ]

let json_container_info { D.container_type; condition; rules } =
  [
    ("container_type", json_string (json_container_type container_type));
    ("condition", json_string condition);
    ("rules", json_statements rules);
  ]

let json_blocks blocks =
  J.List
    (List.map
       (fun (position, rules) ->
         J.Obj
           [ ("position", J.Int position); ("rules", json_statements rules) ])
       blocks)

let rec json_container_diff (container : D.container_diff) =
  let entry change fields =
    J.Obj
      (("kind", json_string "container")
      :: ("change", json_string change)
      :: fields)
  in
  match container with
  | Added info -> entry "added" (json_container_info info)
  | Removed info -> entry "removed" (json_container_info info)
  | Modified { info; actual_rules; rule_changes; container_changes } ->
      entry "modified"
        (json_container_info info
        @ [
            ("actual_rules", json_statements actual_rules);
            ( "changes",
              J.List
                (List.map json_rule_diff rule_changes
                @ List.map json_container_diff container_changes) );
          ])
  | Reordered { info; expected_pos; actual_pos } ->
      entry "reordered"
        (json_container_info info
        @ [
            ("expected_pos", J.Int expected_pos);
            ("actual_pos", J.Int actual_pos);
          ])
  | Block_structure_changed
      { container_type; condition; expected_blocks; actual_blocks } ->
      entry "block_structure_changed"
        [
          ("container_type", json_string (json_container_type container_type));
          ("condition", json_string condition);
          ("expected_blocks", json_blocks expected_blocks);
          ("actual_blocks", json_blocks actual_blocks);
        ]

let json_layer_order { D.expected_order; actual_order; swapped } =
  J.Obj
    [
      ("kind", json_string "layer_order");
      ("expected_order", json_strings expected_order);
      ("actual_order", json_strings actual_order);
      ( "swapped",
        J.List
          (List.map
             (fun (weaker, stronger) ->
               J.Obj
                 [
                   ("weaker", json_string weaker);
                   ("stronger", json_string stronger);
                 ])
             swapped) );
    ]

let json_changes (d : D.t) =
  Option.to_list (Option.map json_layer_order d.layer_order)
  @ List.map json_rule_diff d.rules
  @ List.map json_container_diff d.containers

(* Every field of the record but the two texts, under the names the record
   already uses, so the type and the document stay in step. *)
let json_stats (s : Cascade_diff.Css_compare.stats) =
  J.Obj
    [
      ("expected_chars", J.Int s.expected_chars);
      ("actual_chars", J.Int s.actual_chars);
      ("added_rules", J.Int s.added_rules);
      ("removed_rules", J.Int s.removed_rules);
      ("modified_rules", J.Int s.modified_rules);
      ("reordered_rules", J.Int s.reordered_rules);
      ("rearranged_rules", J.Int s.rearranged_rules);
      ("regrouped_rules", J.Int s.regrouped_rules);
      ("container_changes", J.Int s.container_changes);
      ("layer_order_swaps", J.Int s.layer_order_swaps);
    ]

let json_string_diff (sdiff : Cascade_diff.String_diff.t) =
  let expected_line, actual_line = sdiff.diff_lines in
  J.Obj
    [
      ("position", J.Int sdiff.position);
      ("line_expected", J.Int sdiff.line_expected);
      ("column_expected", J.Int sdiff.column_expected);
      ("line_actual", J.Int sdiff.line_actual);
      ("column_actual", J.Int sdiff.column_actual);
      ( "diff_lines",
        J.Obj
          [
            ("expected", json_string expected_line);
            ("actual", json_string actual_line);
          ] );
    ]

let json_side side message =
  J.Obj [ ("side", json_string side); ("message", json_string message) ]

(* A parse error is the reason there is nothing to compare on that side, so it
   belongs in the document rather than beside it: a consumer that only reads
   stdout would otherwise see an empty comparison and no cause. Both sides
   failing the same way is one fact about the input, as it is for a warning. *)
let json_errors = function
  | Cascade_diff.Css_compare.Both_errors (e1, e2) ->
      let m1 = Cascade.Error.to_string e1 in
      let m2 = Cascade.Error.to_string e2 in
      if String.equal m1 m2 then [ json_side "both" m1 ]
      else [ json_side "expected" m1; json_side "actual" m2 ]
  | Expected_error e -> [ json_side "expected" (Cascade.Error.to_string e) ]
  | Actual_error e -> [ json_side "actual" (Cascade.Error.to_string e) ]
  | No_diff | Tree_diff _ | String_diff _ -> []

(* The library groups the warnings; this only names the sides. *)
let json_warnings (result : Cascade_diff.Css_compare.t) =
  let side = function
    | Cascade_diff.Css_compare.Expected -> "expected"
    | Actual -> "actual"
    | Both -> "both"
  in
  J.List
    (List.map
       (fun (s, w) -> json_side (side s) (Cascade.Error.to_string w))
       (Cascade_diff.Css_compare.warnings result))

(* The counts a consumer reads to assert that nothing was hidden from the
   comparison, beside the verdict they qualify. *)
let json_sides s =
  J.Obj [ ("expected", J.Int s.expected); ("actual", J.Int s.actual) ]

let json_document ~file1 ~file2 ~mode ~css1 ~css2 ~unread result =
  let stats =
    Cascade_diff.Css_compare.stats ~expected_str:css1 ~actual_str:css2 result
  in
  let outcome = result.Cascade_diff.Css_compare.result in
  (* A declaration or a rule the reader refuses is dropped from both sides, so a
     comparison that found no difference has not shown the two files to be
     identical. *)
  let identical =
    match outcome with
    | No_diff -> not (has_unread unread)
    | Tree_diff _ | String_diff _ | Both_errors _ | Expected_error _
    | Actual_error _ ->
        false
  in
  let changes = match outcome with Tree_diff d -> json_changes d | _ -> [] in
  let string_diff =
    match outcome with
    | String_diff sdiff -> [ ("string_diff", json_string_diff sdiff) ]
    | _ -> []
  in
  J.Obj
    ([
       ("expected", json_string file1);
       ("actual", json_string file2);
       ("mode", json_string (json_mode mode));
       ("identical", J.Bool identical);
       ("unreadable_declarations", json_sides unread.declarations);
       ("unreadable_rules", json_sides unread.rules);
       ("stats", json_stats stats);
       ("warnings", json_warnings result);
       ("errors", J.List (json_errors outcome));
       ("changes", J.List changes);
     ]
    @ string_diff)

type canonical_opts = { lossless : bool; prune_unused_custom_props : bool }

let print_json_report ~file1 ~file2 ~mode ~css1 ~css2 ~opts =
  let result =
    run_diff mode ~lossless:opts.lossless
      ~prune_unused_custom_props:opts.prune_unused_custom_props ~css1 ~css2
  in
  let unread = unread_of result in
  let document = json_document ~file1 ~file2 ~mode ~css1 ~css2 ~unread result in
  print_string (Cli_json.to_string document);
  print_newline ();
  match result.Cascade_diff.Css_compare.result with
  | No_diff ->
      if has_unread unread then Stdlib.exit Cli_exit.cannot_determine else Ok ()
  | Tree_diff _ | String_diff _ | Both_errors _ | Expected_error _
  | Actual_error _ ->
      Stdlib.exit 1

(* Each side is its text and the name the report gives it, which is the path for
   a file and [<stdin>] for the stream. *)
let compare_sources ~color ~mode ~limit ~json ~opts (css1, file1) (css2, file2)
    =
  if json then print_json_report ~file1 ~file2 ~mode ~css1 ~css2 ~opts
  else if css1 = css2 then (
    Fmt.pr "CSS files are identical@.";
    Ok ())
  else
    let result =
      run_diff mode ~lossless:opts.lossless
        ~prune_unused_custom_props:opts.prune_unused_custom_props ~css1 ~css2
    in
    match result.Cascade_diff.Css_compare.result with
    | No_diff -> (
        (* Equal ASTs can still hide what the parse dropped; show the warnings
           so the equality verdict is honest about them. *)
        let max = warning_budget limit in
        print_string (render_warnings ~file1 ~file2 ~max result);
        let unread = unread_of result in
        match has_unread unread with
        | false ->
            Fmt.pr "CSS files are identical@.";
            Ok ()
        | true ->
            (* The comparison saw no difference and never saw what the parse
               dropped either, so identity is not a verdict it can give. *)
            print_string (render_unread ~file1 ~file2 unread);
            Fmt.pr "Cannot determine whether the CSS files are identical@.";
            Stdlib.exit Cli_exit.cannot_determine)
    | String_diff _ | Tree_diff _ | Both_errors _ | Expected_error _
    | Actual_error _ ->
        print_diff_report ~color ~file1 ~file2 ~css1 ~css2 ~limit ~mode result;
        (* Differing inputs are a result, not a usage error: exit 1 as
           documented, distinct from cmdliner's reserved error codes. *)
        Stdlib.exit 1

let compare_files file1 file2 style_renderer mode limit json opts () =
  Fmt_tty.setup_std_outputs
    ?style_renderer:(resolve_style_renderer style_renderer)
    ();
  (* The report is built in a plain buffer, so the diff printers cannot see the
     tty; resolve the colour decision Fmt_tty just made (tty detection, --color,
     CASCADE_COLOR, NO_COLOR) and pass it down. *)
  let color =
    match Fmt.style_renderer Fmt.stdout with
    | `Ansi_tty -> true
    | `None -> false
  in
  (* The stream is read once and cannot be rewound, so [-] on both sides has no
     second side to compare the first against. *)
  if file1 = "-" && file2 = "-" then
    Error (`Msg "cannot compare standard input with itself")
  else
    match (read_source file1, read_source file2) with
    | Ok source1, Ok source2 ->
        compare_sources ~color ~mode ~limit ~json ~opts source1 source2
    | Error e, _ | _, Error e -> Error e

let file1_arg =
  let doc = "First CSS file to compare (expected/reference; use - for stdin)" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE1" ~doc)

let file2_arg =
  let doc = "Second CSS file to compare (actual/test; use - for stdin)" in
  Arg.(required & pos 1 (some file) None & info [] ~docv:"FILE2" ~doc)

let mode_arg =
  let doc =
    "Diff mode: 'auto' (smart detection), 'tree' (force structural diff), \
     'string' (force string diff), or 'canonical' (independently optimize and \
     minify both inputs with the same canonical settings, then compare the \
     resulting bytes)"
  in
  let mode_conv =
    Arg.enum
      [
        ("auto", Auto);
        ("tree", Tree);
        ("string", String);
        ("canonical", Canonical);
      ]
  in
  Arg.(value & opt mode_conv Auto & info [ "diff" ] ~docv:"MODE" ~doc)

let limit_arg =
  let doc =
    "How many top-level differences to print: $(b,auto) (default) prints them \
     all when the report is short and otherwise keeps as many as stay \
     readable, each one whole and never fewer than one, $(b,none) prints every \
     one and every parse warning with it, and an integer prints exactly that \
     many. Wherever differences are left over, a $(b,... N more differences) \
     line records how many. Applies to the whole report, so a bound of $(b,1) \
     is one top-level entry however many sections the report has. Ignored \
     under $(b,--json), which replaces the report with a document naming every \
     difference."
  in
  let parse = function
    | "auto" -> Ok Fit_entries
    | "none" -> Ok All_entries
    | s -> (
        match int_of_string_opt s with
        | Some n when n >= 1 -> Ok (Count n)
        | Some _ | None -> err_limit s)
  in
  let print ppf = function
    | Fit_entries -> Fmt.string ppf "auto"
    | All_entries -> Fmt.string ppf "none"
    | Count n -> Fmt.int ppf n
  in
  Arg.(
    value
    & opt (conv ~docv:"LIMIT" (parse, print)) Fit_entries
    & info [ "limit" ] ~docv:"LIMIT" ~doc)

let json_arg =
  let doc =
    "Write the comparison as one JSON document on standard output, in place of \
     the human report, so a harness can branch on the exit status and read the \
     detail from the document. The exit status is unchanged. Nothing else \
     reaches standard output: parse warnings and parse errors are members of \
     the document rather than lines beside it. Colour and $(b,--limit) shape \
     the human report only and are ignored here, since the document reports \
     every difference."
  in
  Arg.(value & flag & info [ "json" ] ~doc)

let lossless_arg =
  let doc =
    "Disable bounded colour and numeric approximation in $(b,--diff=canonical) \
     canonicalisation. Exact rewrites still run, but repeating static numeric \
     arithmetic stays as calc(), static modern colour-space and color-mix() \
     values stay functional, and colour channels keep their full precision. \
     Two stylesheets that differ only within an approximation budget then \
     report as different rather than collapsing to equal. Has no effect \
     outside $(b,--diff=canonical)."
  in
  Arg.(value & flag & info [ "lossless" ] ~doc)

let prune_unused_custom_props_arg =
  let doc =
    "Drop custom-property bindings referenced by nothing on both sides before \
     comparing in $(b,--diff=canonical), so two stylesheets that differ only \
     by a dead binding compare equal. Makes the comparison blind to \
     dead-custom-property divergences (a render-no-op); enable only when that \
     difference is immaterial. Has no effect outside $(b,--diff=canonical)."
  in
  Arg.(value & flag & info [ "prune-unused-custom-props" ] ~doc)

let term =
  let open Term in
  let style_renderer_with_env =
    Fmt_cli.style_renderer
      ~env:
        (Cmd.Env.info "CASCADE_COLOR"
           ~doc:
             "Set to $(b,auto), $(b,always), or $(b,never) to control colour \
              output, like $(b,--color) (overridden by $(b,NO_COLOR)).")
      ()
  in
  let canonical_opts =
    const (fun lossless prune_unused_custom_props ->
        { lossless; prune_unused_custom_props })
    $ lossless_arg $ prune_unused_custom_props_arg
  in
  term_result
    (const compare_files $ file1_arg $ file2_arg $ style_renderer_with_env
   $ mode_arg $ limit_arg $ json_arg $ canonical_opts $ Cli_log.term)

let man =
  [
    `S Manpage.s_description;
    `P
      "$(tname) compares two CSS files and reports structural differences \
       using a tree-based diff format with syntax highlighting.";
    `P "The comparison parses both CSS files and detects:";
    `I ("-", "Added, removed, or modified rules");
    `I ("-", "Property value changes");
    `I ("-", "Reordered rules");
    `I ("-", "Changes in @media, @layer, and other at-rules");
    `S Manpage.s_examples;
    `P "Compare two CSS files:";
    `Pre "  cascade diff reference.css output.css";
    `P "Compare a stylesheet on standard input against a reference:";
    `Pre "  cascade fmt --minify src.css | cascade diff reference.css -";
    `P "Read the comparison as JSON instead of the report:";
    `Pre "  cascade diff --json reference.css output.css";
    `P "Disable colors using flag:";
    `Pre "  cascade diff --color=never reference.css output.css";
    `P "Disable colors using NO_COLOR environment variable:";
    `Pre "  NO_COLOR=1 cascade diff reference.css output.css";
  ]

let cmd =
  let doc = "Compare two CSS files with structural analysis" in
  let exits =
    Cli_exit.with_defaults
      [
        Cmd.Exit.info
          ~doc:
            "if the CSS files are identical and cascade read every declaration \
             and every rule in them"
          0;
        Cmd.Exit.info
          ~doc:
            "if the CSS files differ. Under $(b,--diff=canonical) that is any \
             difference between their canonical forms, whether or not the \
             structural walk reached it. A known difference outranks anything \
             cascade could not read, so this status wins over 2"
          1;
        Cmd.Exit.info
          ~doc:
            "if the comparison found no difference and cascade could not read \
             a declaration or a rule one of the files holds. The reader drops \
             it from both sides, so the comparison never sees it and cannot \
             call the two files identical. The report and the $(b,--json) \
             document count declarations and rules per side"
          Cli_exit.cannot_determine;
        Cmd.Exit.info ~doc:"on command-line errors or unreadable input files"
          124;
      ]
  in
  Cmd.v (Cmd.info "diff" ~doc ~man ~envs:[ no_color_env ] ~exits) term
