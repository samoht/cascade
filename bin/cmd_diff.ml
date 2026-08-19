open Cmdliner

type mode = Auto | Tree | String | Canonical

let err_read path msg = Error (`Msg (Fmt.str "Error reading %s: %s" path msg))

let err_depth s =
  Error
    (`Msg
       (Fmt.str "invalid depth %S: expected auto, max, or a positive integer" s))

let read_file path =
  try Ok (Cli_io.read_file path) with Sys_error msg -> err_read path msg

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

type depth = Fit | Full | Level of int

(* A report past this many lines has stopped summarising and started dumping, so
   [Auto] drops to the deepest level that still fits. *)
let auto_line_budget = 40

(* The probe renders the report once per level, and the diff tree is as deep as
   the stylesheet nests. Stop at a level that summarises any report worth
   summarising; [--depth=max] is the answer for the rest. *)
let max_probe_depth = 5

let count_lines s =
  let n = ref 0 in
  String.iter (fun c -> if c = '\n' then incr n) s;
  !n

(* Parse warnings shown per side before the rest is counted. *)
let auto_warning_budget = 3

let render_diff ~color ~file1 ~file2 ~depth result =
  let buf = Buffer.create 1024 in
  Cascade_diff.Css_compare.pp_diff ~expected:file1 ~actual:file2 ~color ?depth
    buf result;
  Buffer.contents buf

let render_warnings ~file1 ~file2 ~max result =
  let buf = Buffer.create 256 in
  Cascade_diff.Css_compare.pp_warnings ~expected:file1 ~actual:file2 ?max buf
    result;
  if Cascade_diff.Css_compare.has_warnings result then Buffer.add_char buf '\n';
  Buffer.contents buf

(* Deepest level whose report still fits the budget, or level 1 when even the
   roots overflow: the roots are the one thing always worth printing. *)
let fit_depth render =
  let rec go level best =
    if level > max_probe_depth then best
    else
      let body = render (Some level) in
      if count_lines body <= auto_line_budget then go (level + 1) (level, body)
      else best
  in
  go 2 (1, render (Some 1))

let render_at_depth ~color ~file1 ~file2 ~depth result =
  let render depth = render_diff ~color ~file1 ~file2 ~depth result in
  match depth with
  | Full -> (render None, None)
  | Level n -> (render (Some n), None)
  | Fit ->
      let full = render None in
      if count_lines full <= auto_line_budget then (full, None)
      else
        let level, body = fit_depth render in
        (body, Some level)

(* Canonical mode compares the two canonical minified forms, so the text under a
   string diff there is those forms and not the files as written. Say which. *)
let canonical_forms_note mode result =
  match (mode, result) with
  | Canonical, Cascade_diff.Css_compare.String_diff _ ->
      "Canonical forms differ:\n"
  | _ -> ""

let print_diff_report ~color ~file1 ~file2 ~css1 ~css2 ~depth ~mode result =
  let stats =
    Cascade_diff.Css_compare.stats ~expected_str:css1 ~actual_str:css2 result
  in
  let max_warnings =
    match depth with Full -> None | Fit | Level _ -> Some auto_warning_budget
  in
  let buf = Buffer.create 1024 in
  Cascade_diff.Css_compare.pp_stats buf stats;
  Buffer.add_string buf
    (canonical_forms_note mode result.Cascade_diff.Css_compare.result);
  Buffer.add_char buf '\n';
  Buffer.add_string buf (render_warnings ~file1 ~file2 ~max:max_warnings result);
  let body, elided_at = render_at_depth ~color ~file1 ~file2 ~depth result in
  Buffer.add_string buf body;
  Buffer.add_char buf '\n';
  (match elided_at with
  | None -> ()
  | Some level ->
      List.iter (Buffer.add_string buf)
        [
          "(depth ";
          string_of_int level;
          "; use --depth=max for the full report)\n";
        ]);
  print_string (Buffer.contents buf)

type canonical_opts = { lossless : bool; prune_unused_custom_props : bool }

let compare_files file1 file2 style_renderer mode depth opts () =
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
  match (read_file file1, read_file file2) with
  | Ok css1, Ok css2 -> (
      if css1 = css2 then (
        Fmt.pr "CSS files are identical@.";
        Ok ())
      else
        let result =
          run_diff mode ~lossless:opts.lossless
            ~prune_unused_custom_props:opts.prune_unused_custom_props ~css1
            ~css2
        in
        match result.Cascade_diff.Css_compare.result with
        | No_diff ->
            (* Equal ASTs can still hide parse-dropped declarations; show the
               warnings so the equality verdict is honest about them. *)
            let max =
              match depth with
              | Full -> None
              | Fit | Level _ -> Some auto_warning_budget
            in
            print_string (render_warnings ~file1 ~file2 ~max result);
            Fmt.pr "CSS files are identical@.";
            Ok ()
        | String_diff _ | Tree_diff _ | Both_errors _ | Expected_error _
        | Actual_error _ ->
            print_diff_report ~color ~file1 ~file2 ~css1 ~css2 ~depth ~mode
              result;
            (* Differing inputs are a result, not a usage error: exit 1 as
               documented, distinct from cmdliner's reserved error codes. *)
            Stdlib.exit 1)
  | Error e, _ | _, Error e -> Error e

let file1_arg =
  let doc = "First CSS file to compare (expected/reference)" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE1" ~doc)

let file2_arg =
  let doc = "Second CSS file to compare (actual/test)" in
  Arg.(required & pos 1 (some file) None & info [] ~docv:"FILE2" ~doc)

let mode_arg =
  let doc =
    "Diff mode: 'auto' (smart detection), 'tree' (force structural diff), \
     'string' (force string diff), or 'canonical' (compare optimized canonical \
     minified serialization)"
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

let depth_arg =
  let doc =
    "How many levels of the difference tree to print: $(b,auto) (default) \
     prints the whole tree when it is short and otherwise falls back to the \
     deepest level that stays readable, $(b,max) always prints it in full, and \
     an integer pins an exact level ($(b,1) is the top-level entries alone). \
     Wherever children are cut off, a $(b,... N more lines) marker records how \
     much is hidden."
  in
  let parse = function
    | "auto" -> Ok Fit
    | "max" | "full" -> Ok Full
    | s -> (
        match int_of_string_opt s with
        | Some n when n >= 1 -> Ok (Level n)
        | Some _ | None -> err_depth s)
  in
  let print ppf = function
    | Fit -> Fmt.string ppf "auto"
    | Full -> Fmt.string ppf "max"
    | Level n -> Fmt.int ppf n
  in
  Arg.(
    value
    & opt (conv ~docv:"DEPTH" (parse, print)) Fit
    & info [ "depth" ] ~docv:"DEPTH" ~doc)

let lossless_arg =
  let doc =
    "Disable colour approximation in $(b,--diff=canonical) canonicalisation. \
     Exact colour canonicalisation still runs, but static modern colour-space \
     and color-mix() values stay functional and channels keep their full \
     precision. Two stylesheets that only differ by colours folded within the \
     approximation budget then report as different rather than collapsing to \
     equal. Has no effect outside $(b,--diff=canonical)."
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
   $ mode_arg $ depth_arg $ canonical_opts $ Cli_log.term)

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
    `S Manpage.s_options;
    `P "The --diff option controls the comparison mode:";
    `I
      ( "--diff=auto",
        "Smart detection: use tree diff for structural changes, string diff \
         otherwise (default)" );
    `I
      ( "--diff=tree",
        "Force structural tree-based diff (useful for debugging CSS parser \
         behavior)" );
    `I
      ( "--diff=string",
        "Force character-by-character string diff (faster, less intelligent)" );
    `I
      ( "--diff=canonical",
        "Compare canonical minified CSS before reporting a diff" );
    `I
      ( "--depth=auto|max|N",
        "Levels of the difference tree to print. $(b,auto) (default) prints it \
         whole while it stays short, then falls back to the deepest level that \
         fits; $(b,max) always prints it whole; an integer pins a level. Cut \
         subtrees are marked with the number of lines hidden." );
    `I
      ( "--lossless",
        "Disable colour approximation under $(b,--diff=canonical): colour \
         channels keep their authored precision and static modern colour-space \
         values stay functional. No effect outside canonical mode." );
    `S Manpage.s_exit_status;
    `P "$(tname) exits with:";
    `I ("0", "if the CSS files are identical");
    `I
      ( "1",
        "if the CSS files differ. Under $(b,--diff=canonical) that is any \
         difference between their canonical forms, whether or not the \
         structural walk reached it" );
    `I ("124", "on command-line errors or unreadable input files");
    `S Manpage.s_examples;
    `P "Compare two CSS files:";
    `Pre "  cascade diff reference.css output.css";
    `P "Disable colors using flag:";
    `Pre "  cascade diff --color=never reference.css output.css";
    `P "Disable colors using NO_COLOR environment variable:";
    `Pre "  NO_COLOR=1 cascade diff reference.css output.css";
  ]

let cmd =
  let doc = "Compare two CSS files with structural analysis" in
  Cmd.v (Cmd.info "diff" ~doc ~man ~envs:[ no_color_env ]) term
