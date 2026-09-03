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

(* Parse warnings shown per side before the rest is counted. *)
let auto_warning_budget = 3

(* [none] bounds nothing, warnings included. Every other setting keeps the
   report short, which a wall of warnings ahead of it would undo. *)
let warning_budget = function
  | All_entries -> None
  | Fit_entries | Count _ -> Some auto_warning_budget

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

type canonical_opts = { lossless : bool; prune_unused_custom_props : bool }

let compare_files file1 file2 style_renderer mode limit opts () =
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
            let max = warning_budget limit in
            print_string (render_warnings ~file1 ~file2 ~max result);
            Fmt.pr "CSS files are identical@.";
            Ok ()
        | String_diff _ | Tree_diff _ | Both_errors _ | Expected_error _
        | Actual_error _ ->
            print_diff_report ~color ~file1 ~file2 ~css1 ~css2 ~limit ~mode
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
     is one top-level entry however many sections the report has."
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
   $ mode_arg $ limit_arg $ canonical_opts $ Cli_log.term)

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
        Cmd.Exit.info ~doc:"if the CSS files are identical" 0;
        Cmd.Exit.info
          ~doc:
            "if the CSS files differ. Under $(b,--diff=canonical) that is any \
             difference between their canonical forms, whether or not the \
             structural walk reached it"
          1;
        Cmd.Exit.info ~doc:"on command-line errors or unreadable input files"
          124;
      ]
  in
  Cmd.v (Cmd.info "diff" ~doc ~man ~envs:[ no_color_env ] ~exits) term
