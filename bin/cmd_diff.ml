open Cmdliner

type mode = Auto | Tree | String | Canonical

let err_read path msg = Error (`Msg (Fmt.str "Error reading %s: %s" path msg))

let read_file path =
  try
    let ic = open_in path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Ok content
  with Sys_error msg -> err_read path msg

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

let print_diff_report ~color ~file1 ~file2 ~css1 ~css2 result =
  let stats =
    Cascade_diff.Css_compare.stats ~expected_str:css1 ~actual_str:css2 result
  in
  let buf = Buffer.create 1024 in
  Cascade_diff.Css_compare.pp_stats buf stats;
  Buffer.add_char buf '\n';
  Cascade_diff.Css_compare.pp ~expected:file1 ~actual:file2 ~color buf result;
  Buffer.add_char buf '\n';
  print_string (Buffer.contents buf)

type canonical_opts = { lossless : bool; prune_unused_custom_props : bool }

let compare_files file1 file2 style_renderer mode opts memtrace_path () =
  Cli_io.start_memtrace memtrace_path;
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
        | No_diff _ ->
            (* Equal ASTs can still hide parse-dropped declarations; show the
               warnings so the equality verdict is honest about them. *)
            let buf = Buffer.create 256 in
            Cascade_diff.Css_compare.pp ~expected:file1 ~actual:file2 ~color buf
              result;
            print_string (Buffer.contents buf);
            Fmt.pr "CSS files are identical@.";
            Ok ()
        | String_diff _ | Tree_diff _ | Both_errors _ | Expected_error _
        | Actual_error _ ->
            print_diff_report ~color ~file1 ~file2 ~css1 ~css2 result;
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

let memtrace_arg =
  let doc =
    "Write a Memtrace allocation profile to $(docv) covering the diff run. \
     Open it with [memtrace-viewer] to see allocation hotspots."
  in
  Arg.(value & opt (some string) None & info [ "memtrace" ] ~docv:"FILE" ~doc)

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
   $ mode_arg $ canonical_opts $ memtrace_arg $ Cli_log.term)

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
      ( "--lossless",
        "Disable colour approximation under $(b,--diff=canonical): colour \
         channels keep their authored precision and static modern colour-space \
         values stay functional. No effect outside canonical mode." );
    `S Manpage.s_exit_status;
    `P "$(tname) exits with:";
    `I ("0", "if the CSS files are identical");
    `I ("1", "if the CSS files differ");
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
