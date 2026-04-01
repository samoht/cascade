(** Script to compare two CSS files with structural parsing for better diffs *)

open Cmdliner

let err_read path msg = Error (`Msg (Fmt.str "Error reading %s: %s" path msg))

let read_file path =
  try
    let ic = open_in path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Ok content
  with Sys_error msg -> err_read path msg

type diff_mode = Auto | Tree | String

let compare_files file1 file2 style_renderer diff_mode =
  (* Check NO_COLOR environment variable (https://no-color.org/) When present
     and not empty, disable colors regardless of other settings *)
  let style_renderer =
    match Sys.getenv_opt "NO_COLOR" with
    | Some s when s <> "" -> Some `None (* NO_COLOR is set and non-empty *)
    | _ -> style_renderer (* Use command line option *)
  in
  (* Setup Fmt with the style renderer from command line/env *)
  Fmt_tty.setup_std_outputs ?style_renderer ();

  match (read_file file1, read_file file2) with
  | Ok css1, Ok css2 -> (
      if css1 = css2 then (
        Fmt.pr "✓ CSS files are identical@.";
        Ok ())
      else
        (* Use css_compare for detailed structural comparison *)
        let result =
          match diff_mode with
          | Auto -> Css_tools.Css_compare.diff ~expected:css1 ~actual:css2
          | Tree ->
              Css_tools.Css_compare.diff_with_mode ~mode:`Tree ~expected:css1
                ~actual:css2
          | String ->
              Css_tools.Css_compare.diff_with_mode ~mode:`String ~expected:css1
                ~actual:css2
        in
        match result with
        | No_diff ->
            Fmt.pr "✓ CSS files are identical@.";
            Ok ()
        | String_diff _ ->
            (* Treat pure string differences as failures so tests catch ordering
               changes. Encourage --diff=tree for more detail. *)
            let stats =
              Css_tools.Css_compare.stats ~expected_str:css1 ~actual_str:css2
                result
            in
            let buf = Buffer.create 1024 in
            Css_tools.Css_compare.pp_stats buf stats;
            Buffer.add_char buf '\n';
            Css_tools.Css_compare.pp ~expected:file1 ~actual:file2 buf result;
            Buffer.add_char buf '\n';
            print_string (Buffer.contents buf);
            Error (`Msg "CSS files differ (string diff)")
        | Tree_diff _ | Both_errors _ | Expected_error _ | Actual_error _ ->
            let stats =
              Css_tools.Css_compare.stats ~expected_str:css1 ~actual_str:css2
                result
            in
            let buf = Buffer.create 1024 in
            Css_tools.Css_compare.pp_stats buf stats;
            Buffer.add_char buf '\n';
            Css_tools.Css_compare.pp ~expected:file1 ~actual:file2 buf result;
            Buffer.add_char buf '\n';
            print_string (Buffer.contents buf);
            Error (`Msg "CSS files differ"))
  | Error e, _ | _, Error e -> Error e

(* Command line interface *)
let file1_arg =
  let doc = "First CSS file to compare (expected/reference)" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE1" ~doc)

let file2_arg =
  let doc = "Second CSS file to compare (actual/test)" in
  Arg.(required & pos 1 (some file) None & info [] ~docv:"FILE2" ~doc)

let diff_mode_arg =
  let doc =
    "Diff mode: 'auto' (smart detection), 'tree' (force structural diff), or \
     'string' (force string diff)"
  in
  let diff_mode_conv =
    Arg.enum [ ("auto", Auto); ("tree", Tree); ("string", String) ]
  in
  Arg.(value & opt diff_mode_conv Auto & info [ "diff" ] ~docv:"MODE" ~doc)

let term =
  let open Term in
  (* CSSDIFF_COLOR env var for auto|always|never *)
  let style_renderer_with_env =
    Fmt_cli.style_renderer ~env:(Cmd.Env.info "CSSDIFF_COLOR") ()
  in
  term_result
    (const compare_files $ file1_arg $ file2_arg $ style_renderer_with_env
   $ diff_mode_arg)

let man =
  [
    `S Manpage.s_description;
    `P
      "$(tname) compares two CSS files and reports structural differences \
       using a tree-based diff format with syntax highlighting.";
    `P "The comparison parses both CSS files and detects:";
    `I ("•", "Added, removed, or modified rules");
    `I ("•", "Property value changes");
    `I ("•", "Reordered rules");
    `I ("•", "Changes in @media, @layer, and other at-rules");
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
    `S Manpage.s_exit_status;
    `P "$(tname) exits with:";
    `I ("0", "if the CSS files are identical");
    `I ("1", "if the CSS files differ or an error occurred");
    `S Manpage.s_environment;
    `P "Color output can be controlled via environment variables:";
    `I
      ( "NO_COLOR",
        "When set to any non-empty value, disables color output (see \
         https://no-color.org/)" );
    `I
      ( "CSSDIFF_COLOR",
        "Set to 'auto', 'always', or 'never' to control color output \
         (overridden by NO_COLOR)" );
    `S Manpage.s_examples;
    `P "Compare two CSS files:";
    `Pre "  $(tname) reference.css output.css";
    `P "Disable colors using flag:";
    `Pre "  $(tname) --color=never reference.css output.css";
    `P "Disable colors using NO_COLOR environment variable:";
    `Pre "  NO_COLOR=1 $(tname) reference.css output.css";
    `S Manpage.s_bugs;
    `P "Report bugs at https://github.com/samoht/cascade";
  ]

let cmd =
  let doc = "Compare two CSS files with structural analysis" in
  let info = Cmd.info "cssdiff" ~version:Cascade_info.version ~doc ~man in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
