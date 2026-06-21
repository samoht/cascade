open Cmdliner

let () = Printexc.record_backtrace true

let info =
  let doc = "Tools to manipulate CSS files safely" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Cascade ships two subcommands: $(b,fmt) (format, minify, inline) and \
         $(b,diff) (structural CSS diff). When no subcommand is given, \
         $(b,fmt) is used.";
      `S Manpage.s_bugs;
      `P "Report bugs at https://github.com/samoht/cascade";
    ]
  in
  Cmd.info "cascade" ~version:Cascade_info.version ~doc ~man

let cmd =
  Cmd.group info ~default:Cmd_fmt.term
    [ Cmd_fmt.cmd; Cmd_diff.cmd; Cmd_inline.cmd ]

(* cmdliner's [Cmd.group ~default] only invokes the default term when the
   command line is empty or the first positional is a known subcommand name; any
   other unrecognised first positional aborts with "unknown command". Rewrite
   [cascade FILE] to [cascade fmt FILE] so the bare shorthand works the same as
   the explicit [fmt] form. *)
let rewrite_argv_for_default argv =
  if Array.length argv <= 1 then argv
  else
    let first = argv.(1) in
    let is_known_subcommand =
      first = "fmt" || first = "diff" || first = "inline"
    in
    let is_help_or_version =
      first = "--help" || first = "-h" || first = "--version"
    in
    let is_flag = String.length first > 1 && first.[0] = '-' && first <> "-" in
    if is_known_subcommand || is_help_or_version || is_flag then argv
    else
      Array.append
        [| argv.(0); "fmt" |]
        (Array.sub argv 1 (Array.length argv - 1))

let () = exit (Cmd.eval ~argv:(rewrite_argv_for_default Sys.argv) cmd)
