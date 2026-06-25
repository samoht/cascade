open Cmdliner

let () = Printexc.record_backtrace true

let info =
  let doc = "Tools to manipulate CSS files safely" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Cascade ships three subcommands: $(b,fmt) (format, minify, inline \
         imports), $(b,diff) (structural CSS diff) and $(b,apply) (resolve a \
         stylesheet into an HTML page's inline styles). When no subcommand is \
         given, $(b,fmt) is used.";
      `S Manpage.s_bugs;
      `P "Report bugs at https://github.com/samoht/cascade";
    ]
  in
  Cmd.info "cascade" ~version:Cascade_info.version ~doc ~man

let cmd =
  Cmd.group info ~default:Cmd_fmt.term
    [ Cmd_fmt.cmd; Cmd_diff.cmd; Cmd_apply.cmd ]

let known_subcommand = function "fmt" | "diff" | "apply" -> true | _ -> false

let help_or_version = function
  | "--help" | "-h" | "--version" -> true
  | _ -> false

let option_like first =
  String.length first > 1 && first.[0] = '-' && first <> "-"

let default_fmt_arg first =
  not (known_subcommand first || help_or_version first || option_like first)

(* cmdliner's [Cmd.group ~default] only invokes the default term when the
   command line is empty or the first positional is a known subcommand name; any
   other unrecognised first positional aborts with "unknown command". Rewrite
   [cascade FILE] to [cascade fmt FILE] so the bare shorthand works the same as
   the explicit [fmt] form. *)
let rewrite_argv_for_default argv =
  if Array.length argv <= 1 then argv
  else
    let first = argv.(1) in
    if not (default_fmt_arg first) then argv
    else
      Array.append
        [| argv.(0); "fmt" |]
        (Array.sub argv 1 (Array.length argv - 1))

let () = exit (Cmd.eval ~argv:(rewrite_argv_for_default Sys.argv) cmd)
