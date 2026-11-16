open Cmdliner

let () = Printexc.record_backtrace true

(* A standard channel starts in text mode, which on Windows writes each "\n" as
   "\r\n" and reads "\r\n" back as "\n". The CLI writes the bytes it prints, and
   reads standard input as it reads a file, on every platform. *)
let () =
  set_binary_mode_out stdout true;
  set_binary_mode_out stderr true;
  set_binary_mode_in stdin true

let info =
  let doc = "Format, minify, compare and prune CSS" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Cascade has four commands: $(b,fmt) formats and minifies a \
         stylesheet, $(b,diff) compares two stylesheets by what they do, \
         $(b,apply) moves a stylesheet into an HTML page's inline styles, and \
         $(b,prune) removes the rules a set of pages never uses. Without a \
         command, $(b,fmt) runs, so $(b,cascade style.css) is $(b,cascade fmt \
         style.css). $(b,cascade) $(i,COMMAND) $(b,--help) describes each one.";
      `P
        "A parse warning reads $(i,file:line:column): $(b,warning:) \
         $(i,message), followed by the line it is about. $(b,-q) hides them \
         and $(b,-v) prints every one where a repeat is otherwise counted.";
      `S Manpage.s_examples;
      `Pre
        "cascade --minify style.css > style.min.css\n\
         cascade diff --diff=canonical style.css style.min.css\n\
         cascade apply page.html theme.css > inlined.html\n\
         cascade prune --dry-run src/*.html style.css";
      `S Manpage.s_bugs;
      `P "Report bugs at https://github.com/samoht/cascade/issues";
    ]
  in
  Cmd.info "cascade" ~version:Cascade_info.version ~doc ~man

let cmd =
  Cmd.group info ~default:Cmd_fmt.term
    [ Cmd_fmt.cmd; Cmd_diff.cmd; Cmd_apply.cmd; Cmd_prune.cmd ]

let known_subcommand = function
  | "fmt" | "diff" | "apply" | "prune" -> true
  | _ -> false

let help_or_version = function
  | "--help" | "-h" | "--version" -> true
  | _ -> false

let option_like first =
  String.length first > 1 && first.[0] = '-' && first <> "-"

(* A word with no dot or slash that names no file is a mistyped command
   ([cascade fmtt a.css]) rather than a stylesheet, and cmdliner says so, with
   the command it is closest to. *)
let path_like first =
  first = "-" || String.contains first '.' || String.contains first '/'
  || Sys.file_exists first

let default_fmt_arg first =
  path_like first
  && not (known_subcommand first || help_or_version first || option_like first)

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
