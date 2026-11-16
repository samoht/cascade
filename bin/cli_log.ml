open Cmdliner

type verbosity = Quiet | Normal | Verbose

let quiet =
  let doc = "Print errors only: no parse warning and no note." in
  Arg.(value & flag & info [ "q"; "quiet" ] ~doc)

let verbosity =
  let doc =
    "Print every parse warning in full, where a repeat is otherwise counted \
     under the first. Twice ($(b,-vv)) also prints the optimizer's step \
     tracing."
  in
  Arg.(value & flag_all & info [ "v"; "verbose" ] ~doc)

let setup quiet verbose =
  let verbosity, level =
    if quiet then (Quiet, Logs.Error)
    else
      match List.length verbose with
      | 0 -> (Normal, Logs.Warning)
      | 1 -> (Verbose, Logs.Info)
      | _ -> (Verbose, Logs.Debug)
  in
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some level);
  verbosity

let term = Term.(const setup $ quiet $ verbosity)
