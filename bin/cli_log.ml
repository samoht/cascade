open Cmdliner

let quiet =
  let doc = "Suppress all output except errors." in
  Arg.(value & flag & info [ "q"; "quiet" ] ~doc)

let verbosity =
  let doc =
    "Increase verbosity. Use once ($(b,-v)) for info, twice ($(b,-vv)) for \
     debug. Optimizer step tracing is emitted at debug level."
  in
  Arg.(value & flag_all & info [ "v"; "verbose" ] ~doc)

let level_of_verbosity ~quiet ~verbosity =
  if quiet then Some Logs.Error
  else
    match List.length verbosity with
    | 0 -> Some Logs.Warning
    | 1 -> Some Logs.Info
    | _ -> Some Logs.Debug

let setup quiet verbosity =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (level_of_verbosity ~quiet ~verbosity)

let term = Term.(const setup $ quiet $ verbosity)
