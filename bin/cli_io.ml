(** I/O helpers shared across the cascade CLI commands. *)

open Cascade

let read_file path =
  let ic = open_in path in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  content

let read_stdin () =
  let buf = Buffer.create 4096 in
  try
    while true do
      Buffer.add_string buf (input_line stdin);
      Buffer.add_char buf '\n'
    done;
    Buffer.contents buf
  with End_of_file -> Buffer.contents buf

let parse_css ~filename css =
  match Css.of_string ~filename css with
  | Ok { Css.stylesheet; warnings } ->
      List.iter
        (fun w ->
          (* Prefix every line of a multi-line diagnostic so a downstream [grep
             -v "warning"] filters the whole entry, not just the first line. *)
          let msg = Cascade.Error.to_string w in
          String.split_on_char '\n' msg
          |> List.iter (fun line -> Fmt.epr "warning: %s@." line))
        warnings;
      stylesheet
  | Error _ ->
      Fmt.epr "warning: %s: parse failed, dropping content@." filename;
      Css.empty

let read_input path =
  let css = if path = "-" then read_stdin () else read_file path in
  let filename = if path = "-" then "<stdin>" else path in
  parse_css ~filename css

let split_comma s =
  String.split_on_char ',' s |> List.map String.trim
  |> List.filter (fun s -> s <> "")

(* [--memtrace FILE] streams a sampled allocation trace to FILE. The runtime may
   refuse to trace (Failure) and opening the trace file may fail
   (Invalid_argument / Sys_error for a missing directory or unwritable target);
   in either case warn and carry on rather than taking the program down. *)
let start_memtrace = function
  | None -> ()
  | Some path -> (
      try
        let tracer =
          Memtrace.start_tracing ~context:None ~sampling_rate:1e-4
            ~filename:path
        in
        at_exit (fun () ->
            try Memtrace.stop_tracing tracer
            with Failure msg | Invalid_argument msg | Sys_error msg ->
              Fmt.epr "warning: memtrace stop failed (%s); skipping@." msg)
      with Failure msg | Invalid_argument msg | Sys_error msg ->
        Fmt.epr "warning: memtrace unavailable on this runtime (%s); skipping@."
          msg)
