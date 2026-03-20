open Cmdliner

(* Always record backtraces so unexpected exceptions include full stacks *)
let () = Printexc.record_backtrace true

type output_format = Pretty | Minified

let read_file path =
  let ic = open_in path in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  content

let process_css input_path output_format optimize =
  try
    (* Read CSS file *)
    let css_content =
      if input_path = "-" then
        (* Read from stdin *)
        let buf = Buffer.create 4096 in
        try
          while true do
            Buffer.add_string buf (input_line stdin);
            Buffer.add_char buf '\n'
          done;
          Buffer.contents buf
        with End_of_file -> Buffer.contents buf
      else read_file input_path
    in

    (* Parse CSS *)
    let stylesheet =
      match Css.of_string css_content with
      | Ok s -> s
      | Error err ->
          let error_msg = Css.pp_parse_error err in
          Fmt.epr "Error parsing CSS: %s@." error_msg;
          exit 1
    in

    (* Apply optimizations if requested *)
    let stylesheet = if optimize then Css.optimize stylesheet else stylesheet in

    (* Output CSS *)
    let output =
      let minify =
        match output_format with Minified -> true | Pretty -> false
      in
      Css.to_string ~minify stylesheet
    in

    print_string output;
    if output <> "" && output.[String.length output - 1] <> '\n' then
      print_newline ()
  with
  | Sys_error msg ->
      Fmt.epr "Error: %s@." msg;
      exit 1
  | e ->
      Fmt.epr "Unexpected error: %s@." (Printexc.to_string e);
      let bt = Printexc.get_backtrace () in
      if bt <> "" then Fmt.epr "%s@." bt;
      exit 1

(* Command-line interface *)

let input_arg =
  let doc = "CSS file to process (use - for stdin)" in
  Arg.(value & pos 0 string "-" & info [] ~docv:"FILE" ~doc)

let minify =
  let doc = "Minify the output CSS" in
  Arg.(value & flag & info [ "m"; "minify" ] ~doc)

let pretty =
  let doc = "Pretty-print the output CSS (default)" in
  Arg.(value & flag & info [ "p"; "pretty" ] ~doc)

let optimize =
  let doc = "Optimize CSS by merging rules and removing duplicates" in
  Arg.(value & flag & info [ "o"; "optimize" ] ~doc)

let term =
  Term.(
    const (fun input minify pretty optimize ->
        (* Determine output format *)
        let output_format =
          if minify then Minified
          else if pretty then Pretty
          else Pretty (* Default to pretty if neither specified *)
        in

        process_css input output_format optimize)
    $ input_arg $ minify $ pretty $ optimize)

let info =
  let doc = "Process CSS files - pretty-print, minify, or optimize" in
  let man =
    [
      `S Manpage.s_description;
      `P "$(tname) reads a CSS file and outputs it in various formats.";
      `P
        "It can pretty-print for readability, minify for production, or \
         optimize by merging duplicate rules.";
      `S Manpage.s_examples;
      `P "Pretty-print a CSS file:";
      `Pre "  $(tname) style.css";
      `P "Minify a CSS file:";
      `Pre "  $(tname) --minify style.css";
      `P "Optimize and minify:";
      `Pre "  $(tname) --optimize --minify style.css";
      `P "Read from stdin and minify:";
      `Pre "  cat style.css | $(tname) --minify -";
    ]
  in
  Cmd.info "cascade" ~version:"0.1.0" ~doc ~man

let cmd = Cmd.v info term
let () = exit (Cmd.eval cmd)
