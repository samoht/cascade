open Cascade
open Cmdliner

let process_css ~input_path ~minify ~inline_imports_flag ~inline_vars_flag
    ~keep_vars =
  try
    let stylesheet = Cli_io.read_input input_path in
    let stylesheet =
      if inline_imports_flag then
        if input_path = "-" then begin
          Fmt.epr
            "Error: --inline-imports requires a file path (cannot resolve \
             relative URLs from stdin)@.";
          exit 1
        end
        else Cli_inline_imports.run ~base_url:input_path stylesheet
      else stylesheet
    in
    let stylesheet =
      if inline_vars_flag then Cli_inline_vars.run ~keep_vars stylesheet
      else stylesheet
    in
    let stylesheet =
      if minify then Css.optimize ~flatten_nesting:true stylesheet
      else stylesheet
    in
    let output = Css.to_string ~minify ~mode:Css.Variables stylesheet in
    Cli_io.print_output output
  with
  | Sys_error msg ->
      Fmt.epr "Error: %s@." msg;
      exit 1
  | e ->
      Fmt.epr "Unexpected error: %s@." (Printexc.to_string e);
      let bt = Printexc.get_backtrace () in
      if bt <> "" then Fmt.epr "%s@." bt;
      exit 1

let input_arg =
  let doc = "CSS file to process (use - for stdin)" in
  Arg.(value & pos 0 string "-" & info [] ~docv:"FILE" ~doc)

let minify_arg =
  let doc =
    "Minify the output and apply safe transforms (deduplication, rule merging, \
     selector grouping, empty-rule removal)."
  in
  Arg.(value & flag & info [ "m"; "minify" ] ~doc)

let inline_imports_arg =
  let doc =
    "Inline [@import] rules by reading the referenced files from disk \
     (relative to the input file). Closed-world: assumes you control file \
     resolution."
  in
  Arg.(value & flag & info [ "inline-imports" ] ~doc)

let inline_vars_arg =
  let doc =
    "Substitute [var(--name)] references with the value of [--name] \
     declarations in the stylesheet, then drop the now-unused custom property \
     definitions. Closed-world: assumes no runtime mutation of custom \
     properties."
  in
  Arg.(value & flag & info [ "inline-vars" ] ~doc)

let keep_vars_arg =
  let doc =
    "Comma-separated list of custom property names to preserve when using \
     --inline-vars. Names may be given with or without the leading [--]. \
     Example: --keep-vars=theme,--brand."
  in
  Arg.(value & opt string "" & info [ "keep-vars" ] ~docv:"NAMES" ~doc)

let term =
  Term.(
    const
      (fun input minify inline_imports_flag inline_vars_flag keep_vars_str ->
        let keep_vars = Cli_io.split_comma keep_vars_str in
        if List.mem "*" keep_vars then begin
          Fmt.epr
            "Error: --keep-vars does not accept the wildcard \"*\"; list names \
             explicitly.@.";
          Fmt.epr "[1]@.";
          exit 1
        end;
        if keep_vars <> [] && not inline_vars_flag then
          Fmt.epr "Warning: --keep-vars has no effect without --inline-vars@.";
        process_css ~input_path:input ~minify ~inline_imports_flag
          ~inline_vars_flag ~keep_vars)
    $ input_arg $ minify_arg $ inline_imports_arg $ inline_vars_arg
    $ keep_vars_arg)

let man =
  [
    `S Manpage.s_description;
    `P
      "$(tname) reads a CSS file and writes it back, optionally minifying it \
       or inlining [@import] rules and [var(--*)] references.";
    `P
      "Without flags, $(tname) pretty-prints. With $(b,--minify) it applies \
       the standard cssnano-style safe transforms (deduplication, rule \
       merging, selector grouping, empty-rule elimination) and emits minified \
       output.";
    `P
      "The two $(b,--inline-*) flags are explicit closed-world opt-ins: \
       $(b,--inline-imports) assumes you control file resolution and \
       $(b,--inline-vars) assumes no runtime mutation of custom properties.";
    `S Manpage.s_examples;
    `P "Pretty-print a CSS file:";
    `Pre "  cascade fmt style.css";
    `P "Minify a CSS file:";
    `Pre "  cascade fmt --minify style.css";
    `P "Inline [@import]s and [var()] references, then minify:";
    `Pre "  cascade fmt --inline-imports --inline-vars --minify style.css";
    `P "Keep [--theme] and [--brand] as live var() references:";
    `Pre "  cascade fmt --inline-vars --keep-vars=theme,brand style.css";
    `P "Read from stdin and minify:";
    `Pre "  cat style.css | cascade fmt --minify -";
  ]

let cmd =
  let doc = "Format, minify, and inline CSS files" in
  Cmd.v (Cmd.info "fmt" ~doc ~man) term
