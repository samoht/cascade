open Cascade
open Cmdliner

let report_profile () =
  let module O = Cascade.Optimize in
  let total = ref 0.0 in
  let entries =
    Hashtbl.fold
      (fun name s acc ->
        total := !total +. s.O.time;
        (name, s) :: acc)
      O.pass_times []
  in
  let entries =
    List.sort (fun (_, a) (_, b) -> compare b.O.time a.O.time) entries
  in
  Fmt.epr "factor fixpoint (%d iterations, total %.3fs):@."
    O.counters.iterations !total;
  Fmt.epr "  %-22s %8s %6s %6s %9s %9s@." "pass" "time" "calls" "chg" "rules_in"
    "rules_out";
  List.iter
    (fun (name, s) ->
      Fmt.epr "  %-22s %7.3fs %6d %6d %9d %9d@." name s.O.time s.O.calls
        s.O.changes s.O.rules_in s.O.rules_out)
    entries;
  let hits = O.counters.summary_hits in
  let misses = O.counters.summary_misses in
  let total_lookups = hits + misses in
  let hit_pct =
    if total_lookups = 0 then 0.0 else 100. *. float hits /. float total_lookups
  in
  Fmt.epr
    "@.factor anchors scored: %d (prefiltered %d), factorings applied: %d@."
    O.counters.anchors_scored O.counters.anchors_prefiltered
    O.counters.factorings_applied;
  Fmt.epr
    "factor intervals: %d candidates, %d pruned, %d exact-scored, %d selected@."
    O.counters.interval_candidates O.counters.interval_pruned
    O.counters.interval_scored O.counters.interval_selected;
  Fmt.epr "summarize_factor_rule cache: %d hits, %d misses (%.1f%% hit rate)@."
    hits misses hit_pct

let process_css ~input_path ~minify ~scope ~flatten_nesting ~lossless
    ~enforce_spec ~inline_imports_flag ~inline_vars_flag ~keep_vars
    ~memtrace_path ~profile =
  Cli_io.start_memtrace memtrace_path;
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
    (* Parse -> optional inline/resolve -> optimize with scope -> serialise. *)
    let stylesheet =
      if minify then
        Css.optimize ~scope ~flatten_nesting ~lossless ~enforce_spec stylesheet
      else stylesheet
    in
    let output = Css.to_string ~minify ~lossless ~enforce_spec stylesheet in
    Cli_io.print_output output;
    if profile then report_profile ()
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

let scope_arg =
  let scope_conv =
    let parse = function
      | "fragment" -> Ok `Fragment
      | "stylesheet" -> Ok `Stylesheet
      | s -> Error (`Msg ("expected 'fragment' or 'stylesheet', got: " ^ s))
    in
    let print fmt = function
      | `Fragment -> Format.pp_print_string fmt "fragment"
      | `Stylesheet -> Format.pp_print_string fmt "stylesheet"
    in
    Arg.conv ~docv:"SCOPE" (parse, print)
  in
  let doc =
    "Tell the optimizer how much surrounding CSS context the input might be \
     embedded in. $(b,fragment) (default) treats the input as an excerpt that \
     may be concatenated with arbitrary other author CSS - earlier \
     $(b,<link>), later $(b,<style>), bundler concatenation, layer statements \
     outside the file, caller-side composition. Only semantics-preserving \
     rewrites under any surrounding CSS run. $(b,stylesheet) asserts the input \
     is the whole relevant author CSS graph; the optimizer may then synthesize \
     a partial $(b,background) / $(b,border) shorthand whose omitted longhand \
     resets are proved safe because no prior author write can be shadowed."
  in
  Arg.(value & opt scope_conv `Fragment & info [ "scope" ] ~docv:"SCOPE" ~doc)

let enforce_spec_arg =
  let doc =
    "Keep feature queries that default $(b,--minify) would elide using \
     evergreen-browser support facts. With $(b,--enforce-spec), $(tname) still \
     serializes to the shortest CSS form but preserves $(b,@supports) and \
     $(b,supports()) guards unless the CSS text and specification alone prove \
     the rewrite. Has no effect without $(b,--minify)."
  in
  Arg.(value & flag & info [ "enforce-spec" ] ~doc)

let lossless_arg =
  let doc =
    "Disable colour approximation under $(b,--minify). Exact colour \
     canonicalisation still runs, but static modern colour-space and \
     color-mix() values stay functional and colour channels keep their normal \
     serialisation precision. Has no effect without $(b,--minify)."
  in
  Arg.(value & flag & info [ "lossless" ] ~doc)

let flatten_nesting_arg =
  let doc =
    "Compatibility transform: flatten nested style rules into top-level rules \
     for browsers that pre-date the CSS Nesting Module. By default, $(tname) \
     preserves nesting since modern browsers parse it natively and the nested \
     form is usually shorter."
  in
  Arg.(value & flag & info [ "flatten-nesting" ] ~doc)

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

let memtrace_arg =
  let doc =
    "Write a memtrace allocation trace to $(docv). Open it with \
     [memtrace_hotspots] to see allocation hotspots."
  in
  Arg.(value & opt (some string) None & info [ "memtrace" ] ~docv:"FILE" ~doc)

let profile_arg =
  let doc =
    "Print per-pass timings of the optimizer factoring fixpoint to stderr \
     after the run. Use to triage which pass dominates on a slow input. Has no \
     effect without $(b,--minify)."
  in
  Arg.(value & flag & info [ "profile" ] ~doc)

let term =
  Term.(
    const
      (fun
        input
        minify
        scope
        flatten_nesting
        lossless
        enforce_spec
        inline_imports_flag
        inline_vars_flag
        keep_vars_str
        memtrace_path
        profile
        ()
      ->
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
        if scope = `Stylesheet && not minify then
          Fmt.epr "Warning: --scope=stylesheet has no effect without --minify@.";
        if lossless && not minify then
          Fmt.epr "Warning: --lossless has no effect without --minify@.";
        if enforce_spec && not minify then
          Fmt.epr "Warning: --enforce-spec has no effect without --minify@.";
        if profile && not minify then
          Fmt.epr "Warning: --profile has no effect without --minify@.";
        process_css ~input_path:input ~minify ~scope ~flatten_nesting ~lossless
          ~enforce_spec ~inline_imports_flag ~inline_vars_flag ~keep_vars
          ~memtrace_path ~profile)
    $ input_arg $ minify_arg $ scope_arg $ flatten_nesting_arg $ lossless_arg
    $ enforce_spec_arg $ inline_imports_arg $ inline_vars_arg $ keep_vars_arg
    $ memtrace_arg $ profile_arg $ Cli_log.term)

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
