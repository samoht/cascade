open Cascade
open Cmdliner

let profile_entries () =
  let module S = Cascade.Stats in
  let total = ref 0.0 in
  let entries =
    Hashtbl.fold
      (fun name s acc ->
        total := !total +. s.S.time;
        (name, s) :: acc)
      S.pass_times []
  in
  let entries =
    List.sort (fun (_, a) (_, b) -> compare b.S.time a.S.time) entries
  in
  (!total, entries)

let print_iteration_profile () =
  let module S = Cascade.Stats in
  match S.iteration_stats () with
  | [] -> ()
  | iteration_stats ->
      Fmt.epr "  %-3s %-4s %-4s %9s %9s %9s %9s %7s %7s %8s@." "fp" "iter"
        "giter" "rules_in" "rules_out" "bytes_in" "saved" "passes" "chg" "time";
      List.iter
        (fun (s : S.iteration_stat) ->
          Fmt.epr "  %-3d %-4d %-4d %9d %9d %9d %9d %7d %7d %7.3fs@." s.fixpoint
            s.local_iteration s.iteration s.before_rules s.after_rules
            s.before_bytes s.bytes_saved s.active_passes s.changed_passes
            s.elapsed)
        (List.rev iteration_stats)

let print_pass_profile entries =
  let module S = Cascade.Stats in
  Fmt.epr "  %-22s %8s %6s %6s %9s %9s@." "pass" "time" "calls" "chg" "rules_in"
    "rules_out";
  List.iter
    (fun (name, s) ->
      Fmt.epr "  %-22s %7.3fs %6d %6d %9d %9d@." name s.S.time s.S.calls
        s.S.changes s.S.rules_in s.S.rules_out)
    entries

let print_factor_profile_footer () =
  let module S = Cascade.Stats in
  Fmt.epr "@.factor stops: %d marginal, committed savings: %d bytes@."
    S.counters.marginal_stops S.counters.factor_bytes_saved;
  Fmt.epr "factor preflight: %d skipped, estimated gain %d bytes@."
    S.counters.factor_fixpoints_skipped S.counters.factor_preflight_gain;
  if S.counters.factor_transfer_reverts > 0 then
    Fmt.epr "factor transfer gate: %d segments reverted@."
      S.counters.factor_transfer_reverts

let report_profile () =
  let module S = Cascade.Stats in
  let total, entries = profile_entries () in
  Fmt.epr "factor fixpoint (%d runs, %d iterations, total %.3fs):@."
    S.counters.factor_fixpoints_run S.counters.iterations total;
  print_iteration_profile ();
  print_pass_profile entries;
  print_factor_profile_footer ()

let resolve_inline_imports ~input_path stylesheet =
  if input_path = "-" then begin
    Fmt.epr
      "Error: --inline-imports requires a file path (cannot resolve relative \
       URLs from stdin)@.";
    Stdlib.exit 1
  end
  else Cli_inline_imports.run ~base_url:input_path stylesheet

let emit_stylesheet ~minify ~lossless ~enforce_spec stylesheet =
  let buf = Buffer.create 4096 in
  Css.to_buffer buf ~minify ~lossless ~enforce_spec stylesheet;
  let len = Buffer.length buf in
  if len > 0 && Buffer.nth buf (len - 1) <> '\n' then Buffer.add_char buf '\n';
  Buffer.output_buffer stdout buf

let process_css ~input_path ~minify ~scope ~flatten_nesting ~lossless
    ~enforce_spec ~closed_world ~objective ~inline_imports_flag
    ~inline_vars_flag ~keep_vars ~memtrace_path ~profile =
  Cli_io.start_memtrace memtrace_path;
  try
    let stylesheet = Cli_io.read_input input_path in
    let stylesheet =
      if inline_imports_flag then resolve_inline_imports ~input_path stylesheet
      else stylesheet
    in
    let stylesheet =
      if inline_vars_flag then Cli_inline_vars.run ~keep_vars stylesheet
      else stylesheet
    in
    let stylesheet =
      if minify then begin
        Cascade.Stats.set_profile profile;
        (* Raw output ships uncompressed, so the aggressive global-factoring
           fixpoint is worth its cost only here: under the transfer objective
           its extra raw-byte wins grow gzip and get discarded anyway. *)
        let aggressive = objective = `Raw in
        Css.optimize ~scope ~flatten_nesting ~lossless ~enforce_spec ~aggressive
          ~closed_world ~objective stylesheet
      end
      else stylesheet
    in
    emit_stylesheet ~minify ~lossless ~enforce_spec stylesheet;
    if profile then report_profile ()
  with
  | Sys_error msg ->
      Fmt.epr "Error: %s@." msg;
      Stdlib.exit 1
  | e ->
      Fmt.epr "Unexpected error: %s@." (Printexc.to_string e);
      let bt = Printexc.get_backtrace () in
      if bt <> "" then Fmt.epr "%s@." bt;
      Stdlib.exit 1

let input_arg =
  let doc = "CSS file to process (use - for stdin)" in
  Arg.(value & pos 0 string "-" & info [] ~docv:"FILE" ~doc)

let minify_arg =
  let doc =
    "Minify the output using the optimizer's incremental safe-transform \
     pipeline. Local linear rewrites always run; expensive global factoring \
     runs only when its indexed preflight predicts useful savings."
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
     serialisation precision. Also sorts each rule's declarations into a \
     canonical cross-rule order (keeping cascade-significant pairs in place) \
     so gzip back-references line up. Has no effect without $(b,--minify)."
  in
  Arg.(value & flag & info [ "lossless" ] ~doc)

let objective_arg =
  let doc =
    "Size metric $(b,--minify) optimises for. $(b,transfer) (the default) \
     keeps a global-factoring result only when it also shrinks the estimated \
     gzip (DEFLATE) size of the output, since repeated declaration text is \
     nearly free once compressed; $(b,raw) keeps every raw-byte win and drives \
     the factoring fixpoint to convergence, the right objective when the \
     output ships uncompressed (inline style attributes, email HTML), at the \
     cost of markedly more wall clock. Has no effect without $(b,--minify)."
  in
  Arg.(
    value
    & opt (enum [ ("transfer", `Transfer); ("raw", `Raw) ]) `Transfer
    & info [ "objective" ] ~doc ~docv:"OBJECTIVE")

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

let closed_world_arg =
  let doc =
    "Assume you know the exact HTML and that no element ever matches two \
     clashing selectors, so cascade may merge rules it would otherwise keep \
     apart (the assumption tools like SatCSS make from a captured page). \
     Unsafe: the page can render wrong if such an element appears, including \
     one a script adds at runtime. This is about the HTML, separate from \
     $(b,--scope) (how much of the CSS you control). Has no effect without \
     $(b,--minify)."
  in
  Arg.(value & flag & info [ "closed-world" ] ~doc)

let profile_arg =
  let doc =
    "Print per-pass timings of the optimizer factoring fixpoint to stderr \
     after the run. Use to triage which pass dominates on a slow input. Has no \
     effect without $(b,--minify)."
  in
  Arg.(value & flag & info [ "profile" ] ~doc)

(* Flags that only act under [--minify]; warn when set without it. *)
let warn_minify_only ~minify checks =
  if not minify then
    List.iter
      (fun (active, flag) ->
        if active then
          Fmt.epr "Warning: %s has no effect without --minify@." flag)
      checks

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
        closed_world
        objective
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
          Stdlib.exit 1
        end;
        if keep_vars <> [] && not inline_vars_flag then
          Fmt.epr "Warning: --keep-vars has no effect without --inline-vars@.";
        warn_minify_only ~minify
          [
            (scope = `Stylesheet, "--scope=stylesheet");
            (lossless, "--lossless");
            (enforce_spec, "--enforce-spec");
            (closed_world, "--closed-world");
            (objective = `Raw, "--objective");
            (profile, "--profile");
          ];
        process_css ~input_path:input ~minify ~scope ~flatten_nesting ~lossless
          ~enforce_spec ~closed_world ~objective ~inline_imports_flag
          ~inline_vars_flag ~keep_vars ~memtrace_path ~profile)
    $ input_arg $ minify_arg $ scope_arg $ flatten_nesting_arg $ lossless_arg
    $ enforce_spec_arg $ closed_world_arg $ objective_arg $ inline_imports_arg
    $ inline_vars_arg $ keep_vars_arg $ memtrace_arg $ profile_arg
    $ Cli_log.term)

let man =
  [
    `S Manpage.s_description;
    `P
      "$(tname) reads a CSS file and writes it back, optionally minifying it \
       or inlining [@import] rules and [var(--*)] references.";
    `P
      "Without flags, $(tname) pretty-prints. With $(b,--minify) it applies a \
       single incremental optimizer pipeline: local linear rewrites always \
       run, while expensive global factoring runs only when its indexed \
       preflight predicts useful savings.";
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
