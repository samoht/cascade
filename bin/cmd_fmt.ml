open Cascade
open Cmdliner

(* One row per factoring run: the rules and bytes it started from, the rules it
   ended with, what it saved, whether it changed anything, and its time. *)
let print_iteration_profile (report : Stats.snapshot) =
  match report.iteration_stats with
  | [] -> ()
  | iteration_stats ->
      Fmt.epr "  %-3s %9s %9s %9s %9s %7s %8s@." "run" "rules-in" "rules-out"
        "bytes-in" "saved" "changed" "time";
      List.iter
        (fun (s : Stats.iteration_stat) ->
          Fmt.epr "  %-3d %9d %9d %9d %9d %7s %7.3fs@." s.fixpoint
            s.before_rules s.after_rules s.before_bytes s.bytes_saved
            (if s.changed then "yes" else "no")
            s.elapsed)
        (List.rev iteration_stats)

let print_factor_profile_footer (counters : Stats.counters) =
  Fmt.epr "@.factor committed savings: %d bytes@." counters.factor_bytes_saved;
  Fmt.epr "factor preflight: %d skipped, estimated gain %d bytes@."
    counters.factor_fixpoints_skipped counters.factor_preflight_gain;
  Fmt.epr "nested-merge probes: %d@." counters.nested_merge_probes

(* Where the run went, then the factoring passes the optimizer spent most of its
   time in. *)
type phases = { read : float; optimize : float; print : float }

let report_profile phases (report : Stats.snapshot) =
  let total =
    List.fold_left
      (fun total (s : Stats.iteration_stat) -> total +. s.elapsed)
      0.0 report.iteration_stats
  in
  Fmt.epr "time: read %.3fs, optimize %.3fs, print %.3fs, total %.3fs@."
    phases.read phases.optimize phases.print
    (phases.read +. phases.optimize +. phases.print);
  Fmt.epr "factor fixpoint (%d runs, total %.3fs):@."
    report.counters.factor_fixpoints_run total;
  print_iteration_profile report;
  print_factor_profile_footer report.counters

let resolve_inline_imports ~verbosity ~input_path ~import_root stylesheet =
  if input_path = "-" then begin
    Cli_io.error
      "--inline-imports needs a file path, since relative URLs cannot be \
       resolved from standard input";
    Stdlib.exit 1
  end
  else
    Cli_inline_imports.run ~verbosity ~base_url:input_path ~import_root
      stylesheet

let emit_stylesheet ~minify ~lossless ~enforce_spec stylesheet =
  let buf = Buffer.create 4096 in
  Css.to_buffer buf ~minify ~lossless ~enforce_spec stylesheet;
  let len = Buffer.length buf in
  if len > 0 && Buffer.nth buf (len - 1) <> '\n' then Buffer.add_char buf '\n';
  Buffer.output_buffer stdout buf

let process_css ~verbosity ~input_path ~minify ~scope ~flatten_nesting ~lossless
    ~enforce_spec ~closed_world ~objective ~inline_imports_flag ~import_root
    ~inline_vars_flag ~keep_vars ~profile =
  try
    let start = Unix.gettimeofday () in
    let input = Cli_io.read_input verbosity ~enforce_spec input_path in
    Cli_io.check_not_all_dropped input;
    let stylesheet = input.Cli_io.stylesheet in
    let stylesheet =
      if inline_imports_flag then
        resolve_inline_imports ~verbosity ~input_path ~import_root stylesheet
      else stylesheet
    in
    let stylesheet =
      if inline_vars_flag then
        Cli_inline_vars.run verbosity ~keep_vars stylesheet
      else stylesheet
    in
    let stats = Stats.v ~profile () in
    let read = Unix.gettimeofday () in
    let stylesheet =
      if minify then
        (* Raw output ships uncompressed, so only there is the aggressive
           global-factoring search worth its wall clock. *)
        let aggressive = objective = `Raw in
        Css.optimize ~scope ~flatten_nesting ~lossless ~enforce_spec ~aggressive
          ~closed_world ~objective ~stats stylesheet
      else stylesheet
    in
    let optimized = Unix.gettimeofday () in
    emit_stylesheet ~minify ~lossless ~enforce_spec stylesheet;
    let printed = Unix.gettimeofday () in
    (* Without --minify, Css.optimize never runs and the recorder stays empty:
       report only when the fixpoint had a chance to run. *)
    if profile && minify then
      report_profile
        {
          read = read -. start;
          optimize = optimized -. read;
          print = printed -. optimized;
        }
        (Stats.snapshot stats)
  with Sys_error msg ->
    Cli_io.error "%s" msg;
    Stdlib.exit 1

let minify_options = "MINIFY OPTIONS"
let inline_options = "INLINING OPTIONS"

let input_arg =
  let doc =
    "The stylesheet to read; $(b,-) (the default) reads standard input."
  in
  Arg.(value & pos 0 file "-" & info [] ~docv:"FILE" ~doc)

let minify_arg =
  let doc =
    "Write the smallest stylesheet cascade can prove renders the same, instead \
     of pretty-printing. Every other option in this section acts only with it."
  in
  Arg.(value & flag & info [ "m"; "minify" ] ~docs:minify_options ~doc)

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
  Arg.(
    value & opt scope_conv `Fragment
    & info [ "scope" ] ~docs:minify_options ~docv:"SCOPE" ~doc)

let enforce_spec_arg =
  let doc =
    "Drop the evergreen-browser baseline that default $(b,--minify) targets. \
     cascade still serializes to the shortest form the CSS text and the specs \
     prove on their own, so it keeps a vendor-prefixed declaration written \
     beside its unprefixed twin, the $(b,min-)/$(b,max-) spelling of a media \
     or container feature, the $(b,&) prefix on a nested selector, the \
     author's :not(:dir(ltr)) and input:not(:enabled), the number form of an \
     oklab/oklch axis, and the quotes around a multi-word font family. It also \
     has the reader keep a rule whose selector holds a pseudo-class no target \
     reads, $(b,:-o-prefocus) for one, which it otherwise drops with a \
     warning, and holds it to the CSS Syntax 3 sec. 4.2 ident code points; \
     both apply with or without $(b,--minify)."
  in
  Arg.(value & flag & info [ "enforce-spec" ] ~docs:minify_options ~doc)

let lossless_arg =
  let doc =
    "Disable bounded approximation under $(b,--minify). Exact rewrites still \
     run, but repeating static numeric arithmetic stays as calc(), static \
     modern colour-space and color-mix() values stay functional, and colour \
     channels keep their normal serialisation precision. Otherwise-independent \
     declarations retain their authored order rather than being sorted for \
     compression. Has no effect without $(b,--minify)."
  in
  Arg.(value & flag & info [ "lossless" ] ~docs:minify_options ~doc)

let objective_arg =
  let doc =
    "Size metric $(b,--minify) optimises for. $(b,transfer) (the default) \
     minimises output bytes, but merges rules with one body only where they \
     print within 128 rules of each other, because a compressor encodes a \
     repeated declaration in two or three bytes wherever it sits, but a \
     selector moved away from its neighbours stops compressing against them; \
     $(b,raw) merges them at any distance. $(b,raw) also drives the factoring \
     fixpoint to convergence, the right objective when the output ships \
     uncompressed (inline style attributes, email HTML), at the cost of \
     markedly more wall clock. Has no effect without $(b,--minify)."
  in
  Arg.(
    value
    & opt (enum [ ("transfer", `Transfer); ("raw", `Raw) ]) `Transfer
    & info [ "objective" ] ~docs:minify_options ~doc ~docv:"OBJECTIVE")

let flatten_nesting_arg =
  let doc =
    "Flatten nested style rules into top-level rules, for browsers that \
     predate CSS Nesting. Otherwise nesting is kept, since current browsers \
     read it and the nested form is usually shorter. Has no effect without \
     $(b,--minify)."
  in
  Arg.(value & flag & info [ "flatten-nesting" ] ~docs:minify_options ~doc)

let inline_imports_arg =
  let doc =
    "Replace each $(b,@import) with the file it names, read from disk relative \
     to the input file. Without $(b,--import-root), any file the process can \
     read may be read, which is unsafe for untrusted CSS."
  in
  Arg.(value & flag & info [ "inline-imports" ] ~docs:inline_options ~doc)

let import_root_arg =
  let doc =
    "Restrict files read by $(b,--inline-imports) to $(docv) itself and its \
     descendants. The root and every imported path are canonicalised before \
     containment is checked, so $(b,..) and symlink escapes are rejected. \
     Relative roots are interpreted from the current working directory. Has no \
     effect without $(b,--inline-imports)."
  in
  Arg.(
    value
    & opt (some string) None
    & info [ "import-root" ] ~docs:inline_options ~docv:"DIR" ~doc)

let inline_vars_arg =
  let doc =
    "Replace each $(b,var(--name)) with the value the stylesheet gives \
     $(b,--name), then drop the custom properties nothing reads any more. \
     Assumes no script changes a custom property at runtime."
  in
  Arg.(value & flag & info [ "inline-vars" ] ~docs:inline_options ~doc)

let keep_vars_arg =
  let doc =
    "Custom properties $(b,--inline-vars) leaves as live $(b,var()) \
     references, comma-separated, with or without the leading $(b,--): \
     $(b,--keep-vars=theme,--brand). Has no effect without $(b,--inline-vars)."
  in
  Arg.(
    value & opt string ""
    & info [ "keep-vars" ] ~docs:inline_options ~docv:"NAMES" ~doc)

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
  Arg.(value & flag & info [ "closed-world" ] ~docs:minify_options ~doc)

let profile_arg =
  let doc =
    "Print to standard error, after the output, the time spent reading, \
     optimizing and printing, then the optimizer's factoring passes with the \
     rules, bytes and time each took. Has no effect without $(b,--minify)."
  in
  Arg.(value & flag & info [ "profile" ] ~docs:minify_options ~doc)

(* A flag set where it does nothing is noted, unless [-q] asked for errors only.
   --enforce-spec is not one: it also gates the parser's non-ASCII ident range,
   which acts with or without --minify. *)
let note_unused (verbosity : Cli_log.verbosity) ~minify ~scope ~flatten_nesting
    ~lossless ~closed_world ~objective ~profile ~keep_vars ~inline_vars
    ~import_root ~inline_imports =
  let without_minify flag active = (active && not minify, flag, "--minify") in
  let checks =
    [
      (keep_vars <> [] && not inline_vars, "--keep-vars", "--inline-vars");
      ( Option.is_some import_root && not inline_imports,
        "--import-root",
        "--inline-imports" );
      without_minify "--scope=stylesheet" (scope = `Stylesheet);
      without_minify "--lossless" lossless;
      without_minify "--closed-world" closed_world;
      without_minify "--objective" (objective = `Raw);
      without_minify "--profile" profile;
      without_minify "--flatten-nesting" flatten_nesting;
    ]
  in
  match verbosity with
  | Quiet -> ()
  | Normal | Verbose ->
      List.iter
        (fun (active, flag, needs) ->
          if active then Cli_io.warning "%s has no effect without %s" flag needs)
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
        import_root
        inline_vars_flag
        keep_vars_str
        profile
        verbosity
      ->
        let keep_vars = Cli_io.split_comma keep_vars_str in
        if List.mem "*" keep_vars then begin
          Cli_io.error
            "--keep-vars does not accept the wildcard \"*\"; list names \
             explicitly";
          Stdlib.exit 1
        end;
        note_unused verbosity ~minify ~scope ~flatten_nesting ~lossless
          ~closed_world ~objective ~profile ~keep_vars
          ~inline_vars:inline_vars_flag ~import_root
          ~inline_imports:inline_imports_flag;
        process_css ~verbosity ~input_path:input ~minify ~scope ~flatten_nesting
          ~lossless ~enforce_spec ~closed_world ~objective ~inline_imports_flag
          ~import_root ~inline_vars_flag ~keep_vars ~profile)
    $ input_arg $ minify_arg $ scope_arg $ flatten_nesting_arg $ lossless_arg
    $ enforce_spec_arg $ closed_world_arg $ objective_arg $ inline_imports_arg
    $ import_root_arg $ inline_vars_arg $ keep_vars_arg $ profile_arg
    $ Cli_log.term)

let man =
  [
    `S Manpage.s_description;
    `P
      "$(tname) reads a stylesheet and writes it back to standard output: \
       pretty-printed, or with $(b,--minify) as small as cascade can prove \
       renders the same.";
    `P
      "By default the minified output targets current browsers (Chrome 111, \
       Firefox 128, Safari 16.4) and may round a colour by a tiny amount. \
       $(b,--enforce-spec) and $(b,--lossless) turn those off. A few options \
       trade safety for size and are off by default: $(b,--scope=stylesheet), \
       $(b,--closed-world) and the two $(b,--inline-*) options.";
    `P
      "Each parse warning names its $(i,file:line:column) on standard error \
       and the output keeps everything the parser could read. The run fails \
       only when the parser could use none of the input.";
    `S Manpage.s_arguments;
    `S Manpage.s_options;
    `S minify_options;
    `S inline_options;
    `S Manpage.s_examples;
    `Pre
      "cascade style.css > style.formatted.css\n\
       cascade --minify style.css > style.min.css\n\
       cat style.css | cascade --minify -";
    `P "Inline $(b,@import)s and $(b,var()) references, then minify:";
    `Pre "cascade --inline-imports --inline-vars --minify style.css";
    `P "Inline only the imports under $(b,assets/css):";
    `Pre "cascade --inline-imports --import-root assets/css style.css";
    `P "Keep $(b,--theme) and $(b,--brand) as live $(b,var()) references:";
    `Pre "cascade --inline-vars --keep-vars=theme,brand style.css";
  ]

let cmd =
  let doc = "Format or minify a stylesheet" in
  let exits =
    Cli_exit.with_defaults
      [
        Cmd.Exit.info
          ~doc:"on success, including a parse that recovered some of the input"
          0;
        Cmd.Exit.info
          ~doc:
            "if the input exists but cannot be read, or if parse recovery left \
             no statement at all. An output that is empty because the \
             stylesheet held nothing a browser acts on is a success"
          1;
        Cmd.Exit.info
          ~doc:"on command-line errors, a missing input file among them" 124;
      ]
  in
  Cmd.v (Cmd.info "fmt" ~doc ~man ~exits) term
