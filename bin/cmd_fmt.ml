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

module Minify_preflight = struct
  let small_candidate_threshold = 4_000
  let useful_gain_bytes = 2_048
  let useful_gain_ratio_ppm = 120_000

  type summary = {
    mutable source_size : int;
    mutable rule_count : int;
    mutable declaration_count : int;
    mutable adjacent_selector_gain : int;
    mutable identical_body_gain : int;
    mutable shared_declaration_gain : int;
  }

  type state = {
    summary : summary;
    body_groups : (int list, int) Hashtbl.t;
    declaration_counts : (int, int * int) Hashtbl.t;
  }

  let v () =
    {
      summary =
        {
          source_size = 0;
          rule_count = 0;
          declaration_count = 0;
          adjacent_selector_gain = 0;
          identical_body_gain = 0;
          shared_declaration_gain = 0;
        };
      body_groups = Hashtbl.create 256;
      declaration_counts = Hashtbl.create 1024;
    }

  let selector_text sel = Css.Selector.to_string ~minify:true sel

  let declaration_size decl =
    String.length (Css.Declaration.to_string ~minify:true decl)

  let declaration_list_size decls =
    List.fold_left (fun acc decl -> acc + declaration_size decl) 0 decls

  let rule_size selector_size decl_size decl_count =
    selector_size + 2 + decl_size + max 0 (decl_count - 1)

  let add_declaration_count state n =
    state.summary.declaration_count <- state.summary.declaration_count + n

  let record_declaration state decl =
    let hash = Css.Declaration.hash decl in
    let size = declaration_size decl in
    let count, _ =
      match Hashtbl.find_opt state.declaration_counts hash with
      | Some entry -> entry
      | None -> (0, size)
    in
    if count > 0 then
      state.summary.shared_declaration_gain <-
        state.summary.shared_declaration_gain + max 0 (size - 1);
    Hashtbl.replace state.declaration_counts hash (count + 1, size)

  let record_declarations state decls =
    add_declaration_count state (List.length decls);
    List.iter (record_declaration state) decls

  let record_identical_body state decls decl_size =
    let key = List.map Css.Declaration.hash decls in
    match Hashtbl.find_opt state.body_groups key with
    | Some count ->
        state.summary.identical_body_gain <-
          state.summary.identical_body_gain + max 0 (decl_size + 1);
        Hashtbl.replace state.body_groups key (count + 1)
    | None -> Hashtbl.add state.body_groups key 1

  let page_margin_size margins =
    List.fold_left
      (fun acc m -> acc + List.length m.Css.Stylesheet.descriptors)
      0 margins

  let font_feature_value_size blocks =
    List.fold_left (fun acc (_, values) -> acc + List.length values) 0 blocks

  let keyframe state acc (k : Css.Stylesheet.keyframe) =
    record_declarations state k.declarations;
    acc + 2 + declaration_list_size k.declarations

  let descriptor_block_size acc descriptors = acc + 1 + List.length descriptors

  let declaration_block_size state acc decls =
    record_declarations state decls;
    acc + 1 + declaration_list_size decls

  let rec block state acc (stmts : Css.Stylesheet.statement list) =
    match stmts with
    | [] -> acc
    | Rule r :: rest ->
        let acc = rule state acc r in
        block_rules state acc (selector_text r.selector) rest
    | stmt :: rest -> block state (statement state acc stmt) rest

  and block_rules state acc previous_selector
      (stmts : Css.Stylesheet.statement list) =
    match stmts with
    | Rule r :: rest ->
        let selector = selector_text r.selector in
        let selector_size = String.length selector in
        if selector = previous_selector then
          state.summary.adjacent_selector_gain <-
            state.summary.adjacent_selector_gain + selector_size + 1;
        let acc = rule state acc r in
        block_rules state acc selector rest
    | rest -> block state acc rest

  and rule state acc (r : Css.Stylesheet.rule) =
    let selector_size = String.length (selector_text r.selector) in
    let decl_size = declaration_list_size r.declarations in
    let decl_count = List.length r.declarations in
    state.summary.rule_count <- state.summary.rule_count + 1;
    record_declarations state r.declarations;
    if r.nested = [] && decl_count > 0 then
      record_identical_body state r.declarations decl_size;
    block state (acc + rule_size selector_size decl_size decl_count) r.nested

  and statement state acc = function
    | Rule r -> rule state acc r
    | Declarations decls ->
        record_declarations state decls;
        acc + declaration_list_size decls
    | Media (_, block_)
    | Supports (_, block_)
    | Moz_document (_, block_)
    | Layer (_, block_)
    | Container (_, _, block_)
    | Scope (_, _, block_)
    | Starting_style block_
    | When (_, block_)
    | Else (_, block_)
    | Origin (_, block_) ->
        block state (acc + 1) block_
    | Keyframes (_, frames)
    | Webkit_keyframes (_, frames)
    | Moz_keyframes (_, frames) ->
        List.fold_left (keyframe state) (acc + 1) frames
    | Page (_, decls) | Supports_condition (_, decls) | Position_try (_, decls)
      ->
        declaration_block_size state acc decls
    | Page_with_margins (_, decls, margins) ->
        declaration_block_size state acc decls + page_margin_size margins
    | Font_face descriptors -> descriptor_block_size acc descriptors
    | Counter_style (_, descriptors) -> descriptor_block_size acc descriptors
    | Font_palette_values (_, descriptors) ->
        descriptor_block_size acc descriptors
    | View_transition descriptors -> descriptor_block_size acc descriptors
    | Viewport (_, descriptors) -> descriptor_block_size acc descriptors
    | Font_feature_values (_, blocks) ->
        acc + 1 + font_feature_value_size blocks
    | Property _ | Import _ | Namespace _ | Layer_decl _ | Unknown_at_rule _
    | Bang_comment _ | Charset _ ->
        acc + 1

  let summarize stylesheet =
    let state = v () in
    state.summary.source_size <- block state 0 stylesheet;
    state.summary

  let estimated_gain summary =
    summary.adjacent_selector_gain + summary.identical_body_gain
    + (summary.shared_declaration_gain / 32)

  let useful summary =
    if summary.declaration_count <= small_candidate_threshold then true
    else
      let gain = estimated_gain summary in
      gain >= useful_gain_bytes
      && gain * 1_000_000 >= summary.source_size * useful_gain_ratio_ppm
end

let should_optimize ~minify ~scope ~flatten_nesting ~lossless ~enforce_spec
    ~inline_imports_flag ~inline_vars_flag ~profile stylesheet =
  minify
  && (scope = `Stylesheet || flatten_nesting || lossless || enforce_spec
    || inline_imports_flag || inline_vars_flag || profile
     || stylesheet |> Minify_preflight.summarize |> Minify_preflight.useful)

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
      if
        should_optimize ~minify ~scope ~flatten_nesting ~lossless ~enforce_spec
          ~inline_imports_flag ~inline_vars_flag ~profile stylesheet
      then
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
    "Minify the output. Inputs with useful global rewrite candidates and \
     optimizer-dependent modes also run global safe transforms (deduplication, \
     rule merging, selector grouping); low-ROI inputs use the fast minifying \
     serializer by default."
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
      "Without flags, $(tname) pretty-prints. With $(b,--minify) it applies a \
       size-aware heuristic: inputs with useful global rewrite candidates and \
       optimizer-dependent modes run the full global safe-transform pipeline; \
       low-ROI inputs use the fast minifying serializer by default.";
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
