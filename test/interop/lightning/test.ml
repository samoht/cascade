(** Minifier interop tests for cascade.

    Inputs are generated from lightningcss df63db2c by patching
    [minify_test_with_options] to dump every source case. The dumped Lightning
    expected value is one candidate oracle answer. During [@@regen-traces], the
    configured minifier CLIs are run over the same input and their outputs or
    crashes are cached in the trace. Normal test runs never shell out to oracle
    tools: each valid cached candidate receives Cascade's declared target
    prefixes, then Cascade passes if its output is no longer than the shortest
    adjusted candidate.

    Each trace record is treated as a complete stylesheet, matching the way the
    upstream minifier CLIs see the input: closed over the fixture CSS text for
    cascade/dependency/dead-code reasoning, but still open over runtime layout
    state such as DOM shape, writing mode, direction, user styles, and runtime
    custom-property mutation.

    Regenerate Lightning inputs and the package-lock-pinned oracle answers with
    [REGEN=1 dune build @@test/interop/lightning/regen-traces].

    Format of [traces/minify.pairs]: a sequence of length-prefixed records, each
    one:

    {v
    >>> <input_len> <candidate_count> <failure_count>\n
    <input bytes>\n
    OK <tool_len> <css_len>\n<tool bytes><css bytes>\n
    FAIL <tool_len> <command_len> <reason_len>\n<tool bytes><command bytes><reason bytes>\n
    v}

    The suite is split into Alcotest groups by broad CSS feature and then into
    slow fixed-size shard cases. Each case batches enough pairs for useful logs
    without forcing one feature bucket to run for minutes. The summary surfaces
    enough longer-than-industry outputs to drive arbitrage; for full drilldown
    re-run with [VERBOSE=1] in the env.

    Upstream tool bugs are diagnostics only. When a cached upstream minifier
    failure or non-equivalent output is seen, the normal run appends a report to
    [_build/_tests/lightning_minify/upstream-bugs.log]. Override the report
    location with [CASCADE_INTEROP_UPSTREAM_REPORT]. That report has a second
    section for cached answers Cascade has no grounds to judge: an at-rule it
    has no handler for round-trips as opaque source text, so every rewrite of
    that body reads as a change whether or not the meaning moved.

    For local iteration, run one Alcotest case from [dune exec ... -- list],
    e.g. [dune exec test/interop/lightning/test.exe -- test color 0]. *)

open Cascade

let trace_path =
  let local = Filename.concat "traces" "minify.pairs" in
  if Sys.file_exists local then local
  else Filename.concat "test/interop/lightning/traces" "minify.pairs"

type minify_result = Result_css of string | Source_parse_error of string
type outcome = Pass | Parse_error of string | Mismatch of string
type candidate = Trace_pairs.candidate = { tool : string; css : string }
type rejected_candidate = { tool : string; css : string; reason : string }

type failed_candidate = Trace_pairs.failed_candidate = {
  tool : string;
  command : string;
  reason : string;
}

type upstream_issue =
  | Rejected_candidate of rejected_candidate
  | Failed_candidate of failed_candidate

(* Rejecting a cached answer says only that Cascade could not certify it. That
   is evidence against the tool when Cascade understood the input, and evidence
   of nothing when it did not. *)
type verdict =
  | Equivalent of candidate
  | Upstream_bug of rejected_candidate
  | Unjudgeable of rejected_candidate

type classification = {
  outcome : outcome;
  upstream_issues : upstream_issue list;
  unjudgeable : rejected_candidate list;
  opaque_at_keywords : string list;
}

let display_css css = if css = "" then "<empty>" else css

let cascade_minify input =
  match Css.of_string ~strict:false input with
  | Error e -> Source_parse_error (Cascade.Error.to_string e)
  | Ok parsed -> (
      match
        parsed.stylesheet
        |> Css.optimize ~scope:`Stylesheet
        |> Css.to_string ~minify:true
      with
      | s -> Result_css s
      | exception Invalid_argument msg ->
          Source_parse_error ("invalid_argument: " ^ msg))

let contains_substring ~needle haystack =
  try
    let _ = Re.Str.search_forward (Re.Str.regexp_string needle) haystack 0 in
    true
  with Not_found -> false

let shortest_length (candidates : candidate list) =
  List.fold_left
    (fun acc ({ css; _ } : candidate) -> min acc (String.length css))
    max_int candidates

let add_declared_target_prefixes ({ tool; css } : candidate) =
  match Css.of_string ~strict:false css with
  | Error _ -> { tool; css }
  | Ok parsed ->
      let css =
        parsed.stylesheet
        |> Css.Optimize.add_compatibility_prefixes
             ~targets:Css.Optimize.evergreen_targets
        |> Css.to_string ~minify:true
      in
      { tool; css }

let canonical_minified css =
  if css = "" then Ok ""
  else
    match Css.of_string ~strict:false css with
    | Error e -> Error (Cascade.Error.to_string e)
    | Ok parsed -> (
        match
          parsed.stylesheet
          |> Css.optimize ~scope:`Stylesheet
          |> Css.to_string ~minify:true
        with
        | s -> Ok s
        | exception Invalid_argument msg -> Error ("invalid_argument: " ^ msg))

let split_top_level_commas s =
  let len = String.length s in
  let rec loop i depth start acc =
    if i >= len then List.rev (String.sub s start (len - start) :: acc)
    else
      match s.[i] with
      | '(' -> loop (i + 1) (depth + 1) start acc
      | ')' when depth > 0 -> loop (i + 1) (depth - 1) start acc
      | ',' when depth = 0 ->
          let part = String.sub s start (i - start) in
          loop (i + 1) depth (i + 1) (part :: acc)
      | _ -> loop (i + 1) depth start acc
  in
  loop 0 0 0 []

let has_trailing_bare_number segment =
  Re.Str.string_match
    (Re.Str.regexp ".*[ \t\r\n][+-]?\\([0-9]+\\(\\.[0-9]*\\)?\\|\\.[0-9]+\\)$")
    segment 0

let color_mix_has_bare_weight css =
  let needle = "color-mix(" in
  let needle_len = String.length needle in
  let len = String.length css in
  let rec find_matching_close i depth =
    if i >= len then None
    else
      match css.[i] with
      | '(' -> find_matching_close (i + 1) (depth + 1)
      | ')' ->
          if depth = 1 then Some i else find_matching_close (i + 1) (depth - 1)
      | _ -> find_matching_close (i + 1) depth
  in
  let rec loop start =
    try
      let open_ =
        Re.Str.search_forward (Re.Str.regexp_string needle) css start
      in
      let body_start = open_ + needle_len in
      match find_matching_close body_start 1 with
      | None -> false
      | Some close ->
          let body = String.sub css body_start (close - body_start) in
          let stops =
            match split_top_level_commas body with
            | _space :: stops -> stops
            | [] -> []
          in
          if List.exists has_trailing_bare_number stops then true
          else loop (close + 1)
    with Not_found -> false
  in
  loop 0

let unitless_zero_angle_re =
  Re.Perl.compile_pat
    "\\b(?:repeating-)?linear-gradient\\(0(?:[,)]|\\s+in\\b)|\\b(?:repeating-)?conic-gradient\\(from\\s+0(?:[,)]|\\s)|\\b(?:rotate[XYZ]?|skew[XY]?)\\(0(?:[,)]|\\s)"

let has_unitless_zero_angle_slot css = Re.execp unitless_zero_angle_re css

let known_upstream_candidate_bug ({ tool; css } : candidate) =
  if tool = "cssnano" && color_mix_has_bare_weight css then
    Some
      "cssnano emitted a bare number as a color-mix() weight; CSS Color 5 \
       requires <percentage [0,100]>"
  else if has_unitless_zero_angle_slot css then
    Some "emitted unitless zero in an angle slot; Cascade keeps the angle unit"
  else None

(* Which side of the comparison failed decides who is answerable for it. An
   answer cascade cannot match is the tool's, unless the input is opaque: a
   construct cascade has no handler for leaves it no grounds to convict. A
   source cascade cannot re-read implicates no property of the answer at all, so
   it is nobody's bug whatever the input looks like - reporting it as one blames
   a tool for cascade's own failure. *)
let candidate_verdict ~opaque_input ~source_failed rejection =
  if source_failed || opaque_input then Unjudgeable rejection
  else Upstream_bug rejection

let validate_candidate ~opaque_input input (candidate : candidate) =
  (* Candidate filtering is deliberately narrow. A cached upstream answer is an
     oracle only if it parses and canonicalizes to the same stylesheet-scoped
     Cascade output as the source. Raw at-rule shape is not a validity
     requirement here: default minify may remove wrappers that are target-dead,
     redundant, empty, or otherwise non-participating. Hard-coded exclusions
     above cover known tool/source bugs where the CSS text itself is invalid
     despite being short. *)
  let reject ?(source_failed = false) reason =
    let rejection = { tool = candidate.tool; css = candidate.css; reason } in
    candidate_verdict ~opaque_input ~source_failed rejection
  in
  match known_upstream_candidate_bug candidate with
  | Some reason ->
      (* Invalid CSS text, read off the answer itself. Nothing about the input
         is needed to convict, so an opaque input does not excuse it. *)
      Upstream_bug { tool = candidate.tool; css = candidate.css; reason }
  | None -> (
      match (canonical_minified input, canonical_minified candidate.css) with
      | Ok "", Ok "" -> Equivalent candidate
      | Ok source_css, Ok output_css when source_css = output_css ->
          Equivalent candidate
      | Ok source_css, Ok output_css ->
          Fmt.kstr reject
            "semantic fingerprint changed after Cascade parse: %s -> %s"
            (display_css source_css) (display_css output_css)
      | Ok _, Error msg ->
          reject ("candidate output failed Cascade semantic roundtrip: " ^ msg)
      | Error _, _ ->
          reject ~source_failed:true
            "source input failed Cascade parser roundtrip")

let split_equivalent_candidates ~opaque_input input
    (candidates : candidate list) =
  List.fold_right
    (fun candidate (accepted, bugs, unjudgeable) ->
      match validate_candidate ~opaque_input input candidate with
      | Equivalent candidate -> (candidate :: accepted, bugs, unjudgeable)
      | Upstream_bug rejection -> (accepted, rejection :: bugs, unjudgeable)
      | Unjudgeable rejection -> (accepted, bugs, rejection :: unjudgeable))
    candidates ([], [], [])

let format_upstream_issues input issues =
  "UPSTREAM MINIFIER BUGS\n"
  ^ (issues
    |> List.map (function
      | Rejected_candidate rejection ->
          Fmt.str
            "    UPSTREAM BUG in: %s\n\
            \    reason: %s\n\
            \    input:  %s\n\
            \    output: %s"
            rejection.tool rejection.reason input
            (display_css rejection.css)
      | Failed_candidate failure ->
          Fmt.str
            "    UPSTREAM BUG in: %s\n\
            \    command: %s\n\
            \    reason: %s\n\
            \    input:  %s"
            failure.tool failure.command failure.reason input)
    |> String.concat "\n")

let format_unjudgeable input at_keywords rejections =
  Fmt.str
    "CACHED ANSWERS CASCADE CANNOT JUDGE: the input uses %s, which Cascade has \
     no handler for, so the body round-trips as opaque source text and no \
     rewrite of it can be certified equivalent. Rejecting these is correct; \
     they are not upstream bugs.\n\
     %s"
    (at_keywords |> List.map (fun k -> "@" ^ k) |> String.concat " ")
    (rejections
    |> List.map (fun (rejection : rejected_candidate) ->
        Fmt.str
          "    UNJUDGEABLE answer from: %s\n\
          \    reason: %s\n\
          \    input:  %s\n\
          \    output: %s"
          rejection.tool rejection.reason input
          (display_css rejection.css))
    |> String.concat "\n")

let format_source_parse_diagnostics (pair : Trace_pairs.t) =
  let input = pair.input in
  let candidates = pair.candidates in
  let failures = pair.failures in
  let parseable = ref 0 in
  let reports =
    candidates
    |> List.map (fun ({ tool; css } : candidate) ->
        match canonical_minified css with
        | Ok canonical ->
            incr parseable;
            Fmt.str
              "    %s: output parses with Cascade\n\
              \      output:    %s\n\
              \      canonical: %s"
              tool (display_css css) (display_css canonical)
        | Error msg ->
            Fmt.str
              "    %s: output also fails Cascade parser\n\
              \      output: %s\n\
              \      error:  %s"
              tool (display_css css) msg)
    |> String.concat "\n"
  in
  let failure_reports =
    failures
    |> List.map (fun failure ->
        Fmt.str
          "    %s: output unavailable\n      command: %s\n      error:   %s"
          failure.tool failure.command failure.reason)
    |> String.concat "\n"
  in
  let reports =
    if failure_reports = "" then reports else reports ^ "\n" ^ failure_reports
  in
  Fmt.str
    "%s\n\
    \    SOURCE PARSE DIAGNOSTICS: Cascade could not parse the source fixture. \
     Cached oracle outputs are diagnostics only; they are not accepted as \
     proof of semantic equivalence without a successful source parse.\n\
    \    parseable_cached_outputs=%d/%d\n\
     %s"
    input !parseable (List.length candidates) reports

(* Lowercased at-keyword names in [css], scanned rather than parsed: the point
   of the check below is to measure Cascade against the cached text without
   routing through Cascade. Strings and comments are skipped so a [content:
   "@media"] is not read as a rule. *)
let at_keywords css =
  let len = String.length css in
  let is_name c =
    (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9')
    || c = '-' || c = '_'
    || Char.code c >= 0x80
  in
  let rec skip_string quote i =
    if i >= len then i
    else if css.[i] = '\\' then skip_string quote (i + 2)
    else if css.[i] = quote then i + 1
    else skip_string quote (i + 1)
  in
  let rec skip_comment i =
    if i + 1 >= len then len
    else if css.[i] = '*' && css.[i + 1] = '/' then i + 2
    else skip_comment (i + 1)
  in
  let rec name_end i =
    if i < len && is_name css.[i] then name_end (i + 1) else i
  in
  let rec scan i acc =
    if i >= len then acc
    else
      match css.[i] with
      | ('"' | '\'') as q -> scan (skip_string q (i + 1)) acc
      | '/' when i + 1 < len && css.[i + 1] = '*' ->
          scan (skip_comment (i + 2)) acc
      | '@' ->
          let stop = name_end (i + 1) in
          let name =
            String.lowercase_ascii (String.sub css (i + 1) (stop - i - 1))
          in
          scan stop (if name = "" then acc else name :: acc)
      | _ -> scan (i + 1) acc
  in
  List.sort_uniq String.compare (scan 0 [])

(* At-keywords Cascade's own parser reported as having no handler. *)
let unrecognised_at_keywords input =
  match Css.of_string ~strict:false input with
  | Error _ -> []
  | Ok { Css.warnings; _ } ->
      warnings
      |> List.filter_map (fun e ->
          match e.Error.kind with
          | Error.Unknown_at_rule name -> Some (String.lowercase_ascii name)
          | _ -> None)
      |> List.sort_uniq String.compare

(* At-keywords every cached answer kept. *)
let unanimously_kept_at_keywords (pair : Trace_pairs.t) =
  match List.map (fun (c : candidate) -> at_keywords c.css) pair.candidates with
  | [] -> []
  | first :: rest ->
      List.fold_left
        (fun acc kept -> List.filter (fun k -> List.mem k kept) acc)
        first rest

let classify (pair : Trace_pairs.t) =
  let input = pair.input in
  match cascade_minify input with
  | Source_parse_error msg ->
      let diagnostics = format_source_parse_diagnostics pair in
      {
        outcome = Parse_error (msg ^ "\n    input diagnostics: " ^ diagnostics);
        upstream_issues = [];
        unjudgeable = [];
        opaque_at_keywords = [];
      }
  | Result_css actual ->
      (* The at-keywords Cascade's parser reported no handler for. It keeps
         their bodies as opaque source text, so it has no grounds to certify a
         cached rewrite of one, and the same names scope the dropped check
         below. *)
      let opaque_at_keywords = unrecognised_at_keywords input in
      let candidates, rejected, unjudgeable =
        split_equivalent_candidates ~opaque_input:(opaque_at_keywords <> [])
          input pair.candidates
      in
      let upstream_issues =
        List.map (fun issue -> Rejected_candidate issue) rejected
        @ List.map (fun issue -> Failed_candidate issue) pair.failures
      in
      (* The cached CLIs ran without browser targets, while Cascade's default
         optimizer owns a concrete browser baseline. Compare byte counts only
         after applying that one public target transform to each accepted
         candidate; running Cascade's whole optimizer here would turn this
         arbitrage check into a comparison against itself. *)
      let scored_candidates =
        List.map add_declared_target_prefixes candidates
      in
      let best = shortest_length scored_candidates in
      (* Neither measure below can see a construct Cascade deletes. Length
         scoring cannot: a deletion always reads as a shorter output.
         [validate_candidate] cannot either, because it routes the source and
         the candidate through the same Cascade serializer, so whatever that
         serializer drops is missing from both sides and the two compare equal.
         The cached text is the one measure outside that loop. Scope it to the
         at-rules Cascade's parser says it has no handler for: those are the
         ones it has no grounds to rewrite, so when every tool that answered
         kept one, its name has to reach Cascade's output too. At-rules Cascade
         does understand stay out of scope, since unwrapping a satisfied
         [@supports] removes the name and keeps the contents. *)
      let dropped =
        let kept = at_keywords actual in
        let unanimous = unanimously_kept_at_keywords pair in
        opaque_at_keywords
        |> List.filter (fun k -> List.mem k unanimous && not (List.mem k kept))
      in
      let outcome =
        if dropped <> [] then
          Fmt.kstr
            (fun s -> Mismatch s)
            "%s\n\
            \    dropped an at-rule it could not interpret and every cached \
             answer kept: %s"
            (display_css actual)
            (dropped |> List.map (fun k -> "@" ^ k) |> String.concat " ")
        else if candidates = [] then Pass
        else if String.length actual <= best then Pass
        else
          let shortest =
            scored_candidates
            |> List.filter (fun (c : candidate) -> String.length c.css = best)
            |> List.map (fun (c : candidate) ->
                c.tool ^ ":" ^ display_css c.css)
            |> String.concat " | "
          in
          Fmt.kstr
            (fun s -> Mismatch s)
            "%s\n    shortest: %s\n    actual_len=%d best_len=%d" actual
            shortest (String.length actual) best
      in
      { outcome; upstream_issues; unjudgeable; opaque_at_keywords }

let candidate_names pairs =
  pairs
  |> List.fold_left
       (fun names (pair : Trace_pairs.t) ->
         let names =
           List.fold_left
             (fun acc ({ tool; _ } : candidate) -> tool :: acc)
             names pair.candidates
         in
         List.fold_left
           (fun acc ({ tool; _ } : failed_candidate) -> tool :: acc)
           names pair.failures)
       []
  |> List.sort_uniq String.compare

let available_minifier_names pairs =
  match candidate_names pairs with [] -> "none" | xs -> String.concat ", " xs

let format_candidates (pair : Trace_pairs.t) =
  match pair.candidates with
  | [] -> "<none>"
  | candidates ->
      candidates
      |> List.map (fun ({ tool; css } : candidate) ->
          tool ^ ":" ^ display_css css)
      |> String.concat " | "

let format_divergence i (pair : Trace_pairs.t) outcome =
  match outcome with
  | Pass -> assert false
  | Parse_error msg ->
      Fmt.str "  pair_%04d: parse error: %s\n    input: %s" i msg pair.input
  | Mismatch actual ->
      Fmt.str
        "  pair_%04d: mismatch\n\
        \    input:    %s\n\
        \    oracles:  %s\n\
        \    actual:   %s"
        i pair.input (format_candidates pair) actual

let verbose =
  match Sys.getenv_opt "VERBOSE" with Some "1" -> true | _ -> false

let upstream_report_path =
  match Sys.getenv_opt "CASCADE_INTEROP_UPSTREAM_REPORT" with
  | Some path when String.trim path <> "" -> path
  | _ -> "_build/_tests/lightning_minify/upstream-bugs.log"

let rec mkdir_p path =
  if path <> "" && not (Sys.file_exists path) then begin
    mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o755
  end

let append_upstream_report lines =
  mkdir_p (Filename.dirname upstream_report_path);
  let oc =
    open_out_gen
      [ Open_creat; Open_text; Open_append ]
      0o644 upstream_report_path
  in
  output_string oc lines;
  output_char oc '\n';
  close_out oc

let reset_upstream_report () =
  if Sys.file_exists upstream_report_path then Sys.remove upstream_report_path

let emit_report ~summary ~kind ~trailer reports =
  if reports <> [] then begin
    let limit = if verbose then List.length reports else 20 in
    let head =
      List.filteri (fun i _ -> i < limit) reports
      |> List.map (fun (i, report) ->
          Fmt.str "  pair_%04d: %s\n%s" i kind report)
      |> String.concat "\n"
    in
    let tail =
      if List.length reports > limit then
        Fmt.str "\n  ... (%d more; set VERBOSE=1 for full list)"
          (List.length reports - limit)
      else ""
    in
    let report = summary ^ "\n" ^ head ^ tail in
    append_upstream_report report;
    Fmt.epr "%s@.%s: %s@." report trailer upstream_report_path
  end

let feature_of_input input =
  let has s = contains_substring ~needle:s input in
  if has "animation" || has "@keyframes" then "animation"
  else if
    has "color" || has "rgb(" || has "hsl(" || has "hwb(" || has "lab("
    || has "lch(" || has "color-mix(" || has "color("
  then "color"
  else if has "calc(" || has "min(" || has "max(" || has "clamp(" then "math"
  else if
    has ":is(" || has ":where(" || has ":not(" || has ":has(" || has ":nth-"
    || has "::"
  then "selectors"
  else if has "@media" || has "@supports" || has "@container" then "conditions"
  else if has "@layer" || has "@scope" || has "@starting-style" then "cascade"
  else if has "@font-face" || has "font-" then "fonts"
  else if has "all:" || has "revert" || has "unset" || has "initial" then
    "resets"
  else "misc"

let features =
  [
    "animation";
    "color";
    "math";
    "selectors";
    "conditions";
    "cascade";
    "fonts";
    "resets";
    "misc";
  ]

let shard_size = 1

let pairs_by_feature pairs =
  let indexed = List.mapi (fun i pair -> (i, pair)) pairs in
  List.map
    (fun feature ->
      ( feature,
        List.filter
          (fun (_, (pair : Trace_pairs.t)) ->
            feature_of_input pair.input = feature)
          indexed ))
    features

let shard_count selected =
  let n = List.length selected in
  if n = 0 then 0 else (n + shard_size - 1) / shard_size

let selected_shard selected shard =
  selected
  |> List.filteri (fun i _ ->
      let start = shard * shard_size in
      i >= start && i < start + shard_size)

let pair_cases ~total_pairs ~minifier_names selected ~feature ~shard ~shards ()
    =
  let selected = selected_shard selected shard in
  let total = List.length selected in
  let pass = ref 0 in
  let parse_err = ref 0 in
  let mismatch = ref 0 in
  let oracle_bug = ref 0 in
  let oracle_bug_reports = ref [] in
  let unjudgeable = ref 0 in
  let unjudgeable_reports = ref [] in
  let divergences = ref [] in
  List.iter
    (fun (i, (pair : Trace_pairs.t)) ->
      let classified = classify pair in
      let upstream_issues = classified.upstream_issues in
      if upstream_issues <> [] then begin
        oracle_bug := !oracle_bug + List.length upstream_issues;
        oracle_bug_reports :=
          (i, format_upstream_issues pair.input upstream_issues)
          :: !oracle_bug_reports
      end;
      if classified.unjudgeable <> [] then begin
        unjudgeable := !unjudgeable + List.length classified.unjudgeable;
        unjudgeable_reports :=
          ( i,
            format_unjudgeable pair.input classified.opaque_at_keywords
              classified.unjudgeable )
          :: !unjudgeable_reports
      end;
      match classified.outcome with
      | Pass -> incr pass
      | Parse_error _ as o ->
          incr parse_err;
          divergences := (i, pair, o) :: !divergences
      | Mismatch _ as o ->
          incr mismatch;
          divergences := (i, pair, o) :: !divergences)
    selected;
  let divergences = List.rev !divergences in
  let summary =
    Fmt.str
      "minifier interop%s: %d/%d selected pass (%d total pairs; %d parse \
       errors, %d longer-than-shortest mismatches, %d cached oracle bugs, %d \
       cached answers Cascade cannot judge; cached oracle tools: %s)"
      (Fmt.str " [%s %d/%d]" feature (shard + 1) shards)
      !pass total total_pairs !parse_err !mismatch !oracle_bug !unjudgeable
      minifier_names
  in
  print_endline summary;
  emit_report ~summary ~kind:"cached oracle bug warning"
    ~trailer:"upstream minifier bug report"
    (List.rev !oracle_bug_reports);
  emit_report ~summary ~kind:"cached answer Cascade cannot judge"
    ~trailer:"unjudgeable cached answer report"
    (List.rev !unjudgeable_reports);
  if divergences <> [] then begin
    let limit = if verbose then List.length divergences else 20 in
    let head =
      List.filteri (fun i _ -> i < limit) divergences
      |> List.map (fun (i, pair, o) -> format_divergence i pair o)
      |> String.concat "\n"
    in
    let tail =
      if List.length divergences > limit then
        Fmt.str "\n  ... (%d more; set VERBOSE=1 for full list)"
          (List.length divergences - limit)
      else ""
    in
    Alcotest.failf "%s\n%s%s" summary head tail
  end

let grouped_cases pairs =
  let total_pairs = List.length pairs in
  let minifier_names = available_minifier_names pairs in
  pairs_by_feature pairs
  |> List.filter_map (fun (feature, selected) ->
      let shards = shard_count selected in
      if shards = 0 then None
      else
        let cases =
          List.init shards (fun shard ->
              let name = Fmt.str "%02d/%02d" (shard + 1) shards in
              Alcotest.test_case name `Quick
                (pair_cases ~total_pairs ~minifier_names selected ~feature
                   ~shard ~shards))
        in
        Some (feature, cases))

(* Hand-written pairs pinning the label a single cached answer gets. The corpus
   cases cannot: they assert Cascade's own output, and a label only reaches them
   through an aggregate count. *)
let reported_issues pair =
  (classify pair).upstream_issues
  |> List.map (function
    | Rejected_candidate rejection -> "bug:" ^ rejection.tool
    | Failed_candidate failure -> "failed:" ^ failure.tool)

let unjudgeable_answers pair =
  (classify pair).unjudgeable
  |> List.map (fun (rejection : rejected_candidate) -> rejection.tool)

let trace ?(candidates = []) ?(failures = []) input : Trace_pairs.t =
  { input; candidates; failures }

let labelling_case ?(unjudgeable = []) name pair expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check (list string))
        "reported upstream bugs" expected (reported_issues pair);
      Alcotest.(check (list string))
        "answers Cascade cannot judge" unjudgeable (unjudgeable_answers pair))

(* Which side of the comparison failed decides who is answerable for it. A
   candidate cascade cannot re-read is genuinely ambiguous, and an opaque input
   excuses a divergence on a construct cascade has no handler for. A source
   cascade cannot re-read implicates no property of the answer at all, so it is
   never an upstream bug: reporting it as one blames a tool for cascade's own
   failure.

   No trace in the corpus reaches the source arm today, so the policy is pinned
   here rather than through a trace pair. *)
let verdict_name = function
  | Equivalent _ -> "equivalent"
  | Upstream_bug _ -> "bug"
  | Unjudgeable _ -> "unjudgeable"

let verdict_case name ~opaque_input ~source_failed expected =
  Alcotest.test_case name `Quick (fun () ->
      let rejection = { tool = "t"; css = ".a{}"; reason = "probe" } in
      Alcotest.(check string)
        name expected
        (verdict_name
           (candidate_verdict ~opaque_input ~source_failed rejection)))

let verdict_cases =
  [
    verdict_case "a source cascade cannot re-read is nobody's bug"
      ~opaque_input:false ~source_failed:true "unjudgeable";
    verdict_case "an opaque input excuses the answer" ~opaque_input:true
      ~source_failed:false "unjudgeable";
    verdict_case "a divergence on an input cascade read is the tool's"
      ~opaque_input:false ~source_failed:false "bug";
  ]

let labelling_cases =
  [
    labelling_case "equivalent answer is not an issue"
      (trace ".foo { color: red }"
         ~candidates:[ Trace_pairs.{ tool = "t"; css = ".foo{color:red}" } ])
      [];
    labelling_case "divergent answer is an upstream bug"
      (trace ".foo { color: red }"
         ~candidates:[ Trace_pairs.{ tool = "t"; css = ".foo{color:blue}" } ])
      [ "bug:t" ];
    labelling_case "at-rule Cascade cannot interpret is not an upstream bug"
      ~unjudgeable:[ "t" ]
      (trace "@foo test { foo: bar; }"
         ~candidates:[ Trace_pairs.{ tool = "t"; css = "@foo test{foo:bar}" } ])
      [];
    labelling_case "an answer that invents one is still an upstream bug"
      (trace "@font-feature-values foo { @swash { pretty: 1; } }"
         ~candidates:[ Trace_pairs.{ tool = "t"; css = "@swash{pretty:1}" } ])
      [ "bug:t" ];
    labelling_case "a tool that failed outright is an upstream bug"
      (trace ".foo { color: red }"
         ~failures:
           [
             Trace_pairs.
               { tool = "t"; command = "t --minify"; reason = "exit 1" };
           ])
      [ "failed:t" ];
  ]

let () =
  reset_upstream_report ();
  let pairs = Trace_pairs.read trace_path in
  Alcotest.run "lightning_minify"
    (("labelling", labelling_cases)
    :: ("verdict", verdict_cases) :: grouped_cases pairs)
