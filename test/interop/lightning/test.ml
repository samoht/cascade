(** Minifier interop tests for cascade.

    Inputs are generated from lightningcss df63db2c by patching
    [minify_test_with_options] to dump every (source, expected) pair. The stored
    expected value is treated as Lightning CSS' candidate output, not as the
    only oracle. At test time, when other minifier CLIs are present locally,
    each input is run through them too. Cascade passes if its output is no
    longer than the shortest available candidate.

    Regenerate Lightning input candidates with [dune build @@regen-traces].

    Format of [traces/minify.pairs]: a sequence of length-prefixed records, each
    one

    {v >>> <input_len> <expected_len>\n<input bytes><expected bytes>\n v}

    The suite is split into Alcotest groups by broad CSS feature and then into
    slow fixed-size shard cases. Each case batches enough pairs for useful logs
    without forcing one feature bucket to run for minutes. The summary surfaces
    enough longer-than-industry outputs to drive arbitrage; for full drilldown
    re-run with [VERBOSE=1] in the env.

    Extra minifier commands can be supplied with [CASCADE_INTEROP_MINIFIERS],
    separated by [;;]. Commands must read CSS from stdin and write minified CSS
    to stdout. Example:

    {v
    CASCADE_INTEROP_MINIFIERS='esbuild --loader=css --minify;;cleancss -O2 -'
    v}

    For local iteration, run one Alcotest case from [dune exec ... -- list],
    e.g. [dune exec test/interop/lightning/test.exe -- test color 0]. *)

open Cascade

let trace_path =
  let local = Filename.concat "traces" "minify.pairs" in
  if Sys.file_exists local then local
  else Filename.concat "test/interop/lightning/traces" "minify.pairs"

let read_pairs path =
  let ic = open_in_bin path in
  let pairs = ref [] in
  (try
     while true do
       let header = input_line ic in
       Scanf.sscanf header ">>> %d %d" (fun ilen elen ->
           let buf = Bytes.create (ilen + elen) in
           really_input ic buf 0 (ilen + elen);
           let input = Bytes.sub_string buf 0 ilen in
           let expected = Bytes.sub_string buf ilen elen in
           pairs := (input, expected) :: !pairs);
       (* Skip the trailing '\n' separator. *)
       let _ = input_char ic in
       ()
     done
   with End_of_file -> ());
  close_in ic;
  List.rev !pairs

type outcome = Pass | Parse_error of string | Mismatch of string
type candidate = { tool : string; css : string }
type rejected_candidate = { tool : string; css : string; reason : string }

let display_css css = if css = "" then "<empty>" else css

let cascade_minify input =
  match Css.of_string ~strict:false input with
  | Error e -> Parse_error (Css.pp_parse_warning e)
  | Ok parsed -> (
      match
        Css.to_string ~minify:true ~optimize:true ~newline:false
          parsed.Css.stylesheet
      with
      | s -> Mismatch s
      | exception Invalid_argument msg ->
          Parse_error ("invalid_argument: " ^ msg))

let split_commands s =
  Re.Str.split (Re.Str.regexp_string ";;") s
  |> List.map String.trim
  |> List.filter (fun part -> part <> "")

let contains_substring ~needle haystack =
  try
    let _ = Re.Str.search_forward (Re.Str.regexp_string needle) haystack 0 in
    true
  with Not_found -> false

let default_minifier_commands =
  [
    ("esbuild", "esbuild --loader=css --minify");
    ("cleancss", "cleancss -O2 -");
    ("csso", "csso");
    ("cssnano", "cssnano");
    ("lightningcss-cli", "lightningcss --minify");
  ]

let custom_minifier_commands () =
  match Sys.getenv_opt "CASCADE_INTEROP_MINIFIERS" with
  | None | Some "" -> []
  | Some s ->
      split_commands s
      |> List.mapi (fun i command -> (Fmt.str "custom%d" (i + 1), command))

let read_all ic =
  let buf = Buffer.create 4096 in
  let bytes = Bytes.create 4096 in
  let rec loop () =
    match input ic bytes 0 (Bytes.length bytes) with
    | 0 -> Buffer.contents buf
    | n ->
        Buffer.add_subbytes buf bytes 0 n;
        loop ()
  in
  loop ()

let err_process_status label code =
  Fmt.kstr (fun s -> Error s) "%s %d" label code

let err_exit_status n stderr =
  Fmt.kstr
    (fun s -> Error s)
    "exit %d%s" n
    (if stderr = "" then "" else ": " ^ String.trim stderr)

let run_command command input =
  let ic, oc, ec = Unix.open_process_full command (Unix.environment ()) in
  output_string oc input;
  close_out oc;
  let stdout = read_all ic in
  let stderr = read_all ec in
  match Unix.close_process_full (ic, oc, ec) with
  | Unix.WEXITED 0 -> Ok (String.trim stdout)
  | Unix.WEXITED n -> err_exit_status n stderr
  | Unix.WSIGNALED n -> err_process_status "signal" n
  | Unix.WSTOPPED n -> err_process_status "stopped" n

let command_works command =
  match run_command command ".x{color:red}" with
  | Ok _ -> true
  | Error _ -> false

let available_minifiers =
  lazy
    (let commands = custom_minifier_commands () @ default_minifier_commands in
     commands |> List.filter (fun (_, command) -> command_works command))

let external_candidates input =
  Lazy.force available_minifiers
  |> List.filter_map (fun (tool, command) ->
      match run_command command input with
      | Ok css -> Some { tool; css }
      | Error _ -> None)

let all_candidate_outputs input expected =
  { tool = "lightningcss-trace"; css = expected } :: external_candidates input

let shortest_length (candidates : candidate list) =
  List.fold_left
    (fun acc ({ css; _ } : candidate) -> min acc (String.length css))
    max_int candidates

let canonical_minified css =
  if css = "" then Ok ""
  else
    match Css.of_string ~strict:false css with
    | Error e -> Error (Css.pp_parse_warning e)
    | Ok parsed -> (
        match
          Css.to_string ~minify:true ~optimize:true ~newline:false
            parsed.Css.stylesheet
        with
        | s -> Ok s
        | exception Invalid_argument msg -> Error ("invalid_argument: " ^ msg))

let at_rule_fingerprint css =
  if css = "" then Some []
  else
    try
      let sheet = Css.Stylesheet.read_stylesheet (Cursor.of_string css) in
      let rec statements acc = List.fold_left statement acc
      and statement acc = function
        | Css.Stylesheet.Rule rule -> statements acc rule.nested
        | Charset _ -> "charset" :: acc
        | Import _ -> "import" :: acc
        | Namespace _ -> "namespace" :: acc
        | Property _ -> "property" :: acc
        | Layer_decl _ -> "layer-decl" :: acc
        | Layer (_, block) -> statements ("layer" :: acc) block
        | Media (_, block) -> statements ("media" :: acc) block
        | Container (_, _, block) -> statements ("container" :: acc) block
        | Supports (_, block) -> statements ("supports" :: acc) block
        | Starting_style block -> statements ("starting-style" :: acc) block
        | When (_, block) -> statements ("when" :: acc) block
        | Else (_, block) -> statements ("else" :: acc) block
        | Supports_condition _ -> "supports-condition" :: acc
        | Origin (_, block) -> statements ("origin" :: acc) block
        | Scope (_, _, block) -> statements ("scope" :: acc) block
        | Keyframes _ -> "keyframes" :: acc
        | Webkit_keyframes _ -> "webkit-keyframes" :: acc
        | Moz_keyframes _ -> "moz-keyframes" :: acc
        | Font_face _ -> "font-face" :: acc
        | Page _ -> "page" :: acc
        | Page_with_margins _ -> "page" :: acc
        | Font_palette_values _ -> "font-palette-values" :: acc
        | View_transition _ -> "view-transition" :: acc
        | Position_try _ -> "position-try" :: acc
        | Declarations _ -> acc
        | _ -> "unknown" :: acc
      in
      Some (List.rev (statements [] sheet))
    with Cursor.Parse_error _ | Invalid_argument _ -> None

let validate_candidate input (candidate : candidate) =
  match
    ( at_rule_fingerprint input,
      at_rule_fingerprint candidate.css,
      canonical_minified input,
      canonical_minified candidate.css )
  with
  | _, _, Ok "", Ok "" -> Ok candidate
  | Some source, Some output, Ok source_css, Ok output_css
    when source = output && source_css = output_css ->
      Ok candidate
  | Some source, Some output, _, _ when source <> output ->
      Error
        {
          tool = candidate.tool;
          css = candidate.css;
          reason =
            Fmt.str "at-rule fingerprint changed: [%s] -> [%s]"
              (String.concat ", " source)
              (String.concat ", " output);
        }
  | Some _, None, _, _ ->
      Error
        {
          tool = candidate.tool;
          css = candidate.css;
          reason = "candidate output failed Cascade parser roundtrip";
        }
  | _, _, Ok source_css, Ok output_css ->
      Error
        {
          tool = candidate.tool;
          css = candidate.css;
          reason =
            Fmt.str "semantic fingerprint changed after Cascade parse: %s -> %s"
              (display_css source_css) (display_css output_css);
        }
  | _, _, Ok _, Error msg ->
      Error
        {
          tool = candidate.tool;
          css = candidate.css;
          reason = "candidate output failed Cascade semantic roundtrip: " ^ msg;
        }
  | None, _, _, _ | _, _, Error _, _ ->
      Error
        {
          tool = candidate.tool;
          css = candidate.css;
          reason = "source input failed Cascade parser roundtrip";
        }

let split_equivalent_candidates input (candidates : candidate list) =
  List.fold_right
    (fun candidate (accepted, rejected) ->
      match validate_candidate input candidate with
      | Ok candidate -> (candidate :: accepted, rejected)
      | Error rejection -> (accepted, rejection :: rejected))
    candidates ([], [])

let format_rejected_candidates input rejected =
  "UPSTREAM MINIFIER BUGS: rejected non-equivalent candidates\n"
  ^ (rejected
    |> List.map (fun rejection ->
        Fmt.str
          "    UPSTREAM BUG in: %s\n\
          \    reason: %s\n\
          \    input:  %s\n\
          \    output: %s"
          rejection.tool rejection.reason input
          (display_css rejection.css))
    |> String.concat "\n")

let format_source_parse_diagnostics input expected =
  let candidates = all_candidate_outputs input expected in
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
  Fmt.str
    "%s\n\
    \    SOURCE PARSE DIAGNOSTICS: Cascade could not parse the source fixture. \
     External outputs are diagnostics only; they are not accepted as proof of \
     semantic equivalence without a successful source parse.\n\
    \    parseable_external_outputs=%d/%d\n\
     %s"
    input !parseable (List.length candidates) reports

let classify (input, expected) =
  match cascade_minify input with
  | Parse_error msg ->
      let diagnostics = format_source_parse_diagnostics input expected in
      (Parse_error (msg ^ "\n    input diagnostics: " ^ diagnostics), [])
  | Pass -> assert false
  | Mismatch actual ->
      let candidates = all_candidate_outputs input expected in
      let candidates, rejected = split_equivalent_candidates input candidates in
      let best = shortest_length candidates in
      let outcome =
        if String.length actual <= best then Pass
        else
          let shortest =
            candidates
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
      (outcome, rejected)

let available_minifier_names () =
  match Lazy.force available_minifiers with
  | [] -> "none"
  | xs -> xs |> List.map fst |> String.concat ", "

let format_divergence i (input, expected) outcome =
  match outcome with
  | Pass -> assert false
  | Parse_error msg ->
      Fmt.str "  pair_%04d: parse error: %s\n    input: %s" i msg input
  | Mismatch actual ->
      Fmt.str
        "  pair_%04d: mismatch\n\
        \    input:    %s\n\
        \    trace:    %s\n\
        \    actual:   %s"
        i input expected actual

let verbose =
  match Sys.getenv_opt "VERBOSE" with Some "1" -> true | _ -> false

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
          (fun (_, (input, _)) -> feature_of_input input = feature)
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

let pair_cases ~total_pairs selected ~feature ~shard ~shards () =
  let selected = selected_shard selected shard in
  let total = List.length selected in
  let pass = ref 0 in
  let parse_err = ref 0 in
  let mismatch = ref 0 in
  let external_bug = ref 0 in
  let external_bug_reports = ref [] in
  let divergences = ref [] in
  List.iter
    (fun (i, pair) ->
      let outcome, rejected = classify pair in
      if rejected <> [] then begin
        external_bug := !external_bug + List.length rejected;
        external_bug_reports :=
          (i, pair, format_rejected_candidates (fst pair) rejected)
          :: !external_bug_reports
      end;
      match outcome with
      | Pass -> incr pass
      | Parse_error _ as o ->
          incr parse_err;
          divergences := (i, pair, o) :: !divergences
      | Mismatch _ as o ->
          incr mismatch;
          divergences := (i, pair, o) :: !divergences)
    selected;
  let divergences = List.rev !divergences in
  let external_bug_reports = List.rev !external_bug_reports in
  let summary =
    Fmt.str
      "minifier interop%s: %d/%d selected pass (%d total pairs; %d parse \
       errors, %d longer-than-shortest mismatches, %d external minifier bugs; \
       external minifiers: %s)"
      (Fmt.str " [%s %d/%d]" feature (shard + 1) shards)
      !pass total total_pairs !parse_err !mismatch !external_bug
      (available_minifier_names ())
  in
  print_endline summary;
  if external_bug_reports <> [] then begin
    let limit = if verbose then List.length external_bug_reports else 20 in
    let head =
      List.filteri (fun i _ -> i < limit) external_bug_reports
      |> List.map (fun (i, _pair, report) ->
          Fmt.str "  pair_%04d: external minifier bug warning\n%s" i report)
      |> String.concat "\n"
    in
    let tail =
      if List.length external_bug_reports > limit then
        Fmt.str "\n  ... (%d more upstream bugs; set VERBOSE=1 for full list)"
          (List.length external_bug_reports - limit)
      else ""
    in
    prerr_endline (summary ^ "\n" ^ head ^ tail)
  end;
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
  pairs_by_feature pairs
  |> List.filter_map (fun (feature, selected) ->
      let shards = shard_count selected in
      if shards = 0 then None
      else
        let cases =
          List.init shards (fun shard ->
              let name = Fmt.str "%02d/%02d" (shard + 1) shards in
              Alcotest.test_case name `Slow
                (pair_cases ~total_pairs selected ~feature ~shard ~shards))
        in
        Some (feature, cases))

let () =
  let pairs = read_pairs trace_path in
  Alcotest.run "lightning_minify" (grouped_cases pairs)
