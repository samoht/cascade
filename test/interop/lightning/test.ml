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

    The suite is split into slow Alcotest cases by broad CSS feature. Each case
    still batches many pairs so logs stay useful without creating thousands of
    tiny tests. The summary surfaces enough longer-than-industry outputs to
    drive arbitrage; for full drilldown re-run with [VERBOSE=1] in the env.

    Extra minifier commands can be supplied with [CASCADE_INTEROP_MINIFIERS],
    separated by [;;]. Commands must read CSS from stdin and write minified CSS
    to stdout. Example:

    {v
    CASCADE_INTEROP_MINIFIERS='esbuild --loader=css --minify;;cleancss -O2 -'
    v}

    For local iteration, run one Alcotest case, e.g. [interop/color] or
    [interop/animation]. *)

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

let cascade_minify input =
  match Css.of_string input with
  | Error e -> Parse_error (Css.pp_parse_error e)
  | Ok css -> (
      match Css.to_string ~minify:true ~optimize:true ~newline:false css with
      | s -> Mismatch s
      | exception Invalid_argument msg ->
          Parse_error ("invalid_argument: " ^ msg))

let split_commands s =
  Re.Str.split (Re.Str.regexp_string ";;") s
  |> List.map String.trim
  |> List.filter (fun part -> part <> "")

let default_minifier_commands =
  [
    ("esbuild", "esbuild --loader=css --minify");
    ("cleancss", "cleancss -O2 -");
    ("csso", "csso");
    ("cssnano", "cssnano");
    ("lightningcss-cli", "lightningcss --minify");
  ]

let env_minifier_commands () =
  match Sys.getenv_opt "CASCADE_INTEROP_MINIFIERS" with
  | None | Some "" -> []
  | Some s ->
      split_commands s
      |> List.mapi (fun i command -> (Printf.sprintf "env%d" (i + 1), command))

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

let run_command command input =
  let ic, oc, ec = Unix.open_process_full command (Unix.environment ()) in
  output_string oc input;
  close_out oc;
  let stdout = read_all ic in
  let stderr = read_all ec in
  match Unix.close_process_full (ic, oc, ec) with
  | Unix.WEXITED 0 ->
      let css = String.trim stdout in
      if css = "" then Error "empty stdout" else Ok css
  | Unix.WEXITED n ->
      Error
        (Printf.sprintf "exit %d%s" n
           (if stderr = "" then "" else ": " ^ String.trim stderr))
  | Unix.WSIGNALED n -> Error (Printf.sprintf "signal %d" n)
  | Unix.WSTOPPED n -> Error (Printf.sprintf "stopped %d" n)

let command_works command =
  match run_command command ".x{color:red}" with
  | Ok _ -> true
  | Error _ -> false

let available_minifiers =
  lazy
    (let commands = env_minifier_commands () @ default_minifier_commands in
     commands |> List.filter (fun (_, command) -> command_works command))

let external_candidates input =
  Lazy.force available_minifiers
  |> List.filter_map (fun (tool, command) ->
      match run_command command input with
      | Ok css -> Some { tool; css }
      | Error _ -> None)

let shortest_length candidates =
  List.fold_left
    (fun acc { css; _ } -> min acc (String.length css))
    max_int candidates

let classify (input, expected) =
  match cascade_minify input with
  | Parse_error _ as e -> e
  | Pass -> assert false
  | Mismatch actual ->
      let candidates =
        { tool = "lightningcss-trace"; css = expected }
        :: external_candidates input
      in
      let best = shortest_length candidates in
      if String.length actual <= best then Pass
      else
        let shortest =
          candidates
          |> List.filter (fun c -> String.length c.css = best)
          |> List.map (fun c -> c.tool ^ ":" ^ c.css)
          |> String.concat " | "
        in
        Mismatch
          (Printf.sprintf "%s\n    shortest: %s\n    actual_len=%d best_len=%d"
             actual shortest (String.length actual) best)

let available_minifier_names () =
  match Lazy.force available_minifiers with
  | [] -> "none"
  | xs -> xs |> List.map fst |> String.concat ", "

let format_divergence i (input, expected) outcome =
  match outcome with
  | Pass -> assert false
  | Parse_error msg ->
      Printf.sprintf "  pair_%04d: parse error: %s\n    input: %s" i msg input
  | Mismatch actual ->
      Printf.sprintf
        "  pair_%04d: mismatch\n\
        \    input:    %s\n\
        \    trace:    %s\n\
        \    actual:   %s"
        i input expected actual

let verbose =
  match Sys.getenv_opt "VERBOSE" with Some "1" -> true | _ -> false

let contains_substring ~needle haystack =
  try
    let _ = Re.Str.search_forward (Re.Str.regexp_string needle) haystack 0 in
    true
  with Not_found -> false

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

let selected_pairs ?feature pairs =
  let indexed = List.mapi (fun i pair -> (i, pair)) pairs in
  match feature with
  | None -> indexed
  | Some feature ->
      List.filter
        (fun (_, (input, _)) -> feature_of_input input = feature)
        indexed

let test_pairs ?feature () =
  let pairs = read_pairs trace_path in
  let selected = selected_pairs ?feature pairs in
  let total = List.length selected in
  let pass = ref 0 in
  let parse_err = ref 0 in
  let mismatch = ref 0 in
  let divergences = ref [] in
  List.iter
    (fun (i, pair) ->
      match classify pair with
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
    Printf.sprintf
      "minifier interop%s: %d/%d selected pass (%d total pairs; %d parse \
       errors, %d longer-than-shortest mismatches; external minifiers: %s)"
      (match feature with None -> "" | Some f -> " [" ^ f ^ "]")
      !pass total (List.length pairs) !parse_err !mismatch
      (available_minifier_names ())
  in
  print_endline summary;
  if divergences <> [] then begin
    let limit = if verbose then List.length divergences else 20 in
    let head =
      List.filteri (fun i _ -> i < limit) divergences
      |> List.map (fun (i, pair, o) -> format_divergence i pair o)
      |> String.concat "\n"
    in
    let tail =
      if List.length divergences > limit then
        Printf.sprintf "\n  ... (%d more; set VERBOSE=1 for full list)"
          (List.length divergences - limit)
      else ""
    in
    Alcotest.failf "%s\n%s%s" summary head tail
  end

let () =
  Alcotest.run "lightning_minify"
    [
      ( "interop",
        List.map
          (fun feature ->
            Alcotest.test_case feature `Slow (test_pairs ~feature))
          features );
    ]
