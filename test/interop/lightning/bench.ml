(* Throwaway: walk pairs with progress so we can spot pathological cases. *)
open Cascade

let trace_path = Filename.concat "traces" "minify.pairs"

let int_env name default =
  match Sys.getenv_opt name with
  | None -> default
  | Some value -> (
      match int_of_string value with
      | value -> value
      | exception Failure _ -> default)

let () =
  let pairs = Trace_pairs.read trace_path in
  let start = max 0 (int_env "START" 0) in
  let limit = int_env "LIMIT" max_int in
  let pairs =
    pairs
    |> List.mapi (fun i pair -> (i, pair))
    |> List.filter (fun (i, _) -> i >= start && i < start + limit)
  in
  let total = List.length pairs in
  let pass = ref 0 in
  let fail = ref 0 in
  let parse_err = ref 0 in
  let t_global = Unix.gettimeofday () in
  List.iter
    (fun (i, (input, expected)) ->
      let t0 = Unix.gettimeofday () in
      let trace_phase = Sys.getenv_opt "TRACE_PHASE" = Some "1" in
      if trace_phase then Fmt.epr "pair_%04d parse\n%!" i;
      (match Css.of_string ~strict:false input with
      | Error _ -> incr parse_err
      | Ok parsed -> (
          if trace_phase then Fmt.epr "pair_%04d print\n%!" i;
          match
            Css.to_string ~minify:true ~optimize:true ~newline:false
              parsed.Css.stylesheet
          with
          | s when s = expected -> incr pass
          | _ -> incr fail
          | exception _ -> incr fail));
      let dt = Unix.gettimeofday () -. t0 in
      if dt > 0.05 then
        Fmt.epr "slow pair_%04d: %.2fs (input %d bytes): %s\n%!" i dt
          (String.length input)
          (String.sub input 0 (min 80 (String.length input)));
      if (i - start) mod 100 = 99 then
        Fmt.epr
          "... pair_%04d (%d/%d)  pass=%d fail=%d parse_err=%d  (%.1fs)\n%!" i
          (i - start + 1)
          total !pass !fail !parse_err
          (Unix.gettimeofday () -. t_global))
    pairs;
  Fmt.pr "pass=%d fail=%d parse_err=%d total=%d (%.1fs)\n" !pass !fail
    !parse_err total
    (Unix.gettimeofday () -. t_global)
