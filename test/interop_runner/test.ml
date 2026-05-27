(** Shared interop runner.

    Invoked as [test.exe <interop-dir>] (or from a per-tool [dune] rule on
    [@runtest]). Reads [<interop-dir>/traces/cases.trace] and runs the oracle
    gate below for each case.

    Trace format ([CASCADE-INTEROP/v1]):

    {v
      "CASCADE-INTEROP/v1\n"
      record*

      record :=
        ">>> <name_len> <input_len> <n_ok> <n_err>\n"
        "<name bytes>\n"        - e.g. "benchmarks/amazon"
        "<input bytes>\n"
        ok_oracle*
        err_oracle*

      ok_oracle :=
        "OK <tool_len> <css_len>\n"
        "<tool><css>\n"

      err_oracle :=
        "FAIL <tool_len> <reason_len>\n"
        "<tool><reason>\n"
    v}

    Names are [<category>/<case>] so alcotest groups by category.

    Each record is treated as a complete stylesheet, matching standalone
    optimizer/minifier inputs: closed over the fixture CSS text for
    cascade/dependency/dead-code reasoning, but still open over runtime layout
    and environment state.

    Gate per record:

    + Cascade's byte length must be no longer than the shortest cached OK
      oracle, using the oracle bytes as recorded in the trace.

    The runner deliberately does not validate oracle equivalence by feeding the
    oracle back through Cascade. SatCSS is a size-arbitrage benchmark against
    cached minifier output; using Cascade to decide which competitors count
    hides exactly the divergence this suite is meant to expose.

    Failure output includes enough context to inspect the structural diff with
    [cascade diff --diff=tree]. *)

let magic = "CASCADE-INTEROP/v1"

type oracle = { tool : string; raw : string }
type err = { tool : string; reason : string }

type record = {
  name : string;
  category : string;
  case : string;
  input : string;
  oracles : oracle list;
  errors : err list;
}

(* ===== Trace reader ===== *)

let read_all path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  close_in ic;
  Bytes.unsafe_to_string buf

let read_line_at s pos =
  let len = String.length s in
  let rec loop i =
    if i >= len then failwith "trace: unexpected EOF inside line"
    else if s.[i] = '\n' then (String.sub s pos (i - pos), i + 1)
    else loop (i + 1)
  in
  loop pos

let read_bytes_at s pos n =
  if pos + n > String.length s then
    Fmt.failwith "trace: short read (want %d at %d, len %d)" n pos
      (String.length s);
  String.sub s pos n

let split_record name =
  match String.index_opt name '/' with
  | Some i ->
      (String.sub name 0 i, String.sub name (i + 1) (String.length name - i - 1))
  | None -> ("interop", name)

let parse_int s where =
  try int_of_string s
  with Failure _ -> Fmt.failwith "trace: bad int %S at %s" s where

let read_trace path =
  let s = read_all path in
  let header, pos = read_line_at s 0 in
  if header <> magic then
    Fmt.failwith "trace: bad magic %S (expected %S)" header magic;
  let rec loop pos acc =
    if pos >= String.length s then List.rev acc
    else
      let hdr, pos = read_line_at s pos in
      if String.length hdr = 0 then loop pos acc
      else if not (String.length hdr >= 4 && String.sub hdr 0 4 = ">>> ") then
        Fmt.failwith "trace: bad record header %S" hdr
      else
        let parts = String.split_on_char ' ' hdr in
        let name_len, input_len, n_ok, n_err =
          match parts with
          | [ ">>>"; nl; il; o; e ] ->
              ( parse_int nl "name_len",
                parse_int il "input_len",
                parse_int o "n_ok",
                parse_int e "n_err" )
          | _ -> Fmt.failwith "trace: bad record header %S" hdr
        in
        let name = read_bytes_at s pos name_len in
        let pos = pos + name_len in
        let _, pos = read_line_at s pos in
        let input = read_bytes_at s pos input_len in
        let pos = pos + input_len in
        let _, pos = read_line_at s pos in
        let rec read_oks pos remaining acc =
          if remaining = 0 then (List.rev acc, pos)
          else
            let hdr, pos = read_line_at s pos in
            let parts = String.split_on_char ' ' hdr in
            let tool_len, css_len =
              match parts with
              | [ "OK"; tl; cl ] ->
                  (parse_int tl "tool_len", parse_int cl "css_len")
              | _ -> Fmt.failwith "trace: bad OK header %S" hdr
            in
            let tool = read_bytes_at s pos tool_len in
            let pos = pos + tool_len in
            let raw = read_bytes_at s pos css_len in
            let pos = pos + css_len in
            let _, pos = read_line_at s pos in
            read_oks pos (remaining - 1) ({ tool; raw } :: acc)
        in
        let oracles, pos = read_oks pos n_ok [] in
        let rec read_errs pos remaining acc =
          if remaining = 0 then (List.rev acc, pos)
          else
            let hdr, pos = read_line_at s pos in
            let parts = String.split_on_char ' ' hdr in
            let tool_len, reason_len =
              match parts with
              | [ "FAIL"; tl; rl ] ->
                  (parse_int tl "tool_len", parse_int rl "reason_len")
              | _ -> Fmt.failwith "trace: bad FAIL header %S" hdr
            in
            let tool = read_bytes_at s pos tool_len in
            let pos = pos + tool_len in
            let reason = read_bytes_at s pos reason_len in
            let pos = pos + reason_len in
            let _, pos = read_line_at s pos in
            read_errs pos (remaining - 1) ({ tool; reason } :: acc)
        in
        let errors, pos = read_errs pos n_err [] in
        let category, case = split_record name in
        loop pos ({ name; category; case; input; oracles; errors } :: acc)
  in
  loop pos []

(* ===== Oracle gate ===== *)

let cascade_minify input =
  match Cascade.Css.of_string ~strict:false input with
  | Error e -> Error (Cascade.Error.to_string e)
  | Ok { Cascade.Css.stylesheet; warnings = _ } ->
      Ok
        (stylesheet
        |> Cascade.Css.optimize ~scope:`Stylesheet
        |> Cascade.Css.to_string ~minify:true)

let pick_shortest oracles =
  match oracles with
  | [] -> None
  | first :: rest ->
      Some
        (List.fold_left
           (fun best o ->
             if String.length o.raw < String.length best.raw then o else best)
           first rest)

type case_result = Pass | Fail of string

let failf fmt = Fmt.kstr (fun msg -> Fail msg) fmt

let compute_case record : case_result =
  match cascade_minify record.input with
  | Error msg -> failf "cascade parse failure: %s" msg
  | Ok actual -> (
      let oracle_summary oracles =
        List.map
          (fun (o : oracle) -> Fmt.str "%s:%d" o.tool (String.length o.raw))
          oracles
        |> String.concat " "
      in
      let upstream_errors =
        List.map
          (fun (e : err) -> Fmt.str "%s:%s" e.tool e.reason)
          record.errors
        |> String.concat " | "
      in
      match pick_shortest record.oracles with
      | None ->
          failf "no successful oracle for %s\n    upstream_errors: %s"
            record.name
            (if upstream_errors = "" then "none" else upstream_errors)
      | Some shortest ->
          let actual_len = String.length actual in
          let shortest_len = String.length shortest.raw in
          if actual_len <= shortest_len then Pass
          else
            failf
              "cascade output longer than shortest cached OK oracle\n\
              \    cascade:  %d bytes (vs %s: %d bytes)\n\
              \    oracles:  %s\n\
              \    upstream_errors: %s"
              actual_len shortest.tool shortest_len
              (oracle_summary record.oracles)
              (if upstream_errors = "" then "none" else upstream_errors))

let case (result : case_result Lazy.t) () =
  match Lazy.force result with Pass -> () | Fail msg -> Alcotest.fail msg

(* ===== alcotest wiring ===== *)

let group_by_category records =
  let table = Hashtbl.create 8 in
  List.iter
    (fun r ->
      let prev =
        Hashtbl.find_opt table r.category |> Option.value ~default:[]
      in
      Hashtbl.replace table r.category (r :: prev))
    records;
  Hashtbl.fold (fun cat rs acc -> (cat, List.rev rs) :: acc) table []
  |> List.sort (fun (a, _) (b, _) -> compare a b)

let resolve_trace_path arg =
  (* Accept either a directory or an explicit .trace file. *)
  if Filename.check_suffix arg ".trace" then arg
  else
    let path = Filename.concat arg "traces/cases.trace" in
    if Sys.file_exists path then path
    else
      let alt = Filename.concat arg "cases.trace" in
      if Sys.file_exists alt then alt
      else Fmt.failwith "no trace found at %s" path

let suite_name_of arg =
  if Filename.check_suffix arg ".trace" then
    Filename.basename (Filename.chop_suffix arg ".trace")
  else
    let trimmed =
      if Filename.basename arg = "" then Filename.dirname arg else arg
    in
    Filename.basename trimmed

let () =
  (try Memtrace.trace_if_requested () with
  | Failure msg ->
      Fmt.epr "warning: memtrace unavailable on this runtime (%s); skipping@."
        msg
  | Sys_error msg ->
      Fmt.epr "warning: memtrace setup failed (%s); skipping@." msg);
  if Array.length Sys.argv < 2 then begin
    prerr_endline "usage: test.exe <interop-dir-or-trace> [alcotest-args...]";
    exit 2
  end;
  let arg = Sys.argv.(1) in
  let trace_path = resolve_trace_path arg in
  let records = read_trace trace_path in
  (* Precompute each record's pass/fail result across an Eio executor pool so
     cascade.minify / Css.of_string run in parallel. Alcotest then just reports
     the cached results. *)
  let results : (string, case_result) Hashtbl.t =
    Hashtbl.create (List.length records)
  in
  Eio_main.run (fun env ->
      Eio.Switch.run (fun sw ->
          let n_domains = max 1 (Domain.recommended_domain_count () - 1) in
          let pool =
            Eio.Executor_pool.create ~sw ~domain_count:n_domains
              (Eio.Stdenv.domain_mgr env)
          in
          let computed =
            Eio.Fiber.List.map
              (fun r ->
                let res =
                  Eio.Executor_pool.submit_exn pool
                    ~weight:(1.0 /. float_of_int n_domains)
                    (fun () -> compute_case r)
                in
                (r.name, res))
              records
          in
          List.iter
            (fun (name, res) -> Hashtbl.replace results name res)
            computed));
  let groups = group_by_category records in
  let cases =
    List.map
      (fun (cat, rs) ->
        ( cat,
          List.map
            (fun r ->
              let res = lazy (Hashtbl.find results r.name) in
              (r.case, `Quick, case res))
            rs ))
      groups
  in
  let alcotest_argv =
    Array.append
      [| Sys.argv.(0) |]
      (Array.sub Sys.argv 2 (Array.length Sys.argv - 2))
  in
  Alcotest.run ~argv:alcotest_argv (suite_name_of arg) cases
