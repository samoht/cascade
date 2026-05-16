(** Shared interop runner.

    Invoked as [test.exe <interop-dir>] (or from a per-tool [dune] rule on
    [@runtest]). Reads [<interop-dir>/traces/cases.trace] and runs a 3-stage
    gate per case.

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

    3-stage gate per record (against the shortest OK oracle):

    + strict byte-equality between [actual] and the shortest raw oracle;
    + structural equality via [Cascade_diff.Css_compare.equal ~mode:`Canonical];
    + [Cascade_diff.Css_compare.diff ~mode:`Tree] renders the tree diff into the
      alcotest report. *)

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
    failwith
      (Printf.sprintf "trace: short read (want %d at %d, len %d)" n pos
         (String.length s));
  String.sub s pos n

let split_record name =
  match String.index_opt name '/' with
  | Some i ->
      (String.sub name 0 i, String.sub name (i + 1) (String.length name - i - 1))
  | None -> ("interop", name)

let parse_int s where =
  try int_of_string s
  with Failure _ ->
    failwith (Printf.sprintf "trace: bad int %S at %s" s where)

let read_trace path =
  let s = read_all path in
  let header, pos = read_line_at s 0 in
  if header <> magic then
    failwith (Printf.sprintf "trace: bad magic %S (expected %S)" header magic);
  let rec loop pos acc =
    if pos >= String.length s then List.rev acc
    else
      let hdr, pos = read_line_at s pos in
      if String.length hdr = 0 then loop pos acc
      else if not (String.length hdr >= 4 && String.sub hdr 0 4 = ">>> ") then
        failwith (Printf.sprintf "trace: bad record header %S" hdr)
      else
        let parts = String.split_on_char ' ' hdr in
        let name_len, input_len, n_ok, n_err =
          match parts with
          | [ ">>>"; nl; il; o; e ] ->
              ( parse_int nl "name_len",
                parse_int il "input_len",
                parse_int o "n_ok",
                parse_int e "n_err" )
          | _ -> failwith (Printf.sprintf "trace: bad record header %S" hdr)
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
              | _ -> failwith (Printf.sprintf "trace: bad OK header %S" hdr)
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
              | _ -> failwith (Printf.sprintf "trace: bad FAIL header %S" hdr)
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

(* ===== 3-stage gate ===== *)

let cascade_minify input =
  match Cascade.Css.of_string ~strict:false input with
  | Error e -> Error (Cascade.Error.to_string e)
  | Ok { Cascade.Css.stylesheet; warnings = _ } ->
      Ok (Cascade.Css.to_string ~minify:true stylesheet)

let pick_shortest oracles =
  match oracles with
  | [] -> None
  | first :: rest ->
      Some
        (List.fold_left
           (fun best o ->
             if String.length o.raw < String.length best.raw then o else best)
           first rest)

let case record () =
  match cascade_minify record.input with
  | Error msg -> Alcotest.failf "cascade parse failure: %s" msg
  | Ok actual -> (
      match pick_shortest record.oracles with
      | None ->
          if record.errors = [] then
            Alcotest.failf "no oracle for %s" record.name
          else
            (* All upstream tools failed on this input; nothing to compare
               against, but surface upstream's reasons so they don't disappear
               into the trace. *)
            let summary =
              List.map
                (fun (e : err) -> Printf.sprintf "%s:%s" e.tool e.reason)
                record.errors
              |> String.concat " | "
            in
            Alcotest.failf
              "every upstream oracle failed - cascade has nothing to compare \
               against: %s"
              summary
      | Some shortest -> (
          if
            (* Stage 1: strict byte equality with the raw oracle. *)
            shortest.raw = actual
          then ()
          else
            (* One call covers stages 2+3: [`Canonical] returns [No_diff] if the
               cascade-canonical forms agree, otherwise the structural diff. *)
            let diff =
              Cascade_diff.Css_compare.diff ~mode:`Canonical shortest.raw actual
            in
            match diff with
            | No_diff -> ()
            | _ ->
                let buf = Buffer.create 1024 in
                Cascade_diff.Css_compare.pp ~expected:shortest.tool
                  ~actual:"cascade" buf diff;
                let oracle_summary =
                  List.map
                    (fun (o : oracle) ->
                      Printf.sprintf "%s:%d" o.tool (String.length o.raw))
                    record.oracles
                  |> String.concat " "
                in
                Alcotest.failf
                  "cascade output differs from canonical of shortest oracle\n\
                  \    cascade:  %d bytes (vs %s: %d bytes raw)\n\
                  \    oracles:  %s\n\
                   %s"
                  (String.length actual) shortest.tool
                  (String.length shortest.raw)
                  oracle_summary (Buffer.contents buf)))

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
      else failwith (Printf.sprintf "no trace found at %s" path)

let suite_name_of arg =
  if Filename.check_suffix arg ".trace" then
    Filename.basename (Filename.chop_suffix arg ".trace")
  else
    let trimmed =
      if Filename.basename arg = "" then Filename.dirname arg else arg
    in
    Filename.basename trimmed

let () =
  if Array.length Sys.argv < 2 then begin
    prerr_endline "usage: test.exe <interop-dir-or-trace> [alcotest-args...]";
    exit 2
  end;
  let arg = Sys.argv.(1) in
  let trace_path = resolve_trace_path arg in
  let records = read_trace trace_path in
  let groups = group_by_category records in
  let cases =
    List.map
      (fun (cat, rs) -> (cat, List.map (fun r -> (r.case, `Quick, case r)) rs))
      groups
  in
  let alcotest_argv =
    Array.append
      [| Sys.argv.(0) |]
      (Array.sub Sys.argv 2 (Array.length Sys.argv - 2))
  in
  Alcotest.run ~argv:alcotest_argv (suite_name_of arg) cases
