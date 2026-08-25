(* Deterministic observations used to compare two Cascade builds.

   Keep build-specific logic here. [run.sh] builds this executable in each
   worktree, runs the same named profile against both, and diffs the record
   streams. *)

open Cascade

let fail fmt =
  Fmt.kstr
    (fun message ->
      prerr_endline message;
      exit 2)
    fmt

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let fingerprint value =
  Fmt.str "%d:%s" (String.length value) (Digest.to_hex (Digest.string value))

let rec css_files dir acc =
  Array.fold_left
    (fun acc entry ->
      let path = Filename.concat dir entry in
      if Sys.is_directory path then css_files path acc
      else if Filename.check_suffix path ".css" then path :: acc
      else acc)
    acc (Sys.readdir dir)

(* Trace_pairs framing, per test/interop/lightning/trace_pairs.ml. This profile
   needs only the source input, not an external minifier's candidates. *)
let read_pair_inputs path =
  let ic = open_in_bin path in
  let read_exact length =
    let bytes = Bytes.create length in
    really_input ic bytes 0 length;
    Bytes.unsafe_to_string bytes
  in
  let separator ~eof =
    match input_char ic with
    | '\n' -> ()
    | _ -> fail "invalid trace separator in %s" path
    | exception End_of_file when eof -> ()
    | exception End_of_file -> fail "truncated trace record in %s" path
  in
  let inputs = ref [] in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      try
        while true do
          let header = input_line ic in
          Scanf.sscanf header ">>> %d %d %d"
            (fun input_length ok_count fail_count ->
              inputs := read_exact input_length :: !inputs;
              separator ~eof:(ok_count = 0 && fail_count = 0);
              for index = 1 to ok_count do
                let header = input_line ic in
                Scanf.sscanf header "OK %d %d" (fun tool_length css_length ->
                    ignore (read_exact tool_length);
                    ignore (read_exact css_length);
                    separator ~eof:(index = ok_count && fail_count = 0))
              done;
              for index = 1 to fail_count do
                let header = input_line ic in
                Scanf.sscanf header "FAIL %d %d %d"
                  (fun tool_length css_length reason_length ->
                    ignore (read_exact tool_length);
                    ignore (read_exact css_length);
                    ignore (read_exact reason_length);
                    separator ~eof:(index = fail_count))
              done)
        done
      with End_of_file -> List.rev !inputs)

let custom_names stylesheet =
  let names = ref [] in
  Css.Stylesheet.iter_declarations
    (List.iter (fun declaration ->
         match Css.Variables.custom_declaration_name declaration with
         | Some name when not (List.mem name !names) -> names := name :: !names
         | Some _ | None -> ()))
    stylesheet;
  !names

let declaration_census stylesheet =
  let custom = ref 0
  and layered = ref 0
  and tagged = ref 0
  and colours = ref 0 in
  Css.Stylesheet.iter_declarations
    (List.iter (fun declaration ->
         match Css.Variables.custom_declaration_name declaration with
         | None -> ()
         | Some _ -> (
             incr custom;
             if Css.custom_declaration_layer declaration <> None then
               incr layered;
             if Css.meta_of_declaration declaration <> None then incr tagged;
             let value =
               Css.Declaration.string_of_value ~minify:true declaration
             in
             try
               let cursor = Cursor.of_string value in
               ignore (Css.Values.read_color cursor);
               Cursor.ws cursor;
               Cursor.expect_eof cursor;
               incr colours
             with Error.Parse_error _ | Invalid_argument _ | Failure _ -> ())))
    stylesheet;
  (!custom, !layered, !tagged, !colours)

let report_outputs label input =
  match Css.of_string ~strict:false input with
  | Error _ -> Fmt.pr "%s\tPARSE_ERROR\n" label
  | Ok { Css.stylesheet; _ } ->
      let keep_vars = custom_names stylesheet in
      let kept = Css.inline_vars ~keep_vars stylesheet in
      let removed = Css.inline_vars stylesheet in
      let before = declaration_census stylesheet in
      let after = declaration_census kept in
      let census (a, b, c, d) = Fmt.str "%d/%d/%d/%d" a b c d in
      Fmt.pr
        "%s\tmin=%s\tpretty=%s\tinline-keep=%s\tinline-all=%s\tpre=%s\tpost=%s\n"
        label
        (fingerprint (Css.to_string ~minify:true stylesheet))
        (fingerprint (Css.to_string ~minify:false stylesheet))
        (fingerprint (Css.to_string ~minify:true kept))
        (fingerprint (Css.to_string ~minify:true removed))
        (census before) (census after)

let run_outputs root =
  let interop = Filename.concat root "test/interop" in
  if not (Sys.is_directory interop) then
    fail "interop corpus not found: %s" interop;
  let files = List.sort String.compare (css_files interop []) in
  List.iter
    (fun path ->
      let prefix_length = String.length root + 1 in
      let relative =
        String.sub path prefix_length (String.length path - prefix_length)
      in
      report_outputs ("file:" ^ relative) (read_file path))
    files;
  let trace =
    Filename.concat root "test/interop/lightning/traces/minify.pairs"
  in
  let inputs = read_pair_inputs trace in
  List.iteri
    (fun index input -> report_outputs (Fmt.str "lightning:%06d" index) input)
    inputs;
  Fmt.epr "outputs: %d CSS files, %d Lightning inputs\n%!" (List.length files)
    (List.length inputs)

module Candidates = struct
  let state = ref 0x2545F4914F6CDD1DL
  let reseed () = state := 0x2545F4914F6CDD1DL

  let next_int bound =
    let value = !state in
    let value = Int64.logxor value (Int64.shift_left value 13) in
    let value = Int64.logxor value (Int64.shift_right_logical value 7) in
    let value = Int64.logxor value (Int64.shift_left value 17) in
    state := value;
    Int64.to_int (Int64.logand (Int64.shift_right_logical value 11) 0x3fffffffL)
    mod bound

  let small_selectors = [| ".a"; ".bb"; ".cccccccc"; "#d" |]
  let small_atoms = [| "color:red"; "color:blue"; "margin:1px"; "padding:0" |]

  let selectors =
    [| ".a"; ".bb"; ".cccccccc"; "#d"; "p"; ".e.f"; "a:hover"; ".g>.h" |]

  let atoms =
    [|
      "color:red";
      "color:blue";
      "margin:1px";
      "margin:2px";
      "padding:0";
      "display:flex";
      "display:block";
      "font-weight:700";
      "color:red!important";
    |]

  let bodies atoms =
    let count = Array.length atoms in
    let result = ref [] in
    for first = count - 1 downto 0 do
      for second = count - 1 downto 0 do
        if first <> second then
          result := [ atoms.(first); atoms.(second) ] :: !result
      done;
      result := [ atoms.(first) ] :: !result
    done;
    Array.of_list !result

  let small_bodies = bodies small_atoms
  let bodies = bodies atoms
  let source = Buffer.create 512

  let sheet_of selectors bodies picks =
    Buffer.clear source;
    List.iter
      (fun (selector, body) ->
        Buffer.add_string source selectors.(selector);
        Buffer.add_char source '{';
        List.iteri
          (fun index declaration ->
            if index > 0 then Buffer.add_char source ';';
            Buffer.add_string source declaration)
          bodies.(body);
        Buffer.add_char source '}')
      picks;
    Buffer.contents source

  let filler_rules =
    let buffer = Buffer.create 4096 in
    let formatter = Fmt.with_buffer buffer in
    for index = 0 to 128 do
      Fmt.pf formatter ".f%d{left:%dpx;top:%dpx}" index index index
    done;
    Buffer.contents buffer

  let rules css =
    match Css.of_string ~strict:false css with
    | Error _ -> []
    | Ok { stylesheet; _ } ->
        List.filter_map
          (function Stylesheet.Rule rule -> Some rule | _ -> None)
          (Css.statements stylesheet)

  let kind_name = function
    | Rule_rewrite.Identical_body -> "identical-body"
    | Same_selector -> "same-selector"
    | Exact_shared_declarations -> "exact-shared"
    | Selector_branch_inline -> "selector-inline"
    | Default_factoring -> "default-factoring"

  let candidate_count = ref 0
  let default_count = ref 0
  let sheet_count = ref 0
  let observation = Buffer.create 4096
  let observation_formatter = Fmt.with_buffer observation

  let dump_candidates label ctx css =
    let rules = rules css in
    if rules = [] then Fmt.pr "%s\tNO_RULES\n" label
    else begin
      let candidates =
        rules |> Rule_graph.of_rules
        |> Rule_candidate.enumerate ~ctx ~finalize:Fun.id
      in
      Buffer.clear observation;
      List.iter
        (fun (candidate : Rule_rewrite.candidate) ->
          incr candidate_count;
          if candidate.kind = Rule_rewrite.Default_factoring then
            incr default_count;
          Fmt.pf observation_formatter "%s|%d|" (kind_name candidate.kind)
            candidate.generation;
          List.iter
            (fun id ->
              Fmt.pf observation_formatter "%d," (Rule_graph.Node_id.to_int id))
            candidate.consume;
          Fmt.pf observation_formatter "|%d|" candidate.saving;
          List.iter
            (fun rule ->
              Buffer.add_string observation
                (Pp.to_string ~minify:true Stylesheet.pp_rule rule))
            candidate.produce;
          Buffer.add_char observation '\n')
        candidates;
      incr sheet_count;
      Fmt.pr "%s\t%s\n" label (fingerprint (Buffer.contents observation))
    end

  let exhaustive_pairs () =
    let body_count = Array.length small_bodies in
    let count = Array.length small_selectors * body_count in
    for first = 0 to count - 1 do
      for second = 0 to count - 1 do
        let picks =
          [
            (first / body_count, first mod body_count);
            (second / body_count, second mod body_count);
          ]
        in
        let css = sheet_of small_selectors small_bodies picks in
        let label = Fmt.str "pair:%02d:%02d" first second in
        dump_candidates (label ^ ":fragment") Ctx.fragment css;
        dump_candidates (label ^ ":aggressive")
          (Ctx.v ~aggressive:true `Fragment)
          css
      done
    done

  let exhaustive_triples () =
    let body_count = Array.length small_bodies in
    let count = Array.length small_selectors * body_count in
    for first = 0 to count - 1 do
      for second = 0 to count - 1 do
        for third = 0 to count - 1 do
          let picks =
            [
              (first / body_count, first mod body_count);
              (second / body_count, second mod body_count);
              (third / body_count, third mod body_count);
            ]
          in
          dump_candidates
            (Fmt.str "triple:%02d:%02d:%02d:fragment" first second third)
            Ctx.fragment
            (sheet_of small_selectors small_bodies picks);
          dump_candidates
            (Fmt.str "triple:%02d:%02d:%02d:aggressive" first second third)
            (Ctx.v ~aggressive:true `Fragment)
            (sheet_of small_selectors small_bodies picks)
        done
      done
    done

  let random_sheets ~label ~count ~size ~large =
    let selector_count = Array.length selectors in
    let body_count = Array.length bodies in
    for index = 0 to count - 1 do
      let picks =
        List.init size (fun _ -> (next_int selector_count, next_int body_count))
      in
      let css = sheet_of selectors bodies picks in
      let css = if large then css ^ filler_rules else css in
      dump_candidates (Fmt.str "%s:%06d" label index) Ctx.fragment css
    done

  let run ~quick =
    reseed ();
    exhaustive_pairs ();
    if not quick then exhaustive_triples ();
    random_sheets ~label:"random-4"
      ~count:(if quick then 1_000 else 200_000)
      ~size:4 ~large:false;
    random_sheets ~label:"random-5"
      ~count:(if quick then 500 else 200_000)
      ~size:5 ~large:false;
    random_sheets ~label:"random-7"
      ~count:(if quick then 200 else 100_000)
      ~size:7 ~large:false;
    random_sheets ~label:"large-5"
      ~count:(if quick then 50 else 20_000)
      ~size:5 ~large:true;
    Fmt.epr "candidates: %d sheets, %d candidates, %d default-factoring\n%!"
      !sheet_count !candidate_count !default_count
end

let usage () =
  fail "usage: driver outputs CORPUS_ROOT\n       driver candidates [--quick]"

let () =
  match Array.to_list Sys.argv with
  | [ _; "outputs"; root ] -> run_outputs root
  | [ _; "candidates" ] -> Candidates.run ~quick:false
  | [ _; "candidates"; "--quick" ] -> Candidates.run ~quick:true
  | _ -> usage ()
