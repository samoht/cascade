(** Cascade reading back what it writes.

    Two properties, over every input the repo already ships:

    - {b readable}: re-reading an emission raises no diagnostic the input did
      not already raise. A diagnostic on our own bytes is a construct browsers
      drop too, so the emission is a rendering change, not a shorter spelling.
    - {b stable}: emitting the re-read gives the same bytes. One outside pass is
      the contract, because the optimizer already runs its own fixpoint loop; a
      second pass that moves is a first pass that stopped early.

    The two are independent. A rejected emission usually also moves, because the
    reader drops what it rejects, but an emission can read back as a different
    construct and then sit still: [@scope to (...)] prints as [@scopeto (...)],
    an unknown at-rule that round-trips opaquely for ever.

    Inputs come from corpora other harnesses own: the keithamus minifier
    fixtures (both the sources and the upstream minified answers), the
    Lightning-derived trace inputs, the shipped example sheets, and the
    spec-derived property grammar vectors. Nothing here is generated, and
    nothing here needs regenerating.

    Every output mode the CLI offers is checked. None of them is a fixed point
    today, so checking only [--minify] would be picking the mode that makes the
    smallest list.

    Failures are reduced before they are reported. A candidate is always a
    concatenation of substrings of the original input, never a re-print of its
    parse: re-printing would hide exactly the emission bugs this looks for. A
    reduction is kept only when it still fails the same way, judged by the
    reader diagnostics it introduces and by whether it moves on the second pass.
    Reduction runs under a step budget, so a large input can end up partly
    reduced rather than minimal. *)

open Cascade

(* {1 Corpora} *)

type input = { name : string; css : string }

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

(* Dune runs the test from its own build directory; [dune exec] runs it from the
   project root. Both reach the corpora from one repo-relative path. *)
let locate rel =
  let from_build = Filename.concat ".." (Filename.concat ".." rel) in
  if Sys.file_exists from_build then Some from_build
  else if Sys.file_exists rel then Some rel
  else None

let subdirs path =
  Sys.readdir path |> Array.to_list
  |> List.filter (fun name -> Sys.is_directory (Filename.concat path name))
  |> List.sort String.compare

let minify_tests_inputs () =
  match locate "test/interop/css-minify-tests/traces/tests" with
  | None -> []
  | Some root ->
      subdirs root
      |> List.concat_map (fun category ->
          let cat_dir = Filename.concat root category in
          subdirs cat_dir
          |> List.concat_map (fun id ->
              let dir = Filename.concat cat_dir id in
              [ "source.css"; "expected.css" ]
              |> List.filter_map (fun file ->
                  let path = Filename.concat dir file in
                  if Sys.file_exists path then
                    let name = String.concat "/" [ category; id; file ] in
                    Some { name; css = read_file path }
                  else None)))

let lightning_inputs () =
  match locate "test/interop/lightning/traces/minify.pairs" with
  | None -> []
  | Some path ->
      Trace_pairs.read path
      |> List.mapi (fun i record ->
          {
            name = String.concat "" [ "record "; string_of_int i ];
            css = record.Trace_pairs.input;
          })

let example_inputs () =
  match locate "test/examples" with
  | None -> []
  | Some root ->
      Sys.readdir root |> Array.to_list |> List.sort String.compare
      |> List.filter_map (fun name ->
          if Filename.check_suffix name ".css" then
            Some { name; css = read_file (Filename.concat root name) }
          else None)

(* The manifest vectors are bare property values; each becomes the one
   declaration of a rule. Negatives are inputs too: whatever the reader keeps of
   one still has to survive its own printing. *)
let grammar_inputs () =
  let declaration property value =
    String.concat "" [ "a{"; property; ":"; value; "}" ]
  in
  let vectors kind property values =
    List.mapi
      (fun i value ->
        {
          name = String.concat "" [ property; " "; kind; string_of_int i ];
          css = declaration property value;
        })
      values
  in
  Cascade_spec_inventory.Property_grammar.rows
  |> List.concat_map (fun (row : Cascade_spec_inventory.Property_grammar.row) ->
      vectors "+" row.property row.positives
      @ vectors "-" row.property row.negatives)

(* An empty or shrunken corpus and a clean corpus look the same from outside, so
   each one carries the size the repo ships. *)
type corpus = { corpus_name : string; floor : int; load : unit -> input list }

let corpora =
  [
    {
      corpus_name = "css-minify-tests";
      floor = 700;
      load = minify_tests_inputs;
    };
    { corpus_name = "lightning"; floor = 2500; load = lightning_inputs };
    { corpus_name = "examples"; floor = 2; load = example_inputs };
    { corpus_name = "property-grammar"; floor = 2000; load = grammar_inputs };
  ]

(* {1 Modes} *)

type mode = {
  label : string;
  optimize : bool;
  scope : Optimize.scope;
  lossless : bool;
  enforce_spec : bool;
  minify : bool;
}

let base =
  {
    label = "cascade fmt";
    optimize = false;
    scope = `Fragment;
    lossless = false;
    enforce_spec = false;
    minify = false;
  }

let minified =
  { base with label = "cascade fmt --minify"; optimize = true; minify = true }

let modes =
  [
    base;
    minified;
    {
      minified with
      label = "cascade fmt --minify --scope=stylesheet";
      scope = `Stylesheet;
    };
    {
      minified with
      label = "cascade fmt --minify --enforce-spec";
      enforce_spec = true;
    };
    { minified with label = "cascade fmt --minify --lossless"; lossless = true };
  ]

let read mode css =
  Css.of_string ~strict:false ~enforce_spec:mode.enforce_spec css

let emit mode stylesheet =
  let stylesheet =
    if mode.optimize then
      Css.optimize ~scope:mode.scope ~lossless:mode.lossless
        ~enforce_spec:mode.enforce_spec stylesheet
    else stylesheet
  in
  Css.to_string ~minify:mode.minify ~lossless:mode.lossless
    ~enforce_spec:mode.enforce_spec stylesheet

(* {1 The check} *)

(* Diagnostics are compared without their spans: the same complaint about the
   same construct lands at a different offset once the text is minified. *)
let shape (e : Error.t) =
  String.concat ""
    [ String.concat "/" e.path; ": "; Pp.to_string Error.pp_kind e.kind ]

let rec take_out x = function
  | [] -> None
  | y :: tl when String.equal x y -> Some tl
  | y :: tl -> Option.map (fun rest -> y :: rest) (take_out x tl)

let introduced ~before ~after =
  let keep (pool, extra) s =
    match take_out s pool with
    | Some pool -> (pool, extra)
    | None -> (pool, s :: extra)
  in
  List.fold_left keep (before, []) after |> snd |> List.rev

type finding = {
  emitted : string;
  again : string option;  (** second pass, when it differs from the first *)
  diagnostics : string list;  (** what re-reading the emission newly raised *)
  fatal : string option;  (** the reader or the printer gave up *)
}

(* [read] and [emit] are arguments so that the calibration cases can drive the
   same oracle with an emitter that is wrong on purpose. *)
let attempt ~(read : string -> (Css.parse, Error.t) result)
    ~(emit : Css.t -> string) css =
  match read css with
  | Error _ -> None (* Cascade never emitted this, so it owes it nothing. *)
  | Ok parsed -> (
      let emitted = emit parsed.stylesheet in
      let before = List.map shape parsed.warnings in
      match read emitted with
      | Error e ->
          Some
            {
              emitted;
              again = None;
              diagnostics = [];
              fatal = Some (Error.to_string e);
            }
      | Ok back ->
          let diagnostics =
            introduced ~before ~after:(List.map shape back.warnings)
          in
          let second = emit back.stylesheet in
          let again =
            if String.equal second emitted then None else Some second
          in
          if Option.is_none again && List.is_empty diagnostics then None
          else Some { emitted; again; diagnostics; fatal = None })

(* The sweep runs before Alcotest sees a single case, so one input that makes a
   printer raise would take the whole report down with it. *)
let check ~read ~emit css =
  try attempt ~read ~emit css
  with Invalid_argument msg | Failure msg ->
    Some
      {
        emitted = "";
        again = None;
        diagnostics = [];
        fatal = Some (String.concat "" [ "raised: "; msg ]);
      }

(* What makes two failures the same failure, for the reducer. *)
let signature f =
  String.concat "|"
    ((match f.fatal with Some _ -> "fatal" | None -> "read")
    :: (match f.again with Some _ -> "moves" | None -> "stands")
    :: f.diagnostics)

(* {1 Reduction} *)

(* Index just past the construct starting at [i], so that a delimiter inside a
   comment or a string is not mistaken for structure. *)
let skip text ~stop i =
  let rec closing quote j =
    if j >= stop then stop
    else if Char.equal text.[j] '\\' then closing quote (j + 2)
    else if Char.equal text.[j] quote then j + 1
    else closing quote (j + 1)
  in
  let rec comment j =
    if j + 1 >= stop then stop
    else if Char.equal text.[j] '*' && Char.equal text.[j + 1] '/' then j + 2
    else comment (j + 1)
  in
  match text.[i] with
  | '/' when i + 1 < stop && Char.equal text.[i + 1] '*' -> comment (i + 2)
  | ('"' | '\'') as quote -> closing quote (i + 1)
  | '\\' -> min stop (i + 2)
  | _ -> i + 1

(* Index just past the '}' closing the block whose body starts at [i]. *)
let rec skip_block text ~stop i =
  if i >= stop then stop
  else
    match text.[i] with
    | '}' -> i + 1
    | '{' -> skip_block text ~stop (skip_block text ~stop (i + 1))
    | _ -> skip_block text ~stop (skip text ~stop i)

(* Every statement span at every nesting level, as (offset, length) pairs. *)
let rec spans text ~start ~stop acc =
  let rec walk i piece acc =
    if i >= stop then if i > piece then (piece, i - piece) :: acc else acc
    else
      match text.[i] with
      | ';' -> walk (i + 1) (i + 1) ((piece, i + 1 - piece) :: acc)
      | '{' ->
          let after = skip_block text ~stop (i + 1) in
          let body_end =
            if after > i + 1 && Char.equal text.[after - 1] '}' then after - 1
            else after
          in
          let acc = spans text ~start:(i + 1) ~stop:body_end acc in
          walk after after ((piece, after - piece) :: acc)
      | _ -> walk (skip text ~stop i) piece acc
  in
  walk start start acc

let without text (start, len) =
  String.concat ""
    [
      String.sub text 0 start;
      String.sub text (start + len) (String.length text - start - len);
    ]

let longest_first (_, a) (_, b) = Int.compare b a

(* Delete one span at a time, largest first, keeping a deletion only when the
   result still fails the same way. *)
let reduce ~reproduces text =
  let rec loop text budget =
    let spent = ref 0 in
    let smaller (start, len) =
      if !spent >= budget || len = 0 || len >= String.length text then false
      else (
        incr spent;
        reproduces (without text (start, len)))
    in
    let candidates =
      spans text ~start:0 ~stop:(String.length text) []
      |> List.sort longest_first
    in
    if budget <= 0 then text
    else
      match List.find_opt smaller candidates with
      | None -> text
      | Some span -> loop (without text span) (budget - !spent)
  in
  loop text 4000

let minimise ~read ~emit css finding =
  let target = signature finding in
  let reproduces candidate =
    match check ~read ~emit candidate with
    | None -> false
    | Some f -> String.equal (signature f) target
  in
  reduce ~reproduces css

(* {1 Reporting} *)

(* One entry per input that failed, carrying the reduced text rather than the
   fixture it came from. *)
type failure = { input : input; reduced : string; finding : finding }

type result = {
  mode : mode;
  corpus : corpus;
  exercised : int;
  failures : failure list;
}

let sweep mode (corpus, inputs) =
  let read = read mode and emit = emit mode in
  let failures =
    List.filter_map
      (fun input ->
        Option.map
          (fun finding ->
            { input; reduced = minimise ~read ~emit input.css finding; finding })
          (check ~read ~emit input.css))
      inputs
  in
  { mode; corpus; exercised = List.length inputs; failures }

let add_line buf label value =
  Buffer.add_string buf "    ";
  Buffer.add_string buf label;
  Buffer.add_string buf " : ";
  Buffer.add_string buf (String.escaped value);
  Buffer.add_char buf '\n'

let add_failure buf failure =
  Buffer.add_string buf "  ";
  Buffer.add_string buf failure.input.name;
  Buffer.add_char buf '\n';
  add_line buf "input " failure.reduced;
  add_line buf "pass 1" failure.finding.emitted;
  Option.iter (add_line buf "pass 2") failure.finding.again;
  Option.iter (add_line buf "reader") failure.finding.fatal;
  List.iter (add_line buf "reader") failure.finding.diagnostics

let headline result =
  String.concat ""
    [
      string_of_int (List.length result.failures);
      " of ";
      string_of_int result.exercised;
      " ";
      result.corpus.corpus_name;
      " inputs are not a fixed point of `";
      result.mode.label;
      "`";
    ]

(* Alcotest shows the message of the first failing case only, so the whole list
   goes to the console before the run and each case keeps its own count. *)
(* A corpus with no failures says nothing in the list below, so its size is
   stated here: a silent corpus is a clean one only if it was read at all. *)
let add_exercised buf loaded =
  Buffer.add_string buf "exercised";
  List.iter
    (fun (corpus, inputs) ->
      Buffer.add_char buf ' ';
      Buffer.add_string buf corpus.corpus_name;
      Buffer.add_char buf '=';
      Buffer.add_string buf (string_of_int (List.length inputs)))
    loaded;
  Buffer.add_string buf ", in every mode\n"

let print_report loaded results =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "cascade does not read back what it writes\n";
  add_exercised buf loaded;
  List.iter
    (fun result ->
      match result.failures with
      | [] -> ()
      | failures ->
          Buffer.add_char buf '\n';
          Buffer.add_string buf (headline result);
          Buffer.add_char buf '\n';
          List.iter (add_failure buf) failures)
    results;
  Buffer.add_char buf '\n';
  print_string (Buffer.contents buf)

(* {1 Calibration}

   A report of nothing is a clean run or a blind oracle, and the two look alike
   from outside. These drive the same oracle with emitters that are wrong on
   purpose, so they keep discriminating after the defects in the corpora are
   fixed. *)

let honest = emit minified
let reader = read minified

(* The emission class this check exists for: a CSS-wide keyword standing as one
   component of a value the reader only accepts it as the whole of. *)
let keyword_component _ = "a{margin:1px inherit}"

(* Readable but never settling: each pass measures the text the pass before it
   wrote. *)
let growing stylesheet =
  let text = honest stylesheet in
  String.concat ""
    [ text; "b{width:"; string_of_int (String.length text); "px}" ]

let calibration_input = "a{margin:1px 2px}"

let honest_emitter_passes () =
  match check ~read:reader ~emit:honest calibration_input with
  | None -> ()
  | Some finding ->
      Alcotest.failf "%s is not a fixed point: %s" calibration_input
        finding.emitted

let unreadable_emitter_caught () =
  match check ~read:reader ~emit:keyword_component calibration_input with
  | None -> Alcotest.fail "an emission the reader rejects went unreported"
  | Some finding ->
      if List.is_empty finding.diagnostics && Option.is_none finding.fatal then
        Alcotest.failf "no reader diagnostic reported for %s" finding.emitted

let unstable_emitter_caught () =
  match check ~read:reader ~emit:growing calibration_input with
  | None -> Alcotest.fail "an emission that never settles went unreported"
  | Some finding ->
      if Option.is_none finding.again then
        Alcotest.failf "no second pass reported for %s" finding.emitted;
      if not (List.is_empty finding.diagnostics) then
        Alcotest.failf "unexpected reader diagnostic for %s" finding.emitted

(* Reduction has to keep the failure, not just shrink the text. *)
let reduction_keeps_the_failure () =
  let padded =
    String.concat "" [ "b{color:red}"; calibration_input; "c{color:#00f}" ]
  in
  match check ~read:reader ~emit:keyword_component padded with
  | None -> Alcotest.fail "the saboteur stopped failing on a padded input"
  | Some finding ->
      let reduced =
        minimise ~read:reader ~emit:keyword_component padded finding
      in
      if String.length reduced >= String.length padded then
        Alcotest.failf "reduction did not shrink %s" padded;
      if Option.is_none (check ~read:reader ~emit:keyword_component reduced)
      then Alcotest.failf "reduced input %s no longer fails" reduced

let calibration =
  [
    Alcotest.test_case "fixed point accepted" `Quick honest_emitter_passes;
    Alcotest.test_case "unreadable emission caught" `Quick
      unreadable_emitter_caught;
    Alcotest.test_case "unsettled emission caught" `Quick
      unstable_emitter_caught;
    Alcotest.test_case "reduction keeps the failure" `Quick
      reduction_keeps_the_failure;
  ]

(* {1 Entry point} *)

let size_case corpus inputs () =
  let loaded = List.length inputs in
  if loaded < corpus.floor then
    Alcotest.failf
      "%s: loaded %d inputs, expected at least %d - the corpus moved, shrank, \
       or was not copied into the build directory"
      corpus.corpus_name loaded corpus.floor

let sweep_case result () =
  match result.failures with [] -> () | _ -> Alcotest.fail (headline result)

let () =
  let loaded = List.map (fun corpus -> (corpus, corpus.load ())) corpora in
  let sizes =
    List.map
      (fun (corpus, inputs) ->
        Alcotest.test_case corpus.corpus_name `Quick (size_case corpus inputs))
      loaded
  in
  let group mode =
    let results = List.map (sweep mode) loaded in
    let cases =
      List.map
        (fun result ->
          Alcotest.test_case result.corpus.corpus_name `Quick
            (sweep_case result))
        results
    in
    (results, (mode.label, cases))
  in
  let groups = List.map group modes in
  if
    List.exists
      (fun (results, _) ->
        List.exists (fun r -> not (List.is_empty r.failures)) results)
      groups
  then print_report loaded (List.concat_map fst groups);
  Alcotest.run "fixpoint"
    (("calibration", calibration) :: ("corpora", sizes) :: List.map snd groups)
