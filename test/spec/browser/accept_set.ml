(* Does cascade's reader accept exactly what a browser accepts?

   The population is every property [Properties.read_any_property] dispatches
   on, taken from the dispatch table itself, so a property the library learns
   enters this run without a second edit. The values are generated: the
   property's own manifest vectors, a neighbouring property's vectors, every
   CSS-wide keyword, [var()] forms, out-of-range numbers, wrong units, and the
   empty value. Nothing about a value is an expectation. The browser decides.

   Both directions are defects, and they are not the same defect.

   A value the browser accepts and the reader drops means cascade cannot read a
   sheet the browser can, so every verdict it gives over that sheet is unproven
   rather than merely noisy.

   A value the browser discards and the reader keeps means cascade's model of
   the page has a declaration the page does not, so a diff can certify a
   difference that does not exist. Nothing had looked at that direction.

   Two independent browser oracles are asked, as test/spec/browser's manifest
   run asks them, and a vector they disagree about is reported rather than
   decided. Where cascade is right and Chrome is not - grammar Chrome has not
   implemented, or a keyword Chrome takes that no specification grants - the
   entry is in Chrome_gaps with the spec text that justifies it.

   Skips cleanly, with status 0, when node or a headless Chromium is missing.
   CASCADE_NO_BROWSER silences the run, and a silenced gate is not a pass: only
   the value that says so exits 0.

   Reproducing one finding:

   dune exec test/spec/browser/accept_set.exe -- --only column-gap --seed 0 *)

open Cascade

let ( // ) = Filename.concat

(* The value generator lives in the inventory library, so this harness and the
   read-back sweep draw the same population. *)
let values_for = Cascade_spec_inventory.Value_gen.values_for

(* ===== The population ===== *)

let properties = List.sort_uniq String.compare Reader_properties.all

(* A rename in the library, or a generator that stopped generating, would
   otherwise turn this run into a green one that asks nothing. *)
let minimum_properties = 400
let minimum_vectors = 5000

(* A generator that drifted into writing only nonsense would agree with the
   browser about all of it. Agreement on CSS both sides take is the half of the
   population that carries the reject-valid question, so it has a floor of its
   own. *)
let minimum_accepted = 2000

(* ===== Jobs ===== *)

type kind = Probe | Vector
type job = { id : string; property : string; value : string; kind : kind }

(* Any implemented property takes a CSS-wide keyword, so this says whether the
   browser has the property at all without asking about a grammar. *)
let probe_value = "inherit"

let jobs ~seed ~only =
  let n = ref 0 in
  let fresh () =
    incr n;
    string_of_int !n
  in
  let selected name =
    match only with
    | None -> true
    | Some needle ->
        let h = String.length name and k = String.length needle in
        let rec at i =
          i + k <= h && (String.equal (String.sub name i k) needle || at (i + 1))
        in
        at 0
  in
  List.concat_map
    (fun name ->
      if not (selected name) then []
      else
        let probe =
          { id = fresh (); property = name; value = probe_value; kind = Probe }
        in
        if Chrome_gaps.unimplemented_property name then [ probe ]
        else
          probe
          :: List.map
               (fun value ->
                 { id = fresh (); property = name; value; kind = Vector })
               (values_for ~seed name))
    properties

(* ===== The reader ===== *)

(* Accepted means the reader kept a declaration and said nothing against it. A
   warning is the parser reporting that it dropped or recovered something, and a
   declaration flagged invalid is one every serialisation removes, so neither is
   an acceptance. *)
let cascade_accepts ~property ~value =
  let css = String.concat "" [ "a{"; property; ":"; value; "}" ] in
  match Css.of_string css with
  | exception Error.Parse_error _ -> false
  | exception Reader.Parse_error _ -> false
  | Error _ -> false
  | Ok { stylesheet; warnings = _ :: _; _ } ->
      ignore stylesheet;
      false
  | Ok { stylesheet; warnings = []; _ } -> (
      let declarations =
        List.concat_map
          (fun statement ->
            match Css.as_rule statement with
            | Some (_, declarations, _) -> declarations
            | None -> [])
          (Css.statements stylesheet)
      in
      match declarations with
      | [] -> false
      | _ -> not (List.exists Css.Declaration.is_invalid declarations))

(* ===== The driver ===== *)

type verdict = { set_property : bool; supports : bool; error : string option }

let read_lines ic =
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file -> List.rev acc
  in
  loop []

let run_driver ~node ~chrome ~script ~jobs ~work =
  let errors = work // "accept-set.err" in
  let cmd =
    String.concat " "
      [
        String.concat "" [ "CHROME="; Filename.quote chrome ];
        Filename.quote node;
        Filename.quote script;
        Filename.quote jobs;
        Filename.quote work;
        String.concat "" [ "2>"; Filename.quote errors ];
      ]
  in
  let ic = Unix.open_process_in cmd in
  let lines = read_lines ic in
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> Ok lines
  | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
      let ic = open_in errors in
      let text = read_lines ic in
      close_in ic;
      Error (String.concat "\n" text)

let parse_driver_output lines =
  let table = Hashtbl.create 65536 in
  List.iter
    (fun line ->
      match String.split_on_char '\t' line with
      | [ "v"; id; s; c ] ->
          Hashtbl.replace table id
            {
              set_property = String.equal s "1";
              supports = String.equal c "1";
              error = None;
            }
      | "x" :: id :: rest ->
          Hashtbl.replace table id
            {
              set_property = false;
              supports = false;
              error = Some (String.concat "\t" rest);
            }
      | _ -> ())
    lines;
  table

(* ===== Judging ===== *)

type outcome =
  | Agree
  | Rejects_valid of string option
      (** the browser takes it, the reader does not *)
  | Accepts_invalid of string option
      (** the reader keeps it, the browser discards it *)
  | Split  (** the two browser oracles disagree, so neither is the answer *)
  | Unanswered of string  (** the browser did not answer *)

(* Every value the spec-derived manifest declares valid for a property. A row is
   written from a specification's own grammar, so this is that grammar in the
   form the harness can ask. *)
let manifest_positives =
  let table = Hashtbl.create 512 in
  List.iter
    (fun (r : Cascade_spec_inventory.Property_grammar.row) ->
      List.iter (fun v -> Hashtbl.replace table (r.property, v) ()) r.positives)
    Cascade_spec_inventory.Property_grammar.rows;
  table

let manifest_positive ~property ~value =
  Hashtbl.mem manifest_positives (property, value)

let hits = Hashtbl.create 64

(* Why a difference is not a defect is a lookup, not a sentence someone wrote.
   Chrome_gaps derives the BCD compat key naming the production and asks
   Cascade.Support whether this browser has shipped it, so an entry cannot
   outlive the gap it describes and a resample cannot strand it: the key is
   derived from the pair in front of the harness. *)
let judge ~chrome ~property ~value ~cascade verdict =
  match verdict.error with
  | Some e -> Unanswered e
  | None when not (Bool.equal verdict.set_property verdict.supports) -> Split
  | None -> (
      let cite explanation =
        let key = Chrome_gaps.explanation_key explanation in
        Hashtbl.replace hits key ();
        Some key
      in
      match (cascade, verdict.supports) with
      | true, true | false, false -> Agree
      (* Chrome takes it. Either the reader is short of the grammar, or Chrome
         ships a production no specification grants and the library has measured
         it. *)
      | false, true -> (
          match Chrome_gaps.explains_acceptance ~chrome ~property ~value () with
          | Some e -> Rejects_valid (cite e)
          | None -> Rejects_valid None)
      (* The reader takes it. Either it is loose, or the grammar is real and
         Chrome has not caught up. *)
      | true, false -> (
          match Chrome_gaps.explains_rejection ~chrome ~property ~value () with
          | Some e -> Accepts_invalid (cite e)
          | None ->
              (* The manifest is spec-derived, so a row declaring this value a
                 POSITIVE already says the specification grants it. Cascade
                 agreeing with that row and the browser refusing is a browser
                 gap by construction.

                 This is right HERE and wrong in property_vectors, which puts
                 the row itself under test: there a browser rejecting a positive
                 is the question, and answering it from the row would be
                 circular. *)
              if manifest_positive ~property ~value then
                Accepts_invalid
                  (Some
                     (String.concat ""
                        [
                          "the spec-derived manifest lists this as a positive \
                           for ";
                          property;
                        ]))
              else Accepts_invalid None))

(* ===== The classifier, checked against itself ===== *)

(* A run that reports nothing has to be a run that could have. This puts the
   four quadrants through [judge] with made-up verdicts, so the arm that reports
   a difference cannot be dead however the browser behaves. *)
let answer ~set_property ~supports = { set_property; supports; error = None }

let outcome_name = function
  | Agree -> "agree"
  | Rejects_valid None -> "rejects-valid"
  | Rejects_valid (Some _) -> "rejects-valid (explained)"
  | Accepts_invalid None -> "accepts-invalid"
  | Accepts_invalid (Some _) -> "accepts-invalid (explained)"
  | Split -> "split"
  | Unanswered _ -> "unanswered"

let check_classifier () =
  let cases =
    [
      ("both accept", true, answer ~set_property:true ~supports:true, "agree");
      ("both reject", false, answer ~set_property:false ~supports:false, "agree");
      ( "only the browser accepts",
        false,
        answer ~set_property:true ~supports:true,
        "rejects-valid" );
      ( "only the reader accepts",
        true,
        answer ~set_property:false ~supports:false,
        "accepts-invalid" );
      ( "the oracles disagree",
        true,
        answer ~set_property:true ~supports:false,
        "split" );
    ]
  in
  List.iter
    (fun (what, cascade, verdict, expected) ->
      let got =
        outcome_name
          (judge ~chrome:(0, 0) ~property:"cascade-no-such-property"
             ~value:"cascade-no-such-value" ~cascade verdict)
      in
      if not (String.equal got expected) then (
        Fmt.pr "FAIL the classifier calls %s %s, not %s@." what got expected;
        exit 1))
    cases

(* ===== Calibration ===== *)

(* Three vectors put through the whole pipeline - generation, both browser
   oracles, the reader, the comparison - whose outcome the run asserts.

   The two disagreements are citation-backed and permanent. No specification
   grants [resize: auto], so a reader that rejects what Chrome takes there stays
   right. [text-decoration-thickness] takes a <line-width>, so a reader that
   accepts [thin] where Chrome does not stays right. Neither is a defect anybody
   will fix out from under this check, which is what a calibration vector has to
   be: a vector pinned to today's bug goes red the day the bug is fixed. *)
type calibration = { on : string; put : string; must_be : string }

let calibration =
  [
    { on = "color"; put = "red"; must_be = "agree" };
    { on = "resize"; put = "auto"; must_be = "rejects-valid (explained)" };
    {
      on = "text-decoration-thickness";
      put = "thin";
      must_be = "accepts-invalid (explained)";
    };
  ]

(* Vectors whose classification the run prints without asserting it. They are
   the two directions' known defects on the day this was written; the run does
   not pin them, because a harness that goes red when a bug is fixed is a
   harness nobody keeps. What is asserted is that they are still in the
   population and the browser still answered about them: losing a vector
   silently is the blindness this guards. *)
let witnesses =
  [ ("column-gap", "normal"); ("max-width", "auto"); ("fill", "auto") ]

(* ===== Findings ===== *)

type finding = { property : string; value : string }

let rejects_valid : finding list ref = ref []
let accepts_invalid : finding list ref = ref []
let splits : finding list ref = ref []
let unanswered : (finding * string) list ref = ref []

(* Kept apart, because a generator that only ever writes CSS both sides reject
   agrees about everything and asks nothing. *)
let agreed_accept = ref 0
let agreed_reject = ref 0
let explained = ref 0

(* Grouped by the value, because the value is the cause: one loose arm of the
   reader shows up as the same text under twenty property names. *)
let grouped findings =
  let table = Hashtbl.create 256 in
  List.iter
    (fun f ->
      let previous = try Hashtbl.find table f.value with Not_found -> [] in
      Hashtbl.replace table f.value (f.property :: previous))
    findings;
  let groups =
    Hashtbl.fold
      (fun value names acc -> (value, List.rev names) :: acc)
      table []
  in
  List.sort
    (fun (v, a) (w, b) ->
      match compare (List.length b) (List.length a) with
      | 0 -> String.compare v w
      | n -> n)
    groups

let quote value = if String.equal value "" then "<empty>" else value

let report_direction label findings =
  match findings with
  | [] -> ()
  | _ ->
      let groups = grouped findings in
      Fmt.pr "@.%s: %d vector(s) over %d distinct value(s)@." label
        (List.length findings) (List.length groups);
      List.iter
        (fun (value, names) ->
          Fmt.pr "  %s (%d): %s@." (quote value) (List.length names)
            (String.concat ", " names))
        groups

(* ===== Skipping ===== *)

let acknowledged = "unchecked"

let check_suppression () =
  match Browser.getenv "CASCADE_NO_BROWSER" with
  | None -> ()
  | Some v when String.equal v acknowledged ->
      Browser.skip "accept_set"
        (String.concat ""
           [
             "CASCADE_NO_BROWSER="; acknowledged; ", so this run checks nothing";
           ])
  | Some v ->
      prerr_endline
        (String.concat ""
           [
             "FAIL: accept_set is suppressed by CASCADE_NO_BROWSER=";
             v;
             "; a gate that did not run is not a pass. Set CASCADE_NO_BROWSER=";
             acknowledged;
             " to exit 0 and say so.";
           ]);
      exit 1

(* ===== Arguments ===== *)

(* The seed the suite runs. Another one draws another sample, which is what
   --seed is for; the checks that describe a whole population only speak for
   this one. *)
let default_seed = 0
let seed = ref default_seed
let only = ref None
let population_only = ref false

let usage () =
  Fmt.pr "usage: accept_set [--seed N] [--only SUBSTRING] [--population]@.";
  exit 0

let parse_args () =
  let rec loop i =
    if i >= Array.length Sys.argv then ()
    else
      match Sys.argv.(i) with
      | "--seed" when i + 1 < Array.length Sys.argv ->
          seed := int_of_string Sys.argv.(i + 1);
          loop (i + 2)
      | "--only" when i + 1 < Array.length Sys.argv ->
          only := Some Sys.argv.(i + 1);
          loop (i + 2)
      | "--population" ->
          population_only := true;
          loop (i + 1)
      | _ -> usage ()
  in
  loop 1

(* ===== Main ===== *)

let write_jobs file jobs =
  let oc = open_out file in
  output_string oc
    (Json.to_string
       (Json.Arr
          (List.map
             (fun j ->
               Json.Obj
                 [
                   ("id", Json.Str j.id);
                   ("property", Json.Str j.property);
                   ("value", Json.Str j.value);
                 ])
             jobs)));
  close_out oc

let failures = ref 0

let fail line =
  incr failures;
  Fmt.pr "FAIL %s@." line

let describe property value = String.concat "" [ property; ": "; quote value ]

(* The skip list has to describe the browser in front of it, in both directions,
   or a property leaves this run without anybody saying so. *)
let check_unimplemented implemented =
  List.iter
    (fun name ->
      let listed = Chrome_gaps.unimplemented_property name in
      let known =
        match Hashtbl.find_opt implemented name with
        | Some b -> b
        | None -> false
      in
      if listed && known then
        fail
          (String.concat ""
             [ name; ": listed as unimplemented, but the browser has it" ]);
      if (not listed) && not known then
        fail
          (String.concat ""
             [
               name;
               ": the browser does not implement it, so it cannot arbitrate \
                this property; add it to Chrome_gaps.unimplemented";
             ]))
    properties

(* A measurement in the library the generated dataset has caught up with. The
   library carries a fact only while web-features does not, so a key that has
   arrived in the generated table is a measurement to delete, and one the
   dataset now says Chrome ships is a gap that has closed. Neither depends on
   what this seed drew, which is what a hand-written entry could never say. *)
let check_measurements () =
  List.iter
    (fun (m : Cascade.Support.measurement) ->
      if not (Cascade.Support.self_measured m.key) then
        fail
          (String.concat ""
             [
               "web-features now carries this key, so the measurement beside \
                it is redundant: ";
               m.key;
             ]))
    Cascade.Support.measured

let check_calibration outcomes =
  List.iter
    (fun c ->
      match Hashtbl.find_opt outcomes (describe c.on c.put) with
      | None ->
          fail
            (String.concat ""
               [
                 "the calibration vector ";
                 describe c.on c.put;
                 " is not in the population, so a green run proves nothing";
               ])
      | Some got ->
          Fmt.pr "  calibration: %s -> %s@." (describe c.on c.put) got;
          if not (String.equal got c.must_be) then
            fail
              (String.concat ""
                 [
                   "the calibration vector ";
                   describe c.on c.put;
                   " came out ";
                   got;
                   ", not ";
                   c.must_be;
                 ]))
    calibration

let check_witnesses outcomes =
  List.iter
    (fun (property, value) ->
      match Hashtbl.find_opt outcomes (describe property value) with
      | None ->
          fail
            (String.concat ""
               [
                 "the witness ";
                 describe property value;
                 " left the population, so this run stopped asking about it";
               ])
      | Some got -> Fmt.pr "  witness: %s -> %s@." (describe property value) got)
    witnesses

let record ~property ~value ~accepted outcome =
  let f = { property; value } in
  match outcome with
  | Agree -> if accepted then incr agreed_accept else incr agreed_reject
  | Rejects_valid (Some _) | Accepts_invalid (Some _) -> incr explained
  | Rejects_valid None -> rejects_valid := f :: !rejects_valid
  | Accepts_invalid None -> accepts_invalid := f :: !accepts_invalid
  | Split -> splits := f :: !splits
  | Unanswered e -> unanswered := (f, e) :: !unanswered

let is_probe job = match job.kind with Probe -> true | Vector -> false

let summarise ~jobs ~elapsed =
  let vectors = List.length (List.filter (fun j -> not (is_probe j)) jobs) in
  let compared =
    List.length
      (List.filter
         (fun name -> not (Chrome_gaps.unimplemented_property name))
         properties)
  in
  Fmt.pr
    "accept_set: %d propert(ies), %d the browser cannot arbitrate, %d \
     compared@."
    (List.length properties)
    (List.length properties - compared)
    compared;
  Fmt.pr "accept_set: %d vector(s), seed %d, %.1fs@." vectors !seed elapsed;
  vectors

let () =
  parse_args ();
  check_classifier ();
  let jobs = jobs ~seed:!seed ~only:!only in
  let vectors = List.length (List.filter (fun j -> not (is_probe j)) jobs) in
  if !population_only then (
    Fmt.pr "accept_set: %d propert(ies), %d vector(s), seed %d@."
      (List.length properties) vectors !seed;
    exit 0);
  check_suppression ();
  let node =
    match Browser.node_binary () with
    | Some n -> n
    | None -> Browser.skip "accept_set" "no node"
  in
  let chrome =
    match Browser.chrome_binary () with
    | Some c -> c
    | None -> Browser.skip "accept_set" "no headless browser"
  in
  (* The support dataset is keyed by version, so the run says which build it
     measured rather than assuming the one the default contract names. *)
  let chrome_version =
    match Browser.chrome_version chrome with
    | Some v -> v
    | None ->
        prerr_endline
          "accept_set: the browser did not report a version, so no support \
           fact can be keyed to this run";
        exit 1
  in
  let script_dir = Filename.dirname Sys.executable_name in
  let work = Filename.get_temp_dir_name () // "cascade-accept-set" in
  (try Unix.mkdir work 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let jobs_file = work // "jobs.json" in
  write_jobs jobs_file jobs;
  let started = Unix.gettimeofday () in
  let lines =
    match
      run_driver ~node ~chrome
        ~script:(script_dir // "vectors.js")
        ~jobs:jobs_file ~work
    with
    | Ok lines -> lines
    | Error err ->
        prerr_endline
          (String.concat "" [ "accept_set: the browser driver failed:\n"; err ]);
        exit 1
  in
  let elapsed = Unix.gettimeofday () -. started in
  let table = parse_driver_output lines in
  let implemented = Hashtbl.create 1024 in
  let outcomes = Hashtbl.create 65536 in
  List.iter
    (fun job ->
      match Hashtbl.find_opt table job.id with
      | None ->
          fail
            (String.concat ""
               [
                 describe job.property job.value;
                 ": the browser did not answer for this vector";
               ])
      | Some verdict -> (
          match job.kind with
          | Probe ->
              Hashtbl.replace implemented job.property
                (verdict.supports && verdict.set_property)
          | Vector ->
              let cascade =
                cascade_accepts ~property:job.property ~value:job.value
              in
              let outcome =
                judge ~chrome:chrome_version ~property:job.property
                  ~value:job.value ~cascade verdict
              in
              Hashtbl.replace outcomes
                (describe job.property job.value)
                (outcome_name outcome);
              record ~property:job.property ~value:job.value ~accepted:cascade
                outcome))
    jobs;
  let vectors = summarise ~jobs ~elapsed in
  Fmt.pr
    "  agree: %d accepted and %d rejected, explained: %d, rejects-valid: %d, \
     accepts-invalid: %d@."
    !agreed_accept !agreed_reject !explained
    (List.length !rejects_valid)
    (List.length !accepts_invalid);
  (* Every disagreement the dataset explained, grouped by which side it says is
     wrong. The three verdicts are what this harness computes and used to
     discard, so a browser-bug candidate was only ever noticed by a human
     reading the output: [cascade right, the browser has not shipped it] is a
     candidate to report upstream, [cascade wrong] is a defect the lookup would
     be hiding, and [needs measuring] is the honest answer for a production
     web-features does not model. *)
  let tally = Hashtbl.create 4 in
  Hashtbl.iter
    (fun key () ->
      let v = Chrome_gaps.verdict_of Cascade.Support.evergreen key in
      Hashtbl.replace tally v
        (1 + Option.value ~default:0 (Hashtbl.find_opt tally v)))
    hits;
  Fmt.pr "  explained by verdict:@.";
  List.iter
    (fun v ->
      match Hashtbl.find_opt tally v with
      | None | Some 0 -> ()
      | Some n -> Fmt.pr "    %3d  %s@." n (Chrome_gaps.verdict_name v))
    [
      Chrome_gaps.Cascade_wrong;
      Chrome_gaps.Browser_behind;
      Chrome_gaps.Needs_measurement;
    ];
  (* Which of those answers came from the generated dataset and which from this
     project's own measurement. A self-measured fact can drift from the browser
     and a generated one cannot, so the split is worth seeing. *)
  let ours =
    Hashtbl.fold
      (fun key () n -> if Cascade.Support.self_measured key then n + 1 else n)
      hits 0
  in
  if ours > 0 then
    Fmt.pr
      "    (%d of those answered by our own measurement, not the dataset)@."
      ours;
  (* A key the dataset says every target ships explains nothing: the browser's
     answer is the specification's there, so a disagreement it covered would be
     a defect rather than a fact. *)
  Hashtbl.iter
    (fun key () ->
      match Chrome_gaps.verdict_of Cascade.Support.evergreen key with
      | Chrome_gaps.Cascade_wrong ->
          fail
            (String.concat ""
               [ "every target ships what this key explained away: "; key ])
      | Chrome_gaps.Browser_behind | Chrome_gaps.Needs_measurement -> ())
    hits;

  (* A run over an empty or shrunken population is a green run that asks
     nothing, which is worse than a red one. *)
  if List.length properties < minimum_properties then
    fail
      (String.concat ""
         [
           "the inventory yielded ";
           string_of_int (List.length properties);
           " properties, fewer than the ";
           string_of_int minimum_properties;
           " expected";
         ]);
  (match !only with
  | Some _ -> ()
  | None ->
      if vectors < minimum_vectors then
        fail
          (String.concat ""
             [
               "the generator yielded ";
               string_of_int vectors;
               " vectors, fewer than the ";
               string_of_int minimum_vectors;
               " expected";
             ]);
      if !agreed_accept < minimum_accepted then
        fail
          (String.concat ""
             [
               "only ";
               string_of_int !agreed_accept;
               " vectors were CSS both sides take, fewer than the ";
               string_of_int minimum_accepted;
               " expected; a population of nonsense agrees about everything";
             ]);
      check_unimplemented implemented;
      check_measurements ();
      check_calibration outcomes;
      check_witnesses outcomes);
  report_direction "REJECTS VALID (the browser accepts it, cascade drops it)"
    !rejects_valid;
  report_direction "ACCEPTS INVALID (cascade keeps it, the browser discards it)"
    !accepts_invalid;
  report_direction "ORACLE SPLIT (setProperty and CSS.supports disagree)"
    !splits;
  List.iter
    (fun (f, e) ->
      Fmt.pr "  unanswered %s: %s@." (describe f.property f.value) e)
    !unanswered;
  let found =
    List.length !rejects_valid
    + List.length !accepts_invalid
    + List.length !splits + List.length !unanswered
  in
  if found > 0 || !failures > 0 then exit 1
