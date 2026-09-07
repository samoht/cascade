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

(* A generated value the browser rejects and a specification grants. Each entry
   restates a Chrome_gaps entry for another value of the production that entry
   quotes: the generator writes strings the manifest did not, and the fact
   behind them is the one already cited there. An entry that stops excusing
   anything is reported, so a browser that catches up takes its excuse with
   it. *)
let spec_ahead_here : Chrome_gaps.excuse list =
  List.concat_map
    (fun (properties, values, why) ->
      List.map
        (fun value -> { Chrome_gaps.properties; key = None; value; why })
        values)
    [
      ( [
          "width";
          "height";
          "min-width";
          "min-height";
          "max-width";
          "max-height";
          "inline-size";
          "min-inline-size";
          "max-inline-size";
          "block-size";
          "min-block-size";
          "max-block-size";
          "flex-basis";
        ],
        [ "contain" ],
        "CSS Sizing 4 sec. 3.2 adds contain to <box-size>, which every sizing \
         property takes; Chrome has not implemented it" );
      ( [ "text-box-edge" ],
        [ "ideographic-ink" ],
        "CSS Inline 3 sec. 4.4: <text-edge> = [ text | ideographic | \
         ideographic-ink ] | [ text | ideographic | ideographic-ink | cap | ex \
         ] [ text | ideographic | ideographic-ink | alphabetic ]" );
      ( [ "dominant-baseline" ],
        [ "text-bottom" ],
        "CSS Inline 3 sec. 5.2: auto | <baseline-metric>, and sec. 5.1 gives \
         <baseline-metric> = text-bottom | alphabetic | ideographic | middle | \
         central | mathematical | hanging | text-top" );
    ]

(* The same, for a value Chrome reads that no specification grants: an entry
   here restates a {!Chrome_gaps.lenient} fact for a value the generator wrote
   and the manifest did not. *)
let lenient_here : Chrome_gaps.excuse list =
  List.concat_map
    (fun (properties, values, why) ->
      List.map
        (fun value -> { Chrome_gaps.properties; key = None; value; why })
        values)
    [
      ( [ "column-rule"; "column-rule-width" ],
        [ "10px," ],
        "CSS Gaps 1 sec. 4 gives these a comma-separated list, one entry per \
         rule line, so a comma between two entries is theirs to read. The list \
         has no empty entry, and Chrome reads a trailing comma and drops it on \
         serialising. The border shorthands, which have no list at all, answer \
         for a comma through a shape entry" );
    ]

let hits = Hashtbl.create 64
let shape_hits : (string, unit) Hashtbl.t = Hashtbl.create 8

(* The excuse is the whole reason a difference is not a defect, so it is a
   citation or it is nothing. Chrome_gaps carries the shared lists and the spec
   text behind each entry. *)
let judge ~property ~value ~cascade verdict =
  match verdict.error with
  | Some e -> Unanswered e
  | None when not (Bool.equal verdict.set_property verdict.supports) -> Split
  | None -> (
      let excuse table =
        match Chrome_gaps.find table ~property ~value with
        | None -> None
        | Some e -> Some e.why
      in
      (* Only this run's own entries are tracked: the shared ones answer to the
         manifest run, which has its own tally. *)
      let excuse_here table =
        match Chrome_gaps.find table ~property ~value with
        | None -> None
        | Some e ->
            Hashtbl.replace hits (String.concat "\000" [ property; value ]) ();
            Some e.why
      in
      match (cascade, verdict.supports) with
      | true, true | false, false -> Agree
      (* Chrome takes it. Either the reader is short of the grammar, or Chrome
         is past it and an entry says which specification says so. *)
      | false, true -> (
          match excuse Chrome_gaps.lenient with
          | Some why -> Rejects_valid (Some why)
          | None -> (
              (* A shape entry answers for every value of its shape, so a
                 resample cannot strand it the way a literal is stranded. *)
              match
                Chrome_gaps.shape_covering Chrome_gaps.lenient_shapes ~property
                  ~value
              with
              | Some s ->
                  Hashtbl.replace shape_hits s.shape_name ();
                  Rejects_valid (Some s.shape_why)
              | None -> Rejects_valid (excuse_here lenient_here)))
      (* The reader takes it. Either it is loose, or the grammar is real and
         Chrome has not caught up. *)
      | true, false -> (
          match excuse Chrome_gaps.spec_ahead with
          | Some why -> Accepts_invalid (Some why)
          | None -> (
              match
                Chrome_gaps.shape_covering Chrome_gaps.spec_ahead_shapes
                  ~property ~value
              with
              | Some s ->
                  Hashtbl.replace shape_hits s.shape_name ();
                  Accepts_invalid (Some s.shape_why)
              | None -> Accepts_invalid (excuse_here spec_ahead_here))))

(* ===== The classifier, checked against itself ===== *)

(* A run that reports nothing has to be a run that could have. This puts the
   four quadrants through [judge] with made-up verdicts, so the arm that reports
   a difference cannot be dead however the browser behaves. *)
let answer ~set_property ~supports = { set_property; supports; error = None }

let outcome_name = function
  | Agree -> "agree"
  | Rejects_valid None -> "rejects-valid"
  | Rejects_valid (Some _) -> "rejects-valid (excused)"
  | Accepts_invalid None -> "accepts-invalid"
  | Accepts_invalid (Some _) -> "accepts-invalid (excused)"
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
          (judge ~property:"cascade-no-such-property"
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
    { on = "resize"; put = "auto"; must_be = "rejects-valid (excused)" };
    {
      on = "text-decoration-thickness";
      put = "thin";
      must_be = "accepts-invalid (excused)";
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
let excused = ref 0

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

(* An excuse this run wrote for itself and no longer uses is a claim nobody
   checks any more. Only the default seed can say so: another seed draws another
   sample, and a value it did not draw is not a value the browser caught up
   with. The shared Chrome_gaps lists are not checked here either; they answer
   to the manifest run, whose population decides which of them apply. *)
(* A keyed entry answers to the dataset rather than to this run's sample: when
   Chrome ships the production, the entry is stale however the seeded stream
   happens to draw. That is the check a literal cannot have, and it fires
   whether or not the value was drawn. *)
let check_overtaken () =
  List.iter
    (fun (e : Chrome_gaps.excuse) ->
      fail
        (String.concat ""
           [
             "Chrome now ships what this entry excuses: ";
             e.value;
             " (";
             String.concat ", " e.properties;
             ")";
           ]))
    (Chrome_gaps.overtaken
       (Chrome_gaps.spec_ahead @ Chrome_gaps.lenient @ spec_ahead_here
      @ lenient_here))

let check_unused () =
  check_overtaken ();
  List.iter
    (fun (e : Chrome_gaps.excuse) ->
      let used =
        List.exists
          (fun property ->
            Hashtbl.mem hits (String.concat "\000" [ property; e.value ]))
          e.properties
      in
      if not used then
        fail
          (String.concat ""
             [
               "an entry of this run excuses nothing any more: ";
               e.value;
               " (";
               String.concat ", " e.properties;
               ")";
             ]))
    (spec_ahead_here @ lenient_here);
  (* A shape answers for a whole class, so it going quiet is the same signal a
     stranded literal is: the browser agreed, or the generator stopped writing
     anything of the shape. *)
  List.iter
    (fun (s : Chrome_gaps.shape) ->
      if not (Hashtbl.mem shape_hits s.shape_name) then
        fail
          (String.concat ""
             [
               "a shape of this run excuses nothing any more: ";
               s.shape_name;
               " (";
               String.concat ", " s.shape_properties;
               ")";
             ]))
    (Chrome_gaps.lenient_shapes @ Chrome_gaps.spec_ahead_shapes)

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
  | Rejects_valid (Some _) | Accepts_invalid (Some _) -> incr excused
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
                judge ~property:job.property ~value:job.value ~cascade verdict
              in
              Hashtbl.replace outcomes
                (describe job.property job.value)
                (outcome_name outcome);
              record ~property:job.property ~value:job.value ~accepted:cascade
                outcome))
    jobs;
  let vectors = summarise ~jobs ~elapsed in
  Fmt.pr
    "  agree: %d accepted and %d rejected, excused: %d, rejects-valid: %d, \
     accepts-invalid: %d@."
    !agreed_accept !agreed_reject !excused
    (List.length !rejects_valid)
    (List.length !accepts_invalid);
  (* Every excuse in play, grouped by which side the dataset says is wrong. The
     three verdicts are what this harness computes and used to discard, so a
     browser-bug candidate was only ever noticed by a human reading the output:
     [cascade right, the browser has not shipped it] is a candidate to report
     upstream, [cascade wrong] is a defect an entry is hiding, and [needs
     measuring] is the honest answer for a production web-features does not
     model. *)
  let tally = Hashtbl.create 4 in
  List.iter
    (fun (e : Chrome_gaps.excuse) ->
      let v = Chrome_gaps.verdict_of Cascade.Support.evergreen e in
      Hashtbl.replace tally v
        (1 + Option.value ~default:0 (Hashtbl.find_opt tally v)))
    (Chrome_gaps.spec_ahead @ Chrome_gaps.lenient @ spec_ahead_here
   @ lenient_here);
  Fmt.pr "  excuses by verdict:@.";
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
  (* An excuse the dataset says every target ships is not an excuse: the
     browser's answer is the specification's there, so the entry is covering a
     defect rather than citing a fact. *)
  List.iter
    (fun (e : Chrome_gaps.excuse) ->
      match Chrome_gaps.verdict_of Cascade.Support.evergreen e with
      | Chrome_gaps.Cascade_wrong ->
          fail
            (String.concat ""
               [
                 "every target ships what this entry excuses: ";
                 e.value;
                 " (";
                 String.concat ", " e.properties;
                 ")";
               ])
      | Chrome_gaps.Browser_behind | Chrome_gaps.Needs_measurement -> ())
    (Chrome_gaps.spec_ahead @ Chrome_gaps.lenient @ spec_ahead_here
   @ lenient_here);

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
      if !seed = default_seed then check_unused ();
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
