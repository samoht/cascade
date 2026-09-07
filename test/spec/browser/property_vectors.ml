(* Every vector in the property grammar manifest, put to a headless browser.

   The manifest is the largest hand-written oracle in the suite. Nothing in it
   is derived from cascade, but nothing checked it against anything either, so a
   row could be written until cascade agreed with it and the suite would stay
   green. A negative is where that hides: cascade rejects the value, the row
   calls it invalid, the test passes, and a reader limitation reads as a grammar
   rule.

   This run gives the manifest a source of truth that is not cascade. Every
   positive must be valid CSS to a browser and every negative must be invalid to
   it, and a difference stands unless the support dataset names a production
   this browser has not shipped, which is a lookup rather than a sentence
   somebody wrote.

   Skips cleanly, with status 0, when node or a headless Chromium is missing.
   CASCADE_NO_BROWSER silences the run, and a silenced gate is not a pass: only
   the value that says so exits 0. *)

let ( // ) = Filename.concat

(* Why a browser's answer differs from the manifest is a lookup, not a sentence
   someone wrote: Chrome_gaps derives the BCD compat key naming the production
   and asks Cascade.Support whether this build has shipped it. Shared with the
   accept-set differential next door, so a browser that catches up moves both
   answers at the next dataset run. *)

(* ===== Jobs ===== *)

type kind = Positive | Negative | Probe
type job = { id : string; property : string; value : string; kind : kind }

(* Any real property takes a CSS-wide keyword, so this answers "does the browser
   implement this property at all" without asking about a grammar. *)
let probe_value = "inherit"
let unarbitrable_property = Chrome_gaps.unimplemented_property

let jobs () =
  let n = ref 0 in
  let fresh () =
    incr n;
    string_of_int !n
  in
  List.concat_map
    (fun (row : Cascade_spec_inventory.Property_grammar.row) ->
      let probe =
        {
          id = fresh ();
          property = row.property;
          value = probe_value;
          kind = Probe;
        }
      in
      if unarbitrable_property row.property then [ probe ]
      else
        probe
        :: List.map
             (fun value ->
               {
                 id = fresh ();
                 property = row.property;
                 value;
                 kind = Positive;
               })
             row.positives
        @ List.map
            (fun value ->
              { id = fresh (); property = row.property; value; kind = Negative })
            row.negatives)
    Cascade_spec_inventory.Property_grammar.rows

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
  let errors = work // "vectors.err" in
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
  | _ ->
      let ic = open_in errors in
      let text = read_lines ic in
      close_in ic;
      Error (String.concat "\n" text)

let parse_driver_output lines =
  let table = Hashtbl.create 4096 in
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
  | Confirmed  (** the browser and the manifest agree *)
  | Explained of string
      (** they differ, and the support dataset names the production *)
  | Wrong of string  (** they differ, and nothing says why *)
  | Split  (** the two oracles disagree, so neither is the answer *)
  | Unanswered of string  (** the browser did not answer *)

let hits = Hashtbl.create 64

let cite explanation =
  let key = Chrome_gaps.explanation_key explanation in
  Hashtbl.replace hits key ();
  Explained key

let judge ~chrome job verdict =
  match verdict.error with
  | Some e -> Unanswered e
  | None when not (Bool.equal verdict.set_property verdict.supports) -> Split
  | None -> (
      let accepted = verdict.supports in
      match job.kind with
      | Probe -> Confirmed
      | Positive when accepted -> Confirmed
      | Negative when not accepted -> Confirmed
      | Positive -> (
          match
            Chrome_gaps.explains_rejection ~chrome ~property:job.property
              ~value:job.value ()
          with
          | Some e -> cite e
          | None -> Wrong "the browser rejects this positive")
      | Negative -> (
          match
            Chrome_gaps.explains_acceptance ~chrome ~property:job.property
              ~value:job.value ()
          with
          | Some e -> cite e
          | None -> Wrong "the browser accepts this negative"))

(* ===== Reporting ===== *)

let failures = ref 0
let confirmed = ref 0
let explained = ref 0

let fail line =
  incr failures;
  Fmt.pr "FAIL %s@." line

let describe job = String.concat "" [ job.property; ": "; job.value ]

let branch = function
  | Positive -> "positive"
  | Negative -> "negative"
  | Probe -> "probe"

let record job outcome =
  match outcome with
  | Confirmed -> (
      (* A probe only says whether the browser has the property; it is not a
         vector, so it is not something the browser confirmed. *)
      match job.kind with
      | Probe -> ()
      | Positive | Negative -> incr confirmed)
  | Explained _ -> incr explained
  | Wrong why -> fail (String.concat "" [ describe job; ": "; why ])
  | Split ->
      fail
        (String.concat ""
           [
             describe job;
             ": setProperty and CSS.supports disagree about this ";
             branch job.kind;
           ])
  | Unanswered e ->
      fail (String.concat "" [ describe job; ": the browser raised: "; e ])

(* The skip list has to describe the browser in front of it, in both
   directions. *)
let check_unarbitrable implemented =
  List.iter
    (fun (row : Cascade_spec_inventory.Property_grammar.row) ->
      let listed = unarbitrable_property row.property in
      let known =
        match Hashtbl.find_opt implemented row.property with
        | Some b -> b
        | None -> false
      in
      if listed && known then
        fail
          (String.concat ""
             [
               row.property;
               ": listed as unarbitrable, but the browser implements it";
             ]);
      if (not listed) && not known then
        fail
          (String.concat ""
             [
               row.property;
               ": the browser does not implement it, so it cannot arbitrate \
                this row";
             ]))
    Cascade_spec_inventory.Property_grammar.rows

(* ===== Skipping ===== *)

(* Silencing a gate is not the same as passing it, so only the value that says
   the run checks nothing exits 0. A machine with no browser still skips: there
   is nothing there to silence. *)
let acknowledged = "unchecked"

let check_suppression () =
  match Browser.getenv "CASCADE_NO_BROWSER" with
  | None -> ()
  | Some v when String.equal v acknowledged ->
      Browser.skip "property_vectors"
        (String.concat ""
           [
             "CASCADE_NO_BROWSER="; acknowledged; ", so this run checks nothing";
           ])
  | Some v ->
      prerr_endline
        (String.concat ""
           [
             "FAIL: property_vectors is suppressed by CASCADE_NO_BROWSER=";
             v;
             "; a gate that did not run is not a pass. Set CASCADE_NO_BROWSER=";
             acknowledged;
             " to exit 0 and say so.";
           ]);
      exit 1

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

let () =
  check_suppression ();
  let node =
    match Browser.node_binary () with
    | Some n -> n
    | None -> Browser.skip "property_vectors" "no node"
  in
  let chrome =
    match Browser.chrome_binary () with
    | Some c -> c
    | None -> Browser.skip "property_vectors" "no headless browser"
  in
  (* The support dataset is keyed by version, so the run says which build it
     measured rather than assuming the one the default contract names. *)
  let chrome_version =
    match Browser.chrome_version chrome with
    | Some v -> v
    | None ->
        prerr_endline
          "property_vectors: the browser did not report a version, so no \
           support fact can be keyed to this run";
        exit 1
  in
  let jobs = jobs () in
  let script_dir = Filename.dirname Sys.executable_name in
  let work = Filename.get_temp_dir_name () // "cascade-property-vectors" in
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
          (String.concat ""
             [ "property_vectors: the browser driver failed:\n"; err ]);
        exit 1
  in
  let elapsed = Unix.gettimeofday () -. started in
  let table = parse_driver_output lines in
  let implemented = Hashtbl.create 512 in
  List.iter
    (fun job ->
      match Hashtbl.find_opt table job.id with
      | None ->
          fail
            (String.concat ""
               [ describe job; ": the browser did not answer for this vector" ])
      | Some verdict ->
          (match job.kind with
          | Probe ->
              Hashtbl.replace implemented job.property
                (verdict.supports && verdict.set_property)
          | Positive | Negative -> ());
          record job (judge ~chrome:chrome_version job verdict))
    jobs;
  check_unarbitrable implemented;
  let is_probe job =
    match job.kind with Probe -> true | Positive | Negative -> false
  in
  let probes = List.length (List.filter is_probe jobs) in
  (* Chrome_gaps reaches past this manifest, so the count has to be of the rows
     here rather than of the shared list. *)
  let skipped =
    List.length
      (List.filter
         (fun (row : Cascade_spec_inventory.Property_grammar.row) ->
           unarbitrable_property row.property)
         Cascade_spec_inventory.Property_grammar.rows)
  in
  Fmt.pr
    "property_vectors: %d vector(s) over %d row(s), %d of them the browser \
     cannot arbitrate, %.1fs@."
    (List.length jobs - probes)
    (List.length Cascade_spec_inventory.Property_grammar.rows)
    skipped elapsed;
  Fmt.pr "  confirmed: %d, explained: %d, failures: %d@." !confirmed !explained
    !failures;
  (* The same grouping accept_set reports: which side of each explained
     divergence the generated support table blames. A key the dataset says every
     target ships would be covering a defect rather than citing a fact, and
     accept_set fails on one, so this reports the split without repeating that
     check. *)
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
  (* A run that confirms nothing is not a clean run, it is a blind one. *)
  if !confirmed = 0 then fail "not one vector was confirmed against the browser";
  if !failures > 0 then exit 1
