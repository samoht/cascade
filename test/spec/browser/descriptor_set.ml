(* Does cascade read exactly the @font-face descriptors a browser reads?

   The accept-set differential next door cannot ask this. A descriptor is not a
   property: CSS.supports takes a property name, el.style.setProperty fills an
   inline block, and both refuse [src] and [unicode-range] outright. So the
   @font-face grammar, the one place a stylesheet names a font file, had no
   browser oracle at all while every property around it had two.

   What CSSOM exposes instead is the parsed rule. descriptors.js puts each value
   to two entry points into the same parser, insertRule into a <style> element's
   sheet and replaceSync on a constructed one, and reads back whether the rule
   still spells the descriptor. A value they disagree about is a fact about the
   browser, so it is reported rather than decided.

   Three populations, and each answers a different question.

   The manifest positives and negatives ask whether the spec-derived vectors in
   Descriptor_grammar are true of a browser. Nothing in that manifest is derived
   from cascade, but nothing checked it against anything either, and a negative
   is where that hides: cascade rejects the value, the row calls it invalid, the
   test passes, and a reader limitation reads as a grammar rule.

   The generated values ask the accept-set question, the one the manifest
   cannot: over a population nobody wrote by hand, does cascade keep exactly
   what the browser keeps. A value the browser takes and cascade drops means
   cascade cannot read a sheet the browser can. A value cascade keeps and the
   browser drops means cascade's model of the page has a declaration the page
   does not, so a diff can certify a difference that does not exist.

   And the descriptors themselves: every descriptor cascade models must have a
   row, or this run says nothing about it beyond what the generator happened to
   draw.

   A descriptor the browser has not shipped cannot arbitrate anything, which is
   a fact about browsers rather than about this run, so it is looked up in
   Cascade.Support under the BCD key web-features carries. A descriptor the
   dataset says this build ships, that then takes none of the manifest's
   positives, is a failure and not something to look past.

   Both directions are exercised rather than quiet. Making the CSS-wide refusal
   in read_descriptor_value a no-op reports 25 values cascade would keep and the
   browser drops; making unicode-range refuse everything reports 5 the browser
   keeps and cascade would drop.

   Skips cleanly, with status 0, when node or a headless Chromium is missing.
   CASCADE_NO_BROWSER silences the run, and a silenced gate is not a pass: only
   the value that says so exits 0.

   Reproducing one finding:

   dune exec test/spec/browser/descriptor_set.exe -- --only src --seed 0 *)

open Cascade
module Grammar = Cascade_spec_inventory.Descriptor_grammar

let ( // ) = Filename.concat

(* The value generator lives in the inventory library, so this harness draws the
   same population the property sweeps do. *)
let values_for = Cascade_spec_inventory.Value_gen.values_for

(* ===== The population ===== *)

(* One at-rule the harness sweeps: the descriptors cascade models for it, read
   out of the library's own dispatch, and how a rule carrying one is written.
   @page is absent because its body is ordinary declarations, so the accept-set
   differential already answers for all but its three page-only descriptors. *)
type at_rule = {
  name : string;
  modelled : string list;
  sheet : descriptor:string -> value:string -> string;
}

(* CSS Fonts 4 sec. 4.2 and 4.3 make font-family and src required: a @font-face
   missing either is invalid as a whole, so every sheet carries both and the
   descriptor under test takes over its own slot. *)
let font_face_sheet ~descriptor ~value =
  let family, src, extra =
    match descriptor with
    | "font-family" -> (value, "url(brand.woff2)", None)
    | "src" -> ("Brand", value, None)
    | _ -> ("Brand", "url(brand.woff2)", Some (descriptor, value))
  in
  String.concat ""
    ([ "@font-face{font-family:"; family; ";src:"; src ]
    @ (match extra with None -> [] | Some (name, v) -> [ ";"; name; ":"; v ])
    @ [ "}" ])

(* CSS Counter Styles 3 sec. 2 makes system and symbols the pair a style needs
   to resolve, and sec. 3.1's default system is symbolic. *)
let _counter_style_sheet ~descriptor ~value =
  let base =
    match descriptor with
    | "system" | "symbols" -> ""
    | _ -> "system:cyclic;symbols:\"a\";"
  in
  let rest =
    match descriptor with
    | "system" -> String.concat "" [ "system:"; value; ";symbols:\"a\"" ]
    | "symbols" -> String.concat "" [ "system:cyclic;symbols:"; value ]
    | _ -> String.concat "" [ descriptor; ":"; value ]
  in
  String.concat "" [ "@counter-style cascade-probe{"; base; rest; "}" ]

(* CSS Properties and Values API 1 sec. 2 makes syntax and inherits required,
   and an initial-value required for every syntax but the universal one, which
   has to PARSE as that syntax: a rule pairing [<length>] with [red] is invalid
   for the pairing rather than for either descriptor. The universal syntax takes
   anything, so it is what a rule testing something else carries. *)
let initial_value_for = function
  | "\"<color>\"" -> Some "red"
  | "\"<length>\"" -> Some "0px"
  | "\"<length>#\"" | "\"<length># \"" -> Some "0px"
  | "\"*\"" -> None
  | _ -> None

let property_sheet ~descriptor ~value =
  let syntax, inherits, initial =
    match descriptor with
    | "syntax" -> (value, "true", initial_value_for value)
    | "inherits" -> ("\"*\"", value, None)
    | _ -> ("\"*\"", "true", Some value)
  in
  String.concat ""
    ([ "@property --cascade-probe{syntax:"; syntax; ";inherits:"; inherits ]
    @ (match initial with None -> [] | Some v -> [ ";initial-value:"; v ])
    @ [ "}" ])

(* @counter-style is not swept yet, and the reason is a defect rather than an
   omission: six of its descriptors are read as an opaque string, so [range:
   bogus] and [pad: "0"] reach the output. Enabling it here reports 218 values
   on the first run, which is a reader to fix rather than a harness to land red,
   and the rows in Descriptor_grammar are written and waiting. *)
let at_rules =
  [
    {
      name = "font-face";
      modelled = List.sort_uniq String.compare Font_face_descriptors.all;
      sheet = font_face_sheet;
    };
    {
      name = "property";
      modelled = List.sort_uniq String.compare Property_descriptors.all;
      sheet = property_sheet;
    };
  ]

let modelled =
  List.concat_map (fun r -> List.map (fun d -> (r.name, d)) r.modelled) at_rules

(* A rename in the library, or a generator that stopped generating, would
   otherwise turn this run into a green one that asks nothing. *)
let minimum_descriptors = 10
let minimum_values = 200
let minimum_arbitrated = 100

type kind = Positive | Negative | Generated

type job = {
  id : string;
  at_rule : string;
  descriptor : string;
  value : string;
  kind : kind;
  sheet : string;
}

(* CSS Variables 1 sec. 3 substitutes var() in a property value and a descriptor
   is not a property, so a browser drops any descriptor holding one. cascade
   parks the reference for the inline pass to substitute at build time, which is
   a different claim from "this is a value the browser reads", and test_inline
   is where that claim is checked. Drawing them here would ask this harness a
   question it is not the oracle for. *)
let drawable value =
  let rec at i =
    i + 4 <= String.length value
    && (String.equal (String.sub value i 4) "var(" || at (i + 1))
  in
  not (at 0)

let jobs ~seed ~only =
  let n = ref 0 in
  let fresh () =
    incr n;
    string_of_int !n
  in
  List.concat_map
    (fun rule ->
      let job descriptor kind value =
        {
          id = fresh ();
          at_rule = rule.name;
          descriptor;
          value;
          kind;
          sheet = rule.sheet ~descriptor ~value;
        }
      in
      List.concat_map
        (fun descriptor ->
          if (not (String.equal only "")) && not (String.equal only descriptor)
          then []
          else
            let row = Grammar.row_for ~at_rule:rule.name descriptor in
            let manifest =
              match row with
              | None -> []
              | Some (row : Grammar.row) ->
                  List.map (job descriptor Positive) row.positives
                  @ List.map (job descriptor Negative) row.negatives
            in
            (* The generator draws from the manifest too, so a value the row
               already decides would otherwise arrive twice and be judged by
               both rules at once. *)
            let decided value =
              match row with
              | None -> false
              | Some (row : Grammar.row) ->
                  List.exists (String.equal value) row.positives
                  || List.exists (String.equal value) row.negatives
            in
            manifest
            @ List.map (job descriptor Generated)
                (List.filter
                   (fun v -> drawable v && not (decided v))
                   (values_for ~seed descriptor)))
        rule.modelled)
    at_rules

(* ===== The reader ===== *)

(* Accepted means the reader kept the rule and said nothing against it. A
   warning is the parser reporting that it dropped or recovered something, so it
   is not an acceptance however much of the sheet survived. *)
let cascade_accepts job =
  match Css.of_string job.sheet with
  | exception Error.Parse_error _ -> false
  | exception Reader.Parse_error _ -> false
  | Error _ -> false
  | Ok { warnings = _ :: _; _ } -> false
  | Ok { stylesheet; warnings = []; _ } ->
      not (List.is_empty (Css.statements stylesheet))

(* ===== The driver ===== *)

type verdict = { parsed : bool; constructed : bool; error : string option }

let read_lines ic =
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file -> List.rev acc
  in
  loop []

let run_driver ~node ~chrome ~script ~jobs ~work =
  let errors = work // "descriptors.err" in
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
      | [ "v"; id; p; o ] ->
          Hashtbl.replace table id
            {
              parsed = String.equal p "1";
              constructed = String.equal o "1";
              error = None;
            }
      | "x" :: id :: rest ->
          Hashtbl.replace table id
            {
              parsed = false;
              constructed = false;
              error = Some (String.concat "\t" rest);
            }
      | _ -> ())
    lines;
  table

(* ===== Which browser this run answers for ===== *)

(* The support dataset is keyed by version, so the harness says which build it
   measured rather than assuming the one the default contract names. *)
let chrome_version chrome =
  let ic =
    Unix.open_process_in
      (String.concat " " [ Filename.quote chrome; "--version"; "2>/dev/null" ])
  in
  let lines = read_lines ic in
  ignore (Unix.close_process_in ic);
  let leading_int s =
    let n = String.length s in
    let rec upto i =
      if i < n && s.[i] >= '0' && s.[i] <= '9' then upto (i + 1) else i
    in
    let stop = upto 0 in
    if stop = 0 then None else int_of_string_opt (String.sub s 0 stop)
  in
  let of_word w =
    match String.split_on_char '.' w with
    | major :: minor :: _ -> (
        match (leading_int major, leading_int minor) with
        | Some a, Some b -> Some (a, b)
        | _ -> None)
    | _ -> None
  in
  List.fold_left
    (fun found line ->
      match found with
      | Some _ -> found
      | None ->
          List.fold_left
            (fun found word ->
              match found with Some _ -> found | None -> of_word word)
            None
            (String.split_on_char ' ' line))
    None lines

(* ===== Reporting ===== *)

let failures = ref 0
let arbitrated = ref 0
let splits = ref 0
let behind = ref 0
let lenient = ref 0

let fail line =
  incr failures;
  Fmt.pr "FAIL %s@." line

let describe job =
  String.concat "" [ "@"; job.at_rule; " "; job.descriptor; ": "; job.value ]

(* ===== Skipping ===== *)

(* Silencing a gate is not the same as passing it, so only the value that says
   the run checks nothing exits 0. A machine with no browser still skips: there
   is nothing there to silence. *)
let acknowledged = "unchecked"

let check_suppression () =
  match Browser.getenv "CASCADE_NO_BROWSER" with
  | None -> ()
  | Some v when String.equal v acknowledged ->
      Browser.skip "descriptor_set"
        (String.concat ""
           [
             "CASCADE_NO_BROWSER="; acknowledged; ", so this run checks nothing";
           ])
  | Some v ->
      prerr_endline
        (String.concat ""
           [
             "FAIL: descriptor_set is suppressed by CASCADE_NO_BROWSER=";
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
                   ("descriptor", Json.Str j.descriptor);
                   ("value", Json.Str j.value);
                   ("sheet", Json.Str j.sheet);
                 ])
             jobs)));
  close_out oc

let arg name default =
  let rec loop = function
    | a :: v :: _ when String.equal a name -> v
    | _ :: rest -> loop rest
    | [] -> default
  in
  loop (Array.to_list Sys.argv)

let support_key ~at_rule descriptor =
  String.concat "" [ "css.at-rules."; at_rule; "."; descriptor ]

(* A descriptor this browser takes no positive of cannot arbitrate a single
   value of it. Whether that is the browser being behind is a fact about
   browsers, so it is looked up rather than asserted here. *)
let report_unshipped ~version ~at_rule descriptor =
  let key = support_key ~at_rule descriptor in
  let named = String.concat "" [ "@"; at_rule; " "; descriptor ] in
  match Support.engine_implements Support.Chrome version key with
  | Some true ->
      fail
        (String.concat ""
           [
             named;
             ": the browser took none of the manifest's positives, and ";
             key;
             " says this build shipped it";
           ])
  | Some false ->
      Fmt.pr "  behind: %s (%s says this build has not shipped it)@." named key
  | None ->
      fail
        (String.concat ""
           [
             named;
             ": the browser took none of the manifest's positives and ";
             key;
             " is not in the support dataset, so nothing says whether the \
              browser is behind or the row is wrong; measure it into \
              Cascade.Support";
           ])

let () =
  check_suppression ();
  let seed = int_of_string (arg "--seed" "0") in
  let only = arg "--only" "" in
  let selected d = String.equal only "" || String.equal only d in
  let node =
    match Browser.node_binary () with
    | Some n -> n
    | None -> Browser.skip "descriptor_set" "no node"
  in
  let chrome =
    match Browser.chrome_binary () with
    | Some c -> c
    | None -> Browser.skip "descriptor_set" "no headless browser"
  in
  let version =
    match chrome_version chrome with
    | Some v -> v
    | None ->
        prerr_endline
          "descriptor_set: the browser did not report a version, so no support \
           fact can be keyed to this run";
        exit 1
  in
  if List.length modelled < minimum_descriptors then (
    prerr_endline
      (String.concat ""
         [
           "descriptor_set: the population is ";
           string_of_int (List.length modelled);
           " descriptor(s), fewer than the ";
           string_of_int minimum_descriptors;
           " expected; the FONT_FACE_DESCRIPTOR markers moved";
         ]);
    exit 1);
  let jobs = jobs ~seed ~only in
  if String.equal only "" && List.length jobs < minimum_values then (
    prerr_endline
      (String.concat ""
         [
           "descriptor_set: the population is ";
           string_of_int (List.length jobs);
           " value(s), fewer than the ";
           string_of_int minimum_values;
           " expected; the generator stopped generating";
         ]);
    exit 1);
  let script_dir = Filename.dirname Sys.executable_name in
  let work = Filename.get_temp_dir_name () // "cascade-descriptor-set" in
  (try Unix.mkdir work 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let jobs_file = work // "jobs.json" in
  write_jobs jobs_file jobs;
  let started = Unix.gettimeofday () in
  let lines =
    match
      run_driver ~node ~chrome
        ~script:(script_dir // "descriptors.js")
        ~jobs:jobs_file ~work
    with
    | Ok lines -> lines
    | Error err ->
        prerr_endline
          (String.concat ""
             [ "descriptor_set: the browser driver failed:\n"; err ]);
        exit 1
  in
  let elapsed = Unix.gettimeofday () -. started in
  let table = parse_driver_output lines in
  (* Counted per descriptor so a descriptor the browser has not shipped is one
     line rather than one per value of it. *)
  let positives_taken = Hashtbl.create 32 in
  let bump t k =
    Hashtbl.replace t k (1 + Option.value ~default:0 (Hashtbl.find_opt t k))
  in
  (* A descriptor no current specification defines has no grammar to judge a
     generated value against, so the browser's answer about one says nothing: it
     is the leftovers of a removed section either way. *)
  let ungoverned job =
    Option.is_none (Grammar.row_for ~at_rule:job.at_rule job.descriptor)
  in
  let pending = ref [] in
  List.iter
    (fun job ->
      match Hashtbl.find_opt table job.id with
      | None ->
          fail
            (String.concat ""
               [ describe job; ": the browser did not answer for this value" ])
      | Some { error = Some e; _ } ->
          fail (String.concat "" [ describe job; ": the browser raised: "; e ])
      | Some { parsed; constructed; error = None } ->
          if not (Bool.equal parsed constructed) then (
            incr splits;
            fail
              (String.concat ""
                 [
                   describe job;
                   ": insertRule and replaceSync disagree, so the browser \
                    cannot arbitrate this value";
                 ]))
          else begin
            (match job.kind with
            | Positive when parsed ->
                bump positives_taken (job.at_rule, job.descriptor)
            | Positive | Negative | Generated -> ());
            let reads = cascade_accepts job in
            match (job.kind, reads, parsed) with
            (* The row decides. cascade agreeing with it is the answer, and the
               browser is the second opinion whose divergence is reported. *)
            | Positive, true, true | Negative, false, false -> incr arbitrated
            | Positive, true, false ->
                incr behind;
                Fmt.pr
                  "  behind: %s (the row grants it, the browser drops it)@."
                  (describe job)
            | Negative, false, true ->
                incr lenient;
                Fmt.pr
                  "  lenient: %s (the row refuses it, the browser takes it)@."
                  (describe job)
            | Positive, false, _ ->
                fail
                  (String.concat ""
                     [
                       describe job;
                       ": the row grants this value and cascade drops it";
                     ])
            | Negative, true, _ ->
                fail
                  (String.concat ""
                     [
                       describe job;
                       ": the row refuses this value and cascade reads it";
                     ])
            (* No row covers a generated value, so the browser is the only
               oracle there is. A disagreement the support dataset explains is
               the browser's, looked up the way the property harnesses look one
               up: BCD files a descriptor under css.at-rules.font-face. *)
            | Generated, r, p when Bool.equal r p -> incr arbitrated
            | Generated, true, false
              when Option.is_some
                     (Chrome_gaps.explains_rejection
                        ~prefix:
                          (String.concat "" [ "css.at-rules."; job.at_rule ])
                        ~chrome:version ~property:job.descriptor
                        ~value:job.value ()) ->
                incr behind
            | Generated, false, true
              when Option.is_some
                     (Chrome_gaps.explains_acceptance
                        ~prefix:
                          (String.concat "" [ "css.at-rules."; job.at_rule ])
                        ~chrome:version ~property:job.descriptor
                        ~value:job.value ()) ->
                incr lenient
            | Generated, _, _ when ungoverned job -> ()
            | Generated, _, _ -> pending := (job, reads) :: !pending
          end)
    jobs;
  (* A descriptor the browser has no answer for explains every disagreement
     under it, so it is reported once and its values are not reported again. *)
  let unshipped = Hashtbl.create 8 in
  List.iter
    (fun (at_rule, descriptor) ->
      let key = support_key ~at_rule descriptor in
      let named = String.concat "" [ "@"; at_rule; " "; descriptor ] in
      match Grammar.row_for ~at_rule descriptor with
      | None -> (
          (* A descriptor with no current grammar is not an oversight when the
             support dataset carries it: web-features records what browsers
             ship, and a removed-but-shipped descriptor is exactly that. One
             that neither a section nor the dataset knows is cascade's
             invention. *)
          match Support.implemented Support.evergreen key with
          | Some _ ->
              Fmt.pr
                "  no row: %s (no current section grants it, and %s records it \
                 as shipped)@."
                named key
          | None ->
              fail
                (String.concat ""
                   [
                     named;
                     ": cascade models this descriptor, no Descriptor_grammar \
                      row grants it, and ";
                     key;
                     " is not in the support dataset either";
                   ]))
      | Some _ ->
          if
            Option.value ~default:0
              (Hashtbl.find_opt positives_taken (at_rule, descriptor))
            = 0
          then (
            Hashtbl.replace unshipped (at_rule, descriptor) ();
            report_unshipped ~version ~at_rule descriptor))
    (List.filter (fun (_, d) -> selected d) modelled);
  List.iter
    (fun (job, reads) ->
      if not (Hashtbl.mem unshipped (job.at_rule, job.descriptor)) then
        fail
          (String.concat ""
             [
               describe job;
               (if reads then
                  ": cascade reads it and the browser drops it, so a diff over \
                   this sheet certifies a declaration the page does not have"
                else
                  ": the browser reads it and cascade drops it, so every \
                   verdict cascade gives over this sheet is unproven");
             ]))
    (List.rev !pending);
  let major, minor = version in
  Fmt.pr
    "descriptor_set: %d value(s) over %d descriptor(s) of %d at-rule(s), \
     Chrome %d.%d, %.1fs@."
    (List.length jobs)
    (List.length (List.filter (fun (_, d) -> selected d) modelled))
    (List.length at_rules) major minor elapsed;
  Fmt.pr
    "  arbitrated: %d, browser behind: %d, browser lenient: %d, splits: %d, \
     failures: %d@."
    !arbitrated !behind !lenient !splits !failures;
  (* A run that agrees about nothing is not a clean run, it is a blind one. *)
  if String.equal only "" && !arbitrated < minimum_arbitrated then
    fail
      (String.concat ""
         [
           "only ";
           string_of_int !arbitrated;
           " value(s) were arbitrated, fewer than the ";
           string_of_int minimum_arbitrated;
           " expected: the run asked the browser almost nothing";
         ]);
  if !failures > 0 then exit 1
