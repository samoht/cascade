(* Every shorthand declaration must compare identical, under [--diff=canonical],
   to the longhand declarations it stands for.

   The decomposition is not written here and is not taken from cascade's own
   shorthand code: a table derived from the library would agree with the library
   by construction, and would have passed on [border-width] while that gap was
   live. It comes from a headless browser instead. The declaration is set on an
   element's inline style, and the CSSOM block that results is the expansion,
   resets included - the difference between [background: red] and
   [background-color: red] is nine declarations, and the browser is the one that
   knows it.

   Both directions of the property are checked. A shorthand the comparator
   refuses to equate with its own expansion is an over-report: safe, noisy, and
   a modelling gap. A shorthand the comparator equates with an expansion that is
   missing one of its longhands is a false negative, and that is the direction
   that lets a wrong rewrite through.

   Skips cleanly, with status 0, when node or a headless Chromium is missing.
   CASCADE_NO_BROWSER fails instead: see [Browser.suppressed]. *)

open Cascade

let ( // ) = Filename.concat

(* ===== Generated declarations ===== *)

(* A family generates its own values. [Sides] covers the shorthands whose
   grammar is one to [max] repetitions of a component - the box families and the
   two-axis ones - and [Fixed] carries the rest, where a generator would spend
   its time writing values the grammar rejects.

   The pools are loose on purpose. A combination the browser's grammar refuses
   comes back as an empty declaration block, and the run excludes it by name, so
   the browser does the validating. *)
type gen = Fixed of string list | Sides of { pool : string array; max : int }

let lengths = [| "1px"; "2px"; "0"; "3em" |]
let margins = [| "1px"; "2px"; "0"; "3em"; "auto" |]
let colours = [| "red"; "#0f0"; "rgb(1 2 3)"; "currentcolor" |]
let line_styles = [| "solid"; "dashed"; "none"; "double" |]
let overflows = [| "hidden"; "auto"; "visible"; "clip" |]
let alignments = [| "center"; "start"; "end"; "stretch" |]
let positions = [| "10px"; "center"; "left"; "50%" |]
let sides pool max = Sides { pool; max }

(* The shorthands to try. A name the browser does not expand, or a value it
   rejects, is excluded by the run and named in the report, so the list can
   reach past what either the browser or cascade implements. *)
let families =
  [
    (* box and axis families *)
    ("margin", sides margins 4);
    ("padding", sides lengths 4);
    ("inset", sides margins 4);
    ("border-width", sides lengths 4);
    ("border-style", sides line_styles 4);
    ("border-color", sides colours 4);
    ("border-radius", sides lengths 4);
    ("scroll-margin", sides lengths 4);
    ("scroll-padding", sides lengths 4);
    ("margin-block", sides margins 2);
    ("margin-inline", sides margins 2);
    ("padding-block", sides lengths 2);
    ("padding-inline", sides lengths 2);
    ("inset-block", sides margins 2);
    ("inset-inline", sides margins 2);
    ("scroll-margin-block", sides lengths 2);
    ("scroll-margin-inline", sides lengths 2);
    ("scroll-padding-block", sides lengths 2);
    ("scroll-padding-inline", sides lengths 2);
    ("border-block-width", sides lengths 2);
    ("border-inline-width", sides lengths 2);
    ("border-block-style", sides line_styles 2);
    ("border-inline-style", sides line_styles 2);
    ("border-block-color", sides colours 2);
    ("border-inline-color", sides colours 2);
    ("gap", sides lengths 2);
    ("overflow", sides overflows 2);
    ("overscroll-behavior", Fixed [ "contain"; "auto none"; "none contain" ]);
    ("place-content", sides alignments 2);
    ("place-items", sides alignments 2);
    ("place-self", sides alignments 2);
    ("background-position", sides positions 2);
    ("contain-intrinsic-size", Fixed [ "100px"; "100px 200px"; "auto 300px" ]);
    (* the slash form of the one box family that has one *)
    ( "border-radius",
      Fixed [ "1px 2px 3px 4px / 5px"; "10% / 20%"; "1px 2px / 3px 4px" ] );
    (* line families *)
    ( "border",
      Fixed [ "1px solid red"; "medium none currentcolor"; "2px dashed" ] );
    ("border-top", Fixed [ "1px solid red"; "thin dotted #0f0" ]);
    ("border-right", Fixed [ "1px solid red"; "thick double" ]);
    ("border-bottom", Fixed [ "1px solid red"; "0 none" ]);
    ("border-left", Fixed [ "1px solid red"; "3em groove currentcolor" ]);
    ("border-block", Fixed [ "1px solid red"; "2px dotted #0f0" ]);
    ("border-inline", Fixed [ "1px solid red"; "2px dotted #0f0" ]);
    ("border-block-start", Fixed [ "1px solid red"; "thin dashed" ]);
    ("border-block-end", Fixed [ "1px solid red"; "thin dashed" ]);
    ("border-inline-start", Fixed [ "1px solid red"; "thin dashed" ]);
    ("border-inline-end", Fixed [ "1px solid red"; "thin dashed" ]);
    ("outline", Fixed [ "1px solid red"; "thick dashed currentcolor" ]);
    ("column-rule", Fixed [ "1px solid red"; "medium none" ]);
    ("-webkit-text-stroke", Fixed [ "1px red"; "0 currentcolor" ]);
    (* the rest *)
    ("animation", Fixed [ "spin 2s linear"; "1s 2s infinite alternate none" ]);
    ("animation-range", Fixed [ "normal"; "entry 10% exit 90%" ]);
    ( "background",
      Fixed
        [
          "red";
          "url(a.png) no-repeat center / cover";
          "none";
          "#0f0 url(b.png) repeat-x fixed border-box content-box";
        ] );
    ( "border-image",
      Fixed [ "url(a.png) 30 / 1 / 0 stretch"; "none"; "url(b.png) 10 fill" ] );
    ("caret", Fixed [ "red auto"; "auto" ]);
    ("columns", Fixed [ "2 10em"; "auto"; "3" ]);
    ("container", Fixed [ "card / inline-size"; "none" ]);
    ("flex", Fixed [ "1"; "2 3 10px"; "none"; "auto" ]);
    ("flex-flow", Fixed [ "row wrap"; "column"; "wrap-reverse" ]);
    ("font", Fixed [ "italic bold 12px/1.5 serif"; "12px sans-serif" ]);
    ("font-synthesis", Fixed [ "none"; "weight style" ]);
    ("font-variant", Fixed [ "small-caps"; "normal"; "common-ligatures" ]);
    ("grid", Fixed [ "auto-flow / 1fr 1fr"; "none"; "1fr / auto-flow 2em" ]);
    ("grid-area", Fixed [ "1 / 2 / 3 / 4"; "a"; "auto" ]);
    ("grid-column", Fixed [ "1 / 3"; "span 2"; "auto" ]);
    ("grid-row", Fixed [ "1 / 3"; "span 2"; "auto" ]);
    ("grid-template", Fixed [ {|"a b" 1fr / 1fr 1fr|}; "none"; "1fr / 2fr" ]);
    ("list-style", Fixed [ "square inside"; "none"; "disc outside none" ]);
    ( "mask",
      Fixed [ "url(a.png) center / cover no-repeat"; "none"; "url(b.png)" ] );
    ("mask-border", Fixed [ "url(a.png) 30 / 1 / 0 stretch"; "none" ]);
    ("offset", Fixed [ {|path("M 0 0 L 10 10") 10px auto|}; "none" ]);
    ("position-try", Fixed [ "most-width --a"; "normal --b" ]);
    ("scroll-timeline", Fixed [ "--t block"; "none" ]);
    ("text-decoration", Fixed [ "underline dotted red"; "none"; "overline 2px" ]);
    ("text-emphasis", Fixed [ "filled red"; "none" ]);
    ("text-wrap", Fixed [ "balance"; "wrap"; "nowrap" ]);
    ("transition", Fixed [ "color 1s"; "all 2s ease-in 1s"; "none" ]);
    ("view-timeline", Fixed [ "--v block"; "none" ]);
    ("white-space", Fixed [ "pre-wrap"; "normal"; "pre" ]);
    (* [all] is listed so the run excludes it rather than this table: the
       browser leaves it whole in a declaration block, and the run says so. *)
    ("all", Fixed [ "initial"; "unset" ]);
  ]

let per_family = 8

let generated rng gen =
  match gen with
  | Fixed l -> l
  | Sides { pool; max } ->
      List.init per_family (fun _ ->
          let k = 1 + Random.State.int rng max in
          String.concat " "
            (List.init k (fun _ ->
                 pool.(Random.State.int rng (Array.length pool)))))
      |> List.sort_uniq String.compare

type job = { id : string; property : string; value : string }

let jobs () =
  let rng = Random.State.make [| 0x5104 |] in
  let n = ref 0 in
  List.concat_map
    (fun (property, gen) ->
      List.map
        (fun value ->
          incr n;
          { id = Fmt.str "sh-%d" !n; property; value })
        (generated rng gen))
    families

(* ===== Driver protocol ===== *)

(* One longhand of an expansion. [specified] is what the browser's declaration
   block holds; [substituted] differs only where that is the keyword [initial],
   and then carries the computed value of the longhand on an untouched element.
   The two are the same declaration written two ways, and the check accepts
   either, so a comparator that does not fold [initial] into a property's
   initial value is not counted as a shorthand gap. *)
type longhand = { name : string; specified : string; substituted : string }

type expansion = {
  count : int;  (** longhands reported, 0 when the browser rejected the value *)
  longhands : longhand list;
  error : string option;
}

let empty_expansion = { count = 0; longhands = []; error = None }
let int_of s = try int_of_string (String.trim s) with Failure _ -> 0

let parse_driver_output lines =
  let table = Hashtbl.create 512 in
  let get id = try Hashtbl.find table id with Not_found -> empty_expansion in
  List.iter
    (fun line ->
      match String.split_on_char '\t' line with
      | [ "n"; id; n ] ->
          Hashtbl.replace table id { (get id) with count = int_of n }
      | [ "l"; id; name; specified; substituted ] ->
          let e = get id in
          Hashtbl.replace table id
            {
              e with
              longhands = e.longhands @ [ { name; specified; substituted } ];
            }
      | "x" :: id :: rest ->
          Hashtbl.replace table id
            { (get id) with error = Some (String.concat "\t" rest) }
      | _ -> ())
    lines;
  table

let read_lines ic =
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file -> List.rev acc
  in
  loop []

let run_driver ~node ~chrome ~script ~jobs ~work =
  let errors = work // "shorthand.err" in
  let cmd =
    Fmt.str "CHROME=%s %s %s %s %s 2>%s" (Filename.quote chrome)
      (Filename.quote node) (Filename.quote script) (Filename.quote jobs)
      (Filename.quote work) (Filename.quote errors)
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

(* ===== The property ===== *)

let wrap decls = String.concat "" [ ".a{"; decls; "}" ]

let declarations pick longhands =
  String.concat ";"
    (List.map (fun l -> String.concat ":" [ l.name; pick l ]) longhands)

(* A parse warning means that side lost a declaration, so its canonical form
   stands for less than its text does and no verdict about it is worth having.
   The comparison reports both sides' warnings, so they are reported here rather
   than compared through. *)
type verdict = Equal | Differs | Dropped of string

let first_line s =
  match String.index_opt s '\n' with Some i -> String.sub s 0 i | None -> s

let verdict shorthand decls =
  match
    Cascade_diff.Css_compare.diff ~mode:`Canonical shorthand (wrap decls)
  with
  | exception (Reader.Parse_error _ | Failure _ | Invalid_argument _) -> Differs
  | d -> (
      match (d.expected_warnings, d.actual_warnings) with
      | e :: _, _ ->
          Dropped
            (String.concat ""
               [ "the shorthand: "; first_line (Error.to_string e) ])
      | [], a :: _ ->
          Dropped
            (String.concat ""
               [ "a longhand: "; first_line (Error.to_string a) ])
      | [], [] -> (
          match d.result with
          | Cascade_diff.Css_compare.No_diff -> Equal
          | _ -> Differs))

(* An expansion with one longhand dropped is a different declaration set: the
   shorthand still writes the longhand that is gone. A comparator that equates
   the two has equated something it must not. *)
let without n longhands = List.filteri (fun i _ -> i <> n) longhands

type outcome =
  | Excluded of string  (** the browser cannot answer for this one *)
  | Dropped_by_cascade of string  (** valid CSS the parser refused *)
  | Equated
  | Gap of string  (** the expansion the comparator did not equate *)
  | Conflated of string  (** an expansion missing a longhand, equated anyway *)

(* The shorthand equated with an expansion that keeps every longhand but one is
   the false negative. It is looked for only once the full expansion is equated,
   which is the only state it can hide in. *)
let conflation shorthand pick longhands =
  let short =
    List.filteri
      (fun i _ ->
        verdict shorthand (declarations pick (without i longhands)) = Equal)
      longhands
  in
  match short with
  | [] -> Equated
  | l :: _ ->
      Conflated
        (String.concat "" [ "equated with the expansion without "; l.name ])

let outcome job expansion =
  match expansion.error with
  | Some e -> Excluded (String.concat "" [ "the browser raised: "; e ])
  | None when expansion.count <> List.length expansion.longhands ->
      Excluded "the driver reported fewer longhands than it counted"
  | None -> (
      let shorthand = wrap (String.concat ":" [ job.property; job.value ]) in
      match expansion.longhands with
      | [] -> Excluded "the browser rejected the value"
      | [ l ] when l.name = job.property ->
          Excluded "the browser does not expand this property"
      | longhands -> (
          let specified l = l.specified and substituted l = l.substituted in
          let judge pick = verdict shorthand (declarations pick longhands) in
          match (judge specified, judge substituted) with
          | Dropped why, _ | _, Dropped why -> Dropped_by_cascade why
          | Differs, Differs -> Gap (declarations specified longhands)
          | Equal, _ | _, Equal ->
              if verdict shorthand "" = Equal then
                Excluded "both sides canonicalize to an empty rule"
              else
                let pick =
                  if judge specified = Equal then specified else substituted
                in
                conflation shorthand pick longhands))

(* ===== Reporting ===== *)

let only = ref None
let list_only = ref false

let usage () =
  Fmt.pr "usage: shorthand_expand [--only SUBSTRING] [--list]@.";
  exit 0

let parse_args () =
  let rec loop i =
    if i < Array.length Sys.argv then (
      (match Sys.argv.(i) with
      | "--only" when i + 1 < Array.length Sys.argv ->
          only := Some Sys.argv.(i + 1)
      | "--list" -> list_only := true
      | "-h" | "--help" -> usage ()
      | _ -> ());
      loop (i + 1))
  in
  loop 1

let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec at i =
    i + n <= h && (String.sub haystack i n = needle || at (i + 1))
  in
  n = 0 || at 0

(* What one family's declarations came to, in the order the report prints
   them. *)
type family = {
  name : string;
  equated : int;
  excluded : (string * int) list;
  gaps : (string * string) list;  (** value, the expansion not equated *)
  dropped : (string * string) list;  (** value, why the parser refused it *)
  conflations : (string * string) list;  (** value, what was equated *)
}

let empty name =
  {
    name;
    equated = 0;
    excluded = [];
    gaps = [];
    dropped = [];
    conflations = [];
  }

let failing f =
  List.length f.gaps + List.length f.dropped + List.length f.conflations

let tally jobs table =
  let order = ref [] and by_name = Hashtbl.create 128 in
  List.iter
    (fun job ->
      let f =
        match Hashtbl.find_opt by_name job.property with
        | Some f -> f
        | None ->
            order := job.property :: !order;
            empty job.property
      in
      let expansion =
        try Hashtbl.find table job.id with Not_found -> empty_expansion
      in
      let f =
        match outcome job expansion with
        | Equated -> { f with equated = f.equated + 1 }
        | Excluded reason ->
            let n = try List.assoc reason f.excluded with Not_found -> 0 in
            {
              f with
              excluded = (reason, n + 1) :: List.remove_assoc reason f.excluded;
            }
        | Gap decls -> { f with gaps = f.gaps @ [ (job.value, decls) ] }
        | Dropped_by_cascade why ->
            { f with dropped = f.dropped @ [ (job.value, why) ] }
        | Conflated what ->
            { f with conflations = f.conflations @ [ (job.value, what) ] }
      in
      Hashtbl.replace by_name job.property f)
    jobs;
  List.rev_map (fun name -> Hashtbl.find by_name name) !order

(* Every declaration behind a failing family, so one run is the whole list and
   nobody has to fix one value to see the next. *)
let report_family f =
  List.iter
    (fun (value, what) -> Fmt.pr "  conflated %s: %s: %s@." f.name value what)
    f.conflations;
  List.iter
    (fun (value, why) ->
      Fmt.pr "  dropped %s: %s: cascade rejects %s@." f.name value why)
    f.dropped;
  List.iter
    (fun (value, decls) ->
      Fmt.pr "  %s: %s@." f.name value;
      Fmt.pr "  expands to: %s@." decls)
    f.gaps

(* ===== Main ===== *)

let () =
  parse_args ();
  let jobs =
    match !only with
    | None -> jobs ()
    | Some needle -> List.filter (fun j -> contains j.property needle) (jobs ())
  in
  if !list_only then (
    List.iter (fun j -> Fmt.pr "%s: %s@." j.property j.value) jobs;
    exit 0);
  Browser.suppressed "shorthand_expand";
  let node =
    match Browser.node_binary () with
    | Some n -> n
    | None -> Browser.skip "shorthand_expand" "no node"
  in
  let chrome =
    match Browser.chrome_binary () with
    | Some c -> c
    | None -> Browser.skip "shorthand_expand" "no headless browser"
  in
  let script_dir = Filename.dirname Sys.executable_name in
  let work = Filename.get_temp_dir_name () // "cascade-shorthand-expand" in
  (try Unix.mkdir work 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let jobs_file = work // "jobs.json" in
  let oc = open_out jobs_file in
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
  close_out oc;
  let started = Unix.gettimeofday () in
  let lines =
    match
      run_driver ~node ~chrome
        ~script:(script_dir // "shorthand.js")
        ~jobs:jobs_file ~work
    with
    | Ok lines -> lines
    | Error err ->
        prerr_endline
          (String.concat ""
             [ "shorthand_expand: the browser driver failed:\n"; err ]);
        exit 1
  in
  let elapsed = Unix.gettimeofday () -. started in
  let families = tally jobs (parse_driver_output lines) in
  let failures = ref 0 in
  let equated = ref 0 and excluded = ref 0 in
  let gaps = ref 0 and dropped = ref 0 and conflations = ref 0 in
  List.iter
    (fun f ->
      equated := !equated + f.equated;
      excluded :=
        !excluded + List.fold_left (fun a (_, n) -> a + n) 0 f.excluded;
      gaps := !gaps + List.length f.gaps;
      dropped := !dropped + List.length f.dropped;
      conflations := !conflations + List.length f.conflations;
      let n = failing f in
      if n > 0 then (
        incr failures;
        Fmt.pr "FAIL %s: %d of %d declaration(s) fail@." f.name n (n + f.equated);
        report_family f))
    families;
  List.iter
    (fun f ->
      List.iter
        (fun (reason, n) -> Fmt.pr "  excluded %s (%d): %s@." f.name n reason)
        f.excluded)
    families;
  Fmt.pr "shorthand_expand: %d declaration(s) over %d famil(ies), %.1fs@."
    (List.length jobs) (List.length families) elapsed;
  Fmt.pr
    "  equated: %d, not equated: %d, dropped by the parser: %d, conflated: %d, \
     excluded: %d@."
    !equated !gaps !dropped !conflations !excluded;
  (* A whole run that equates nothing is not a clean run, it is a blind one. A
     filtered run is free to cover only families that fail. *)
  if !equated = 0 && !only = None then (
    incr failures;
    Fmt.pr "FAIL: no declaration was equated with its expansion at all@.");
  Fmt.pr "  failures: %d@." !failures;
  if !failures > 0 then exit 1
