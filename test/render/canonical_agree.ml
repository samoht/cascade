(* When [--diff=canonical] reports no difference, a browser must compute the
   same style for both sheets. The README states that as the guarantee -
   "canonical reports a difference only when some element would compute a
   different value" - and nothing checked it. [render_diff] asks whether the
   optimizer changed a render, and [shorthand_expand] asks the question of one
   shorthand against its own expansion; neither asks it of the verdict.

   The oracle is a headless browser, never cascade. Every pair the comparator
   calls identical is rendered against a document derived from its selectors,
   and every computed-style property the two sheets disagree on is a conflation:
   the comparator equated two sheets that are not the same stylesheet.

   getComputedStyle spells one value more than one way, and the spelling it
   reports is the one the sheet wrote: [ease] and [cubic-bezier(.25,.1,.25,1)]
   are the same easing, [0% 0%] and [0px 0px] the same background position. A
   raw string difference is therefore only a candidate, and the candidates are
   filtered the way [render_diff] filters them, with cascade's own comparator on
   the two values alone.

   That filter is the one place this harness leans on the code it tests, and it
   is worth naming what it hides: a comparator that equates two property values
   the browser resolves differently makes the sheets holding them invisible
   here. Value-level equality is what [shorthand_expand] and the declaration
   suite ask about; the question here is the sheet-level one, which nothing else
   asked.

   The pairs are mechanical, not chosen. Each base sheet is paired with what the
   tool emits for it, and with mutants of itself: one declaration dropped, two
   adjacent declarations swapped, one rule dropped, two adjacent rules swapped,
   one rule split into two rules sharing its selector. Whether a mutant changes
   the render is not decided here, and the comparator's verdict is only a
   filter: a pair it calls different costs no browser time, because an
   over-report is safe and the README documents several.

   Skips cleanly, with status 0, when node or a headless Chromium is missing.
   CASCADE_NO_BROWSER fails instead: see [Browser.suppressed]. *)

open Cascade

let ( // ) = Filename.concat

(* ===== Mutations ===== *)

(* [Css.map] reaches every rule at any depth and walks them depth-first, so a
   declaration is addressed by its position in the sheet's declaration sequence
   and the address does not depend on which block holds it. *)
let rewrite_declarations f sheet =
  let base = ref 0 in
  Css.v
    (Css.map
       (fun selector decls ->
         let start = !base in
         base := start + List.length decls;
         Css.rule ~selector (f start decls))
       (Css.statements sheet))

let declaration_count sheet =
  let n = ref 0 in
  ignore
    (Css.map
       (fun selector decls ->
         n := !n + List.length decls;
         Css.rule ~selector decls)
       (Css.statements sheet));
  !n

let drop_declaration i sheet =
  rewrite_declarations
    (fun start decls -> List.filteri (fun k _ -> start + k <> i) decls)
    sheet

(* Swap declaration [i] with the one after it, when both belong to one rule: a
   swap across a rule boundary is a rule reorder, which has its own mutation. *)
let swap_declarations i sheet =
  rewrite_declarations
    (fun start decls ->
      let n = List.length decls in
      let a = i - start in
      if a < 0 || a + 1 >= n then decls
      else
        let at k = List.nth decls k in
        List.mapi
          (fun k d ->
            if k = a then at (a + 1) else if k = a + 1 then at a else d)
          decls)
    sheet

(* Rule mutations stay at the top level. A rule inside a conditional group
   at-rule needs the group rebuilt around it, and the declaration mutations
   above already reach those rules. *)
let drop_rule j sheet =
  Css.v (List.filteri (fun k _ -> k <> j) (Css.statements sheet))

let swap_rules j sheet =
  let stmts = Css.statements sheet in
  let at k = List.nth stmts k in
  Css.v
    (List.mapi
       (fun k s -> if k = j then at (j + 1) else if k = j + 1 then at j else s)
       stmts)

(* A flat rule split into two rules sharing its selector: same selector, same
   specificity, same relative order, so the cascade is unchanged and the two
   sheets must render alike. That makes the split both an input shape - a
   longhand run the optimizer has to contract across a rule boundary - and a
   check on this harness, since a split that renders differently is a bug here
   rather than in the library. *)
let split_rule j k sheet =
  let stmts = Css.statements sheet in
  Css.v
    (List.concat
       (List.mapi
          (fun i s ->
            match (i = j, Css.as_rule s) with
            | true, Some (selector, decls, [])
              when k > 0 && k < List.length decls ->
                [
                  Css.rule ~selector (List.filteri (fun p _ -> p < k) decls);
                  Css.rule ~selector (List.filteri (fun p _ -> p >= k) decls);
                ]
            | (true | false), (Some _ | None) -> [ s ])
          stmts))

let split_points sheet =
  List.concat
    (List.mapi
       (fun j s ->
         match Css.as_rule s with
         | Some (_, decls, []) ->
             let n = List.length decls in
             List.filter_map
               (fun k -> if k > 0 && k < n then Some (j, k) else None)
               (List.init n (fun k -> k))
         | Some _ | None -> [])
       (Css.statements sheet))

(* ===== Base sheets ===== *)

(* Longhand runs, one family each. Only the values are written here: what the
   family expands to, and whether contracting it back is safe, is the browser's
   answer, and the split mutation turns each run into every way of writing it
   across two rules sharing a selector. That shape is the one the corpus does
   not carry - a corpus sheet writes a family in one rule - and it is where a
   contraction can quietly drop the slot the shorthand resets. *)
let longhand_runs =
  [
    ( "transition",
      "transition-behavior:allow-discrete;transition-property:color;transition-duration:1s;transition-timing-function:ease;transition-delay:0s"
    );
    ( "animation",
      "animation-name:spin;animation-duration:2s;animation-timing-function:linear;animation-delay:0s;animation-iteration-count:1;animation-direction:normal;animation-fill-mode:none;animation-play-state:running"
    );
    ( "background",
      "background-image:none;background-position:0 \
       0;background-size:auto;background-repeat:repeat;background-origin:padding-box;background-clip:border-box;background-attachment:scroll;background-color:red"
    );
    ( "border-top",
      "border-top-width:1px;border-top-style:solid;border-top-color:red" );
    ( "font",
      "font-style:italic;font-variant:normal;font-weight:700;font-stretch:normal;font-size:12px;line-height:1.5;font-family:serif"
    );
    ("flex", "flex-grow:2;flex-shrink:3;flex-basis:10px");
    ( "list-style",
      "list-style-position:inside;list-style-image:none;list-style-type:square"
    );
    ( "text-decoration",
      "text-decoration-line:underline;text-decoration-style:dotted;text-decoration-color:red;text-decoration-thickness:2px"
    );
    ("outline", "outline-width:1px;outline-style:solid;outline-color:red");
    ("overflow", "overflow-x:hidden;overflow-y:auto");
    ("columns", "column-width:10em;column-count:2");
    ("place-content", "align-content:center;justify-content:start");
    ("gap", "row-gap:9px;column-gap:1px");
    ( "margin",
      "margin-top:1px;margin-right:2px;margin-bottom:3px;margin-left:4px" );
    ( "grid-area",
      "grid-row-start:1;grid-column-start:2;grid-row-end:3;grid-column-end:4" );
    ( "border-image",
      "border-image-source:url(a.png);border-image-slice:30;border-image-width:1;border-image-outset:0;border-image-repeat:stretch"
    );
    ( "mask",
      "mask-image:url(a.png);mask-position:center;mask-size:cover;mask-repeat:no-repeat;mask-origin:border-box;mask-clip:border-box;mask-composite:add;mask-mode:match-source"
    );
    ( "offset",
      "offset-path:path(\"M 0 0 L 10 \
       10\");offset-distance:10px;offset-rotate:auto;offset-anchor:auto" );
    ("container", "container-name:card;container-type:inline-size");
    ( "font-synthesis",
      "font-synthesis-weight:none;font-synthesis-style:none;font-synthesis-small-caps:none"
    );
  ]

let run_sheet decls = String.concat "" [ ".ca{"; decls; "}" ]

let read_file file =
  let ic = open_in_bin file in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let entries dir =
  match Sys.readdir dir with
  | exception Sys_error _ -> []
  | e ->
      Array.sort String.compare e;
      Array.to_list e

(* The committed corpora sit at a fixed place in the tree; the run happens both
   from the repository root and from the build directory a dune rule gives it,
   so it looks for them upwards from wherever it started. *)
let repo_relative rel =
  let rec up dir fuel =
    if fuel = 0 then None
    else if Sys.file_exists (dir // rel) then Some (dir // rel)
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else up parent (fuel - 1)
  in
  up (Sys.getcwd ()) 12

let corpus_files () =
  match
    repo_relative
      ("test" // "interop" // "css-minify-tests" // "traces" // "tests")
  with
  | None -> []
  | Some root ->
      List.concat_map
        (fun category ->
          let dir = root // category in
          if not (try Sys.is_directory dir with Sys_error _ -> false) then []
          else
            List.filter_map
              (fun id ->
                let file = dir // id // "source.css" in
                if Sys.file_exists file then
                  Some
                    ( String.concat "" [ "corpus-"; category; "-"; id ],
                      read_file file )
                else None)
              (entries dir))
        (entries root)

let example_files () =
  match repo_relative ("test" // "examples") with
  | None -> []
  | Some dir ->
      List.filter_map
        (fun file ->
          if Filename.check_suffix file ".css" then
            Some
              ( String.concat "" [ "example-"; Filename.remove_extension file ],
                read_file (dir // file) )
          else None)
        (entries dir)

(* Spread the sample over the whole list rather than taking a prefix, so every
   category is represented. *)
let sample n l =
  let total = List.length l in
  if n <= 0 || total <= n then l
  else
    let step = (total + n - 1) / n in
    List.filteri (fun i _ -> i mod step = 0) l

(* ===== Variants ===== *)

(* [against] is the text the comparator is asked about, which is not always the
   text the browser renders against. A split renders like the sheet it came
   from, so the browser compares the split's minified form with the source while
   the comparator compares it with the split - the verdict is about the pair a
   caller would actually run [diff] on. [required] marks the variants rendered
   whatever the verdict says, which is how the split's own claim is checked. *)
type kind =
  | Tool  (** what the tool emits for the sheet *)
  | Split  (** a rule written as two rules sharing its selector *)
  | Mutant  (** a declaration or rule dropped or moved *)

type variant = {
  label : string;
  css : string;
  against : string;
  against_label : string;
  kind : kind;
  lossy : bool;
  required : bool;
}

let minified sheet = Css.to_string ~minify:true (Css.optimize sheet)

let lossless_minified sheet =
  Css.to_string ~minify:true ~lossless:true (Css.optimize ~lossless:true sheet)

let mutant label ~source sheet =
  {
    label;
    css = Css.to_string sheet;
    against = source;
    against_label = "source";
    kind = Mutant;
    lossy = false;
    required = false;
  }

let variants ~splits sheet =
  let source = Css.to_string sheet in
  let tool =
    [
      {
        label = "minify";
        css = minified sheet;
        against = source;
        against_label = "source";
        kind = Tool;
        lossy = true;
        required = false;
      };
      {
        label = "lossless";
        css = lossless_minified sheet;
        against = source;
        against_label = "source";
        kind = Tool;
        lossy = false;
        required = false;
      };
    ]
  in
  let split_variants =
    List.concat_map
      (fun (j, k) ->
        let name =
          String.concat "" [ "split-"; string_of_int j; "-"; string_of_int k ]
        in
        let s = split_rule j k sheet in
        let text = Css.to_string s in
        [
          {
            label = name;
            css = text;
            against = source;
            against_label = "source";
            kind = Split;
            lossy = false;
            required = true;
          };
          {
            label = String.concat "" [ name; "-minify" ];
            css = minified s;
            against = text;
            against_label = name;
            kind = Split;
            lossy = true;
            required = false;
          };
        ])
      splits
  in
  (tool, split_variants)

let mutants ~decls ~rules sheet =
  let source = Css.to_string sheet in
  let n = declaration_count sheet in
  let tops = List.length (Css.statements sheet) in
  let decl_ids = sample decls (List.init n (fun i -> i)) in
  let rule_ids = sample rules (List.init tops (fun i -> i)) in
  List.concat_map
    (fun i ->
      [
        mutant
          (String.concat "" [ "drop-decl-"; string_of_int i ])
          ~source (drop_declaration i sheet);
        mutant
          (String.concat "" [ "swap-decl-"; string_of_int i ])
          ~source
          (swap_declarations i sheet);
      ])
    decl_ids
  @ List.concat_map
      (fun j ->
        [
          mutant
            (String.concat "" [ "drop-rule-"; string_of_int j ])
            ~source (drop_rule j sheet);
        ]
        @
        if j + 1 < tops then
          [
            mutant
              (String.concat "" [ "swap-rule-"; string_of_int j ])
              ~source (swap_rules j sheet);
          ]
        else [])
      rule_ids

(* ===== The verdict ===== *)

(* A parse warning means that side lost a declaration, so its canonical form
   stands for less than its text does. The pair is still two real stylesheets
   and the browser still arbitrates, so the warning is reported alongside the
   verdict rather than used to drop the pair. *)
type verdict = Identical | Different | Unreadable

let verdict a b =
  match Cascade_diff.Css_compare.diff ~mode:`Canonical a b with
  | exception (Reader.Parse_error _ | Failure _ | Invalid_argument _) ->
      Unreadable
  | d -> (
      match d.result with
      | Cascade_diff.Css_compare.No_diff -> Identical
      | Tree_diff _ | String_diff _ | Both_errors _ | Expected_error _
      | Actual_error _ ->
          Different)

(* ===== Driver protocol ===== *)

type diff = {
  variant : string;
  element : string;
  property : string;
  reference : string;
  observed : string;
}

type result = { diffs : diff list; errors : string list }

let empty_result = { diffs = []; errors = [] }

let parse_driver_output lines =
  let table = Hashtbl.create 512 in
  let get id = try Hashtbl.find table id with Not_found -> empty_result in
  List.iter
    (fun line ->
      match String.split_on_char '\t' line with
      | "d" :: id :: variant :: _index :: element :: property :: reference
        :: observed :: _ ->
          let r = get id in
          Hashtbl.replace table id
            {
              r with
              diffs =
                r.diffs
                @ [ { variant; element; property; reference; observed } ];
            }
      | "x" :: id :: rest ->
          let r = get id in
          Hashtbl.replace table id
            { r with errors = r.errors @ [ String.concat "\t" rest ] }
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
  let errors = work // "driver.err" in
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

(* ===== Candidate filter ===== *)

let wrap prop value = String.concat "" [ "x{"; prop; ":"; value; "}" ]

let same_value prop a b =
  try
    Cascade_diff.Css_compare.equal ~mode:`Canonical (wrap prop a) (wrap prop b)
  with Reader.Parse_error _ | Failure _ | Invalid_argument _ -> false

let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec at i =
    i + n <= h && (String.equal (String.sub haystack i n) needle || at (i + 1))
  in
  n = 0 || at 0

(* [optimize] without [~lossless] approximates colours by design, and
   [--diff=canonical] matches it by design: the README says so and offers
   [--lossless] to turn it off. A colour difference under a variant that
   optimize built is therefore counted rather than failed. Everything else is a
   computed value the browser resolved differently on a pair the comparator
   called identical. *)
let colour_valued prop =
  contains prop "color"
  || List.exists (String.equal prop)
       [
         "fill";
         "stroke";
         "background-image";
         "border-image-source";
         "mask-image";
         "box-shadow";
         "text-shadow";
         "filter";
         "backdrop-filter";
       ]

(* ===== Inputs ===== *)

let corpus = ref None
let runs = ref true
let decls_budget = ref 8
let rules_budget = ref 4

(* Every cut point of every rule: the split is where the shapes the corpus does
   not write come from, and the whole sweep runs in seconds. *)
let splits_budget = ref 0
let only = ref None

let usage () =
  Fmt.pr
    "usage: canonical_agree [--corpus N] [--no-runs] [--decls N] [--rules N] \
     [--splits N] [--only SUBSTRING]@.";
  Fmt.pr "a budget of 0 takes every one, which is also the default@.";
  exit 0

let parse_args () =
  let rec loop i =
    if i < Array.length Sys.argv then (
      let arg () =
        if i + 1 < Array.length Sys.argv then Sys.argv.(i + 1) else ""
      in
      (match Sys.argv.(i) with
      | "--corpus" -> corpus := int_of_string_opt (arg ())
      | "--no-runs" -> runs := false
      | "--decls" ->
          decls_budget := Option.value ~default:8 (int_of_string_opt (arg ()))
      | "--rules" ->
          rules_budget := Option.value ~default:4 (int_of_string_opt (arg ()))
      | "--splits" ->
          splits_budget := Option.value ~default:0 (int_of_string_opt (arg ()))
      | "--only" -> only := Some (arg ())
      | "-h" | "--help" -> usage ()
      | _ -> ());
      loop (i + 1))
  in
  loop 1

let base_sheets () =
  let corpus_n = Option.value ~default:0 !corpus in
  let from_runs =
    if !runs then
      List.map
        (fun (name, decls) ->
          (String.concat "" [ "run-"; name ], run_sheet decls))
        longhand_runs
    else []
  in
  let all = from_runs @ sample corpus_n (corpus_files ()) @ example_files () in
  match !only with
  | None -> all
  | Some needle -> List.filter (fun (id, _) -> contains id needle) all

(* ===== Main ===== *)

let skip reason = Browser.skip "canonical_agree" reason

type job = {
  id : string;
  source : string;
  rendered : variant list;
  over_reports : int;
  unreadable : int;
}

let build_job (id, text) =
  match Css.of_string ~strict:false text with
  | Error _ -> None
  | Ok { stylesheet; _ } ->
      let source = Css.to_string stylesheet in
      let splits = sample !splits_budget (split_points stylesheet) in
      let tool, split_variants = variants ~splits stylesheet in
      let candidates =
        tool @ split_variants
        @ mutants ~decls:!decls_budget ~rules:!rules_budget stylesheet
      in
      let over = ref 0 and bad = ref 0 in
      let rendered =
        List.filter
          (fun v ->
            if String.equal v.css v.against then false
            else
              match verdict v.against v.css with
              | Identical -> true
              | Unreadable ->
                  incr bad;
                  v.required
              | Different ->
                  incr over;
                  v.required)
          candidates
      in
      Some { id; source; rendered; over_reports = !over; unreadable = !bad }

let job_json job dom =
  let fields =
    match Dom_of_css.to_json dom with
    | Json.Obj fields -> fields
    | Str _ | Int _ | Arr _ -> []
  in
  Json.Obj
    ((("id", Json.Str job.id) :: fields)
    @ [
        ( "sheets",
          Json.Arr
            (Json.Obj
               [ ("name", Json.Str "source"); ("css", Json.Str job.source) ]
            :: List.map
                 (fun v ->
                   Json.Obj
                     [ ("name", Json.Str v.label); ("css", Json.Str v.css) ])
                 job.rendered) );
      ])

let write file contents =
  let oc = open_out file in
  output_string oc contents;
  close_out oc

(* A sheet reads as one report line: the pair matters more than its layout. *)
let one_line css =
  let b = Buffer.create (String.length css) in
  String.iter
    (fun c ->
      match c with
      | '\n' | '\r' | '\t' | ' ' ->
          if Buffer.length b > 0 && Buffer.nth b (Buffer.length b - 1) <> ' '
          then Buffer.add_char b ' '
      | c -> Buffer.add_char b c)
    css;
  String.trim (Buffer.contents b)

let () =
  parse_args ();
  Browser.suppressed "canonical_agree";
  let node =
    match Browser.node_binary () with Some n -> n | None -> skip "no node"
  in
  let chrome =
    match Browser.chrome_binary () with
    | Some c -> c
    | None -> skip "no headless browser"
  in
  let script_dir = Filename.dirname Sys.executable_name in
  let work = Filename.get_temp_dir_name () // "cascade-canonical-agree" in
  (try Unix.mkdir work 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let jobs = List.filter_map build_job (base_sheets ()) in
  let jobs =
    List.filter
      (fun j -> match j.rendered with [] -> false | _ :: _ -> true)
      jobs
  in
  let payload =
    List.filter_map
      (fun job ->
        match Css.of_string ~strict:false job.source with
        | Error _ -> None
        | Ok { stylesheet; _ } ->
            Some (job_json job (Dom_of_css.of_stylesheet stylesheet)))
      jobs
  in
  let jobs_file = work // "jobs.json" in
  write jobs_file (Json.to_string (Json.Arr payload));
  let started = Unix.gettimeofday () in
  let lines =
    match
      run_driver ~node ~chrome
        ~script:(script_dir // "driver.js")
        ~jobs:jobs_file ~work
    with
    | Ok lines -> lines
    | Error err ->
        prerr_endline
          (String.concat ""
             [ "canonical_agree: the browser driver failed:\n"; err ]);
        exit 1
  in
  let elapsed = Unix.gettimeofday () -. started in
  let table = parse_driver_output lines in
  let failures = ref 0 in
  let pairs = ref 0 and over = ref 0 and unreadable = ref 0 in
  let tool_pairs = ref 0 and split_pairs = ref 0 and mutant_pairs = ref 0 in
  let approximations = ref 0 and spellings = ref 0 in
  let conflations = ref 0 in
  let split_breaks = ref 0 in
  List.iter
    (fun job ->
      pairs := !pairs + List.length job.rendered;
      List.iter
        (fun v ->
          match v.kind with
          | Tool -> incr tool_pairs
          | Split -> incr split_pairs
          | Mutant -> incr mutant_pairs)
        job.rendered;
      over := !over + job.over_reports;
      unreadable := !unreadable + job.unreadable;
      let r = try Hashtbl.find table job.id with Not_found -> empty_result in
      List.iter
        (fun e ->
          incr failures;
          prerr_endline
            (String.concat "" [ "canonical_agree: "; job.id; ": "; e ]))
        r.errors;
      (* Every property a pair disagrees on, under the pair that carries them,
         so one run is the whole list and nobody has to fix one to see the
         next. *)
      List.iter
        (fun v ->
          let mine =
            List.filter (fun d -> String.equal d.variant v.label) r.diffs
          in
          let kept =
            List.filter
              (fun d ->
                if same_value d.property d.reference d.observed then (
                  incr spellings;
                  false)
                else if v.lossy && colour_valued d.property then (
                  incr approximations;
                  false)
                else true)
              mine
          in
          match kept with
          | [] -> ()
          | _ :: _ ->
              incr failures;
              if v.required then (
                incr split_breaks;
                Fmt.pr
                  "FAIL %s: %s does not render like the rule it was split \
                   from; this harness is wrong, not the library@."
                  job.id v.label)
              else (
                incr conflations;
                Fmt.pr "FAIL %s: canonical equates %s with %s@." job.id
                  v.against_label v.label);
              Fmt.pr "  %s: %s@." v.against_label (one_line v.against);
              Fmt.pr "  %s: %s@." v.label (one_line v.css);
              List.iter
                (fun d ->
                  Fmt.pr "  %s %s: %S -> %S@." d.element d.property d.reference
                    d.observed)
                kept)
        job.rendered)
    jobs;
  Fmt.pr "canonical_agree: %d sheet(s), %d equated pair(s), %.1fs@."
    (List.length jobs) !pairs elapsed;
  Fmt.pr "  equated: %d tool, %d split, %d mutant@." !tool_pairs !split_pairs
    !mutant_pairs;
  Fmt.pr "  conflations: %d, broken splits: %d@." !conflations !split_breaks;
  Fmt.pr
    "  candidates the value filter dropped: %d spelling(s), %d lossy \
     colour(s)@."
    !spellings !approximations;
  Fmt.pr "  pairs the comparator kept apart: %d, unreadable: %d@." !over
    !unreadable;
  (* A run that renders no equated pair is not a clean run, it is a blind one:
     the comparator equates the tool's own output with its input on any sheet
     that parses, so an empty population means the harness stopped working. *)
  if !pairs = 0 then (
    incr failures;
    Fmt.pr "FAIL: no pair reached the browser at all@.");
  Fmt.pr "  failures: %d@." !failures;
  if !failures > 0 then exit 1
