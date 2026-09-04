(* Render a stylesheet and its optimized forms in a headless browser and check
   that every element computes the same style under each. A disagreement is an
   optimizer bug: the two sheets do not render the same page.

   Skips cleanly, with status 0, when node or a headless Chromium is missing.
   CASCADE_NO_BROWSER fails instead: see [Browser.suppressed]. *)

open Cascade

let ( // ) = Filename.concat

(* ===== Environment ===== *)

let getenv = Browser.getenv

(* ===== Files ===== *)

let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec at i =
    i + n <= h && (String.sub haystack i n = needle || at (i + 1))
  in
  n = 0 || at 0

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then (
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let write file contents =
  let oc = open_out file in
  output_string oc contents;
  close_out oc

let read_file file =
  let ic = open_in_bin file in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* ===== Inputs ===== *)

(* One tiny hand-written sheet, kept next to the harness rather than in a
   fixture file: it holds a shorthand and a longhand of the same family in each
   order - the shape behind two render-changing reorders - plus a selector list,
   a child and a sibling combinator, a position pseudo-class, an attribute
   selector and a pseudo-element. *)
let smoke_sheet =
  {css|
.card { margin: 0; margin-top: 8px; color: red }
.card > h2:first-child { row-gap: 9px; gap: 1px; font-weight: bold }
ul li + li { padding: 4px 8px; padding-left: 2px }
a[href^="https"], .card p { text-decoration: underline; color: #00f }
.card p::before { content: "> "; background: red; background-position-x: 10px }
|css}

(* The [transition] shorthand resets [transition-behavior], and a run of the
   other transition longhands does not write that slot. Contracting the run into
   the shorthand therefore has to carry the behaviour over, or the element stops
   transitioning discrete values. getComputedStyle reports the slot, so the
   browser settles it. *)
let transition_behavior_sheet =
  {css|
.rd-tb { transition-behavior: allow-discrete; transition-property: color; transition-duration: 1s; transition-timing-function: ease; transition-delay: 0s }
|css}

(* [of S] counted from the end. No corpus sheet carries the form and no pair
   below pins it, since a sheet without it renders the same page; what it buys
   is the probe - the driver reports a selector the derived document fails to
   match. *)
let nth_last_of_sheet = {css|li:nth-last-child(2 of .rd-c) { color: #090 }|css}

(* A sheet paired with a form that renders differently: [gap] resets the row gap
   the longhand set, so the two orders give the rule a different row gap. The
   canary fails when the harness reports no difference - a harness that cannot
   see a known render change proves nothing about the ones it misses. *)
let canary_sheet = {css|.k { row-gap: 9px; gap: 1px }|css}
let canary_reordered = {css|.k { gap: 1px; row-gap: 9px }|css}

(* CSS Nesting 1 sec. 4 reads [&] as [:is(<parent selector list>)], so a comma
   group under [&:first-child] paints a first child only. The document holds two
   adjacent [.rd-c], so a flattening that hands the pseudo-class to the last
   branch alone paints the second one too. The engine reads the nesting itself
   and the pair renders it against what flattening produced, so the rewrite is
   judged rather than repeated. *)
let nest_list_source =
  {css|
:is(.rd-c, .rd-d):first-child { color: #00f }
.rd-c + .rd-c { font-style: italic }
|css}

let nest_list_nested = {css|.rd-c, .rd-d { &:first-child { color: #00f } }|css}

let flattened css =
  match Css.of_string ~strict:false css with
  | Ok { stylesheet; _ } ->
      Css.to_string ~minify:true (Css.optimize ~flatten_nesting:true stylesheet)
  | Error _ -> css

(* Two more pairs, one per selector form the derived document has to get right.
   The [i] flag has to build a value the unflagged selector misses, and [of S]
   has to place the element among the siblings [S] matches; a document that
   ignores either renders the pair the same way, and the harness says so. *)
let attr_flag_sheet = {css|[data-rd="b" i] { color: #00f }|css}
let attr_flag_unflagged = {css|[data-rd="b"] { color: #00f }|css}
let nth_of_sheet = {css|:nth-child(2 of .rd-a) { color: #00f }|css}
let nth_of_other = {css|:nth-child(2 of .rd-b) { color: #00f }|css}

(* Two same-condition [@container] blocks with a rule between them, over an
   element the sheet makes a query container: the optimizer hoists the second
   block over that rule, and the browser has to compute the same style either
   way. Nothing generated carries a container query, so without this the sweep
   never renders one. *)
let distant_container_sheet =
  {css|
.rd-cq { container-type: inline-size; width: 300px }
@container (width >= 100px) { .rd-cq .rd-cr { color: #00f } }
.rd-cs { background-color: #0f0 }
@container (width >= 100px) { .rd-cq .rd-ct { padding-left: 6px } }
|css}

(* The same shape with the crossed rule writing the colour the blocks write, and
   beside it the merge a pass that skipped the conflict check would make. The
   source paints red and the merge paints green, so a harness that reports no
   difference here cannot see a wrong container merge at all. *)
let container_conflict_sheet =
  {css|
.rd-cw { container-type: inline-size; width: 300px }
@container (width >= 100px) { .rd-cw .rd-cx { color: #00f } }
.rd-cw .rd-cx { color: #090 }
@container (width >= 100px) { .rd-cw .rd-cx { color: #f00 } }
|css}

let container_conflict_hoisted =
  {css|
.rd-cw { container-type: inline-size; width: 300px }
@container (width >= 100px) { .rd-cw .rd-cx { color: #00f } .rd-cw .rd-cx { color: #f00 } }
.rd-cw .rd-cx { color: #090 }
|css}

(* [Differs] is the expected-failure marker: the harness fails when the pair it
   names renders the same, so a fix cannot leave the pin behind. *)
type expectation = Same | Differs of string

type input = {
  id : string;
  source : string; (* the document is derived from this sheet *)
  sheets : (string * string) list option; (* None: the optimize variants *)
  expect : expectation;
}

(* Inputs the sweep is known to fail. Each is a render change the optimizer
   makes today; fixing one is its own piece of work, so the sweep pins them here
   and stays green on everything else. *)
let known = []

let sheet id source =
  let expect =
    match List.assoc_opt id known with
    | Some reason -> Differs reason
    | None -> Same
  in
  { id; source; sheets = None; expect }

(* The committed corpora sit at a fixed place in the tree; the sweep runs both
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

let entries dir =
  match Sys.readdir dir with
  | exception Sys_error _ -> []
  | e ->
      Array.sort compare e;
      Array.to_list e

(* test/interop/css-minify-tests: 398 small sheets across 28 categories, one
   [source.css] each. Enough shapes - shorthands, nesting, layers, selectors -
   that the derived documents cover most of what a browser has to agree on. *)
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
                  Some ("corpus-" ^ category ^ "-" ^ id, file)
                else None)
              (entries dir))
        (entries root)

(* test/examples: the two committed hand-written sheets, an order of magnitude
   larger than a corpus case. *)
let example_files () =
  match repo_relative ("test" // "examples") with
  | None -> []
  | Some dir ->
      List.filter_map
        (fun file ->
          if Filename.check_suffix file ".css" then
            Some ("example-" ^ Filename.remove_extension file, dir // file)
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

let full = ref false
let seeds = ref None
let corpus = ref None
let only = ref None

let usage () =
  Fmt.pr
    "usage: render_diff [--full] [--seeds N] [--corpus N] [--only SUBSTRING]@.";
  exit 0

let parse_args () =
  let rec loop i =
    if i < Array.length Sys.argv then (
      let arg n =
        if i + 1 < Array.length Sys.argv then Sys.argv.(i + 1) else n
      in
      (match Sys.argv.(i) with
      | "--full" -> full := true
      | "--seeds" -> seeds := int_of_string_opt (arg "")
      | "--corpus" -> corpus := int_of_string_opt (arg "")
      | "--only" -> only := Some (arg "")
      | "-h" | "--help" -> usage ()
      | _ -> ());
      loop (i + 1))
  in
  loop 1

let inputs () =
  parse_args ();
  (* The whole committed corpus renders in under two seconds, so the default
     sweep takes all of it; [--full] only widens the generated half. *)
  let n_seeds =
    match !seeds with Some n -> n | None -> if !full then 256 else 32
  in
  let n_corpus = match !corpus with Some n -> n | None -> 0 in
  let files = sample n_corpus (corpus_files ()) @ example_files () in
  let generated =
    List.init n_seeds (fun i ->
        sheet
          ("seed-" ^ string_of_int i)
          (Css.to_string (Gen_sheet.stylesheet ~seed:i)))
  in
  let all =
    [
      sheet "smoke" smoke_sheet;
      sheet "transition-behavior" transition_behavior_sheet;
      sheet "nth-last-child-of" nth_last_of_sheet;
      sheet "distant-container" distant_container_sheet;
      {
        id = "container-conflict";
        source = container_conflict_sheet;
        sheets =
          Some
            [
              ("original", container_conflict_sheet);
              ("hoisted", container_conflict_hoisted);
            ];
        expect =
          Differs "the hoisted block loses the colour the crossed rule set";
      };
      {
        id = "canary";
        source = canary_sheet;
        sheets =
          Some [ ("original", canary_sheet); ("reordered", canary_reordered) ];
        expect = Differs "gap resets the row gap the longhand set";
      };
      {
        id = "nest-list";
        source = nest_list_source;
        sheets =
          Some
            [
              ("nested", nest_list_nested);
              ("flattened", flattened nest_list_nested);
            ];
        expect = Same;
      };
      {
        id = "attr-flag";
        source = attr_flag_sheet;
        sheets =
          Some
            [
              ("original", attr_flag_sheet); ("unflagged", attr_flag_unflagged);
            ];
        expect = Differs "the i flag matches a value the unflagged form misses";
      };
      {
        id = "nth-child-of";
        source = nth_of_sheet;
        sheets = Some [ ("original", nth_of_sheet); ("other-of", nth_of_other) ];
        expect = Differs "the second .rd-a is not a .rd-b";
      };
    ]
    @ List.map (fun (id, file) -> sheet id (read_file file)) files
    @ generated
  in
  match !only with
  | None -> all
  | Some needle -> List.filter (fun i -> contains i.id needle) all

(* ===== Variants ===== *)

(* The reference is the printed source: comparing against it isolates what
   [optimize] did from what the parser and the printer did. *)
let variants sheet =
  let optimized = Css.optimize sheet in
  let minified = Css.to_string ~minify:true optimized in
  let reparsed =
    match Css.of_string ~strict:false minified with
    | Ok { stylesheet; _ } -> Css.to_string ~minify:true stylesheet
    | Error _ -> minified
  in
  [
    ("original", Css.to_string sheet);
    ("optimize", minified);
    ( "lossless",
      Css.to_string ~minify:true ~lossless:true
        (Css.optimize ~lossless:true sheet) );
    ("reparsed", reparsed);
  ]

(* ===== Canonical filter ===== *)

(* getComputedStyle spells one render more than one way ([0% 0%] against [0px
   0px], [red] against [rgb(255, 0, 0)]), so a raw string difference is only a
   candidate. It survives when the two values are not canonically equal, which
   is when the optimizer changed the render. *)
let wrap prop value = String.concat "" [ "x{"; prop; ":"; value; "}" ]

let canonically_equal prop a b =
  try
    Cascade_diff.Css_compare.equal ~mode:`Canonical (wrap prop a) (wrap prop b)
  with Reader.Parse_error _ | Failure _ | Invalid_argument _ -> false

(* [optimize] without [~lossless] approximates colours by design: it rounds a
   channel and rewrites a colour into whichever space spells it shortest. Both
   change what getComputedStyle reports for a colour-valued property, so under
   the lossy variants such a difference is counted and reported rather than
   failed. Everything else, and every difference at all under [lossless], is a
   render change the optimizer must not make. *)
let lossy variant = variant = "optimize" || variant = "reparsed"

let colour_valued prop =
  contains prop "color"
  || List.mem prop
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

(* ===== Driver protocol ===== *)

type diff = {
  variant : string;
  element : string;
  property : string;
  reference : string;
  observed : string;
}

type result = {
  diffs : diff list;
  candidates : int;
  approximations : int;
  matched : int;
  unmatched : int;
  rejected : int;
  unmatched_examples : string list;
  errors : string list;
}

let empty_result =
  {
    diffs = [];
    candidates = 0;
    approximations = 0;
    matched = 0;
    unmatched = 0;
    rejected = 0;
    unmatched_examples = [];
    errors = [];
  }

let int_of s = try int_of_string (String.trim s) with Failure _ -> 0

let parse_driver_output lines =
  let table = Hashtbl.create 16 in
  let get id = try Hashtbl.find table id with Not_found -> empty_result in
  List.iter
    (fun line ->
      match String.split_on_char '\t' line with
      | "d" :: id :: variant :: _index :: element :: property :: reference
        :: observed :: _ ->
          let r = get id in
          let r = { r with candidates = r.candidates + 1 } in
          let r =
            if canonically_equal property reference observed then r
            else if lossy variant && colour_valued property then
              { r with approximations = r.approximations + 1 }
            else
              {
                r with
                diffs =
                  r.diffs
                  @ [ { variant; element; property; reference; observed } ];
              }
          in
          Hashtbl.replace table id r
      | [ "c"; id; matched; unmatched; rejected ] ->
          let r = get id in
          Hashtbl.replace table id
            {
              r with
              matched = int_of matched;
              unmatched = int_of unmatched;
              rejected = int_of rejected;
            }
      | [ "u"; id; selector ] ->
          let r = get id in
          Hashtbl.replace table id
            { r with unmatched_examples = r.unmatched_examples @ [ selector ] }
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

(* ===== Artefacts ===== *)

(* A page that rebuilds the derived document with the builder the driver used,
   so opening the artefact in a browser reproduces the run. *)
let repro ~css ~dom =
  String.concat ""
    [
      "<!doctype html><html><head><meta charset=\"utf-8\"><style>";
      css;
      "</style></head><body><script id=\"rd-doc\" type=\"application/json\">";
      dom;
      "</script><script src=\"dom.js\"></script><script>rdBuild(document, \
       JSON.parse(document.getElementById('rd-doc').textContent));</script></body></html>";
    ]

let write_artefacts ~dir ~script_dir ~dom ~sheets ~diffs =
  mkdir_p dir;
  write (dir // "dom.json") dom;
  write (dir // "dom.js") (read_file (script_dir // "dom.js"));
  List.iter (fun (name, css) -> write (dir // (name ^ ".css")) css) sheets;
  List.iter
    (fun (name, css) ->
      write (dir // ("page-" ^ name ^ ".html")) (repro ~css ~dom))
    sheets;
  let buf = Buffer.create 4096 in
  let out = Fmt.with_buffer buf in
  List.iter
    (fun d ->
      Fmt.pf out "%s\t%s\t%s\t%s\t%s\n" d.variant d.element d.property
        d.reference d.observed)
    diffs;
  Fmt.flush out ();
  write (dir // "diff.tsv") (Buffer.contents buf)

(* ===== Main ===== *)

(* Absolute, so the path the run prints can be opened from anywhere and the
   driver can turn the page into a file:// URL. *)
let artefact_root () =
  let dir =
    match getenv "CASCADE_RENDER_ARTIFACTS" with
    | Some d -> d
    | None -> "tmp" // "render-diff"
  in
  if Filename.is_relative dir then Sys.getcwd () // dir else dir

let skip reason = Browser.skip "render_diff" reason

let job_of_input input sheets doms unparsed =
  let id = input.id in
  match Css.of_string ~strict:false input.source with
  | Error _ ->
      incr unparsed;
      None
  | Ok { stylesheet; _ } ->
      let dom = Dom_of_css.of_stylesheet stylesheet in
      let vs =
        match input.sheets with Some vs -> vs | None -> variants stylesheet
      in
      Hashtbl.replace sheets id vs;
      Hashtbl.replace doms id dom;
      let fields =
        match Dom_of_css.to_json dom with
        | Json.Obj fields -> fields
        | other -> [ ("dom", other) ]
      in
      Some
        (Json.Obj
           ((("id", Json.Str id) :: fields)
           @ [
               ( "sheets",
                 Json.Arr
                   (List.map
                      (fun (name, css) ->
                        Json.Obj
                          [ ("name", Json.Str name); ("css", Json.Str css) ])
                      vs) );
             ]))

let () =
  Browser.suppressed "render_diff";
  let node =
    match Browser.node_binary () with Some n -> n | None -> skip "no node"
  in
  let chrome =
    match Browser.chrome_binary () with
    | Some c -> c
    | None -> skip "no headless browser"
  in
  let script_dir = Filename.dirname Sys.executable_name in
  let root = artefact_root () in
  let work = root // ".work" in
  mkdir_p work;
  let sheets = Hashtbl.create 16 in
  let doms = Hashtbl.create 16 in
  let unparsed = ref 0 in
  let inputs = inputs () in
  let jobs =
    List.filter_map (fun i -> job_of_input i sheets doms unparsed) inputs
  in
  let jobs_file = work // "jobs.json" in
  write jobs_file (Json.to_string (Json.Arr jobs));
  let started = Unix.gettimeofday () in
  let lines =
    match
      run_driver ~node ~chrome
        ~script:(script_dir // "driver.js")
        ~jobs:jobs_file ~work
    with
    | Ok lines -> lines
    | Error err ->
        prerr_endline ("render_diff: the browser driver failed:\n" ^ err);
        exit 1
  in
  let elapsed = Unix.gettimeofday () -. started in
  let table = parse_driver_output lines in
  let failures = ref 0 in
  let selectors = ref 0 and synthesised = ref 0 and elements = ref 0 in
  let skipped = Hashtbl.create 16 in
  let matched = ref 0 and unmatched = ref 0 and rejected = ref 0 in
  let candidates = ref 0 in
  let approximations = ref 0 in
  let surviving = ref 0 in
  let canaries = ref 0 in
  let unmatched_shown = ref 0 in
  List.iter
    (fun input ->
      let id = input.id in
      match Hashtbl.find_opt doms id with
      | None -> ()
      | Some dom -> (
          selectors := !selectors + Dom_of_css.selectors dom;
          synthesised := !synthesised + Dom_of_css.synthesised dom;
          elements := !elements + Dom_of_css.elements dom;
          List.iter
            (fun (reason, n) ->
              let prev =
                try Hashtbl.find skipped reason with Not_found -> 0
              in
              Hashtbl.replace skipped reason (prev + n))
            (Dom_of_css.skipped dom);
          let r = try Hashtbl.find table id with Not_found -> empty_result in
          matched := !matched + r.matched;
          unmatched := !unmatched + r.unmatched;
          rejected := !rejected + r.rejected;
          candidates := !candidates + r.candidates;
          approximations := !approximations + r.approximations;
          List.iter
            (fun e -> prerr_endline ("render_diff: " ^ id ^ ": " ^ e))
            r.errors;
          if r.errors <> [] then incr failures;
          List.iter
            (fun s ->
              if !unmatched_shown < 10 then (
                incr unmatched_shown;
                Fmt.pr "  probe matched no element: %s (%s)@." s id))
            r.unmatched_examples;
          match input.expect with
          | Differs reason ->
              if r.diffs = [] then (
                incr failures;
                Fmt.pr
                  "FAIL %s: expected a difference (%s) and saw none; remove \
                   the pin@."
                  id reason)
              else (
                incr canaries;
                let dir = root // id in
                write_artefacts ~dir ~script_dir
                  ~dom:(Json.to_string (Dom_of_css.to_json dom))
                  ~sheets:(Hashtbl.find sheets id) ~diffs:r.diffs;
                Fmt.pr "known %s: %s@." id reason;
                Fmt.pr "  artefacts: %s@." dir)
          | Same ->
              if r.diffs <> [] then (
                surviving := !surviving + List.length r.diffs;
                incr failures;
                let dir = root // id in
                write_artefacts ~dir ~script_dir
                  ~dom:(Json.to_string (Dom_of_css.to_json dom))
                  ~sheets:(Hashtbl.find sheets id) ~diffs:r.diffs;
                Fmt.pr "FAIL %s: %d computed-style difference(s)@." id
                  (List.length r.diffs);
                List.iteri
                  (fun i d ->
                    if i < 6 then
                      Fmt.pr "  [%s] %s %s: %S -> %S@." d.variant d.element
                        d.property d.reference d.observed)
                  r.diffs;
                Fmt.pr "  artefacts: %s@." dir)))
    inputs;
  Fmt.pr
    "render_diff: %d sheet(s), %d selector(s), %d synthesised, %d element(s), \
     %.1fs@."
    (Hashtbl.length doms) !selectors !synthesised !elements elapsed;
  Fmt.pr "  probes: %d matched, %d unmatched, %d rejected@." !matched !unmatched
    !rejected;
  Fmt.pr
    "  computed-style candidates: %d, lossy colour approximations: %d, render \
     differences: %d, canaries seen: %d@."
    !candidates !approximations !surviving !canaries;
  if !unparsed > 0 then Fmt.pr "  unparsed inputs: %d@." !unparsed;
  Hashtbl.fold (fun reason n acc -> (reason, n) :: acc) skipped []
  |> List.sort (fun (_, a) (_, b) -> compare b a)
  |> List.iter (fun (reason, n) ->
      Fmt.pr "  not synthesised: %-42s %d@." reason n);
  Fmt.pr "  failures: %d@." !failures;
  if !failures > 0 then exit 1
