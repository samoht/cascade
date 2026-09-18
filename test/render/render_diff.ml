(* Render a stylesheet and the forms cascade's transforms make of it in a
   headless browser and check that the page paints the same under each. The
   oracle is [Browser_compare.run] and nothing else: the page is loaded afresh
   under each sheet at every viewport and state the sheets can use, captured
   whole and compared pixel for pixel, and [Browser_compare.identical] is the
   verdict. A render that differs is a bug in the transform, since the two
   sheets do not paint the same page; nothing here decides that two spellings
   are equivalent, the browser paints them alike or it does not.

   The document is derived from the sheet's own selectors ([Dom_of_css]), with a
   word in every element so a declaration has something to paint. The transforms
   are the ones the README promises preserve rendering, each as one pair against
   the source: [Css.optimize], [Css.optimize ~lossless:true], the minified text
   read back, [Css.inline_vars], and [Cascade.Prune] over the derived page. A
   failure writes the page and both sheets next to it, so it reproduces with
   [cascade diff --browser --html].

   A pair costs a browser launch, a second or two, so the default run is a
   sample: the hand-written inputs, 24 corpus sheets, the two examples and 16
   seeds, about a hundred pairs in two to three minutes. [--full] renders every
   corpus sheet and 128 seeds, a thousand pairs in some twenty minutes;
   [--corpus N], [--seeds N] and [--only SUBSTRING] size the run by hand.

   Skips cleanly, with status 0, when node or a headless Chromium is missing.
   CASCADE_NO_BROWSER fails instead: see [Browser.suppressed]. *)

open Cascade
module Html = Cascade_html.Html
module Prune = Cascade.Prune.Make (Cascade_html.Html_node)

let ( // ) = Filename.concat

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
  Out_channel.with_open_bin file (fun oc -> output_string oc contents)

let read_file file = In_channel.with_open_bin file In_channel.input_all

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

(* [of S] counted from the end. No corpus sheet carries the form and no pair
   below pins it, since a sheet without it renders the same page; what it buys
   is the probe - the harness reports a selector the derived document fails to
   match. *)
let nth_last_of_sheet = {css|li:nth-last-child(2 of .rd-c) { color: #090 }|css}

(* A sheet paired with a form that renders differently: [margin] resets the top
   margin the longhand set, so the two orders put the element's text at a
   different height. The canary fails when the harness reports no difference - a
   harness that cannot see a known render change proves nothing about the ones
   it misses. *)
let canary_sheet = {css|.k { margin-top: 8px; margin: 0 }|css}
let canary_reordered = {css|.k { margin: 0; margin-top: 8px }|css}

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
   block over that rule, and the browser has to paint the same page either way.
   Nothing generated carries a container query, so without this the sweep never
   renders one. *)
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
  sheets : (string * string) list option;
      (* the sheets rendered, the first against each other; [None] for the
         transforms of [source] *)
  expect : expectation;
}

(* Pairs the sweep is known to fail, as [(input, variant, reason)]. Each is a
   render change a transform makes today; fixing one is its own piece of work,
   so the sweep pins them here and stays green on everything else. The lossy
   colour fold rounds a channel within the README's colour-difference budget,
   and a rounded channel is a pixel the browser paints a unit apart. *)
let lossy_colour = "the lossy colour fold rounds a channel the browser paints"

let known =
  [
    ("corpus-colors-0047", "optimize", lossy_colour);
    ("corpus-colors-0048", "optimize", lossy_colour);
    ("corpus-colors-0049", "optimize", lossy_colour);
    ("corpus-colors-0051", "optimize", lossy_colour);
    ("corpus-colors-0056", "optimize", lossy_colour);
  ]

let sheet id source = { id; source; sheets = None; expect = Same }

let pair id ~source ~expect first second =
  { id; source; sheets = Some [ first; second ]; expect }

let hand_written =
  [
    sheet "smoke" smoke_sheet;
    sheet "nth-last-child-of" nth_last_of_sheet;
    sheet "distant-container" distant_container_sheet;
    pair "container-conflict" ~source:container_conflict_sheet
      ~expect:
        (Differs "the hoisted block loses the colour the crossed rule set")
      ("original", container_conflict_sheet)
      ("hoisted", container_conflict_hoisted);
    pair "canary" ~source:canary_sheet
      ~expect:(Differs "margin resets the top margin the longhand set")
      ("original", canary_sheet)
      ("reordered", canary_reordered);
    pair "nest-list" ~source:nest_list_source ~expect:Same
      ("nested", nest_list_nested)
      ("flattened", flattened nest_list_nested);
    pair "attr-flag" ~source:attr_flag_sheet
      ~expect:(Differs "the i flag matches a value the unflagged form misses")
      ("original", attr_flag_sheet)
      ("unflagged", attr_flag_unflagged);
    pair "nth-child-of" ~source:nth_of_sheet
      ~expect:(Differs "the second .rd-a is not a .rd-b")
      ("original", nth_of_sheet) ("other-of", nth_of_other);
  ]

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
                  Some (String.concat "-" [ "corpus"; category; id ], file)
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
            Some
              ( String.concat "-" [ "example"; Filename.remove_extension file ],
                dir // file )
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
  Fmt.pr "a corpus budget of 0 takes every sheet@.";
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
  let n_seeds =
    match !seeds with Some n -> n | None -> if !full then 128 else 16
  in
  let n_corpus =
    match !corpus with Some n -> n | None -> if !full then 0 else 24
  in
  let files = sample n_corpus (corpus_files ()) @ example_files () in
  let generated =
    List.init n_seeds (fun i ->
        sheet
          (String.concat "-" [ "seed"; string_of_int i ])
          (Css.to_string (Gen_sheet.stylesheet ~seed:i)))
  in
  let all =
    hand_written
    @ List.map (fun (id, file) -> sheet id (read_file file)) files
    @ generated
  in
  match !only with
  | None -> all
  | Some needle -> List.filter (fun i -> contains i.id needle) all

(* ===== Variants ===== *)

let minified sheet = Css.to_string ~minify:true sheet

let reparsed css =
  match Css.of_string ~strict:false css with
  | Ok { stylesheet; _ } -> minified stylesheet
  | Error _ -> css

let exact sheet = Css.to_string ~minify:true ~lossless:true sheet

(* The reference is the printed source, so a difference is the transform's and
   not the parser's. [optimize] and its reparse are printed as [cascade
   --minify] prints them, approximation included; the other transforms are
   printed exactly, so a difference under one is that transform's and not the
   printer's. [prune] is judged over the derived page itself, the page the
   pruned sheet must still render. *)
let variants ~roots sheet =
  let optimized = minified (Css.optimize sheet) in
  [
    ("original", Css.to_string sheet);
    ("optimize", optimized);
    ("lossless", exact (Css.optimize ~lossless:true sheet));
    ("reparsed", reparsed optimized);
    ("inline-vars", exact (Css.inline_vars sheet));
    ("prune", exact (Prune.analyse ~sheet roots).sheet);
  ]

(* ===== Probes ===== *)

(* Whether the tree parsed back from the page holds an element for each selector
   the document was built for: one rule per probe, judged as [cascade prune]
   judges them. [Unmodelled] is a selector the matcher has no model for, which
   is neither answer. The rule carries a declaration because flattening drops an
   empty one before anything judges it. *)
type probes = { matched : int; unmatched : int; unmodelled : int }

let probe ~roots selectors =
  let decl = Css.color (Css.Values.hex "#000") in
  let sheet =
    Css.v (List.map (fun selector -> Css.rule ~selector [ decl ]) selectors)
  in
  List.fold_left
    (fun (p, examples) (e : Cascade.Prune.entry) ->
      match e.verdict with
      | Used _ -> ({ p with matched = p.matched + 1 }, examples)
      | Unused ->
          ( { p with unmatched = p.unmatched + 1 },
            Selector.to_string e.selector :: examples )
      | Unmodelled -> ({ p with unmodelled = p.unmodelled + 1 }, examples))
    ({ matched = 0; unmatched = 0; unmodelled = 0 }, [])
    (Prune.analyse ~sheet roots).entries
  |> fun (p, examples) -> (p, List.rev examples)

(* ===== Artefacts ===== *)

(* The page and both sheets of a pair, so the run reproduces with [cascade diff
   --browser --html page.html first.css second.css], and the report as the
   library prints it. *)
let write_artefacts ~dir ~html ~first ~second report =
  mkdir_p dir;
  write (dir // "page.html") html;
  write (dir // String.concat "" [ fst first; ".css" ]) (snd first);
  write (dir // String.concat "" [ fst second; ".css" ]) (snd second);
  write (dir // "report.txt")
    (Browser_compare.to_string ~first:(fst first) ~second:(fst second)
       ~html:"page.html" report);
  Fmt.pr "  artefacts: %s@." dir;
  Fmt.pr "  reproduce: cascade diff --browser --html %s %s %s@."
    (dir // "page.html")
    (dir // String.concat "" [ fst first; ".css" ])
    (dir // String.concat "" [ fst second; ".css" ])

(* Absolute, so the path the run prints can be opened from anywhere. *)
let artefact_root () =
  let dir =
    match Browser.getenv "CASCADE_RENDER_ARTIFACTS" with
    | Some d -> d
    | None -> "tmp" // "render-diff"
  in
  if Filename.is_relative dir then Sys.getcwd () // dir else dir

(* ===== The run ===== *)

type stats = {
  mutable sheets : int;
  mutable selectors : int;
  mutable synthesised : int;
  mutable elements : int;
  mutable probes : probes;
  mutable unmatched_shown : int;
  mutable pairs : int;
  mutable same_text : int;
  mutable differing : int;
  mutable canaries : int;
  mutable unparsed : int;
  mutable failures : int;
  skipped : (string, int) Hashtbl.t;
}

let stats () =
  {
    sheets = 0;
    selectors = 0;
    synthesised = 0;
    elements = 0;
    probes = { matched = 0; unmatched = 0; unmodelled = 0 };
    unmatched_shown = 0;
    pairs = 0;
    same_text = 0;
    differing = 0;
    canaries = 0;
    unparsed = 0;
    failures = 0;
    skipped = Hashtbl.create 16;
  }

let show_renders (report : Browser_compare.t) =
  List.iter
    (fun (r : Browser_compare.render) ->
      Fmt.pr "  %s %s: %dx%d pixels differ at (%d,%d)@." r.viewport r.state
        r.width r.height r.x r.y)
    report.renders;
  List.iteri
    (fun i (d : Browser_compare.difference) ->
      if i < 8 then
        Fmt.pr "  %s%s %s: %S -> %S@." d.element d.pseudo d.property d.first
          d.second)
    report.differences;
  let n = List.length report.differences in
  if n > 8 then Fmt.pr "  ... %d computed value(s) differ in all@." n

(* One pair through the browser, judged against what the input expects, or
   against its pin. *)
let check_pair st ~root ~id ~expect ~html first second =
  st.pairs <- st.pairs + 1;
  let expect =
    match
      List.find_opt
        (fun (input, variant, _) ->
          String.equal input id && String.equal variant (fst second))
        known
    with
    | Some (_, _, reason) -> Differs reason
    | None -> expect
  in
  match (Browser_compare.run ~html [ first; second ], expect) with
  | Error e, _ ->
      st.failures <- st.failures + 1;
      Fmt.pr "FAIL %s: %s against %s: %s@." id (fst second) (fst first) e
  | Ok report, Differs reason ->
      if Browser_compare.identical report then (
        st.failures <- st.failures + 1;
        Fmt.pr
          "FAIL %s: expected a difference (%s) and saw none; remove the pin@."
          id reason)
      else (
        st.canaries <- st.canaries + 1;
        Fmt.pr "known %s: %s: %s@." id (fst second) reason;
        write_artefacts
          ~dir:(root // String.concat "-" [ id; fst second ])
          ~html ~first ~second report)
  | Ok report, Same ->
      if not (Browser_compare.identical report) then (
        st.differing <- st.differing + 1;
        st.failures <- st.failures + 1;
        Fmt.pr "FAIL %s: %s renders differently from %s@." id (fst second)
          (fst first);
        show_renders report;
        write_artefacts
          ~dir:(root // String.concat "-" [ id; fst second ])
          ~html ~first ~second report)

(* The pairs an input renders: its first sheet against each other one, less any
   whose text is the first's or an earlier one's, which paints the same by
   construction. *)
let check_pairs st ~root ~html input sheets =
  match sheets with
  | [] -> ()
  | first :: others ->
      let seen = ref [ snd first ] in
      List.iter
        (fun (name, css) ->
          if List.mem css !seen then st.same_text <- st.same_text + 1
          else (
            seen := css :: !seen;
            check_pair st ~root ~id:input.id ~expect:input.expect ~html first
              (name, css)))
        others

let note_document st dom =
  st.sheets <- st.sheets + 1;
  st.selectors <- st.selectors + Dom_of_css.selectors dom;
  st.synthesised <- st.synthesised + Dom_of_css.synthesised dom;
  st.elements <- st.elements + Dom_of_css.elements dom;
  List.iter
    (fun (reason, n) ->
      let prev = try Hashtbl.find st.skipped reason with Not_found -> 0 in
      Hashtbl.replace st.skipped reason (prev + n))
    (Dom_of_css.skipped dom)

let note_probes st ~id (p, examples) =
  st.probes <-
    {
      matched = st.probes.matched + p.matched;
      unmatched = st.probes.unmatched + p.unmatched;
      unmodelled = st.probes.unmodelled + p.unmodelled;
    };
  List.iter
    (fun s ->
      if st.unmatched_shown < 10 then (
        st.unmatched_shown <- st.unmatched_shown + 1;
        Fmt.pr "  probe matched no element: %s (%s)@." s id))
    examples

let check_input st ~root input =
  match Css.of_string ~strict:false input.source with
  | Error _ -> st.unparsed <- st.unparsed + 1
  | Ok { stylesheet; _ } ->
      let dom = Dom_of_css.of_stylesheet stylesheet in
      let html = Dom_of_css.to_html dom in
      let roots = Html.roots (Html.parse html) in
      note_document st dom;
      note_probes st ~id:input.id (probe ~roots (Dom_of_css.probes dom));
      let sheets =
        match input.sheets with
        | Some sheets -> sheets
        | None -> variants ~roots stylesheet
      in
      check_pairs st ~root ~html input sheets

let summary st ~elapsed =
  Fmt.pr
    "render_diff: %d sheet(s), %d selector(s), %d synthesised, %d element(s), \
     %d pair(s), %.1fs@."
    st.sheets st.selectors st.synthesised st.elements st.pairs elapsed;
  Fmt.pr "  probes: %d matched, %d unmatched, %d unmodelled@." st.probes.matched
    st.probes.unmatched st.probes.unmodelled;
  Fmt.pr
    "  variants printing as the source or an earlier variant: %d, render \
     differences: %d, canaries seen: %d@."
    st.same_text st.differing st.canaries;
  if st.unparsed > 0 then Fmt.pr "  unparsed inputs: %d@." st.unparsed;
  Hashtbl.fold (fun reason n acc -> (reason, n) :: acc) st.skipped []
  |> List.sort (fun (_, a) (_, b) -> compare b a)
  |> List.iter (fun (reason, n) ->
      Fmt.pr "  not synthesised: %-42s %d@." reason n);
  (* A run that rendered no pair is not a clean run, it is a blind one: the
     sheets are read from committed corpora, so an empty population means the
     harness stopped working, not that there was nothing to check. *)
  if st.sheets = 0 || st.elements = 0 || st.pairs = 0 then (
    st.failures <- st.failures + 1;
    Fmt.pr "FAIL: no page reached the browser at all@.");
  Fmt.pr "  failures: %d@." st.failures

let skip reason = Browser.skip "render_diff" reason

let () =
  Browser.suppressed "render_diff";
  (match Browser.node_binary () with Some _ -> () | None -> skip "no node");
  (match Browser.chrome_binary () with
  | Some _ -> ()
  | None -> skip "no headless browser");
  let root = artefact_root () in
  let st = stats () in
  let started = Unix.gettimeofday () in
  List.iter (check_input st ~root) (inputs ());
  summary st ~elapsed:(Unix.gettimeofday () -. started);
  if st.failures > 0 then exit 1
