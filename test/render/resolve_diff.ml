(* Resolve a stylesheet against a document twice and check that the two paint
   the same page: once by the browser, which cascades the sheet itself, and once
   by {!Cascade.Apply}, whose answer - the declarations {!Cascade.Resolve} says
   win for each element - is written into the element's style attribute and
   rendered under no sheet, as a page [cascade apply] wrote would be. A render
   that differs is a declaration one of them let win and the other did not.

   The oracle is [Browser_compare.run] and nothing else: it renders one document
   under two sheets, so the page carries the tree twice, once plain and once
   with the projected style attributes, and each sheet hides the other's copy.
   The sheet side is the plain tree under the stylesheet; the applied side is
   the styled tree under what the projection kept for want of an inline form,
   which for a generated sheet is the layer statements that order both sides and
   for the calibration the one [:empty] rule the default reading declines.
   [Browser_compare.identical] is the verdict, and nothing here derives an
   expectation from the library under test.

   The projection is asked under the default [Browser] reading, as [cascade
   apply] asks it. That reading declines [:empty] and the [s] attribute flag,
   and a declined rule keeps every rule writing the same slots out of the
   projection with it, so the generator writes neither: every element holds a
   letter, and no generated element would be [:empty] to any reading. The one
   [:empty] element is in the calibration document, where it holds no children
   of any kind and is [:empty] to Level 2, to Level 4 and to Chrome alike.

   A pair costs a browser launch, a second or two, so the default run is the
   calibration, the probes and 32 seeds, about seventy pairs in two minutes;
   [--seeds N] widens it and [--only SUBSTRING] narrows it.

   Skips cleanly, with status 0, when node or a headless Chromium is missing.
   CASCADE_NO_BROWSER fails instead: see [Browser.suppressed]. *)

open Cascade
module Html = Cascade_html.Html
module Apply = Cascade.Apply.Make (Cascade_html.Html_node)

let ( // ) = Filename.concat

(* ===== Files ===== *)

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then (
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let write file contents =
  Out_channel.with_open_bin file (fun oc -> output_string oc contents)

let read_file file = In_channel.with_open_bin file In_channel.input_all

let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec at i =
    i + n <= h && (String.sub haystack i n = needle || at (i + 1))
  in
  n = 0 || at 0

(* ===== Jobs ===== *)

(* [Differs] is a control, not a pin: the projection is fed a perturbed sheet
   whose cascade lands somewhere else, so the browser must contradict it. A
   control that reports nothing says the comparison is blind, and a run that
   cannot see a wrong winner it planted proves nothing about the ones it did
   not. *)
type expect = Same | Differs of string

type job = {
  id : string;
  expect : expect;
  doc : Resolve_gen.doc;
  rendered : Css.t;  (** what the browser cascades *)
  projected : Css.t;  (** what {!Cascade.Apply} is asked about *)
}

let parse css =
  match Css.of_string ~strict:false css with
  | Ok { stylesheet; _ } -> stylesheet
  | Error _ ->
      prerr_endline (String.concat "" [ "resolve_diff: unparsed sheet: "; css ]);
      exit 1

(* ===== The document the hand-written jobs run against =====

   The [p] holds no children of any kind, so it is [:empty]; its inline border
   paints in its colour, so a colour resolved onto it still shows. *)

let probe_doc =
  Resolve_gen.(
    doc
      [
        elt ~id:"kid" ~classes:[ "k"; "a"; "card" ] "div"
          [
            elt ~classes:[ "k"; "b" ] ~text:false
              ~attrs:[ ("style", "border-bottom:2px solid") ]
              "p" [];
          ];
      ])

(* ===== Calibration =====

   Each pair plants one cascade answer twice: once as the library gets it, which
   must agree with the browser, and once with the projection resolving a sheet
   perturbed so that a different declaration wins, which the browser must
   contradict. The perturbation is in the harness's own input, never in the
   library. *)
let calibration =
  [
    ( "order",
      "p.k{color:#f00}p.k{color:#00f}",
      "p.k{color:#00f}p.k{color:#f00}",
      "the later of two equal rules wins" );
    ( "specificity",
      ".k{color:#f00}#kid{color:#00f}",
      ".k{color:#f00}:where(#kid){color:#00f}",
      ":where() adds no specificity, so the class would win" );
    ( "important",
      ".k{color:#f00!important}#kid{color:#00f}",
      ".k{color:#f00}#kid{color:#00f}",
      "without the flag the more specific rule wins" );
    ( "layer-order",
      "@layer a{.k{color:#f00}}@layer b{.k{color:#00f}}",
      "@layer b{.k{color:#00f}}@layer a{.k{color:#f00}}",
      "the last layer wins, so swapping them changes the winner" );
    ( "layer-important",
      "@layer a{.k{color:#f00!important}}@layer b{.k{color:#00f!important}}",
      "@layer b{.k{color:#00f!important}}@layer a{.k{color:#f00!important}}",
      "important reverses the layer order, so swapping them changes the winner"
    );
    (* A pair over the one [:empty] element: the pseudo-class outranks the
       class, and the browser still contradicts a wrong winner planted behind
       it, so a rule carrying one is compared and not merely counted. *)
    ( "empty",
      ".k{color:#f00}p:empty{color:#00f}",
      ".k{color:#f00}:where(p:empty){color:#00f}",
      ":where() adds no specificity, so the class would win" );
    (* The winning set holds a shorthand and a longhand at once, so the control
       has to move their order rather than replace either. *)
    ( "shorthand-order",
      "#kid{margin:4px}.k{margin-top:20px}",
      ":where(#kid){margin:4px}.k{margin-top:20px}",
      "with the id's specificity gone the longhand outranks the shorthand" );
  ]

let calibration_jobs =
  List.concat_map
    (fun (name, css, mutated, why) ->
      let sheet = parse css in
      [
        {
          id = String.concat "" [ "calib-"; name ];
          expect = Same;
          doc = probe_doc;
          rendered = sheet;
          projected = sheet;
        };
        {
          id = String.concat "" [ "calib-"; name; "-perturbed" ];
          expect = Differs why;
          doc = probe_doc;
          rendered = sheet;
          projected = parse mutated;
        };
      ])
    calibration

(* ===== Probes =====

   One mechanism each, written small enough that a failure names the rule it
   broke. The browser answers every one of them. *)
let probes =
  [
    (* css-cascade-5 sec. 6.4.4: among normal declarations an unlayered one
       beats every layered one, whichever order they are written in. *)
    ("unlayered-wins", "@layer a{.k{color:#f00}}.k{color:#00f}");
    ("unlayered-wins-first", ".k{color:#00f}@layer a{.k{color:#f00}}");
    (* And among important ones that reverses: the layered declaration wins. *)
    ( "unlayered-loses-important",
      "@layer a{.k{color:#f00!important}}.k{color:#00f!important}" );
    ( "important-beats-normal-across-layers",
      "@layer a{.k{color:#f00!important}}.k{color:#00f}" );
    (* css-cascade-5 (ED) sec. 6.4.3: "Cascade layers are sorted by the order in
       which they first are declared, with nested layers grouped within their
       parent layer. Unlayered rules are sorted later than any layered rules
       within the same parent layer": a layer's own rules sit in an implicit
       sublayer after its explicit ones, so [a] beats [a.b] and, being first,
       loses to it among important declarations. *)
    ( "sublayer-before-parent",
      "@layer a.b{.k{color:#f00}}@layer a{.k{color:#00f}}" );
    ( "sublayer-before-parent-important",
      "@layer a.b{.k{color:#f00!important}}@layer a{.k{color:#00f!important}}"
    );
    ("sublayer-in-a-block", "@layer a{@layer b{.k{color:#f00}}.k{color:#00f}}");
    ( "parent-before-sublayer",
      "@layer a{.k{color:#f00}}@layer a.b{.k{color:#00f}}" );
    ( "sublayer-inside-the-subtree",
      "@layer a.b{.k{color:#00f}}@layer c{.k{color:#f00}}@layer \
       a.d{.k{color:#0f0}}" );
    (* sec. 6.4.1: a layer statement declares the order before any block
       does. *)
    ( "statement-fixes-the-order",
      "@layer util,base;@layer base{.k{color:#f00}}@layer util{.k{color:#00f}}"
    );
    ( "statement-order-important",
      "@layer util,base;@layer base{.k{color:#f00!important}}@layer \
       util{.k{color:#00f!important}}" );
    (* Each anonymous block is a layer of its own, ordered where it is
       written. *)
    ("anonymous-layers", "@layer{.k{color:#f00}}@layer{.k{color:#00f}}");
    ( "anonymous-layers-important",
      "@layer{.k{color:#f00!important}}@layer{.k{color:#00f!important}}" );
    (* selectors-4 sec. 17: [:is()] and [:not()] take their most specific
       argument, [:where()] takes none. *)
    ("is-takes-its-argument", ".k{color:#f00}:is(#nope,.k){color:#00f}");
    ("where-takes-nothing", ".k{color:#00f}:where(#kid){color:#f00}");
    ("not-takes-its-argument", "p{color:#f00}:not(.zzz){color:#00f}");
    (* An attribute selector is a class-level component. *)
    ("attribute-specificity", "div{color:#f00}[id]{color:#00f}");
    (* CSS Nesting 1 sec. 4: [&] carries the specificity of [:is(<parent
       selector list>)], which is its most specific branch, not the branch that
       matched. *)
    ( "nesting-parent-list-specificity",
      ".a,#lead{& .b{color:#00f}}.card .b{color:#f00}" );
    ("nesting-implicit-descendant", ".card{color:#f00;.b{color:#00f}}");
    ("nesting-compound", "p{color:#f00}.card{&.a{color:#00f}}");
    (* A shorthand resets every longhand it covers, so which one wins the slot
       depends on where each sits in the cascade, not on which is longer. *)
    ("shorthand-loses-a-slot", "#kid{margin-top:20px}.k{margin:4px}");
    ("shorthand-wins-a-slot", "#kid{margin:4px}.k{margin-top:20px}");
    ( "shorthand-loses-to-important",
      "#kid{margin:4px}.k{margin-top:20px!important}" );
    ("shorthand-across-layers", "@layer a{.k{margin-top:20px}}.k{margin:4px}");
    (* A selector list cascades as one rule per branch, each with its own
       specificity. *)
    ("selector-list-branch-specificity", "#kid,.zzz{color:#f00}p.k{color:#00f}");
  ]

let probe_jobs =
  List.map
    (fun (name, css) ->
      let sheet = parse css in
      {
        id = String.concat "" [ "probe-"; name ];
        expect = Same;
        doc = probe_doc;
        rendered = sheet;
        projected = sheet;
      })
    probes

(* ===== Generated jobs ===== *)

let generated seeds =
  List.map
    (fun seed ->
      let sheet = Resolve_gen.stylesheet ~seed in
      {
        id = String.concat "" [ "seed-"; string_of_int seed ];
        expect = Same;
        doc = Resolve_gen.document ~seed;
        rendered = sheet;
        projected = sheet;
      })
    seeds

(* ===== The page =====

   The tree twice under [body], each copy in a wrapper no generated selector
   names, so a rule matches the same elements in both and a sibling combinator
   never crosses from one copy to the other. Each side's sheet hides the other
   copy, so a capture shows one tree at the top of the page. *)

let sheet_wrapper = "rd-sheet"
let applied_wrapper = "rd-applied"

let page tree =
  String.concat ""
    [
      "<!DOCTYPE html>\n\
       <html><head><meta charset=\"utf-8\"></head><body><main id=\"";
      sheet_wrapper;
      "\">";
      tree;
      "</main><main id=\"";
      applied_wrapper;
      "\">";
      tree;
      "</main></body></html>\n";
    ]

let hide id = String.concat "" [ "#"; id; "{display:none}" ]

let rec under id (e : Html.element) =
  match Html.attribute e "id" with
  | Some i when String.equal i id -> true
  | Some _ | None -> (
      match e.parent with Some p -> under id p | None -> false)

(* The projection as [cascade apply] writes it: the page parsed, every element
   resolved, and the winning declarations set on the elements of the applied
   copy in the order the cascade ranked them, weakest first, so a shorthand and
   a longhand of one family land in the style attribute in that order and the
   browser resolves their overlap the way it would have in the sheet. *)
let project job =
  let doc = Html.parse (page (Resolve_gen.html_of_doc job.doc)) in
  let { Cascade.Apply.styles; keep_css; kept } =
    Apply.compute ~sheet:job.projected (Html.roots doc)
  in
  let styled = ref 0 in
  List.iter
    (fun (node, decls) ->
      match decls with
      | [] -> ()
      | decls when under applied_wrapper node ->
          incr styled;
          Html.set_attribute node "style"
            (Css.inline_style_of_declarations ~minify:true ~mode:Variables decls)
      | _ -> ())
    styles;
  (Html.to_string doc, keep_css, kept, !styled)

(* ===== Artefacts ===== *)

let write_artefacts ~dir ~html ~sheet ~applied report =
  mkdir_p dir;
  write (dir // "page.html") html;
  write (dir // "sheet.css") sheet;
  write (dir // "applied.css") applied;
  write (dir // "report.txt")
    (Browser_compare.to_string ~first:"sheet.css" ~second:"applied.css"
       ~html:"page.html" report);
  Fmt.pr "  artefacts: %s@." dir;
  Fmt.pr "  reproduce: cascade diff --browser --html %s %s %s@."
    (dir // "page.html") (dir // "sheet.css") (dir // "applied.css")

let artefact_root () =
  let dir =
    match Browser.getenv "CASCADE_RENDER_ARTIFACTS" with
    | Some d -> d // "resolve"
    | None -> "tmp" // "resolve-diff"
  in
  if Filename.is_relative dir then Sys.getcwd () // dir else dir

(* ===== The run ===== *)

type stats = {
  mutable pairs : int;
  mutable styled : int;
  mutable kept : int;
  mutable controls : int;
  mutable differences : int;
  mutable failures : int;
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
        Fmt.pr "  %s%s %s: browser %S, cascade %S@." d.element d.pseudo
          d.property d.first d.second)
    report.differences;
  let n = List.length report.differences in
  if n > 8 then Fmt.pr "  ... %d computed value(s) differ in all@." n

let check_job st ~root job =
  let html, keep_css, kept, styled = project job in
  st.pairs <- st.pairs + 1;
  st.styled <- st.styled + styled;
  st.kept <- st.kept + kept;
  let sheet =
    String.concat ""
      [ Css.to_string ~minify:true job.rendered; hide applied_wrapper ]
  in
  let applied = String.concat "" [ keep_css; hide sheet_wrapper ] in
  match
    ( Browser_compare.run ~html [ ("sheet", sheet); ("applied", applied) ],
      job.expect )
  with
  | Error e, _ ->
      st.failures <- st.failures + 1;
      Fmt.pr "FAIL %s: %s@." job.id e
  | Ok report, Same ->
      if not (Browser_compare.identical report) then (
        st.failures <- st.failures + 1;
        st.differences <- st.differences + 1;
        Fmt.pr "FAIL %s: the projection renders differently from the sheet@."
          job.id;
        Fmt.pr "  sheet: %s@." (Css.to_string ~minify:true job.rendered);
        show_renders report;
        write_artefacts ~dir:(root // job.id) ~html ~sheet ~applied report)
  | Ok report, Differs why ->
      if Browser_compare.identical report then (
        st.failures <- st.failures + 1;
        Fmt.pr
          "FAIL %s: the perturbed projection (%s) renders the same page, so \
           the comparison is blind@."
          job.id why)
      else st.controls <- st.controls + 1

let is_control job = match job.expect with Differs _ -> true | Same -> false

let summary st ~jobs ~elapsed =
  Fmt.pr
    "resolve_diff: %d job(s), %d pair(s), %d element(s) styled, %d rule(s) \
     kept out of the projection, %.1fs@."
    (List.length jobs) st.pairs st.styled st.kept elapsed;
  Fmt.pr "  controls that reported a planted wrong winner: %d of %d@."
    st.controls
    (List.length (List.filter is_control jobs));
  Fmt.pr "  differences: %d@." st.differences;
  Fmt.pr "  failures: %d@." st.failures

let skip reason = Browser.skip "resolve_diff" reason

let jobs ~seeds ~only ~sheet_files ~sheet_doc =
  let jobs =
    match sheet_files with
    | [] ->
        calibration_jobs @ probe_jobs
        @ generated (List.init seeds (fun i -> i + 1))
    | files ->
        List.map
          (fun file ->
            let sheet = parse (read_file file) in
            {
              id = Filename.remove_extension (Filename.basename file);
              expect = Same;
              doc = Resolve_gen.document ~seed:sheet_doc;
              rendered = sheet;
              projected = sheet;
            })
          files
  in
  match only with
  | None -> jobs
  | Some needle -> List.filter (fun j -> contains j.id needle) jobs

let () =
  let seeds = ref 32 in
  let only = ref None in
  let sheet_files = ref [] in
  let sheet_doc = ref 1 in
  let args =
    [
      ("--seeds", Arg.Set_int seeds, "N generated sheets (default 32)");
      ( "--only",
        Arg.String (fun s -> only := Some s),
        "SUBSTRING run the jobs whose id holds SUBSTRING" );
      ( "--sheet",
        Arg.String (fun f -> sheet_files := f :: !sheet_files),
        "FILE run a job over the CSS in FILE, repeatable" );
      ( "--doc",
        Arg.Set_int sheet_doc,
        "SEED the document --sheet runs against (default 1)" );
    ]
  in
  Arg.parse args
    (fun a -> raise (Arg.Bad (String.concat "" [ "unexpected argument "; a ])))
    "resolve_diff [--seeds N] [--only SUBSTRING] [--sheet FILE [--doc SEED]]";
  Browser.suppressed "resolve_diff";
  (match Browser.node_binary () with Some _ -> () | None -> skip "no node");
  (match Browser.chrome_binary () with
  | Some _ -> ()
  | None -> skip "no headless browser");
  let jobs =
    jobs ~seeds:!seeds ~only:!only ~sheet_files:(List.rev !sheet_files)
      ~sheet_doc:!sheet_doc
  in
  let root = artefact_root () in
  let st =
    {
      pairs = 0;
      styled = 0;
      kept = 0;
      controls = 0;
      differences = 0;
      failures = 0;
    }
  in
  let started = Unix.gettimeofday () in
  List.iter (check_job st ~root) jobs;
  (* An empty population and a clean run look the same from outside, so the run
     says which it was. *)
  if
    !sheet_files = [] && Option.is_none !only
    && (st.pairs < 40 || st.styled < 150)
  then (
    st.failures <- st.failures + 1;
    Fmt.pr
      "FAIL the population is too thin to have tested anything: %d pair(s), %d \
       element(s) styled@."
      st.pairs st.styled);
  summary st ~jobs ~elapsed:(Unix.gettimeofday () -. started);
  if st.failures > 0 then exit 1
