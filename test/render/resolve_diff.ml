(* Resolve a stylesheet against a document twice and check that the two agree:
   once by the browser, which cascades the sheet itself, and once by
   {!Cascade.Resolve}, whose answer is written into each element's style
   attribute and computed by the same browser. A disagreement is a declaration
   one of them let win and the other did not.

   The browser is the oracle for both halves: it decides which declaration wins
   in the first leg and what a winning declaration computes to in the second, so
   nothing here derives an expectation from the library under test.

   Skips cleanly, with status 0, when node or a headless Chromium is missing, or
   when CASCADE_NO_BROWSER is set. *)

open Cascade

let ( // ) = Filename.concat

module R = Resolve.Make (Resolve_gen.Node)

(* ===== Files ===== *)

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
  projected : Css.t;  (** what {!Cascade.Resolve} is asked about *)
}

let parse css =
  match Css.of_string ~strict:false css with
  | Ok { stylesheet; _ } -> stylesheet
  | Error _ ->
      prerr_endline (String.concat "" [ "resolve_diff: unparsed sheet: "; css ]);
      exit 1

(* ===== The documents the hand-written jobs run against ===== *)

let probe_doc =
  Resolve_gen.(
    doc
      [
        elt ~id:"kid" ~classes:[ "k"; "a"; "card" ] "div"
          [ elt ~classes:[ "k"; "b" ] "p" [] ];
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

(* ===== Projection =====

   The declarations {!Cascade.Resolve} says win, in the order it returns them:
   weakest first, so a shorthand and a longhand of one family land in the style
   attribute in the order the cascade ranked them and the browser resolves their
   overlap the way it would have in the sheet. *)
let style_of prepared node =
  R.resolve_prepared prepared node
  |> List.map (Declaration.to_string ~minify:true)
  |> String.concat ";"

let job_json job =
  let prepared = Resolve.prepare job.projected in
  let subjects = Resolve_gen.subjects job.doc in
  let fields =
    match Resolve_gen.json_of_doc job.doc with
    | Json.Obj fields -> fields
    | other -> [ ("dom", other) ]
  in
  Json.Obj
    ((("id", Json.Str job.id) :: fields)
    @ [
        ("css", Json.Str (Css.to_string ~minify:true job.rendered));
        ( "tags",
          Json.Arr (List.map (fun e -> Json.Str (Resolve_gen.tag e)) subjects)
        );
        ( "styles",
          Json.Arr (List.map (fun e -> Json.Str (style_of prepared e)) subjects)
        );
      ])

(* ===== Driver protocol ===== *)

type diff = {
  index : int;
  property : string;
  browser : string;
  cascade : string;
}

type result = {
  diffs : diff list;
  reported : int;
  elements : int;
  properties : int;
  styled : int;
  errors : string list;
}

let empty_result =
  {
    diffs = [];
    reported = 0;
    elements = 0;
    properties = 0;
    styled = 0;
    errors = [];
  }

let int_of s = try int_of_string (String.trim s) with Failure _ -> 0
let user_agent = ref "unknown"

let parse_driver_output lines =
  let table = Hashtbl.create 16 in
  let get id = try Hashtbl.find table id with Not_found -> empty_result in
  List.iter
    (fun line ->
      match String.split_on_char '\t' line with
      | [ "d"; id; index; property; browser; cascade ] ->
          let r = get id in
          Hashtbl.replace table id
            {
              r with
              diffs =
                r.diffs
                @ [ { index = int_of index; property; browser; cascade } ];
            }
      | [ "n"; id; elements; properties; styled; diffs ] ->
          let r = get id in
          Hashtbl.replace table id
            {
              r with
              elements = int_of elements;
              properties = int_of properties;
              styled = int_of styled;
              reported = int_of diffs;
            }
      | [ "v"; ua ] -> user_agent := ua
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
  | _ ->
      let ic = open_in errors in
      let text = read_lines ic in
      close_in ic;
      Error (String.concat "\n" text)

(* ===== Artefacts ===== *)

(* Two pages that rebuild the run: one the browser cascades, one carrying
   cascade's answer, so a failure opens in a browser as the pair it was. *)
let repro ~css ~dom ~styles =
  String.concat ""
    [
      "<!doctype html><html><head><meta charset=\"utf-8\"><style>";
      css;
      "</style></head><body><script id=\"cd-doc\" type=\"application/json\">";
      dom;
      "</script><script id=\"cd-styles\" type=\"application/json\">";
      styles;
      "</script><script src=\"dom.js\"></script><script>rdBuild(document, \
       JSON.parse(document.getElementById('cd-doc').textContent));var \
       s=JSON.parse(document.getElementById('cd-styles').textContent);if(s.length){var \
       e=document.body.querySelectorAll('*');for(var \
       i=0;i<e.length;i++)e[i].setAttribute('style',s[i]);}</script></body></html>";
    ]

let write_artefacts ~dir ~script_dir ~job ~diffs =
  mkdir_p dir;
  let dom = Json.to_string (Resolve_gen.json_of_doc job.doc) in
  let prepared = Resolve.prepare job.projected in
  let subjects = Resolve_gen.subjects job.doc in
  let styles = List.map (fun e -> style_of prepared e) subjects in
  let css = Css.to_string ~minify:true job.rendered in
  write (dir // "dom.json") dom;
  write (dir // "dom.js") (read_file (script_dir // "dom.js"));
  write (dir // "sheet.css") (Css.to_string job.rendered);
  if not (String.equal css (Css.to_string ~minify:true job.projected)) then
    write (dir // "projected.css") (Css.to_string job.projected);
  write (dir // "page-cascaded.html") (repro ~css ~dom ~styles:"[]");
  write
    (dir // "page-projected.html")
    (repro ~css:"" ~dom
       ~styles:
         (Json.to_string (Json.Arr (List.map (fun s -> Json.Str s) styles))));
  let buf = Buffer.create 4096 in
  List.iteri
    (fun i (e, s) ->
      Buffer.add_string buf (string_of_int i);
      Buffer.add_char buf '\t';
      Buffer.add_string buf (Resolve_gen.label job.doc e);
      Buffer.add_char buf '\t';
      Buffer.add_string buf s;
      Buffer.add_char buf '\n')
    (List.combine subjects styles);
  write (dir // "styles.tsv") (Buffer.contents buf);
  let buf = Buffer.create 4096 in
  List.iter
    (fun d ->
      Buffer.add_string buf
        (String.concat "\t"
           [ string_of_int d.index; d.property; d.browser; d.cascade ]);
      Buffer.add_char buf '\n')
    diffs;
  write (dir // "diff.tsv") (Buffer.contents buf)

(* ===== Main ===== *)

let artefact_root () =
  let dir =
    match Browser.getenv "CASCADE_RENDER_ARTIFACTS" with
    | Some d -> d // "resolve"
    | None -> "tmp" // "resolve-diff"
  in
  if Filename.is_relative dir then Sys.getcwd () // dir else dir

let skip reason = Browser.skip "resolve_diff" reason
let is_control job = match job.expect with Differs _ -> true | Same -> false

let () =
  let seeds = ref 48 in
  let only = ref None in
  let sheet_files = ref [] in
  let sheet_doc = ref 1 in
  let args =
    [
      ("--seeds", Arg.Set_int seeds, "N generated sheets (default 48)");
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
  if Option.is_some (Browser.getenv "CASCADE_NO_BROWSER") then
    skip "CASCADE_NO_BROWSER is set";
  let node =
    match Browser.node_binary () with Some n -> n | None -> skip "no node"
  in
  let chrome =
    match Browser.chrome_binary () with
    | Some c -> c
    | None -> skip "no headless browser"
  in
  let jobs =
    match List.rev !sheet_files with
    | [] ->
        calibration_jobs @ probe_jobs
        @ generated (List.init !seeds (fun i -> i + 1))
    | files ->
        List.map
          (fun file ->
            let sheet = parse (read_file file) in
            {
              id = Filename.remove_extension (Filename.basename file);
              expect = Same;
              doc = Resolve_gen.document ~seed:!sheet_doc;
              rendered = sheet;
              projected = sheet;
            })
          files
  in
  let jobs =
    match !only with
    | None -> jobs
    | Some needle -> List.filter (fun j -> contains j.id needle) jobs
  in
  let script_dir = Filename.dirname Sys.executable_name in
  let root = artefact_root () in
  let work = root // ".work" in
  mkdir_p work;
  let jobs_file = work // "jobs.json" in
  write jobs_file (Json.to_string (Json.Arr (List.map job_json jobs)));
  let started = Unix.gettimeofday () in
  let lines =
    match
      run_driver ~node ~chrome
        ~script:(script_dir // "resolve_diff.js")
        ~jobs:jobs_file ~work
    with
    | Ok lines -> lines
    | Error err ->
        prerr_endline
          (String.concat "\n"
             [ "resolve_diff: the browser driver failed:"; err ]);
        exit 1
  in
  let elapsed = Unix.gettimeofday () -. started in
  let table = parse_driver_output lines in
  let failures = ref 0 in
  let elements = ref 0 in
  let styled = ref 0 in
  let comparisons = ref 0 in
  let properties = ref 0 in
  let differences = ref 0 in
  let controls = ref 0 in
  List.iter
    (fun job ->
      let r = try Hashtbl.find table job.id with Not_found -> empty_result in
      elements := !elements + r.elements;
      styled := !styled + r.styled;
      properties := max !properties r.properties;
      comparisons := !comparisons + (r.elements * r.properties);
      List.iter
        (fun e ->
          incr failures;
          Fmt.pr "FAIL %s: %s@." job.id e)
        r.errors;
      let subjects = Resolve_gen.subjects job.doc in
      let report () =
        differences := !differences + r.reported;
        let dir = root // job.id in
        write_artefacts ~dir ~script_dir ~job ~diffs:r.diffs;
        Fmt.pr "FAIL %s: %d computed-style difference(s)@." job.id r.reported;
        Fmt.pr "  sheet: %s@." (Css.to_string ~minify:true job.rendered);
        List.iteri
          (fun i d ->
            if i < 8 then
              let label =
                match List.nth_opt subjects d.index with
                | Some e -> Resolve_gen.label job.doc e
                | None -> string_of_int d.index
              in
              Fmt.pr "  %s %s: browser %S, cascade %S@." label d.property
                d.browser d.cascade)
          r.diffs;
        Fmt.pr "  artefacts: %s@." dir
      in
      match job.expect with
      | Same ->
          if r.reported > 0 then (
            incr failures;
            report ())
      | Differs why ->
          if r.reported = 0 then (
            incr failures;
            Fmt.pr
              "FAIL %s: the perturbed projection (%s) computed the same style, \
               so the comparison is blind@."
              job.id why)
          else incr controls)
    jobs;
  Fmt.pr
    "resolve_diff: %d job(s), %d element(s), %d styled, %d properties, %d \
     comparison(s), %.1fs@."
    (List.length jobs) !elements !styled !properties !comparisons elapsed;
  Fmt.pr "  browser: %s@." !user_agent;
  Fmt.pr "  controls that reported a planted wrong winner: %d of %d@." !controls
    (List.length (List.filter is_control jobs));
  Fmt.pr "  differences: %d@." !differences;
  (* An empty population and a clean run look the same from outside, so the run
     says which it was. *)
  let thin =
    !elements < 300 || !styled < 150 || !properties < 100
    || !comparisons < 100_000
  in
  if List.length !sheet_files = 0 && Option.is_none !only && thin then (
    incr failures;
    Fmt.pr
      "FAIL the population is too thin to have tested anything: %d element(s), \
       %d styled, %d properties, %d comparison(s)@."
      !elements !styled !properties !comparisons);
  Fmt.pr "  failures: %d@." !failures;
  if !failures > 0 then exit 1
