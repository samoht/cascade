(* Render a stylesheet and its optimized forms in a headless browser and check
   that every element computes the same style under each. A disagreement is an
   optimizer bug: the two sheets do not render the same page.

   Skips cleanly, with status 0, when node or a headless Chromium is missing, or
   when CASCADE_NO_BROWSER is set. *)

open Cascade

let ( // ) = Filename.concat

(* ===== Environment ===== *)

let getenv name =
  match Sys.getenv_opt name with Some "" | None -> None | Some v -> Some v

let executable path =
  (try Sys.file_exists path && not (Sys.is_directory path)
   with Sys_error _ -> false)
  &&
    try
      Unix.access path [ Unix.X_OK ];
      true
    with Unix.Unix_error _ | Sys_error _ -> false

let on_path name =
  let dirs =
    String.split_on_char ':' (Option.value ~default:"" (getenv "PATH"))
  in
  List.find_map
    (fun d ->
      if d = "" then None
      else
        let p = d // name in
        if executable p then Some p else None)
    dirs

(* The browser caches keep one directory per version, so the largest path is the
   newest build. *)
let rec search_tree root names depth acc =
  if depth <= 0 then acc
  else
    match Sys.readdir root with
    | exception Sys_error _ -> acc
    | entries ->
        Array.fold_left
          (fun acc entry ->
            let p = root // entry in
            if try Sys.is_directory p with Sys_error _ -> false then
              search_tree p names (depth - 1) acc
            else if List.mem entry names && executable p then p :: acc
            else acc)
          acc entries

let chrome_binary () =
  match getenv "CHROME" with
  | Some c when executable c -> Some c
  | Some _ | None -> (
      let names =
        [
          "chromium";
          "chromium-browser";
          "google-chrome";
          "google-chrome-stable";
        ]
      in
      match List.find_map on_path names with
      | Some c -> Some c
      | None -> (
          let home = Option.value ~default:"" (getenv "HOME") in
          let caches =
            [
              home // ".cache" // "puppeteer";
              home // "Library" // "Caches" // "ms-playwright";
              home // ".cache" // "ms-playwright";
            ]
          in
          let binaries =
            [
              "chrome-headless-shell";
              "headless_shell";
              "Chromium";
              "Google Chrome for Testing";
            ]
          in
          let found =
            List.concat_map (fun c -> search_tree c binaries 6 []) caches
          in
          match List.sort compare found with
          | [] ->
              let app =
                "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
              in
              if executable app then Some app else None
          | l -> Some (List.nth l (List.length l - 1))))

let node_binary () =
  match getenv "NODE" with
  | Some n when executable n -> Some n
  | Some _ | None -> on_path "node"

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

(* A sheet paired with a form that renders differently: [gap] resets the row gap
   the longhand set, so the two orders give the rule a different row gap. The
   canary fails when the harness reports no difference - a harness that cannot
   see a known render change proves nothing about the ones it misses. *)
let canary_sheet = {css|.k { row-gap: 9px; gap: 1px }|css}
let canary_reordered = {css|.k { gap: 1px; row-gap: 9px }|css}

type input = {
  id : string;
  source : string; (* the document is derived from this sheet *)
  sheets : (string * string) list option; (* None: the optimize variants *)
  expect_diff : bool;
}

let sheet id source = { id; source; sheets = None; expect_diff = false }

let inputs () =
  [
    sheet "smoke" smoke_sheet;
    {
      id = "canary";
      source = canary_sheet;
      sheets =
        Some [ ("original", canary_sheet); ("reordered", canary_reordered) ];
      expect_diff = true;
    };
  ]

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
    ("lossless", Css.to_string ~minify:true (Css.optimize ~lossless:true sheet));
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

let skip reason =
  print_endline ("SKIP: render_diff (" ^ reason ^ ")");
  exit 0

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
  if getenv "CASCADE_NO_BROWSER" <> None then skip "CASCADE_NO_BROWSER is set";
  let node = match node_binary () with Some n -> n | None -> skip "no node" in
  let chrome =
    match chrome_binary () with
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
  let surviving = ref 0 in
  let canaries = ref 0 in
  let unmatched_shown = ref 0 in
  List.iter
    (fun input ->
      let id = input.id in
      match Hashtbl.find_opt doms id with
      | None -> ()
      | Some dom ->
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
          if input.expect_diff then
            if r.diffs = [] then (
              incr failures;
              Fmt.pr
                "FAIL %s: the harness saw no difference between two sheets \
                 that render differently@."
                id)
            else incr canaries
          else if r.diffs <> [] then (
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
            Fmt.pr "  artefacts: %s@." dir))
    inputs;
  Fmt.pr
    "render_diff: %d sheet(s), %d selector(s), %d synthesised, %d element(s), \
     %.1fs@."
    (Hashtbl.length doms) !selectors !synthesised !elements elapsed;
  Fmt.pr "  probes: %d matched, %d unmatched, %d rejected@." !matched !unmatched
    !rejected;
  Fmt.pr
    "  computed-style candidates: %d, render differences: %d, canaries seen: \
     %d@."
    !candidates !surviving !canaries;
  if !unparsed > 0 then Fmt.pr "  unparsed inputs: %d@." !unparsed;
  Hashtbl.fold (fun reason n acc -> (reason, n) :: acc) skipped []
  |> List.sort (fun (_, a) (_, b) -> compare b a)
  |> List.iter (fun (reason, n) ->
      Fmt.pr "  not synthesised: %-42s %d@." reason n);
  Fmt.pr "  failures: %d@." !failures;
  if !failures > 0 then exit 1
