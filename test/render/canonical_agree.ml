(* When [--diff=canonical] reports no difference, a browser must paint the same
   page under both sheets. The README states that as the guarantee - "canonical
   reports a difference only when some element would compute a different value"
   - and nothing checked it. [render_diff] asks whether a transform changed a
   render, and [shorthand_expand] asks the question of one shorthand against its
   own expansion; neither asks it of the verdict.

   The oracle is [Browser_compare.run], never cascade. Every pair the comparator
   calls identical is rendered over a document derived from its selectors, at
   every viewport and state the pair can use, and [Browser_compare.identical] is
   the verdict: a pair that paints differently is a conflation, the comparator
   equated two sheets that are not the same stylesheet. Nothing here reads a
   value back or decides that two spellings are one; the browser paints them
   alike or it does not.

   The comparator's verdict is used once, to choose the pairs: a pair it calls
   different costs no browser time, because an over-report is safe and the
   README documents several. That is the only place the harness leans on the
   code it tests, and it can only hide a pair by keeping it apart, never by
   passing one.

   The pairs are mechanical, not chosen. Each base sheet is paired with what the
   tool emits for it, and with mutants of itself: one declaration dropped, two
   adjacent declarations swapped, one rule dropped, two adjacent rules swapped,
   one rule split into two rules sharing its selector. Whether a mutant changes
   the render is not decided here.

   A pair costs a browser launch, a second or two, so the default run renders
   the longhand runs, the two examples and eight corpus sheets, with one split
   point and a few mutants per sheet: some 130 pairs in about two minutes.
   [--splits 0], [--decls 0] and [--rules 0] take every split point and every
   mutant, [--corpus 0] every corpus sheet, and [--only SUBSTRING] narrows the
   base sheets; the whole of it is an hour or more.

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

(* [against] is the text the comparator is asked about, and the text the browser
   renders the variant against: the verdict is about the pair a caller would
   actually run [diff] on. [required] marks the variants rendered whatever the
   verdict says, which is how the split's own claim is checked. *)
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
        required = false;
      };
      {
        label = "lossless";
        css = lossless_minified sheet;
        against = source;
        against_label = "source";
        kind = Tool;
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
            required = true;
          };
          {
            label = String.concat "" [ name; "-minify" ];
            css = minified s;
            against = text;
            against_label = name;
            kind = Split;
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

(* ===== Inputs ===== *)

let corpus = ref None
let runs = ref true
let decls_budget = ref 4
let rules_budget = ref 2

(* One cut point per sheet: the split is where the shapes the corpus does not
   write come from, and every cut point is a browser launch each. *)
let splits_budget = ref 1
let only = ref None

let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec at i =
    i + n <= h && (String.equal (String.sub haystack i n) needle || at (i + 1))
  in
  n = 0 || at 0

let usage () =
  Fmt.pr
    "usage: canonical_agree [--corpus N] [--no-runs] [--decls N] [--rules N] \
     [--splits N] [--only SUBSTRING]@.";
  Fmt.pr
    "a budget of 0 takes every one; the defaults are 8 corpus sheets, 4 \
     declarations, 2 rules and 1 split@.";
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
          decls_budget := Option.value ~default:4 (int_of_string_opt (arg ()))
      | "--rules" ->
          rules_budget := Option.value ~default:2 (int_of_string_opt (arg ()))
      | "--splits" ->
          splits_budget := Option.value ~default:1 (int_of_string_opt (arg ()))
      | "--only" -> only := Some (arg ())
      | "-h" | "--help" -> usage ()
      | _ -> ());
      loop (i + 1))
  in
  loop 1

let base_sheets () =
  let corpus_n = Option.value ~default:8 !corpus in
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
  html : string;
  rendered : variant list;
  over_reports : int;
  unreadable : int;
}

let build_job (id, text) =
  match Css.of_string ~strict:false text with
  | Error _ -> None
  | Ok { stylesheet; _ } ->
      let splits = sample !splits_budget (split_points stylesheet) in
      let tool, split_variants = variants ~splits stylesheet in
      let candidates =
        tool @ split_variants
        @ mutants ~decls:!decls_budget ~rules:!rules_budget stylesheet
      in
      let over = ref 0 and bad = ref 0 in
      (* A variant printing as its reference, or as one already rendered against
         the same reference, paints the same by construction. *)
      let seen = ref [] in
      let rendered =
        List.filter
          (fun v ->
            if String.equal v.css v.against || List.mem (v.against, v.css) !seen
            then false
            else (
              seen := (v.against, v.css) :: !seen;
              match verdict v.against v.css with
              | Identical -> true
              | Unreadable ->
                  incr bad;
                  v.required
              | Different ->
                  incr over;
                  v.required))
          candidates
      in
      let html = Dom_of_css.to_html (Dom_of_css.of_stylesheet stylesheet) in
      Some { id; html; rendered; over_reports = !over; unreadable = !bad }

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

type stats = {
  mutable pairs : int;
  mutable tool_pairs : int;
  mutable split_pairs : int;
  mutable mutant_pairs : int;
  mutable conflations : int;
  mutable split_breaks : int;
  mutable over : int;
  mutable unreadable : int;
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
      if i < 6 then
        Fmt.pr "  %s%s %s: %S -> %S@." d.element d.pseudo d.property d.first
          d.second)
    report.differences;
  let n = List.length report.differences in
  if n > 6 then Fmt.pr "  ... %d computed value(s) differ in all@." n

(* Every pair that paints differently is reported, under the pair that carries
   it, so one run is the whole list and nobody has to fix one to see the
   next. *)
let check_pair st job v =
  st.pairs <- st.pairs + 1;
  (match v.kind with
  | Tool -> st.tool_pairs <- st.tool_pairs + 1
  | Split -> st.split_pairs <- st.split_pairs + 1
  | Mutant -> st.mutant_pairs <- st.mutant_pairs + 1);
  match
    Browser_compare.run ~html:job.html
      [ (v.against_label, v.against); (v.label, v.css) ]
  with
  | Error e ->
      st.failures <- st.failures + 1;
      Fmt.pr "FAIL %s: %s against %s: %s@." job.id v.label v.against_label e
  | Ok report when Browser_compare.identical report -> ()
  | Ok report ->
      st.failures <- st.failures + 1;
      if v.required then (
        st.split_breaks <- st.split_breaks + 1;
        Fmt.pr
          "FAIL %s: %s does not render like the rule it was split from; this \
           harness is wrong, not the library@."
          job.id v.label)
      else (
        st.conflations <- st.conflations + 1;
        Fmt.pr "FAIL %s: canonical equates %s with %s@." job.id v.against_label
          v.label);
      Fmt.pr "  %s: %s@." v.against_label (one_line v.against);
      Fmt.pr "  %s: %s@." v.label (one_line v.css);
      show_renders report

let summary st ~jobs ~elapsed =
  Fmt.pr "canonical_agree: %d sheet(s), %d equated pair(s), %.1fs@." jobs
    st.pairs elapsed;
  Fmt.pr "  equated: %d tool, %d split, %d mutant@." st.tool_pairs
    st.split_pairs st.mutant_pairs;
  Fmt.pr "  conflations: %d, broken splits: %d@." st.conflations st.split_breaks;
  Fmt.pr "  pairs the comparator kept apart: %d, unreadable: %d@." st.over
    st.unreadable;
  (* A run that renders no equated pair is not a clean run, it is a blind one:
     the comparator equates the tool's own output with its input on any sheet
     that parses, so an empty population means the harness stopped working. *)
  if st.pairs = 0 then (
    st.failures <- st.failures + 1;
    Fmt.pr "FAIL: no pair reached the browser at all@.");
  Fmt.pr "  failures: %d@." st.failures

let () =
  parse_args ();
  Browser.suppressed "canonical_agree";
  (match Browser.node_binary () with Some _ -> () | None -> skip "no node");
  (match Browser.chrome_binary () with
  | Some _ -> ()
  | None -> skip "no headless browser");
  let jobs = List.filter_map build_job (base_sheets ()) in
  let jobs =
    List.filter
      (fun j -> match j.rendered with [] -> false | _ :: _ -> true)
      jobs
  in
  let st =
    {
      pairs = 0;
      tool_pairs = 0;
      split_pairs = 0;
      mutant_pairs = 0;
      conflations = 0;
      split_breaks = 0;
      over = 0;
      unreadable = 0;
      failures = 0;
    }
  in
  let started = Unix.gettimeofday () in
  List.iter
    (fun job ->
      st.over <- st.over + job.over_reports;
      st.unreadable <- st.unreadable + job.unreadable;
      List.iter (check_pair st job) job.rendered)
    jobs;
  summary st ~jobs:(List.length jobs) ~elapsed:(Unix.gettimeofday () -. started);
  if st.failures > 0 then exit 1
