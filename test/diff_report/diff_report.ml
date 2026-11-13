(* Whether a difference [cascade diff] REPORTS is accurate.

   [test/render/canonical_agree.ml] asks the other half of the certification
   claim: when the verdict is "no difference", do the two sheets really render
   alike. Nothing asked whether a report is true of the files it describes, and
   README makes that a claim - "added, removed, modified, and reordered rules
   are detected structurally, and property value changes are reported in terms
   of CSS values", with exit 1 for a difference and exit 0 for identity - so a
   report that names a rule neither file changed, or misses one both files
   changed, is a defect on the same footing as a wrong verdict.

   Three oracles, none of them cascade's own diff:

   1. Constructed ground truth. A seeded model of a stylesheet is rendered to
   text, one addressed mutation is applied to the model, and both sides are
   rendered again. The mutation's site is known before the diff runs, so the set
   of selectors the report names must be exactly the set the mutation touched: a
   name outside it is an over-report, a name missing from it is a miss. The
   values a value change reports are read back from the two parsed sheets, never
   from the diff. Weakness: only the mutations written here.

   2. Claim verification. Every entry a report emits asserts something about the
   two files - this selector is in the actual side, that declaration is in the
   expected side, this property went from X to Y. Each such assertion is checked
   against the two parsed stylesheets. This needs no ground truth, so it runs
   over corpus pairs too. Weakness: it can only falsify what the report says,
   never notice what it failed to say.

   3. Self-consistency, which needs no oracle. The summary line counts what the
   body shows; a shortened report's elision count closes the arithmetic; the
   exit status matches the verdict printed above it; the same pair reports the
   same bytes twice.

   No browser. The browser is the arbiter for "is this reported difference real"
   under [--diff=canonical], and that question is worth asking, but the pairs
   worth asking it about are the ones a transform README calls cascade-neutral
   produced, and for those the neutrality is already established: README names
   them, and [canonical_agree] renders the split shape against its source on
   every run. Constructing them here and asserting the verdict is [No_diff]
   tests the same claim without a browser launch. What that leaves uncovered is
   an over-report on a pair nobody predicted to be neutral, which is the
   browser's to find and is not asked here.

   Every disagreement is reported, never the first only, and a run that
   exercises no pair fails rather than passing quietly. *)

open Cascade
module C = Cascade_diff.Css_compare
module D = Cascade_diff.Tree_diff
open Diff_report_index
open Diff_report_model
open Diff_report_claims

(* ===== Findings ===== *)

type category = Over | Wrong | Miss | Inconsistent | Control

let equal_category a b =
  match (a, b) with
  | Over, Over
  | Wrong, Wrong
  | Miss, Miss
  | Inconsistent, Inconsistent
  | Control, Control ->
      true
  | (Over | Wrong | Miss | Inconsistent | Control), _ -> false

let category_name = function
  | Over -> "over-report"
  | Wrong -> "wrong-name"
  | Miss -> "missed"
  | Inconsistent -> "inconsistent"
  | Control -> "control"

type finding = {
  category : category;
  pair : string;
  detail : string;
  repro : string;
}

let findings : finding list ref = ref []

let report category ~pair ~detail ~repro =
  findings := { category; pair; detail; repro } :: !findings

(* ===== Running the binary, for the exit status ===== *)

let bin = ref None

let write file contents =
  Out_channel.with_open_bin file (fun oc -> output_string oc contents)

let read_all ic =
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  Buffer.contents buf

let work () =
  let dir =
    Filename.concat (Filename.get_temp_dir_name ()) "cascade-diff-report"
  in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  dir

(* [cascade diff] on two files, as a caller runs it: the status and the bytes it
   writes on standard output and standard error. No shell stands between: a
   POSIX command line is not one [cmd.exe] can run. *)
let run_cli ~exe ~mode ~extra a b =
  let dir = work () in
  let fa = Filename.concat dir "expected.css" in
  let fb = Filename.concat dir "actual.css" in
  write fa a;
  write fb b;
  let args =
    [ exe; "diff"; String.concat "" [ "--diff="; mode ] ] @ extra @ [ fa; fb ]
  in
  let environment =
    Array.of_list
      ("NO_COLOR=1"
      :: List.filter
           (fun b -> not (String.starts_with ~prefix:"NO_COLOR=" b))
           (Array.to_list (Unix.environment ())))
  in
  let out, child_out = Unix.pipe ~cloexec:true () in
  let pid =
    Fun.protect
      ~finally:(fun () -> Unix.close child_out)
      (fun () ->
        Unix.create_process_env exe (Array.of_list args) environment Unix.stdin
          child_out child_out)
  in
  let ic = Unix.in_channel_of_descr out in
  let output =
    Fun.protect ~finally:(fun () -> close_in ic) (fun () -> read_all ic)
  in
  let status =
    match Unix.waitpid [] pid with
    | _, Unix.WEXITED n -> n
    | _, (Unix.WSIGNALED n | Unix.WSTOPPED n) -> 128 + n
  in
  (status, output)

let cli_repro ~mode ~extra =
  String.concat " "
    ([ "cascade diff"; String.concat "" [ "--diff="; mode ] ]
    @ extra
    @ [ "expected.css actual.css" ])

(* ===== One pair, every check that needs no ground truth ===== *)

let checked_pairs = ref 0
let checked_claims = ref 0
let cli_pairs = ref 0

let diff_of ~mode a b =
  match C.diff ~mode a b with
  | d -> Some d
  | exception (Failure _ | Invalid_argument _) -> None

let show css = if String.length css <= 200 then css else String.sub css 0 200

(* The claims a report makes, the shape of the printed report, and the status
   the binary exits with. [cli] bounds how many pairs pay for a process. *)
let check_pair ~pair ~mode ~cli ~expected_str ~actual_str result =
  incr checked_pairs;
  let mode_name =
    match mode with
    | `Auto -> "auto"
    | `Tree -> "tree"
    | `String -> "string"
    | `Canonical -> "canonical"
  in
  let repro = cli_repro ~mode:mode_name ~extra:[] in
  let bodies =
    String.concat "\n"
      [
        "  expected.css: ";
        show expected_str;
        "  actual.css:   ";
        show actual_str;
      ]
  in
  (* 1. Every claim the report makes is true of the two files. *)
  (match
     ( C.as_tree_diff result,
       index_for mode expected_str,
       index_for mode actual_str )
   with
  | Some d, Some expected, Some actual ->
      List.iter
        (fun c ->
          incr checked_claims;
          if not c.ok then
            report Wrong ~pair
              ~detail:
                (String.concat "\n"
                   [ String.concat "" [ "the report claims "; c.text ]; bodies ])
              ~repro)
        (claims ~expected ~actual d)
  | _ -> ());
  (* 2. The summary counts what the body shows. A layer-order entry prints its
     swaps at the same indentation as a top-level entry, so a report carrying
     one is left out of the arithmetic rather than counted wrong. *)
  (match C.as_tree_diff result with
  | Some d when Option.is_none d.layer_order ->
      let s = C.stats ~expected_str ~actual_str result in
      let body = render_report result in
      let hidden = elided body in
      let printed =
        List.length (top_level_lines body) - if hidden > 0 then 1 else 0
      in
      if printed + hidden <> stats_total s then
        report Inconsistent ~pair
          ~detail:
            (String.concat "\n"
               [
                 String.concat ""
                   [
                     "summary counts ";
                     string_of_int (stats_total s);
                     " top-level difference(s), the report shows ";
                     string_of_int printed;
                     " and elides ";
                     string_of_int hidden;
                   ];
                 String.concat ""
                   [
                     "  summary: ";
                     String.trim
                       (render_summary ~expected_str ~actual_str result);
                   ];
                 bodies;
               ])
          ~repro;
      (* 3. A shortened report closes its own arithmetic. *)
      let total = printed + hidden in
      if total > 1 then begin
        let short = render_report ~entries:1 result in
        let short_hidden = elided short in
        let short_printed =
          List.length (top_level_lines short)
          - if short_hidden > 0 then 1 else 0
        in
        if short_printed + short_hidden <> total then
          report Inconsistent ~pair
            ~detail:
              (String.concat ""
                 [
                   "--limit=1 shows ";
                   string_of_int short_printed;
                   " and elides ";
                   string_of_int short_hidden;
                   ", against ";
                   string_of_int total;
                   " in the full report";
                 ])
            ~repro:(cli_repro ~mode:mode_name ~extra:[ "--limit=1" ])
      end
  | _ -> ());
  (* 4. No entry asserts nothing. *)
  (match C.as_tree_diff result with
  | None -> ()
  | Some d ->
      List.iter
        (fun selector ->
          report Wrong ~pair
            ~detail:
              (String.concat "\n"
                 [
                   String.concat ""
                     [
                       "the report calls ";
                       selector;
                       " modified and shows nothing: the same declarations \
                        before and after, and no property change, added or \
                        removed property";
                     ];
                   bodies;
                 ])
            ~repro)
        (vacuous_entries d));
  (* 5. A difference is a report. *)
  (match result.C.result with
  | C.Tree_diff _ | C.String_diff _ ->
      let body = String.trim (render_report result) in
      if String.equal body "" then
        report Inconsistent ~pair
          ~detail:
            (String.concat "\n"
               [ "the comparison differs and the report is empty"; bodies ])
          ~repro
  | C.No_diff | C.Both_errors _ | C.Expected_error _ | C.Actual_error _ -> ());
  (* 6. The binary's status matches the verdict, and two runs agree. *)
  match (!bin, cli) with
  | Some exe, true ->
      incr cli_pairs;
      let status, out =
        run_cli ~exe ~mode:mode_name ~extra:[] expected_str actual_str
      in
      let status', out' =
        run_cli ~exe ~mode:mode_name ~extra:[] expected_str actual_str
      in
      if status <> status' || not (String.equal out out') then
        report Inconsistent ~pair ~detail:"two runs of the same pair disagree"
          ~repro;
      let differs =
        match result.C.result with
        | C.No_diff -> false
        | C.Tree_diff _ | C.String_diff _ | C.Both_errors _ | C.Expected_error _
        | C.Actual_error _ ->
            true
      in
      if differs && status <> 1 then
        report Inconsistent ~pair
          ~detail:
            (String.concat ""
               [
                 "the comparison reports a difference and the binary exits ";
                 string_of_int status;
                 " (README: differences exit 1)";
               ])
          ~repro
      else if (not differs) && status = 1 then
        report Inconsistent ~pair
          ~detail:"the comparison reports no difference and the binary exits 1"
          ~repro
      else if status > 2 then
        report Inconsistent ~pair
          ~detail:
            (String.concat ""
               [
                 "the binary exits ";
                 string_of_int status;
                 ", outside 0/1/2:\n";
                 out;
               ])
          ~repro
  | _, _ -> ()

(* ===== The ground-truth arm ===== *)

let ground_truth_cases = ref 0
let dropped_cases = ref 0

let check_ground_truth ~pair ~expected_str ~actual_str ~site result =
  match C.as_tree_diff result with
  | None ->
      (* The mutation is a difference, so the walk has to reach it. A string
         diff names no rule at all. *)
      report Miss ~pair
        ~detail:
          (String.concat ""
             [
               "the mutation changed ";
               String.concat ", " site.selectors;
               " and the comparison produced no structural report";
             ])
        ~repro:(cli_repro ~mode:"auto" ~extra:[])
  | Some d -> (
      incr ground_truth_cases;
      let named = named_selectors d in
      let partners = swap_partners d in
      let touched = List.sort_uniq String.compare site.selectors in
      let repro = cli_repro ~mode:"auto" ~extra:[] in
      let bodies =
        String.concat "\n"
          [
            String.concat "" [ "  expected.css: "; show expected_str ];
            String.concat "" [ "  actual.css:   "; show actual_str ];
            String.concat "" [ "  report names: "; String.concat ", " named ];
          ]
      in
      let extra =
        List.filter (fun s -> not (List.exists (String.equal s) touched)) named
      in
      (match extra with
      | [] -> ()
      | _ :: _ ->
          report Over ~pair
            ~detail:
              (String.concat "\n"
                 [
                   String.concat ""
                     [
                       "the mutation touched ";
                       String.concat ", " touched;
                       "; the report also names ";
                       String.concat ", " extra;
                     ];
                   bodies;
                 ])
            ~repro);
      let missing =
        List.filter (fun s -> not (List.exists (String.equal s) named)) touched
      in
      (* A swap names its partner in the entry rather than as an entry of its
         own, and that is naming it. *)
      let missing =
        List.filter
          (fun s -> not (List.exists (String.equal s) partners))
          missing
      in
      (match missing with
      | [] -> ()
      | _ :: _ ->
          report Miss ~pair
            ~detail:
              (String.concat "\n"
                 [
                   String.concat ""
                     [
                       "the mutation touched ";
                       String.concat ", " touched;
                       "; the report does not name ";
                       String.concat ", " missing;
                     ];
                   bodies;
                 ])
            ~repro);
      (* A condition is spelled by the report, not by the sheet: [(a:b)] in the
         source reads back as [(a: b)], and a [@supports] names its condition
         without the at-keyword. Compared without whitespace, and by containment
         so the at-keyword is not the difference. *)
      let site_containers = List.map tight site.containers in
      let containers = List.map tight (named_containers d) in
      let stray =
        List.filter
          (fun c ->
            not
              (List.exists
                 (fun s -> contains s c || contains c s)
                 site_containers))
          containers
      in
      (match (site.containers, stray) with
      | [], _ | _, [] -> ()
      | _ :: _, _ :: _ ->
          report Over ~pair
            ~detail:
              (String.concat "\n"
                 [
                   String.concat ""
                     [
                       "the mutation is inside ";
                       String.concat ", " site.containers;
                       "; the report names container(s) ";
                       String.concat ", " stray;
                     ];
                   bodies;
                 ])
            ~repro);
      (* The values a value change reports are the two the files hold. *)
      (match site.value_change with
      | None -> ()
      | Some (sel, prop, before, after) ->
          let changes =
            List.filter_map
              (fun (r : D.rule_diff) ->
                match r with
                | Content_changed { selector; property_changes; _ }
                  when String.equal selector sel ->
                    Some property_changes
                | _ -> None)
              d.rules
            @ List.concat_map
                (fun c ->
                  let rec go (c : D.container_diff) =
                    match c with
                    | Modified { rule_changes; container_changes; _ } ->
                        List.filter_map
                          (fun (r : D.rule_diff) ->
                            match r with
                            | Content_changed { selector; property_changes; _ }
                              when String.equal selector sel ->
                                Some property_changes
                            | _ -> None)
                          rule_changes
                        @ List.concat_map go container_changes
                    | Added _ | Removed _ | Reordered _
                    | Block_structure_changed _ ->
                        []
                  in
                  go c)
                d.containers
          in
          let all = List.concat changes in
          let matched =
            List.exists
              (fun (c : D.declaration) ->
                String.equal c.property_name prop
                && String.equal c.expected_value before
                && String.equal c.actual_value after)
              all
          in
          if not matched then
            report Wrong ~pair
              ~detail:
                (String.concat "\n"
                   [
                     String.concat ""
                       [
                         sel;
                         " { ";
                         prop;
                         " } went from ";
                         before;
                         " to ";
                         after;
                         "; the report says ";
                         (match all with
                         | [] -> "nothing about it"
                         | _ ->
                             String.concat "; "
                               (List.map
                                  (fun (c : D.declaration) ->
                                    String.concat ""
                                      [
                                        c.property_name;
                                        ": ";
                                        c.expected_value;
                                        " -> ";
                                        c.actual_value;
                                      ])
                                  all));
                       ];
                     bodies;
                   ])
              ~repro);
      (* A dropped or added declaration is named as one, under its selector. *)
      let named_removed =
        List.concat_map
          (fun (r : D.rule_diff) ->
            match r with
            | Content_changed { selector; removed_properties; _ } ->
                List.map (fun (p, _) -> (selector, p)) removed_properties
            | Removed { selector; declarations } ->
                List.map
                  (fun d -> (selector, Css.declaration_name d))
                  declarations
            | _ -> [])
          d.rules
      in
      let named_added =
        List.concat_map
          (fun (r : D.rule_diff) ->
            match r with
            | Content_changed { selector; added_properties; _ } ->
                List.map (fun (p, _) -> (selector, p)) added_properties
            | Added { selector; declarations } ->
                List.map
                  (fun d -> (selector, Css.declaration_name d))
                  declarations
            | _ -> [])
          d.rules
      in
      (match site.gone with
      | Some (sel, p)
        when (not (has_pair (sel, p) named_removed))
             && match site.containers with [] -> true | _ :: _ -> false ->
          report Miss ~pair
            ~detail:
              (String.concat "\n"
                 [
                   String.concat ""
                     [ sel; " lost { "; p; " } and the report does not say so" ];
                   bodies;
                 ])
            ~repro
      | _ -> ());
      match site.came with
      | Some (sel, p)
        when (not (has_pair (sel, p) named_added))
             && match site.containers with [] -> true | _ :: _ -> false ->
          report Miss ~pair
            ~detail:
              (String.concat "\n"
                 [
                   String.concat ""
                     [
                       sel; " gained { "; p; " } and the report does not say so";
                     ];
                   bodies;
                 ])
            ~repro
      | _ -> ())

open Diff_report_cases

(* ===== Calibration ===== *)

(* Both directions, on controls this harness owns, so a green run is not a blind
   one and a library fix cannot turn a control red. *)
let calibrate () =
  let fail what =
    report Control ~pair:"calibration" ~detail:what
      ~repro:"dune build @test/diff_report/runtest"
  in
  let idx text = Option.get (index_of_text text) in
  (* The claim checker must reject a claim that is false of the two files. *)
  let expected = idx ".a{color:red}" and actual = idx ".a{color:blue}" in
  let bogus : D.rule_diff =
    Removed { selector = ".nowhere"; declarations = [] }
  in
  (match rule_claims ~expected ~actual bogus with
  | [] -> fail "the claim checker says nothing about a Removed entry"
  | cs ->
      if List.for_all (fun c -> c.ok) cs then
        fail "the claim checker accepts a rule neither file holds");
  let wrong_value : D.rule_diff =
    Content_changed
      {
        selector = ".a";
        old_declarations = [];
        new_declarations = [];
        property_changes =
          [
            {
              property_name = "color";
              expected_value = "green";
              actual_value = "blue";
            };
          ];
        added_properties = [];
        removed_properties = [];
      }
  in
  if List.for_all (fun c -> c.ok) (rule_claims ~expected ~actual wrong_value)
  then fail "the claim checker accepts a value the expected file does not hold";
  (* And accept the claims a correct report would make about the same pair. *)
  let right : D.rule_diff =
    Content_changed
      {
        selector = ".a";
        old_declarations = [];
        new_declarations = [];
        property_changes =
          [
            {
              property_name = "color";
              expected_value = "red";
              actual_value = "blue";
            };
          ];
        added_properties = [];
        removed_properties = [];
      }
  in
  if not (List.for_all (fun c -> c.ok) (rule_claims ~expected ~actual right))
  then fail "the claim checker rejects a report that is true of the two files";
  (* The entry-count arithmetic must notice a summary that over-counts. A
     hand-built diff with one Content_changed that shows nothing is one the
     summary counts and the body cannot print. *)
  let silent : D.t =
    {
      rules =
        [
          Content_changed
            {
              selector = ".a";
              old_declarations = [];
              new_declarations = [];
              property_changes = [];
              added_properties = [];
              removed_properties = [];
            };
        ];
      containers = [];
      layer_order = None;
    }
  in
  let buf = Buffer.create 256 in
  D.pp ~color:false buf silent;
  let body = Buffer.contents buf in
  let printed = List.length (top_level_lines body) in
  if printed <> 0 then
    fail "the entry counter sees a top-level entry in a report that prints none";
  (* The counter must see the entries a real report does print. *)
  let two : D.t =
    {
      rules =
        [
          Added { selector = ".a"; declarations = [] };
          Removed { selector = ".b"; declarations = [] };
        ];
      containers = [];
      layer_order = None;
    }
  in
  let buf = Buffer.create 256 in
  D.pp ~color:false buf two;
  if List.length (top_level_lines (Buffer.contents buf)) <> 2 then
    fail "the entry counter miscounts a two-entry report";
  (* The elision reader must read the count a shortened report writes. *)
  let many : D.t =
    {
      rules =
        List.init 5 (fun i : D.rule_diff ->
            Added
              {
                selector = String.concat "" [ ".r"; string_of_int i ];
                declarations = [];
              });
      containers = [];
      layer_order = None;
    }
  in
  let buf = Buffer.create 256 in
  D.pp ~color:false ~entries:2 buf many;
  let body = Buffer.contents buf in
  if elided body <> 3 then
    fail
      (String.concat ""
         [ "the elision reader reads "; string_of_int (elided body); ", not 3" ]);
  if
    elided (render_report (C.diff ~mode:`Tree ".a{color:red}" ".a{color:blue}"))
    <> 0
  then fail "the elision reader invents an elision in a full report";
  (* A pair that differs must be reported, and an identical pair must not. *)
  (match (C.diff ~mode:`Tree ".a{color:red}" ".a{color:blue}").result with
  | C.Tree_diff _ -> ()
  | _ -> fail "a changed value is not a tree diff");
  (match (C.diff ~mode:`Tree ".a{color:red}" ".a{color:red}").result with
  | C.No_diff -> ()
  | _ -> fail "an identical pair is not No_diff");
  (* The ground-truth arm itself, both ways. A site the mutation did not touch
     must produce an over-report and a miss; the true site must produce neither.
     Findings the controls raise are taken back off the list. *)
  let expected_str = ".a{color:red}" and actual_str = ".a{color:blue}" in
  let result = C.diff ~mode:`Auto expected_str actual_str in
  let saved = !findings in
  let run site =
    findings := [];
    check_ground_truth ~pair:"calibration" ~expected_str ~actual_str ~site
      result;
    let got = List.map (fun f -> f.category) !findings in
    findings := saved;
    got
  in
  let truth =
    {
      empty_site with
      selectors = [ ".a" ];
      value_change = Some (".a", "color", "red", "blue");
    }
  in
  let wrong =
    {
      empty_site with
      selectors = [ ".nowhere" ];
      value_change = Some (".a", "color", "green", "blue");
    }
  in
  (match run truth with
  | [] -> ()
  | _ ->
      fail "the ground-truth arm rejects a report that names exactly the site");
  let got = run wrong in
  if not (List.exists (equal_category Over) got) then
    fail "the ground-truth arm misses a selector the mutation did not touch";
  if not (List.exists (equal_category Miss) got) then
    fail "the ground-truth arm misses a site the report does not name";
  if not (List.exists (equal_category Wrong) got) then
    fail "the ground-truth arm accepts a value change the files do not hold"

(* ===== Corpora ===== *)

let ( // ) = Filename.concat

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

(* Spread the sample over the whole list rather than taking a prefix, so every
   category is represented. *)
let sample n l =
  let total = List.length l in
  if n <= 0 || total <= n then l
  else
    let step = (total + n - 1) / n in
    List.filteri (fun i _ -> i mod step = 0) l

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

(* ===== Arguments ===== *)

let seeds = ref 40
let corpus = ref 0
let cli_budget = ref 200

let usage () =
  Fmt.pr "usage: diff_report [--bin PATH] [--seeds N] [--corpus N] [--cli N]@.";
  exit 0

let parse_args () =
  let n = Array.length Sys.argv in
  let rec loop i =
    if i < n then begin
      let arg () = if i + 1 < n then Sys.argv.(i + 1) else "" in
      (match Sys.argv.(i) with
      | "--bin" -> bin := Some (arg ())
      | "--seeds" ->
          seeds := Option.value ~default:!seeds (int_of_string_opt (arg ()))
      | "--corpus" ->
          corpus := Option.value ~default:!corpus (int_of_string_opt (arg ()))
      | "--cli" ->
          cli_budget :=
            Option.value ~default:!cli_budget (int_of_string_opt (arg ()))
      | "-h" | "--help" -> usage ()
      | _ -> ());
      loop (i + 1)
    end
  in
  loop 1

(* ===== Main ===== *)

let mutation_pairs = ref 0
let neutral_over = ref 0

let () =
  parse_args ();
  calibrate ();
  (* The controls run the same checks, so they move the same counters. *)
  ground_truth_cases := 0;
  checked_pairs := 0;
  checked_claims := 0;
  let cli_left = ref !cli_budget in
  let cli () =
    if !cli_left > 0 then (
      decr cli_left;
      true)
    else false
  in
  (* 1. Constructed ground truth. *)
  List.iter
    (fun seed ->
      let m = model ~seed in
      let expected_str = render m in
      match index_of_text expected_str with
      | None -> incr dropped_cases
      | Some idx_before ->
          let before_all = idx_before.rules in
          let rules = model_rules m in
          (* The model and the parse must line up, or an address means nothing.
             A sheet that does not round-trip is dropped and counted. *)
          let aligned =
            List.length before_all = List.length rules
            && List.for_all2
                 (fun e r ->
                   List.length e.props = List.length r.decls
                   && List.for_all2
                        (fun (p, _) d -> String.equal p d.prop)
                        e.props r.decls)
                 before_all rules
          in
          if not aligned then incr dropped_cases
          else
            List.iter
              (fun mutation ->
                let actual_str = render (apply m mutation) in
                match index_of_text actual_str with
                | None -> incr dropped_cases
                | Some idx_after -> (
                    let after_all = idx_after.rules in
                    match
                      site_of ~before:before_all ~after:after_all m mutation
                    with
                    | None -> incr dropped_cases
                    | Some site -> (
                        incr mutation_pairs;
                        let pair =
                          String.concat ""
                            [ "seed-"; string_of_int seed; "/"; label mutation ]
                        in
                        match diff_of ~mode:`Auto expected_str actual_str with
                        | None -> incr dropped_cases
                        | Some result ->
                            check_pair ~pair ~mode:`Auto ~cli:(cli ())
                              ~expected_str ~actual_str result;
                            check_ground_truth ~pair ~expected_str ~actual_str
                              ~site result)))
              (mutations m))
    (List.init !seeds (fun i -> i + 1));
  (* 1b. One-site changes in the at-rules and the nesting the model cannot
     write, each with the names a report has to carry. *)
  List.iter
    (fun tc ->
      let pair = String.concat "" [ "at-rule/"; tc.case ] in
      incr mutation_pairs;
      match diff_of ~mode:`Auto tc.before tc.after with
      | None -> incr dropped_cases
      | Some result ->
          check_pair ~pair ~mode:`Auto ~cli:true ~expected_str:tc.before
            ~actual_str:tc.after result;
          check_ground_truth ~pair ~expected_str:tc.before ~actual_str:tc.after
            ~site:
              {
                empty_site with
                selectors = tc.names;
                containers = tc.under;
                value_change = tc.holds;
              }
            result)
    text_cases;
  (* 2. Neutral transforms under canonical: README says the verdict is equal. *)
  List.iter
    (fun (name, a, b) ->
      let pair = String.concat "" [ "neutral/"; name ] in
      match diff_of ~mode:`Canonical a b with
      | None -> incr dropped_cases
      | Some result ->
          check_pair ~pair ~mode:`Canonical ~cli:true ~expected_str:a
            ~actual_str:b result;
          (match result.C.result with
          | C.No_diff -> ()
          | _ ->
              incr neutral_over;
              report Over ~pair
                ~detail:
                  (String.concat "\n"
                     [
                       "the two sheets are one stylesheet written two ways and \
                        canonical reports a difference";
                       String.concat "" [ "  expected.css: "; a ];
                       String.concat "" [ "  actual.css:   "; b ];
                       String.concat ""
                         [ "  report: "; String.trim (render_report result) ];
                     ])
                ~repro:(cli_repro ~mode:"canonical" ~extra:[]));
          (* The same pair under the structural modes must still make only true
             claims about the two files. *)
          Option.iter
            (fun r ->
              check_pair
                ~pair:(String.concat "" [ pair; "/tree" ])
                ~mode:`Tree ~cli:false ~expected_str:a ~actual_str:b r)
            (diff_of ~mode:`Tree a b))
    neutral_pairs;
  (* 3. Pairs that do differ, in every mode, so the claim checker sees
     reports. *)
  List.iter
    (fun (name, a, b) ->
      List.iter
        (fun (mode : C.mode) ->
          let mode_name =
            match mode with
            | `Auto -> "auto"
            | `Tree -> "tree"
            | `String -> "string"
            | `Canonical -> "canonical"
          in
          let pair = String.concat "" [ "differing/"; name; "/"; mode_name ] in
          match diff_of ~mode a b with
          | None -> incr dropped_cases
          | Some result ->
              check_pair ~pair ~mode ~cli:true ~expected_str:a ~actual_str:b
                result)
        [ `Auto; `Tree; `Canonical ])
    differing_pairs;
  (* 4. The committed corpora, mutated structurally, for claims about shapes the
     model does not write: nesting, @layer, keyframes, descriptors, vendor
     prefixes, repeated selectors. No ground truth here - the claim arm and the
     report's own arithmetic are what these pairs answer. *)
  List.iter
    (fun (name, text) ->
      match parse text with
      | None -> ()
      | Some sheet ->
          let source = Css.to_string sheet in
          let stmts = Css.statements sheet in
          let n = List.length stmts in
          let step = max 1 (n / 6) in
          let indices =
            List.filter (fun j -> j < n) (List.init 6 (fun j -> j * step))
          in
          let variants =
            List.concat_map
              (fun j ->
                [
                  ( String.concat "" [ "drop-rule-"; string_of_int j ],
                    Css.to_string
                      (Css.v (List.filteri (fun k _ -> k <> j) stmts)) );
                ]
                @
                if j + 1 < n then
                  [
                    ( String.concat "" [ "swap-rule-"; string_of_int j ],
                      Css.to_string
                        (Css.v
                           (List.mapi
                              (fun k s ->
                                if k = j then List.nth stmts (j + 1)
                                else if k = j + 1 then List.nth stmts j
                                else s)
                              stmts)) );
                  ]
                else [])
              indices
          in
          List.iter
            (fun (vlabel, variant) ->
              let pair = String.concat "" [ name; "/"; vlabel ] in
              List.iter
                (fun (mode : C.mode) ->
                  match diff_of ~mode source variant with
                  | None -> incr dropped_cases
                  | Some result ->
                      check_pair ~pair ~mode ~cli:(cli ()) ~expected_str:source
                        ~actual_str:variant result)
                [ `Auto; `Tree ])
            variants)
    (example_files () @ sample !corpus (corpus_files ()));
  (* ===== The run ===== *)
  let all = List.rev !findings in
  let by c = List.filter (fun f -> equal_category c f.category) all in
  List.iter
    (fun c ->
      match by c with
      | [] -> ()
      | fs ->
          Fmt.pr "@.=== %s: %d ===@." (category_name c) (List.length fs);
          List.iter
            (fun f ->
              Fmt.pr "FAIL [%s] %s@.  %s@.  repro: %s@." (category_name c)
                f.pair f.detail f.repro)
            fs)
    [ Control; Over; Wrong; Miss; Inconsistent ];
  Fmt.pr "@.diff_report: %d pair(s) checked, %d claim(s) verified@."
    !checked_pairs !checked_claims;
  Fmt.pr "  ground truth: %d mutation pair(s), %d structural report(s)@."
    !mutation_pairs !ground_truth_cases;
  Fmt.pr "  neutral pairs: %d, of which canonical calls %d different@."
    (List.length neutral_pairs)
    !neutral_over;
  Fmt.pr "  binary runs: %d pair(s)%s@." !cli_pairs
    (if Option.is_none !bin then " (no --bin: the exit status is unchecked)"
     else "");
  Fmt.pr "  cases dropped for want of an address: %d@." !dropped_cases;
  let bad = List.length all in
  (* A run that verifies nothing is not a clean run. The model alone produces
     hundreds of mutation pairs on any seed, so a small population means the
     harness stopped working rather than that the library got better. *)
  let blind =
    !mutation_pairs < 100 || !checked_claims < 100 || !ground_truth_cases < 100
  in
  if blind then
    Fmt.pr "FAIL: the population is too small to stand for anything@.";
  Fmt.pr "  findings: %d@." bad;
  if bad > 0 || blind then exit 1
