let trace_root = "traces"

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let parse path css =
  match Cascade.Css.of_string ~strict:false css with
  | Ok { Cascade.Css.stylesheet; _ } -> stylesheet
  | Error e -> Alcotest.failf "%s: %s" path (Cascade.Error.to_string e)

(* SatCSS minifies each stylesheet against the page's captured DOM, so it makes
   merges only safe for that specific DOM. [~closed_world:true] is the matching
   opt-in: the caller asserts no element matches two distinct selectors that
   would otherwise tie, so cascade makes the same DOM-dependent merges and the
   comparison is like-for-like. (cascade's default still assumes any DOM.)
   [~objective:`Raw] matches SatCSS's raw-character objective: this suite
   compares structural rule-merging power, not compressed transfer size. *)
let cascade_output input =
  input |> parse "input.css"
  |> Cascade.Css.optimize ~scope:`Stylesheet ~closed_world:true ~objective:`Raw
  |> Cascade.Css.to_string ~minify:false

let satcss_output expected =
  expected |> parse "expected.css" |> Cascade.Css.to_string ~minify:false

let case_dir name =
  let input_path =
    Filename.concat (Filename.concat trace_root name) "input.css"
  in
  let expected_path =
    Filename.concat (Filename.concat trace_root name) "expected.css"
  in
  Sys.file_exists input_path && Sys.file_exists expected_path

let cases () =
  if not (Sys.file_exists trace_root) then []
  else
    Sys.readdir trace_root |> Array.to_list
    |> List.filter (fun name ->
        (not (String.equal name ".gitignore")) && case_dir name)
    |> List.sort String.compare

(* Residual cases where cascade stays a little larger than SatCSS even under
   closed-world (where it already makes the same DOM-dependent merges). The
   remainder is a structural-shape difference in the merged form, not a refused
   merge. Each entry asserts cascade is STILL larger, so a fix that closes the
   gap fails the test and prompts removing the entry. *)
let known_larger =
  [
    ( "vk-3",
      "Residual +17 bytes under closed-world: SatCSS's grouping of \
       `.blocked_about_login`/`.blocked_no_code` (tied on `padding-top`) lands \
       a marginally tighter structure than cascade's equivalent merge." );
    ( "google-2",
      "Residual +39 bytes under closed-world: with the DOM-disjointness \
       assumption cascade groups the `position` rules \
       (`#viewport,.goog-inline-block,...` and `#footer,.jfk-bubble,...`) but \
       reaches all but ~39 bytes of SatCSS's exact shape." );
  ]

let check_case name () =
  let dir = Filename.concat trace_root name in
  let input = read_file (Filename.concat dir "input.css") in
  let expected = read_file (Filename.concat dir "expected.css") in
  let cascade = String.length (cascade_output input) in
  let satcss = String.length (satcss_output expected) in
  match List.assoc_opt name known_larger with
  | Some reason ->
      Alcotest.(check bool)
        (Fmt.str
           "%s is a known-larger case (if this fails, cascade closed the gap - \
            remove from known_larger): %s"
           name reason)
        true (cascade > satcss)
  | None ->
      Alcotest.(check bool)
        "cascade is no longer than SatCSS structural oracle" true
        (cascade <= satcss)

let no_traces () = Alcotest.skip ()

let () =
  let tests =
    match cases () with
    | [] -> [ ("no local traces", `Quick, no_traces) ]
    | names -> List.map (fun name -> (name, `Slow, check_case name)) names
  in
  Alcotest.run "satcss" [ ("satcss", tests) ]
