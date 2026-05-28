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

let cascade_output input =
  input |> parse "input.css"
  |> Cascade.Css.optimize ~scope:`Stylesheet
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

(* Cases where cascade's output is legitimately larger than SatCSS's because the
   merge SatCSS makes is unsafe for an arbitrary DOM. SatCSS minifies each
   stylesheet against that page's captured DOM, so it can merge rules that are
   only safe for that specific DOM. cascade preserves rendering for ANY DOM (it
   is scoped to CSS text, never a runtime DOM; see {!Cascade.Optimize.scope}),
   so it correctly leaves such merges undone. These are not cascade deficiencies
   and cannot be closed without stepping outside cascade's scope.

   Each entry asserts cascade is STILL larger, so a fix that closes the gap
   fails the test and prompts removing the entry. *)
let known_larger =
  [
    ( "sohu-3",
      "DOM-dependent: hoisting shared declarations out of `.auto-search1 \
       .log-search` / `.hangxing-select .hangxing-input` would cross \
       `.auto-hot-tag li` (a descendant-combinator tie writing a different \
       `border`/`height`). For a possible `li.log-search` element this changes \
       rendering at any hoist position; SatCSS proves no such element exists \
       from the page DOM." );
    ( "vk-3",
      "DOM-dependent: `.blocked_about_login` ties `.blocked_no_code` on \
       `padding-top` with a different value, so grouping them is safe only if \
       no element carries both classes - which only the page DOM can \
       establish." );
    ( "google-2",
      "DOM-dependent: SatCSS groups the `position:relative` rules \
       (`#viewport,.goog-inline-block,.logo-subtext`) and the \
       `position:absolute` rules (`#footer,.jfk-bubble,.jfk-bubble-arrow`). \
       Both cross `.goog-inline-block{position:relative}`, a class selector \
       (0,1,0) that ties `.jfk-bubble` (0,1,0) with a different `position` \
       value - hoisting across it would flip an element matching \
       `.jfk-bubble.goog-inline-block`. cascade correctly refuses; SatCSS \
       relies on the page DOM proving no such element exists." );
    ( "outlook-3",
      "DOM-dependent: after cost-aware extraction of the `padding-top` group, \
       the residual gap is SatCSS grouping `background-color` and `dir`-scoped \
       (`html[dir=ltr]`/`html[dir=rtl]`) `left`/`right`/`text-align` rules \
       across class ties only the page DOM can prove disjoint." );
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
        (Printf.sprintf
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
