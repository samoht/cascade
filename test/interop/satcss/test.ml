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

let check_case name () =
  let dir = Filename.concat trace_root name in
  let input = read_file (Filename.concat dir "input.css") in
  let expected = read_file (Filename.concat dir "expected.css") in
  let cascade = cascade_output input in
  let satcss = satcss_output expected in
  Alcotest.(check bool)
    "cascade is no longer than SatCSS structural oracle" true
    (String.length cascade <= String.length satcss)

let no_traces () = Alcotest.skip ()

let () =
  let tests =
    match cases () with
    | [] -> [ ("no local traces", `Quick, no_traces) ]
    | names -> List.map (fun name -> (name, `Slow, check_case name)) names
  in
  Alcotest.run "satcss" [ ("satcss", tests) ]
