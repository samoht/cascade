(** SatCSS benchmark interop.

    Drives [Cascade.Css.of_string] + [~minify:true] over the 75-site SatCSS
    benchmark corpus (Hague, Lin, Hong, "CSS Minification via Constraint
    Solving", TOPLAS 2019). Per site:

    - [traces/inputs/<site>-stripmq.css] is the raw input.
    - [traces/oracles/<site>-<tool>.css] holds the cascade-canonical form of
      each [{cleancss,cssmin,cssnano,csso,minify,yui}] minifier output,
      precomputed by the regen script. Test-time is a plain string compare.

    Pass condition: Cascade's minified output equals at least one oracle's
    cascade-canonical form byte-for-byte. On failure the report lists oracle
    byte sizes and Cascade's size.

    Traces are NOT committed to git (upstream has no LICENSE; CSS is
    third-party). Fresh checkouts produce zero alcotest cases. Run
    [dune build @regen-traces] to fetch the corpus, then [dune runtest]. *)

let inputs_dir = "traces/inputs"
let oracles_dir = "traces/oracles"

let read_file path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  close_in ic;
  Bytes.unsafe_to_string buf

let oracle_tools = [ "cleancss"; "cssmin"; "cssnano"; "csso"; "minify"; "yui" ]

let sites () =
  if not (Sys.file_exists inputs_dir) then []
  else
    Sys.readdir inputs_dir |> Array.to_list |> List.sort compare
    |> List.filter_map (fun name ->
        match Filename.chop_suffix_opt ~suffix:"-stripmq.css" name with
        | Some site -> Some site
        | None -> None)

let cascade_minify input =
  match Cascade.Css.of_string ~strict:false input with
  | Error e -> Error (Cascade.Error.to_string e)
  | Ok { Cascade.Css.stylesheet; warnings = _ } ->
      Ok (Cascade.Css.to_string ~minify:true stylesheet)

let case site () =
  let input = read_file (Filename.concat inputs_dir (site ^ "-stripmq.css")) in
  match cascade_minify input with
  | Error msg -> Alcotest.failf "cascade parse failure: %s" msg
  | Ok actual ->
      let oracles =
        List.filter_map
          (fun tool ->
            let path =
              Filename.concat oracles_dir (Printf.sprintf "%s-%s.css" site tool)
            in
            if Sys.file_exists path then
              (* The CLI [fmt --minify] appends a trailing newline; strip it so
                 we compare canonical bytes only. *)
              Some (tool, String.trim (read_file path))
            else None)
          oracle_tools
      in
      if oracles = [] then
        Alcotest.failf "no canonical oracle available for %s" site
      else if List.exists (fun (_, expected) -> expected = actual) oracles then
        ()
      else
        let oracle_summary =
          List.map
            (fun (tool, expected) ->
              Printf.sprintf "%s:%d" tool (String.length expected))
            oracles
          |> String.concat " "
        in
        let closest_tool, closest_expected =
          List.fold_left
            (fun (t0, e0) (t, e) ->
              let d0 = abs (String.length e0 - String.length actual) in
              let d = abs (String.length e - String.length actual) in
              if d < d0 then (t, e) else (t0, e0))
            (List.hd oracles) (List.tl oracles)
        in
        let n = min (String.length actual) (String.length closest_expected) in
        let rec find_diff i =
          if i >= n then i
          else if actual.[i] = closest_expected.[i] then find_diff (i + 1)
          else i
        in
        let pos = find_diff 0 in
        let window s ~at ~radius =
          let start = max 0 (at - radius) in
          let len = min (2 * radius) (String.length s - start) in
          String.sub s start len
        in
        Alcotest.failf
          "cascade output differs from every canonical oracle\n\
          \    cascade:  %d bytes\n\
          \    oracles:  %s\n\
          \    first diff vs %s (closest) at byte %d:\n\
          \      %s: %s\n\
          \      cascade : %s"
          (String.length actual) oracle_summary closest_tool pos closest_tool
          (String.escaped (window closest_expected ~at:pos ~radius:60))
          (String.escaped (window actual ~at:pos ~radius:60))

let () =
  let cases = List.map (fun site -> (site, `Quick, case site)) (sites ()) in
  Alcotest.run "satcss" [ ("benchmarks", cases) ]
