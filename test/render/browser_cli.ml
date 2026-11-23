(* [cascade diff --browser --html DOC A B] against the browser it drives: the
   pair the README's parity example rests on has to report the pseudo-element
   losing its content, an identical pair has to report nothing, and a run that
   cannot reach a browser has to fail rather than answer. The CLI is the one
   under test, so it is run as a process with the browser the harnesses share,
   and its exit status and report are what is checked.

   Skips cleanly, with status 0, when node or a headless Chromium is missing.
   CASCADE_NO_BROWSER fails instead: see [Browser.suppressed]. *)

let ( // ) = Filename.concat
let skip reason = Browser.skip "browser_cli" reason

let write file contents =
  Out_channel.with_open_bin file (fun oc -> output_string oc contents)

(* The command's stdout and exit status, run with no shell between; stderr goes
   to the file named. *)
let run ~env prog args ~stderr =
  let lines, status = Browser.run ~env ~stderr prog args in
  let status =
    match status with
    | Unix.WEXITED n -> n
    | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> -1
  in
  (String.concat "\n" lines, status)

let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec at i =
    i + n <= h && (String.equal (String.sub haystack i n) needle || at (i + 1))
  in
  n = 0 || at 0

let failures = ref 0

let check name ok =
  if ok then print_endline ("PASS: " ^ name)
  else (
    incr failures;
    print_endline ("FAIL: " ^ name))

let () =
  Browser.suppressed "browser_cli";
  let cascade =
    match Sys.argv with
    | [| _; exe |] -> exe
    | _ ->
        prerr_endline "usage: browser_cli CASCADE";
        exit 2
  in
  let node =
    match Browser.node_binary () with Some n -> n | None -> skip "no node"
  in
  let chrome =
    match Browser.chrome_binary () with
    | Some c -> c
    | None -> skip "no headless browser"
  in
  let work = Filename.get_temp_dir_name () // "cascade-browser-cli" in
  (try Unix.mkdir work 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let doc = work // "doc.html" in
  let a = work // "a.css" and b = work // "b.css" in
  write doc "<p style=\"position:relative\">You should use Rust</p>";
  let content =
    "p::after{content:\"You should use \
     OCaml\";position:absolute;inset:0;background:white}"
  in
  write a ("p::after{all:unset}" ^ content);
  write b (content ^ "p::after{all:unset}");
  let env browser =
    [ ("CHROME", browser); ("NODE", node); ("CASCADE_COLOR", "never") ]
  in
  let diff args =
    run ~env:(env chrome) cascade
      ([ "diff"; "--browser"; "--html"; doc ] @ args)
      ~stderr:(work // "stderr.txt")
  in
  (* The reset crossing the content is what the browser has to see. *)
  let report, status = diff [ a; b ] in
  check "a pair that differs exits 1" (status = 1);
  check "the pseudo-element is named" (contains report "p:nth-child(1)::after");
  check "its content is reported going from the string to none"
    (contains report "content: \"You should use OCaml\" -> none");
  check "and the render that differs is placed"
    (contains report "Renders that differ: 1"
    && contains report "1024x768 none: "
    && contains report " pixels differ at (");
  (* The same pair as JSON carries the same change, machine-readable. *)
  let json, status = diff [ "--json"; a; b ] in
  check "the JSON document exits 1 too" (status = 1);
  check "the JSON names the property"
    (contains json "\"property\": \"content\"");
  check "the JSON records the browser"
    (contains json "\"browser\": {" && contains json "\"version\": \"");
  check "the JSON counts what was rendered"
    (contains json "\"captures\": " && contains json "\"elements\": ");
  check "and places the render that differs"
    (contains json "\"renders\": [" && contains json "\"first_size\": ");
  (* Identical inputs are identical to the browser. *)
  let report, status = diff [ a; a ] in
  check "an identical pair exits 0" (status = 0);
  check "and reports no render that differs"
    (contains report "Renders that differ: 0");
  (* A browser that answers nothing is a failure, never an equivalence: a
     stand-in that exits without a page is what a broken or missing browser
     looks like to the driver. *)
  let broken =
    if Sys.win32 then (
      let broken = work // "broken-browser.cmd" in
      write broken "@exit /b 1\r\n";
      broken)
    else
      let broken = work // "broken-browser" in
      write broken "#!/bin/sh\nexit 1\n";
      Unix.chmod broken 0o755;
      broken
  in
  let _, status =
    run ~env:(env broken) cascade
      [ "diff"; "--browser"; "--html"; doc; a; b ]
      ~stderr:(work // "stderr.txt")
  in
  check "a browser that answers nothing exits 2" (status = 2);
  (* The document is required, and the CLI says so rather than comparing the
     CSS. *)
  let _, status =
    run ~env:(env chrome) cascade
      [ "diff"; "--browser"; a; b ]
      ~stderr:(work // "stderr.txt")
  in
  check "--browser without --html is a usage error" (status = 124);
  if !failures > 0 then exit 1
