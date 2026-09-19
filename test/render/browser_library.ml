(* [Browser_compare.run] against the browser it drives, the way a program that
   links the library calls it: a changed declaration is a render that differs,
   explained by a difference naming the element and the property, a
   pseudo-element's content is read, an identical pair has no render that
   differs, two spellings of one picture have none either, and a browser that
   answers nothing is an error rather than an empty report. A viewport width is
   rendered when a rule some element of the page can match names it, and not
   when only rules no element matches do; a sheet whose reader dropped a rule is
   rendered as written.

   Skips cleanly, with status 0, when node or a headless Chromium is missing.
   CASCADE_NO_BROWSER fails instead: see [Browser.suppressed]. *)

let skip reason = Browser.skip "browser_library" reason
let failures = ref 0

let check name ok =
  if ok then print_endline ("PASS: " ^ name)
  else (
    incr failures;
    print_endline ("FAIL: " ^ name))

let html = "<!doctype html><p class=\"a\">text</p>"
let first = ".a{color:red}.a::before{content:\"x\"}"

let differences sheets =
  match Browser_compare.run ~html sheets with
  | Ok report -> Some report.differences
  | Error _ -> None

let renders sheets =
  match Browser_compare.run ~html sheets with
  | Ok report -> Some report.renders
  | Error _ -> None

let () =
  Browser.suppressed "browser_library";
  (* The count is checked before any browser is looked for, so this holds where
     the rest of the file skips. *)
  (match
     Browser_compare.run ~html
       [ ("first", first); ("second", first); ("third", first) ]
   with
  | Error _ -> check "a third sheet is refused before a browser is needed" true
  | Ok _ -> check "a third sheet is refused before a browser is needed" false);
  let node =
    match Browser.node_binary () with Some n -> n | None -> skip "no node"
  in
  (match Browser.chrome_binary () with
  | Some _ -> ()
  | None -> skip "no headless browser");
  (match renders [ ("first", first); ("second", first) ] with
  | Some [] -> check "an identical pair renders the same" true
  | Some _ | None -> check "an identical pair renders the same" false);
  let changed =
    [ ("first", first); ("second", ".a{color:blue}.a::before{content:\"y\"}") ]
  in
  (match renders changed with
  | Some (_ :: _) -> check "a changed declaration renders differently" true
  | Some [] | None -> check "a changed declaration renders differently" false);
  (match differences changed with
  | Some ds ->
      let on property pseudo =
        List.exists
          (fun (d : Browser_compare.difference) ->
            String.equal d.property property && String.equal d.pseudo pseudo)
          ds
      in
      check "the difference names the property" (on "color" "");
      check "a pseudo-element's content is read" (on "content" "::before")
  | None -> check "a changed pair runs" false);
  (* Two spellings of one picture are not a difference, whatever a script reads
     back: the browser serialises them apart and paints them alike. *)
  let same_picture name a b =
    match renders [ ("first", a); ("second", b) ] with
    | Some [] -> check name true
    | Some _ | None -> check name false
  in
  same_picture "a colour keyword and its hex render the same" ".a{color:red}"
    ".a{color:#f00}";
  same_picture "background:none and background:0 0 render the same"
    ".a{background:none}" ".a{background:0 0}";
  same_picture "a position keyword and its length render the same"
    ".a{background:url() left top red}" ".a{background:url() 0 0 red}";
  (* A width named only by a rule no element of the page matches cannot change
     what the page computes, so it is not sampled; one named by a rule that does
     match is. A rule the reader drops leaves nothing to say whether it matches,
     so that sheet is read as written. *)
  let viewports sheets =
    match Browser_compare.run ~html sheets with
    | Ok report ->
        Option.value ~default:"" (List.assoc_opt "viewports" report.meta)
    | Error _ -> ""
  in
  let samples width vs =
    List.exists
      (fun v -> String.equal v (string_of_int width ^ "x768"))
      (String.split_on_char ' ' vs)
  in
  let widths =
    viewports
      [
        ( "first",
          ".a{color:red}@media (width>=700px){.a{color:green}}@media \
           (width>=900px){.not-on-the-page{color:blue}}" );
        ("second", ".a{color:red}");
      ]
  in
  check "a width a matching rule names is sampled" (samples 700 widths);
  check "a width only an unmatched rule names is not sampled"
    (not (samples 900 widths));
  let dropped =
    viewports
      [
        ("first", ".a{color:red}");
        ( "second",
          ".a{color:red}@media (width>=800px){.b:zz-unknown{color:blue}}" );
      ]
  in
  check "a sheet the reader dropped a rule from is sampled as written"
    (samples 800 dropped);
  (* CSS Images 3 sec. 3.5.3 places an unpositioned last colour stop at 100%, so
     a mask written with the position and one written without it are the same
     pixels, though the browser serialises the two differently. A first stop
     moved to 50% is another image. *)
  let mask stops =
    String.concat ""
      [
        ".a{background:red;mask-image:radial-gradient(25% 50% at 30% 50%,";
        stops;
        ")}";
      ]
  in
  same_picture "an unpositioned last stop renders as one at 100%"
    (mask "#fff 100%,#0000 100%")
    (mask "#fff 100%,#0000");
  (match
     renders
       [
         ("first", mask "#fff 100%,#0000 100%");
         ("second", mask "#fff 50%,#0000 100%");
       ]
   with
  | Some (_ :: _) -> check "a first stop at 50% renders differently" true
  | Some [] | None -> check "a first stop at 50% renders differently" false);
  (* A stand-in that exits without a page is what a broken browser looks like to
     the driver. *)
  let broken =
    Filename.concat (Filename.get_temp_dir_name ()) "cascade-broken"
  in
  Out_channel.with_open_bin broken (fun oc ->
      output_string oc "#!/bin/sh\nexit 1\n");
  Unix.chmod broken 0o755;
  Unix.putenv "CHROME" broken;
  Unix.putenv "NODE" node;
  check "a browser that answers nothing is an error"
    (match
       Browser_compare.run ~html [ ("first", first); ("second", first) ]
     with
    | Error _ -> true
    | Ok _ -> false);
  print_endline (String.concat "" [ "  failures: "; string_of_int !failures ]);
  exit (if !failures = 0 then 0 else 1)
