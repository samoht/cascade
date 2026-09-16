(* [Browser_compare.run] against the browser it drives, the way a program that
   links the library calls it: a changed declaration is a difference naming the
   element and the property, a pseudo-element's content is sampled, an identical
   pair has no difference, and a browser that answers nothing is an error rather
   than an empty report. A viewport width is sampled when a rule some element of
   the page can match names it, and not when only rules no element matches do; a
   sheet whose reader dropped a rule is sampled as written.

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

let () =
  Browser.suppressed "browser_library";
  let node =
    match Browser.node_binary () with Some n -> n | None -> skip "no node"
  in
  (match Browser.chrome_binary () with
  | Some _ -> ()
  | None -> skip "no headless browser");
  (match differences [ ("first", first); ("second", first) ] with
  | Some [] -> check "an identical pair has no difference" true
  | Some _ | None -> check "an identical pair has no difference" false);
  (match
     differences
       [
         ("first", first); ("second", ".a{color:blue}.a::before{content:\"y\"}");
       ]
   with
  | Some ds ->
      let on property pseudo =
        List.exists
          (fun (d : Browser_compare.difference) ->
            String.equal d.property property && String.equal d.pseudo pseudo)
          ds
      in
      check "a changed declaration is reported" (on "color" "");
      check "and its paint differs"
        (List.exists
           (fun (d : Browser_compare.difference) ->
             String.equal d.property "color"
             && String.equal d.pseudo "" && not d.paints_same)
           ds);
      check "a pseudo-element's content is sampled" (on "content" "::before")
  | None -> check "a changed pair runs" false);
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
     pixels, though the browser serialises the two differently. A last stop at
     50% is another image. *)
  let mask stops =
    String.concat ""
      [ ".a{mask-image:radial-gradient(25% 50% at 30% 50%,"; stops; ")}" ]
  in
  let mask_paints stops =
    match
      differences
        [ ("first", mask "#fff 100%,#0000 100%"); ("second", mask stops) ]
    with
    | Some ds ->
        let masks =
          List.filter
            (fun (d : Browser_compare.difference) ->
              String.equal d.property "mask-image")
            ds
        in
        Some
          ( masks <> [],
            List.for_all (fun d -> d.Browser_compare.paints_same) masks )
    | None -> None
  in
  (match mask_paints "#fff 100%,#0000" with
  | Some (true, paints) ->
      check "an unpositioned last stop paints as one at 100%" paints
  | Some (false, _) -> check "the implied stop is spelled differently" false
  | None -> check "the implied stop pair runs" false);
  (match mask_paints "#fff 100%,#0000 50%" with
  | Some (true, paints) ->
      check "a last stop at 50% paints differently" (not paints)
  | Some (false, _) -> check "the 50% stop is reported" false
  | None -> check "the 50% stop pair runs" false);
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
