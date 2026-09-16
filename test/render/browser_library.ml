(* [Browser_compare.run] against the browser it drives, the way a program that
   links the library calls it: a changed declaration is a difference naming the
   element and the property, a pseudo-element's content is sampled, an identical
   pair has no difference, and a browser that answers nothing is an error rather
   than an empty report.

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
