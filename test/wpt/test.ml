(** WPT css-syntax vector harness.

    Reads [test/vectors/wpt/css-syntax/*.html] and runs each extracted CSS input
    through {!Css.parse}. Raises are reported as Alcotest failures; per-file
    assertions about property-level behaviour are out of scope (we don't ship
    CSSOM), so this harness's job is to stress the parser against spec-derived
    strings and surface crashes / unexpected errors.

    Extraction is text-level, not a real HTML/JS parse. It covers two patterns
    that account for most WPT test inputs:

    - [<style>...</style>] blocks: CSS lifted out of HTML-embedded stylesheets.
    - [parseRule(`...`)] calls: CSS passed as a template-literal argument in the
      test JS.

    Tests that build input strings dynamically (e.g. a loop over code points in
    [non-ascii-codepoints.html], or [document.querySelector] / inline
    [style=...] attributes in [unclosed-url-at-eof.html]) are not covered; they
    would need a JS runtime to extract faithfully.

    Failure policy: no skip list. A failing vector is a code bug or a
    mis-extraction to be fixed, not silenced. *)

let vectors_dir = "../vectors/wpt/css-syntax"

(** {1 String scanning} *)

(* Find the first substring [needle] in [s] starting at [from]. *)
let find_from ~from s needle =
  let nlen = String.length needle in
  let slen = String.length s in
  let rec loop i =
    if i + nlen > slen then None
    else if String.sub s i nlen = needle then Some i
    else loop (i + 1)
  in
  loop from

(* Extract substrings between [open_tag] and [close_tag] pairs, advancing past
   each match. Tags are matched literally; not a real HTML parser. *)
let extract_between ~open_tag ~close_tag s =
  let olen = String.length open_tag in
  let rec loop acc from =
    match find_from ~from s open_tag with
    | None -> List.rev acc
    | Some o -> (
        (* Find the end of the opening tag (closing '>'). *)
        let after_open =
          match find_from ~from:(o + olen) s ">" with
          | None -> o + olen
          | Some i -> i + 1
        in
        match find_from ~from:after_open s close_tag with
        | None -> List.rev acc
        | Some c ->
            let body = String.sub s after_open (c - after_open) in
            loop (body :: acc) (c + String.length close_tag))
  in
  loop [] 0

(* Extract template-literal arguments from [name(`...`)] calls. The backtick
   body runs until the next unescaped backtick on the same JS line or across
   lines; we accept either since JS template literals do. *)
let extract_template_args ~call_name s =
  let marker = call_name ^ "(`" in
  let mlen = String.length marker in
  let rec loop acc from =
    match find_from ~from s marker with
    | None -> List.rev acc
    | Some i -> (
        let body_start = i + mlen in
        (* Find the next backtick that isn't preceded by a backslash. *)
        let rec find_close j =
          match find_from ~from:j s "`" with
          | None -> None
          | Some k when k > 0 && s.[k - 1] = '\\' -> find_close (k + 1)
          | some -> some
        in
        match find_close body_start with
        | None -> List.rev acc
        | Some c ->
            let body = String.sub s body_start (c - body_start) in
            loop (body :: acc) (c + 1))
  in
  loop [] 0

(** {1 Per-file extraction} *)

type case = {
  source_file : string;
  origin : string;  (** "[n]: <style>" or "[n]: parseRule" *)
  css : string;
}

let nontrivial s = String.trim s <> ""

let cases_from_html ~source_file ~contents =
  let style_bodies =
    extract_between ~open_tag:"<style" ~close_tag:"</style>" contents
    |> List.filter nontrivial
  in
  let parse_rule_bodies =
    extract_template_args ~call_name:"parseRule" contents
    |> List.filter nontrivial
  in
  let styles =
    List.mapi
      (fun i body ->
        { source_file; origin = Printf.sprintf "<style>[%d]" i; css = body })
      style_bodies
  in
  let rules =
    List.mapi
      (fun i body ->
        { source_file; origin = Printf.sprintf "parseRule[%d]" i; css = body })
      parse_rule_bodies
  in
  styles @ rules

let cases_from_css ~source_file ~contents =
  [ { source_file; origin = "<file>"; css = contents } ]

let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  close_in ic;
  Bytes.unsafe_to_string buf

let list_files dir =
  Sys.readdir dir |> Array.to_list |> List.sort compare
  |> List.filter_map (fun entry ->
      let path = Filename.concat dir entry in
      if Sys.is_directory path then None else Some (entry, path))

let collect_cases () =
  let top = list_files vectors_dir in
  let support =
    let dir = Filename.concat vectors_dir "support" in
    if Sys.file_exists dir then
      list_files dir |> List.map (fun (e, p) -> ("support/" ^ e, p))
    else []
  in
  List.concat_map
    (fun (name, path) ->
      let contents = read_file path in
      if Filename.check_suffix name ".html" then
        cases_from_html ~source_file:name ~contents
      else if Filename.check_suffix name ".css" then
        cases_from_css ~source_file:name ~contents
      else [])
    (top @ support)

(** {1 Alcotest wiring} *)

let test_case case () =
  match try Ok (Cascade.Css.parse case.css) with e -> Error e with
  | Ok _ -> ()
  | Error e ->
      Alcotest.failf "%s (%s) raised: %s" case.source_file case.origin
        (Printexc.to_string e)

let build_suite () =
  let cases = collect_cases () in
  let tests =
    List.map
      (fun case ->
        let name = Printf.sprintf "%s %s" case.source_file case.origin in
        Alcotest.test_case name `Quick (test_case case))
      cases
  in
  ("wpt css-syntax", tests)

let () = Alcotest.run "wpt" [ build_suite () ]
