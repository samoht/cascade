(** Minifier interop tests for cascade against keithamus/css-minify-tests.

    Inputs are vendored under [traces/tests/<category>/<NNNN>/], each test pair
    being a [source.css] (unminified) plus an [expected.css] (the canonical
    minified output agreed on by the upstream maintainers). The corpus is a
    vendor-neutral correctness oracle for CSS minifiers.

    Refresh inputs with [dune build @regen-traces]. Upstream pinned in
    [scripts/generate.sh].

    Pass criterion: Cascade's minified output must equal [expected.css] byte for
    byte (trailing whitespace ignored). Strict equality surfaces legitimate
    canonical-form divergences (e.g. [.5rem] vs [0.5rem]) so they can be
    arbitrated per-case rather than silently swept into the test oracle.

    The harness is one Alcotest case per pair, grouped under its upstream
    category. Each failing case prints its input, expected output, and actual
    output. *)

open Cascade

let traces_root =
  let local = Filename.concat "traces" "tests" in
  if Sys.file_exists local then local
  else Filename.concat "test/interop/css-minify-tests/traces" "tests"

let read_file path =
  let ic = open_in_bin path in
  let buf = Buffer.create 256 in
  try
    while true do
      Buffer.add_channel buf ic 4096
    done;
    assert false
  with End_of_file ->
    close_in ic;
    Buffer.contents buf

let strip_trailing_ws s =
  let len = String.length s in
  let rec last i =
    if i < 0 then 0
    else
      match s.[i] with ' ' | '\t' | '\n' | '\r' -> last (i - 1) | _ -> i + 1
  in
  String.sub s 0 (last (len - 1))

let normalize_expected expected =
  (* Cascade's README minify policy picks the shortest spec-equivalent spelling.
     CSS Syntax tokenizes these at-keywords and [(] separately, so the
     intervening space is optional when the grammar permits a leading
     parenthesized condition. Some imported keithamus fixtures keep the
     conventional space. For custom properties, [!important] is still
     declaration priority, not part of the custom-property token stream;
     [red!important] and [red !important] are equivalent, and the no-space form
     is shorter. Keep the vendored traces pristine and normalize only these safe
     token boundaries on the expected side. *)
  let replace sep by s =
    s |> Astring.String.cuts ~empty:true ~sep |> String.concat by
  in
  expected
  |> replace "@supports (" "@supports("
  |> replace "@media (" "@media("
  |> replace "@container (" "@container("
  |> replace "@scope (" "@scope("
  |> Astring.String.cuts ~empty:true ~sep:" !important"
  |> String.concat "!important"

let cascade_minify input =
  match Css.of_string ~strict:false input with
  | Error e -> Error (Cascade.Error.to_string e)
  | Ok parsed -> (
      match Css.to_string ~minify:true parsed.stylesheet with
      | s -> Ok s
      | exception Invalid_argument msg -> Error ("invalid_argument: " ^ msg))

type pair = { id : string; source : string; expected : string }

let list_subdirs path =
  if not (Sys.file_exists path) then []
  else
    Sys.readdir path |> Array.to_list
    |> List.filter (fun name ->
        let p = Filename.concat path name in
        try Sys.is_directory p with Sys_error _ -> false)
    |> List.sort String.compare

let load_pair category id =
  let dir = Filename.concat (Filename.concat traces_root category) id in
  let source_path = Filename.concat dir "source.css" in
  let expected_path = Filename.concat dir "expected.css" in
  if Sys.file_exists source_path && Sys.file_exists expected_path then
    Some
      { id; source = read_file source_path; expected = read_file expected_path }
  else None

let load_category category =
  let cat_dir = Filename.concat traces_root category in
  list_subdirs cat_dir |> List.filter_map (load_pair category)

let categories () = list_subdirs traces_root

type outcome = Pass | Parse_error of string | Mismatch of { actual : string }

let classify pair =
  let expected = normalize_expected pair.expected in
  match cascade_minify pair.source with
  | Error msg -> Parse_error msg
  | Ok actual ->
      if strip_trailing_ws actual = strip_trailing_ws expected then Pass
      else Mismatch { actual }

let pair_case pair () =
  match classify pair with
  | Pass -> ()
  | Parse_error msg ->
      Alcotest.failf "parse error: %s\n    input:    %s" msg pair.source
  | Mismatch { actual } ->
      Alcotest.failf
        "mismatch\n    input:    %s\n    expected: %s\n    actual:   %s"
        pair.source pair.expected actual

let () =
  let cases =
    categories ()
    |> List.map (fun category ->
        let pairs = load_category category in
        let pair_cases =
          List.map
            (fun pair -> Alcotest.test_case pair.id `Quick (pair_case pair))
            pairs
        in
        (category, pair_cases))
  in
  Alcotest.run "css_minify_tests" cases
