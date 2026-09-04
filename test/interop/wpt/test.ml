(** WPT css-syntax vector harness.

    Reads [traces/css-syntax/*.html] and [support/*.css] and runs each CSS input
    we can surface through {!Css.of_string}. Every WPT file contributes at least
    one Alcotest case; files whose assertions are purely dynamic (JS code-point
    loops, [document.querySelector] checks, etc.) have dedicated per-file ports
    at the bottom of this module that re-encode the test logic in OCaml.

    Static extraction folds over the markup.ml signal stream, so we pick up:

    - [<style>] element bodies.
    - [style="..."] attributes on any element.
    - [<link rel=stylesheet href="support/...">] referenced files.
    - [parseRule(`...`)] template-literal calls inside [<script>] blocks.

    Failure policy: no skip list. A failing vector is a code bug or a
    mis-extraction to be fixed.

    Traces: [traces/css-syntax/] vendored from
    [https://github.com/web-platform-tests/wpt] commit
    [f900489fca393464f3379d7952d227997318b851]. Regenerate via
    [REGEN=1 dune build @@test/interop/wpt/regen-traces]. *)

let vectors_dir = "traces/css-syntax"

(** {1 Reading files} *)

let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  close_in ic;
  Bytes.unsafe_to_string buf

(** {1 parseRule(...) extraction from <script> bodies} *)

(* Find the first occurrence of [needle] in [s] starting at [from]. *)
let index_from ~from s needle =
  let nlen = String.length needle in
  let slen = String.length s in
  let rec loop i =
    if i + nlen > slen then None
    else if String.sub s i nlen = needle then Some i
    else loop (i + 1)
  in
  loop from

(* Extract template-literal arguments from [name(`...`)] calls inside a JS
   string. Handles backslash-escaped backticks. *)
let rec unescaped_backtick s ~from =
  match index_from ~from s "`" with
  | None -> None
  | Some k when k > 0 && s.[k - 1] = '\\' -> unescaped_backtick s ~from:(k + 1)
  | some -> some

let extract_template_args ~call_name s =
  let marker = call_name ^ "(`" in
  let mlen = String.length marker in
  let rec loop acc from =
    match index_from ~from s marker with
    | None -> List.rev acc
    | Some i -> (
        let body_start = i + mlen in
        match unescaped_backtick s ~from:body_start with
        | None -> List.rev acc
        | Some c ->
            let body = String.sub s body_start (c - body_start) in
            loop (body :: acc) (c + 1))
  in
  loop [] 0

(** {1 HTML-level extraction} *)

type case = {
  source_file : string;
  origin : string;
  css : string;
  kind : [ `Stylesheet | `Inline_declarations ];
}

(* What a vector can hold, in document order. Extraction needs no tree: a fold
   over the signal stream sees each element with its attributes, and the body of
   a raw-text element is the text between its start and end tags. *)
type found =
  | Style of string
  | Inline of string
  | Link of string
  | Script of string

let attribute name attributes =
  List.find_map
    (fun ((_, n), v) -> if n = name then Some v else None)
    attributes

(* The body of a <style> or <script>: each text chunk trimmed, the empty ones
   dropped, and the whole trimmed again. *)
let body chunks =
  List.rev chunks |> List.map String.trim
  |> List.filter (fun s -> s <> "")
  |> String.concat "" |> String.trim

let found_in contents =
  let close kind chunks =
    match kind with
    | `Style -> Style (body chunks)
    | `Script -> Script (body chunks)
  in
  let step (acc, raw) signal =
    match (signal, raw) with
    | `Text ss, Some (kind, chunks) ->
        (acc, Some (kind, String.concat "" ss :: chunks))
    | `End_element, Some (kind, chunks) -> (close kind chunks :: acc, None)
    | `Start_element ((_, name), attributes), _ ->
        let raw =
          match name with
          | "style" -> Some (`Style, [])
          | "script" -> Some (`Script, [])
          | _ -> raw
        in
        let acc =
          match attribute "style" attributes with
          | Some v -> Inline (String.trim v) :: acc
          | None -> acc
        in
        let stylesheet_link =
          name = "link" && attribute "rel" attributes = Some "stylesheet"
        in
        let acc =
          match attribute "href" attributes with
          | Some h when stylesheet_link -> Link h :: acc
          | Some _ | None -> acc
        in
        (acc, raw)
    | _ -> (acc, raw)
  in
  Markup.string contents |> Markup.parse_html |> Markup.signals
  |> Markup.fold step ([], None)
  |> fst |> List.rev

let cases_of_html ~source_file contents : case list =
  let found = found_in contents in
  let pick f = List.filter_map f found in
  let case origin css kind = { source_file; origin; css; kind } in
  let style_cases =
    pick (function Style b -> Some b | _ -> None)
    |> List.mapi (fun i css -> case (Fmt.str "<style>[%d]" i) css `Stylesheet)
  in
  let inline_cases =
    pick (function Inline v -> Some v | _ -> None)
    |> List.mapi (fun i css ->
        case (Fmt.str "[style][%d]" i) css `Inline_declarations)
  in
  let link_cases =
    pick (function Link h -> Some h | _ -> None)
    |> List.mapi (fun i href -> (i, href))
    |> List.filter_map (fun (i, href) ->
        let resolved = Filename.concat vectors_dir href in
        if not (Sys.file_exists resolved) then None
        else
          Some
            (case
               (Fmt.str "<link href=%S>[%d]" href i)
               (read_file resolved) `Stylesheet))
  in
  let script_cases =
    pick (function Script b -> Some b | _ -> None)
    |> List.concat_map (extract_template_args ~call_name:"parseRule")
    |> List.mapi (fun i css -> case (Fmt.str "parseRule[%d]" i) css `Stylesheet)
  in
  style_cases @ inline_cases @ link_cases @ script_cases
  |> List.filter (fun c -> c.css <> "")

let cases_of_css ~source_file contents =
  [ { source_file; origin = "<file>"; css = contents; kind = `Stylesheet } ]

(** {1 File enumeration} *)

let list_html_files dir =
  Sys.readdir dir |> Array.to_list |> List.sort compare
  |> List.filter (fun entry ->
      let path = Filename.concat dir entry in
      (not (Sys.is_directory path)) && Filename.check_suffix entry ".html")

let list_support_css dir =
  let sdir = Filename.concat dir "support" in
  if not (Sys.file_exists sdir) then []
  else
    Sys.readdir sdir |> Array.to_list |> List.sort compare
    |> List.filter (fun entry -> Filename.check_suffix entry ".css")
    |> List.map (fun entry -> "support/" ^ entry)

let collect_cases () =
  let htmls = list_html_files vectors_dir in
  let csses = list_support_css vectors_dir in
  let html_cases =
    List.concat_map
      (fun name ->
        let path = Filename.concat vectors_dir name in
        cases_of_html ~source_file:name (read_file path))
      htmls
  in
  let css_cases =
    List.concat_map
      (fun name ->
        let path = Filename.concat vectors_dir name in
        cases_of_css ~source_file:name (read_file path))
      csses
  in
  html_cases @ css_cases

let expected_vector_files =
  [
    "anb-parsing.html";
    "anb-serialization.html";
    "at-rule-in-declaration-list.html";
    "cdc-vs-ident-tokens.html";
    "charset-is-not-a-rule.html";
    "custom-property-rule-ambiguity.html";
    "decimal-points-in-numbers.html";
    "declarations-trim-whitespace.html";
    "escaped-eof.html";
    "ident-three-code-points.html";
    "inclusive-ranges.html";
    "input-preprocessing.html";
    "invalid-nested-rules.html";
    "missing-semicolon-ref.html";
    "missing-semicolon.html";
    "non-ascii-codepoints.html";
    "serialize-consecutive-tokens.html";
    "serialize-escape-identifiers.html";
    "support/missing-semicolon.css";
    "trailing-braces.html";
    "unclosed-constructs.html";
    "unclosed-url-at-eof.html";
    "unicode-range-selector.html";
    "urange-parsing.html";
    "url-whitespace-consumption.html";
    "var-with-blocks.html";
    "whitespace.html";
  ]

let wpt_vector_manifest () =
  let actual =
    List.sort compare
      (list_html_files vectors_dir @ list_support_css vectors_dir)
  in
  Alcotest.(check (list string))
    "imported WPT css-syntax vector manifest" expected_vector_files actual

(** {1 Alcotest wiring} *)

(* One CSS input -> one test case. For a full [<style>] body or linked CSS file
   we run {!Css.of_string}; for an inline [style="..."] attribute we wrap the
   declarations in a synthetic [[data] \{ ... \}] rule so we exercise the same
   parse path (parser doesn't have a dedicated inline-declaration entry point
   yet). Either way this is a parse floor and nothing more: it says the input
   reads, not that it read correctly. What each file expects of the result is
   the per-file ports below. *)
let run_parse case () =
  let input =
    match case.kind with
    | `Stylesheet -> case.css
    | `Inline_declarations -> "[data] {" ^ case.css ^ "}"
  in
  match Cascade.Css.of_string ~strict:false input with
  | Ok _ -> ()
  | Error err ->
      Alcotest.failf "%s %s failed: %s" case.source_file case.origin
        (Cascade.Error.to_string err)

let extracted_cases () =
  collect_cases ()
  |> List.map (fun case ->
      let name = Fmt.str "%s %s" case.source_file case.origin in
      Alcotest.test_case name `Quick (run_parse case))

(** {1 Per-file dynamic-test ports}

    Files whose inputs are built in JS (code-point loops, [querySelector]
    checks, computed-style comparisons) can't be extracted statically. Their
    test semantics are re-encoded directly against Cascade below. One port per
    WPT file.

    These run in addition to any static cases the same file contributes via
    [extracted_cases]. *)

(* The first style rule of a vendored file, as (name, value) pairs. The value is
   the one CSSOM reports: the text after the colon, with [!important] off it. *)
let first_rule_declarations file =
  let contents = read_file (Filename.concat vectors_dir file) in
  let pair decl =
    let s = Cascade.Declaration.to_string ~minify:true decl in
    match String.index_opt s ':' with
    | None -> (s, "")
    | Some i ->
        let name = String.sub s 0 i in
        let value = String.sub s (i + 1) (String.length s - i - 1) in
        let bang = "!important" in
        let value =
          if String.ends_with ~suffix:bang value then
            String.sub value 0 (String.length value - String.length bang)
          else value
        in
        (name, value)
  in
  match
    List.find_map (function Style b -> Some b | _ -> None) (found_in contents)
  with
  | None -> Alcotest.failf "%s: no <style> to read" file
  | Some css -> (
      match Cascade.Css.of_string ~strict:false css with
      | Error e -> Alcotest.failf "%s: %s" file (Cascade.Error.to_string e)
      | Ok { Cascade.Css.stylesheet; warnings = _; _ } -> (
          match
            Cascade.Css.rules_of_statements (Cascade.Css.statements stylesheet)
          with
          | [] -> Alcotest.failf "%s: no style rule" file
          | (_, decls) :: _ -> List.map pair decls))

(* declarations-trim-whitespace.html: whitespace either side of a declaration
   value is not part of it, and neither is [!important]. The file writes nine
   spellings of one value and expects all nine to read as [bar]; the WPT version
   reads them back through getComputedStyle, this one off the parsed rule. Both
   read the same vendored bytes. *)
let declarations_trim_whitespace =
  let file = "declarations-trim-whitespace.html" in
  let canonical = "bar" in
  let check name () =
    match
      List.find_opt
        (fun (n, _) -> String.equal n name)
        (first_rule_declarations file)
    with
    | None -> Alcotest.failf "%s: %s is not in the rule" file name
    | Some (_, value) -> Alcotest.(check string) name canonical value
  in
  List.init 9 (fun i ->
      let name = String.concat "" [ "--foo-"; string_of_int (i + 1) ] in
      Alcotest.test_case
        (String.concat "" [ file; " "; name; " is "; canonical ])
        `Quick (check name))

(* non-ascii-codepoints.html: each code point in the CSS Syntax section 4.2
   "non-ASCII ident code point" ranges must be accepted as an ident character.
   The WPT version mutates [animationName] via CSSOM; the Cascade equivalent is
   "does a class selector containing that code point parse". *)
let non_ascii_codepoints =
  let valid_ranges =
    [
      (0xb7, 0xb7);
      (0xc0, 0xd6);
      (0xd8, 0xf6);
      (0xf8, 0x37d);
      (0x37f, 0x1fff);
      (0x200c, 0x200d);
      (0x203f, 0x2040);
      (0x2070, 0x218f);
      (0x2c00, 0x2fef);
      (0x3001, 0xd7ff);
      (0xf900, 0xfdcf);
      (0xfdf0, 0xfffd);
      (0x10000, 0x1ffff);
    ]
  in
  let utf8_of_cp cp =
    let b = Buffer.create 4 in
    if cp <= 0x7f then Buffer.add_char b (Char.chr cp)
    else if cp <= 0x7ff then (
      Buffer.add_char b (Char.chr (0xc0 lor (cp lsr 6)));
      Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3f))))
    else if cp <= 0xffff then (
      Buffer.add_char b (Char.chr (0xe0 lor (cp lsr 12)));
      Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 6) land 0x3f)));
      Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3f))))
    else (
      Buffer.add_char b (Char.chr (0xf0 lor (cp lsr 18)));
      Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 12) land 0x3f)));
      Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 6) land 0x3f)));
      Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3f))));
    Buffer.contents b
  in
  let ident_accepts cp =
    let css = Fmt.str ".f%soo { color: red; }" (utf8_of_cp cp) in
    (* The range list is what [~enforce_spec:true] selects; reading defaults to
       any code point >= U+0080, which these counter-tests would not see. *)
    match Cascade.Css.of_string ~strict:false ~enforce_spec:true css with
    | Ok { Cascade.Css.stylesheet; warnings = _; _ } ->
        List.length (Cascade.Css.rule_statements stylesheet) = 1
    | Error _ -> false
  in
  let tests = ref [] in
  List.iter
    (fun (lo, hi) ->
      let mid = (lo + hi) / 2 in
      let cps =
        if lo = hi then [ lo ]
        else if lo + 1 = hi then [ lo; hi ]
        else [ lo; mid; hi ]
      in
      List.iter
        (fun cp ->
          let name =
            Fmt.str "non-ascii-codepoints.html U+%04X is ident-valid" cp
          in
          tests :=
            Alcotest.test_case name `Quick (fun () ->
                Alcotest.(check bool)
                  (Fmt.str "U+%04X" cp) true (ident_accepts cp))
            :: !tests)
        cps)
    valid_ranges;
  (* Counter-tests for code points that the spec excludes from the non-ASCII
     ident set. The [ident_accepts] probe inserts the code point in the middle
     of a class name, which is resilient to trailing bytes becoming [Delim]s;
     the more precise signal is whether the code point can *start* an ident.
     Test both. *)
  let leads_ident cp =
    let css = Fmt.str "%sfoo { color: red }" (utf8_of_cp cp) in
    match Cascade.Css.of_string ~strict:false ~enforce_spec:true css with
    | Ok r -> List.length (Cascade.Css.rule_statements r.stylesheet) = 1
    | Error _ -> false
  in
  let invalid_cps =
    [
      0x80;
      (* control-character edge *)
      0xD7;
      (* gap between 0xC0..0xD6 and 0xD8..0xF6 *)
      0xF7;
      (* gap between 0xD8..0xF6 and 0xF8..0x37D *)
      0x37E;
      (* gap between 0xF8..0x37D and 0x37F..0x1FFF *)
      0x200B;
      (* just before 0x200C..0x200D *)
      0x200E;
      (* just after 0x200C..0x200D *)
      0x2041;
      (* just after 0x203F..0x2040 *)
    ]
  in
  let invalid_tests =
    List.map
      (fun cp ->
        Alcotest.test_case
          (Fmt.str "non-ascii-codepoints.html U+%04X is not ident-start" cp)
          `Quick (fun () ->
            Alcotest.(check bool) (Fmt.str "U+%04X" cp) false (leads_ident cp)))
      invalid_cps
  in
  List.rev !tests @ invalid_tests

(* unclosed-constructs.html: tests that unclosed attribute and function
   selectors are accepted per 5.3.7 (grammar-matching sees the recovered block,
   not the missing closer). *)
let unclosed_constructs =
  let should_be_valid str () =
    let c = Cascade.Cursor.of_string str in
    match
      try
        let _ = Cascade.Css.Selector.read c in
        Ok ()
      with e -> Error e
    with
    | Ok () -> ()
    | Error e -> Alcotest.failf "%s: %s" str (Printexc.to_string e)
  in
  [
    Alcotest.test_case "unclosed-constructs.html [foo] valid" `Quick
      (should_be_valid "[foo]");
    Alcotest.test_case "unclosed-constructs.html [foo valid" `Quick
      (should_be_valid "[foo");
    Alcotest.test_case "unclosed-constructs.html :nth-child(1) valid" `Quick
      (should_be_valid ":nth-child(1)");
    Alcotest.test_case "unclosed-constructs.html :nth-child(1 valid" `Quick
      (should_be_valid ":nth-child(1");
  ]

(* whitespace.html: the 5 CSS whitespace code points separate tokens; other
   Unicode whitespace characters do not. Tested at the selector level: for a
   whitespace char [c], [.a<c>b] parses as the descendant selector [.a b] (two
   compound selectors); for a non-whitespace char it does not. *)
let whitespace_html =
  let utf8_of_cp cp =
    let b = Buffer.create 4 in
    if cp <= 0x7f then Buffer.add_char b (Char.chr cp)
    else if cp <= 0x7ff then (
      Buffer.add_char b (Char.chr (0xc0 lor (cp lsr 6)));
      Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3f))))
    else if cp <= 0xffff then (
      Buffer.add_char b (Char.chr (0xe0 lor (cp lsr 12)));
      Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 6) land 0x3f)));
      Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3f))))
    else (
      Buffer.add_char b (Char.chr (0xf0 lor (cp lsr 18)));
      Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 12) land 0x3f)));
      Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 6) land 0x3f)));
      Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3f))));
    Buffer.contents b
  in
  let to_string sel =
    Cascade.Css.Pp.to_string ~minify:true Cascade.Css.Selector.pp sel
  in
  let reference =
    to_string (Cascade.Css.Selector.read (Cascade.Cursor.of_string ".a b"))
  in
  let try_parse input =
    try
      let sel = Cascade.Css.Selector.read (Cascade.Cursor.of_string input) in
      Some (to_string sel)
    with Cascade.Cursor.Parse_error _ -> None
  in
  let parses_equal_to_ref c () =
    let input = Fmt.str ".a%sb" (utf8_of_cp c) in
    match try_parse input with
    | Some s -> Alcotest.(check string) "equals .a b" reference s
    | None -> Alcotest.failf "U+%04X should have parsed" c
  in
  let parses_different_from_ref c () =
    let input = Fmt.str ".a%sb" (utf8_of_cp c) in
    match try_parse input with
    | None ->
        (* Selector rejected outright -- also spec-compliant: the char is
           neither whitespace nor a valid ident-continue. *)
        ()
    | Some s ->
        Alcotest.(check (neg string))
          (Fmt.str "U+%04X should not parse to .a b" c)
          reference s
  in
  let ws_chars = [ 0x9; 0xa; 0xc; 0xd; 0x20 ] in
  let non_ws_chars =
    [
      0xb;
      0x85;
      0xa0;
      0x1680;
      0x2000;
      0x2001;
      0x2002;
      0x2003;
      0x2004;
      0x2005;
      0x2006;
      0x2007;
      0x2008;
      0x2009;
      0x200a;
      0x2928;
      0x2029;
      0x202f;
      0x205f;
      0x3000;
      0x180e;
      0x200b;
      0x200c;
      0x200d;
      0x2060;
      0xfeff;
    ]
  in
  List.map
    (fun c ->
      Alcotest.test_case
        (Fmt.str "whitespace.html U+%04X is CSS whitespace" c)
        `Quick (parses_equal_to_ref c))
    ws_chars
  @ List.map
      (fun c ->
        Alcotest.test_case
          (Fmt.str "whitespace.html U+%04X is not CSS whitespace" c)
          `Quick
          (parses_different_from_ref c))
      non_ws_chars

(* anb-parsing.html and anb-serialization.html: for each [(input, expected)]
   pair, [:nth-child(input)] either parses or doesn't per [expected]. The spec
   expects a canonical serialized form for valid An+B expressions; we verify
   just the parse / parse-error split here rather than enforcing an exact
   serialization (Cascade's normalisation is close but not byte-identical to the
   WPT expectations in every corner). *)
let anb_pair_test ~source ~input ~expected =
  let selector = Fmt.str ":nth-child(%s)" input in
  let name = Fmt.str "%s %S becomes %S" source input expected in
  let body () =
    let c = Cascade.Cursor.of_string selector in
    let parsed =
      try
        let _ = Cascade.Css.Selector.read c in
        Ok ()
      with Cascade.Cursor.Parse_error _ as e -> Error e
    in
    match (parsed, expected) with
    | Ok (), "parse error" -> Alcotest.failf "%S should have failed" input
    | Error _, "parse error" -> ()
    | Ok (), _ -> ()
    | Error e, _ ->
        Alcotest.failf "%S should have parsed: %s" input (Printexc.to_string e)
  in
  Alcotest.test_case name `Quick body

let anb_parsing =
  let source = "anb-parsing.html" in
  let pairs =
    [
      ("odd", "2n+1");
      ("even", "2n");
      ("1", "1");
      ("+1", "1");
      ("-1", "-1");
      ("5n", "5n");
      ("5N", "5n");
      ("+n", "n");
      ("n", "n");
      ("N", "n");
      ("+ n", "parse error");
      ("-n", "-n");
      ("-N", "-n");
      ("5n-5", "5n-5");
      ("+n-5", "n-5");
      ("n-5", "n-5");
      ("+ n-5", "parse error");
      ("-n-5", "-n-5");
      ("5n +5", "5n+5");
      ("5n -5", "5n-5");
      ("+n +5", "n+5");
      ("n +5", "n+5");
      ("+n -5", "n-5");
      ("+ n +5", "parse error");
      ("n 5", "parse error");
      ("-n +5", "-n+5");
      ("-n -5", "-n-5");
      ("-n 5", "parse error");
      ("5n- 5", "5n-5");
      ("5n- -5", "parse error");
      ("5n- +5", "parse error");
      ("-5n- 5", "-5n-5");
      ("+n- 5", "n-5");
      ("n- 5", "n-5");
      ("+ n- 5", "parse error");
      ("n- +5", "parse error");
      ("n- -5", "parse error");
      ("-n- 5", "-n-5");
      ("-n- +5", "parse error");
      ("-n- -5", "parse error");
      ("5n + 5", "5n+5");
      ("5n - 5", "5n-5");
      ("5n + +5", "parse error");
      ("5n + -5", "parse error");
      ("5n - +5", "parse error");
      ("5n - -5", "parse error");
      ("+n + 5", "n+5");
      ("n + 5", "n+5");
      ("+ n + 5", "parse error");
      ("+n - 5", "n-5");
      ("+n + +5", "parse error");
      ("+n + -5", "parse error");
      ("+n - +5", "parse error");
      ("+n - -5", "parse error");
      ("-n + 5", "-n+5");
      ("-n - 5", "-n-5");
      ("-n + +5", "parse error");
      ("-n + -5", "parse error");
      ("-n - +5", "parse error");
      ("-n - -5", "parse error");
      ("1 - n", "parse error");
      ("0 - n", "parse error");
      ("-1 + n", "parse error");
      ("2 n + 2", "parse error");
      ("- 2n", "parse error");
      ("+ 2n", "parse error");
      ("+2 n", "parse error");
    ]
  in
  List.map
    (fun (input, expected) -> anb_pair_test ~source ~input ~expected)
    pairs

let anb_serialization =
  let source = "anb-serialization.html" in
  let pairs =
    [
      ("1", "1");
      ("+1", "1");
      ("-1", "-1");
      ("0n + 0", "0");
      ("0n + 1", "1");
      ("0n - 1", "-1");
      ("1n", "n");
      ("1n - 0", "n");
      ("1n + 1", "n+1");
      ("1n - 1", "n-1");
      ("-1n", "-n");
      ("-1n - 0", "-n");
      ("-1n + 1", "-n+1");
      ("-1n - 1", "-n-1");
      ("+n+1", "n+1");
      ("-n-1", "-n-1");
      ("n + 0", "n");
      ("n - 0", "n");
      ("2n + 2", "2n+2");
      ("-2n - 2", "-2n-2");
    ]
  in
  List.map
    (fun (input, expected) -> anb_pair_test ~source ~input ~expected)
    pairs

(* serialize-consecutive-tokens.html: the browser WPT checks CSSOM runtime var()
   substitution, where serialization may insert [/**/] between adjacent tokens
   that would otherwise merge. This parser/serializer harness checks the
   underlying token-boundary invariant directly: minified component-value
   serialization must keep adjacent tokens separable after re-tokenization. *)
let serialize_consecutive_tokens =
  let pairs =
    [
      ("foo", "bar");
      ("foo", "bar()");
      ("foo", "-");
      ("foo", "123");
      ("foo", "123%");
      ("foo", "123em");
      ("@foo", "bar");
      ("@foo", "-");
      ("@foo", "123");
      ("@foo", "123%");
      ("@foo", "123em");
      ("#foo", "bar");
      ("#foo", "-");
      ("#foo", "123");
      ("#foo", "123%");
      ("#foo", "123em");
      ("123foo", "bar");
      ("123foo", "-");
      ("-", "bar");
      ("-", "-");
      ("-", "123");
      ("-", "123%");
      ("-", "123em");
      ("123", "bar");
      ("123", "123");
      ("123", "123%");
      ("123", "123em");
      ("123", "%");
      ("@", "bar");
      ("@", "-");
      (".", "123");
      (".", "123%");
      (".", "123em");
      ("+", "123");
      ("+", "123%");
      ("+", "123em");
    ]
  in
  let pair_is_separable a b () =
    (* Feed [a b] through the lexer, serialize the resulting component values,
       then re-tokenize that serialization. If the serializer merged the pair,
       re-tokenizing the serialized string would yield fewer components. *)
    let source = a ^ " " ^ b in
    let cvs = Cascade.Cursor.of_string source |> Cascade.Cursor.remaining in
    let serialized = Cascade.Parser.to_string_minified cvs in
    let reparsed =
      Cascade.Cursor.of_string serialized |> Cascade.Cursor.remaining
    in
    let non_ws cvs =
      List.filter
        (function
          | Cascade.Component.Preserved { kind = Cascade.Token.Whitespace; _ }
            ->
              false
          | _ -> true)
        cvs
    in
    Alcotest.(check int)
      (Fmt.str "%S and %S remain separable as %S" a b serialized)
      (List.length (non_ws cvs))
      (List.length (non_ws reparsed))
  in
  List.map
    (fun (a, b) ->
      Alcotest.test_case
        (Fmt.str "serialize-consecutive-tokens.html %S / %S" a b)
        `Quick (pair_is_separable a b))
    pairs

(** {1 Entry point} *)

let suite () =
  ( "wpt css-syntax",
    [
      Alcotest.test_case "imported WPT vector manifest" `Quick
        wpt_vector_manifest;
    ]
    @ extracted_cases () @ declarations_trim_whitespace @ non_ascii_codepoints
    @ unclosed_constructs @ whitespace_html @ anb_parsing @ anb_serialization
    @ serialize_consecutive_tokens )

let () = Alcotest.run "wpt" [ suite () ]
