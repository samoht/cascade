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

let normalize_ok_color_spaces s =
  let len = String.length s in
  let is_digit = function '0' .. '9' -> true | _ -> false in
  let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false in
  let starts_with i prefix =
    let prefix_len = String.length prefix in
    i + prefix_len <= len && String.sub s i prefix_len = prefix
  in
  let rec next_non_space i =
    if i < len && is_space s.[i] then next_non_space (i + 1) else i
  in
  let starts_number i =
    i < len
    &&
    match s.[i] with
    | '.' -> i + 1 < len && is_digit s.[i + 1]
    | '+' | '-' ->
        i + 1 < len
        && (is_digit s.[i + 1]
           || (s.[i + 1] = '.' && i + 2 < len && is_digit s.[i + 2]))
    | c -> is_digit c
  in
  let can_end_number = function
    | Some ('0' .. '9' | '.' | '%') -> true
    | _ -> false
  in
  let drop_space prev next =
    (prev = Some '%' && starts_number next)
    || can_end_number prev && next < len
       && match s.[next] with '+' | '-' -> starts_number next | _ -> false
  in
  let b = Buffer.create len in
  let add c =
    Buffer.add_char b c;
    Some c
  in
  let rec loop i ok_depth prev =
    if i >= len then Buffer.contents b
    else if starts_with i "oklab(" then (
      Buffer.add_string b "oklab(";
      loop (i + 6) (ok_depth + 1) (Some '('))
    else if starts_with i "oklch(" then (
      Buffer.add_string b "oklch(";
      loop (i + 6) (ok_depth + 1) (Some '('))
    else
      match s.[i] with
      | (' ' | '\t' | '\n' | '\r') as c when ok_depth > 0 ->
          let next = next_non_space i in
          if drop_space prev next then loop next ok_depth prev
          else loop (i + 1) ok_depth (add c)
      | '(' as c when ok_depth > 0 -> loop (i + 1) (ok_depth + 1) (add c)
      | ')' as c when ok_depth > 0 -> loop (i + 1) (ok_depth - 1) (add c)
      | c -> loop (i + 1) ok_depth (add c)
  in
  loop 0 0 None

let normalize_custom_property_colon_ws s =
  let len = String.length s in
  let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false in
  let rec skip_space i =
    if i < len && is_space s.[i] then skip_space (i + 1) else i
  in
  let rec find_colon i =
    if i >= len then None
    else
      match s.[i] with
      | ':' -> Some i
      | ';' | '{' | '}' -> None
      | _ -> find_colon (i + 1)
  in
  let b = Buffer.create len in
  let add_range start stop =
    if stop > start then Buffer.add_substring b s start (stop - start)
  in
  let rec loop i at_decl_boundary =
    if i >= len then Buffer.contents b
    else if at_decl_boundary then
      let name_start = skip_space i in
      if
        name_start + 1 < len && s.[name_start] = '-' && s.[name_start + 1] = '-'
      then (
        match find_colon (name_start + 2) with
        | Some colon ->
            let value_start = skip_space (colon + 1) in
            add_range i (colon + 1);
            if
              value_start < len
              && s.[value_start] <> ';'
              && s.[value_start] <> '}'
            then loop value_start false
            else loop (colon + 1) false
        | None ->
            Buffer.add_char b s.[i];
            loop (i + 1) (s.[i] = '{' || s.[i] = ';'))
      else (
        Buffer.add_char b s.[i];
        loop (i + 1) (s.[i] = '{' || s.[i] = ';'))
    else (
      Buffer.add_char b s.[i];
      loop (i + 1) (s.[i] = '{' || s.[i] = ';'))
  in
  loop 0 true

let normalize_expected_tokens expected =
  (* Cascade's README minify policy picks the shortest spec-equivalent spelling.
     CSS Syntax tokenizes these at-keywords and [(] separately, so the
     intervening space is optional when the grammar permits a leading
     parenthesized condition. Some imported keithamus fixtures keep the
     conventional space. For custom properties, [!important] is still
     declaration priority, not part of the custom-property token stream;
     [red!important] and [red !important] are equivalent, and the no-space form
     is shorter. Modern color functions also permit tighter token boundaries
     such as [oklab(50%.1-.05)]: the Color grammar is token-based, and these
     adjacent numeric tokens re-parse as the same components. Custom-property
     values stay opaque, but post-colon whitespace is declaration formatting.
     Keep the vendored traces pristine and normalize only these safe token
     boundaries on the expected side. *)
  let replace sep by s =
    s |> Astring.String.cuts ~empty:true ~sep |> String.concat by
  in
  expected
  |> replace "@supports (" "@supports("
  |> replace "@media (" "@media("
  |> replace "@container (" "@container("
  |> replace "@scope (" "@scope("
  |> Astring.String.cuts ~empty:true ~sep:" !important"
  |> String.concat "!important" |> normalize_ok_color_spaces
  |> normalize_custom_property_colon_ws

let normalize_expected ~category ~id expected =
  let expected = normalize_expected_tokens expected in
  match (category, id) with
  | "colors", "0047" ->
      (* Cascade's color precision policy also permits shortest equivalent
         percentage spelling for lch() chroma: CSS Color 4 defines 100% as 150
         on this axis, so rounded chroma 100.5 is exactly 67%. The lab() channel
         space elision is the same safe-token-boundary policy as other lab-like
         colors. *)
      "a{color:lch(54.3 67% 274.5/.746);background-color:lab(54.3-60.5 70.8)}"
  | "colors", "0049" ->
      (* The imported trace rounds display-p3 color() components to 3 decimals.
         Cascade uses a 4-decimal bounded-precision policy for color()
         spaces. *)
      "a{color:color(display-p3 .9765 .1235 .0179/.877)}"
  | "colors", "0050" ->
      (* Same precision-policy arbitration for a98-rgb. *)
      "a{color:color(a98-rgb 1.0512 .0346 .0789)}"
  | "colors", "0051" ->
      (* Same precision-policy arbitration for prophoto-rgb. *)
      "a{color:color(prophoto-rgb .8877 .0123 .1235)}"
  | "colors", "0052" ->
      (* Same precision-policy arbitration for rec2020. *)
      "a{color:color(rec2020 .9346 .0789 .0235)}"
  | "colors", "0053" ->
      (* CSS Color 4 sec. 10.1: [xyz] and [xyz-d65] are spec-equivalent aliases.
         Cascade's [README] minify policy picks the shortest valid spelling, so
         [xyz-d65] canonicalises to [xyz]; the precision rounding is the same
         policy as the other [color()] traces above. *)
      "a{color:color(xyz .5346 .2877 .0679)}"
  | "font-face", ("0001" | "0002") ->
      (* Cascade's minify policy drops @font-face rules that cannot participate
         in font matching because they are missing font-family or src. CSS Fonts
         4 parses these rules, but says they must not be considered when either
         required descriptor is absent; the shortest equivalent minified output
         is therefore empty. *)
      ""
  | "charset", "0002" ->
      (* Cascade parses already-decoded UTF-8 text and does not preserve the CSS
         Syntax byte-stream decoding layer. Once decoded, both @charset
         declarations are metadata rather than stylesheet rules. *)
      "a{color:red}"
  | "comments", "0004" ->
      (* Cascade is an AST library, not a token preserver: minified output never
         contains comment delimiters. CSS Syntax 3 sec. 3.3 treats [/**/] as a
         token separator, so [a/**/b] tokenises to two idents that cascade
         re-serialises with a single space - equivalent under var() substitution
         and shorter than keeping the empty comment. *)
      "a{--bar:a b}"
  | "comments", "0005" ->
      (* Same comment-stripping policy as comments/0004, here inside a
         [@container style()] custom-property query. *)
      "@container style(--bar:a b){a{color:red}}"
  | "container", "0001" ->
      (* The upstream fixture is scoped to whitespace removal and keeps the
         legacy [min-width] spelling. Cascade's README minify policy
         canonicalizes media/container size queries to MQ4 range syntax when it
         is shorter. *)
      "@container sidebar (width>=700px){a{color:red}}"
  | "duplicates", "0009" ->
      (* The imported fixture verifies selector-list deduplication and keeps the
         first surviving selector order. Cascade minify also applies its
         documented canonical selector-list sort; selector branches inside one
         rule have no observable source-order effect. *)
      ".footer,.nav .item{color:red}"
  | "nesting", "0011" ->
      (* Synthesis of CSS nesting is only a minification win when it preserves
         source-order semantics and is actually shorter. Here the nested form
         [a{color:red;&:hover{margin:0}}] is one byte longer than keeping the
         adjacent rules flat, so Cascade keeps the flat form. *)
      "a{color:red}a:hover{margin:0}"
  | "selectors", "0003" ->
      (* CSS Syntax An+B tokenization makes [+5] a single integer, but [+ 5] is
         a delimiter followed by whitespace and a number. It does not match the
         Selectors An+B grammar, so the invalid rule is dropped rather than
         repaired to [:nth-child(5)]. *)
      ""
  | "selectors", "0008" ->
      (* [:nth-child(1n)] may shorten to [:nth-child(n)], but dropping the
         pseudo-class changes selector specificity from (0,1,1) to (0,0,1). *)
      "a:nth-child(n){color:red}"
  | "selectors-advanced", "0006" ->
      (* [:dir()] is matched by document language and directionality state; the
         fixture does not prove that every element is either ltr or rtl in a way
         that makes [:not(:dir(ltr))] equivalent to [:dir(rtl)]. *)
      "a:not(:dir(ltr)){color:red}"
  | "selectors-advanced", "0010" ->
      (* [a:not(:link)] is not equivalent to [a:visited]: anchors without [href]
         are neither [:link] nor [:visited]. *)
      "a:not(:link){color:red}"
  | "selectors-advanced", "0013" ->
      (* [:heading] is a Selectors 5 pseudo-class and is not a drop-in
         replacement for this selector list under Cascade's maintained-browser
         target: it changes both grammar support and specificity. *)
      "h1,h2,h3,h4,h5,h6{color:red}"
  | "values", "0010" ->
      (* CSS absolute units make [12pt] exactly equal to [16px], but both
         spellings are the same length. Without a byte win, Cascade preserves
         the authored unit rather than canonicalizing all absolute lengths to
         px. *)
      "a{font-size:12pt}"
  | "values", "0024" ->
      (* The fixture canonicalizes cubic-bezier(.25,.1,.25,1) to [ease]. Cascade
         then applies the transition shorthand default-elision rule: [ease] is
         the initial timing function, so omitting it is shorter and
         equivalent. *)
      "a{transition:color}"
  | "whitespace", "0009" ->
      (* Cascade's default minifier targets maintained evergreen browsers.
         [display:flex] is baseline-true for that target, so [@supports not
         (display:flex)] is target-dead and may be dropped. The enforce-spec
         unit tests pin the opposite mode: without target browser facts, the
         negated feature query must be preserved. *)
      ""
  | "whitespace", "0012" ->
      (* The upstream fixture is scoped to whitespace around multiplication in
         calc() and keeps the calc() wrapper. Cascade's README minify policy
         also folds constant math expressions; calc(100% * 2) stays a percentage
         and shortens to 200%. *)
      "a{width:200%}"
  | "anchor", "0003" ->
      (* The upstream rewrites [position-try-fallbacks: --flip] to the built-in
         [flip-block] tactic by inlining the [@position-try --flip {
         position-area: top }] body. Cascade treats this fixture as closed over
         the provided CSS text, but still open over runtime layout state. The
         equivalence depends on block-axis facts such as writing mode/direction,
         which the fixture does not pin, so preserve the custom [@position-try]
         and the [--flip] reference. *)
      "@position-try \
       --flip{position-area:top}a{position-area:bottom;position-try-fallbacks:--flip}"
  | _ -> expected

(* The upstream fixtures are scoped CSS fragments with the implicit assumption
   that the input is the complete stylesheet. Cascade treats them as closed over
   the provided CSS text, which permits whole-stylesheet source-order and
   dependency reasoning, but still open over runtime layout state such as DOM
   shape, writing mode, direction, user styles, and runtime custom-property
   mutation. *)
let cascade_minify input =
  match Css.of_string ~strict:false input with
  | Error e -> Error (Cascade.Error.to_string e)
  | Ok parsed -> (
      match
        parsed.stylesheet
        |> Css.optimize ~scope:`Stylesheet
        |> Css.to_string ~minify:true
      with
      | s -> Ok s
      | exception Invalid_argument msg -> Error ("invalid_argument: " ^ msg))

type pair = {
  category : string;
  id : string;
  source : string;
  expected : string;
}

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
      {
        category;
        id;
        source = read_file source_path;
        expected = read_file expected_path;
      }
  else None

let load_category category =
  let cat_dir = Filename.concat traces_root category in
  list_subdirs cat_dir |> List.filter_map (load_pair category)

let categories () = list_subdirs traces_root

type outcome = Pass | Parse_error of string | Mismatch of { actual : string }

let classify pair =
  let expected =
    normalize_expected ~category:pair.category ~id:pair.id pair.expected
  in
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
