(** Fuzz tests for the CSS Selector module.

    Tests crash safety of selector parsing and roundtrip consistency. *)

open Cascade
open Alcobar

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte_at buf i mod List.length xs)

let matches_literal s literal =
  let re = Re.(compile (str literal)) in
  Re.execp re s

let parse_selector s =
  try Some (Css.Selector.of_string s)
  with Css.Cursor.Parse_error _ | Invalid_argument _ -> None

let minified sel = Css.Selector.to_string ~minify:true sel

let assert_stable input =
  match parse_selector input with
  | None -> fail (Fmt.str "selector should parse: %S" input)
  | Some sel -> (
      let output = minified sel in
      match parse_selector output with
      | None -> fail (Fmt.str "minified selector did not reparse: %S" output)
      | Some reparsed ->
          let before = Css.Selector.specificity sel in
          let after = Css.Selector.specificity reparsed in
          if before <> after then
            fail (Fmt.str "specificity changed: %S -> %S" input output))

let assert_reject input =
  match parse_selector input with
  | None -> ()
  | Some sel ->
      fail (Fmt.str "selector should reject: %S -> %S" input (minified sel))

(** Selector.of_string — must not crash on arbitrary input. *)
let test_of_string buf =
  try ignore (Css.Selector.of_string buf) with
  | Css.Cursor.Parse_error _ -> ()
  | Invalid_argument _ -> ()

(** Selector.read — must not crash. *)
let test_read buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Selector.read r) with Css.Cursor.Parse_error _ -> ()

(** Selector.read_selector_list — must not crash. *)
let test_read_selector_list buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Selector.read_selector_list r)
  with Css.Cursor.Parse_error _ -> ()

(** Selector.read_combinator — must not crash. *)
let test_read_combinator buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Selector.read_combinator r)
  with Css.Cursor.Parse_error _ -> ()

(** Selector.read_attribute_match — must not crash. *)
let test_read_attribute_match buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Selector.read_attribute_match r)
  with Css.Cursor.Parse_error _ -> ()

(** Selector.read_nth — must not crash. *)
let test_read_nth buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Selector.read_nth r) with Css.Cursor.Parse_error _ -> ()

(** Roundtrip: parse → to_string → parse should not crash. *)
let test_roundtrip buf =
  match
    try Some (Css.Selector.of_string buf)
    with Css.Cursor.Parse_error _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some sel -> (
      let s = Css.Selector.to_string sel in
      try ignore (Css.Selector.of_string s)
      with Css.Cursor.Parse_error _ | Invalid_argument _ ->
        fail "roundtrip re-parse crashed")

(** pp — must not crash on any parsed selector. *)
let test_pp buf =
  match
    try Some (Css.Selector.of_string buf)
    with Css.Cursor.Parse_error _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some sel -> ignore (Css.Selector.to_string sel)

(** Selectors specificity is structural and must be stable across
    parse/serialize/reparse for accepted selectors. *)
let test_specificity_roundtrip buf =
  match
    try Some (Css.Selector.of_string buf)
    with Css.Cursor.Parse_error _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some sel -> (
      let serialized = Css.Selector.to_string ~minify:true sel in
      match
        try Some (Css.Selector.of_string serialized)
        with Css.Cursor.Parse_error _ | Invalid_argument _ -> None
      with
      | None -> fail "specificity roundtrip serialization did not reparse"
      | Some reparsed ->
          let before = Css.Selector.specificity sel in
          let after = Css.Selector.specificity reparsed in
          if before <> after then
            fail
              (Fmt.str "specificity changed across serialization: %S -> %S" buf
                 serialized))

let test_serialization_idempotent buf =
  match
    try Some (Css.Selector.of_string buf)
    with Css.Cursor.Parse_error _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some sel ->
      let once = Css.Selector.to_string ~minify:true sel in
      let twice = Css.Selector.(once |> of_string |> to_string ~minify:true) in
      if once <> twice then
        fail (Fmt.str "selector serialization drifted: %S -> %S" once twice)

let test_selector_list_serialization_idempotent buf =
  let r = Css.Cursor.of_string buf in
  match
    try Some (Css.Selector.read_selector_list r)
    with Css.Cursor.Parse_error _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some selectors ->
      let once = Css.Selector.to_string ~minify:true selectors in
      let r2 = Css.Cursor.of_string once in
      let selectors2 = Css.Selector.read_selector_list r2 in
      let twice = Css.Selector.to_string ~minify:true selectors2 in
      if once <> twice then fail "selector-list serialization drifted"

let test_noisy_forgiving buf =
  let wrapper = pick [ ":is"; ":where" ] buf 0 in
  let first = pick [ ".ok"; "#main"; "section"; "[data-state=--open]" ] buf 1 in
  let noisy =
    pick [ ":future-pseudo"; "::before"; "[=bad]"; ".a::before.class" ] buf 2
  in
  let last = pick [ ".fallback"; ":where(.zero)"; "[role=button]" ] buf 3 in
  let input = Fmt.str "%s(%s,%s,%s)" wrapper first noisy last in
  match parse_selector input with
  | None -> fail (Fmt.str "forgiving selector should parse: %S" input)
  | Some sel ->
      let output = minified sel in
      if
        matches_literal output "future-pseudo"
        || matches_literal output "before"
        || matches_literal output "[=bad]"
      then fail (Fmt.str "invalid forgiving branch survived: %S" output);
      assert_stable output

let test_empty_forgiving buf =
  let wrapper = pick [ ":is"; ":where" ] buf 0 in
  assert_stable (wrapper ^ "()");
  assert_stable (wrapper ^ "(,)");
  assert_stable (wrapper ^ "(,:future-pseudo,::before)")

let test_noisy_unforgiving buf =
  let noisy =
    pick
      [
        ".ok,:future-pseudo";
        ":not(.ok,:future-pseudo)";
        ".card:has(> img,:future-pseudo)";
        ".card:has(:has(img))";
        ".card:has(::before)";
        ".a::before.class";
        ".a::before::marker";
      ]
      buf 0
  in
  assert_reject noisy

let test_has_relative buf =
  let input =
    pick
      [
        ".card:has(> img)";
        ".card:has(+ .summary)";
        ".card:has(~ .summary)";
        "section:has(:scope > h2)";
        ".card:has(> :is(img,picture))";
      ]
      buf 0
  in
  assert_stable input

let test_attr_flags buf =
  let input =
    pick
      [
        "[data-x=--value]";
        "[data-x=\"--value\"]";
        "[type=button I]";
        "[class=\"My Class\" S]";
        "[svg|href=\"--icon\" I]";
        "|a";
        "|*";
      ]
      buf 0
  in
  match parse_selector input with
  | None -> fail (Fmt.str "attribute/namespace selector should parse: %S" input)
  | Some sel ->
      let output = minified sel in
      if matches_literal output " I]" || matches_literal output " S]" then
        fail (Fmt.str "attribute flag should be ASCII-lowercase: %S" output);
      assert_stable output

let test_where_specificity_zero buf =
  let branch =
    pick
      [
        "#id";
        ".class[attr=value]";
        "section > h1.title";
        ":is(#id,.class,article)";
        ".card:has(> img.selected)";
      ]
      buf 0
  in
  match parse_selector (":where(" ^ branch ^ ")") with
  | None -> fail (Fmt.str ":where() branch should parse: %S" branch)
  | Some sel ->
      let specificity = Css.Selector.specificity sel in
      if
        specificity.ids <> 0 || specificity.classes <> 0
        || specificity.elements <> 0
      then
        fail
          (Fmt.str ":where() specificity was not zero after parsing: %S" branch)

let test_selector_specificity_preserved_by_minify buf =
  let input =
    pick
      [
        ":is(main,.a,#id)";
        ":not(main,.a,#id)";
        ".card:has(> img.selected)";
        "li:nth-child(2n+1 of .visible:not([hidden]))";
        "article :is(h1,h2,h3):not(.muted)";
      ]
      buf 0
  in
  match parse_selector input with
  | None -> fail (Fmt.str "specificity vector should parse: %S" input)
  | Some sel -> (
      let serialized = minified sel in
      match parse_selector serialized with
      | None ->
          fail
            (Fmt.str "specificity vector did not reparse after minify: %S"
               serialized)
      | Some reparsed ->
          if Css.Selector.specificity sel <> Css.Selector.specificity reparsed
          then
            fail
              (Fmt.str "selector minification changed specificity: %S -> %S"
                 input serialized))

let suite =
  ( "selector",
    [
      test_case "of_string crash safety" [ bytes ] test_of_string;
      test_case "read crash safety" [ bytes ] test_read;
      test_case "read_selector_list crash safety" [ bytes ]
        test_read_selector_list;
      test_case "read_combinator crash safety" [ bytes ] test_read_combinator;
      test_case "read_attribute_match crash safety" [ bytes ]
        test_read_attribute_match;
      test_case "read_nth crash safety" [ bytes ] test_read_nth;
      test_case "roundtrip" [ bytes ] test_roundtrip;
      test_case "pp crash safety" [ bytes ] test_pp;
      test_case "specificity roundtrip" [ bytes ] test_specificity_roundtrip;
      test_case "serialization idempotent" [ bytes ]
        test_serialization_idempotent;
      test_case "selector list serialization idempotent" [ bytes ]
        test_selector_list_serialization_idempotent;
      test_case "forgiving noisy branches" [ bytes ] test_noisy_forgiving;
      test_case "empty forgiving branches" [ bytes ] test_empty_forgiving;
      test_case "unforgiving noisy branches" [ bytes ] test_noisy_unforgiving;
      test_case "relative has selectors" [ bytes ] test_has_relative;
      test_case "attribute flags and namespaces" [ bytes ] test_attr_flags;
      test_case "where specificity zero invariant" [ bytes ]
        test_where_specificity_zero;
      test_case "selector specificity preserved by minify" [ bytes ]
        test_selector_specificity_preserved_by_minify;
    ] )
