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
  with Cursor.Parse_error _ | Invalid_argument _ -> None

let minified sel = Css.Selector.to_string ~minify:true sel

let assert_stable input =
  match parse_selector input with
  | None -> failf "selector should parse: %S" input
  | Some sel -> (
      let output = minified sel in
      match parse_selector output with
      | None -> failf "minified selector did not reparse: %S" output
      | Some reparsed ->
          let before = Css.Selector.specificity sel in
          let after = Css.Selector.specificity reparsed in
          if before <> after then
            failf "specificity changed: %S -> %S" input output)

let assert_reject input =
  match parse_selector input with
  | None -> ()
  | Some sel -> failf "selector should reject: %S -> %S" input (minified sel)

(** Selector.of_string — must not crash on arbitrary input. *)
let test_of_string buf =
  try ignore (Css.Selector.of_string buf) with
  | Cursor.Parse_error _ -> ()
  | Invalid_argument _ -> ()

(** Selector.read — must not crash. *)
let test_read buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Selector.read r) with Cursor.Parse_error _ -> ()

(** Selector.read_selector_list — must not crash. *)
let test_read_selector_list buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Selector.read_selector_list r)
  with Cursor.Parse_error _ -> ()

(** Selector.read_combinator — must not crash. *)
let test_read_combinator buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Selector.read_combinator r) with Cursor.Parse_error _ -> ()

(** Selector.read_attribute_match — must not crash. *)
let test_read_attribute_match buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Selector.read_attribute_match r)
  with Cursor.Parse_error _ -> ()

(** Selector.read_nth — must not crash. *)
let test_read_nth buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Selector.read_nth r) with Cursor.Parse_error _ -> ()

(** Roundtrip: parse → to_string → parse should not crash. *)
let test_roundtrip buf =
  match
    try Some (Css.Selector.of_string buf)
    with Cursor.Parse_error _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some sel -> (
      let s = Css.Selector.to_string sel in
      try ignore (Css.Selector.of_string s)
      with Cursor.Parse_error _ | Invalid_argument _ ->
        fail "roundtrip re-parse crashed")

(** pp — must not crash on any parsed selector. *)
let test_pp buf =
  match
    try Some (Css.Selector.of_string buf)
    with Cursor.Parse_error _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some sel -> ignore (Css.Selector.to_string sel)

(** Selectors specificity is structural and must be stable across
    parse/serialize/reparse for accepted selectors. *)
let test_specificity_roundtrip buf =
  match
    try Some (Css.Selector.of_string buf)
    with Cursor.Parse_error _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some sel -> (
      let serialized = Css.Selector.to_string ~minify:true sel in
      match
        try Some (Css.Selector.of_string serialized)
        with Cursor.Parse_error _ | Invalid_argument _ -> None
      with
      | None -> fail "specificity roundtrip serialization did not reparse"
      | Some reparsed ->
          let before = Css.Selector.specificity sel in
          let after = Css.Selector.specificity reparsed in
          if before <> after then
            failf "specificity changed across serialization: %S -> %S" buf
              serialized)

(* Allow one canonicalization pass (CSS Syntax 4.3.7 NUL -> U+FFFD, escape
   canonical form) that only fires on re-parse, then require fixed point.
   Initial garbage input may fail to parse (out of scope); once a parse succeeds
   the serializer must always emit reparseable output. *)
let parse_or_skip parse buf k =
  match
    try Some (parse buf)
    with Cursor.Parse_error _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some x -> k x

let reparse_or_fail parse step s =
  try parse s
  with (Cursor.Parse_error _ | Invalid_argument _) as exn ->
    failf "serializer emitted un-reparseable output at %s: %S (%s)" step s
      (Printexc.to_string exn)

let test_serialization_idempotent buf =
  parse_or_skip Css.Selector.of_string buf @@ fun sel ->
  let serialize s = Css.Selector.to_string ~minify:true s in
  let once = serialize sel in
  let twice =
    serialize (reparse_or_fail Css.Selector.of_string "first reparse" once)
  in
  let thrice =
    serialize (reparse_or_fail Css.Selector.of_string "second reparse" twice)
  in
  if twice <> thrice then
    failf "selector serialization drifted past canonicalization: %S -> %S -> %S"
      once twice thrice

let test_selector_list_serialization_idempotent buf =
  let serialize = Css.Selector.to_string ~minify:true in
  let parse s = Css.Selector.read_selector_list (Cursor.of_string s) in
  parse_or_skip parse buf @@ fun selectors ->
  let once = serialize selectors in
  let twice = serialize (reparse_or_fail parse "first reparse" once) in
  let thrice = serialize (reparse_or_fail parse "second reparse" twice) in
  if twice <> thrice then
    failf
      "selector-list serialization drifted past canonicalization: %S -> %S -> \
       %S"
      once twice thrice

(* canonicalize reaches a fixed point in one pass: it de-duplicates and sorts
   selector-list branches and drops redundant universals, all idempotent. A
   second pass is therefore a genuine no-op and must return the very same
   physical value. A structurally-equal reallocation means canonicalize rebuilds
   branches it never changed - wasted work that also breaks the sharing the
   optimizer relies on when it keeps an unchanged rule's selector by
   identity. *)
let test_canonicalize_preserves_physical_identity buf =
  parse_or_skip Css.Selector.of_string buf @@ fun sel ->
  let canon = Css.Selector.canonicalize sel in
  let again = Css.Selector.canonicalize canon in
  if again == canon then ()
  else if again = canon then
    fail
      "canonicalize reallocated an already-canonical selector instead of \
       returning it unchanged (x = canonicalize x but not x == canonicalize x)"
  else
    failf "canonicalize is not idempotent: %S -> %S" (minified canon)
      (minified again)

(* Same fixed-point identity contract on a selector list, the construct
   canonicalize sorts and de-duplicates. *)
let test_list_canon_identity buf =
  let parse s = Css.Selector.read_selector_list (Cursor.of_string s) in
  parse_or_skip parse buf @@ fun selectors ->
  let canon = Css.Selector.canonicalize selectors in
  let again = Css.Selector.canonicalize canon in
  if again == canon then ()
  else if again = canon then
    fail
      "canonicalize reallocated an already-canonical selector list instead of \
       returning it unchanged (x = canonicalize x but not x == canonicalize x)"
  else
    failf "canonicalize is not idempotent on a selector list: %S -> %S"
      (minified canon) (minified again)

let test_noisy_forgiving buf =
  let wrapper = pick [ ":is"; ":where" ] buf 0 in
  let first = pick [ ".ok"; "#main"; "section"; "[data-state=--open]" ] buf 1 in
  let noisy =
    pick [ ":future-pseudo"; "::before"; "[=bad]"; ".a::before.class" ] buf 2
  in
  let last = pick [ ".fallback"; ":where(.zero)"; "[role=button]" ] buf 3 in
  let input = Fmt.str "%s(%s,%s,%s)" wrapper first noisy last in
  match parse_selector input with
  | None -> failf "forgiving selector should parse: %S" input
  | Some sel ->
      let output = minified sel in
      if
        matches_literal output "future-pseudo"
        || matches_literal output "before"
        || matches_literal output "[=bad]"
      then failf "invalid forgiving branch survived: %S" output;
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
  | None -> failf "attribute/namespace selector should parse: %S" input
  | Some sel ->
      let output = minified sel in
      if matches_literal output " I]" || matches_literal output " S]" then
        failf "attribute flag should be ASCII-lowercase: %S" output;
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
  | None -> failf ":where() branch should parse: %S" branch
  | Some sel ->
      let specificity = Css.Selector.specificity sel in
      if
        specificity.ids <> 0 || specificity.classes <> 0
        || specificity.elements <> 0
      then failf ":where() specificity was not zero after parsing: %S" branch

let test_selector_specificity_minify buf =
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
  | None -> failf "specificity vector should parse: %S" input
  | Some sel -> (
      let serialized = minified sel in
      match parse_selector serialized with
      | None ->
          failf "specificity vector did not reparse after minify: %S" serialized
      | Some reparsed ->
          if Css.Selector.specificity sel <> Css.Selector.specificity reparsed
          then
            failf "selector minification changed specificity: %S -> %S" input
              serialized)

let test_pseudo_class_family_vectors buf =
  let input =
    pick
      [
        ":nth-child(-n+3)";
        ":nth-last-child(odd of :not([hidden]))";
        ":lang(en, fr)";
        ":dir(ltr)";
        ":autofill";
        ":placeholder-shown";
        ":user-valid";
        ":user-invalid";
        ":active-view-transition-type(forwards, backwards)";
        ":state(selected)";
      ]
      buf 0
  in
  assert_stable input

let test_pseudo_element_family_vectors buf =
  let input =
    pick
      [
        "::target-text";
        "::spelling-error";
        "::grammar-error";
        "::part(tab panel)";
        "::slotted(img.selected)";
        "::highlight(search-results)";
        "::view-transition-group(root)";
        "::view-transition-image-pair(root)";
        "::view-transition-old(root)";
        "::view-transition-new(root)";
      ]
      buf 0
  in
  assert_stable input

let test_invalid_pseudo_family_vectors buf =
  let input =
    pick
      [
        ":nth-child(n+)";
        ":lang()";
        ":dir()";
        ":checked()";
        ":state()";
        "::part()";
        "::part(a,b)";
        "::slotted()";
        "::highlight()";
        "::view-transition-old()";
      ]
      buf 0
  in
  assert_reject input

let test_selector_l4_serialization_matrix buf =
  let input =
    pick
      [
        "svg|a[href^=\"https\" i] > :is(img,picture):not([hidden])";
        ":where(main,article,aside) :has(> h2 + p)";
        "section:has(:scope > h2,:scope > h3)";
        "li:nth-child(2n+1 of .item:not([hidden]))";
        "li:nth-last-child(-n+3 of :where(.visible,[data-visible]))";
        "li:nth-child(even of :is(.item,[data-visible]))";
        "li:nth-last-child(3n+1 of :where(.visible,:not([hidden])))";
        ":where(:modal,:popover-open)";
        ":has(+ :is(summary,.summary))";
        "::part(tab panel)";
        "::slotted(*:not([hidden]))";
        "dialog:modal::backdrop";
        ":host(.active) ::slotted(img.selected)";
        ":is(:lang(en, fr),:dir(ltr),:state(selected))";
      ]
      buf 1
  in
  match parse_selector input with
  | None -> failf "selector L4 serialization vector rejected: %S" input
  | Some selector -> (
      let serialized = minified selector in
      match parse_selector serialized with
      | None ->
          failf "selector L4 serialization did not reparse: %S -> %S" input
            serialized
      | Some reparsed ->
          if
            Css.Selector.specificity selector
            <> Css.Selector.specificity reparsed
          then
            failf "selector L4 serialization changed specificity: %S -> %S"
              input serialized)

let ident buf i = pick [ "article"; "card"; "item"; "nav"; "dialog" ] buf i

let class_name buf i =
  pick [ "active"; "selected"; "dark"; "group"; "peer" ] buf i

let attr_selector buf i =
  let attr = pick [ "data-state"; "aria-expanded"; "role"; "href" ] buf i in
  let op = pick [ "="; "~="; "|="; "^="; "$="; "*=" ] buf (i + 1) in
  let value = pick [ "open"; "true"; "button"; "https" ] buf (i + 2) in
  let flag = pick [ ""; " i"; " s" ] buf (i + 3) in
  "[" ^ attr ^ op ^ "\"" ^ value ^ "\"" ^ flag ^ "]"

let pseudo_class buf i =
  match byte_at buf i mod 6 with
  | 0 -> ":is(." ^ class_name buf (i + 1) ^ ",#" ^ ident buf (i + 2) ^ ")"
  | 1 -> ":where(" ^ ident buf (i + 1) ^ ",." ^ class_name buf (i + 2) ^ ")"
  | 2 -> ":not([hidden],." ^ class_name buf (i + 1) ^ ")"
  | 3 -> ":has(> " ^ ident buf (i + 1) ^ "." ^ class_name buf (i + 2) ^ ")"
  | 4 ->
      ":nth-child(" ^ pick [ "odd"; "even"; "2n+1"; "-n+3" ] buf (i + 1) ^ ")"
  | _ -> ":" ^ pick [ "enabled"; "disabled"; "checked"; "modal" ] buf (i + 1)

let pseudo_element buf i =
  match byte_at buf i mod 4 with
  | 0 -> "::before"
  | 1 -> "::part(" ^ class_name buf (i + 1) ^ ")"
  | 2 -> "::slotted(" ^ ident buf (i + 1) ^ "." ^ class_name buf (i + 2) ^ ")"
  | _ -> "::view-transition-old(" ^ ident buf (i + 1) ^ ")"

let generated_selector buf =
  let base = ident buf 0 in
  let simple =
    base ^ "." ^ class_name buf 1 ^ attr_selector buf 2 ^ pseudo_class buf 6
  in
  match byte_at buf 10 mod 4 with
  | 0 -> simple
  | 1 -> simple ^ " > " ^ ident buf 11 ^ "." ^ class_name buf 12
  | 2 -> ":where(" ^ simple ^ "," ^ ident buf 13 ^ ")"
  | _ -> simple ^ pseudo_element buf 14

let invalid_selector_mutation buf =
  match byte_at buf 0 mod 6 with
  | 0 -> generated_selector buf ^ " > > " ^ ident buf 1
  | 1 -> generated_selector buf ^ "[data-state=]"
  | 2 -> ":not()"
  | 3 -> ":has(:has(" ^ ident buf 1 ^ "))"
  | 4 -> generated_selector buf ^ "::before." ^ class_name buf 1
  | _ -> ident buf 1 ^ ":nth-child(n+)"

let test_generated_selector_grammar buf = assert_stable (generated_selector buf)

let test_unterminated_attribute_recovery buf =
  let compound =
    ident buf 0 ^ "." ^ class_name buf 1 ^ attr_selector buf 2
    ^ pseudo_class buf 6
  in
  assert_stable (compound ^ "[data-state")

let test_invalid_selector_mutations buf =
  assert_reject (invalid_selector_mutation buf)

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
      test_case "canonicalize preserves physical identity on a fixed point"
        [ bytes ] test_canonicalize_preserves_physical_identity;
      test_case "selector-list canonicalize preserves physical identity"
        [ bytes ] test_list_canon_identity;
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
        test_selector_specificity_minify;
      test_case "pseudo-class family vectors" [ bytes ]
        test_pseudo_class_family_vectors;
      test_case "pseudo-element family vectors" [ bytes ]
        test_pseudo_element_family_vectors;
      test_case "invalid pseudo family vectors rejected" [ bytes ]
        test_invalid_pseudo_family_vectors;
      test_case "selector L4 serialization matrix" [ bytes ]
        test_selector_l4_serialization_matrix;
      test_case "generated selector grammar" [ bytes ]
        test_generated_selector_grammar;
      test_case "unterminated attribute selector recovery" [ bytes ]
        test_unterminated_attribute_recovery;
      test_case "invalid selector mutations rejected" [ bytes ]
        test_invalid_selector_mutations;
    ] )
