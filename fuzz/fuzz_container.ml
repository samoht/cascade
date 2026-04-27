(** Fuzz tests for the CSS Container module. *)

open Cascade
open Alcobar

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte_at buf i mod List.length xs)
let name buf i = pick [ "card"; "sidebar"; "layout"; "main" ] buf i

let raw buf i =
  pick
    [
      "(inline-size > 30em)";
      "(30em <= inline-size < 60em)";
      "style(--variant: featured)";
      "scroll-state(stuck: top)";
      "(aspect-ratio > 1/1)";
    ]
    buf i

let condition buf i =
  let open Css.Container in
  match byte_at buf i mod 5 with
  | 0 -> Min_width_rem (Float.of_int (byte_at buf (i + 1)) /. 4.)
  | 1 -> Min_width_px (byte_at buf (i + 1))
  | 2 -> Named (name buf (i + 1), Min_width_rem 24.)
  | 3 -> Named (name buf (i + 1), Raw (raw buf (i + 2)))
  | _ -> Raw (raw buf (i + 1))

let test_to_string_no_empty buf =
  let s = Css.Container.to_string (condition buf 0) in
  if s = "" then fail "container serialization produced an empty query"

let test_named_kind_matches_inner buf =
  let inner = condition buf 3 in
  let named = Css.Container.Named (name buf 0, inner) in
  if Css.Container.kind named <> Css.Container.kind inner then
    fail "named container query changed kind bucket"

let test_compare_antisymmetric buf =
  let a = condition buf 0 in
  let b = condition buf 7 in
  let ab = Css.Container.compare a b in
  let ba = Css.Container.compare b a in
  if ab = 0 && ba <> 0 then fail "container compare equality not symmetric";
  if ab < 0 && ba <= 0 then fail "container compare not antisymmetric";
  if ab > 0 && ba >= 0 then fail "container compare not antisymmetric"

let test_compare_transitive buf =
  let sorted =
    List.sort Css.Container.compare
      [ condition buf 0; condition buf 5; condition buf 9 ]
  in
  match sorted with
  | [ a; b; c ] ->
      if Css.Container.compare a b > 0 || Css.Container.compare b c > 0 then
        fail "container compare sort result is not ordered"
  | _ -> fail "container sort changed list length"

let test_named_prefix_stable buf =
  let name = name buf 0 in
  let query = Css.Container.Named (name, condition buf 4) in
  let serialized = Css.Container.to_string query in
  let prefix = name ^ " " in
  let prefix_len = String.length prefix in
  if
    String.length serialized < prefix_len
    || String.sub serialized 0 prefix_len <> prefix
  then fail (Fmt.str "named container query lost name: %S" serialized)

let test_container_evaluation_stub_identity buf =
  match
    Css.Stylesheet.evaluate_container_query ~condition:(condition buf 0)
      ~container:".card inline-size=40rem"
  with
  | Error (Css.Stylesheet.Requires_platform_context { feature; detail }) ->
      if feature <> "container query evaluation" || detail = "" then
        fail "container evaluation stub lost feature/detail identity"
  | Error (Css.Stylesheet.Requires_document_context _) ->
      fail "container evaluation returned document-context error"
  | Error (Css.Stylesheet.Unsupported_value_alias _) ->
      fail "container evaluation returned value-alias error"
  | Ok _ -> fail "container evaluation stub unexpectedly succeeded"

let test_raw_query_stable buf =
  let raw = raw buf 0 in
  let query = Css.Container.Raw raw in
  if Css.Container.to_string query <> raw then
    fail "raw container query serialization changed"

let suite =
  ( "container",
    [
      test_case "to_string non-empty" [ bytes ] test_to_string_no_empty;
      test_case "named kind matches inner" [ bytes ]
        test_named_kind_matches_inner;
      test_case "compare antisymmetric" [ bytes ] test_compare_antisymmetric;
      test_case "compare transitive" [ bytes ] test_compare_transitive;
      test_case "named serialization keeps name prefix" [ bytes ]
        test_named_prefix_stable;
      test_case "container evaluation stub identity" [ bytes ]
        test_container_evaluation_stub_identity;
      test_case "raw style/scroll-state serialization stable" [ bytes ]
        test_raw_query_stable;
    ] )
