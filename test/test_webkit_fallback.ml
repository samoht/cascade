(** The compatibility-prefix pairs, pinned through the emission.

    {!Cascade.Optimize} synthesises a prefixed declaration beside a standard one
    the targets cannot read, and the rule factoring has to recognise the same
    pairs or it separates the two halves. Both sides read one table; these ask
    the observable question that table settles, for a family from each of the
    two relations that used to answer it apart. *)

open Cascade

let minify css =
  match Css.of_string ~strict:false css with
  | Ok { Css.stylesheet; _ } ->
      String.trim (Css.to_string ~minify:true (Css.optimize stylesheet))
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let emits input expected = Alcotest.(check string) input expected (minify input)

(* The synthesis writes the prefixed half in front of the standard one, so a
   WebKit that reads only the prefixed spelling still loses to a later standard
   declaration in the cascade. *)
let test_synthesis_writes_the_pair () =
  emits "a{user-select:all}" "a{-webkit-user-select:all;user-select:all}";
  emits "a{mask-size:auto}" "a{-webkit-mask-size:auto;mask-size:auto}";
  emits "a{hyphens:auto}" "a{-webkit-hyphens:auto;hyphens:auto}"

(* An authored prefix is the author's, so nothing is synthesised beside it and
   there is no pair to keep together. *)
let test_authored_prefix_is_left_alone () =
  emits "a{-webkit-user-select:all}" "a{-webkit-user-select:all}";
  emits "a{-webkit-mask-size:auto}" "a{-webkit-mask-size:auto}"

(* The factoring may not take one half into a shared rule and leave the other
   behind: the synthesis reads one rule at a time, so it would write the missing
   half back beside the half that stayed and the element would carry it twice.

   [user-select] was answered by Shorthand's hand-written vendor twins and
   [mask-size] by nothing, which is how the second used to come out longer than
   the rules the rewrite replaced. Both are asked here, and in both orders,
   since neither half knows which arrived first. *)
let test_factoring_keeps_a_pair_together () =
  emits "a{user-select:all}b{-webkit-user-select:ALL}"
    "a{-webkit-user-select:all;user-select:all}b{-webkit-user-select:all}";
  emits "a{-webkit-user-select:all}b{user-select:ALL}"
    "a{-webkit-user-select:all}b{-webkit-user-select:all;user-select:all}";
  emits "a{mask-size:auto}b{-webkit-mask-size:AUTO}"
    "a{-webkit-mask-size:auto;mask-size:auto}b{-webkit-mask-size:auto}";
  emits "a{-webkit-mask-size:auto}b{mask-size:AUTO}"
    "a{-webkit-mask-size:auto}b{-webkit-mask-size:auto;mask-size:auto}"

(* Two declarations of one family that carry different values, or disagree about
   [!important], are not a pair, so nothing stops the factoring reaching them.
   Without this the guard would be a rule against grouping [-webkit-] anything,
   which is not what the synthesis needs. *)
let test_a_differing_value_is_not_a_pair () =
  emits "a{-webkit-mask-size:cover}b{-webkit-mask-size:cover}"
    "a,b{-webkit-mask-size:cover}";
  emits "a{-webkit-user-select:none}b{-webkit-user-select:none}"
    "a,b{-webkit-user-select:none}"

let decl css = Css_test_helpers.ok (Declaration.of_string css)

let check_pair label expected a b =
  Alcotest.(check bool)
    label expected
    (Webkit_fallback.is_pair (decl a) (decl b));
  Alcotest.(check bool)
    (label ^ ", reversed") expected
    (Webkit_fallback.is_pair (decl b) (decl a))

(* The two halves of a pair are the standard and the prefixed spelling of one
   family, at one importance, with one value. *)
let pairs_are_one_family_one_value () =
  check_pair "user-select" true "user-select:none" "-webkit-user-select:none";
  check_pair "another value" false "user-select:none" "-webkit-user-select:text";
  check_pair "another importance" false "user-select:none!important"
    "-webkit-user-select:none";
  check_pair "another family" false "backdrop-filter:blur(1px)"
    "-webkit-user-select:none";
  check_pair "a property with no prefixed spelling" false "color:red"
    "-webkit-user-select:none";
  check_pair "mask-image" true "mask-image:url(a.png)"
    "-webkit-mask-image:url(a.png)";
  check_pair "mask, whose prefixed spelling has no typed property" true
    "mask:url(a.png)" "-webkit-mask:url(a.png)"

(* The filter answers whether a family's prefixed spelling is there, not whether
   a name starts with [-webkit-]: a prefixed property no family pairs holds no
   pair. *)
let filter_reads_the_families () =
  let holds css =
    Webkit_fallback.holds_a_prefixed_spelling (List.map decl css)
  in
  Alcotest.(check bool)
    "a prefixed half" true
    (holds [ "color:red"; "-webkit-user-select:none" ]);
  Alcotest.(check bool) "no prefixed half" false (holds [ "color:red" ]);
  Alcotest.(check bool)
    "a prefixed property of no family" false
    (holds [ "-webkit-appearance:none" ])

let suite =
  ( "webkit_fallback",
    [
      Alcotest.test_case "synthesis writes the pair" `Quick
        test_synthesis_writes_the_pair;
      Alcotest.test_case "an authored prefix is left alone" `Quick
        test_authored_prefix_is_left_alone;
      Alcotest.test_case "factoring keeps a pair together" `Quick
        test_factoring_keeps_a_pair_together;
      Alcotest.test_case "a differing value is not a pair" `Quick
        test_a_differing_value_is_not_a_pair;
      Alcotest.test_case "pairs are one family with one value" `Quick
        pairs_are_one_family_one_value;
      Alcotest.test_case "the filter reads the families" `Quick
        filter_reads_the_families;
    ] )
