(** {!Cascade.Support} answers a targets question from the generated
    web-features table, so these pin the three answers it can give and the
    version arithmetic behind them, against keys chosen for what they
    demonstrate rather than for being interesting. *)

open Cascade

let answer = Alcotest.(check (option bool))

(* A production every engine shipped long ago answers yes; one web-features does
   not model at all answers nothing. A vendor-prefixed property is the everyday
   case of the second, and is why the answer is an option rather than a bool. *)
let test_known_and_unknown () =
  answer "color" (Some true)
    (Support.implemented Support.evergreen "css.properties.color");
  answer "a vendor-prefixed property" None
    (Support.implemented Support.evergreen
       "css.properties.-webkit-mask-composite");
  answer "an invented key" None
    (Support.implemented Support.evergreen "css.properties.not-a-property")

(* Nothing implements this, so no target version satisfies it. *)
let test_unimplemented_everywhere () =
  answer "animation-timeline in the shorthand" (Some false)
    (Support.implemented Support.evergreen
       "css.properties.animation.animation-timeline_included")

(* One engine has it and the rest do not, which is still a gap for a run that
   answers for all four. *)
let test_one_engine_only () =
  answer "cross-fade()" (Some false)
    (Support.implemented Support.evergreen "css.types.image.cross-fade");
  answer "the text-overflow string arm" (Some false)
    (Support.implemented Support.evergreen "css.properties.text-overflow.string")

(* The answer moves with the targets. Any key whose four engines all name a
   version demonstrates it, so this picks one from the table rather than naming
   a production that may leave the dataset. *)
let test_targets_move_the_answer () =
  let everywhere =
    List.find_opt
      (fun (_, (s : Baseline.support)) ->
        List.for_all Option.is_some
          [ s.chrome; s.firefox; s.safari; s.safari_ios ])
      Baseline.support
  in
  match everywhere with
  | None -> Alcotest.fail "no table entry names a version for all four engines"
  | Some (key, s) ->
      let versions =
        List.filter_map Fun.id [ s.chrome; s.firefox; s.safari; s.safari_ios ]
      in
      let newest =
        List.fold_left (fun a (major, _) -> max a major) 0 versions
      in
      let all v =
        { Support.chrome = v; firefox = v; safari = v; ios_safari = v }
      in
      answer "targets past every shipped version" (Some true)
        (Support.implemented (all (newest + 1, 0)) key);
      answer "targets before every shipped version" (Some false)
        (Support.implemented (all (0, 0)) key)

(* [unimplemented_by] reads as "known to be missing", so an unknown key is not a
   gap. *)
let test_unknown_is_not_a_gap () =
  Alcotest.(check bool)
    "an unknown key" false
    (Support.unimplemented_by Support.evergreen "css.properties.color");
  Alcotest.(check bool)
    "a key nothing implements" true
    (Support.unimplemented_by Support.evergreen
       "css.properties.animation.animation-timeline_included")

(* A production Chrome takes and no specification grants: cascade turns the
   value away, and the browser-backed harness can only tell that apart from a
   reader gap through a measured key. The pair is the contract, so both halves
   are asked here.

   CSS Values 4 sec. 4.2 excludes the CSS-wide keywords from [<custom-ident>]
   itself, so no word of a font family sequence is one. CSS Sizing 4 (ED) sec.
   5.2 spells [contain-intrinsic-size] as [[ auto? [ none | <length [0,inf]> ] ]
   {1,2}], so a trailing bare [auto] is no slot. *)
let test_measured_beyond_the_grammar () =
  List.iter
    (fun (key, css) ->
      Alcotest.(check bool)
        (key ^ " is measured here")
        true
        (Support.self_measured key);
      answer
        (key ^ " is what Chrome ships")
        (Some true)
        (Support.engine_implements Support.Chrome (153, 0) key);
      Alcotest.(check bool)
        (css ^ " is no declaration")
        true
        (Result.is_error
           (Css.of_string ~strict:true (String.concat "" [ "x{"; css; "}" ]))))
    [
      ("css.properties.font-family.inherit", "font-family:inherit inherit");
      ( "css.properties.contain-intrinsic-size.auto_none_auto",
        "contain-intrinsic-size:auto none auto" );
    ]

let suite =
  ( "support",
    [
      Alcotest.test_case "measured beyond the grammar" `Quick
        test_measured_beyond_the_grammar;
      Alcotest.test_case "known and unknown" `Quick test_known_and_unknown;
      Alcotest.test_case "unimplemented everywhere" `Quick
        test_unimplemented_everywhere;
      Alcotest.test_case "one engine only" `Quick test_one_engine_only;
      Alcotest.test_case "targets move the answer" `Quick
        test_targets_move_the_answer;
      Alcotest.test_case "unknown is not a gap" `Quick test_unknown_is_not_a_gap;
    ] )
