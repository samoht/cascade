open Cascade

let rules css =
  match Css.of_string css with
  | Ok { stylesheet; _ } ->
      List.filter_map
        (function Stylesheet.Rule r -> Some r | _ -> None)
        stylesheet
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let rule css =
  match rules css with
  | [ r ] -> r
  | rs -> Alcotest.failf "expected one rule, got %d" (List.length rs)

let same_decl a b = a == b || a = b

let test_pseudo_and_vendor_detection () =
  Alcotest.(check (option string))
    "pseudo at selector tail" (Some "::before")
    (Option.map Selector.to_string
       (Merge.pseudo (Selector.of_string ".a .b::before")));
  Alcotest.(check bool)
    "vendor pseudo detected" true
    (Merge.vendor (Selector.of_string ".a::-webkit-scrollbar"));
  Alcotest.(check bool)
    "regular selector is not vendor" false
    (Merge.vendor (Selector.of_string ".a::before"))

let test_declarations_equal_fast_and_structural_paths () =
  let r = rule ".a{color:red;width:1px}" in
  Alcotest.(check bool)
    "physical list fast path" true
    (Merge.declarations_equal ~same:same_decl r.declarations r.declarations);
  let r2 = rule ".b{color:red;width:1px}" in
  Alcotest.(check bool)
    "structural path" true
    (Merge.declarations_equal ~same:same_decl r.declarations r2.declarations);
  let r3 = rule ".c{color:red}" in
  Alcotest.(check bool)
    "different length" false
    (Merge.declarations_equal ~same:same_decl r.declarations r3.declarations)

(* [compatible] is reflexive and symmetric but NOT transitive. A plain selector
   sits beside one carrying a newer pseudo-class, and beside an
   [:is(:where(...))] variant, while those two never sit together: a browser
   that does not know [:user-valid] drops the whole rule, where the forgiving
   variant would have survived. Grouping by sorting a run and checking only
   neighbours is unsound for exactly this reason. *)
let test_compatible_is_not_transitive () =
  let newer = Selector.of_string ".x:user-valid" in
  let plain = Selector.of_string ".y" in
  let guarded = Selector.of_string ":is(:where(.group):hover .z)" in
  Alcotest.(check bool) "newer with plain" true (Merge.compatible newer plain);
  Alcotest.(check bool)
    "plain with guarded" true
    (Merge.compatible plain guarded);
  Alcotest.(check bool)
    "newer with guarded" false
    (Merge.compatible newer guarded);
  Alcotest.(check bool)
    "and the other way round" false
    (Merge.compatible guarded newer);
  Alcotest.(check bool) "reflexive on newer" true (Merge.compatible newer newer);
  Alcotest.(check bool)
    "reflexive on guarded" true
    (Merge.compatible guarded guarded)

(* Every shape [compatible] distinguishes: a selector with neither marker, one
   carrying a newer pseudo-class where a browser can see it, one carrying it
   inside a forgiving [:is()], an [:is(:where(.group))] and an
   [:is(:where(.peer))] variant, one that is both, and a [:where()] holding
   neither marker. *)
let compat_pool =
  List.map Selector.of_string
    [
      ".a";
      ".b:hover";
      ".c:user-valid";
      ".d:user-invalid .e";
      ":is(:where(.group):hover .f)";
      ":is(:where(.peer):checked~.g)";
      ":is(:where(.group):hover .h):user-valid";
      ":where(.i,.j) .k";
      ":is(.l:user-valid) .m";
      ".n::before";
    ]

(* The relation as it was written before [all_compatible]: read
   [has_is_where_pattern] off both, and where they disagree read
   [has_newer_pseudo_class] off the one that lacks it. *)
let compatible_by_hand sel1 sel2 =
  let sel1_complex = Selector.has_is_where_pattern sel1 in
  let sel2_complex = Selector.has_is_where_pattern sel2 in
  if sel1_complex <> sel2_complex then
    let plain_sel = if sel1_complex then sel2 else sel1 in
    not (Selector.has_newer_pseudo_class plain_sel)
  else true

let test_compatible_matches_the_predicates () =
  List.iter
    (fun a ->
      List.iter
        (fun b ->
          if Merge.compatible a b <> compatible_by_hand a b then
            Alcotest.failf "compatible %s %s" (Selector.to_string a)
              (Selector.to_string b))
        compat_pool)
    compat_pool

(* [all_compatible] answers for the whole list what [compatible] answers per
   pair, so the two must agree on every list: all of them up to five selectors
   drawn from the pool, then longer random ones. *)
let all_compatible_by_hand sels =
  let rec loop = function
    | [] | [ _ ] -> true
    | sel :: rest ->
        List.for_all (fun other -> Merge.compatible sel other) rest && loop rest
  in
  loop sels

let check_agrees sels =
  if Merge.all_compatible sels <> all_compatible_by_hand sels then
    Alcotest.failf "all_compatible [%s]"
      (String.concat "; " (List.map Selector.to_string sels))

let test_all_compatible_matches_the_pair_loop () =
  let pool = Array.of_list compat_pool in
  let rec exhaustive depth acc =
    check_agrees (List.rev acc);
    if depth > 0 then
      Array.iter (fun sel -> exhaustive (depth - 1) (sel :: acc)) pool
  in
  exhaustive 5 [];
  let state = Random.State.make [| 0x5EED |] in
  for _ = 1 to 5000 do
    let len = Random.State.int state 30 in
    check_agrees
      (List.init len (fun _ ->
           pool.(Random.State.int state (Array.length pool))))
  done

(* The group decision as [Rule.merge_adjacent_identical] used to make it: a
   candidate joins the group when it is [compatible] with every member already
   in it. It decides which rules end up sharing a selector list, so the run has
   to accept exactly the same prefix of every sequence. *)
let take_by_pairs sels =
  let rec loop group = function
    | sel :: rest when List.for_all (fun g -> Merge.compatible g sel) group ->
        loop (sel :: group) rest
    | _ -> List.rev group
  in
  loop [] sels

let take_by_run sels =
  let rec loop run group = function
    | sel :: rest ->
        let run = Merge.extend_run run sel in
        if Merge.run_compatible run then loop run (sel :: group) rest
        else List.rev group
    | [] -> List.rev group
  in
  loop Merge.empty_run [] sels

let check_take sels =
  if not (List.equal Selector.equal (take_by_pairs sels) (take_by_run sels))
  then
    Alcotest.failf "run disagrees with the pair loop on [%s]"
      (String.concat "; " (List.map Selector.to_string sels))

let test_run_matches_the_pair_loop () =
  let pool = Array.of_list compat_pool in
  let rec exhaustive depth acc =
    check_take (List.rev acc);
    if depth > 0 then
      Array.iter (fun sel -> exhaustive (depth - 1) (sel :: acc)) pool
  in
  exhaustive 5 [];
  let state = Random.State.make [| 0x217A |] in
  for _ = 1 to 5000 do
    let len = Random.State.int state 30 in
    check_take
      (List.init len (fun _ ->
           pool.(Random.State.int state (Array.length pool))))
  done

(* Selectors whose distinguishing part sits behind a prefix they share, which is
   where a table keyed by [key] has to work hardest. *)
let key_pool =
  Array.map Selector.of_string
    [|
      ".a";
      ".p0 .p1 .p2 .p3 .p4 .p5 .p6 .p7 .t1";
      ".p0 .p1 .p2 .p3 .p4 .p5 .p6 .p7 .t2";
      ".p0>.p1>.p2>.p3>.p4>.p5>.p6>.t1";
      ":is(.p0 .p1 .p2 .p3 .p4 .p5 .t1)";
      ".p0 .p1 .p2 .p3 .p4 .p5 .p6[data-x=one]";
      ".p0 .p1 .p2 .p3 .p4 .p5 .p6::before";
      "div.p0 .p1 .p2 .p3 .p4 .p5 .p6 span.t1";
    |]

let key_of sels = Merge.key (Merge.selector_list sels)
let keys_equal a b = List.equal Selector.equal (key_of a) (key_of b)
let render sels = String.concat "," (List.map Selector.to_string sels)

(* [key] answers for a selector list which SET of targets it writes, so any two
   orderings of one list share a key and two different sets never do. A table
   keyed by [key] inherits exactly that, which is what makes it sound to merge
   the rules it groups. *)
let check_key_is_the_set sels =
  let shuffled = List.rev sels in
  if not (keys_equal sels shuffled) then
    Alcotest.failf "key is not order-insensitive on [%s]" (render sels);
  let sorted = List.sort Selector.compare sels in
  if not (keys_equal sels sorted) then
    Alcotest.failf "key disagrees with its own sort on [%s]" (render sels)

let test_key_is_the_selector_set () =
  let pool = key_pool in
  let n = Array.length pool in
  (* Every list up to four selectors drawn from the pool, then longer random
     ones. *)
  let rec exhaustive depth acc =
    if acc <> [] then check_key_is_the_set (List.rev acc);
    if depth > 0 then
      Array.iter (fun sel -> exhaustive (depth - 1) (sel :: acc)) pool
  in
  exhaustive 4 [];
  let state = Random.State.make [| 0x5E1EC |] in
  for _ = 1 to 5000 do
    let len = 1 + Random.State.int state 12 in
    check_key_is_the_set
      (List.init len (fun _ -> pool.(Random.State.int state n)))
  done;
  (* Distinctness: two singleton lists share a key only when the selector is the
     same one. *)
  Array.iter
    (fun a ->
      Array.iter
        (fun b ->
          if keys_equal [ a ] [ b ] <> Selector.equal a b then
            Alcotest.failf "key conflates %s with %s" (Selector.to_string a)
              (Selector.to_string b))
        pool)
    pool

let suite =
  ( "merge",
    [
      Alcotest.test_case "pseudo and vendor detection" `Quick
        test_pseudo_and_vendor_detection;
      Alcotest.test_case "compatible is not transitive" `Quick
        test_compatible_is_not_transitive;
      Alcotest.test_case "compatible matches the two predicates" `Quick
        test_compatible_matches_the_predicates;
      Alcotest.test_case "all_compatible matches the pair loop" `Quick
        test_all_compatible_matches_the_pair_loop;
      Alcotest.test_case "run matches the pair loop" `Quick
        test_run_matches_the_pair_loop;
      Alcotest.test_case "declarations_equal fast and structural paths" `Quick
        test_declarations_equal_fast_and_structural_paths;
      Alcotest.test_case "key is the selector set" `Quick
        test_key_is_the_selector_set;
    ] )
