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

(* Selectors 4 sec. 3.9: a list is read as its selectors are only when every
   target drops all of them or none. Chromium and WebKit drop
   [::-moz-placeholder], which Firefox reads, so two of them list together and
   apart from a plain selector. Every target parses a [::-webkit-]
   pseudo-element (Chromium and WebKit take any such name, Firefox an unknown
   one since 91 at least, Servo's [PseudoElement::UnknownWebkit]). Which targets
   read a pseudo-element cascade does not know is not known, so it lists with
   nothing. *)
let test_listable () =
  let v = Selector.v in
  Alcotest.(check bool)
    "two -moz- pseudo-elements" true
    (Merge.compatible (v ".a::-moz-placeholder") (v ".b::-moz-placeholder"));
  Alcotest.(check bool)
    "-moz- and plain" false
    (Merge.compatible (v ".a::-moz-placeholder") (v ".b"));
  Alcotest.(check bool)
    "-webkit- and plain" true
    (Merge.compatible (v ".a::-webkit-scrollbar") (v ".b"));
  Alcotest.(check bool)
    "unknown pseudo-element" false
    (Merge.listable (v ".a::-moz-selection"))

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

(* Selectors 4 sec. 3.9: a browser drops a whole list when it does not parse one
   of its selectors, so two selectors share a list only when the same default
   targets drop each. Chrome 111 and Safari 16.4 predate [:user-valid] and
   [:user-invalid] (Chrome 119, Safari 16.5 in MDN's compatibility data), and
   every target parses the rest of the pool. Inside a forgiving [:is()] or
   [:where()] (sec. 4.2 and 4.3) a selector a browser does not parse drops that
   argument alone, so it drops nothing there. Each entry carries whether the two
   targets drop it. *)
let compat_pool =
  [
    (".a", false);
    (".b:hover", false);
    (".c:user-valid", true);
    (".d:user-invalid .e", true);
    (":is(:where(.group):hover .f)", false);
    (":is(:where(.peer):checked~.g)", false);
    (":is(:where(.group):hover .h):user-valid", true);
    (":where(.i,.j) .k", false);
    (":is(.l:user-valid) .m", false);
    (".n::before", false);
  ]
  |> List.map (fun (s, newer) -> (Selector.v s, newer))

let test_compatible_reads_which_targets_drop () =
  List.iter
    (fun (a, newer_a) ->
      List.iter
        (fun (b, newer_b) ->
          if Merge.compatible a b <> Bool.equal newer_a newer_b then
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
  let pool = Array.of_list (List.map fst compat_pool) in
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
  let pool = Array.of_list (List.map fst compat_pool) in
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
  Array.map Selector.v
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

(* The targets are floors, so two selectors share a list only when every browser
   at or above them drops both or neither. Chrome 111 drops [:popover-open] and
   [:user-valid] alike, but Chrome 114 reads the first and Chrome 119 the
   second; [:user-valid] and [:user-invalid] ship together in every engine. *)
let test_compatible_past_the_floors () =
  let v = Selector.v in
  Alcotest.(check bool)
    "shipped in different releases" false
    (Merge.compatible (v ".a:popover-open") (v ".b:user-valid"));
  Alcotest.(check bool)
    "shipped in the same releases" true
    (Merge.compatible (v ".a:user-valid") (v ".b:user-invalid"))

let suite =
  ( "merge",
    [
      Alcotest.test_case "listable" `Quick test_listable;
      Alcotest.test_case "compatible past the floors" `Quick
        test_compatible_past_the_floors;
      Alcotest.test_case "compatible reads which targets drop" `Quick
        test_compatible_reads_which_targets_drop;
      Alcotest.test_case "all_compatible matches the pair loop" `Quick
        test_all_compatible_matches_the_pair_loop;
      Alcotest.test_case "run matches the pair loop" `Quick
        test_run_matches_the_pair_loop;
      Alcotest.test_case "declarations_equal fast and structural paths" `Quick
        test_declarations_equal_fast_and_structural_paths;
      Alcotest.test_case "key is the selector set" `Quick
        test_key_is_the_selector_set;
    ] )
