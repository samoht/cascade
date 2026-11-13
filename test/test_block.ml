open Cascade
open Css_test_helpers

let block css =
  match Css.of_string css with
  | Ok { stylesheet; _ } -> stylesheet
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let render stmts = Css.to_string ~minify:true stmts
let id stmts = stmts

let test_merge_layers_combines_same_name () =
  let stmts = block "@layer a{.x{color:red}}@layer a{.y{color:blue}}" in
  let merged = Block.merge_consecutive_layers ~optimize_merged_block:id stmts in
  let out = render merged in
  Alcotest.(check string)
    "two same-name @layer blocks merge into one"
    "@layer a{.x{color:red}.y{color:blue}}" out

let test_merge_layers_keeps_distinct_names () =
  let stmts = block "@layer a{.x{color:red}}@layer b{.y{color:blue}}" in
  let merged = Block.merge_consecutive_layers ~optimize_merged_block:id stmts in
  let out = render merged in
  Alcotest.(check string)
    "different-name @layer blocks stay separate"
    "@layer a{.x{color:red}}@layer b{.y{color:blue}}" out

(* CSS Cascade 5 sec. 6.4.1: [@layer a\2e b] names one layer whose single ident
   holds a dot and [@layer a.b] names the sublayer [b] of [a], so merging the
   two would move a declaration into a layer the input never wrote it in. Two
   spellings of the one ident still name the one layer. *)
let test_merge_layers_keeps_escaped_dot_apart () =
  let stmts = block "@layer a\\2e b{.x{color:red}}@layer a.b{.y{color:blue}}" in
  let merged = Block.merge_consecutive_layers ~optimize_merged_block:id stmts in
  Alcotest.(check string)
    "an ident holding a dot is not the sublayer of the same spelling"
    "@layer a\\.b{.x{color:red}}@layer a.b{.y{color:blue}}" (render merged)

let test_merge_layers_combines_escaped_dot () =
  let stmts =
    block "@layer a\\2e b{.x{color:red}}@layer a\\.b{.y{color:blue}}"
  in
  let merged = Block.merge_consecutive_layers ~optimize_merged_block:id stmts in
  Alcotest.(check string)
    "two spellings of one ident merge"
    "@layer a\\.b{.x{color:red}.y{color:blue}}" (render merged)

let test_merge_media_combines_same_condition () =
  let stmts =
    block
      "@media (min-width:1px){.x{color:red}}@media \
       (min-width:1px){.y{color:blue}}"
  in
  let merged = Block.merge_consecutive_media ~optimize_merged_block:id stmts in
  let out = render merged in
  Alcotest.(check bool)
    ("two same-cond @media blocks merge: " ^ out)
    true
    (let needle = ".y" in
     let len = String.length needle in
     let found = ref false in
     for i = 0 to String.length out - len do
       if String.sub out i len = needle then found := true
     done;
     !found
     &&
     (* both bodies in one @media *)
     let media_count = ref 0 in
     String.iter (fun c -> if c = '@' then incr media_count) out;
     !media_count = 1)

(* CSS Conditional Rules 5 sec. 5: a [@container] rule applies when its
   container name and query both match, so two adjacent rules sharing both
   evaluate identically and merge. A [style()] query is one query whatever it
   spells, values and ranges included. *)
let test_merge_containers_by_condition () =
  let merged condition =
    let css =
      String.concat ""
        [
          "@container ";
          condition;
          "{.a{color:red}}@container ";
          condition;
          "{.b{color:blue}}";
        ]
    in
    Block.merge_consecutive_containers ~optimize_merged_block:id (block css)
  in
  List.iter
    (fun (name, condition) ->
      Alcotest.(check int)
        (name ^ ": adjacent same-condition @container blocks merge into one")
        1
        (List.length (merged condition)))
    [
      ("size query", "(min-width: 100px)");
      ("boolean style query", "style(--x)");
      ("style declaration query", "style(--x: 1)");
      ("style range query", "style(1px < --w < 5px)");
      ("compound style query", "style((--x: 1) and (--y: 2))");
      ("named style query", "card style(--x: 1)");
    ]

(* Two style queries that ask different things gate different content, so they
   stay two rules. *)
let test_merge_containers_keeps_distinct_conditions () =
  let stmts =
    block
      "@container style(--x: 1){.a{color:red}}@container style(--x: \
       2){.b{color:blue}}"
  in
  Alcotest.(check int)
    "@container blocks with different style queries stay separate" 2
    (List.length
       (Block.merge_consecutive_containers ~optimize_merged_block:id stmts))

(* Conditional Rules 5 sec. 5.4: a [<container-query>] that names an unknown
   container feature selects no query container, so it is never true.
   [@container (inline-size\ \>\=\ 10px)] is one escaped ident, a boolean
   feature named [inline-size >= 10px], while [@container (inline-size >= 10px)]
   is the size range that inline-size containers are made for. The two spell the
   same characters, and merging them applies declarations the input never let
   match. Same shape as the escaped [@layer] dot and the escaped [@media] type
   above. *)
let test_merge_containers_keeps_an_escaped_feature_name_apart () =
  let css =
    {|@container (inline-size\ \>\=\ 10px){.x{color:red}}@container (inline-size>=10px){.y{color:blue}}|}
  in
  Alcotest.(check int)
    "an unknown container feature is not merged into a real one" 2
    (List.length
       (Block.merge_consecutive_containers ~optimize_merged_block:id (block css)))

(* Conditional Rules 5 sec. 6.1 gives a [<size-feature>] the media feature
   syntax, so Media Queries 4 sec. 2.4.4 applies: a [min-] prefix on a range
   feature is the [>=] comparison. These two blocks carry one bound in two
   spellings and the cascade evaluates them identically. *)
let test_merge_containers_joins_two_spellings_of_one_bound () =
  let stmts =
    block
      "@container (min-width:10px){.x{color:red}}@container \
       (width>=10px){.y{color:blue}}"
  in
  Alcotest.(check int)
    "two spellings of one bound merge into one block" 1
    (List.length
       (Block.merge_consecutive_containers ~optimize_merged_block:id stmts))

let test_drop_empty_rules () =
  let stmts = block ".a{}.b{color:red}.c{}" in
  let out = render (Block.drop_empty_rules stmts) in
  Alcotest.(check string) "empty rules removed" ".b{color:red}" out

(* CSS Conditional 3 sec. 3: a conditional group rule applies its contents when
   its condition holds, so one with no contents applies nothing whatever the
   condition, and dropping it changes no cascade. That covers [@-moz-document]
   alongside the conditional groups already dropped. *)
let test_drop_empty_moz_document () =
  let stmts = block "@-moz-document url-prefix(){}.b{color:red}" in
  Alcotest.(check string)
    "an empty @-moz-document is dropped" ".b{color:red}"
    (render (Block.drop_empty_rules stmts))

(* css-conditional-5 sec. 3 binds an [@else] to the [@when] or [@else] before
   it, so an empty one of those is only inert when nothing chains onto it.
   Dropping an antecedent leaves a bare [@else], which cascade's own reader and
   a browser both reject, and the branch that followed stops applying. *)
let test_drop_empty_when_and_else () =
  Alcotest.(check string)
    "an empty @when with nothing after it is dropped" ".b{color:red}"
    (render
       (Block.drop_empty_rules (block "@when media(width>0px){}.b{color:red}")));
  Alcotest.(check string)
    "an empty @else closing a chain is dropped"
    "@when media(width>0px){.x{color:red}}"
    (render
       (Block.drop_empty_rules
          (block "@when media(width>0px){.x{color:red}}@else{}")))

let test_keep_empty_branch_with_a_chained_else () =
  let chained = "@when media(width>0px){}@else{.z{color:green}}" in
  Alcotest.(check string)
    "an empty @when keeps the @else that binds to it" chained
    (render (Block.drop_empty_rules (block chained)));
  let middle =
    String.concat ""
      [
        "@when media(width>0px){.x{color:red}}";
        "@else supports(color:red){}";
        "@else{.z{color:green}}";
      ]
  in
  Alcotest.(check string)
    "an empty @else in the middle of a chain stays" middle
    (render (Block.drop_empty_rules (block middle)))

(* An origin wrapper has no CSS syntax and gates nothing: it records where a
   block came from, which an empty one still does. *)
let test_keep_empty_origin () =
  let stmts = [ Stylesheet.Origin (Author, []) ] in
  Alcotest.(check int)
    "an empty origin wrapper survives" 1
    (List.length (Block.drop_empty_rules stmts))

let test_is_layer_empty () =
  let stmts = block "@layer a{}" in
  match stmts with
  | [ Stylesheet.Layer (_, body) ] ->
      Alcotest.(check bool) "empty layer body" true (Block.is_layer_empty body)
  | _ -> Alcotest.fail "expected one layer statement"

(* A rule with no declarations of its own still carries whatever it nests, so a
   layer holding one is a layer with contents. Reading only the declarations
   turned the block into a bare [@layer a;] and deleted the CSS under it. *)
let test_is_layer_not_empty_with_nested () =
  let cases =
    [
      ("a rule nesting two rules", "@layer a{.x{.y{color:red}.z{color:blue}}}");
      ("a selector list nesting a rule", "@layer a{.x,.w{.y{color:red}}}");
      ("a rule nesting an at-rule", "@layer a{.x{@media print{color:red}}}");
    ]
  in
  List.iter
    (fun (name, css) ->
      match block css with
      | [ Stylesheet.Layer (_, body) ] ->
          Alcotest.(check bool)
            (name ^ ": the layer has contents")
            false
            (Block.is_layer_empty body)
      | _ -> Alcotest.fail "expected one layer statement")
    cases

(* Media Queries 4 sec. 3.2: an unknown media type never matches. [@media
   screen\ and\ \(min-width\:\ 10px\)] names one escaped ident as its media
   type, so it selects nothing, while [@media screen and (min-width: 10px)]
   selects a screen at 10px or wider. The two spell the same characters, and
   merging them would apply declarations the input never let match. Same shape
   as the escaped [@layer] dot above. *)
(* A preference query nested in the body is not a reason to keep two
   same-condition [@media] blocks apart. The condition compared is the outer
   one, and concatenating the bodies preserves their order, so the merge changes
   nothing a browser evaluates. A nested [@supports] already merges; the three
   preference features were the one exception. *)
let test_merge_media_combines_over_a_nested_preference () =
  let css =
    "@media (min-width:40rem){@media \
     (prefers-color-scheme:dark){.x{color:red}}}@media \
     (min-width:40rem){@media (prefers-color-scheme:dark){.y{color:blue}}}"
  in
  let merged =
    Block.merge_consecutive_media ~optimize_merged_block:id (block css)
  in
  Alcotest.(check int)
    "a nested preference query does not keep the outer blocks apart" 1
    (List.length merged)

let test_merge_media_keeps_an_escaped_media_type_apart () =
  let css =
    "@media screen\\ and\\ \\(min-width\\:\\ 10px\\){.x{color:red}}@media \
     screen and (min-width:10px){.y{color:blue}}"
  in
  let stmts = block css in
  let merged = Block.merge_consecutive_media ~optimize_merged_block:id stmts in
  Alcotest.(check int)
    "an unknown media type is not merged into a real one" 2 (List.length merged);
  let distant = Block.merge_distant_media ~optimize_merged_block:id stmts in
  Alcotest.(check int)
    "the distant merge keeps them apart too" 2 (List.length distant)

(* Media Queries 4 sec. 2.4.4: a [min-] prefix on a range feature is the [>=]
   comparison, so these two blocks carry one condition in two spellings and the
   cascade evaluates them identically. *)
let test_merge_media_joins_two_spellings_of_one_bound () =
  let stmts =
    block
      "@media (min-width:10px){.x{color:red}}@media \
       (width>=10px){.y{color:blue}}"
  in
  let merged = Block.merge_consecutive_media ~optimize_merged_block:id stmts in
  Alcotest.(check int)
    "two spellings of one bound merge into one block" 1 (List.length merged)

(* --- distant merges and the cascade --- *)

let merge_distant_media css =
  render (Block.merge_distant_media ~optimize_merged_block:id (block css))

(* An element carrying both classes has one ::before box. With the query true it
   reads [blue] then [green] at one specificity, (0,1,1), and computes green;
   hoisting the later block above the list would hand it blue. The list meets
   [.b::before] through its [.a::before] branch, so the merge stays refused. *)
let test_distant_media_keeps_order_across_a_pseudo_element_list () =
  let css =
    "@media (min-width:1px){.x{color:red}}.a:after,.a:before{color:blue}@media \
     (min-width:1px){.b:before{color:green}}"
  in
  Alcotest.(check string)
    "the later block stays behind the list"
    (render (block css))
    (merge_distant_media css)

(* CSS Cascade 5 sec. 6.1 ranks specificity before order of appearance. On an
   element matching both, [#y] (1,0,0) beats [.b] (0,1,0) from either position,
   and where only one matches order decides nothing, so the later block may move
   above [#y]. *)
let test_distant_media_crosses_a_more_specific_rule () =
  Alcotest.(check string)
    "the later block joins the first across a rule it cannot outweigh"
    (render
       (block
          "@media (min-width:1px){.x{color:red}.b{color:green}}#y{color:blue}"))
    (merge_distant_media
       "@media (min-width:1px){.x{color:red}}#y{color:blue}@media \
        (min-width:1px){.b{color:green}}")

(* A missed optimisation, not a cascade rule. CSS Cascade 6 sec. 3.1 (Cascade
   Sorting Order) ranks scope proximity after specificity and before order of
   appearance, and gives a rule without a scoping root infinite proximity. On an
   element both [@scope (.s) { & .b }] and [.b] match, either specificity or the
   scoped rule's finite proximity decides, so order never does and the later
   block could move above the scope body. The pass reads no scoping root (sec.
   3.5.1, Effects of @scope, makes [&] the root), so it refuses: a pass that
   learns to weigh the pair may merge here. *)
let test_distant_media_refuses_to_cross_a_scope_body () =
  let css =
    "@media (min-width:1px){.x{color:red}}@media print{@scope (.s){& \
     .b{color:blue}}}@media (min-width:1px){.b{color:green}}"
  in
  Alcotest.(check string)
    "the pass does not yet move the later block over the scope body"
    (render (block css))
    (merge_distant_media css)

(* CSS Nesting 1 sec. 4 (Nesting Selector) gives [&] the largest specificity of
   the enclosing rule's selector, here [.outer .card], (0,2,0), not [.card]
   alone. So [&+&] weighs (0,4,0), as much as [.outer .card .y.z] (Selectors 4
   sec. 15, Calculating a selector's specificity), and on an element both match
   order of appearance decides: blue then green. Whatever else the optimizer
   does, the [&+&] block stays behind [.y.z]. Weighing [&] as [.card] would give
   (0,2,0) against (0,3,0) and let it move. *)
let test_distant_media_weighs_a_nested_body_by_its_ancestors () =
  let out =
    render
      (Css.optimize
         (block
            ".outer{color:#000;.card{@media(width>=1px){.a{color:red}}.y.z{color:#00f}@media(width>=1px){&+&{color:green}}}}"))
  in
  match
    ( Astring.String.find_sub ~sub:".y.z{color:#00f}" out,
      Astring.String.find_sub ~sub:"&+&{color:green}" out )
  with
  | Some y_z, Some amp -> Alcotest.(check bool) out true (y_z < amp)
  | _ -> Alcotest.failf "both rules survive: %s" out

(* --- allocation / complexity guard --- *)

(* [n] plain rules with one [@media print] block at each end, so the pass has
   exactly one merge to make and [n] statements to carry while it looks for the
   partner. Every rule writes its own property on its own selector, so nothing
   conflicts and the merge goes through. *)
let distant_media_run n =
  let b = Buffer.create ((n * 24) + 64) in
  let out = Fmt.with_buffer b in
  Buffer.add_string b "@media print{.m0{color:red}}";
  for i = 1 to n do
    Fmt.pf out ".r%d{--p%d:%d}" i i i
  done;
  Buffer.add_string b "@media print{.m1{color:blue}}";
  block (Buffer.contents b)

(* Carrying the accumulator by appending to its end copies it once per
   statement, so the walk costs a square in the statement count rather than a
   line. The partner search is one scan and does not depend on [n] twice. *)
let test_merge_distant_media_is_subquadratic () =
  let small = distant_media_run 2_000 in
  let large = distant_media_run 4_000 in
  let merge stmts = Block.merge_distant_media ~optimize_merged_block:id stmts in
  let small_words = measure (fun () -> merge small) in
  let large_words = measure (fun () -> merge large) in
  let ratio = large_words /. small_words in
  Alcotest.(check bool)
    (Fmt.str "alloc %.0f -> %.0f (%.2fx for 2x N)" small_words large_words ratio)
    true (ratio < 3.)

let suite =
  ( "block",
    [
      Alcotest.test_case "merge same-name @layer" `Quick
        test_merge_layers_combines_same_name;
      Alcotest.test_case "keep distinct-name @layer" `Quick
        test_merge_layers_keeps_distinct_names;
      Alcotest.test_case "keep an escaped dot out of a sublayer" `Quick
        test_merge_layers_keeps_escaped_dot_apart;
      Alcotest.test_case "merge one ident spelled two ways" `Quick
        test_merge_layers_combines_escaped_dot;
      Alcotest.test_case "merge same-condition @media" `Quick
        test_merge_media_combines_same_condition;
      Alcotest.test_case "keep an escaped media type out of a merge" `Quick
        test_merge_media_keeps_an_escaped_media_type_apart;
      Alcotest.test_case "merge @media over a nested preference query" `Quick
        test_merge_media_combines_over_a_nested_preference;
      Alcotest.test_case "merge two spellings of one bound" `Quick
        test_merge_media_joins_two_spellings_of_one_bound;
      Alcotest.test_case "merge same-condition @container" `Quick
        test_merge_containers_by_condition;
      Alcotest.test_case "keep distinct-condition @container" `Quick
        test_merge_containers_keeps_distinct_conditions;
      Alcotest.test_case "keep an escaped container feature out of a merge"
        `Quick test_merge_containers_keeps_an_escaped_feature_name_apart;
      Alcotest.test_case "merge two spellings of one container bound" `Quick
        test_merge_containers_joins_two_spellings_of_one_bound;
      Alcotest.test_case "drop empty rules" `Quick test_drop_empty_rules;
      Alcotest.test_case "drop empty @-moz-document" `Quick
        test_drop_empty_moz_document;
      Alcotest.test_case "drop empty @when and @else" `Quick
        test_drop_empty_when_and_else;
      Alcotest.test_case "keep an empty branch a later @else binds to" `Quick
        test_keep_empty_branch_with_a_chained_else;
      Alcotest.test_case "keep an empty origin wrapper" `Quick
        test_keep_empty_origin;
      Alcotest.test_case "is_layer_empty" `Quick test_is_layer_empty;
      Alcotest.test_case "is_layer_empty sees nested rules" `Quick
        test_is_layer_not_empty_with_nested;
      Alcotest.test_case "distant @media stays behind a pseudo-element list"
        `Quick test_distant_media_keeps_order_across_a_pseudo_element_list;
      Alcotest.test_case "distant @media crosses a more specific rule" `Quick
        test_distant_media_crosses_a_more_specific_rule;
      Alcotest.test_case "distant @media refuses to cross a scope body (missed)"
        `Quick test_distant_media_refuses_to_cross_a_scope_body;
      Alcotest.test_case "distant @media weighs a nested body by its ancestors"
        `Quick test_distant_media_weighs_a_nested_body_by_its_ancestors;
      Alcotest.test_case "merge distant @media is subquadratic" `Quick
        test_merge_distant_media_is_subquadratic;
    ] )
