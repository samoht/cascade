open Cascade

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
      Alcotest.test_case "merge same-condition @container" `Quick
        test_merge_containers_by_condition;
      Alcotest.test_case "keep distinct-condition @container" `Quick
        test_merge_containers_keeps_distinct_conditions;
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
    ] )
