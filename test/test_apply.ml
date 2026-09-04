open Cascade

type node = {
  name : string;
  id : string option;
  classes : string list;
  attrs : (string * string) list;
  text : string list;
  mutable parent : node option;
  children : node list;
}

module Node = struct
  type t = node

  let equal = ( == )
  let name n = Some n.name
  let id n = n.id
  let classes n = n.classes
  let attribute n key = List.assoc_opt key n.attrs
  let parent n = n.parent
  let children n = n.children
  let text_children n = n.text
end

module A = Apply.Make (Node)

let node ?id ?(classes = []) ?(attrs = []) ?(text = []) ?(children = []) name =
  let n = { name; id; classes; attrs; text; parent = None; children } in
  List.iter (fun c -> c.parent <- Some n) children;
  n

let inline_style decls =
  Css.inline_style_of_declarations ~minify:true ~mode:Variables decls

(* [A.compute] takes a parsed sheet, so the fixtures parse here. They are all
   meant to parse; one that does not is a broken test, not a case under test. *)
let parse css =
  match Css.of_string css with
  | Ok p -> p.Css.stylesheet
  | Error e -> Alcotest.failf "fixture did not parse: %s" (Error.to_string e)

let projects_static_rule_to_inline_style () =
  let n = node ~classes:[ "card" ] "div" in
  let result = A.compute ~sheet:(parse ".card{color:red;margin:0}") [ n ] in
  match result.styles with
  | [ (node, decls) ] ->
      Alcotest.(check bool) "same node" true (Node.equal node n);
      Alcotest.(check string)
        "inline declarations" "color:red;margin:0" (inline_style decls);
      Alcotest.(check string) "no kept css" "" result.keep_css;
      Alcotest.(check int) "kept count" 0 result.kept
  | _ -> Alcotest.fail "expected one inline assignment"

let keeps_stateful_rule_in_css () =
  let n = node ~classes:[ "card" ] "div" in
  let result =
    A.compute ~sheet:(parse ".card{margin:0}.card:hover{color:blue}") [ n ]
  in
  Alcotest.(check string)
    "inline static rule" "margin:0"
    (match result.styles with
    | [ (_, decls) ] -> inline_style decls
    | _ -> Alcotest.fail "expected one inline assignment");
  Alcotest.(check string)
    "kept dynamic rule" ".card:hover{color:blue}" result.keep_css;
  Alcotest.(check int) "kept count" 1 result.kept

(* A @layer block applies unconditionally, so its rules project onto elements
   just like top-level ones instead of being kept wholesale in a <style>. *)
let projects_layered_rule_to_inline_style () =
  let n = node ~classes:[ "card" ] "div" in
  let result =
    A.compute ~sheet:(parse "@layer u{.card{color:red;margin:0}}") [ n ]
  in
  match result.styles with
  | [ (node, decls) ] ->
      Alcotest.(check bool) "same node" true (Node.equal node n);
      Alcotest.(check string)
        "inline declarations" "color:red;margin:0" (inline_style decls);
      Alcotest.(check string) "no kept css" "" result.keep_css;
      Alcotest.(check int) "kept count" 0 result.kept
  | _ -> Alcotest.fail "expected one inline assignment"

(* Inside a layer the static/dynamic split still holds: the plain rule inlines,
   the stateful one stays in the kept <style>, and it stays in its layer because
   that is what orders it against the other kept rules. *)
let keeps_stateful_rule_inside_layer_in_css () =
  let n = node ~classes:[ "card" ] "div" in
  let result =
    A.compute
      ~sheet:(parse "@layer u{.card{margin:0}.card:hover{color:blue}}")
      [ n ]
  in
  Alcotest.(check string)
    "inline static rule" "margin:0"
    (match result.styles with
    | [ (_, decls) ] -> inline_style decls
    | _ -> Alcotest.fail "expected one inline assignment");
  Alcotest.(check string)
    "kept dynamic rule" "@layer u{.card:hover{color:blue}}" result.keep_css;
  Alcotest.(check int) "kept count" 1 result.kept

(* The inline style [css] projects onto a single [p.x], as a minified
   declaration list. [style] is the style attribute the element already
   carries. *)
let projected ?style css =
  let attrs = match style with None -> [] | Some s -> [ ("style", s) ] in
  let n = node ~classes:[ "x" ] ~attrs "p" in
  let result = A.compute ~sheet:(parse css) [ n ] in
  match result.styles with
  | [ (_, decls) ] -> inline_style decls
  | _ -> Alcotest.fail "expected one inline assignment"

(* css-cascade-5 sec. 6.4.3: among normal declarations an unlayered one beats
   every layer, whatever the source order. *)
let unlayered_beats_layer () =
  Alcotest.(check string)
    "unlayered beats the layer" "color:red"
    (projected ".x{color:red}@layer u{.x{color:blue}}");
  Alcotest.(check string)
    "and still beats it when the layer comes last" "color:red"
    (projected "@layer u{.x{color:blue}}.x{color:red}")

(* The criterion reverses for important declarations: a layered important beats
   an unlayered one, again whatever the source order. *)
let layer_beats_unlayered_important () =
  Alcotest.(check string)
    "layered important wins" "color:blue!important"
    (projected ".x{color:red!important}@layer u{.x{color:blue!important}}");
  Alcotest.(check string)
    "and still wins when it comes first" "color:blue!important"
    (projected "@layer u{.x{color:blue!important}}.x{color:red!important}")

(* Between two layers the last declared wins for normal declarations and the
   first for important ones. *)
let later_layer_wins_normal_earlier_wins_important () =
  Alcotest.(check string)
    "later layer wins" "color:blue"
    (projected "@layer a{.x{color:red}}@layer b{.x{color:blue}}");
  Alcotest.(check string)
    "earlier layer wins for important" "color:red!important"
    (projected
       "@layer a{.x{color:red!important}}@layer b{.x{color:blue!important}}")

(* An [@layer b, a;] statement fixes the order before any block appears, so the
   blocks that follow do not order themselves. *)
let layer_statement_orders_the_blocks () =
  Alcotest.(check string)
    "the statement decides which layer is last" "color:red"
    (projected "@layer b,a;@layer a{.x{color:red}}@layer b{.x{color:blue}}");
  Alcotest.(check string)
    "and the important order follows it" "color:blue!important"
    (projected
       "@layer b,a;@layer a{.x{color:red!important}}@layer \
        b{.x{color:blue!important}}")

(* Layers outrank specificity in the cascade sorting order (css-cascade-5 sec.
   6), so the plain class in the later layer beats the compound selector in the
   earlier one. *)
let layer_outranks_specificity () =
  Alcotest.(check string)
    "later layer beats higher specificity" "color:blue"
    (projected "@layer a{p.x{color:red}}@layer b{.x{color:blue}}")

(* A control: layers that set different properties do not compete, so both
   declarations project as they did before. *)
let non_competing_layers_both_project () =
  Alcotest.(check string)
    "both layers contribute" "color:red;margin:0"
    (projected "@layer a{.x{color:red}}@layer b{.x{margin:0}}")

(* css-cascade-5 sec. 6.1: declarations that tie on origin, importance, layer
   and specificity are sorted by order of appearance, the last one winning. The
   style attribute is overlaid on top of the author cascade, it does not reorder
   it: a longhand written after the shorthand it belongs to still wins, whether
   or not the element carries a style attribute. *)
let style_attribute_keeps_author_order () =
  Alcotest.(check string)
    "author order, then the attribute"
    "margin:0;margin-top:5px;border:1px solid \
     black;border-color:red;color:green"
    (projected ~style:"color:green"
       ".x{margin:0;margin-top:5px;border:1px solid black;border-color:red}");
  Alcotest.(check string)
    "the control: the same sheet with no attribute to overlay"
    "margin:0;margin-top:5px;border:1px solid black;border-color:red"
    (projected
       ".x{margin:0;margin-top:5px;border:1px solid black;border-color:red}")

(* CSS Syntax 3 sec. 5.4: consuming a declaration takes component values to the
   end of the input, and only [{], [(] and [[] open a block there - a [}] is a
   preserved token inside the value. A style attribute is one such list, so
   [color:red}p{color:lime] is a single [color] declaration whose value is
   invalid, and it is dropped. Splicing the attribute into [a{...}] instead lets
   the [}] close the rule and revives a declaration no browser applies. *)
let malformed_style_attribute_is_dropped () =
  Alcotest.(check string)
    "the whole attribute is one invalid declaration" "color:blue"
    (projected ~style:"color:red}p{color:lime" ".x{color:blue}");
  Alcotest.(check string)
    "the control: the same attribute with nothing to escape from" "color:red"
    (projected ~style:"color:red" ".x{color:blue}")

(* [minimal] drops an inherited declaration that restates what the element
   already inherits. That is only sound where nothing else declares the
   property: css-cascade-5 sec. 6.1 sorts by origin before anything else and
   sec. 6.2 puts the UA origin under the author one, so a property the UA
   declares for the element is cascaded, never inherited. Dropping
   [a{color:blue}] uncovers the UA link colour and dropping [h1{font-size}]
   uncovers UA [font-size:2em]. *)
let minimal_keeps_what_a_ua_rule_would_win () =
  let a = node ~attrs:[ ("href", "#") ] "a" in
  let h1 = node "h1" in
  let p = node "p" in
  let body = node ~children:[ a; h1; p ] "body" in
  let css =
    "body{color:blue;font-size:16px}a{color:blue}h1{font-size:16px}p{color:blue}"
  in
  let result = A.compute ~minimal:true ~sheet:(parse css) [ body ] in
  let style n =
    match List.find_opt (fun (m, _) -> Node.equal m n) result.styles with
    | Some (_, decls) -> inline_style decls
    | None -> Alcotest.fail "no assignment for the node"
  in
  Alcotest.(check string)
    "the UA link colour is underneath" "color:blue" (style a);
  Alcotest.(check string) "UA h1 is font-size:2em" "font-size:16px" (style h1);
  Alcotest.(check string)
    "the control: no UA rule sets a paragraph's colour" "" (style p)

(* [minimal] compares the value as written, but a relative one resolves against
   the element it lands on, so the same text one level down is a different
   computed value. css-values-4 sec. 6.1 makes a font-relative unit a multiple
   of the element's own font, and [rem] a multiple of the root's - which the
   ancestor holding the value may itself be, where [font-size] instead resolves
   against the initial value. Sec. 6.4 makes a container unit a fraction of the
   query container, which an element in between can become. A percentage
   resolves against a per-property basis, and css-fonts-4 sec. 3.5 reads the
   parent outright for [larger] and [bolder]. A [var()] reads a custom property
   an element in between can redefine, [light-dark()] reads the inherited
   [color-scheme], and [currentColor] inside [color-mix()] is resolved at
   computed-value time rather than carried through.

   Chrome 146 on [div{font-size:2em}div p{font-size:2em}]: the paragraph
   computes 64px, and 32px once the declaration is dropped. *)
let minimal_keeps_a_relative_value () =
  let projected ?(parent = "div") decl =
    let child = node "p" in
    let root = node ~children:[ child ] parent in
    let css =
      String.concat "" [ parent; "{"; decl; "}"; parent; " p{"; decl; "}" ]
    in
    let result = A.compute ~minimal:true ~sheet:(parse css) [ root ] in
    match List.find_opt (fun (m, _) -> Node.equal m child) result.styles with
    | Some (_, decls) -> inline_style decls
    | None -> Alcotest.failf "no assignment for the child of %s" parent
  in
  let kept ?parent decl =
    Alcotest.(check string) decl decl (projected ?parent decl)
  in
  let dropped ?parent decl =
    Alcotest.(check string) decl "" (projected ?parent decl)
  in
  kept "font-size:2em";
  kept "font-size:150%";
  kept "font-size:larger";
  kept "font-weight:bolder";
  kept "letter-spacing:1ex";
  kept "letter-spacing:calc(1em + 2px)";
  kept "text-indent:5cqw";
  kept "color:var(--tone)";
  kept "color:light-dark(black,white)";
  kept "color:color-mix(in srgb,currentcolor,white)";
  (* [rem] on the root: [html{font-size:2rem}] is twice the initial font size, a
     descendant's [2rem] twice that. *)
  kept ~parent:"html" "font-size:2rem";
  (* Nothing left to resolve, so the restatement goes. A percentage inside a
     colour function is a channel coordinate, not a fraction of the element. *)
  dropped "font-size:16px";
  dropped "font-weight:700";
  dropped "line-height:1.5";
  dropped "letter-spacing:normal";
  dropped "color:hsl(210 40% 96%)";
  dropped "color:#f1f5f9"

(* CSS Writing Modes 4 sec. 7.1 makes [writing-mode] inherited. A declaration on
   a child that restates its ancestor's element-independent value therefore
   contributes nothing in minimal mode, just like the other inherited
   properties. *)
let minimal_drops_a_writing_mode_restatement () =
  let child = node "p" in
  let root = node ~children:[ child ] "div" in
  let result =
    A.compute ~minimal:true
      ~sheet:(parse "div{writing-mode:vertical-rl}p{writing-mode:vertical-rl}")
      [ root ]
  in
  let style n =
    match List.find_opt (fun (m, _) -> Node.equal m n) result.styles with
    | Some (_, decls) -> inline_style decls
    | None -> Alcotest.fail "no assignment for the node"
  in
  Alcotest.(check string)
    "the ancestor establishes writing-mode" "writing-mode:vertical-rl"
    (style root);
  Alcotest.(check string)
    "the inherited restatement is redundant" "" (style child)

(* [minimal] drops a restated inherited declaration because the same text is
   already in force from an ancestor, but a shorthand also resets every longhand
   it does not mention, and an element in between can set one of those: the
   restatement is then the thing that puts it back. css-cascade-5 sec. 6.1 lets
   the middle element's declaration cascade onto the descendant by inheritance,
   so dropping the descendant's shorthand changes the render. The reset sets:
   css-fonts-4 sec. 2.7 for [font], sec. 6.10 for [font-variant], css-lists-3
   sec. 3.4 for [list-style], css-text-4 sec. 3 for [white-space], and
   css-cascade-5 sec. 3.2 for [all], which resets every property there is. A
   property cascade does not type carries no reset set at all, so it counts as
   resetting everything.

   Chrome 146 on [#p{font:16px serif}#mid{font-weight:bold}#c{font:16px serif}]:
   the innermost element computes font-weight 700, and 400 once the [font]
   declaration is dropped. *)
let minimal_keeps_a_restatement_that_resets_a_longhand () =
  let projected ~middle decl =
    let c = node ~id:"c" "span" in
    let mid = node ~id:"mid" ~children:[ c ] "span" in
    let root = node ~id:"p" ~children:[ mid ] "div" in
    let css =
      String.concat "" [ "#p{"; decl; "}#mid{"; middle; "}#c{"; decl; "}" ]
    in
    let result = A.compute ~minimal:true ~sheet:(parse css) [ root ] in
    match List.find_opt (fun (m, _) -> Node.equal m c) result.styles with
    | Some (_, decls) -> inline_style decls
    | None -> Alcotest.fail "no assignment for the innermost element"
  in
  let case expected ~middle decl =
    Alcotest.(check string)
      (decl ^ " under " ^ middle)
      expected (projected ~middle decl)
  in
  let kept ~middle decl = case decl ~middle decl in
  let dropped ~middle decl = case "" ~middle decl in
  (* A longhand in between, and the shorthand restated below it. *)
  kept ~middle:"font-weight:bold" "font:16px serif";
  kept ~middle:"line-height:48px" "font:16px serif";
  kept ~middle:"font-style:italic" "font:16px serif";
  kept ~middle:"list-style-image:url(a.gif)" "list-style:square";
  kept ~middle:"font-variant-caps:small-caps" "font-variant:normal";
  kept ~middle:"text-wrap-mode:nowrap" "white-space:pre-wrap";
  (* The other way round: the shorthand in between resets the longhand the
     descendant restates. *)
  kept ~middle:"font:16px serif" "font-weight:700";
  (* A shorthand cascade does not type, and the one that resets everything. *)
  kept ~middle:"font-variant:small-caps" "font:16px serif";
  kept ~middle:"all:initial" "color:red";
  (* The controls: nothing in between touches a slot the restatement writes, so
     the restatement still goes. *)
  dropped ~middle:"color:red" "font:16px serif";
  dropped ~middle:"font-weight:700" "color:red";
  dropped ~middle:"font-style:italic" "color:red"

(* The whole split of [css] over [roots]: the style attribute written onto [n],
   the <style> body kept beside it, and the count of kept rules. *)
let check_split name ?roots ~css ~inline ~keep ~kept n =
  let result =
    A.compute ~sheet:(parse css) (Option.value roots ~default:[ n ])
  in
  let decls =
    match List.find_opt (fun (m, _) -> Node.equal m n) result.styles with
    | Some (_, decls) -> decls
    | None -> Alcotest.failf "%s: no assignment for the node" name
  in
  Alcotest.(check string) (name ^ ": inline style") inline (inline_style decls);
  Alcotest.(check string) (name ^ ": kept css") keep result.keep_css;
  Alcotest.(check int) (name ^ ": kept rules") kept result.kept

(* A [@scope] block gates its rules on where the element sits in the tree, so it
   has no inline form and stays in the sheet. The properties it sets therefore
   count as dynamic: moving the competing declaration into the style attribute
   would place it above the kept rule, which is not where the author put it. *)
let keeps_property_a_scoped_rule_sets () =
  check_split "scoped" ~css:".x{color:red}@scope(.root){.x{color:blue}}"
    ~inline:"" ~keep:".x{color:red}@scope(.root){.x{color:blue}}" ~kept:2
    (node ~classes:[ "x" ] "p")

(* Same for [@starting-style]: its declarations apply for the frame in which the
   element is inserted, which no style attribute can express. *)
let keeps_property_a_starting_style_rule_sets () =
  check_split "starting-style"
    ~css:".x{color:red}@starting-style{.x{color:blue}}" ~inline:""
    ~keep:".x{color:red}@starting-style{.x{color:blue}}" ~kept:2
    (node ~classes:[ "x" ] "p")

(* A control for both: a conditional block that sets another property competes
   with nothing, so the split is unchanged. *)
let non_competing_scoped_rule_still_inlines () =
  check_split "scoped, other property"
    ~css:".x{color:red}@scope(.root){.x{margin:0}}" ~inline:"color:red"
    ~keep:"@scope(.root){.x{margin:0}}" ~kept:1
    (node ~classes:[ "x" ] "p")

(* A kept rule and an inlinable one can compete under different property names:
   [margin] resets [margin-right] (css-box-4 sec. 4.2), so a kept rule setting
   the longhand is one the shorthand can overwrite. Inlining the shorthand puts
   it in a style attribute, which css-cascade-5 sec. 6.3 ranks above every
   selector, and the kept rule loses a fight it wins in the browser. *)
let keeps_shorthand_a_kept_longhand_writes () =
  check_split "shorthand over kept longhand"
    ~css:
      "ul{margin:0}.m{margin-right:20px}@media(min-width:1px){.q{margin-right:0}}"
    ~inline:""
    ~keep:
      "ul{margin:0}.m{margin-right:20px}@media(min-width:1px){.q{margin-right:0}}"
    ~kept:3
    (node ~classes:[ "m" ] "ul")

(* The same the other way round: a kept shorthand can overwrite an inlinable
   longhand, so the longhand has to stay in the sheet too. *)
let keeps_longhand_a_kept_shorthand_writes () =
  check_split "longhand under kept shorthand"
    ~css:".x{margin-top:1px}@media(min-width:1px){.q{margin:0}}" ~inline:""
    ~keep:".x{margin-top:1px}@media(min-width:1px){.q{margin:0}}" ~kept:2
    (node ~classes:[ "x" ] "p")

(* A flow-relative property and its physical twin write one slot once the
   writing mode is known (css-logical-1 sec. 2), and the stylesheet does not
   know it, so [margin-inline-end] and [margin-right] have to be treated as the
   same competition. *)
let keeps_logical_a_kept_physical_writes () =
  check_split "logical under kept physical"
    ~css:".x{margin-inline-end:30px}@media(min-width:1px){.q{margin-right:0}}"
    ~inline:""
    ~keep:".x{margin-inline-end:30px}@media(min-width:1px){.q{margin-right:0}}"
    ~kept:2
    (node ~classes:[ "x" ] "p")

(* The control: a kept rule that writes another family competes with nothing, so
   the shorthand still projects onto the element. *)
let unrelated_kept_property_still_inlines () =
  check_split "shorthand, unrelated kept property"
    ~css:"ul{margin:0}@media(min-width:1px){.q{color:red}}" ~inline:"margin:0"
    ~keep:"@media(min-width:1px){.q{color:red}}" ~kept:1
    (node ~classes:[ "m" ] "ul")

(* selectors-4 sec. 6.3: an [i] flag matches the attribute's value ASCII
   case-insensitively, so the rule projects onto an element carrying the value
   in another case. *)
let projects_attribute_rule_with_case_flag () =
  check_split "case-insensitive attribute" ~css:"p[data-k=\"X\" i]{color:red}"
    ~inline:"color:red" ~keep:"" ~kept:0
    (node ~attrs:[ ("data-k", "x") ] "p")

(* Inlining takes the rule out of the sheet, so the projection has to agree with
   the browser that reads what is left. Selectors 4 sec. 6.3 gives [s]
   "identical to" semantics and every engine refuses the selector outright; sec.
   13.2 matches an element holding only document white space, and its own note
   records that Level 2 and Level 3 did not - the change engines have not taken.
   Painting either element writes a style the page did not have. *)
let keeps_a_rule_no_engine_implements () =
  check_split "case-sensitive attribute" ~css:"p[data-k=\"X\" s]{color:red}"
    ~inline:"" ~keep:"p[data-k=X s]{color:red}" ~kept:1
    (node ~attrs:[ ("data-k", "X") ] "p");
  check_split "white space alone" ~css:"p:empty{color:red}" ~inline:""
    ~keep:"p:empty{color:red}" ~kept:1 (node ~text:[ " " ] "p")

(* The control: without the flag the selector is representable, so it projects
   onto the element and leaves nothing behind. *)
let projects_attribute_rule_to_inline_style () =
  check_split "attribute" ~css:"p[data-k=\"X\"]{color:red}" ~inline:"color:red"
    ~keep:"" ~kept:0
    (node ~attrs:[ ("data-k", "X") ] "p")

(* Namespaces are not modelled either, so the matcher would answer for [svg|p]
   what it answers for [p] and paint an HTML paragraph. *)
let keeps_rule_with_namespaced_element () =
  check_split "namespaced element"
    ~css:"@namespace svg url(http://www.w3.org/2000/svg);svg|p{color:red}"
    ~inline:""
      (* The minified printer writes the namespace URL as a string, and the
         [@namespace] statement counts towards the kept statements. *)
    ~keep:"@namespace svg\"http://www.w3.org/2000/svg\";svg|p{color:red}"
    ~kept:2 (node "p")

(* [>>>] is a legacy shadow-piercing combinator with no tree relation behind it,
   so the matcher has no answer for it and the rule stays in the sheet. *)
let keeps_rule_with_shadow_piercing_combinator () =
  let span = node "span" in
  let root = node ~children:[ node ~children:[ span ] "div" ] "section" in
  check_split "shadow-piercing" ~roots:[ root ] ~css:"div>>>span{color:red}"
    ~inline:"" ~keep:"div>>>span{color:red}" ~kept:1 span

(* The control: the descendant combinator is modelled, so the same shape of rule
   projects onto the span. *)
let projects_descendant_combinator_rule () =
  let span = node "span" in
  let root = node ~children:[ node ~children:[ span ] "div" ] "section" in
  check_split "descendant" ~roots:[ root ] ~css:"div span{color:red}"
    ~inline:"color:red" ~keep:"" ~kept:0 span

(* [kept] is reported to the user as a number of rules, so a grouping at-rule
   contributes the rules inside it rather than counting once for its wrapper: a
   @media block holding three rules keeps three rules out of the inline
   projection. *)
let kept_counts_the_rules_a_media_block_holds () =
  let n = node ~classes:[ "a" ] "p" in
  let result =
    A.compute
      ~sheet:
        (parse
           "@media(min-width:10px){.a{color:red}.b{color:blue}.c{color:green}}")
      [ n ]
  in
  Alcotest.(check int) "kept rules" 3 result.kept

(* An at-rule that holds no rules of its own is itself the one thing kept, so it
   counts once. *)
let kept_counts_a_rule_less_at_rule_once () =
  let n = node ~classes:[ "a" ] "p" in
  let result =
    A.compute ~sheet:(parse "@font-face{font-family:x;src:url(a.woff2)}") [ n ]
  in
  Alcotest.(check int) "kept rules" 1 result.kept

(* A stylesheet the parser had to recover and an empty one project onto exactly
   the same nothing, so the projection cannot be what tells them apart. Taking a
   parsed sheet leaves that to {!Css.of_string}, whose warnings the caller reads
   before any of this runs; parsing the CSS text here would swallow them and
   report both as the same empty result. ["a{"] projects onto the same nothing
   for a third reason: CSS Syntax 3 closes the block at EOF, so the rule is
   valid and holds no declarations, but the CR snapshot calls that EOF branch a
   parse error and the repair is reported. A correct rule and a reportable
   defect are not in conflict. *)
let invalid_css_is_not_empty_css () =
  let warnings css =
    match Css.of_string css with
    | Ok p -> p.Css.warnings
    | Error e ->
        Alcotest.failf "unexpected fatal parse error: %s" (Error.to_string e)
  in
  Alcotest.(check bool)
    "recovered CSS carries diagnostics" true
    (warnings "@@@@ }}} {{{ !!! ;;;" <> []);
  Alcotest.(check bool) "empty CSS carries none" true (warnings "" = []);
  Alcotest.(check bool)
    "a block repaired at EOF carries a diagnostic" true
    (warnings "a{" <> []);
  List.iter
    (fun css ->
      let result = A.compute ~sheet:(parse css) [ node "div" ] in
      Alcotest.(check string)
        ("inline declarations: " ^ css)
        ""
        (match result.styles with
        | [ (_, decls) ] -> inline_style decls
        | _ -> Alcotest.fail "expected one inline assignment");
      Alcotest.(check string) ("kept css: " ^ css) "" result.keep_css;
      Alcotest.(check int) ("kept count: " ^ css) 0 result.kept)
    [ "@@@@ }}} {{{ !!! ;;;"; ""; "a{" ]

(* [Apply] resolves through the same declaration reader, so a value the reader
   dropped over its own [!important] never reached the resolved style. CSS
   Syntax 3 (ED) sec. 5.5.6 lifts that tail out of the value before any grammar
   sees it, and CSS Cascade 5 sec. 6.2 ranks what it flags above a normal
   declaration of the same property. Asserted on the declaration rather than on
   the serialised attribute, which orders the two ranks. *)
let important_declaration_reaches_the_resolved_style () =
  List.iter
    (fun (css, property, value, important) ->
      let n = node ~classes:[ "x" ] "p" in
      let result = A.compute ~sheet:(parse css) [ n ] in
      let decls =
        match result.styles with
        | [ (_, decls) ] -> decls
        | _ -> Alcotest.failf "expected one inline assignment for %s" css
      in
      match
        List.find_opt
          (fun d -> String.equal (Css.Declaration.property_name d) property)
          decls
      with
      | None ->
          Alcotest.failf "%s: %s did not reach the resolved style" css property
      | Some d ->
          Alcotest.(check string)
            (css ^ ": " ^ property)
            value
            (Css.Declaration.string_of_value ~minify:true d);
          Alcotest.(check bool)
            (css ^ ": " ^ property ^ " keeps its flag")
            important
            (Css.Declaration.is_important d))
    [
      ( ".x{font-style:oblique !important;color:red}",
        "font-style",
        "oblique",
        true );
      ( ".x{color-scheme:dark !important;color:red}",
        "color-scheme",
        "dark",
        true );
      (".x{text-box:none;color:red}", "text-box", "none", false);
      (* The flag still decides the cascade: the important declaration wins over
         a later normal one for the same property. *)
      (".x{color:red !important}.x{color:blue}", "color", "red", true);
    ]

let suite =
  ( "apply",
    [
      Alcotest.test_case "projects static rule to inline style" `Quick
        projects_static_rule_to_inline_style;
      Alcotest.test_case "important declaration reaches the resolved style"
        `Quick important_declaration_reaches_the_resolved_style;
      Alcotest.test_case "keeps stateful rule in css" `Quick
        keeps_stateful_rule_in_css;
      Alcotest.test_case "projects layered rule to inline style" `Quick
        projects_layered_rule_to_inline_style;
      Alcotest.test_case "keeps stateful rule inside layer in css" `Quick
        keeps_stateful_rule_inside_layer_in_css;
      Alcotest.test_case "unlayered beats layer" `Quick unlayered_beats_layer;
      Alcotest.test_case "layer beats unlayered for important" `Quick
        layer_beats_unlayered_important;
      Alcotest.test_case "later layer wins normal, earlier wins important"
        `Quick later_layer_wins_normal_earlier_wins_important;
      Alcotest.test_case "layer statement orders the blocks" `Quick
        layer_statement_orders_the_blocks;
      Alcotest.test_case "layer outranks specificity" `Quick
        layer_outranks_specificity;
      Alcotest.test_case "non-competing layers both project" `Quick
        non_competing_layers_both_project;
      Alcotest.test_case "style attribute keeps author order" `Quick
        style_attribute_keeps_author_order;
      Alcotest.test_case "malformed style attribute is dropped" `Quick
        malformed_style_attribute_is_dropped;
      Alcotest.test_case "minimal keeps what a UA rule would win" `Quick
        minimal_keeps_what_a_ua_rule_would_win;
      Alcotest.test_case "minimal keeps a relative value" `Quick
        minimal_keeps_a_relative_value;
      Alcotest.test_case "minimal drops a writing-mode restatement" `Quick
        minimal_drops_a_writing_mode_restatement;
      Alcotest.test_case "minimal keeps a restatement that resets a longhand"
        `Quick minimal_keeps_a_restatement_that_resets_a_longhand;
      Alcotest.test_case "keeps the property a scoped rule sets" `Quick
        keeps_property_a_scoped_rule_sets;
      Alcotest.test_case "keeps the property a starting-style rule sets" `Quick
        keeps_property_a_starting_style_rule_sets;
      Alcotest.test_case "non-competing scoped rule still inlines" `Quick
        non_competing_scoped_rule_still_inlines;
      Alcotest.test_case "keeps a shorthand a kept longhand writes" `Quick
        keeps_shorthand_a_kept_longhand_writes;
      Alcotest.test_case "keeps a longhand a kept shorthand writes" `Quick
        keeps_longhand_a_kept_shorthand_writes;
      Alcotest.test_case "keeps a logical property a kept physical one writes"
        `Quick keeps_logical_a_kept_physical_writes;
      Alcotest.test_case "unrelated kept property still inlines" `Quick
        unrelated_kept_property_still_inlines;
      Alcotest.test_case "projects an attribute rule with a case flag" `Quick
        projects_attribute_rule_with_case_flag;
      Alcotest.test_case "keeps a rule no engine implements" `Quick
        keeps_a_rule_no_engine_implements;
      Alcotest.test_case "projects an attribute rule to inline style" `Quick
        projects_attribute_rule_to_inline_style;
      Alcotest.test_case "keeps a rule with a namespaced element" `Quick
        keeps_rule_with_namespaced_element;
      Alcotest.test_case "keeps a rule with a shadow-piercing combinator" `Quick
        keeps_rule_with_shadow_piercing_combinator;
      Alcotest.test_case "projects a descendant combinator rule" `Quick
        projects_descendant_combinator_rule;
      Alcotest.test_case "kept counts the rules a media block holds" `Quick
        kept_counts_the_rules_a_media_block_holds;
      Alcotest.test_case "kept counts a rule-less at-rule once" `Quick
        kept_counts_a_rule_less_at_rule_once;
      Alcotest.test_case "invalid css is not empty css" `Quick
        invalid_css_is_not_empty_css;
    ] )
