open Cascade

let statements css =
  match Css.of_string ~strict:false css with
  | Ok { stylesheet; _ } -> Css.statements stylesheet
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let render stmts = Pp.to_string ~minify:true Stylesheet.pp_stylesheet stmts
let canonical css = statements css |> Rule_order.canonicalize |> render

let sorts_independent_rules_into_content_order () =
  Alcotest.(check string)
    "independent rules sort by serialized content" ".a{margin:0}.b{color:red}"
    (canonical ".b{color:red}.a{margin:0}")

let keeps_conflicting_rules_in_source_order () =
  (* Same-selector rules coalesce and the merged declarations reduce with the
     cascade dedup: the last write of a property wins and the dead earlier one
     is dropped, so the two source orders keep their opposite winners and stay
     distinct. *)
  Alcotest.(check string)
    "same selector and property keeps the last winner" ".a{color:blue}"
    (canonical ".a{color:red}.a{color:blue}");
  Alcotest.(check string)
    "opposite source order keeps the opposite winner" ".a{color:red}"
    (canonical ".a{color:blue}.a{color:red}")

(* Two rules writing one value tie and may swap, but each still follows an
   earlier rule writing another value to an element both can match: on [class="z
   a"] the source makes [.a] win, so [.a] stays after [.z] although it sorts
   first. *)
let tied_writers_follow_an_earlier_conflict () =
  Alcotest.(check string)
    "the second writer of a value stays after the conflict"
    ".z{color:red}.a{color:blue}.j{color:blue}"
    (canonical ".z{color:red}.j{color:blue}.a{color:blue}")

let custom_property_position_converges () =
  (* A custom-property definition and a [var()] user are cascade-independent, so
     both source orderings reach one canonical form. *)
  let want = ".x{filter:blur(var(--b))}:root{--b:1}" in
  Alcotest.(check string)
    "definition after the user" want
    (canonical ".x{filter:blur(var(--b))}:root{--b:1}");
  Alcotest.(check string)
    "definition before the user" want
    (canonical ":root{--b:1}.x{filter:blur(var(--b))}")

let custom_property_importance_survives () =
  (* Cascade 5 sec. 6.2: an important declaration beats a normal one whatever
     the order, so [!important] on a custom property is cascade-significant and
     the projection has to carry it over. *)
  Alcotest.(check string)
    "important is kept" ":root{--x:red!important}"
    (canonical ":root{--x:red !important}");
  Alcotest.(check bool)
    "an important definition does not project like a normal one" true
    (canonical ":root{--x:red !important}" <> canonical ":root{--x:red}")

let custom_property_layer_and_meta_survive () =
  (* A custom declaration also carries the cascade layer it was written in and
     its caller metadata. The projection rebuilds the declaration from its
     value, so both have to be carried over with it. *)
  let inject, project = Css.meta () in
  let decl, _ =
    Css.var ~layer:"theme" ~meta:(inject "tag") "x" Css.Length (Css.Rem 1.)
  in
  let projected =
    Rule_order.canonicalize [ Css.rule ~selector:(Selector.v ":root") [ decl ] ]
  in
  match List.map Css.Stylesheet.statement_declarations projected with
  | [ [ d ] ] ->
      Alcotest.(check (option string))
        "layer survives" (Some "theme")
        (Declaration.custom_declaration_layer d);
      Alcotest.(check (option string))
        "meta survives" (Some "tag")
        (Option.bind (Declaration.meta_of_declaration d) project)
  | ds ->
      Alcotest.failf "expected one rule holding one declaration, got %d rules"
        (List.length ds)

let custom_property_font_name_quoting_converges () =
  (* CSS Fonts 4 sec. 2.1.1: a multi-word family name spells the same family
     quoted or as the ident sequence it unquotes to, and a custom property
     holding a font stack substitutes either form identically into
     [font-family]. Emission keeps whichever the author wrote, so only the
     projection can bring the two spellings together. *)
  let quoted =
    {|:root{--font-sans:ui-sans-serif,system-ui,"Noto Color Emoji"}|}
  in
  let unquoted =
    ":root{--font-sans:ui-sans-serif,system-ui,Noto Color Emoji}"
  in
  Alcotest.(check string)
    "the quoted spelling folds onto the ident sequence" (canonical unquoted)
    (canonical quoted);
  (* Gated on a generic family being present: without one the property is
     type-unknown, and a single-word string could be a [<custom-ident>] some
     other substitution context reads as a string. *)
  Alcotest.(check bool)
    "a single-word string stays opaque" true
    (canonical {|.a{--x:"foo"}|} <> canonical ".a{--x:foo}")

(* A custom property is an opaque token stream and the AST keeps the spelling
   the author wrote, so a run of spaces after a comma survives pretty-printing
   and a comparison meets two spellings of one value wherever two minifiers
   wrote the inputs. The projection settles that by re-serialising every custom
   value through its minified form, which is what
   {!Declaration.map_custom_value} does on the way in. A comma inside a string
   is text rather than a token separator, so the space beside it is content. *)
let custom_property_comma_spacing_normalizes () =
  let projected css =
    Pp.to_string ~minify:false Stylesheet.pp_stylesheet
      (Rule_order.canonicalize (statements css))
  in
  Alcotest.(check string)
    "the space after a top-level comma goes"
    (projected ":root{--a:var(--b),var(--c)}")
    (projected ":root{--a:var(--b), var(--c)}");
  Alcotest.(check string)
    "and so does a run of them"
    (projected ":root{--a:foo,bar}")
    (projected ":root{--a:foo,   bar}");
  Alcotest.(check bool)
    "a comma inside a string is text, not a separator" true
    (projected {|:root{--a:"x, y"}|} <> projected {|:root{--a:"x,y"}|})

(* Every block at-rule that holds a statement list, each as the CSS text that
   opens and closes it. [@else] has no standalone form (css-conditional-5 sec. 3
   binds it to the [@when] before it), so its opener carries the antecedent.
   [Origin] has no CSS syntax at all and is exercised separately. *)
let block_at_rules =
  [
    ("@layer", "@layer L{", "}");
    ("@media", "@media (width>0px){", "}");
    ("@container", "@container (width>0px){", "}");
    ("@supports", "@supports (color:red){", "}");
    ("@-moz-document", "@-moz-document url-prefix(){", "}");
    ("@starting-style", "@starting-style{", "}");
    ("@when", "@when media(width>0px){", "}");
    ("@else", "@when media(width<0px){.z{color:red}}@else{", "}");
    ("@scope", "@scope (.card){", "}");
  ]

(* A projection fold reads a declaration or a media prelude; neither depends on
   the block at-rule the statement was written in, since a wrapper decides when
   its contents apply and never what they mean. So a pair that converges at the
   top level converges inside every wrapper - otherwise the canonical diff
   answers differently for the same rule depending on what encloses it, which is
   the phantom difference the projection exists to remove. *)
let converges_everywhere name a b =
  Alcotest.(check string)
    (String.concat " " [ name; "at the top level" ])
    (canonical b) (canonical a);
  List.iter
    (fun (label, before, after) ->
      let wrap body = String.concat "" [ before; body; after ] in
      Alcotest.(check string)
        (String.concat " " [ name; "inside"; label ])
        (canonical (wrap b))
        (canonical (wrap a)))
    block_at_rules;
  let origin css =
    render
      (Rule_order.canonicalize [ Stylesheet.Origin (Author, statements css) ])
  in
  Alcotest.(check string)
    (String.concat " " [ name; "inside an origin wrapper" ])
    (origin b) (origin a)

let custom_font_quoting_converges_under_every_wrapper () =
  converges_everywhere "a quoted family name" {|.a{--f:"Fira Code",monospace}|}
    ".a{--f:Fira Code,monospace}"

let color_spelling_converges_under_every_wrapper () =
  (* CSS Color 4 sec. 10.2 scales each [color(srgb ...)] channel by 255, so
     [color(srgb 1 0 0)] is the sRGB red the keyword names. *)
  converges_everywhere "a whole-byte color(srgb)" ".a{color:color(srgb 1 0 0)}"
    ".a{color:red}"

let media_level_spelling_converges_under_every_wrapper () =
  converges_everywhere "the Level 3 negation"
    "@media not all and (width>100px){.a{color:red}}"
    "@media not (width>100px){.a{color:red}}"

let at_rule_block_takes_the_projection_folds () =
  (* The projection's value folds apply to a declaration wherever it sits: an
     at-rule that groups rules holds the same rules a caller could have written
     at the top level, so two sheets differing only by a folded spelling inside
     one still project alike. *)
  Alcotest.(check string)
    "a media query folds inside @-moz-document"
    (canonical
       "@-moz-document url-prefix(){@media not (min-width:1px){.a{color:red}}}")
    (canonical
       "@-moz-document url-prefix(){@media not all and \
        (min-width:1px){.a{color:red}}}");
  Alcotest.(check string)
    "a font stack unquotes inside @scope"
    (canonical
       "@scope (.p){:root{--font-sans:ui-sans-serif,system-ui,Noto Color \
        Emoji}}")
    (canonical
       {|@scope (.p){:root{--font-sans:ui-sans-serif,system-ui,"Noto Color Emoji"}}|})

let media_and_independent_rule_converge () =
  (* A conditional block whose rules cannot conflict with a neighbouring rule
     reorders with it, so both source orderings reach one canonical form. *)
  let grouped = ".a{display:block}@media (min-width:48rem){.b{flex-grow:1}}" in
  let split = "@media (min-width:48rem){.b{flex-grow:1}}.a{display:block}" in
  Alcotest.(check string)
    "both orders converge" (canonical grouped) (canonical split);
  Alcotest.(check bool)
    "rule sorts before the block" true
    (String.length (canonical split) > 2
    && String.sub (canonical split) 0 2 = ".a")

let media_conflict_keeps_source_order () =
  (* The media rule writes the same property on the same selector, so the
     relative order is cascade-significant and both spellings stay put. *)
  let media_first = "@media print{.a{display:flex}}.a{display:block}" in
  let rule_first = ".a{display:block}@media print{.a{display:flex}}" in
  Alcotest.(check string)
    "media first stays"
    (render (statements media_first))
    (canonical media_first);
  Alcotest.(check string)
    "rule first stays"
    (render (statements rule_first))
    (canonical rule_first)

let layer_blocks_stay_put () =
  (* [@layer] order pins cascade-layer priority at first occurrence; layer
     blocks are barriers. The projection writes the order out as one statement,
     so the statement and the blocks name the same order and project
     together. *)
  let css = "@layer b{.y{color:blue}}@layer a{.x{color:red}}" in
  Alcotest.(check string)
    "layer blocks keep source order"
    "@layer b,a;@layer b{.y{color:blue}}@layer a{.x{color:red}}" (canonical css)

let pinned_layer_blocks_take_the_layer_order () =
  (* CSS Cascade 5 sec. 6.1 sorts by layer before order of appearance, so the
     projection writes the layer order out as one statement and leaves the
     blocks to sit in it. A body of style rules under [@starting-style] cascades
     by layer the same way and moves too, across unlayered rules. *)
  Alcotest.(check string)
    "starting-style body sorted"
    "x{color:red}@layer a,b;@layer a{y{color:blue}}@layer \
     b{@starting-style{x{opacity:0}}}"
    (canonical
       "@layer a,b;@layer b{@starting-style{x{opacity:0}}}x{color:red}@layer \
        a{y{color:blue}}");
  Alcotest.(check string)
    "a block holding a sublayer stays"
    "@layer a,b;@layer b{@layer c;@layer c{x{color:red}}}@layer \
     a{y{color:blue}}"
    (canonical
       "@layer a,b;@layer b{@layer c{x{color:red}}}@layer a{y{color:blue}}")

let redundant_layer_pin_folds () =
  (* CSS Cascade 5 sec. 6.4.3: cascade layers are sorted by the order in which
     they first are declared. The projection states that order once, so the pin
     and the block that repeats it reach one form. *)
  Alcotest.(check string)
    "a pin the following block repeats reaches one form"
    "@layer a;@layer a{x{color:red}}"
    (canonical "@layer a;@layer a{x{color:red}}")

let order_changing_layer_pin_is_kept () =
  (* Sec. 6.4.3 read the other way: here the pins are the only thing putting [a]
     before [b], since [b]'s block comes first. The projection states that
     order, so [a] stays first and the pinned and unpinned sheets stay apart. *)
  let pinned =
    "@layer a;@layer b;@layer b{x{color:red}}@layer a{y{color:blue}}"
  in
  let unpinned = "@layer b{x{color:red}}@layer a{y{color:blue}}" in
  Alcotest.(check string)
    "the order the pin fixes stays"
    "@layer a,b;@layer a{y{color:blue}}@layer b{x{color:red}}"
    (canonical pinned);
  Alcotest.(check bool)
    "pinned and unpinned sheets stay distinct" true
    (canonical pinned <> canonical unpinned)

let leading_layer_pins_matching_the_blocks_fold () =
  (* The shape a generator writes: every layer named up front, then the blocks
     in that same order. The projection states that order either way. *)
  Alcotest.(check string)
    "pins repeating the block order reach one form"
    "@layer a,b;@layer a{x{color:red}}@layer b{y{color:blue}}"
    (canonical "@layer a,b;@layer a{x{color:red}}@layer b{y{color:blue}}");
  Alcotest.(check string)
    "written as two statements they reach the same form"
    "@layer a,b;@layer a{x{color:red}}@layer b{y{color:blue}}"
    (canonical "@layer a;@layer b;@layer a{x{color:red}}@layer b{y{color:blue}}");
  Alcotest.(check bool)
    "the reversed pin order is another sheet and stays" true
    (canonical "@layer b,a;@layer a{x{color:red}}@layer b{y{color:blue}}"
    <> canonical "@layer a{x{color:red}}@layer b{y{color:blue}}")

let layer_pin_folds_one_name_not_the_statement () =
  (* Sec. 6.4.4.2: [@layer a, b;] declares two layers in that order. The
     projection writes the full order it reads, so the statement and the blocks
     reach one form whatever spelling put [a] before [b]. *)
  Alcotest.(check string)
    "the order the pin gives, written by the blocks"
    "@layer a,b;@layer a{y{color:blue}}@layer b{x{color:red}}"
    (canonical "@layer a,b;@layer b{x{color:red}}@layer a{y{color:blue}}");
  Alcotest.(check string)
    "a pin kept for a name no block declares"
    "@layer a,c,b;@layer a{y{color:blue}}@layer b{x{color:red}}"
    (canonical "@layer a,c,b;@layer b{x{color:red}}@layer a{y{color:blue}}")

let layer_pin_names_are_ident_lists () =
  (* Sec. 6.4.2: [a.b] is the sublayer [b] of [a] and declares [a] on the way,
     so the order names [a] then [a.b]. An escaped dot makes one ident instead,
     a layer of its own. *)
  Alcotest.(check string)
    "a sublayer names its parent on the way"
    "@layer a,a.b;@layer a.b{x{color:red}}"
    (canonical "@layer a;@layer a.b{x{color:red}}");
  Alcotest.(check bool)
    "a pin naming an ident that holds a dot stays" true
    (canonical "@layer a\\2e b;@layer a.b{x{color:red}}"
    <> canonical "@layer a.b{x{color:red}}")

let unblocked_layer_pin_is_kept () =
  (* A pin whose layer never gets a block of its own is the only declaration of
     that layer, and sec. 6.4.3 sorts it before every layer declared after
     it. *)
  Alcotest.(check string)
    "a pin with no block of its own stays" "@layer a,b;@layer b{x{color:red}}"
    (canonical "@layer a;@layer b{x{color:red}}")

let layer_pin_over_conditional_declaration_is_kept () =
  (* Sec. 6.4.3: a layer declared inside a conditional group rule contributes to
     the order only when the condition holds, so no name is hoisted across the
     [@media]; the run before it keeps the pin and the run after it states its
     own order. *)
  let css =
    "@layer a;@media print{@layer a{x{color:red}}}@layer \
     b{y{color:blue}}@layer a{z{color:green}}"
  in
  Alcotest.(check string)
    "a pin over an unreadable position stays"
    "@layer a;@media print{@layer a;@layer a{x{color:red}}}@layer b,a;@layer \
     b{y{color:blue}}@layer a{z{color:green}}"
    (canonical css)

let layer_pin_over_conditional_layer_is_the_only_position () =
  (* The test above leaves an [@layer b] block between the pin and the block
     that repeats it, and that named position keeps the pin whatever the
     [@media] contributes. Here the conditional block is the only thing between
     them, so whether it raises a position at all is the whole question. *)
  let over_a_layer =
    "@layer a;@media print{@layer q{x{color:red}}}@layer a{y{color:blue}}"
  in
  let over_plain_rules =
    "@layer a;@media print{x{color:red}}@layer a{y{color:blue}}"
  in
  Alcotest.(check string)
    "a pin over a block declaring a layer stays"
    "@layer a;@media print{@layer q;@layer q{x{color:red}}}@layer a;@layer \
     a{y{color:blue}}"
    (canonical over_a_layer);
  Alcotest.(check string)
    "the same pin over a block declaring none folds"
    "@media print{x{color:red}}@layer a;@layer a{y{color:blue}}"
    (canonical over_plain_rules)

let layer_pin_over_import_is_kept () =
  (* Sec. 6.4.1: an [@import] with no layer keyword puts the imported sheet's
     own layers into this order at that point, so the position between the pin
     and the block is unknown here too. *)
  let css = "@layer a;@import url(x.css);@layer a{y{color:red}}" in
  Alcotest.(check string)
    "a pin over an import stays"
    "@layer a;@import\"x.css\";@layer a{y{color:red}}" (canonical css)

let layer_pin_fold_is_idempotent () =
  (* The projection is a projection: what it emits is already canonical. *)
  let once =
    canonical "@layer a,b;@layer b{x{color:red}}@layer a{y{color:blue}}"
  in
  Alcotest.(check string)
    "projecting the projection changes nothing" once (canonical once)

let block_with_layer_content_is_barrier () =
  (* A conditional block is only reorderable when its transitive content is
     plain rules; a nested [@layer] pins it in place, and the run inside states
     its own order. *)
  let css = "@media print{@layer a{.x{color:red}}}.b{margin:0}" in
  Alcotest.(check string)
    "media wrapping a layer stays put"
    "@media print{@layer a;@layer a{.x{color:red}}}.b{margin:0}" (canonical css)

let hoisted_group_converges_with_inline () =
  (* A shared declaration hoisted into a selector-list group is the same
     declaration written inline, so both factorings project to one form. *)
  Alcotest.(check string)
    "hoisted and inline forms converge"
    (canonical
       ".absolute{position:absolute}.sr-only{position:absolute;white-space:nowrap}")
    (canonical
       ".absolute,.sr-only{position:absolute}.sr-only{white-space:nowrap}")

let selector_list_expands_without_coalesce () =
  (* Every selector-list rule expands onto its branches, whether or not a branch
     occurs elsewhere. [.a,.b{margin:0}] and [.a{margin:0}.b{margin:0}] are the
     same stylesheet, so they have to project to the same form; leaving a list
     with no shared branch grouped made the projection depend on how the input
     happened to be written. *)
  Alcotest.(check string)
    "a list rule expands onto its branches" ".a{margin:0}.b{margin:0}"
    (canonical ".a,.b{margin:0}")

let selector_list_expands_when_a_branch_is_shared () =
  (* When a branch also appears in another rule, the list is expanded so the
     coalesce can fold that branch; the non-shared branch keeps its
     declarations. *)
  Alcotest.(check string)
    "shared branch expands and coalesces" ".a{margin:0}.b{color:red;margin:0}"
    (canonical ".a,.b{margin:0}.b{color:red}")

(* Two occurrences of [.a] fold when no chain of conflicts leads from the first
   to the second, even where each one alone is held by a neighbour: [.b] must
   follow the first (width) and [.c] precede the second (height), and neither
   orders the other, so [.c], then [.a] holding both, then [.b] keeps every
   constrained pair. The sheet written that way projects to the same form. *)
let coalesce_through_two_held_occurrences () =
  let want = ".c{height:3px}.a{height:4px;width:1px}.b{width:2px}" in
  Alcotest.(check string)
    "both occurrences held" want
    (canonical ".a{width:1px}.b{width:2px}.c{height:3px}.a{height:4px}");
  Alcotest.(check string)
    "already folded" want
    (canonical ".c{height:3px}.a{height:4px;width:1px}.b{width:2px}");
  (* [.b] now writes both, so the first [.a] precedes it and it precedes the
     second: the chain orders the two occurrences and they stay apart. *)
  Alcotest.(check string)
    "a chain between them keeps them apart"
    ".a{width:1px}.b{height:5px;width:2px}.a{height:4px}"
    (canonical ".a{width:1px}.b{width:2px;height:5px}.a{height:4px}");
  Alcotest.(check string)
    "and a chain through two rules"
    ".a{width:1px}.b{color:blue;width:2px}.d{color:red;height:6px}.a{height:4px}"
    (canonical
       ".a{width:1px}.b{width:2px;color:blue}.d{color:red;height:6px}.a{height:4px}")

(* A fold moves an occurrence's writes to the position it folds into, so a later
   fold across that position meets them there. The first [.o] folds down into
   the second; the second [.g] then may not climb past that position, where
   [width:76px] now stands, since on [class="g o"] the source makes [.g]'s width
   win. *)
let coalesce_reads_writes_a_fold_moved () =
  let want =
    ".o{display:none;width:76px}.g{display:none;width:24px}.p{display:block}"
  in
  Alcotest.(check string)
    "the moved width still holds the climb" want
    (canonical
       ".o{width:76px}.g,.o{display:none}.p{display:block}.g{width:24px}");
  Alcotest.(check string)
    "the folded sheet" want
    (canonical
       ".o{width:76px;display:none}.g{display:none;width:24px}.p{display:block}")

(* Reduced from tmall-1 against its [--objective=raw] output. Each conflict is
   one declaration against one other: [.t]'s colour precedes [.l]'s, its width
   precedes [.v]'s, and [.l]'s [padding-top] precedes [.t]'s. Neither [.t]
   occurrence is ordered against the other or against the second [.l], so the
   sheet that folds [.t] and the one that leaves it split are one sheet, and
   Chromium paints them alike. Folding one held pair first must not strand the
   other. Moving [.t]'s [padding-top] before [.l]'s is another sheet. *)
let fold_order_does_not_decide_the_form () =
  let split =
    ".t{color:red;width:20px}.l{padding-top:0}.v{width:30px}.t{padding-top:10px}.l{color:blue}"
  in
  let folded =
    ".l{padding-top:0}.t{color:red;width:20px;padding-top:10px}.v{width:30px}.l{color:blue}"
  in
  let reordered =
    ".t{color:red;width:20px;padding-top:10px}.l{padding-top:0}.v{width:30px}.l{color:blue}"
  in
  Alcotest.(check string)
    "the split and the folded sheet" (canonical folded) (canonical split);
  Alcotest.(check bool)
    "a padding moved past the one it follows stays apart" true
    (canonical split <> canonical reordered)

(* A guarded write that repeats an earlier unguarded one changes nothing only
   where it could stand right after it, and that is not so here: the repeat is
   what overrides the guarded [margin:0] before it, and without it the
   [margin:0] writes the top margin again. Under [screen], [class="a b"] takes
   [margin-top:1px] from the first sheet of each pair and 0 from the second, as
   Chromium paints them. *)
let a_repeat_keeps_what_it_overrides () =
  Alcotest.(check bool)
    "a guarded shorthand between the write and its repeat" true
    (canonical
       ".a{margin-top:1px}@media screen{.a{margin:0}}@media \
        screen{.a{margin-top:1px}}"
    <> canonical ".a{margin-top:1px}@media screen{.a{margin:0}}");
  Alcotest.(check bool)
    "a guarded shorthand the dropped repeat overrode" true
    (canonical
       "@media screen{.a{margin:0}}.b{margin-top:5px}.a{margin-top:1px}@media \
        screen{.a{margin-top:1px}}"
    <> canonical
         ".b{margin-top:5px}.a{margin-top:1px}@media screen{.a{margin:0}}")

(* Two adjacent rules over one selector list hold, for every element, the
   declarations of the one rule that list over both bodies, in the same order.
   Read branch by branch, [speak], which the footprint model cannot place and so
   reads as touching [top], chains the [.b] occurrences through [.a]'s, so the
   list has to be joined before it is expanded. *)
let adjacent_lists_join_before_expanding () =
  let want = ".a{speak:none;top:7px}.b{speak:none;top:7px}" in
  Alcotest.(check string)
    "two rules over one list" want
    (canonical ".a,.b{speak:none}.a,.b{top:7px}");
  Alcotest.(check string)
    "one rule over the list" want
    (canonical ".a,.b{speak:none;top:7px}")

(* Two rules with one declaration list apply that list twice to an element both
   match, whichever comes first, so they never decide each other. Two different
   declarations in the list may still touch, as an unplaced [-moz-] longhand
   touches everything, and that alone must not order the two. In the first body
   [-webkit-animation-duration] is [animation-duration] under its legacy name
   (WHATWG Compatibility Standard sec. 3.4.1), and the [-moz-] copy beside its
   identical unprefixed twin is one no target needs, so the [-moz-] longhand
   that touches everything is the second body's. *)
let identical_bodies_never_conflict () =
  let css parts = String.concat "" parts in
  let check body body' =
    let want = css [ ".d"; body'; ".u"; body'; "html .u{animation-name:a}" ] in
    Alcotest.(check string)
      "written apart" want
      (canonical (css [ ".u"; body; "html .u{animation-name:a}.d"; body ]));
    Alcotest.(check string)
      "written as one list" want
      (canonical (css [ ".d,.u"; body; "html .u{animation-name:a}" ]))
  in
  check
    "{-moz-animation-duration:.1s;-webkit-animation-duration:.1s;animation-fill-mode:both}"
    "{animation-duration:.1s;animation-fill-mode:both}";
  let untwinned = "{-moz-animation-duration:.2s;animation-fill-mode:both}" in
  check untwinned untwinned

(* CSS Cascade 5 sec. 6.1 sorts by importance before order of appearance, so in
   one rule a normal and an important declaration never decide each other,
   whichever is written first. [margin] still writes three sides the important
   [margin-top] leaves it, so neither is dead. *)
let importance_commutes_within_a_rule () =
  let want = ".a{margin-top:1px!important;margin:0}" in
  Alcotest.(check string)
    "important written last" want
    (canonical ".a{margin:0}.a{margin-top:1px!important}");
  Alcotest.(check string)
    "important written first" want
    (canonical ".a{margin-top:1px!important;margin:0}")

(* A later rule with the identical selector shadows an earlier declaration of a
   property it writes at the same importance or above: the same element set at
   the same specificity, and CSS Cascade 5 sec. 6.1 puts importance first, then
   order of appearance. A later normal one leaves an earlier important one
   standing. *)
let important_shadows_an_earlier_write () =
  Alcotest.(check string)
    "important over important, a rule apart"
    ".b{margin:0!important}.a{float:left;margin:0 0 0 8px!important}"
    (canonical
       ".a{margin:0 8px!important;float:left}.b{margin:0!important}.a{margin:0 \
        0 0 8px!important}");
  Alcotest.(check string)
    "important over normal, a rule apart"
    ".a{color:green!important;top:0}.b{top:1px}"
    (canonical ".a{color:red;top:0}.b{top:1px}.a{color:green!important}");
  (* and the other way round, the later normal one is the dead write *)
  Alcotest.(check string)
    "normal under important" ".a{color:green!important}.b{top:0}"
    (canonical ".a{color:green!important}.b{top:0}.a{color:red}")

(* A declaration may stand in either of two occurrences of its selector when no
   chain of conflicts orders it before the later one, even where a chain holds
   the two occurrences apart. Here [.l] holds the [.h] occurrences apart (width,
   then background-position), and [display] may stand in either, so the sheet
   that wrote it in the first projects as the one that wrote it in the
   second. *)
let declaration_sinks_to_the_later_occurrence () =
  let want =
    "#c{display:inline-block}.h{width:40px}.l{background-position:0 \
     0;width:132px}.h{background-position:-84px -98px;display:inline-block}"
  in
  Alcotest.(check string)
    "written in the later" want
    (canonical
       "#c{display:inline-block}.h{width:40px}.l{background-position:0 \
        0;width:132px}.h{background-position:-84px -98px;display:inline-block}");
  Alcotest.(check string)
    "written in the earlier" want
    (canonical
       ".h{width:40px}#c,.h{display:inline-block}.l{background-position:0 \
        0;width:132px}.h{background-position:-84px -98px}");
  (* [.l] writing [display] too orders the declaration before the later [.h],
     through [background-position], and it stays where it was written *)
  Alcotest.(check string)
    "a chain keeps it"
    ".h{display:inline-block;width:40px}.l{background-position:0 \
     0;display:block;width:132px}.h{background-position:-84px -98px}"
    (canonical
       ".h{width:40px;display:inline-block}.l{display:block;width:132px;background-position:0 \
        0}.h{background-position:-84px -98px}")

(* Two rules that write the same declarations wherever they touch never decide
   each other either: [outline] meets nothing in [.h], so on an element both
   match the [border] and [margin] they share are written twice, once in each
   order, and [outline] once. *)
let agreeing_writes_never_conflict () =
  let shared = "border:1px solid #ddd;border-bottom-color:#ccc;margin:0" in
  let css parts = String.concat "" parts in
  let want =
    ".f{border:1px \
     solid#ddd;border-bottom-color:#ccc;margin:0;outline:0}.h{border:1px \
     solid#ddd;border-bottom-color:#ccc;margin:0}"
  in
  Alcotest.(check string)
    "the wider rule written second" want
    (canonical (css [ ".h{"; shared; "}.f{"; shared; ";outline:0}" ]));
  Alcotest.(check string)
    "the wider rule written first" want
    (canonical (css [ ".f{"; shared; ";outline:0}.h{"; shared; "}" ]))

(* The declaration dedup keeps a [content] sequence written in one rule verbatim
   where the later value may be one some browser rejects, as a fallback pattern,
   so a later rule with the identical selector does not shadow such an earlier
   write either: the sheet that wrote the two in one rule then projects as the
   one that wrote them in two. A later plain string supersedes the earlier one
   in both (CSS Cascade 5 sec. 6.1). *)
let shadowing_keeps_what_dedup_keeps () =
  List.iter
    (fun (name, want, two, one) ->
      Alcotest.(check string) (name ^ ", two rules") want (canonical two);
      Alcotest.(check string) (name ^ ", one rule") want (canonical one))
    [
      ( "an attr() fallback",
        ".a{content:\"x\";content:attr(data-x)}.b{top:0}",
        ".a{content:'x'}.b{top:0}.a{content:attr(data-x)}",
        ".a{content:'x';content:attr(data-x)}.b{top:0}" );
    ]

(* A rule between two occurrences that meets a declaration only on slots the
   later occurrence writes again does not hold the declaration back: whichever
   side of it the declaration stands, the later occurrence decides those slots.
   [border-bottom] meets [:hover]'s [border-color] only on the bottom colour,
   which the second [a.n.p] writes, at the same specificity, so the two sheets
   render alike, as the browser confirms in both states. *)
let a_rewritten_slot_does_not_hold () =
  let want =
    "a.n:hover{border-bottom:3px \
     solid;border-color:#cc181e;height:29px}a.n.p{border-bottom:3px \
     solid;border-color:#767676;height:29px}"
  in
  Alcotest.(check string)
    "written in the later" want
    (canonical
       "a.n:hover{border-bottom:3px \
        solid;border-color:#cc181e;height:29px}a.n.p{border-bottom:3px \
        solid;height:29px}a.n.p{border-color:#767676}");
  Alcotest.(check string)
    "written in the earlier" want
    (canonical
       "a.n.p{border-bottom:3px solid}a.n:hover{border-bottom:3px \
        solid;border-color:#cc181e;height:29px}a.n.p{border-color:#767676;height:29px}")

(* A declaration that sinks still precedes every rule after the later occurrence
   it conflicts with: [button.r .i] follows the first [a.n.p], which wrote
   [height], so it follows the occurrence [height] sinks into, though that one
   alone writes nothing it meets. *)
let a_sunk_declaration_keeps_its_followers () =
  let hover =
    "a.n:hover{border-bottom:3px solid;border-color:#cc181e;height:29px}"
  in
  Alcotest.(check string)
    "the later rule stays after"
    (String.concat ""
       [
         hover;
         "a.n.p{border-bottom:3px \
          solid;border-color:#767676;height:29px}button.r .i{height:5px}";
       ])
    (canonical
       (String.concat ""
          [
            "a.n.p,";
            hover;
            "a.n.p{border-color:#767676}button.r .i{height:5px}";
          ]))

let coalesce_blocked_by_intervening_conflict () =
  (* [.b] can match the same element as [.a] at equal specificity and writes the
     same property, so folding the two [.a] occurrences together past it is not
     observable-free: they stay split, in source order. The first [color] is
     dropped all the same - a later rule with the *identical* selector writes
     it, so it never wins for any element. *)
  Alcotest.(check string)
    "conflicting write between occurrences keeps them split"
    ".b{color:blue}.a{color:green}"
    (canonical ".a{color:red}.b{color:blue}.a{color:green}");
  (* what the rule leaves behind still coalesces *)
  Alcotest.(check string)
    "a declaration the later occurrence does not write survives"
    ".b{color:blue}.a{color:green;margin:0}"
    (canonical ".a{color:red;margin:0}.b{color:blue}.a{color:green}");
  (* [!important] changes the winner, so nothing is dead *)
  Alcotest.(check string)
    "an important declaration is never shadowed" ".a{color:red!important}"
    (canonical ".a{color:red!important}.a{color:green}")

let coalesce_drops_exact_duplicates () =
  Alcotest.(check string)
    "duplicate declaration collapses to the later occurrence"
    ".a{color:red;margin:0}"
    (canonical ".a{color:red}.a{color:red;margin:0}")

let coalesce_drops_dead_override () =
  (* When optimize factors a shared default into a group and specialises one
     branch ([.a,.b{color:red}] then [.b{color:blue}]), expanding and coalescing
     [.b] must drop the dead default so the coalesced rule holds the same
     declarations a sheet that folded [.b] directly would - otherwise the two
     project to different declaration sets and the structural comparison
     desynchronises (the tw-parity shadow/scrollbar regression). *)
  Alcotest.(check string)
    "coalesced branch drops the overridden default"
    ".a{color:red}.b{color:blue}"
    (canonical ".a,.b{color:red}.b{color:blue}")

let commuting_declarations_converge () =
  (* Declarations that write disjoint cascade slots sort into one canonical
     order, so two rules holding the same set in a different commuting order
     converge (the .sr-only phantom: every declaration writes a distinct
     property). *)
  let a =
    ".x{position:absolute;clip-path:inset(50%);width:1px;overflow:hidden}"
  in
  let b =
    ".x{width:1px;overflow:hidden;position:absolute;clip-path:inset(50%)}"
  in
  Alcotest.(check string)
    "both commuting orders converge" (canonical a) (canonical b)

let overlapping_declarations_keep_order () =
  (* A shorthand and its longhand write a common slot, so their relative order
     is cascade-significant and the two source orders must not converge. *)
  Alcotest.(check bool)
    "shorthand/longhand order stays distinct" true
    (canonical ".x{margin:0;margin-top:5px}"
    <> canonical ".x{margin-top:5px;margin:0}")

let logical_border_style_keeps_its_place () =
  (* CSS Logical 1 sec. 4: [border-block-end-style] resolves to whichever
     physical style side the writing mode picks, so the [border] written after
     it resets it. Sorting the pair by content puts the longhand first, and
     Chrome 146 then computes [border-bottom-style: solid] for the rule where
     the source computes [dashed]. *)
  Alcotest.(check string)
    "a logical border style side stays after the shorthand that resets it"
    ".a{border:1px solid red;border-block-end-style:dashed}"
    (canonical ".a{border:1px solid red;border-block-end-style:dashed}")

let nested_conditionals_participate () =
  let css =
    "@media print{@supports (display:flex){.z{color:red}}}.b{margin:0}"
  in
  Alcotest.(check bool)
    "independent rule sorts before the nested block" true
    (String.length (canonical css) > 2 && String.sub (canonical css) 0 2 = ".b")

(* A guarded rule repeating an unguarded write is dead only while nothing writes
   one of its cascade slots in between. A shorthand writes its longhands' slots
   and [all] writes every slot, so each of these sheets gives [.a] on screen a
   value the sheet without its last block does not (CSS Cascade 5 sec. 6.1,
   order of appearance). *)
let guarded_repeat_after_a_slot_write_stays () =
  let differ name with_repeat without =
    Alcotest.(check bool)
      name true
      (not (String.equal (canonical with_repeat) (canonical without)))
  in
  differ "a shorthand between keeps a longhand repeat"
    ".a{margin-top:1px}@media screen{.a{margin:0}}@media \
     screen{.a{margin-top:1px}}"
    ".a{margin-top:1px}@media screen{.a{margin:0}}";
  differ "a longhand between keeps a shorthand repeat"
    ".a{margin:0}@media screen{.a{margin-top:1px}}@media screen{.a{margin:0}}"
    ".a{margin:0}@media screen{.a{margin-top:1px}}";
  differ "all between keeps a repeat"
    ".a{color:red}@media screen{.a{all:unset}}@media screen{.a{color:red}}"
    ".a{color:red}@media screen{.a{all:unset}}";
  differ "an earlier write in the rule keeps a repeat"
    ".a{color:red}@media screen{.a{color:blue;color:red}}"
    ".a{color:red}@media screen{.a{color:blue}}";
  differ "an earlier write in the block keeps a repeat"
    ".a{color:red}@media screen{.b{color:blue}.a{color:red}}"
    ".a{color:red}@media screen{.b{color:blue}}";
  Alcotest.(check string)
    "a repeat with no slot written in between is still dropped"
    (canonical ".a{margin-top:1px}@media screen{.a{color:red}}")
    (canonical
       ".a{margin-top:1px}@media screen{.a{color:red}}@media \
        screen{.a{margin-top:1px}}")

let same_condition_blocks_fold_together () =
  (* Two blocks under one condition write one cascade slot, so the projection
     folds them the way it folds two occurrences of a selector. A sheet whose
     source kept them apart converges with one that wrote them together. *)
  let split =
    "@media print{.a{color:red}}.zzz{order:1}@media print{.b{color:blue}}"
  in
  let together = "@media print{.a{color:red}.b{color:blue}}.zzz{order:1}" in
  Alcotest.(check string)
    "separated blocks fold together" (canonical together) (canonical split);
  Alcotest.(check bool)
    "one block survives" true
    (String.length (canonical split)
     - String.length
         (String.concat "" (String.split_on_char '@' (canonical split)))
    = 1)

(* Conditional Rules 5 sec. 5.4: an unknown container feature never matches.
   [(inline-size\>\=10px)] is one escaped feature name, not the real size range
   [(inline-size >= 10px)], even though their compact serialisations are
   identical. The canonical projection must therefore keep their blocks in
   separate cascade slots. *)
let container_key_keeps_structurally_distinct_queries_apart () =
  let css =
    {|@container (inline-size\>\=10px){.x{color:red}}@container (inline-size>=10px){.y{color:blue}}|}
  in
  let projected =
    Css.v (statements css) |> Css.canonicalize_rule_order |> Css.statements
  in
  Alcotest.(check int)
    "an unknown feature is not projected into the size range" 2
    (List.length projected)

let conflicting_block_fold_is_refused () =
  (* Folding the second block up moves a conditional write of [color] on [.a]
     before an unconditional write of [color] on [.a], which changes the winner
     when the query matches, so the guarded [blue] stays after [green]. The
     first block's [red] never wins: whenever it applies, [blue] applies too,
     under the same selector, and comes later, and CSS Cascade 5 sec. 6.1
     (Cascade Sorting Order) has the last declaration in order of appearance
     win. The sheet is the one without it. *)
  let css =
    "@media print{.a{color:red}}.a{color:green}@media print{.a{color:blue}}"
  in
  Alcotest.(check string)
    "the overridden guarded write is dead"
    (canonical ".a{color:green}@media print{.a{color:blue}}")
    (canonical css);
  Alcotest.(check bool)
    "the guarded write stays after the unconditional one" true
    (canonical css
    <> canonical "@media print{.a{color:red;color:blue}}.a{color:green}")

(* One run of [n] rules on distinct classes that all write [color], so every
   pair meets on a declaration and none on a selector, and no two fold. *)
let independent_run n =
  let buffer = Buffer.create (n * 24) in
  let formatter = Fmt.with_buffer buffer in
  for i = n - 1 downto 0 do
    Fmt.pf formatter ".c%d{color:red;top:%dpx}" i i
  done;
  Fmt.flush formatter ();
  statements (Buffer.contents buffer)

(* Settling a run orders it once, and that order is the only thing a graph of
   the run is needed for: the folds before and after it ask whether two rules
   conflict, which is read off the two rules. So the whole projection has to
   cost less than two graphs of its run. Allocated words rather than time, so
   the bound holds on a loaded machine. *)
let settling_a_run_builds_one_graph () =
  let stmts = independent_run 1_000 in
  let rules =
    List.filter_map (function Stylesheet.Rule r -> Some r | _ -> None) stmts
  in
  let graph =
    Css_test_helpers.measure (fun () ->
        Rule_graph.of_rules ~pin_shared_branches:false rules)
  in
  let projection =
    Css_test_helpers.measure (fun () -> Rule_order.canonicalize stmts)
  in
  Alcotest.(check bool)
    (Fmt.str "projection %.0f words, one graph %.0f (%.2fx)" projection graph
       (projection /. graph))
    true
    (projection < 2. *. graph)

(* A round after the first finds the run as the round before left it, except for
   the elements that folded, so it rereads those and keeps the graph of the
   rest. The two blocks below fold once the first round has sorted them next to
   each other, which takes a second round, and the projection still costs less
   than one and a half graphs of its run. *)
let a_round_keeps_the_graph () =
  let stmts =
    independent_run 1_000
    @ statements "@media(width>=1px){.m{left:0}}@media(width>=1px){.n{right:0}}"
  in
  let rules =
    List.filter_map (function Stylesheet.Rule r -> Some r | _ -> None) stmts
  in
  let graph =
    Css_test_helpers.measure (fun () ->
        Rule_graph.of_rules ~pin_shared_branches:false rules)
  in
  let projection =
    Css_test_helpers.measure (fun () -> Rule_order.canonicalize stmts)
  in
  Alcotest.(check bool)
    (Fmt.str "projection %.0f words, one graph %.0f (%.2fx)" projection graph
       (projection /. graph))
    true
    (projection < 1.5 *. graph)

let suite =
  ( "rule_order",
    [
      Alcotest.test_case "sorts independent rules" `Quick
        sorts_independent_rules_into_content_order;
      Alcotest.test_case "keeps conflicting order" `Quick
        keeps_conflicting_rules_in_source_order;
      Alcotest.test_case "tied writers follow an earlier conflict" `Quick
        tied_writers_follow_an_earlier_conflict;
      Alcotest.test_case "custom-property position converges" `Quick
        custom_property_position_converges;
      Alcotest.test_case "custom-property importance survives" `Quick
        custom_property_importance_survives;
      Alcotest.test_case "custom-property layer and meta survive" `Quick
        custom_property_layer_and_meta_survive;
      Alcotest.test_case "custom-property font-name quoting converges" `Quick
        custom_property_font_name_quoting_converges;
      Alcotest.test_case "custom-property comma spacing normalizes" `Quick
        custom_property_comma_spacing_normalizes;
      Alcotest.test_case
        "custom-property font-name quoting converges under every wrapper" `Quick
        custom_font_quoting_converges_under_every_wrapper;
      Alcotest.test_case "color spelling converges under every wrapper" `Quick
        color_spelling_converges_under_every_wrapper;
      Alcotest.test_case "media level spelling converges under every wrapper"
        `Quick media_level_spelling_converges_under_every_wrapper;
      Alcotest.test_case "at-rule block takes the projection folds" `Quick
        at_rule_block_takes_the_projection_folds;
      Alcotest.test_case "media and independent rule converge" `Quick
        media_and_independent_rule_converge;
      Alcotest.test_case "media conflict keeps source order" `Quick
        media_conflict_keeps_source_order;
      Alcotest.test_case "layer blocks stay put" `Quick layer_blocks_stay_put;
      Alcotest.test_case "hoisted group converges with inline" `Quick
        hoisted_group_converges_with_inline;
      Alcotest.test_case "selector list expands without coalesce" `Quick
        selector_list_expands_without_coalesce;
      Alcotest.test_case "selector list expands when a branch is shared" `Quick
        selector_list_expands_when_a_branch_is_shared;
      Alcotest.test_case "coalesce through two held occurrences" `Quick
        coalesce_through_two_held_occurrences;
      Alcotest.test_case "coalesce reads writes a fold moved" `Quick
        coalesce_reads_writes_a_fold_moved;
      Alcotest.test_case "fold order does not decide the form" `Quick
        fold_order_does_not_decide_the_form;
      Alcotest.test_case "a repeat keeps what it overrides" `Quick
        a_repeat_keeps_what_it_overrides;
      Alcotest.test_case "adjacent lists join before expanding" `Quick
        adjacent_lists_join_before_expanding;
      Alcotest.test_case "identical bodies never conflict" `Quick
        identical_bodies_never_conflict;
      Alcotest.test_case "importance commutes within a rule" `Quick
        importance_commutes_within_a_rule;
      Alcotest.test_case "important shadows an earlier write" `Quick
        important_shadows_an_earlier_write;
      Alcotest.test_case "declaration sinks to the later occurrence" `Quick
        declaration_sinks_to_the_later_occurrence;
      Alcotest.test_case "agreeing writes never conflict" `Quick
        agreeing_writes_never_conflict;
      Alcotest.test_case "shadowing keeps what dedup keeps" `Quick
        shadowing_keeps_what_dedup_keeps;
      Alcotest.test_case "a rewritten slot does not hold" `Quick
        a_rewritten_slot_does_not_hold;
      Alcotest.test_case "a sunk declaration keeps its followers" `Quick
        a_sunk_declaration_keeps_its_followers;
      Alcotest.test_case "coalesce blocked by intervening conflict" `Quick
        coalesce_blocked_by_intervening_conflict;
      Alcotest.test_case "coalesce drops exact duplicates" `Quick
        coalesce_drops_exact_duplicates;
      Alcotest.test_case "coalesce drops dead override" `Quick
        coalesce_drops_dead_override;
      Alcotest.test_case "commuting declarations converge" `Quick
        commuting_declarations_converge;
      Alcotest.test_case "overlapping declarations keep order" `Quick
        overlapping_declarations_keep_order;
      Alcotest.test_case "logical border style keeps its place" `Quick
        logical_border_style_keeps_its_place;
      Alcotest.test_case "redundant layer pin folds" `Quick
        redundant_layer_pin_folds;
      Alcotest.test_case "pinned layer blocks take the layer order" `Quick
        pinned_layer_blocks_take_the_layer_order;
      Alcotest.test_case "order-changing layer pin is kept" `Quick
        order_changing_layer_pin_is_kept;
      Alcotest.test_case "leading layer pins matching the blocks fold" `Quick
        leading_layer_pins_matching_the_blocks_fold;
      Alcotest.test_case "layer pin folds one name not the statement" `Quick
        layer_pin_folds_one_name_not_the_statement;
      Alcotest.test_case "layer pin names are ident lists" `Quick
        layer_pin_names_are_ident_lists;
      Alcotest.test_case "unblocked layer pin is kept" `Quick
        unblocked_layer_pin_is_kept;
      Alcotest.test_case "layer pin over conditional declaration is kept" `Quick
        layer_pin_over_conditional_declaration_is_kept;
      Alcotest.test_case "layer pin over conditional layer is the only position"
        `Quick layer_pin_over_conditional_layer_is_the_only_position;
      Alcotest.test_case "layer pin over import is kept" `Quick
        layer_pin_over_import_is_kept;
      Alcotest.test_case "layer pin fold is idempotent" `Quick
        layer_pin_fold_is_idempotent;
      Alcotest.test_case "block with layer content is barrier" `Quick
        block_with_layer_content_is_barrier;
      Alcotest.test_case "nested conditionals participate" `Quick
        nested_conditionals_participate;
      Alcotest.test_case "guarded repeat after a slot write stays" `Quick
        guarded_repeat_after_a_slot_write_stays;
      Alcotest.test_case "same-condition blocks fold together" `Quick
        same_condition_blocks_fold_together;
      Alcotest.test_case "container key keeps distinct queries apart" `Quick
        container_key_keeps_structurally_distinct_queries_apart;
      Alcotest.test_case "conflicting block fold is refused" `Quick
        conflicting_block_fold_is_refused;
      Alcotest.test_case "settling a run builds one graph" `Quick
        settling_a_run_builds_one_graph;
      Alcotest.test_case "a round keeps the graph" `Quick
        a_round_keeps_the_graph;
    ] )
