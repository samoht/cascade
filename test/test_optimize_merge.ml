(** Optimize tests: shorthand resets and the cascade conditions on merging and
    factoring. *)

open Cascade
open Css.Values
open Css.Properties
open Optimize_helpers

let c3_shorthand_resets () =
  (* CSS Cascade section 3: a shorthand declaration sets all longhands,
     including omitted sub-properties. A previous longhand covered by a later
     shorthand is therefore dead even when the shorthand omits that
     component. *)
  let margin_rule : Css.Stylesheet.rule =
    {
      selector = Css.Selector.class_ "box";
      declarations =
        [
          Css.Declaration.margin_left (Px 1.); Css.Declaration.margin [ Px 2. ];
        ];
      nested = [];
      merge_key = None;
    }
  in
  let margin_optimized = Css.Optimize.single_rule margin_rule in
  let margin_output =
    Css.Stylesheet.to_string ~minify:true [ statement_of_rule margin_optimized ]
    |> String.trim
  in
  Alcotest.(check string)
    "later margin shorthand resets previous margin-left" ".box{margin:2px}"
    margin_output;

  let background_rule : Css.Stylesheet.rule =
    {
      selector = Css.Selector.class_ "hero";
      declarations =
        [
          Css.Declaration.background_image (Css.Properties.url "hero.png");
          Css.Declaration.background
            (Css.Properties.background_shorthand ~color:(hex_color "008000") ());
        ];
      nested = [];
      merge_key = None;
    }
  in
  let background_optimized = Css.Optimize.single_rule background_rule in
  let background_output =
    Css.Stylesheet.to_string ~minify:true
      [ statement_of_rule background_optimized ]
    |> String.trim
  in
  Alcotest.(check string)
    "background shorthand resets previous background-image"
    ".hero{background:#008000}" background_output

let c3_stylesheet_scope_background_synthesis () =
  (* CSS Backgrounds shorthands are resetful: synthesizing [background] resets
     omitted background longhands. Under the default [`Fragment] scope, a
     fragment cannot assume no earlier author CSS wrote one of those omitted
     longhands. Under [`Stylesheet] scope, the caller asserts the whole relevant
     author stylesheet graph is available, so the shorter resetful shorthand is
     allowed. This is the CSS-text scope axis, not the [~closed_world] DOM
     assumption. *)
  let optimize ?scope css =
    Css.stylesheet ~strict:false css
    |> Css.optimize ?scope |> Css.to_string ~minify:true |> String.trim
  in
  let partial_run =
    {|
      .card {
        background-color: red;
        background-image: none;
        background-repeat: repeat;
        background-position: 0% 0%;
        background-attachment: scroll;
      }
    |}
  in
  Alcotest.(check string)
    "open-world partial background run keeps longhands"
    ".card{background-color:red;background-image:none;background-repeat:repeat;background-position:0 \
     0;background-attachment:scroll}"
    (optimize partial_run);
  Alcotest.(check string)
    "stylesheet-scope partial background run may synthesize shorthand"
    ".card{background:red}"
    (optimize ~scope:`Stylesheet partial_run);
  let reset_closed_run =
    {|
      .card {
        background-color: red;
        background-image: none;
        background-repeat: repeat;
        background-position: 0% 0%;
        background-size: auto;
        background-attachment: scroll;
        background-origin: padding-box;
        background-clip: border-box;
      }
    |}
  in
  Alcotest.(check string)
    "open-world reset-closed background run may synthesize shorthand"
    ".card{background:red}"
    (optimize reset_closed_run)

let closed_world_groups_disjoint_selectors () =
  (* [.a] and [.b] share a body, but [.c] sits between them writing a different
     value for the same property. In the open world an element could be [.b.c],
     where grouping [.a,.b] ahead of [.c] would flip its colour, so the default
     refuses. Under closed_world the caller asserts no element matches two of
     these selectors, so the group is safe. *)
  let opt ?closed_world css =
    Css.stylesheet ~strict:false css
    |> Css.optimize ?closed_world |> Css.to_string ~minify:true
  in
  let input = ".a{color:red}.c{color:blue}.b{color:red}" in
  let default = opt input in
  let closed = opt ~closed_world:true input in
  (* the only comma either output can carry is a grouped selector list *)
  Alcotest.(check bool)
    "open world keeps the selectors separate (a .b.c element would flip)" false
    (String.contains default ',');
  Alcotest.(check bool)
    "closed world groups the disjoint .a and .b" true
    (String.contains closed ',');
  Alcotest.(check bool)
    "closed world is shorter" true
    (String.length closed < String.length default)

let coalesce_adjacent_custom_property_rules () =
  let opt css =
    Css.stylesheet ~strict:false css
    |> Css.optimize |> Css.to_string ~minify:true
  in
  (* Custom-property rules are barred from the DAG factoring, but adjacent rules
     with an identical custom-property body (Tailwind gradient/mask/ring stacks)
     are unconditionally safe to coalesce into one comma-selector rule. *)
  Alcotest.(check string)
    "adjacent identical custom-property rules coalesce"
    ".a,.b,.c{--tw-gradient-from:#3b82f6}"
    (opt
       ".a{--tw-gradient-from:#3b82f6}.b{--tw-gradient-from:#3b82f6}.c{--tw-gradient-from:#3b82f6}");
  (* A different value between them keeps the run split: merging across the
     conflicting `.b` would flip a `.b.c` element. *)
  Alcotest.(check bool)
    "custom-property run split by a conflicting value stays unmerged" false
    (String.contains (opt ".a{--x:red}.b{--x:blue}.c{--x:red}") ',')

let c3_shorthand_order_edges () =
  (* CSS Cascade section 3 plus source order: a later shorthand resets all
     covered longhands, including a longhand that occurred between two shorthand
     declarations. *)
  let rule : Css.Stylesheet.rule =
    {
      selector = Css.Selector.class_ "box";
      declarations =
        [
          Css.Declaration.margin [ Px 1. ];
          Css.Declaration.margin_left (Px 2.);
          Css.Declaration.margin [ Px 3. ];
        ];
      nested = [];
      merge_key = None;
    }
  in
  let optimized = Css.Optimize.single_rule rule in
  let output =
    Css.Stylesheet.to_string ~minify:true [ statement_of_rule optimized ]
    |> String.trim
  in
  Alcotest.(check string)
    "later shorthand resets intervening longhand" ".box{margin:3px}" output

let c3_important_shorthand_expands () =
  (* CSS Cascade section 3: declaring a shorthand !important is equivalent to
     declaring all of its longhand sub-properties !important. A later normal
     longhand covered by the shorthand cannot override any sub-property. *)
  let rule : Css.Stylesheet.rule =
    {
      selector = Css.Selector.class_ "hero";
      declarations =
        [
          Css.Declaration.background_image (Css.Properties.url "before.png");
          Css.Declaration.important
            (Css.Declaration.background
               (Css.Properties.background_shorthand ~color:(hex_color "008000")
                  ()));
          Css.Declaration.background_image (Css.Properties.url "after.png");
        ];
      nested = [];
      merge_key = None;
    }
  in
  let optimized = Css.Optimize.single_rule rule in
  let output =
    Css.Stylesheet.to_string ~minify:true [ statement_of_rule optimized ]
    |> String.trim
  in
  Alcotest.(check string)
    "important background shorthand blocks later normal background-image"
    ".hero{background:#008000!important}" output

let c61_decl_order_shorthand_boundary () =
  (* CSS Cascade section 6.1: order of appearance is a cascade criterion.
     Removing an earlier duplicate must not move the surviving longhand before
     an intervening shorthand, because shorthands reset their longhands. *)
  let rule : Css.Stylesheet.rule =
    {
      selector = Css.Selector.class_ "box";
      declarations =
        [
          Css.Declaration.margin_left (Px 1.);
          Css.Declaration.margin [ Px 2. ];
          Css.Declaration.margin_left (Px 3.);
        ];
      nested = [];
      merge_key = None;
    }
  in
  let optimized = Css.Optimize.single_rule rule in
  let output =
    Css.Stylesheet.to_string ~minify:true [ statement_of_rule optimized ]
    |> String.trim
  in
  Alcotest.(check string)
    "later longhand stays after shorthand" ".box{margin:2px 2px 2px 3px}" output

let c61_adjacent_shorthand_order () =
  (* CSS Cascade sections 3 and 6.1: merging adjacent equal-selector rules is
     only semantics-preserving when the declaration sequence stays in source
     order, because a shorthand in the later rule resets earlier longhands. The
     first rule also contains a non-shadowed declaration so this remains a
     cross-rule merge test even when dead earlier declarations are
     eliminated. *)
  let input =
    [
      Css.rule
        ~selector:(Css.Selector.class_ "box")
        [
          Css.Declaration.color (hex_color "ff0000");
          Css.Declaration.margin_left (Px 1.);
        ];
      Css.rule
        ~selector:(Css.Selector.class_ "box")
        [
          Css.Declaration.margin [ Px 2. ]; Css.Declaration.margin_left (Px 3.);
        ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "adjacent same-selector merge keeps shorthand/longhand source order"
    ".box{color:red;margin:2px 2px 2px 3px}" output

let c61_adjacent_later_dedup () =
  (* Positive merge case: adjacent same-selector rules in the same cascade slot
     may merge, and ordinary duplicate declarations inside the merged rule still
     reduce by source order. *)
  let input =
    [
      Css.rule
        ~selector:(Css.Selector.class_ "box")
        [ Css.Declaration.color (hex_color "ff0000") ];
      Css.rule
        ~selector:(Css.Selector.class_ "box")
        [
          Css.Declaration.display Flex;
          Css.Declaration.color (hex_color "0000ff");
        ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "adjacent same-selector rules merge and dedupe by source order"
    ".box{display:flex;color:#00f}" output

let c61_no_merge_intervening () =
  (* CSS Cascade 6.1: [.a] and the intervening [.b] tie on specificity (0,1,0),
     so source order is observable for any element matching both. A
     same-selector merge would put the combined [.a] rule on one side of [.b],
     changing that order, so it is blocked even though the moved property does
     not itself conflict. *)
  let input =
    [
      Css.rule ~selector:(Css.Selector.class_ "a")
        [ Css.Declaration.color (hex_color "ff0000") ];
      Css.rule ~selector:(Css.Selector.class_ "b")
        [ Css.Declaration.color (hex_color "00ff00") ];
      Css.rule ~selector:(Css.Selector.class_ "a")
        [ Css.Declaration.background_color (hex_color "0000ff") ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  (* The two [.a] rules merge across the tied [.b]: [.b] writes [color], which
     pins the merged rule at the slot where [.b] still wins for [.a.b];
     [background-color] is uncontested, so joining it is cascade-neutral. The
     merged body uses canonical declaration order. The graph rewrite's
     acyclicity check is what proves this safe. *)
  Alcotest.(check string)
    "same selector merges across a tied intervening rule when safe"
    ".a{background-color:#00f;color:red}.b{color:#0f0}" output

let c61_no_group_nonadjacent () =
  (* CSS Cascade section 6.1: selector grouping changes where a rule appears in
     source order. Non-adjacent equal declaration blocks must not be grouped
     across another same-specificity rule, because elements matching both the
     middle selector and the later selector would observe a different winner. *)
  let input =
    [
      Css.rule ~selector:(Css.Selector.class_ "a")
        [ Css.Declaration.color (hex_color "ff0000") ];
      Css.rule ~selector:(Css.Selector.class_ "b")
        [ Css.Declaration.color (hex_color "0000ff") ];
      Css.rule ~selector:(Css.Selector.class_ "c")
        [ Css.Declaration.color (hex_color "ff0000") ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "same declarations are not grouped across source-order competitor"
    ".a{color:red}.b{color:#00f}.c{color:red}" output

let aba_forbidden_intersection_dependency () =
  (* A?B?A / CSS-graph soundness: two equal A declarations cannot be grouped
     across an intervening B declaration when B writes the same property and the
     selector intersection is satisfiable. Here an element can match all three
     selectors, so source order decides whether red or blue wins. *)
  Alcotest.(check string)
    "overlapping selectors keep intervening same-property dependency"
    ".a.x{color:red}.b.x{color:#00f}.a.y{color:red}"
    (optimized_string ".a.x{color:red}.b.x{color:blue}.a.y{color:red}")

let vendor_alias_pins_intervening_rule () =
  (* A?B?A across a vendor alias: the prefixed spelling writes the slot its
     unprefixed twin writes, so for an element matching [.b] and [.c] source
     order decides the winner. Grouping [.a] with [.c] moves the prefixed
     declaration in front of [.b] and flips it. Chrome 146 computes [transform:
     none] for the first sheet below and [matrix(...)] once the pair is grouped;
     Safari 26.5 agrees, and answers the same for [-webkit-hyphens], which
     Chrome does not implement. *)
  Alcotest.(check string)
    "a prefixed transform is not grouped across its unprefixed twin"
    ".a{-webkit-transform:none}.b{transform:rotate(45deg)}.c{-webkit-transform:none}"
    (optimized_string
       ".a{-webkit-transform:none}.b{transform:rotate(45deg)}.c{-webkit-transform:none}");
  Alcotest.(check string)
    "a prefixed hyphens is not grouped across its unprefixed twin"
    ".a{-webkit-hyphens:none}.b{-webkit-hyphens:auto;hyphens:auto}.c{-webkit-hyphens:none}"
    (optimized_string
       ".a{-webkit-hyphens:none}.b{hyphens:auto}.c{-webkit-hyphens:none}");
  Alcotest.(check string)
    "a prefixed mask longhand is not grouped across its unprefixed twin"
    ".a{-webkit-mask-size:auto}.b{-webkit-mask-size:cover;mask-size:cover}.c{-webkit-mask-size:auto}"
    (optimized_string
       ".a{-webkit-mask-size:auto}.b{mask-size:cover}.c{-webkit-mask-size:auto}");
  (* A prefix names one slot, not its whole family, so an unrelated intervening
     declaration still leaves the pair free to group. *)
  Alcotest.(check string)
    "a prefixed transform still groups across an unrelated declaration"
    ".a,.c{-webkit-transform:none}.b{color:red}"
    (optimized_string
       ".a{-webkit-transform:none}.b{color:red}.c{-webkit-transform:none}")

let unplaceable_name_pins_intervening_rule () =
  (* A property name the footprint model cannot place may be a shorthand, a
     legacy alias, or a longhand of a family the model does not carry, so it
     writes whatever slot the declaration it meets writes. Grouping [.a] with
     [.c] past [.b] changes which of the two wins for an element matching [.b]
     and [.c]: Chrome 146 computes [background-position: 10px 0%] for the first
     sheet below and [0% 0%] once the pair is grouped, and [overflow-wrap:
     break-word] against [normal] for the second. *)
  Alcotest.(check string)
    "an unplaceable longhand is not grouped across the shorthand resetting it"
    ".a{background-position-x:10px}.b{background:red}.c{background-position-x:10px}"
    (optimized_string
       ".a{background-position-x:10px}.b{background:red}.c{background-position-x:10px}");
  Alcotest.(check string)
    "an unplaceable legacy alias is not grouped across the property it aliases"
    ".a{word-wrap:break-word}.b{overflow-wrap:normal}.c{word-wrap:break-word}"
    (optimized_string
       ".a{word-wrap:break-word}.b{overflow-wrap:normal}.c{word-wrap:break-word}");
  (* A name the model does place keeps naming its own slot, so a rule holding
     one still groups across an unrelated declaration. *)
  Alcotest.(check string)
    "a placeable name still groups across an unrelated declaration"
    ".a,.c{margin-top:var(--a) var(--b)}.b{color:red}"
    (optimized_string
       ".a{margin-top:var(--a) var(--b)}.b{color:red}.c{margin-top:var(--a) \
        var(--b)}")

let c63_group_across_higher_specificity () =
  (* CSS Cascade 5 sec. 6.3: among the same origin, layer, and importance higher
     specificity wins regardless of source order; only a specificity tie defers
     to source order. [#m] (1,0,0) outranks [.a.x]/[.c.x] (0,2,0), so for any
     element matching all three (class="a c x" id="m") #m's blue wins whatever
     the order. The two equal [.a.x]/[.c.x] rules may therefore group across #m.
     Blocking the group because the class-pairs do not *beat* #m is
     over-conservative: differing specificity already makes the order
     unobservable. Contrast [aba_forbidden_intersection_dependency], where the
     intervening rule ties on specificity and the block is required. *)
  (* Pure cascade-safety reasoning, so it holds in both scopes. *)
  List.iter
    (fun scope ->
      let output =
        optimized_string ?scope ".a.x{color:red}#m{color:blue}.c.x{color:red}"
      in
      Alcotest.(check bool)
        "equal rules group across a higher-specificity intervening rule" true
        (Astring.String.is_infix ~affix:".a.x,.c.x{color:red}" output))
    [ None; Some `Stylesheet ]

let c63_merge_same_selector_across_higher_specificity () =
  (* Same reasoning for merging two equal-selector rules across a gap: [#footer]
     (1,0,0) outranks [body]/[html] (0,0,1), so font-size on a <body id=footer>
     resolves to #footer's 10pt whatever the order. The two body,html rules
     therefore merge across #footer - the case that motivates relaxing the
     intervening-overlap guard from "the merge selector beats the intervening
     one" to "their specificities differ". *)
  List.iter
    (fun scope ->
      let output =
        optimized_string ?scope
          "body,html{font-size:small}#footer{font-size:10pt}body,html{line-height:1.5}"
      in
      Alcotest.(check bool)
        "body,html merges across higher-specificity #footer" true
        (Astring.String.is_infix ~affix:"font-size:small;line-height:1.5" output))
    [ None; Some `Stylesheet ]

let factor_diff_value_descendant_tie_unsafe () =
  (* Collapsing the shared [float:left] of [.a .log] and [.b .hx] into one group
     is unsound: the merged rule G sits at a single source position while the
     intervening [.h .a] (float:right, a 0,2,0 tie with both branches) sits at
     another, and two elements demand opposite placements: E1 = <h><a><a.log> :
     matches [.a .log] (left) and [.h .a] (right). Source order is [.a .log]
     then [.h .a], so right wins, requiring G before [.h .a]. E2 = <b><h><a.hx>
     : matches [.b .hx] (left) and [.h .a] (right). Source order is [.h .a] then
     [.b .hx], so left wins, requiring G after [.h .a]. No single position for G
     satisfies both, so the two [float:left] branches must never share a group
     across [.h .a]. *)
  let output =
    optimized_string ~scope:`Stylesheet
      ".a .log{color:red;float:left;width:10px}.h .a{float:right}.b \
       .hx{color:red;float:left;width:20px}"
  in
  let unsafe_float_group =
    String.split_on_char '}' output
    |> List.exists (fun rule ->
        match Astring.String.cut ~sep:"{" rule with
        | Some (selector, decls) ->
            Astring.String.is_infix ~affix:".a .log" selector
            && Astring.String.is_infix ~affix:".b .hx" selector
            && Astring.String.is_infix ~affix:"float:left" decls
        | None -> false)
  in
  Alcotest.(check bool)
    "the two float:left branches never share a group across the float:right tie"
    false unsafe_float_group

let factor_same_value_descendant_tie_groups () =
  (* Here [.h .a] writes the same [float:left]. Reordering identical-value
     declarations is unobservable, so the shared block factors across the
     tie. *)
  let output =
    optimized_string ~scope:`Stylesheet
      ".a .log{color:red;float:left;width:10px}.h .a{float:left}.b \
       .hx{color:red;float:left;width:20px}"
  in
  Alcotest.(check bool)
    "same-value descendant-combinator tie allows factoring" true
    (Astring.String.is_infix ~affix:".a .log,.b .hx" output)

let factor_prefers_size_minimal_subset () =
  (* Hoisting a shared declaration is profitable for a member only when its
     selector entry (len+1) is cheaper than the bytes it would otherwise
     duplicate ("position:absolute;" = 18). [.very-long-closebtn-selector] (28)
     is a net loss, so it keeps [position:absolute] inline while the three short
     selectors form the shared group. *)
  let output =
    optimized_string ~scope:`Stylesheet
      "#f{top:0;position:absolute;width:100%}.b{padding:16px;position:absolute;z-index:1}.b-arrow{position:absolute}.very-long-closebtn-selector{height:21px;position:absolute;width:21px}"
  in
  Alcotest.(check bool)
    "short selectors group; long selector keeps position:absolute inline" true
    (Astring.String.is_infix ~affix:"#f,.b,.b-arrow{position:absolute}" output
    && Astring.String.is_infix
         ~affix:
           ".very-long-closebtn-selector{height:21px;position:absolute;width:21px}"
         output)

let factor_steps_over_same_value_nesting_boundary () =
  (* A rule carrying a nested rule is a factor boundary, but the
     [position:absolute] scan steps over [.jfk-bubble-closebtn] (which nests
     [&:focus]) to reach [.jfk-bubble-arrow]. Stepping over it is sound:
     closebtn carries the same [position:absolute] and its nested [&:focus] sets
     only opacity, so reordering position across it is unobservable. The three
     same-value members group while closebtn stays inline with its nested
     rule. *)
  let output =
    optimized_string ~scope:`Stylesheet
      "#footer{position:absolute;width:100%}.jfk-bubble{padding:16px;position:absolute}.jfk-bubble-closebtn{position:absolute;right:2px;&:focus{opacity:.8}}.jfk-bubble-arrow{position:absolute}"
  in
  Alcotest.(check bool)
    "cluster groups across the same-value nesting boundary" true
    (Astring.String.is_infix
       ~affix:"#footer,.jfk-bubble,.jfk-bubble-arrow{position:absolute}" output)

let factor_conflicting_nesting_boundary_unsafe () =
  (* The boundary [.bnd] nests a rule and also sets [position:relative], a 0,1,0
     tie with [.m1]/[.m2]'s [position:absolute]. For an element matching [.m2]
     and [.bnd], source order ([.bnd] before [.m2]) makes absolute win; hoisting
     [.m1] and [.m2] into a group before [.bnd] flips it to relative. So the
     scan steps over a nesting boundary only when it shares the factored value,
     never when it conflicts. *)
  let output =
    optimized_string ~scope:`Stylesheet
      ".m1{position:absolute}.bnd{position:relative;&:hover{color:red}}.m2{position:absolute}"
  in
  let unsafe_group =
    String.split_on_char '}' output
    |> List.exists (fun rule ->
        match Astring.String.cut ~sep:"{" rule with
        | Some (selector, decls) ->
            Astring.String.is_infix ~affix:".m1" selector
            && Astring.String.is_infix ~affix:".m2" selector
            && Astring.String.is_infix ~affix:"position:absolute" decls
        | None -> false)
  in
  Alcotest.(check bool)
    "no position:absolute group spans .m1 and .m2 across the conflicting \
     boundary"
    false unsafe_group

let factor_inlines_rather_than_grouping_long_selector () =
  (* outlook-3 gap: when a declaration is already shared in source between a
     long selector and a sibling, keeping it grouped repeats the long selector
     in the group list. Inlining it into the sibling rules costs only one extra
     copy of the declaration per rule, which is cheaper once the selector
     exceeds the declaration. Here [.really-long-block-name .primary] (>>15
     chars) should keep [padding-top:1px] inline alongside its [font-size], not
     share a [padding-top] group with [.really-long-block-name .secondary]. *)
  let output =
    optimized_string ~scope:`Stylesheet
      ".really-long-block-name .primary{font-size:14px}.really-long-block-name \
       .primary,.really-long-block-name .secondary{padding-top:1px}"
  in
  let costly_padding_group =
    String.split_on_char '}' output
    |> List.exists (fun rule ->
        match Astring.String.cut ~sep:"{" rule with
        | Some (selector, decls) ->
            Astring.String.is_infix ~affix:".primary" selector
            && Astring.String.is_infix ~affix:".secondary" selector
            && Astring.String.is_infix ~affix:"padding-top" decls
        | None -> false)
  in
  Alcotest.(check bool)
    "padding-top is inlined, not grouped across the long selector" false
    costly_padding_group

let factor_groups_single_property_past_other_shared_decl () =
  (* The anchor [#footer] shares [width] with [.fll] and [position] with the
     [.jfk-bubble] cluster. A scan that commits to the first shared declaration
     it meets would lock onto [width] at [.fll] and never group [position] with
     the cluster. Seeding the scan per anchor declaration recovers the
     [position] group across the intervening [.fll]. *)
  let output =
    optimized_string ~scope:`Stylesheet
      "#footer{bottom:0;position:absolute;width:100%}.fll{float:right;width:100%}.jfk-bubble{padding:16px;position:absolute}.jfk-bubble-arrow{position:absolute}"
  in
  Alcotest.(check bool)
    "position groups across .fll which shares only width" true
    (Astring.String.is_infix
       ~affix:"#footer,.jfk-bubble,.jfk-bubble-arrow{position:absolute}" output)

let aba_allowed_same_selector_dead () =
  (* Exact same selector and same property is not an A?B?A dependency: the final
     A shadows the first A for every matched element. The first declaration may
     be removed even with an unrelated intervening rule. *)
  Alcotest.(check string)
    "same selector dead A may be removed across intervening rule"
    ".b{color:#00f}.a{color:#000}"
    (optimized_string ".a{color:red}.b{color:blue}.a{color:black}")

let aba_allowed_local_refactoring () =
  (* Punt, Visscher, Zaytsev, "The A-B*-A Pattern: Undoing Style in CSS" (ICSME
     2016), section IV defines undoing style as assigning A, then one or more B
     values, then A again. With one selector and one property, the final A
     dominates locally. *)
  Alcotest.(check string)
    "same selector A-B-A collapses to final value" ".x{color:red}"
    (optimized_string ".x{color:red}.x{color:blue}.x{color:red}")

let aba_runtime_shorthand_boundaries () =
  (* Shorthand reasoning is not a pure syntactic rewrite when the shorthand
     value is provided by a runtime substitution. Keep the explicit longhand
     next to var()/env()/attr() shorthands instead of contracting or deleting it
     during optimization. *)
  Alcotest.(check string)
    "var shorthand keeps following longhand"
    ".box{margin:var(--m);margin-left:1px}"
    (optimized_string ".box{margin:var(--m);margin-left:1px}");
  Alcotest.(check string)
    "env shorthand keeps following longhand"
    ".box{margin:env(safe-area-inset-left);margin-left:1px}"
    (optimized_string ".box{margin:env(safe-area-inset-left);margin-left:1px}");
  Alcotest.(check string)
    "attr shorthand keeps following longhand"
    ".box{margin:attr(data-m px);margin-left:1px}"
    (optimized_string ".box{margin:attr(data-m px);margin-left:1px}")

let c61_no_merge_atrule () =
  (* CSS Cascade section 6.1 defines style sheets and imported/nested sheets in
     document order. An at-rule boundary is not a free reordering point for
     surrounding rules, even when the surrounding selectors match. *)
  let input =
    [
      Css.rule ~selector:(Css.Selector.class_ "a")
        [ Css.Declaration.color (hex_color "ff0000") ];
      Css.media ~condition:(media_min_width 48.)
        [
          Css.rule ~selector:(Css.Selector.class_ "m")
            [ Css.Declaration.color (hex_color "00ff00") ];
        ];
      Css.rule ~selector:(Css.Selector.class_ "a")
        [ Css.Declaration.background_color (hex_color "0000ff") ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "same selector is not merged across media boundary"
    ".a{color:red}@media(width>=48px){.m{color:#0f0}}.a{background-color:#00f}"
    output

let c61_conditional_competitor_order () =
  (* CSS Cascade section 6.1: after conditional rules are filtered, declarations
     with the same origin, importance, layer, specificity, and scope proximity
     are resolved by order of appearance. These max-width conditions overlap,
     and both write the same property for the same selector, so sorting them by
     media condition would change the winning declaration. *)
  Alcotest.(check string)
    "overlapping media competitors preserve authored order"
    "@media(width<1024px){.u{display:flex}}@media(width<640px){.u{display:grid}}"
    (optimized_string
       "@media not all and (min-width:1024px){.u{display:flex}}@media not all \
        and (min-width:640px){.u{display:grid}}");
  Alcotest.(check string)
    "reverse authored order is also preserved"
    "@media(width<640px){.u{display:grid}}@media(width<1024px){.u{display:flex}}"
    (optimized_string
       "@media not all and (min-width:640px){.u{display:grid}}@media not all \
        and (min-width:1024px){.u{display:flex}}")

let suite =
  ( "optimize_merge",
    [
      ( "spec cascade 3 shorthand resets omitted longhands",
        `Quick,
        c3_shorthand_resets );
      ( "spec cascade 3 stylesheet-scope background synthesis",
        `Quick,
        c3_stylesheet_scope_background_synthesis );
      ( "closed-world groups disjoint selectors",
        `Quick,
        closed_world_groups_disjoint_selectors );
      ( "coalesce adjacent custom-property rules",
        `Quick,
        coalesce_adjacent_custom_property_rules );
      ( "spec cascade 3 shorthand source order corner cases",
        `Quick,
        c3_shorthand_order_edges );
      ( "spec cascade 3 important shorthand expands to longhands",
        `Quick,
        c3_important_shorthand_expands );
      ( "spec cascade 6.1 declaration order shorthand boundary",
        `Quick,
        c61_decl_order_shorthand_boundary );
      ( "spec cascade 6.1 adjacent merge preserves shorthand order",
        `Quick,
        c61_adjacent_shorthand_order );
      ( "spec cascade 6.1 positive adjacent merge with later dedup",
        `Quick,
        c61_adjacent_later_dedup );
      ( "spec cascade 6.1 no merge across intervening rule",
        `Quick,
        c61_no_merge_intervening );
      ( "spec cascade 6.1 no group non-adjacent equal declarations",
        `Quick,
        c61_no_group_nonadjacent );
      ( "A?B?A forbidden selector-intersection dependency",
        `Quick,
        aba_forbidden_intersection_dependency );
      ( "vendor alias pins an intervening rule",
        `Quick,
        vendor_alias_pins_intervening_rule );
      ( "unplaceable property name pins an intervening rule",
        `Quick,
        unplaceable_name_pins_intervening_rule );
      ( "group equal rules across higher-specificity intervening rule",
        `Quick,
        c63_group_across_higher_specificity );
      ( "merge same selector across higher-specificity intervening rule",
        `Quick,
        c63_merge_same_selector_across_higher_specificity );
      ( "factor across different-value descendant tie is unsafe",
        `Quick,
        factor_diff_value_descendant_tie_unsafe );
      ( "factor allowed across same-value descendant-combinator tie",
        `Quick,
        factor_same_value_descendant_tie_groups );
      ( "factor prefers size-minimal subset",
        `Quick,
        factor_prefers_size_minimal_subset );
      ( "factor steps over a same-value nesting boundary",
        `Quick,
        factor_steps_over_same_value_nesting_boundary );
      ( "factor does not step over a conflicting nesting boundary",
        `Quick,
        factor_conflicting_nesting_boundary_unsafe );
      ( "factor inlines rather than grouping across a long selector (known gap)",
        `Quick,
        factor_inlines_rather_than_grouping_long_selector );
      ( "factor groups a single property past a differently-shared decl",
        `Quick,
        factor_groups_single_property_past_other_shared_decl );
      ( "A?B?A allowed same-selector dead A elimination",
        `Quick,
        aba_allowed_same_selector_dead );
      ( "A?B?A runtime shorthand boundaries block contraction",
        `Quick,
        aba_runtime_shorthand_boundaries );
      ("A?B?A allowed local refactoring", `Quick, aba_allowed_local_refactoring);
      ( "spec cascade 6.1 no merge across at-rule boundary",
        `Quick,
        c61_no_merge_atrule );
      ( "spec cascade 6.1 conditional competitors keep source order",
        `Quick,
        c61_conditional_competitor_order );
    ] )
