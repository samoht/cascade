(** Canonical cascade-safe rule ordering: project each run of reorderable
    statements through {!Rule_graph} to its canonical linear extension. *)

open Stylesheet

(* A run element is a plain style rule, or a conditional at-rule block
   ([@media]/[@supports]/[@container]) whose transitive content is only such
   elements; {!expand_blocks} splits a block to one rule before the sort, and a
   block moves atomically with its rules supplying the conflict footprint.
   Custom properties are keyed by name in the dependency graph, so two writers
   of one property on overlapping selectors keep order while a [var()] reader
   (which writes its own property, not the one it reads) moves freely. An
   [@layer] statement pins the layer order at its first occurrence, so it stays
   a barrier; a named [@layer] block only holds its place among layer blocks. *)
let rec element_rules (stmt : statement) : rule list option =
  match stmt with
  | Rule r -> if r.nested = [] then Some [ r ] else None
  | Media (_, b) | Supports (_, b) | Container (_, _, b) -> block_rules b
  | Declarations _ | Bang_comment _ | Charset _ | Import _ | Namespace _
  | Property _ | Layer_decl _ | Layer _ | Moz_document _ | Starting_style _
  | When _ | Else _ | Supports_condition _ | Origin _ | Scope _ | Keyframes _
  | Webkit_keyframes _ | Moz_keyframes _ | Font_face _ | Counter_style _
  | Page _ | Page_with_margins _ | Font_palette_values _ | Font_feature_values _
  | View_transition _ | Position_try _ | Viewport _ | Unknown_at_rule _ ->
      None

and block_rules (stmts : statement list) : rule list option =
  List.fold_left
    (fun acc stmt ->
      match (acc, element_rules stmt) with
      | Some acc, Some rules -> Some (List.rev_append rules acc)
      | _ -> None)
    (Some []) stmts

(* Source-order-preserving edge count: [indeg.(j)] is the number of
   source-earlier declarations that overlap [j], which must all be emitted
   before [j]. [footprints] is the overlap key list of each declaration, hoisted
   out of the pair loop: computing it names the property, which serializes the
   declaration's property through a fresh buffer. *)
(* CSS Cascade 5 sec. 6.1 sorts by importance before order of appearance, so two
   declarations of different importance never decide each other in one rule,
   whatever they write. *)
let orders a a_keys b b_keys =
  Bool.equal (Declaration.is_important a) (Declaration.is_important b)
  && Shorthand.declarations_overlap_with_keys a a_keys b b_keys

let overlap_indegrees (arr : Declaration.declaration array) footprints :
    int array =
  let n = Array.length arr in
  let indeg = Array.make n 0 in
  for j = 0 to n - 1 do
    for i = 0 to j - 1 do
      if orders arr.(i) footprints.(i) arr.(j) footprints.(j) then
        indeg.(j) <- indeg.(j) + 1
    done
  done;
  indeg

(* Among not-yet-emitted declarations whose overlapping predecessors are all
   emitted ([indeg = 0]), the one with the smallest key (then index). *)
let ready_min ~compare ~emitted ~indeg ~keys =
  let best = ref (-1) in
  for i = 0 to Array.length keys - 1 do
    if (not emitted.(i)) && indeg.(i) = 0 then
      if
        !best < 0
        ||
        let c = compare keys.(i) keys.(!best) in
        c < 0 || (c = 0 && i < !best)
      then best := i
  done;
  !best

(* A rule's declarations in the order [compare] gives their [key]s, within what
   the cascade allows. Two declarations whose footprints overlap - the same
   property, or a shorthand and a longhand writing a common slot - keep their
   source order because it is cascade-significant; two with disjoint footprints
   can never change each other's computed value, so they sort. Preserves
   physical identity when the source order is already that order. *)
let ordered_declarations ~key ~compare (decls : Declaration.declaration list) :
    Declaration.declaration list =
  match decls with
  | [] | [ _ ] -> decls
  | _ ->
      let arr = Array.of_list decls in
      let n = Array.length arr in
      let keys = Array.map key arr in
      let footprints = Array.map Shorthand.declaration_overlap_keys arr in
      let indeg = overlap_indegrees arr footprints in
      let emitted = Array.make n false in
      let order = Array.make n 0 in
      let changed = ref false in
      for pos = 0 to n - 1 do
        let b = ready_min ~compare ~emitted ~indeg ~keys in
        if b <> pos then changed := true;
        emitted.(b) <- true;
        order.(pos) <- b;
        for j = b + 1 to n - 1 do
          if
            (not emitted.(j))
            && orders arr.(b) footprints.(b) arr.(j) footprints.(j)
          then indeg.(j) <- indeg.(j) - 1
        done
      done;
      if not !changed then decls
      else Array.to_list (Array.map (fun i -> arr.(i)) order)

let declaration_text d = Pp.to_string ~minify:true Declaration.pp d

(* Canonical order for a rule's declarations: the commuting ones by content. The
   declaration-level analogue of the rule-level reordering, so two rules holding
   the same declarations in a different commuting order project to one canonical
   form. *)
let canonical_declarations decls =
  ordered_declarations ~key:declaration_text ~compare:String.compare decls

(* The order a rule's declarations are composed in. The shorthand composers fold
   a run of adjacent longhands, so a declaration standing between two longhands
   of one family keeps them apart, and whether [top], [right], [bottom] and
   [left] become [inset] would turn on where the source, or the merge of two
   rules, happened to put an unrelated [position]. Ranking each declaration by
   its {!Shorthand.footprint_family} first brings every family together wherever
   the overlap order lets it, so the composers read the declarations a rule
   holds, not the order it holds them in. Within a family the wider footprint
   comes first, so [border-width], [border-style] and [border-color] stand
   together ahead of a [border-top-color] that overrides one side of them. *)
let gathered_declarations decls =
  let key d =
    ( Option.value ~default:max_int (Shorthand.footprint_family d),
      List.length (Shorthand.declaration_overlap_keys d),
      declaration_text d )
  in
  let compare (f, w, t) (f', w', t') =
    match Int.compare f f' with
    | 0 -> ( match Int.compare w' w with 0 -> String.compare t t' | c -> c)
    | c -> c
  in
  ordered_declarations ~key ~compare decls

(* A grouped rule is the sequence of its per-branch rules, and a hoisted shared
   declaration is the same declaration written inline, so two sheets that factor
   the same content differently ([.absolute,.sr-only {position:absolute}] vs the
   declaration inline in [.sr-only]) only converge once grouping is undone.
   Every list expands, whether or not a branch occurs elsewhere: the projection
   must not depend on how the input happened to group its selectors. *)
let expand_lists (stmt : statement) : statement list =
  match stmt with
  | Rule r when r.nested = [] && r.merge_key = None -> (
      match Edge.selectors r.selector with
      | [] | [ _ ] -> [ stmt ]
      | branches -> List.map (fun selector -> Rule { r with selector }) branches
      )
  | _ -> [ stmt ]

(* Coalescing two occurrences of a selector concatenates their declarations, so
   an earlier write that a later one overrides (as when one occurrence carried a
   shared default the other specialises) becomes dead. Reduce with the same
   cascade dedup the optimizer uses - keep the last write of each property,
   preserving genuine fallback pairs - so a coalesced rule holds the same
   declaration set a sheet that never split it would. Composition waits for
   {!composed}: a family composed from part of its longhands here could not take
   the rest when a later merge brings them. *)
let coalesced_declarations ~ctx decls =
  canonical_declarations
    (Shorthand.deduplicate_declarations_with ~ctx ~merge_box:false decls)

(* Two adjacent rules over one selector list give every element the declarations
   of one rule over both bodies, in the same order, whichever branches it
   matches. {!expand_lists} would write each branch's two halves as occurrences
   the projection must fold back, and a declaration the footprint model cannot
   place reads as touching the other branch's second half, which holds them
   apart. So the two join while the list still says they are one. *)
let join_adjacent_lists ~ctx changed (stmts : statement list) : statement list =
  let list_rule = function
    | Rule
        ({ selector = Selector.List _; nested = []; merge_key = None; _ } as r)
      ->
        Some r
    | _ -> None
  in
  let same (a : rule) (b : rule) =
    String.equal
      (Pp.to_string ~minify:true Selector.pp a.selector)
      (Pp.to_string ~minify:true Selector.pp b.selector)
  in
  let rec go acc = function
    | a :: b :: rest -> (
        match (list_rule a, list_rule b) with
        | Some ra, Some rb when same ra rb ->
            let declarations =
              coalesced_declarations ~ctx (ra.declarations @ rb.declarations)
            in
            changed := true;
            go acc (Rule { ra with declarations } :: rest)
        | _ -> go (a :: acc) (b :: rest))
    | rest -> List.rev_append acc rest
  in
  go [] stmts

(* Whether two of [decls] share a footprint family, the least a composer
   needs. *)
let composable decls =
  let rec go seen = function
    | [] -> false
    | d :: rest -> (
        match Shorthand.footprint_family d with
        | None -> go seen rest
        | Some f -> List.exists (Int.equal f) seen || go (f :: seen) rest)
  in
  go [] decls

(* A rule body once every merge is done, composed over its gathered order. One
   rule, and the merge of two that hold the same declarations, reach one body:
   CSSOM serializes a declaration block the same way, composing a shorthand
   whenever the block holds all of its longhands, wherever they stand. *)
let composed_declarations ~ctx decls =
  if not (composable decls) then decls
  else
    Common.List.preserve decls
      (canonical_declarations
         (Shorthand.deduplicate_declarations_with ~ctx
            (gathered_declarations decls)))

(* The body of a conditional block. *)
let conditional_body : statement -> statement list option = function
  | Media (_, b) | Supports (_, b) | Container (_, _, b) -> Option.Some b
  | Rule _ | Declarations _ | Bang_comment _ | Charset _ | Import _
  | Namespace _ | Property _ | Layer_decl _ | Layer _ | Moz_document _
  | Starting_style _ | When _ | Else _ | Supports_condition _ | Origin _
  | Scope _ | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _ | Font_face _
  | Counter_style _ | Page _ | Page_with_margins _ | Font_palette_values _
  | Font_feature_values _ | View_transition _ | Position_try _ | Viewport _
  | Unknown_at_rule _ ->
      Option.None

let with_conditional_body stmt body =
  match stmt with
  | Media (q, _) -> Media (q, body)
  | Supports (c, _) -> Supports (c, body)
  | Container (n, c, _) -> Container (n, c, body)
  | Rule _ | Declarations _ | Bang_comment _ | Charset _ | Import _
  | Namespace _ | Property _ | Layer_decl _ | Layer _ | Moz_document _
  | Starting_style _ | When _ | Else _ | Supports_condition _ | Origin _
  | Scope _ | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _ | Font_face _
  | Counter_style _ | Page _ | Page_with_margins _ | Font_palette_values _
  | Font_feature_values _ | View_transition _ | Position_try _ | Viewport _
  | Unknown_at_rule _ ->
      stmt

(* A conditional block is the sequence of one block per rule it holds, the way
   {!expand_lists} reads a selector list as one rule per branch. A block moves
   through a run as one unit and stands in the graph for every selector it holds
   paired with every declaration it holds, so a rule is pinned by the writes of
   whatever the input happened to group beside it: a sheet that moved the rule
   into another block of the same condition, across an interval that observes
   none of its writes, could not be brought back. One rule per block gives every
   graph node its exact footprint, and coalescing folds adjacent blocks of one
   condition back together. *)
let rec expand_blocks (stmt : statement) : statement list =
  match (conditional_body stmt, element_rules stmt) with
  | Some body, Some _ -> (
      match List.concat_map expand_blocks body with
      | [] | [ _ ] -> [ stmt ]
      | pieces ->
          List.map (fun piece -> with_conditional_body stmt [ piece ]) pieces)
  | _ -> [ stmt ]

(* A declaration a later rule with the *identical* selector also writes is dead
   when the later one is at least as important: same element set, same
   specificity, and CSS Cascade 5 sec. 6.1 ranks importance before order of
   appearance, then the later wins. Dropping it lets a sheet that hoisted the
   declaration into a shared group converge with one that wrote it inline - the
   hoisted copy survives expansion only to be overridden. A later normal write
   leaves an earlier important one standing. A property whose duplicates the
   declaration dedup keeps verbatim ([content], [outline]) is shadowed only
   where that dedup would drop the earlier write in one rule, so a sheet that
   wrote the two in one rule converges with one that wrote them in two. A later
   write a browser at or above the floors drops at parse time leaves the earlier
   one as what that browser paints ({!Declaration.fallback_for}). *)
(* Whether the later write [l] of [d]'s property shadows [d]. *)
let shadows ~ctx d l =
  if Shorthand.is_intentionally_duplicated d then
    not
      (List.memq d
         (Shorthand.deduplicate_declarations_with ~ctx ~merge_box:false [ d; l ]))
  else
    (Declaration.is_important l || not (Declaration.is_important d))
    && not (Declaration.fallback_for ~floors:(Ctx.floors ctx) ~later:l d)

(* One backward pass: [later] holds, per selector, the declarations of each
   property the statements already walked write, in source order, and those are
   exactly the statements after the current one. Consulting it is a lookup
   rather than a rescan of the tail, so no selector is serialised more than
   once. *)
let drop_shadowed_declarations ~ctx stmts =
  (* Properties are keyed by their AST identity: two constructors that print
     alike are different properties, and a name-keyed table would have one
     shadow the other. *)
  let later :
      ( string,
        (Declaration.prop_key, Declaration.declaration list) Hashtbl.t )
      Hashtbl.t =
    Hashtbl.create 64
  in
  let written sel =
    match Hashtbl.find_opt later sel with
    | Some props -> props
    | None ->
        let props = Hashtbl.create 8 in
        Hashtbl.replace later sel props;
        props
  in
  let record props declarations =
    List.iter
      (fun d ->
        let key = Declaration.property_key d in
        let laters = Option.value ~default:[] (Hashtbl.find_opt props key) in
        Hashtbl.replace props key (d :: laters))
      (List.rev declarations)
  in
  let shadowed props d =
    match Hashtbl.find_opt props (Declaration.property_key d) with
    | None -> false
    | Some laters -> List.exists (shadows ~ctx d) laters
  in
  let rec go = function
    | [] -> []
    | (Rule r as stmt) :: rest ->
        (* The tail first, so [later] describes it by the time [stmt] asks. *)
        let rest = go rest in
        let props =
          written (Pp.to_string ~minify:true Selector.pp r.selector)
        in
        let kept =
          List.filter (fun d -> not (shadowed props d)) r.declarations
        in
        record props r.declarations;
        if List.compare_lengths kept r.declarations = 0 then stmt :: rest
        else Rule { r with declarations = kept } :: rest
    | stmt :: rest -> stmt :: go rest
  in
  go stmts

(* Canonicalise every cascade context. At-rule block bodies inherit the current
   nesting [parent]; a style rule's nested body is canonicalised under the
   rule's own (expanded) selector as the new parent, so nested relative
   selectors are compared on their effective form. *)
let rec recurse ~ctx ~(parent : Selector.t option) changed (stmt : statement) :
    statement =
  let here b = canonicalize_block ~ctx ~parent changed b in
  (* A conditional block of plain rules is an element of the run around it,
     which reads its entries again with every other entry of that run in view:
     only there is it known whether an entry is a repeat that nothing else's
     order rests on, so its own run drops none. *)
  let element b =
    canonicalize_block ~ctx ~parent
      ~drops:(Option.is_none (element_rules stmt))
      changed b
  in
  match stmt with
  | Rule r ->
      let child_parent =
        match parent with
        | None -> r.selector
        | Some p -> Nest.combine p r.selector
      in
      let nested =
        canonicalize_block ~ctx ~parent:(Some child_parent) changed r.nested
      in
      let declarations = canonical_declarations r.declarations in
      if declarations != r.declarations then changed := true;
      if nested == r.nested && declarations == r.declarations then stmt
      else Rule { r with declarations; nested }
  | Layer (n, b) -> Layer (n, here b)
  | Media (m, b) -> Media (m, element b)
  | Container (n, c, b) -> Container (n, c, element b)
  | Supports (s, b) -> Supports (s, element b)
  | Moz_document (c, b) -> Moz_document (c, here b)
  | When (c, b) -> When (c, here b)
  | Else (c, b) -> Else (c, here b)
  | Starting_style b -> Starting_style (here b)
  | Origin (o, b) -> Origin (o, here b)
  | Scope (s, e, b) -> Scope (s, e, here b)
  | Declarations _ | Bang_comment _ | Charset _ | Import _ | Namespace _
  | Property _ | Layer_decl _ | Supports_condition _ | Keyframes _
  | Webkit_keyframes _ | Moz_keyframes _ | Font_face _ | Counter_style _
  | Page _ | Page_with_margins _ | Font_palette_values _ | Font_feature_values _
  | View_transition _ | Position_try _ | Viewport _ | Unknown_at_rule _ ->
      stmt

and canonicalize_block ~ctx ~parent ?drops changed (stmts : statement list) :
    statement list =
  (* Canonicalise interiors first so run elements are ranked on their canonical
     serialized form, then undo grouping so equivalent factorings converge. *)
  let stmts = List.map (recurse ~ctx ~parent changed) stmts in
  let expanded =
    join_adjacent_lists ~ctx changed stmts
    |> List.concat_map expand_lists
    |> drop_shadowed_declarations ~ctx
    |> List.concat_map expand_blocks
  in
  if List.compare_lengths expanded stmts <> 0 then changed := true;
  (* CSS Cascade 5 sec. 6.1 sorts declarations by layer before order of
     appearance, so where a layer block stands among unlayered elements decides
     no tie with any of them. A stretch of elements and layer blocks reads as
     its elements, settled as one run, then its layer blocks in the order they
     came, which is the order of the layers. Nothing else ends the stretch
     differently: an [@layer] statement or a block declaring a layer inside it
     is not part of one, so every position in the layer order stays put. *)
  let rec go = function
    | [] -> []
    | stmt :: _ as stmts
      when (match stmt with Layer _ -> true | _ -> false)
           || Option.is_some (element_rules stmt) ->
        let rec take run layers moved = function
          | (Layer _ as s) :: r -> take run (s :: layers) moved r
          | s :: r as l -> (
              match element_rules s with
              | Some rs ->
                  take ((s, rs) :: run) layers (moved || layers <> []) r
              | None -> (List.rev run, List.rev layers, moved, l))
          | [] -> (List.rev run, List.rev layers, moved, [])
        in
        let run, layers, moved, rest = take [] [] false stmts in
        if moved then changed := true;
        let settled =
          match run with
          | [] -> []
          | run ->
              changed := true;
              Rule_trace.settle ~ctx ?parent ?drops (List.map fst run)
        in
        settled @ layers @ go rest
    | stmt :: rest -> stmt :: go rest
  in
  go expanded

(* A custom property is an opaque token stream, so a minifier that treats it as
   text keeps the spacing the author wrote while one that re-serialises a typed
   value drops it: [var(--a, var(--b), var(--c))] against
   [var(--a,var(--b),var(--c))]. The two are the same value, and
   {!Declaration.map_custom_value} settles it by construction: it rebuilds the
   declaration from the MINIFIED serialisation of its value, so the identity
   below is a re-serialisation and not a no-op. Rewriting that text is what a
   caller would reach for and there is nothing left to rewrite.

   A font name is the other half, and does need a rewrite: it spells the same
   family quoted or as the bare ident sequence it unquotes to, one word or
   several (CSS Fonts 4 sec. 2.1.1). A bare generic family in the stream is what
   proves the custom property holds a font stack; with that proof either form
   substitutes identically into [font-family], and without it the stream is
   arbitrary tokens and neither form may move. Emission keeps whichever the
   author wrote, so the projection folds the quoted form onto the ident sequence
   - the same normalisation the structural comparator applies through
   {!Css.declaration_value_for_equivalence}.

   An angle token is the third: CSS Values 4 sec. 6.1 makes [deg], [grad], [rad]
   and [turn] one dimension, and a [rad] converts through pi, so the fold spends
   the six-significant-figure budget a quotient takes and [lossless] holds it
   off, the unit kept as written as the typed path keeps it. *)
let normalize_custom_declaration ~lossless ~queried d =
  let d = Declaration.map_custom_value Fun.id d in
  let d =
    match d with
    | Declaration.Declaration { property = Custom_property name; _ }
      when List.mem name queried ->
        d
    | d ->
        let d = Declaration.canonicalize_custom_time d in
        if lossless then d else Declaration.canonicalize_custom_angle d
  in
  Declaration.unquote_custom_font_strings d

(* The fold reads one declaration, so it holds wherever the declaration sits: a
   [@keyframes] frame and a [@page] box spell a custom property exactly as a
   style rule does, and no block at-rule changes what the token stream means.
   {!Stylesheet.map_declarations} reaches every one of those sites, so the pass
   does not carry a list of the statements it descends through - the list is
   what left [@scope] and [@starting-style] answering differently from
   [@layer]. *)
let normalize_custom_values ~lossless (stmts : statement list) : statement list
    =
  (* CSS Conditional 5 sec. 6.2: a [style()] query on an unregistered property
     compares the declared tokens as written, so a property a query anywhere in
     the sheet names keeps its spelling. *)
  let queried =
    Stylesheet.fold_statements
      (fun acc stmt ->
        match stmt with
        | Container (_, Some condition, _) ->
            Container.queried_names condition @ acc
        | _ -> acc)
      [] stmts
  in
  Stylesheet.map_declarations
    (Common.List.map_preserve (normalize_custom_declaration ~lossless ~queried))
    stmts

(* CSS Color 4 sec. 10.2: [color(srgb r g b)] scales each channel by 255, so
   [color(srgb 1 0 0)] and [rgb(255 0 0)] are one colour written two ways. Under
   [--lossless] the optimizer keeps whichever function the author used, which
   leaves the projection reading the spelling as a difference. Fold the
   exactly-representable ones so it does not.

   Projection only, and only when the fold fires: the declaration is compared
   against the same normalisation without the flag, and kept as written unless
   the colour actually moved. So no other value fold rides along, and the
   emitted form is untouched - the two spellings are not interchangeable on
   output, since [color()] needs a browser that parses it. *)
let canonical_color_spelling ~plain decl =
  let folded = Declaration.normalize ~lossless:true ~exact_srgb:true decl in
  if folded == decl then decl
  else if Declaration.equal_declaration folded (Lazy.force plain) then decl
  else folded

(* A non-terminating quotient the minifier keeps as the author's [calc()]
   renders within the six-significant-figure budget of the number it folds to,
   and Tailwind writes [w-1/3] as [calc(1/3 * 100%)] where lightningcss writes
   [33.3333%]. The projection spends the budget on every typed quotient, as it
   is spent on a number's already, so the two spellings meet; [lossless] keeps
   the quotient and the two apart. *)
let canonical_quotient ~plain decl =
  let ctx = { Values.default_calc_ctx with budget = true } in
  let folded = Declaration.normalize ~lossless:true ~ctx decl in
  if folded == decl then decl
  else if Declaration.equal_declaration folded (Lazy.force plain) then decl
  else folded

(* CSS Color 4 sec. 4.4: "a missing component behaves as a zero value, in the
   appropriate unit for that component", for every purpose but the ones that
   combine two colours. A minifier that reaches an achromatic colour by
   conversion writes those channels as [none] where Cascade writes the hex the
   colour folds to, and the projection has to read the two as one colour.

   Sec. 13.3 draws the other edge: interpolation gives a missing component the
   other colour's analogous component rather than a zero, so at a position that
   interpolates the value the two spellings name two different results.
   {!Declaration.normalize} resolves the sentinel only for a colour standing as
   a whole colour-longhand value or as a colour function of a custom-property
   token stream, which leaves a gradient stop, a [color-mix()] operand and a
   shadow colour reading their [none] as written. The stream's colour is a
   colour wherever the stream substitutes, and the site it lands at is as
   invisible to the projection as it is to the hex fold the stream already
   takes. A declaration the sheet interpolates from elsewhere is held back too:
   [@keyframes] and [@starting-style] are the two blocks that exist to be one
   endpoint of that, so the pass does not descend into them, and a colour whose
   own rule transitions the property it writes keeps its [none], a custom
   property named by its own name included. That guard reads one rule. A
   transition one rule declares for a colour another rule sets still folds,
   since seeing it would mean deciding that two selectors match one element,
   which the projection does not do.

   Guarded like [canonical_color_spelling]: the flagged normalisation is kept
   only where it moves the colour, so no other value fold rides along. *)
let canonical_missing_components ~lossless ~plain decl =
  let folded = Declaration.normalize ~lossless ~resolve_missing:true decl in
  if folded == decl then decl
  else if Declaration.equal_declaration folded (Lazy.force plain) then decl
  else folded

(* The three folds above each compare the declaration they fold against its
   normalisation without their flag, and two of them, and all three under
   [lossless], compare against one normalisation of one declaration. So a
   declaration goes through the three in turn and each normalisation it is
   compared against is worked out once, when a fold first needs it. *)
let lossless_plain decl = lazy (Declaration.normalize ~lossless:true decl)

let canonical_value ~lossless decl =
  let plain = lossless_plain decl in
  let quotient = if lossless then decl else canonical_quotient ~plain decl in
  let plain = if quotient == decl then plain else lossless_plain quotient in
  let color = canonical_color_spelling ~plain quotient in
  (color, if color == quotient then plain else lossless_plain color)

let canonical_missing ~lossless decls (decl, plain) =
  let plain =
    if lossless then plain else lazy (Declaration.normalize ~lossless decl)
  in
  let folded = canonical_missing_components ~lossless ~plain decl in
  if folded == decl || not (Shorthand.transitioned_in_rule decls decl) then
    folded
  else decl

let canonical_declaration_values ~lossless ~missing decls =
  let values = List.map (canonical_value ~lossless) decls in
  let folded =
    if List.for_all2 (fun d (v, _) -> d == v) decls values then decls
    else List.map fst values
  in
  if not missing then folded
  else
    let missed = List.map (canonical_missing ~lossless folded) values in
    if List.for_all2 ( == ) folded missed then folded else missed

let rec canonical_values ~lossless ~missing (stmts : statement list) :
    statement list =
  Common.List.map_preserve
    (fun stmt ->
      let missing =
        missing
        &&
        match stmt with
        | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _ | Starting_style _
          ->
            false
        | _ -> true
      in
      stmt
      |> Stylesheet.map_statement_declarations
           (canonical_declaration_values ~lossless ~missing)
      |> Stylesheet.map_statement_children (canonical_values ~lossless ~missing))
    stmts

(* CSS Conditional 3 sec. 6.1 makes a [<general-enclosed>] term false, so an
   [@supports] guard false in every world selects no user agent and its block
   never applies: Tailwind writes [@supports (@media(width>=1px): var(--tw))]
   for [supports-[@media(width>=1px)]:flex] where tw writes nothing, and the two
   render alike. The optimizer keeps such a guard open for a sheet it emits; the
   projection answers for the spec as written and the browsers of today, and
   neither reading answers the guard yes, so the block goes under [enforce_spec]
   too. *)
let rec drop_unanswerable_supports (stmts : statement list) : statement list =
  List.filter_map
    (fun stmt ->
      match stmt with
      | Supports (cond, _) when Supports.never_holds cond -> None
      | stmt ->
          Some
            (Stylesheet.map_statement_children drop_unanswerable_supports stmt))
    stmts

(* The normal optimizer drops this typed alias under its maintained-browser
   policy, but the canonical optimizer runs spec-literally so it does not erase
   other compatibility content. Apply only the alias equivalence promised by
   canonical comparison, at every declaration site: a legacy name alias reads as
   the property it names, which the specs make it whatever the targets, so the
   passes after this see one property written twice. The optimizer hands the
   projection normalised declarations, so the one read under the property's
   grammar is normalised too: [0s] in a [transition] read afresh and the same
   [0s] after {!Declaration.normalize} print alike but are two values to the
   dedup. A comparison that judges for targets also drops, from both sides, a
   declaration of a prefix no target reads ({!Shorthand.read_by_no_target}):
   every target ignores it, and one the reader cannot type would otherwise touch
   every other declaration. *)
let canonical_vendor_aliases ~lossless ~judged (stmts : statement list) :
    statement list =
  let as_property d =
    let aliased = Shorthand.as_aliased_property d in
    if aliased == d then d else Declaration.normalize ~lossless aliased
  in
  let unread decls =
    if judged then
      Common.List.filter_preserve
        (fun d -> not (Shorthand.read_by_no_target d))
        decls
    else decls
  in
  Stylesheet.map_declarations
    (fun decls ->
      unread
        (Shorthand.drop_redundant_decoration_color_aliases
           (Common.List.map_preserve as_property decls)))
    stmts

(* CSS Properties and Values API 1 sec. 2: registrations for different custom
   property names are order-independent, and for the same name the last one
   wins. So a run of [@property] rules canonicalises to that run sorted by name,
   keeping the last registration of each - two sheets that register the same set
   then differ only in the order they happened to emit them. A non-registration
   statement splits a run, since reordering across it is not covered by that
   argument. *)
let sort_property_run (run : (string * statement) list) : statement list =
  let last = Hashtbl.create (List.length run) in
  List.iteri (fun i (name, _) -> Hashtbl.replace last name i) run;
  run
  |> List.filteri (fun i (name, _) -> Hashtbl.find last name = i)
  |> List.stable_sort (fun (a, _) (b, _) -> String.compare a b)
  |> List.map snd

let rec sort_property_runs (stmts : statement list) : statement list =
  let name_of stmt =
    match stmt with Stylesheet_intf.Property p -> Some p.name | _ -> None
  in
  (* [@property] is valid inside a conditional group rule and inside [@layer],
     so every block body is a run context of its own. *)
  let descend stmt =
    let here = sort_property_runs in
    match stmt with
    | Layer (n, b) -> Layer (n, here b)
    | Media (m, b) -> Media (m, here b)
    | Container (n, c, b) -> Container (n, c, here b)
    | Supports (s, b) -> Supports (s, here b)
    | Moz_document (c, b) -> Moz_document (c, here b)
    | When (c, b) -> When (c, here b)
    | Else (c, b) -> Else (c, here b)
    | Starting_style b -> Starting_style (here b)
    | Origin (o, b) -> Origin (o, here b)
    | Scope (s, e, b) -> Scope (s, e, here b)
    | Rule _ | Declarations _ | Bang_comment _ | Charset _ | Import _
    | Namespace _ | Property _ | Layer_decl _ | Supports_condition _
    | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _ | Font_face _
    | Counter_style _ | Page _ | Page_with_margins _ | Font_palette_values _
    | Font_feature_values _ | View_transition _ | Position_try _ | Viewport _
    | Unknown_at_rule _ ->
        stmt
  in
  let rec go acc = function
    | [] -> List.rev acc
    | stmt :: rest -> (
        match name_of stmt with
        | None -> go (descend stmt :: acc) rest
        | Some name ->
            let rec take run = function
              | s :: r as l -> (
                  match name_of s with
                  | Some n -> take ((n, s) :: run) r
                  | None -> (List.rev run, l))
              | [] -> (List.rev run, [])
            in
            let run, rest = take [ (name, stmt) ] rest in
            go (List.rev_append (sort_property_run run) acc) rest)
  in
  go [] stmts

(* Media Queries 4 sec. 2.3 makes [all] the identity media type, so the Level 3
   [not all and (X)] is the Level 4 [not (X)]; sec. 4.2 gives [min-X]/[max-X]
   and the range form one meaning, and a lower bound met by an upper bound one
   two-sided interval. Every one of those is a respelling - nothing is dropped
   and no query changes what it matches - so the projection takes them all.
   {!Media.lower_for_minify} is the rewrite, and it fires here on the input the
   optimizer leaves alone: statements a caller projects without optimizing, and
   spec-literal optimized output, which keeps the longer spellings because a
   Level 3 parser rejects the shorter ones. *)
let canonical_media : Media.t -> Media.t = Media.lower_for_minify

(* An [@container] prelude carries a media condition of its own, and the same
   respellings hold inside it, plus one the emitter cannot take: CSS Values 4
   sec. 9 makes a function name ASCII case-insensitive, so [style(] and [STYLE(]
   open one query. The AST keeps the case the author wrote and
   {!Container.lower_for_minify} leaves it alone for that reason, since emission
   round-trips it. A comparison has the opposite job, and {!Container.normalize}
   is the spelling its own equivalence class is compared by -- what
   {!Container.equal} already reads these two as. *)
let canonical_container : Container.t -> Container.t = Container.normalize

(* [@media] and [@container] are the only statements whose prelude this
   rewrites, so they are the only ones named; the descent below them is
   {!Stylesheet.map_statement_children}'s and reaches every block at-rule,
   including the ones they can be written inside. *)
let rec canonical_query_preludes (stmts : statement list) : statement list =
  Common.List.map_preserve
    (fun stmt ->
      let stmt =
        match stmt with
        | Media (q, inner) ->
            let q' = canonical_media q in
            if q' == q then stmt else Media (q', inner)
        | Container (name, Some cond, inner) ->
            let cond' = canonical_container cond in
            if cond' == cond then stmt else Container (name, Some cond', inner)
        | stmt -> stmt
      in
      Stylesheet.map_statement_children canonical_query_preludes stmt)
    stmts

(* CSS Conditional 3 sec. 2: a rule inside nested [@media] blocks applies when
   every condition holds, so which block encloses which is not part of what the
   rule matches, and a condition nested inside itself holds once. The projection
   reads each statement with the [@media] conditions around it, sorts and
   deduplicates them, and re-nests each run of statements sharing a set.
   Statements keep their order, so no rule moves past another. Only [@media] is
   read through: an [@supports] or [@container] between two of them ends the
   chain, and its own contents are projected on their own. *)
let same_media_conditions a b =
  List.equal (fun x y -> Media.compare x y = 0) a b

let rec media_leaves conds acc (stmts : statement list) =
  List.fold_left
    (fun acc stmt ->
      match stmt with
      | Media (q, inner) -> media_leaves (q :: conds) acc inner
      | stmt ->
          ( List.sort_uniq Media.compare conds,
            Stylesheet.map_statement_children canonical_media_nesting stmt )
          :: acc)
    acc stmts

and canonical_media_nesting (stmts : statement list) : statement list =
  let rec runs acc = function
    | [] -> List.rev acc
    | (conds, stmt) :: rest -> (
        match acc with
        | (c, run) :: acc' when same_media_conditions c conds ->
            runs ((c, stmt :: run) :: acc') rest
        | _ -> runs ((conds, [ stmt ]) :: acc) rest)
  in
  let nest (conds, run) =
    List.fold_right (fun q body -> [ Media (q, body) ]) conds (List.rev run)
  in
  List.concat_map nest (runs [] (List.rev (media_leaves [] [] stmts)))

(* CSS Animations 1 sec. 3: a keyframe block is a declaration block, so the
   declarations of one frame commute exactly as a style rule's do, and
   {!canonical_declarations} gives them the same order. *)
let canonical_frames (frames : Stylesheet_intf.keyframe list) =
  Common.List.map_preserve
    (fun (f : Stylesheet_intf.keyframe) ->
      let declarations = canonical_declarations f.declarations in
      if declarations == f.declarations then f else { f with declarations })
    frames

let rec canonical_keyframe_declarations (stmts : statement list) :
    statement list =
  Common.List.map_preserve
    (fun stmt ->
      match stmt with
      | Keyframes (n, frames) ->
          let frames' = canonical_frames frames in
          if frames' == frames then stmt else Keyframes (n, frames')
      | Webkit_keyframes (n, frames) ->
          let frames' = canonical_frames frames in
          if frames' == frames then stmt else Webkit_keyframes (n, frames')
      | Moz_keyframes (n, frames) ->
          let frames' = canonical_frames frames in
          if frames' == frames then stmt else Moz_keyframes (n, frames')
      | stmt ->
          Stylesheet.map_statement_children canonical_keyframe_declarations stmt)
    stmts

(* Every rule body of the settled sheet through {!composed_declarations}, at any
   depth. It runs once the runs have settled, so the longhands a merge gathers
   are all in one body before any of them composes. *)
let rec composed ~ctx (stmts : statement list) : statement list =
  Common.List.map_preserve
    (fun stmt ->
      match Stylesheet.map_statement_children (composed ~ctx) stmt with
      | Rule r as stmt ->
          let declarations = composed_declarations ~ctx r.declarations in
          if declarations == r.declarations then stmt
          else Rule { r with declarations }
      | stmt -> stmt)
    stmts

let canonicalize ?(lossless = false) ?(enforce_spec = false) ?judge
    (stmts : statement list) : statement list =
  let changed = ref false in
  let ctx = Ctx.of_scope ~enforce_spec ?judge None in
  let normalized =
    Rule_order_layer.fold_layer_pins
      (canonical_media_nesting
      @@ canonical_query_preludes
           (canonical_keyframe_declarations
           @@ sort_property_runs
                (canonical_values ~lossless ~missing:true
                   (normalize_custom_values ~lossless
                      (canonical_vendor_aliases ~lossless
                         ~judged:(Option.is_some judge)
                         (drop_unanswerable_supports stmts))))))
  in
  let result =
    canonicalize_block ~ctx
      ~parent:(None : Selector.t option)
      changed normalized
  in
  (* The runs above leave layer blocks side by side, which is where their order
     can be read. *)
  let settled = if !changed then result else normalized in
  Rule_order_layer.canonical_layer_declarations
    (Rule_order_layer.sort_layer_blocks (composed ~ctx settled))
