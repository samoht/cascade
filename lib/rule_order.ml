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

(* The graph node standing for one element. A block collapses to a single
   pseudo-rule holding every contained selector branch and declaration: its edge
   set is the cross product, a superset of the true per-rule edges, so conflict
   detection stays conservative while the block reorders as one unit. The
   applicability condition itself is irrelevant to safety: whether or not the
   condition holds, a swap with a non-conflicting neighbour cannot change any
   computed value. *)
let element_node (stmt : statement) (rules : rule list) : rule =
  match (stmt, rules) with
  | Rule r, _ -> r
  | _, [] ->
      {
        selector = Selector.universal;
        declarations = [];
        nested = [];
        merge_key = None;
      }
  | _, rules ->
      let branches =
        List.concat_map (fun (r : rule) -> Edge.selectors r.selector) rules
      in
      let selector =
        match branches with [ sel ] -> sel | sels -> Selector.List sels
      in
      {
        selector;
        declarations = List.concat_map (fun (r : rule) -> r.declarations) rules;
        nested = [];
        merge_key = None;
      }

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

(* The conditions every rule of an element sits under: a block's own, and those
   of the one block it holds, if it holds exactly one. A condition only some of
   its rules sit under is not the element's. *)
type condition =
  | Media_condition of Media.t
  | Supports_condition of Supports.t
  | Container_condition of string option * Container.t

let rec element_conditions (stmt : statement) =
  let inner = function [ only ] -> element_conditions only | _ -> [] in
  match stmt with
  | Media (q, b) -> Media_condition q :: inner b
  | Supports (c, b) -> Supports_condition c :: inner b
  | Container (name, Option.Some c, b) -> (
      match (name, c) with
      | Option.None, Container.Named (name, c) ->
          Container_condition (Option.Some name, c) :: inner b
      | _ -> Container_condition (name, c) :: inner b)
  | Container (_, Option.None, _)
  | Rule _ | Declarations _ | Bang_comment _ | Charset _ | Import _
  | Namespace _ | Property _ | Layer_decl _ | Layer _ | Moz_document _
  | Starting_style _ | When _ | Else _ | Supports_condition _ | Origin _
  | Scope _ | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _ | Font_face _
  | Counter_style _ | Page _ | Page_with_margins _ | Font_palette_values _
  | Font_feature_values _ | View_transition _ | Position_try _ | Viewport _
  | Unknown_at_rule _ ->
      []

(* Media Queries 4 sec. 3.4: [not] negates the query it prefixes, all of it.
   [only] is not a negation and a query list has no single one. *)
let negated_media : Media.t -> Media.t option = function
  | Cond (Not c) -> Option.Some (Cond c)
  | Cond c -> Option.Some (Cond (Not c))
  | Type ({ prefix = Option.None; _ } as q) ->
      Option.Some (Type { q with prefix = Option.Some Media.Not })
  | Type ({ prefix = Option.Some Media.Not; _ } as q) ->
      Option.Some (Type { q with prefix = Option.None })
  | Type { prefix = Option.Some Media.Only; _ } | List _ -> Option.None

(* Whether two conditions cannot both hold for one element. Only a condition and
   its exact negation are read: Media Queries 4 sec. 3.4 and CSS Conditional 3
   sec. 6.1 have [not] negate the whole query or condition, turning true into
   false, false into true and unknown into unknown, which applies nothing, so
   the two are never true together. CSS Conditional 5 sec. 7.2 picks an
   element's query container by name and by the features the query asks, which a
   negation shares, so a container query and its negation on one name ask one
   container. *)
let excludes a b =
  let negates negate equal x y =
    match negate y with Option.Some ny -> equal x ny | Option.None -> false
  in
  match (a, b) with
  | Media_condition x, Media_condition y ->
      negates negated_media Media.equal x y
      || negates negated_media Media.equal y x
  | Supports_condition x, Supports_condition y ->
      Supports.equal x (Supports.Not y) || Supports.equal y (Supports.Not x)
  | Container_condition (nx, x), Container_condition (ny, y) ->
      Option.equal String.equal nx ny
      && (Container.equal x (Container.Not y)
         || Container.equal y (Container.Not x))
  | _ -> false

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

(* Two rules that agree wherever they meet. Restrict each to the declarations
   that overlap the other rule at all; when those two sequences are equal, the
   writes reaching any one property are the same declarations in the same order
   whichever rule comes first, so swapping the two changes no computed value.
   The graph's test is pairwise, and a prefixed property beside its unprefixed
   twin writes one slot twice, so it reads the two copies of a hoisted group as
   tied writers of that slot. Factoring produces exactly that shape: it writes
   one declaration list into every branch of the group. *)
let overlapping_writes_equal (a : rule) (b : rule) =
  let with_keys (r : rule) =
    List.map (fun d -> (d, Shorthand.declaration_overlap_keys d)) r.declarations
  in
  let touching x y =
    List.filter_map
      (fun (decl, keys) ->
        let meets (other, other_keys) =
          Shorthand.declarations_overlap_with_keys decl keys other other_keys
        in
        if List.exists meets y then Some decl else None)
      x
  in
  let a = with_keys a and b = with_keys b in
  List.equal Declaration.same_minified (touching a b) (touching b a)

(* The shape a run element folds in: a style rule merges declaration lists under
   one selector, a conditional block concatenates bodies under an unchanged
   prelude. [element_shape] is the one place the statements that can pair are
   enumerated - the key that decides two elements share a cascade slot and the
   merge that folds them both read it, so a statement cannot become mergeable
   for one and stay unknown to the other. *)
type block_prelude =
  | Media_query of Media.t
  | Feature_query of Supports.t
  | Container_query of string option * Container.t option

type merge_shape = Style of rule | Block of block_prelude * statement list

let element_shape (stmt : statement) =
  match stmt with
  | Rule r when r.merge_key = None -> Some (Style r)
  | Media (m, body) -> Some (Block (Media_query m, body))
  | Supports (c, body) -> Some (Block (Feature_query c, body))
  | Container (name, cond, body) ->
      Some (Block (Container_query (name, cond), body))
  | Rule _ | Declarations _ | Bang_comment _ | Charset _ | Import _
  | Namespace _ | Property _ | Layer_decl _ | Layer _ | Moz_document _
  | Starting_style _ | When _ | Else _ | Supports_condition _ | Origin _
  | Scope _ | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _ | Font_face _
  | Counter_style _ | Page _ | Page_with_margins _ | Font_palette_values _
  | Font_feature_values _ | View_transition _ | Position_try _ | Viewport _
  | Unknown_at_rule _ ->
      None

let block_statement prelude body =
  match prelude with
  | Media_query m -> Media (m, body)
  | Feature_query c -> Supports (c, body)
  | Container_query (name, cond) -> Container (name, cond, body)

(* What makes two run elements the same cascade slot, so that one can fold into
   the other. Style, media and supports shapes retain their text keys, while a
   container condition uses the structural equality that decides whether two
   queries select the same containers. Its normalised spelling is only a hash:
   collisions still reach [equal] instead of becoming merges. A key carries its
   hash, worked out once: the rounds of {!settle} look every element's key up
   several times each. *)
module Shape_key = struct
  type shape =
    | Text of string
    | Container of string option * Container.t option

  type t = { shape : shape; hash : int }

  let v shape =
    let hash =
      match shape with
      | Text text -> Hashtbl.hash (0, text)
      | Container (name, cond) ->
          let cond =
            Option.map
              (fun c ->
                Container.to_string ~minify:true (Container.normalize c))
              cond
          in
          Hashtbl.hash (1, name, cond)
    in
    { shape; hash }

  let equal a b =
    Int.equal a.hash b.hash
    &&
    match (a.shape, b.shape) with
    | Text a, Text b -> String.equal a b
    | Container (an, ac), Container (bn, bc) ->
        Option.equal String.equal an bn && Option.equal Container.equal ac bc
    | (Text _ | Container _), _ -> false

  let hash t = t.hash
end

module Shape_table = Hashtbl.Make (Shape_key)

let shape_key shape =
  Shape_key.v
    (match shape with
    | Style r -> Text (Pp.to_string ~minify:true Selector.pp r.selector)
    | Block (Media_query m, _) ->
        Text (String.concat "" [ "@media "; Media.to_string m ])
    | Block (Feature_query c, _) ->
        Text (String.concat "" [ "@supports "; Supports.to_string c ])
    | Block (Container_query (name, cond), _) -> Container (name, cond))

let element_key (stmt : statement) = Option.map shape_key (element_shape stmt)

(* A run element and what the rounds of {!settle} read off it, worked out once:
   the graph node standing for it, what {!Rule_graph.facts_conflict} reads off
   that node, its shape key, the conditions every rule of it sits under, and its
   serialized text, which ranks it among the elements it is free to pass. *)
type element = {
  stmt : statement;
  rules : rule list;
  node : rule;
  facts : Rule_graph.facts Lazy.t;
  plain : bool;
  key : Shape_key.t option;
  conditions : condition list;
  text : string Lazy.t;
}

let element ?parent stmt rules =
  let node = element_node stmt rules in
  {
    stmt;
    rules;
    node;
    facts = lazy (Rule_graph.facts ?parent node);
    plain = (match stmt with Rule _ -> true | _ -> false);
    key = element_key stmt;
    conditions = element_conditions stmt;
    text = lazy (Pp.to_string ~minify:true pp_stylesheet [ stmt ]);
  }

(* The elements a run has held, by graph node: the ones it started with, then
   each one a fold produced, numbered as {!Rule_graph.fold} numbers its
   nodes. *)
type store = { mutable elements : element array; mutable count : int }

let add_element store e =
  if store.count = Array.length store.elements then begin
    let grown = Array.make (2 * store.count) e in
    Array.blit store.elements 0 grown 0 store.count;
    store.elements <- grown
  end;
  store.elements.(store.count) <- e;
  store.count <- store.count + 1

(* Two nodes of a run, the one whose writes would move and the one they would
   move to. *)
module Move_table = Hashtbl.Make (struct
  type t = int * int

  let equal (a, b) (c, d) = Int.equal a c && Int.equal b d
  let hash = Hashtbl.hash
end)

(* One run being settled: its elements, the graph of their order, and for each
   move a round found observed, the node that observed it, all kept across
   rounds. *)
type settling = {
  canon_body : statement list -> statement list;
  ctx : Ctx.t;
  parent : Selector.t option;
  store : store;
  graph : Rule_graph.run;
  observers : int Move_table.t;
  held : unit Move_table.t;
      (** pairs of nodes a chain of conflicts orders, for {!join_held} *)
  sunk : unit Move_table.t;
      (** held pairs no declaration of the first can leave, for {!sink_held} *)
}

(* Mutable state of one coalescing scan over the run's current order: the node
   standing at each position as the scan found it, which is what the conflict
   test reads, the element each position holds once the scan has folded into it,
   the positions accumulated into each surviving one, and which remain. *)
type scan = {
  st : settling;
  start : int array;
  where : int array;  (** the position of each node in [start], or [-1] *)
  current : (statement * rule list) array;
  members : int list array;
  alive : bool array;
  mutable merged_any : bool;
}

let start_element scan i = scan.st.store.elements.(scan.start.(i))
let node = Rule_graph.Node_id.of_int_exn

let nodes_conflict scan i j =
  Rule_graph.run_conflict scan.st.graph
    (node scan.start.(i))
    (node scan.start.(j))

(* Nothing strictly between [lo] and [hi] observes any accumulated occurrence in
   [mems] moving across it. A conflict between two rules that write the
   overlapping slots identically is not observable, so it does not count; only
   plain style rules qualify, since a conditional block's node is the
   concatenation of its interior rules and that sequence is not the one any
   single element sees. A position holds the writes of every occurrence the scan
   folded into it, so each of them observes the move too. *)
let observer scan ~lo ~hi mems =
  let observed m a =
    nodes_conflict scan m a
    &&
    let em = start_element scan m and ea = start_element scan a in
    not (em.plain && ea.plain && overlapping_writes_equal em.node ea.node)
  in
  let writers m = if scan.alive.(m) then scan.members.(m) else [ m ] in
  let rec find m =
    if m >= hi then Option.None
    else if List.exists (fun w -> List.exists (observed w) mems) (writers m)
    then Option.Some m
    else find (m + 1)
  in
  find (lo + 1)

(* [moving] is [lo] or [hi], and its writes cross the interval. A move of one
   element no element folded into yet is decided by its two nodes and the nodes
   between them, and those do not change between rounds, only where they stand:
   a node that observed it once and still stands between the two observes it
   again, and the interval need not be read. *)
let interval_clear scan ~lo ~hi ~moving =
  match scan.members.(moving) with
  | [ only ] as mems when only = moving -> (
      let other = if moving = lo then hi else lo in
      let move = (scan.start.(moving), scan.start.(other)) in
      let between node =
        let p = scan.where.(node) in
        lo < p && p < hi
      in
      match Move_table.find_opt scan.st.observers move with
      | Option.Some node when between node -> false
      | _ -> (
          match observer scan ~lo ~hi mems with
          | Option.Some m ->
              Move_table.replace scan.st.observers move scan.start.(m);
              false
          | Option.None -> true))
  | mems -> Option.is_none (observer scan ~lo ~hi mems)

(* Moving an earlier [@supports] block down to a later block with the same
   condition happens only when that condition is true. In that case a
   declaration in the later block can make an intervening declaration
   irrelevant: an identical selector writing the same non-important property
   wins after both the original and merged order. Remove only those exact
   shadowed writes before asking whether the earlier block still conflicts with
   the interval. Other statements and importance combinations stay pinned. *)
let drop_shadowed_by_rules later_rules (rule : rule) : rule =
  let selector = Pp.to_string ~minify:true Selector.pp rule.selector in
  let shadowed : (Declaration.prop_key, unit) Hashtbl.t = Hashtbl.create 8 in
  List.iter
    (fun (later : rule) ->
      if
        String.equal selector
          (Pp.to_string ~minify:true Selector.pp later.selector)
      then
        List.iter
          (fun decl ->
            if not (Declaration.is_important decl) then
              Hashtbl.replace shadowed (Declaration.property_key decl) ())
          later.declarations)
    later_rules;
  let declarations =
    List.filter
      (fun decl ->
        Declaration.is_important decl
        || not (Hashtbl.mem shadowed (Declaration.property_key decl)))
      rule.declarations
  in
  if List.compare_lengths declarations rule.declarations = 0 then rule
  else { rule with declarations }

let supports_merge_down_clear scan ~from ~into =
  match (fst scan.current.(from), fst scan.current.(into)) with
  | Supports _, Supports _ ->
      let moving =
        element_node (fst scan.current.(from)) (snd scan.current.(from))
      in
      let later_rules = snd scan.current.(into) in
      let rec clear i =
        if i >= into then true
        else
          let conflicts =
            match fst scan.current.(i) with
            | Rule rule ->
                let residual = drop_shadowed_by_rules later_rules rule in
                residual.declarations <> []
                && Rule_graph.rules_conflict ?parent:scan.st.parent moving
                     residual
            | _ -> List.exists (nodes_conflict scan i) scan.members.(from)
          in
          (not conflicts) && clear (i + 1)
      in
      clear (from + 1)
  | _ -> false

(* The element a merge produces. Two elements only ever pair on an equal
   [element_key], and the key carries the slot's shape, so the two sides always
   agree; a pair that did not agree declines to merge, which is always sound
   since leaving a run alone is what the coalescing scan starts from. A block
   merge concatenates the bodies in source order and re-canonicalises the
   result: the two halves were each canonical alone, but their concatenation
   need not be. *)
let merged_rule ~ctx (a : rule) (b : rule) =
  {
    a with
    declarations = coalesced_declarations ~ctx (a.declarations @ b.declarations);
  }

let merged_element scan ~from ~into =
  let earlier, later = if from < into then (from, into) else (into, from) in
  match
    ( element_shape (fst scan.current.(earlier)),
      element_shape (fst scan.current.(later)) )
  with
  | Some (Style a), Some (Style b) ->
      let merged = merged_rule ~ctx:scan.st.ctx a b in
      Some (Rule merged, [ merged ])
  | Some (Block (prelude, ba)), Some (Block (_, bb)) ->
      let stmt = block_statement prelude (scan.st.canon_body (ba @ bb)) in
      Some (stmt, Option.value ~default:[] (element_rules stmt))
  | (Some (Style _ | Block _) | None), _ -> None

let merge scan changed ~from ~into =
  match merged_element scan ~from ~into with
  | None -> ()
  | Some element ->
      scan.current.(into) <- element;
      scan.members.(into) <- scan.members.(from) @ scan.members.(into);
      scan.alive.(from) <- false;
      scan.merged_any <- true;
      changed := true

(* Fold the earlier occurrence [i] down into [j] when nothing in between
   observes its writes moving; otherwise fold [j]'s writes up into [i] when
   nothing in between observes those. *)
let try_merge scan changed ~last ~key i j =
  if
    interval_clear scan ~lo:i ~hi:j ~moving:i
    || supports_merge_down_clear scan ~from:i ~into:j
  then begin
    merge scan changed ~from:i ~into:j;
    Shape_table.replace last key j
  end
  else if interval_clear scan ~lo:i ~hi:j ~moving:j then
    merge scan changed ~from:j ~into:i
  else Shape_table.replace last key j

(* Coalesce same-slot elements within one run. Folding an occurrence into
   another moves its writes past every element in between, which is observable
   only if one of those elements conflicts with what moved; the conflict test is
   {!Rule_graph.facts_conflict}, which is the conflict of the graph over the
   run, against every original occurrence already accumulated into the surviving
   element. A conditional block's node is the union of its interior selectors
   and declarations, a superset of its true conflicts, so the same test covers a
   block merge. [blocks] says whether blocks fold at all. An element the scan
   folded into becomes one new node of the graph. *)
(* The run after [scan]: each surviving element, an element the scan folded
   into standing as one new node of the graph in place of every node folded
   into it. *)
let folded scan =
  let st = scan.st and ids = scan.start in
  let survivor p =
    match scan.members.(p) with
    | [ q ] when q = p -> ids.(p)
    | members ->
        let stmt, rules = scan.current.(p) in
        let e = element ?parent:st.parent stmt rules in
        (* the graph asks [exclusive] about the node it adds *)
        add_element st.store e;
        Rule_graph.Node_id.to_int
          (Rule_graph.fold st.graph
             (List.map (fun q -> node ids.(q)) members)
             e.node
             ~at:(node ids.(p)))
  in
  let rec collect p acc =
    if p < 0 then acc
    else collect (p - 1) (if scan.alive.(p) then survivor p :: acc else acc)
  in
  Array.of_list (collect (Array.length ids - 1) [])

let coalesce ~blocks st changed (ids : int array) : int array =
  let n = Array.length ids in
  if n < 2 then ids
  else
    let scan =
      {
        st;
        start = ids;
        where =
          (let where = Array.make st.store.count (-1) in
           Array.iteri (fun p id -> where.(id) <- p) ids;
           where);
        current =
          Array.map
            (fun id ->
              let e = st.store.elements.(id) in
              (e.stmt, e.rules))
            ids;
        members = Array.init n (fun i -> [ i ]);
        alive = Array.make n true;
        merged_any = false;
      }
    in
    let last = Shape_table.create 16 in
    let key e =
      match e.stmt with Rule _ -> e.key | _ -> if blocks then e.key else None
    in
    for j = 0 to n - 1 do
      match key (start_element scan j) with
      | None -> ()
      | Some key -> (
          match Shape_table.find_opt last key with
          | Some i when scan.alive.(i) -> try_merge scan changed ~last ~key i j
          | _ -> Shape_table.replace last key j)
    done;
    if scan.merged_any then folded scan else ids

(* Reorder a run into a canonical linear extension of its dependency graph,
   breaking ties among independent elements by serialized statement content so
   two source orderings converge to one form, while preserving the run as it
   stands when that order already matches it. The graph expands each element's
   relative selectors against the enclosing nesting context, so overlap is
   computed on the effective selector, and records no conflict between two
   elements under a condition and its negation, which never apply together. *)
let sort st changed ids =
  let n = Array.length ids in
  if n < 2 then ids
  else
    let text i = Lazy.force st.store.elements.(ids.(i)).text in
    let ranked = Array.init n Fun.id in
    Array.sort
      (fun a b ->
        match String.compare (text a) (text b) with
        | 0 -> Int.compare a b
        | c -> c)
      ranked;
    let rank = Array.make st.store.count 0 in
    Array.iteri (fun pos i -> rank.(ids.(i)) <- pos) ranked;
    let order =
      Array.map Rule_graph.Node_id.to_int
        (Rule_graph.run_order st.graph (fun id ->
             rank.(Rule_graph.Node_id.to_int id)))
    in
    if Array.for_all2 Int.equal order ids then ids
    else begin
      changed := true;
      order
    end

(* The style rule standing at position [p] and its shape key. *)
let style_at st ids p =
  let e = st.store.elements.(ids.(p)) in
  match (element_shape e.stmt, e.key) with
  | Option.Some (Style r), Option.Some key -> Option.Some (r, key)
  | _ -> Option.None

(* The consecutive occurrences of one selector among the style rules of the run
   as it stands, as [(i, a, p, b)] with [a] at [i] and [b] at [p], by increasing
   [p]. *)
let selector_pairs st ids =
  let last = Shape_table.create 16 in
  let pairs = ref [] in
  for p = 0 to Array.length ids - 1 do
    match style_at st ids p with
    | Option.None -> ()
    | Option.Some (b, key) ->
        (match Shape_table.find_opt last key with
        | Option.Some (i, a) -> pairs := (i, a, p, b) :: !pairs
        | Option.None -> ());
        Shape_table.replace last key (p, b)
  done;
  List.rev !pairs

(* [ids] without position [i]. *)
let without ids i =
  Array.init
    (Array.length ids - 1)
    (fun q -> if q < i then ids.(q) else ids.(q + 1))

(* Apply [step] to the pairs of [selector_pairs] in turn, each one only when it
   stands wholly after every position an earlier step rewrote. A step rewrites
   the positions from its first occurrence to its later one: what it produced
   may have to follow nodes standing between the two, so only there does the
   order the run last stood in stop being one its graph allows. The nodes past
   the later occurrence keep their places and still follow every node before it,
   so what is read about a pair standing among them, every chain of conflicts
   leading to its later occurrence and what stands between the two, is what a
   fresh order would show. The first pair taken is the one a run that took one
   pair a round would take. [step] returns the run it leaves, [shift] the
   positions it removed from before its later occurrence, or [None] to leave the
   pair alone. *)
let batched st ids step =
  let ids, _, _ =
    List.fold_left
      (fun ((ids, shift, reach) as acc) (i, a, p, b) ->
        if i <= reach then acc
        else
          match step ids (i - shift, a, p - shift, b) with
          | Option.None -> acc
          | Option.Some (ids, removed) -> (ids, shift + removed, p))
      (ids, 0, -1) (selector_pairs st ids)
  in
  ids

(* Fold two occurrences of one selector that {!coalesce} keeps apart because
   each is held by a rule between them that conflicts with it, when no chain of
   conflicts through another rule orders the first before the second: the rules
   the first must precede then follow the fold and the rest precede it, an order
   that keeps every constrained pair. A chain found once stays, since a fold
   only joins nodes, so a pair it orders is not asked again. Every pair that
   folds is folded, on a run standing as {!sort} left it, through {!batched}. *)
let join_held st changed ids =
  let held ids i p =
    let pair = (ids.(i), ids.(p)) in
    Move_table.mem st.held pair
    || Rule_graph.run_orders st.graph (node ids.(i)) (node ids.(p))
       &&
       (Move_table.replace st.held pair ();
        true)
  in
  let join ids (i, a, p, b) =
    if held ids i p then Option.None
    else
      let merged = merged_rule ~ctx:st.ctx a b in
      let e = element ?parent:st.parent (Rule merged) [ merged ] in
      add_element st.store e;
      let id =
        Rule_graph.Node_id.to_int
          (Rule_graph.join st.graph
             [ node ids.(i); node ids.(p) ]
             e.node
             ~at:(node ids.(p)))
      in
      changed := true;
      let joined = Array.copy ids in
      joined.(p) <- id;
      Option.Some (without joined i, 1)
  in
  let joined = batched st ids join in
  if joined == ids then Option.None else Option.Some joined

(* Move into the later of two held occurrences of one selector every declaration
   of the earlier that no chain of conflicts orders before the later: the rules
   between the two that conflict with the declaration, and every rule they
   reach, stand no earlier than the later occurrence. The declaration then comes
   first in it, as it came before it. Each declaration only moves later, so this
   ends; a pair none of whose declarations moves stays that way, since a fold
   only joins nodes, and is not asked again. Two sheets that wrote one
   declaration in different occurrences of its selector, where the cascade lets
   it stand in either, so meet. *)
(* The rules standing after position [i], other than the later occurrence [b]
   at [p], that [d], written in [a], conflicts with and so has to precede
   wherever it stands, those past [p] only when [past]. A rule between the two
   whose every declaration [d] conflicts with [b] writes again is not one: [b]
   then decides those slots on whichever side of the rule [d] stands. *)
let followers ~past st ids i p (a : rule) (b : rule) d =
  let alone =
    Rule_graph.facts ?parent:st.parent { a with declarations = [ d ] }
  in
  let conflicts e = Rule_graph.facts_conflict alone (Lazy.force e.facts) in
  let rewritten x =
    List.exists
      (fun e ->
        Bool.equal (Declaration.is_important e) (Declaration.is_important x)
        && Shorthand.declaration_covers e x)
      b.declarations
  in
  let held_by e =
    conflicts e
    && not
         (List.for_all
            (fun x ->
              (not
                 (Shorthand.declarations_conflict_with_keys d
                    (Shorthand.declaration_overlap_keys d)
                    x
                    (Shorthand.declaration_overlap_keys x)))
              || rewritten x)
            e.node.declarations)
  in
  let n = if past then Array.length ids else p in
  let rec collect q acc =
    if q >= n then acc
    else
      let e = st.store.elements.(ids.(q)) in
      let follows = if q < p then held_by e else q > p && conflicts e in
      collect (q + 1) (if follows then node ids.(q) :: acc else acc)
  in
  collect (i + 1) []

(* [a]'s declarations no chain of conflicts orders before position [p], and the
   rest. A chain reaching [p] stands before it, so the rules past [p] that a
   declaration has to precede start none and are not collected. *)
let movable st ids i p (a : rule) b =
  List.partition
    (fun d ->
      not
        (Rule_graph.run_reaches st.graph
           (followers ~past:false st ids i p a b d)
           (node ids.(p))))
    a.declarations

(* Sink every held pair with a declaration to move, through {!batched}. A pair
   none of whose declarations moves is remembered as such. *)
let sink_held st changed ids =
  let sink ids (i, a, p, b) =
    if Move_table.mem st.sunk (ids.(i), ids.(p)) then Option.None
    else
      match movable st ids i p a b with
      | [], _ ->
          Move_table.replace st.sunk (ids.(i), ids.(p)) ();
          Option.None
      | moved, kept -> (
          let follow =
            List.sort_uniq Rule_graph.Node_id.compare
              (List.concat_map (followers ~past:true st ids i p a b) moved)
          in
          let replace q rule fold =
            let e = element ?parent:st.parent (Rule rule) [ rule ] in
            add_element st.store e;
            Rule_graph.Node_id.to_int (fold e.node ~at:(node ids.(q)))
          in
          let into =
            merged_rule ~ctx:st.ctx { a with declarations = moved } b
          in
          let sunk = Array.copy ids in
          changed := true;
          match kept with
          | [] ->
              sunk.(p) <-
                replace p into (fun rule ~at ->
                    Rule_graph.join ~follow ~from:[ at ] st.graph
                      [ node ids.(i); at ]
                      rule ~at);
              Option.Some (without sunk i, 1)
          | _ :: _ ->
              sunk.(i) <-
                replace i { a with declarations = kept } (fun rule ~at ->
                    Rule_graph.fold st.graph [ at ] rule ~at);
              sunk.(p) <-
                replace p into (fun rule ~at ->
                    Rule_graph.join ~follow st.graph [ at ] rule ~at);
              Option.Some (sunk, 0))
  in
  let sunk = batched st ids sink in
  if sunk == ids then Option.None else Option.Some sunk

(* Sort and coalesce to a fixed point: sorting can empty the interval between
   two same-selector occurrences that a conflicting element previously kept
   apart, enabling a merge the source order hid, so equivalent sheets converge
   regardless of which arrangement they started from. Each merging round removes
   at least one element, so this terminates. Each round reads the graph and the
   elements the rounds before it left, so a round costs what changed since.

   Fold what the run already offers before sorting it. Sorting is free to move
   two same-selector occurrences apart, and a pair the input wrote adjacent has
   an empty interval and so always merges; taking it first is what makes the
   merged and unmerged spellings of that pair converge, where sorting first
   strands the one that arrived already foldable.

   Blocks are the exception. {!expand_blocks} has already written every block as
   one block per rule, so the merged and unmerged spellings of a block pair
   arrive identical and there is nothing to converge; folding them before the
   sort would instead regroup the rules in the order the input wrote them, and
   the block each rule landed in would decide where it sorts. They fold once the
   sort has put the rules in canonical order. *)
(* Only two elements of one shape key fold, and folding keeps the key, so an
   element alone with its key stays in the graph to the end. *)
let folds elements =
  let sharing = Shape_table.create 16 in
  Array.iter
    (fun e ->
      Option.iter
        (fun key ->
          Shape_table.replace sharing key
            (1 + Option.value ~default:0 (Shape_table.find_opt sharing key)))
        e.key)
    elements;
  fun id ->
    match elements.(Rule_graph.Node_id.to_int id).key with
    | Some key -> Shape_table.find sharing key > 1
    | None -> false

let settling ~canon_body ~ctx ?parent elements =
  let store = { elements; count = Array.length elements } in
  let exclusive i j =
    let conditions id =
      store.elements.(Rule_graph.Node_id.to_int id).conditions
    in
    let cj = conditions j in
    List.exists (fun a -> List.exists (excludes a) cj) (conditions i)
  in
  let graph =
    Rule_graph.run ?parent ~exclusive ~folds:(folds elements)
      (Array.to_list (Array.map (fun e -> e.node) elements))
  in
  {
    canon_body;
    ctx;
    parent;
    store;
    graph;
    observers = Move_table.create 16;
    held = Move_table.create 16;
    sunk = Move_table.create 16;
  }

let settle ~canon_body ~ctx ?parent changed (run : (statement * rule list) list)
    : statement list =
  match run with
  | [] -> []
  | [ (stmt, _) ] -> [ stmt ]
  | run ->
      let elements =
        Array.of_list
          (List.map (fun (stmt, rules) -> element ?parent stmt rules) run)
      in
      let st = settling ~canon_body ~ctx ?parent elements in
      let rec rounds ids =
        let ids = coalesce ~blocks:false st changed ids in
        let ids = sort st changed ids in
        let coalesced = coalesce ~blocks:true st changed ids in
        if coalesced != ids then rounds coalesced
        else
          match join_held st changed ids with
          | Option.Some joined -> rounds (sort st changed joined)
          | Option.None -> (
              match sink_held st changed ids with
              | Option.None -> ids
              | Option.Some sunk -> rounds (sort st changed sunk))
      in
      rounds (Array.init (Array.length elements) Fun.id)
      |> Array.map (fun id -> st.store.elements.(id).stmt)
      |> Array.to_list

(* A declaration a conditional group repeats from the rule with the identical
   selector before it, value and all, changes nothing: an engine reading the
   guard sets the same value twice and one that does not reads the first rule
   alone, so the sheet with the repeat and the one without paint alike. The
   repeat is dropped so the two converge, unless a rule between the two writes
   the property: an element both match then reads the repeat as the winner. Only
   a rule directly inside the group is read, and only a declaration neither side
   marks [!important]. *)
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

let keys_written stmt =
  Stylesheet.fold_statements
    (fun acc stmt ->
      match stmt with
      | Rule r -> List.map Declaration.property_key r.declarations @ acc
      | _ -> acc)
    [] [ stmt ]

(* Per selector, the declarations of the last unguarded rule and where it
   stands; per property key, where the last rule to write it stands. A key
   written since that rule is one whose write stands after it. Writing every key
   into a set per selector instead cost each statement a pass over every
   selector seen, which is the square of the sheet. *)
type repeats = {
  seen : (string, Declaration.declaration list * int) Hashtbl.t;
  last_write : (Declaration.prop_key, int) Hashtbl.t;
  dropped : bool ref;
}

let selector_text sel = Pp.to_string ~minify:true Selector.pp sel

let note_writes t position stmt =
  List.iter
    (fun k -> Hashtbl.replace t.last_write k position)
    (keys_written stmt)

let repeated t sel d =
  (not (Declaration.is_important d))
  &&
  match Hashtbl.find_opt t.seen sel with
  | Some (decls, position) ->
      (match Hashtbl.find_opt t.last_write (Declaration.property_key d) with
        | Some written -> written <= position
        | None -> true)
      && List.exists
           (fun d0 ->
             (not (Declaration.is_important d0))
             && Declaration.equal_declaration d0 d)
           decls
  | None -> false

let prune_guarded_rule t : statement -> statement option = function
  | Rule r when r.nested = [] ->
      let sel = selector_text r.selector in
      let kept = List.filter (fun d -> not (repeated t sel d)) r.declarations in
      if List.compare_lengths kept r.declarations = 0 then Option.Some (Rule r)
      else (
        t.dropped := true;
        if kept = [] then Option.None
        else Option.Some (Rule { r with declarations = kept }))
  | stmt -> Option.Some stmt

(* A group emptied by the pruning goes with it; one that was empty already stays
   as it was. *)
let prune_guard t stmt body : statement option =
  let body' = List.filter_map (prune_guarded_rule t) body in
  if body' = [] && body <> [] then Option.None
  else Option.Some (with_conditional_body stmt body')

let drop_guarded_repeats outer_changed stmts =
  let t =
    {
      seen = Hashtbl.create 16;
      last_write = Hashtbl.create 64;
      dropped = ref false;
    }
  in
  let rec go position = function
    | [] -> []
    | (Rule r as stmt) :: rest ->
        note_writes t position stmt;
        Hashtbl.replace t.seen (selector_text r.selector)
          (r.declarations, position);
        stmt :: go (position + 1) rest
    | stmt :: rest -> (
        let pruned =
          match conditional_body stmt with
          | Option.Some body -> prune_guard t stmt body
          | Option.None -> Option.Some stmt
        in
        match pruned with
        | Option.Some stmt' ->
            note_writes t position stmt';
            stmt' :: go (position + 1) rest
        | Option.None -> go (position + 1) rest)
  in
  let result = go 0 stmts in
  if !(t.dropped) then (
    outer_changed := true;
    result)
  else stmts

(* A declaration a later rule with the *identical* selector also writes is dead
   when the later one is at least as important: same element set, same
   specificity, and CSS Cascade 5 sec. 6.1 ranks importance before order of
   appearance, then the later wins. Dropping it lets a sheet that hoisted the
   declaration into a shared group converge with one that wrote it inline - the
   hoisted copy survives expansion only to be overridden. A later normal write
   leaves an earlier important one standing. A property whose duplicates the
   declaration dedup keeps verbatim ([content], [outline]) is shadowed only
   where that dedup would drop the earlier write in one rule, so a sheet that
   wrote the two in one rule converges with one that wrote them in two. *)
(* Whether the later write [l] of [d]'s property shadows [d]. *)
let shadows ~ctx d l =
  if Shorthand.is_intentionally_duplicated d then
    not
      (List.memq d
         (Shorthand.deduplicate_declarations_with ~ctx ~merge_box:false [ d; l ]))
  else Declaration.is_important l || not (Declaration.is_important d)

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
  | Media (m, b) -> Media (m, here b)
  | Container (n, c, b) -> Container (n, c, here b)
  | Supports (s, b) -> Supports (s, here b)
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

and canonicalize_block ~ctx ~parent changed (stmts : statement list) :
    statement list =
  (* Canonicalise interiors first so run elements are ranked on their canonical
     serialized form, then undo grouping so equivalent factorings converge. *)
  let stmts = List.map (recurse ~ctx ~parent changed) stmts in
  let expanded =
    join_adjacent_lists ~ctx changed stmts
    |> List.concat_map expand_lists
    |> drop_shadowed_declarations ~ctx
    |> drop_guarded_repeats changed
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
        let canon_body = canonicalize_block ~ctx ~parent changed in
        let settled =
          match run with
          | [] -> []
          | run -> settle ~canon_body ~ctx ?parent changed run
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
   canonical comparison, at every declaration site. A comparison that judges for
   targets also drops, from both sides, a declaration of a prefix no target
   reads ({!Shorthand.read_by_no_target}): every target ignores it, and one the
   reader cannot type would otherwise touch every other declaration. *)
let canonical_vendor_aliases ~judged (stmts : statement list) : statement list =
  let unread decls =
    if judged then
      Common.List.filter_preserve
        (fun d -> not (Shorthand.read_by_no_target d))
        decls
    else decls
  in
  Stylesheet.map_declarations
    (fun decls ->
      unread (Shorthand.drop_redundant_decoration_color_aliases decls))
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
                      (canonical_vendor_aliases ~judged:(Option.is_some judge)
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
