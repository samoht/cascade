(** Canonical cascade-safe rule ordering: project each run of reorderable
    statements through {!Rule_graph} to its canonical linear extension. *)

open Stylesheet

(* A run element is a plain style rule, or a conditional at-rule block
   ([@media]/[@supports]/[@container]) whose transitive content is only such
   elements; the block moves atomically and its rules supply the conflict
   footprint. Custom properties are keyed by name in the dependency graph, so
   two writers of one property on overlapping selectors keep order while a
   [var()] reader (which writes its own property, not the one it reads) moves
   freely. A named [@layer] block or declaration pins the layer order at its
   first occurrence, so it stays a barrier. *)
let rec element_rules (stmt : statement) : rule list option =
  match stmt with
  | Rule r -> if r.nested = [] then Some [ r ] else None
  | Media (_, b) | Supports (_, b) | Container (_, _, b) -> block_rules b
  | _ -> None

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

let is_identity order =
  let id = ref true in
  Array.iteri
    (fun i v -> if i <> Rule_graph.Node_id.to_int v then id := false)
    order;
  !id

(* Source-order-preserving edge count: [indeg.(j)] is the number of
   source-earlier declarations that overlap [j], which must all be emitted
   before [j]. [footprints] is the overlap key list of each declaration, hoisted
   out of the pair loop: computing it names the property, which serializes the
   declaration's property through a fresh buffer. *)
let overlap_indegrees (arr : Declaration.declaration array) footprints :
    int array =
  let n = Array.length arr in
  let indeg = Array.make n 0 in
  for j = 0 to n - 1 do
    for i = 0 to j - 1 do
      if
        Shorthand.declarations_overlap_with_keys arr.(i) footprints.(i) arr.(j)
          footprints.(j)
      then indeg.(j) <- indeg.(j) + 1
    done
  done;
  indeg

(* Among not-yet-emitted declarations whose overlapping predecessors are all
   emitted ([indeg = 0]), the one with the smallest content key (then index). *)
let ready_min ~emitted ~indeg ~keys =
  let best = ref (-1) in
  for i = 0 to Array.length keys - 1 do
    if (not emitted.(i)) && indeg.(i) = 0 then
      if
        !best < 0
        ||
        let c = String.compare keys.(i) keys.(!best) in
        c < 0 || (c = 0 && i < !best)
      then best := i
  done;
  !best

(* Canonical order for a rule's declarations. Two declarations whose footprints
   overlap - the same property, or a shorthand and a longhand writing a common
   slot - keep their source order because it is cascade-significant; two with
   disjoint footprints can never change each other's computed value, so they
   sort into a deterministic content order. The declaration-level analogue of
   the rule-level reordering, so two rules holding the same declarations in a
   different commuting order project to one canonical form. Preserves physical
   identity when the source order is already canonical. *)
let canonical_declarations (decls : Declaration.declaration list) :
    Declaration.declaration list =
  match decls with
  | [] | [ _ ] -> decls
  | _ ->
      let arr = Array.of_list decls in
      let n = Array.length arr in
      let keys =
        Array.map (fun d -> Pp.to_string ~minify:true Declaration.pp d) arr
      in
      let footprints = Array.map Shorthand.declaration_overlap_keys arr in
      let indeg = overlap_indegrees arr footprints in
      let emitted = Array.make n false in
      let order = Array.make n 0 in
      let changed = ref false in
      for pos = 0 to n - 1 do
        let b = ready_min ~emitted ~indeg ~keys in
        if b <> pos then changed := true;
        emitted.(b) <- true;
        order.(pos) <- b;
        for j = b + 1 to n - 1 do
          if
            (not emitted.(j))
            && Shorthand.declarations_overlap_with_keys arr.(b) footprints.(b)
                 arr.(j) footprints.(j)
          then indeg.(j) <- indeg.(j) - 1
        done
      done;
      if not !changed then decls
      else Array.to_list (Array.map (fun i -> arr.(i)) order)

(* Reorder one maximal run of elements into a canonical linear extension of its
   dependency graph, breaking ties among independent elements by serialized
   statement content so two source orderings converge to one form, while
   preserving physical identity when that order already matches the input.
   [parent], when set, is the enclosing nesting context: the graph expands each
   element's relative selectors against it so overlap is computed on the
   effective selector. *)
let sort_run ?parent changed (run : (statement * rule list) list) :
    statement list =
  let arr = Array.of_list run in
  let n = Array.length arr in
  if n < 2 then List.map fst run
  else
    let g =
      Rule_graph.of_rules ?parent ~pin_shared_branches:false
        (List.map (fun (stmt, rules) -> element_node stmt rules) run)
    in
    let keys =
      Array.map
        (fun (stmt, _) -> Pp.to_string ~minify:true pp_stylesheet [ stmt ])
        arr
    in
    let ranked = Array.init n (fun i -> i) in
    Array.sort
      (fun a b ->
        match String.compare keys.(a) keys.(b) with
        | 0 -> Int.compare a b
        | c -> c)
      ranked;
    let rank = Array.make n 0 in
    Array.iteri (fun pos i -> rank.(i) <- pos) ranked;
    let order =
      Rule_graph.canonical_order_by g (fun node ->
          rank.(Rule_graph.Node_id.to_int node))
    in
    if is_identity order then List.map fst run
    else begin
      changed := true;
      Array.to_list
        (Array.map (fun i -> fst arr.(Rule_graph.Node_id.to_int i)) order)
    end

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
   declaration set a sheet that never split it would. *)
let coalesced_declarations decls = Shorthand.deduplicate_declarations decls

(* Mutable state of one coalescing scan: the run's elements, the graph nodes
   accumulated into each surviving element, which elements remain, and which
   were plain style rules in the run as it was read. *)
type scan = {
  graph : Rule_graph.t;
  arr : (statement * rule list) array;
  plain : bool array;
  members : int list array;
  alive : bool array;
  mutable merged_any : bool;
}

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

(* Nothing strictly between [lo] and [hi] observes any accumulated occurrence in
   [mems] moving across it. A graph conflict between two rules that write the
   overlapping slots identically is not observable, so it does not count; only
   plain style rules qualify, since a conditional block stands in the graph for
   the concatenation of its interior rules and that sequence is not the one any
   single element sees. *)
let interval_clear scan ~lo ~hi mems =
  let node i = Rule_graph.Node_id.of_int_exn i in
  let observed m a =
    Rule_graph.conflict scan.graph (node m) (node a)
    && not
         (scan.plain.(m) && scan.plain.(a)
         && overlapping_writes_equal
              (Rule_graph.node_rule scan.graph (node m))
              (Rule_graph.node_rule scan.graph (node a)))
  in
  let ok = ref true in
  for m = lo + 1 to hi - 1 do
    if List.exists (observed m) mems then ok := false
  done;
  !ok

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

let rules_conflict ?parent a b =
  let graph = Rule_graph.of_rules ?parent ~pin_shared_branches:false [ a; b ] in
  Rule_graph.conflict graph
    (Rule_graph.Node_id.of_int_exn 0)
    (Rule_graph.Node_id.of_int_exn 1)

let supports_merge_down_clear ?parent scan ~from ~into =
  match (fst scan.arr.(from), fst scan.arr.(into)) with
  | Supports _, Supports _ ->
      let moving = element_node (fst scan.arr.(from)) (snd scan.arr.(from)) in
      let later_rules = snd scan.arr.(into) in
      let node i = Rule_graph.Node_id.of_int_exn i in
      let rec clear i =
        if i >= into then true
        else
          let conflicts =
            match fst scan.arr.(i) with
            | Rule rule ->
                let residual = drop_shadowed_by_rules later_rules rule in
                residual.declarations <> []
                && rules_conflict ?parent moving residual
            | _ ->
                List.exists
                  (fun member ->
                    Rule_graph.conflict scan.graph (node i) (node member))
                  scan.members.(from)
          in
          (not conflicts) && clear (i + 1)
      in
      clear (from + 1)
  | _ -> false

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
  | _ -> None

let block_statement prelude body =
  match prelude with
  | Media_query m -> Media (m, body)
  | Feature_query c -> Supports (c, body)
  | Container_query (name, cond) -> Container (name, cond, body)

(* The element a merge produces. Two elements only ever pair on an equal
   [element_key], and the key carries the slot's shape, so the two sides always
   agree; a pair that did not agree declines to merge, which is always sound
   since leaving a run alone is what the coalescing scan starts from. A block
   merge concatenates the bodies in source order and re-canonicalises the
   result: the two halves were each canonical alone, but their concatenation
   need not be. *)
let merged_element ~canon_body scan ~from ~into =
  let earlier, later = if from < into then (from, into) else (into, from) in
  match
    ( element_shape (fst scan.arr.(earlier)),
      element_shape (fst scan.arr.(later)) )
  with
  | Some (Style a), Some (Style b) ->
      let merged =
        {
          a with
          declarations =
            canonical_declarations
              (coalesced_declarations (a.declarations @ b.declarations));
        }
      in
      Some (Rule merged, [ merged ])
  | Some (Block (prelude, ba)), Some (Block (_, bb)) ->
      let stmt = block_statement prelude (canon_body (ba @ bb)) in
      Some (stmt, Option.value ~default:[] (element_rules stmt))
  | (Some (Style _ | Block _) | None), _ -> None

let merge ~canon_body scan changed ~from ~into =
  match merged_element ~canon_body scan ~from ~into with
  | None -> ()
  | Some element ->
      scan.arr.(into) <- element;
      scan.members.(into) <- scan.members.(from) @ scan.members.(into);
      scan.alive.(from) <- false;
      scan.merged_any <- true;
      changed := true

(* What makes two run elements the same cascade slot, so that one can fold into
   the other. Style, media and supports shapes retain their text keys, while a
   container condition uses the structural equality that decides whether two
   queries select the same containers. Its normalised spelling is only a hash:
   collisions still reach [equal] instead of becoming merges. *)
module Shape_key = struct
  type t = Text of string | Container of string option * Container.t option

  let equal a b =
    match (a, b) with
    | Text a, Text b -> String.equal a b
    | Container (an, ac), Container (bn, bc) ->
        Option.equal String.equal an bn && Option.equal Container.equal ac bc
    | (Text _ | Container _), _ -> false

  let hash = function
    | Text text -> Hashtbl.hash (0, text)
    | Container (name, cond) ->
        let cond =
          Option.map
            (fun c -> Container.to_string ~minify:true (Container.normalize c))
            cond
        in
        Hashtbl.hash (1, name, cond)
end

module Shape_table = Hashtbl.Make (Shape_key)

(* Fold the earlier occurrence [i] down into [j] when nothing in between
   observes its writes moving; otherwise fold [j]'s writes up into [i] when
   nothing in between observes those. *)
let try_merge ~canon_body ?parent scan changed ~last ~key i j =
  if
    interval_clear scan ~lo:i ~hi:j scan.members.(i)
    || supports_merge_down_clear ?parent scan ~from:i ~into:j
  then begin
    merge ~canon_body scan changed ~from:i ~into:j;
    Shape_table.replace last key j
  end
  else if interval_clear scan ~lo:i ~hi:j scan.members.(j) then
    merge ~canon_body scan changed ~from:j ~into:i
  else Shape_table.replace last key j

let shape_key = function
  | Style r -> Shape_key.Text (Pp.to_string ~minify:true Selector.pp r.selector)
  | Block (Media_query m, _) ->
      Shape_key.Text (String.concat "" [ "@media "; Media.to_string m ])
  | Block (Feature_query c, _) ->
      Shape_key.Text (String.concat "" [ "@supports "; Supports.to_string c ])
  | Block (Container_query (name, cond), _) -> Shape_key.Container (name, cond)

let element_key (stmt : statement) = Option.map shape_key (element_shape stmt)

(* Coalesce same-slot elements within one run. Folding an occurrence into
   another moves its writes past every element in between, which is observable
   only if one of those elements conflicts with what moved; the conflict test is
   the graph's, against every original occurrence already accumulated into the
   surviving element. A conditional block stands in the graph for the union of
   its interior selectors and declarations, a superset of its true edges, so the
   same test covers a block merge. *)
let coalesce ~canon_body ?parent changed (run : (statement * rule list) list) :
    (statement * rule list) list =
  match run with
  | [] | [ _ ] -> run
  | _ ->
      let arr = Array.of_list run in
      let n = Array.length arr in
      let graph =
        Rule_graph.of_rules ?parent ~pin_shared_branches:false
          (List.map (fun (stmt, rules) -> element_node stmt rules) run)
      in
      let scan =
        {
          graph;
          arr;
          plain =
            Array.map
              (fun (stmt, _) -> match stmt with Rule _ -> true | _ -> false)
              arr;
          members = Array.init n (fun i -> [ i ]);
          alive = Array.make n true;
          merged_any = false;
        }
      in
      let last = Shape_table.create 16 in
      for j = 0 to n - 1 do
        match element_key (fst arr.(j)) with
        | None -> ()
        | Some key -> (
            match Shape_table.find_opt last key with
            | Some i when scan.alive.(i) ->
                try_merge ~canon_body ?parent scan changed ~last ~key i j
            | _ -> Shape_table.replace last key j)
      done;
      if not scan.merged_any then run
      else Array.to_list arr |> List.filteri (fun i _ -> scan.alive.(i))

(* Sort and coalesce to a fixed point: sorting can empty the interval between
   two same-selector occurrences that a conflicting element previously kept
   apart, enabling a merge the source order hid, so equivalent sheets converge
   regardless of which arrangement they started from. Each merging round removes
   at least one element, so this terminates.

   Fold what the run already offers before sorting it. Sorting is free to move
   two same-selector occurrences apart, and a pair the input wrote adjacent has
   an empty interval and so always merges; taking it first is what makes the
   merged and unmerged spellings of that pair converge, where sorting first
   strands the one that arrived already foldable. *)
let rec settle ~canon_body ?parent changed (run : (statement * rule list) list)
    : statement list =
  let run = coalesce ~canon_body ?parent changed run in
  let stmts = sort_run ?parent changed run in
  let sorted =
    List.filter_map
      (fun s ->
        match element_rules s with
        | Some rules -> Some (s, rules)
        | None -> None)
      stmts
  in
  let coalesced = coalesce ~canon_body ?parent changed sorted in
  if coalesced == sorted then stmts
  else settle ~canon_body ?parent changed coalesced

(* A declaration a later rule with the *identical* selector also writes is dead:
   same element set, same specificity, later wins. Dropping it lets a sheet that
   hoisted the declaration into a shared group converge with one that wrote it
   inline - the hoisted copy survives expansion only to be overridden. Neither
   may carry [!important], which changes the winner. *)
(* One backward pass: [later] holds, per selector, the properties that the
   statements already walked write without [!important], and those are exactly
   the statements after the current one. Consulting it is a lookup rather than a
   rescan of the tail, so no selector is serialised more than once. *)
let drop_shadowed_declarations stmts =
  (* Properties are keyed by their AST identity: two constructors that print
     alike are different properties, and a name-keyed table would have one
     shadow the other. *)
  let later : (string, (Declaration.prop_key, unit) Hashtbl.t) Hashtbl.t =
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
  let record sel declarations =
    let props = written sel in
    List.iter
      (fun d ->
        if not (Declaration.is_important d) then
          Hashtbl.replace props (Declaration.property_key d) ())
      declarations
  in
  let rec go = function
    | [] -> []
    | (Rule r as stmt) :: rest ->
        (* The tail first, so [later] describes it by the time [stmt] asks. *)
        let rest = go rest in
        let sel = Pp.to_string ~minify:true Selector.pp r.selector in
        let shadowed = written sel in
        let kept =
          List.filter
            (fun d ->
              Declaration.is_important d
              || not (Hashtbl.mem shadowed (Declaration.property_key d)))
            r.declarations
        in
        record sel r.declarations;
        if List.compare_lengths kept r.declarations = 0 then stmt :: rest
        else Rule { r with declarations = kept } :: rest
    | stmt :: rest -> stmt :: go rest
  in
  go stmts

(* Canonicalise every cascade context. At-rule block bodies inherit the current
   nesting [parent]; a style rule's nested body is canonicalised under the
   rule's own (expanded) selector as the new parent, so nested relative
   selectors are compared on their effective form. *)
let rec recurse ~(parent : Selector.t option) changed (stmt : statement) :
    statement =
  let here b = canonicalize_block ~parent changed b in
  match stmt with
  | Rule r ->
      let child_parent =
        match parent with
        | None -> r.selector
        | Some p -> Nest.combine p r.selector
      in
      let nested =
        canonicalize_block ~parent:(Some child_parent) changed r.nested
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
  | other -> other

and canonicalize_block ~parent changed (stmts : statement list) : statement list
    =
  (* Canonicalise interiors first so run elements are ranked on their canonical
     serialized form, then undo grouping so equivalent factorings converge. *)
  let stmts = List.map (recurse ~parent changed) stmts in
  let expanded =
    List.concat_map expand_lists stmts |> drop_shadowed_declarations
  in
  if List.compare_lengths expanded stmts <> 0 then changed := true;
  let rec go = function
    | [] -> []
    | stmt :: rest -> (
        match element_rules stmt with
        | Some rules ->
            let rec take acc = function
              | s :: r as l -> (
                  match element_rules s with
                  | Some rs -> take ((s, rs) :: acc) r
                  | None -> (List.rev acc, l))
              | [] -> (List.rev acc, [])
            in
            let run, rest = take [ (stmt, rules) ] rest in
            let canon_body = canonicalize_block ~parent changed in
            settle ~canon_body ?parent changed run @ go rest
        | None -> stmt :: go rest)
  in
  go expanded

(* A custom property is an opaque token stream, so a minifier that treats it as
   text keeps the spacing the author wrote while one that re-serialises a typed
   value drops it: [var(--a, var(--b), var(--c))] against
   [var(--a,var(--b),var(--c))]. The two are the same value, so the projection
   normalises the space after a top-level comma. Text inside quotes is left
   alone. *)
let normalize_custom_value v =
  let len = String.length v in
  let buf = Buffer.create len in
  let rec go i quote =
    if i >= len then ()
    else
      let c = v.[i] in
      match quote with
      | Some q ->
          Buffer.add_char buf c;
          go (i + 1) (if c = q then None else quote)
      | None ->
          if c = '"' || c = '\'' then begin
            Buffer.add_char buf c;
            go (i + 1) (Some c)
          end
          else if c = ',' then begin
            Buffer.add_char buf ',';
            let rec skip j =
              if j < len && v.[j] = ' ' then skip (j + 1) else j
            in
            go (skip (i + 1)) None
          end
          else begin
            Buffer.add_char buf c;
            go (i + 1) None
          end
  in
  go 0 None;
  Buffer.contents buf

(* A font name spells the same family quoted or as the bare ident sequence it
   unquotes to, one word or several (CSS Fonts 4 sec. 2.1.1). A bare generic
   family in the stream is what proves the custom property holds a font stack;
   with that proof either form substitutes identically into [font-family], and
   without it the stream is arbitrary tokens and neither form may move. Emission
   keeps whichever the author wrote, so the projection folds the quoted form
   onto the ident sequence - the same normalisation the structural comparator
   applies through {!Css.declaration_value_for_equivalence}. *)
let normalize_custom_declaration d =
  Declaration.unquote_custom_font_strings
    (Declaration.map_custom_value normalize_custom_value d)

(* The fold reads one declaration, so it holds wherever the declaration sits: a
   [@keyframes] frame and a [@page] box spell a custom property exactly as a
   style rule does, and no block at-rule changes what the token stream means.
   {!Stylesheet.map_declarations} reaches every one of those sites, so the pass
   does not carry a list of the statements it descends through - the list is
   what left [@scope] and [@starting-style] answering differently from
   [@layer]. *)
let normalize_custom_values (stmts : statement list) : statement list =
  Stylesheet.map_declarations
    (Common.List.map_preserve normalize_custom_declaration)
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
let canonical_color_spelling decl =
  let folded = Declaration.normalize ~lossless:true ~exact_srgb:true decl in
  if folded == decl then decl
  else if
    Declaration.equal_declaration folded
      (Declaration.normalize ~lossless:true decl)
  then decl
  else folded

let canonical_color_spellings (stmts : statement list) : statement list =
  Stylesheet.map_declarations
    (Common.List.map_preserve canonical_color_spelling)
    stmts

(* CSS Color 4 sec. 4.4: "a missing component behaves as a zero value, in the
   appropriate unit for that component", for every purpose but the ones that
   combine two colours. A minifier that reaches an achromatic colour by
   conversion writes those channels as [none] where Cascade writes the hex the
   colour folds to, and the projection has to read the two as one colour.

   Sec. 13.3 draws the other edge: interpolation gives a missing component the
   other colour's analogous component rather than a zero, so at a position that
   interpolates the value the two spellings name two different results.
   {!Declaration.normalize} resolves the sentinel only for a colour standing as
   a whole colour-longhand value, which leaves a gradient stop, a [color-mix()]
   operand, a shadow colour and a custom-property token stream reading their
   [none] as written. A declaration the sheet interpolates from elsewhere is
   held back too: [@keyframes] and [@starting-style] are the two blocks that
   exist to be one endpoint of that, so the pass does not descend into them, and
   a colour whose own rule transitions the property it writes keeps its [none].
   That guard reads one rule. A transition one rule declares for a colour
   another rule sets still folds, since seeing it would mean deciding that two
   selectors match one element, which the projection does not do.

   Guarded like [canonical_color_spelling]: the flagged normalisation is kept
   only where it moves the colour, so no other value fold rides along. *)
let canonical_missing_components ~lossless decl =
  let folded = Declaration.normalize ~lossless ~resolve_missing:true decl in
  if folded == decl then decl
  else if
    Declaration.equal_declaration folded (Declaration.normalize ~lossless decl)
  then decl
  else folded

let canonical_missing_components_in_rule ~lossless decls =
  Common.List.map_preserve
    (fun decl ->
      let folded = canonical_missing_components ~lossless decl in
      if folded == decl || not (Shorthand.transitioned_in_rule decls decl) then
        folded
      else decl)
    decls

let rec canonical_missing_component_colors ~lossless (stmts : statement list) :
    statement list =
  Common.List.map_preserve
    (fun stmt ->
      match stmt with
      | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _ | Starting_style _ ->
          stmt
      | stmt ->
          stmt
          |> Stylesheet.map_statement_declarations
               (canonical_missing_components_in_rule ~lossless)
          |> Stylesheet.map_statement_children
               (canonical_missing_component_colors ~lossless))
    stmts

(* The normal optimizer drops this typed alias under its maintained-browser
   policy, but the canonical optimizer runs spec-literally so it does not erase
   other compatibility content. Apply only the alias equivalence promised by
   canonical comparison, at every declaration site. *)
let canonical_vendor_aliases (stmts : statement list) : statement list =
  Stylesheet.map_declarations Shorthand.drop_redundant_decoration_color_aliases
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
    | other -> other
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

(* CSS Cascade 5 sec. 6.4.3: cascade layers are sorted by the order in which
   they first are declared, so all an [@layer a;] statement contributes is the
   position it gives [a] in that order (sec. 6.4.4.2). A name whose removal
   leaves the whole order untouched contributes nothing, and the projection
   drops it, so [@layer a;@layer a{...}] and [@layer a{...}] read as one sheet.
   Emission keeps the statement: it is visible through the CSSOM, and a sheet
   concatenated after this one can bind the position it fixes. *)

(* One position in the layer order. [Named] dedups by layer name, a
   re-declaration naming no new layer. [Opaque] stands for a position whose
   names cannot be read here: an anonymous layer (sec. 6.4.2.1), the layers an
   [@import] carries in with it, or a layer declared inside a conditional group
   rule, which sec. 6.4.3 has contribute to the order only when the condition
   holds. Each carries the index of the statement that raised it, so the two
   orders being weighed line their opaque positions up while no name ever dedups
   into one: a pin whose name is refilled across an unreadable position is left
   alone, which is what makes reading the sheet this coarsely safe. *)
type slot = Named of layer_name | Opaque of int

let equal_slot a b =
  match (a, b) with
  | Named a, Named b -> equal_layer_name a b
  | Opaque a, Opaque b -> Int.equal a b
  | (Named _ | Opaque _), _ -> false

(* Sec. 6.4.2: [a.b] is shorthand for those layers nested in order, so naming a
   sublayer declares each of its parents first. *)
let layer_name_slots (name : layer_name) : slot list =
  let rec loop acc rev_prefix = function
    | [] -> List.rev acc
    | ident :: rest ->
        let rev_prefix = ident :: rev_prefix in
        loop (Named (List.rev rev_prefix) :: acc) rev_prefix rest
  in
  loop [] [] name

let rec declares_layer (stmts : statement list) : bool =
  List.exists
    (fun stmt ->
      match stmt with
      | Layer _ | Layer_decl _ | Import _ -> true
      | stmt -> declares_layer (Stylesheet.statement_children stmt))
    stmts

(* The positions statement [i] adds to the layer order. A named [@layer] block
   or an [@import ... layer(x)] declares its own name, and whatever either holds
   inside is a sublayer sorting within it rather than a position out here. *)
let statement_slots i (stmt : statement) : slot list =
  match stmt with
  | Layer_decl names -> List.concat_map layer_name_slots names
  | Layer (Some name, _) -> layer_name_slots name
  | Import { layer = Some (_ :: _ as name); _ } -> layer_name_slots name
  | Layer (None, _) | Import _ -> [ Opaque i ]
  | stmt ->
      if declares_layer (Stylesheet.statement_children stmt) then [ Opaque i ]
      else []

(* [fixed.(i)] is what statement [i] contributes when it is not a pin, read
   once; [pins.(i)] is the names a pin still carries, which the pruning below
   shortens. A pin left with no name contributes nothing and keeps its index, so
   the opaque positions stay keyed the way [target] saw them. *)
let layer_order ~(fixed : slot list array)
    ~(pins : layer_name list option array) =
  let add (seen, rev) slot =
    match slot with
    | Named name when List.exists (equal_layer_name name) seen -> (seen, rev)
    | Named name -> (name :: seen, slot :: rev)
    | Opaque _ -> (seen, slot :: rev)
  in
  let rec loop i acc =
    if i >= Array.length fixed then List.rev (snd acc)
    else
      let slots =
        match pins.(i) with
        | None -> fixed.(i)
        | Some names -> List.concat_map layer_name_slots names
      in
      loop (i + 1) (List.fold_left add acc slots)
  in
  loop 0 ([], [])

(* Weigh one pin's names left to right, each candidate against the order the
   sheet came in with rather than against the last candidate, so every name kept
   is one the sheet's own order needs. *)
let rec prune_pin ~(fixed : slot list array)
    ~(pins : layer_name list option array) ~target i kept_rev = function
  | [] -> List.rev kept_rev
  | name :: rest ->
      pins.(i) <- Some (List.rev_append kept_rev rest);
      if List.equal equal_slot (layer_order ~fixed ~pins) target then
        prune_pin ~fixed ~pins ~target i kept_rev rest
      else begin
        pins.(i) <- Some (List.rev_append kept_rev (name :: rest));
        prune_pin ~fixed ~pins ~target i (name :: kept_rev) rest
      end

(* Top level only. A pin written inside a [@layer] block names a sublayer of it,
   and the sublayer order runs across every block of that layer rather than
   within one, so a block on its own does not say whether such a pin binds. *)
let fold_layer_pins (stmts : statement list) : statement list =
  let is_pin = function Layer_decl (_ :: _) -> true | _ -> false in
  if not (List.exists is_pin stmts) then stmts
  else
    let work = Array.of_list stmts in
    let pins : layer_name list option array =
      Array.map (function Layer_decl names -> Some names | _ -> None) work
    in
    let fixed =
      Array.mapi
        (fun i stmt ->
          match stmt with Layer_decl _ -> [] | stmt -> statement_slots i stmt)
        work
    in
    let target = layer_order ~fixed ~pins in
    (* One sweep is not enough: a name only reads as needed while a later one it
       is weighed against is still there, and drops out once that one goes. Each
       sweep only removes, so repeating to a fixed point terminates, and every
       candidate is still weighed against the order the sheet came in with. *)
    let sweep () =
      let dropped = ref false in
      Array.iteri
        (fun i names ->
          match names with
          | Some (_ :: _ as names) ->
              let kept = prune_pin ~fixed ~pins ~target i [] names in
              if List.compare_lengths kept names <> 0 then dropped := true
          | Some [] | None -> ())
        pins;
      !dropped
    in
    let rec settle changed = if sweep () then settle true else changed in
    if not (settle false) then stmts
    else
      let rebuild i (stmt : statement) : statement option =
        match (stmt, pins.(i)) with
        | Layer_decl _, Some [] -> None
        | Layer_decl _, Some (_ :: _ as names) -> Some (Layer_decl names)
        | stmt, (None | Some _) -> Some stmt
      in
      List.filter_map Fun.id (List.mapi rebuild stmts)

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
   respellings hold inside it. *)
let canonical_container : Container.t -> Container.t =
  Container.lower_for_minify

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

let canonicalize ?(lossless = false) (stmts : statement list) : statement list =
  let changed = ref false in
  let normalized =
    fold_layer_pins
      (canonical_query_preludes
         (sort_property_runs
            (canonical_missing_component_colors ~lossless
               (canonical_color_spellings
                  (normalize_custom_values (canonical_vendor_aliases stmts))))))
  in
  let result =
    canonicalize_block ~parent:(None : Selector.t option) changed normalized
  in
  if !changed then result else normalized
