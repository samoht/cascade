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
   before [j]. *)
let overlap_indegrees (arr : Declaration.declaration array) : int array =
  let n = Array.length arr in
  let indeg = Array.make n 0 in
  for j = 0 to n - 1 do
    for i = 0 to j - 1 do
      if Shorthand.declarations_overlap arr.(i) arr.(j) then
        indeg.(j) <- indeg.(j) + 1
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
        Array.map
          (fun d -> Pp.to_string ~minify:true Declaration.pp_declaration d)
          arr
      in
      let indeg = overlap_indegrees arr in
      let emitted = Array.make n false in
      let order = Array.make n 0 in
      let changed = ref false in
      for pos = 0 to n - 1 do
        let b = ready_min ~emitted ~indeg ~keys in
        if b <> pos then changed := true;
        emitted.(b) <- true;
        order.(pos) <- b;
        for j = b + 1 to n - 1 do
          if (not emitted.(j)) && Shorthand.declarations_overlap arr.(b) arr.(j)
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
      Rule_graph.of_rules ?parent
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
   accumulated into each surviving element, and which elements remain. *)
type scan = {
  graph : Rule_graph.t;
  arr : (statement * rule list) array;
  members : int list array;
  alive : bool array;
  mutable merged_any : bool;
}

(* Nothing strictly between [lo] and [hi] conflicts with any accumulated
   occurrence in [mems]: moving those writes across the interval is
   unobservable. *)
let interval_clear scan ~lo ~hi mems =
  let node i = Rule_graph.Node_id.of_int_exn i in
  let ok = ref true in
  for m = lo + 1 to hi - 1 do
    if
      List.exists
        (fun a -> Rule_graph.conflict scan.graph (node m) (node a))
        mems
    then ok := false
  done;
  !ok

(* The element a merge produces. Two elements only ever pair on an equal
   [element_key], which is derived from the constructor, so the two sides always
   match here. A block merge concatenates the bodies in source order and
   re-canonicalises the result: the two halves were each canonical alone, but
   their concatenation need not be. *)
let merged_element ~canon_body scan ~from ~into =
  let earlier, later = if from < into then (from, into) else (into, from) in
  match (fst scan.arr.(earlier), fst scan.arr.(later)) with
  | Rule a, Rule b ->
      let merged =
        {
          a with
          declarations =
            canonical_declarations
              (coalesced_declarations (a.declarations @ b.declarations));
        }
      in
      (Rule merged, [ merged ])
  | Media (m, ba), Media (_, bb) ->
      let stmt = Media (m, canon_body (ba @ bb)) in
      (stmt, Option.value ~default:[] (element_rules stmt))
  | Supports (c, ba), Supports (_, bb) ->
      let stmt = Supports (c, canon_body (ba @ bb)) in
      (stmt, Option.value ~default:[] (element_rules stmt))
  | Container (n, c, ba), Container (_, _, bb) ->
      let stmt = Container (n, c, canon_body (ba @ bb)) in
      (stmt, Option.value ~default:[] (element_rules stmt))
  | _ -> assert false

let merge ~canon_body scan changed ~from ~into =
  scan.arr.(into) <- merged_element ~canon_body scan ~from ~into;
  scan.members.(into) <- scan.members.(from) @ scan.members.(into);
  scan.alive.(from) <- false;
  scan.merged_any <- true;
  changed := true

(* Fold the earlier occurrence [i] down into [j] when nothing in between
   observes its writes moving; otherwise fold [j]'s writes up into [i] when
   nothing in between observes those. *)
let try_merge ~canon_body scan changed ~last ~key i j =
  if interval_clear scan ~lo:i ~hi:j scan.members.(i) then begin
    merge ~canon_body scan changed ~from:i ~into:j;
    Hashtbl.replace last key j
  end
  else if interval_clear scan ~lo:i ~hi:j scan.members.(j) then
    merge ~canon_body scan changed ~from:j ~into:i
  else Hashtbl.replace last key j

(* What makes two run elements the same cascade slot, so that one can fold into
   the other. A style rule is keyed by its selector, a conditional block by its
   kind and condition; the [@]-prefix keeps the two namespaces apart. A block
   whose interior is not summarisable never became a run element, so it cannot
   reach here. *)
let element_key (stmt : statement) =
  match stmt with
  | Rule r when r.merge_key = None ->
      Some (Pp.to_string ~minify:true Selector.pp r.selector)
  | Media (m, _) -> Some (String.concat "" [ "@media "; Media.to_string m ])
  | Supports (c, _) ->
      Some (String.concat "" [ "@supports "; Supports.to_string c ])
  | Container (name, cond, _) ->
      Some
        (String.concat ""
           [
             "@container ";
             Option.value ~default:"" name;
             " ";
             (match cond with
             | Some c -> Container.to_string ~minify:true c
             | None -> "");
           ])
  | _ -> None

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
        Rule_graph.of_rules ?parent
          (List.map (fun (stmt, rules) -> element_node stmt rules) run)
      in
      let scan =
        {
          graph;
          arr;
          members = Array.init n (fun i -> [ i ]);
          alive = Array.make n true;
          merged_any = false;
        }
      in
      let last = Hashtbl.create 16 in
      for j = 0 to n - 1 do
        match element_key (fst arr.(j)) with
        | None -> ()
        | Some key -> (
            match Hashtbl.find_opt last key with
            | Some i when scan.alive.(i) ->
                try_merge ~canon_body scan changed ~last ~key i j
            | _ -> Hashtbl.replace last key j)
      done;
      if not scan.merged_any then run
      else Array.to_list arr |> List.filteri (fun i _ -> scan.alive.(i))

(* Sort and coalesce to a fixed point: sorting can empty the interval between
   two same-selector occurrences that a conflicting element previously kept
   apart, enabling a merge the source order hid, so equivalent sheets converge
   regardless of which arrangement they started from. Each merging round removes
   at least one element, so this terminates. *)
let rec settle ~canon_body ?parent changed (run : (statement * rule list) list)
    : statement list =
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

(* A multi-word font name spells the same family quoted or as the bare ident
   sequence it unquotes to (CSS Fonts 4 sec. 15.3), and a custom property
   holding a font stack substitutes either form identically into [font-family].
   Emission keeps whichever the author wrote, so the projection folds the quoted
   form onto the ident sequence - the same normalisation the structural
   comparator applies through {!Css.declaration_value_for_equivalence}. *)
let normalize_custom_declaration d =
  Declaration.unquote_custom_font_strings
    (Declaration.map_custom_value normalize_custom_value d)

let rec normalize_custom_values (stmts : statement list) : statement list =
  List.map
    (fun stmt ->
      match stmt with
      | Rule r ->
          Rule
            {
              r with
              declarations =
                List.map normalize_custom_declaration r.declarations;
              nested = normalize_custom_values r.nested;
            }
      | Layer (n, inner) -> Layer (n, normalize_custom_values inner)
      | Media (c, inner) -> Media (c, normalize_custom_values inner)
      | Supports (c, inner) -> Supports (c, normalize_custom_values inner)
      | other -> other)
    stmts

(* CSS Color 4 sec. 10: [color(srgb r g b)] scales each channel by 255, so
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

let rec canonical_color_spellings (stmts : statement list) : statement list =
  List.map
    (fun stmt ->
      match stmt with
      | Rule r ->
          Rule
            {
              r with
              declarations = List.map canonical_color_spelling r.declarations;
              nested = canonical_color_spellings r.nested;
            }
      | Layer (n, inner) -> Layer (n, canonical_color_spellings inner)
      | Media (c, inner) -> Media (c, canonical_color_spellings inner)
      | Supports (c, inner) -> Supports (c, canonical_color_spellings inner)
      | Container (n, c, inner) ->
          Container (n, c, canonical_color_spellings inner)
      | other -> other)
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

(* Media Queries 4 sec. 2.1: [all] matches every media type, so it is the
   identity in [<media-type> and <condition>] and the Level 3 spelling [not all
   and (X)] is the same query as the Level 4 [not (X)]. Bare [not all] has no
   condition form - it matches nothing - so it stays, and the unnegated [all and
   (X)] never reaches here because {!Optimize} already drops it.

   Projection only. The Level 4 form is eight characters shorter, but a Level 3
   parser rejects a [not] with no media type and an unrecognised query never
   matches, so rewriting one to the other on output would silently drop the
   block in those browsers. *)
let rec canonical_media (query : Media.t) : Media.t =
  match query with
  | Media.Type { prefix = Some Media.Not; type_ = Media.All; trailing = Some c }
    ->
      Media.Cond (Media.Not c)
  | Media.List qs -> Media.List (List.map canonical_media qs)
  | query -> query

let rec canonical_media_queries (stmts : statement list) : statement list =
  List.map
    (fun stmt ->
      match stmt with
      | Rule r -> Rule { r with nested = canonical_media_queries r.nested }
      | Media (q, inner) ->
          Media (canonical_media q, canonical_media_queries inner)
      | Layer (n, inner) -> Layer (n, canonical_media_queries inner)
      | Supports (c, inner) -> Supports (c, canonical_media_queries inner)
      | Container (n, c, inner) ->
          Container (n, c, canonical_media_queries inner)
      | other -> other)
    stmts

let canonicalize (stmts : statement list) : statement list =
  let changed = ref false in
  let normalized =
    canonical_media_queries
      (sort_property_runs
         (canonical_color_spellings (normalize_custom_values stmts)))
  in
  let result =
    canonicalize_block ~parent:(None : Selector.t option) changed normalized
  in
  if !changed then result else normalized
