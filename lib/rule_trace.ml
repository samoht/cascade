(** The canonical form of a run of style rules and conditional blocks, read as
    the trace of its declarations. *)

open Stylesheet

let declaration_text d = Pp.to_string ~minify:true Declaration.pp d
let selector_text sel = Pp.to_string ~minify:true Selector.pp sel

(* A condition a block puts its rules under, as {!excludes} reads it. *)
type condition =
  | Media_condition of Media.t
  | Supports_condition of Supports.t
  | Container_condition of string option * Container.t

(* Media Queries 4 sec. 3.4: [not] negates the query it prefixes, all of it.
   [only] is not a negation and a query list has no single one. *)
let negated_media : Media.t -> Media.t option = function
  | Cond (Not c) -> Some (Cond c)
  | Cond c -> Some (Cond (Not c))
  | Type ({ prefix = None; _ } as q) ->
      Some (Type { q with prefix = Some Media.Not })
  | Type ({ prefix = Some Media.Not; _ } as q) ->
      Some (Type { q with prefix = None })
  | Type { prefix = Some Media.Only; _ } | List _ -> None

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
    match negate y with Some ny -> equal x ny | None -> false
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

type block_prelude =
  | Media_query of Media.t
  | Feature_query of Supports.t
  | Container_query of string option * Container.t option

(* The prelude and body of a conditional block, the one kind of at-rule a run
   holds. *)
let block_of (stmt : statement) =
  match stmt with
  | Media (m, body) -> Some (Media_query m, body)
  | Supports (c, body) -> Some (Feature_query c, body)
  | Container (name, cond, body) -> Some (Container_query (name, cond), body)
  | Rule _ | Declarations _ | Bang_comment _ | Charset _ | Import _
  | Namespace _ | Property _ | Layer_decl _ | Layer _ | Moz_document _
  | Starting_style _ | When _ | Else _ | Supports_condition _ | Origin _
  | Scope _ | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _ | Font_face _
  | Counter_style _ | Page _ | Page_with_margins _ | Font_palette_values _
  | Font_feature_values _ | View_transition _ | Position_try _ | Viewport _
  | Unknown_at_rule _ ->
      None

(* The condition a block prelude puts its rules under, a container's name read
   out of its query. A container query without a condition has none. *)
let prelude_condition = function
  | Media_query q -> Some (Media_condition q)
  | Feature_query c -> Some (Supports_condition c)
  | Container_query (None, Some (Container.Named (name, c))) ->
      Some (Container_condition (Some name, c))
  | Container_query (name, Some c) -> Some (Container_condition (name, c))
  | Container_query (_, None) -> None

let block_statement prelude body =
  match prelude with
  | Media_query m -> Media (m, body)
  | Feature_query c -> Supports (c, body)
  | Container_query (name, cond) -> Container (name, cond, body)

(* What makes two block preludes one condition. Media and supports preludes are
   keyed by their text, while a container condition uses the structural equality
   that decides whether two queries select the same containers: its normalised
   spelling is only a hash, and collisions still reach [equal]. The spelling is
   also where the prelude sorts. *)
module Prelude_key = struct
  type shape =
    | Text of string
    | Container of string option * Container.t option

  type t = { shape : shape; text : string }

  let v prelude =
    match prelude with
    | Media_query m ->
        let text = String.concat "" [ "@media "; Media.to_string m ] in
        { shape = Text text; text }
    | Feature_query c ->
        let text = String.concat "" [ "@supports "; Supports.to_string c ] in
        { shape = Text text; text }
    | Container_query (name, cond) ->
        let cond_text =
          Option.fold ~none:""
            ~some:(fun c ->
              Container.to_string ~minify:true (Container.normalize c))
            cond
        in
        {
          shape = Container (name, cond);
          text =
            String.concat " "
              [ "@container"; Option.value ~default:"" name; cond_text ];
        }

  let equal a b =
    String.equal a.text b.text
    &&
    match (a.shape, b.shape) with
    | Text a, Text b -> String.equal a b
    | Container (an, ac), Container (bn, bc) ->
        Option.equal String.equal an bn && Option.equal Container.equal ac bc
    | (Text _ | Container _), _ -> false
end

(* ===== The entries of a run ===== *)

(* A run reads as a sequence of entries: one per declaration, each under the
   selector and the block conditions of the rule that holds it. Two entries
   depend on each other ({!depends}) when swapping them could change a value
   some element computes, and two that do not may swap wherever they stand side
   by side. So a run is one trace: the partial order its dependencies put on its
   entries, the same for every sequence of the same entries that swaps only
   independent neighbours. The canonical form is one linear extension of that
   order, chosen by {!emit} from the order and the entries' text alone, so two
   runs with one order print one form whatever sequence wrote them. *)

module Int_set = Set.Make (Int)
module Int_table = Hashtbl.Make (Int)

module Property_table = Hashtbl.Make (struct
  type t = Declaration.prop_key

  let equal = Declaration.equal_prop_key
  let hash = Declaration.hash_prop_key
end)

(* A run's (conditions, selector) pair. Every entry of the run sits under one,
   and the canonical form writes each as one or more rules. [name] is where it
   sorts: the block preludes it sits under, then its selector. *)
type group = {
  chain : (block_prelude * Prelude_key.t) array;
  conditions : condition list;
  template : rule;
  name : string;
  selector_key : string;  (** the selector and the merge key *)
  branches : Rule_graph.branches;
}

(* The slots an entry still writes once the later writes of its own group are
   read: a footprint the model spells, or a broad one less the slots
   rewritten. *)
type footprint =
  | Spelled of Shorthand.overlap_key list
  | Broad_except of Shorthand.overlap_key list

type entry = {
  group : int;
  decl : Declaration.declaration;
  keys : Shorthand.overlap_key list;
  text : string;
  text_id : int;  (** the same for two entries of one text *)
  value : string Lazy.t;
  property : int;  (** the id of the property *)
  unknown : bool;  (** the model does not type the property *)
  special : bool;  (** a custom property or [all] *)
  broad : bool;  (** the footprint meets any key *)
  important : bool;
  mutable live : footprint option;  (** [None] when no slot was rewritten *)
  mutable dead : bool;
}

let rec unguarded = function
  | Declaration.Theme_guarded { decl; _ } -> unguarded decl
  | d -> d

let is_unknown d =
  match unguarded d with
  | Declaration.Declaration { property = Unknown_property _; _ } -> true
  | _ -> false

let group_name chain selector =
  let b = Buffer.create 64 in
  Array.iter
    (fun (_, (key : Prelude_key.t)) ->
      Buffer.add_string b key.text;
      Buffer.add_char b '{')
    chain;
  Buffer.add_string b (selector_text selector);
  Buffer.contents b

let same_group (g : group) chain (r : rule) name =
  String.equal g.name name
  && Array.length g.chain = Array.length chain
  && Array.for_all2 (fun (_, a) (_, b) -> Prelude_key.equal a b) g.chain chain
  && Option.equal String.equal g.template.merge_key r.merge_key

(* Every rule an element holds, with the block preludes it sits under, outermost
   first. *)
let rec element_chains acc stmt f =
  match stmt with
  | Rule r -> f (Array.of_list (List.rev acc)) r
  | _ -> (
      match block_of stmt with
      | Some (prelude, body) ->
          let acc = (prelude, Prelude_key.v prelude) :: acc in
          List.iter (fun stmt -> element_chains acc stmt f) body
      | None -> ())

(* What reading a run accumulates: its groups, by name, and its entries, newest
   first; [ids] numbers texts and property names. *)
type reader = {
  parent : Selector.t option;
  by_name : (string, group * int) Hashtbl.t;
  mutable groups : group list;
  mutable group_count : int;
  ids : (string, int) Hashtbl.t;
  properties : int Property_table.t;
  mutable entries : entry list;
}

(* A declaration the typed reader refused still names the property it spells, so
   it shares that property's id. *)
let property_id r decl =
  let key =
    match Declaration.property_key decl with
    | Declaration.Key (Unknown_property { own = Some (Prop own); _ }) ->
        Declaration.Key own
    | key -> key
  in
  match Property_table.find_opt r.properties key with
  | Some id -> id
  | None ->
      let id = Property_table.length r.properties in
      Property_table.replace r.properties key id;
      id

let text_id r text =
  match Hashtbl.find_opt r.ids text with
  | Some id -> id
  | None ->
      let id = Hashtbl.length r.ids in
      Hashtbl.replace r.ids text id;
      id

let group_of r chain (rule : rule) =
  let name = group_name chain rule.selector in
  match
    List.find_opt
      (fun (g, _) -> same_group g chain rule name)
      (Hashtbl.find_all r.by_name name)
  with
  | Some (_, id) -> id
  | None ->
      let g =
        {
          chain;
          conditions =
            List.filter_map
              (fun (prelude, _) -> prelude_condition prelude)
              (Array.to_list chain);
          template = rule;
          name;
          selector_key =
            String.concat "\n"
              [
                selector_text rule.selector;
                Option.value ~default:"" rule.merge_key;
              ];
          branches = Rule_graph.branches_under ?parent:r.parent rule.selector;
        }
      in
      let id = r.group_count in
      r.group_count <- id + 1;
      r.groups <- g :: r.groups;
      Hashtbl.add r.by_name name (g, id);
      id

let entry r ~group decl =
  let text = declaration_text decl in
  let keys = Shorthand.declaration_overlap_keys decl in
  {
    group;
    decl;
    keys;
    text;
    text_id = text_id r text;
    value = lazy (Declaration.string_of_value ~minify:true decl);
    property = property_id r decl;
    unknown = is_unknown decl;
    special =
      Option.is_some (Shorthand.custom_property_name decl)
      || Shorthand.is_all_declaration (unguarded decl);
    broad = Shorthand.overlap_keys_broad keys;
    important = Declaration.is_important decl;
    live = None;
    dead = false;
  }

let read_rule r chain (rule : rule) =
  let group = group_of r chain rule in
  List.iter
    (fun decl -> r.entries <- entry r ~group decl :: r.entries)
    rule.declarations

(* ===== Rewritten slots ===== *)

let rec keys_minus a b =
  match (a, b) with
  | [], _ -> []
  | _, [] -> a
  | x :: xs, y :: ys ->
      let c = Shorthand.overlap_key_compare x y in
      if c = 0 then keys_minus xs ys
      else if c < 0 then x :: keys_minus xs b
      else keys_minus a ys

let rec keys_within a b =
  match (a, b) with
  | [], _ -> true
  | _ :: _, [] -> false
  | x :: xs, y :: ys ->
      let c = Shorthand.overlap_key_compare x y in
      if c = 0 then keys_within xs ys else c > 0 && keys_within a ys

(* An entry whose footprint can lose the slots a later write rewrites: one the
   overlap test reads through its keys alone, not a property whose duplicates
   the dedup keeps as fallbacks, and not a vendor-prefixed one, which an engine
   may read without reading the write after it. *)
let narrowable d =
  (not (Shorthand.is_intentionally_duplicated d))
  &&
  match d with
  | Declaration.Theme_guarded _ -> false
  | Declaration.Declaration { property; _ }
    when Properties.is_vendor_prefixed property ->
      false
  | Declaration.Declaration { property = Custom_property _; _ } -> false
  | Declaration.Declaration { property = Unknown_property _; _ } ->
      Shorthand.declaration_is_broad d
  | Declaration.Declaration _ -> true

(* Whether [d] is gone once [deduplicate_declarations_with] has read [first]
   then [second], one of which it is. *)
let deduplicated ~ctx d first second =
  not
    (List.memq d
       (Shorthand.deduplicate_declarations_with ~ctx ~merge_box:false
          [ first; second ]))

(* Whether [d] writes a vendor's own property, which an engine may read without
   reading a later write of the same slots under another name. *)
let vendor_prefixed d =
  match unguarded d with
  | Declaration.Declaration { property; _ } ->
      Properties.is_vendor_prefixed property
  | Declaration.Theme_guarded _ -> false

(* Whether [y] at position [q] of [x]'s group overrides [x] at [p]: it comes
   later at [x]'s importance, or it is important and [x] is not. The group has
   one selector and one set of conditions, so [y] applies to every element [x]
   does, at the same specificity, and CSS Cascade 5 sec. 6.1 ranks importance
   before order of appearance. *)
let overrides x p y q =
  q <> p
  && ((y.important && not x.important)
     || (Bool.equal y.important x.important && q > p))

(* What the writes of [x]'s group that override it leave of [x]. A later
   identical write ends it. Another of its own property ends it where the
   declaration dedup, reading the two in the order they stand, drops it: that
   keeps a fallback pair, which a browser reads alike within one rule and across
   two, so where the group's rules begin and end decides nothing. A later write
   of the same slots under a property every engine reading [x] reads, as
   [transform] is for [-webkit-transform], ends it where the dedup drops [x]
   before it. Any other that meets it rewrites the slots it names in every
   engine ({!Shorthand.declaration_slot_keys}), and [x] keeps the rest as its
   footprint, or is dead once none is left. *)
let kill_entry ~ctx entries members p =
  let x = entries.(members.(p)) in
  let rewritten = ref [] in
  Array.iteri
    (fun q yi ->
      let y = entries.(yi) in
      if (not x.dead) && (not y.dead) && overrides x p y q then
        if Declaration.same_property x.decl y.decl then begin
          let first, second = if q > p then (x, y) else (y, x) in
          if
            (q > p && Int.equal x.text_id y.text_id)
            || deduplicated ~ctx x.decl first.decl second.decl
          then x.dead <- true
        end
        else if
          q > p
          && List.equal Shorthand.overlap_key_equal x.keys y.keys
          && (not (vendor_prefixed y.decl))
          && deduplicated ~ctx x.decl x.decl y.decl
        then x.dead <- true
        else if Shorthand.overlap_keys_intersect x.keys y.keys then
          rewritten :=
            List.rev_append
              (Shorthand.declaration_slot_keys
                 ~enforce_spec:(Ctx.enforce_spec ctx) y.decl)
              !rewritten)
    members;
  match !rewritten with
  | [] -> ()
  | _ when x.dead || not (narrowable x.decl) -> ()
  | rewritten -> (
      let rewritten = List.sort_uniq Shorthand.overlap_key_compare rewritten in
      if x.broad then x.live <- Some (Broad_except rewritten)
      else
        match keys_minus x.keys rewritten with
        | [] -> x.dead <- true
        | rest when List.compare_lengths rest x.keys = 0 -> ()
        | rest -> x.live <- Some (Spelled rest))

(* Both facts {!kill_entry} reads hold in every sequence of the trace: [x] and a
   write that overrides it at its own importance depend on each other
   ({!depends}), so the write stays after it. {!drop_repeats} never drops the
   write a slot was read off for ({!supports}). *)
let kill ~ctx entries members =
  Array.iteri (fun p _ -> kill_entry ~ctx entries members p) members

(* ===== Dependencies ===== *)

let footprint e =
  match e.live with
  | Some f -> f
  | None -> if e.broad then Broad_except [] else Spelled e.keys

(* Whether two entries the overlap test pairs still meet once each has lost the
   slots its own group rewrites. *)
let live_meet x y =
  match (x.live, y.live) with
  | None, None -> true
  | _ -> (
      match (footprint x, footprint y) with
      | Spelled a, Spelled b -> Shorthand.overlap_keys_intersect a b
      | Spelled a, Broad_except k | Broad_except k, Spelled a ->
          not (keys_within a k)
      | Broad_except _, Broad_except _ -> true)

(* What the dependency test reads: the run's groups and entries, the answers it
   last gave for pairs of groups, in a table indexed by a hash of the pair, and
   once the drops are done the twins ({!twin}). *)
(* The last live entry of each text one group holds: [texts] sorted, and
   [last.(k)] the last entry writing [texts.(k)]. *)
type lasts = { texts : int array; last : int array }

type order = {
  groups : group array;
  entries : entry array;
  met_pairs : int array;
  met : bool array;
  mutable twins : lasts array;
      (** by group, the last live entry of each text ({!twin}) *)
  unknown_meet : bool Int_table.t;  (** by pair of untyped property names *)
}

(* Slots for the answers about pairs of [n] things: room for each to pair with a
   few others, a power of two, and no more than 4096. *)
let pair_slots n =
  let rec grow slots =
    if slots >= 4 * n || slots >= 4096 then slots else grow (2 * slots)
  in
  grow 16

(* [ask pairs answers n a b f] is [f a b] for the unordered pair [a], [b] of
   numbers below [n], read back from [answers] when the slot the pair hashes to
   last held it. *)
let ask pairs answers n a b f =
  let a, b = if a < b then (a, b) else (b, a) in
  let pair = (a * n) + b in
  let slot = Hashtbl.hash pair land (Array.length pairs - 1) in
  if Int.equal pairs.(slot) pair then answers.(slot)
  else begin
    let answer = f a b in
    pairs.(slot) <- pair;
    answers.(slot) <- answer;
    answer
  end

(* Whether an element can sit under both groups and match both selectors at one
   specificity: the selector half of {!Rule_graph.rules_conflict}, with no
   conflict between a condition and its negation. *)
let groups_meet o a b =
  Int.equal a b
  || ask o.met_pairs o.met (Array.length o.groups) a b (fun a b ->
      let ga = o.groups.(a) and gb = o.groups.(b) in
      (not
         (List.exists
            (fun c -> List.exists (excludes c) gb.conditions)
            ga.conditions))
      && Rule_graph.branches_conflict ga.branches gb.branches)

(* Whether a live entry of [x]'s group after [x] writes [y]'s text, or one of
   [y]'s group after [y] writes [x]'s: the later one then writes every slot [y]
   (or [x]) writes, so wherever [x] and [y] stand side by side it comes after
   both. On an element both apply to, an engine that reads that text takes the
   later write over both, at their importance and, being of the group, at the
   specificity of the one it shares a group with; an engine that does not read
   it drops [y] (or [x]) too, and the other stands alone. Either way the two may
   swap. The later write meets the entry of its group, so the group keeps it
   after that one in every sequence of the trace. *)
(* Whether [lasts] holds text [id] at an entry after [k], between [lo] and
   [hi]. *)
let rec last_after lasts id k lo hi =
  lo < hi
  &&
  let mid = (lo + hi) / 2 in
  let c = Int.compare lasts.texts.(mid) id in
  if c = 0 then lasts.last.(mid) > k
  else if c < 0 then last_after lasts id k (mid + 1) hi
  else last_after lasts id k lo mid

let later twins o k other =
  let lasts = twins.(o.entries.(k).group) in
  last_after lasts o.entries.(other).text_id k 0 (Array.length lasts.texts)

let twin twins o i j = later twins o i j || later twins o j i

(* The last live entry of each text in each group. [members] lists a group's
   entries in run order. *)
let twins entries members =
  Array.map
    (fun m ->
      let live =
        List.filter (fun i -> not entries.(i).dead) (Array.to_list m)
      in
      let by_text =
        List.stable_sort
          (fun a b -> Int.compare entries.(a).text_id entries.(b).text_id)
          live
      in
      let rec lasts = function
        | a :: (b :: _ as rest)
          when Int.equal entries.(a).text_id entries.(b).text_id ->
            lasts rest
        | a :: rest -> a :: lasts rest
        | [] -> []
      in
      let picked = Array.of_list (lasts by_text) in
      { texts = Array.map (fun i -> entries.(i).text_id) picked; last = picked })
    members

(* [x] and [y] write one value to the same slots, as [-moz-transform] and
   [-webkit-transform] can, so either order ends at it in an engine that reads
   both, and one that reads one of them only ever sees that one. *)
let same_write x y =
  (not (Declaration.same_property x.decl y.decl))
  && List.equal Shorthand.overlap_key_equal x.keys y.keys
  && String.equal (Lazy.force x.value) (Lazy.force y.value)

(* {!Shorthand.declarations_overlap_with_keys}, which reads an untyped name
   afresh on every call, asked once per pair of untyped names. Against a typed
   property an untyped one meets what its keys meet, since a name the model
   cannot place carries the broad key. *)
let overlap o x y =
  if x.unknown && y.unknown then begin
    let a, b =
      if x.property < y.property then (x.property, y.property)
      else (y.property, x.property)
    in
    let pair = (a lsl 31) lor b in
    match Int_table.find_opt o.unknown_meet pair with
    | Some meet -> meet
    | None ->
        let meet =
          Shorthand.declarations_overlap_with_keys x.decl x.keys y.decl y.keys
        in
        Int_table.replace o.unknown_meet pair meet;
        meet
  end
  else if (x.unknown || y.unknown) && not (x.special || y.special) then
    Shorthand.overlap_keys_intersect x.keys y.keys
  else Shorthand.declarations_overlap_with_keys x.decl x.keys y.decl y.keys

(* {!Shorthand.declarations_conflict_with_keys} for two overlapping entries of
   different texts. An untyped declaration covers only its own property, so no
   other implies it. *)
let differ x y =
  x.unknown || y.unknown
  || Shorthand.declarations_conflict_with_keys x.decl x.keys y.decl y.keys

let settled_meet o i j x y = live_meet x y && not (twin o.twins o i j)

let crosses o i j x y =
  (not (Int.equal x.text_id y.text_id))
  && overlap o x y
  && groups_meet o x.group y.group
  && settled_meet o i j x y && differ x y
  && not (same_write x y)

(* Whether entries [i] and [j] of the run may not swap. Two entries of one group
   keep their order wherever they meet, since one may override the other: that
   keeps every override {!kill} read true in every sequence of the trace. Two of
   different groups keep it where they write a common slot differently, at one
   importance, on an element both can match at one specificity. Two identical
   declarations and two spellings of one write never decide each other.

   Two more facts free a pair, each read off a third entry of one's group: a
   slot a later write of the group rewrites is no longer one the entry writes
   ({!kill}), and a later twin of either in the other's group writes over both
   ({!twin}). A drop never removes that third entry ({!supports}), so the facts
   hold for the run with or without what {!drop_repeats} drops. *)
let depends o i j =
  let x = o.entries.(i) and y = o.entries.(j) in
  Bool.equal x.important y.important
  && if Int.equal x.group y.group then overlap o x y else crosses o i j x y

(* ===== The order's edges ===== *)

(* Where an entry is filed so that every entry it may depend on is found: by
   overlap key, specificity and importance. A broad footprint meets any key and
   an untyped name may meet another by name alone, so those are filed whole as
   well. *)
type bucket_kind = Key of Shorthand.overlap_key | Broad | Every | Unknown

module Bucket = Hashtbl.Make (struct
  type t = bucket_kind * int * bool

  let equal (ka, sa, ia) (kb, sb, ib) =
    Int.equal sa sb && Bool.equal ia ib
    &&
    match (ka, kb) with
    | Key a, Key b -> Shorthand.overlap_key_equal a b
    | Broad, Broad | Every, Every | Unknown, Unknown -> true
    | (Key _ | Broad | Every | Unknown), _ -> false

  let hash (k, s, i) =
    let k =
      match k with
      | Key key -> Shorthand.overlap_key_hash key
      | Broad -> 1
      | Every -> 2
      | Unknown -> 3
    in
    Hashtbl.hash (k, s, i)
end)

type positions = { mutable items : int array; mutable size : int }

let push bucket i =
  if bucket.size = Array.length bucket.items then begin
    let grown = Array.make (max 8 (2 * bucket.size)) 0 in
    Array.blit bucket.items 0 grown 0 bucket.size;
    bucket.items <- grown
  end;
  bucket.items.(bucket.size) <- i;
  bucket.size <- bucket.size + 1

let specificity_code ({ ids; classes; elements } : Selector.specificity) =
  (ids lsl 40) lor (classes lsl 20) lor elements

let bucket buckets key =
  match Bucket.find_opt buckets key with
  | Some b -> b
  | None ->
      let b = { items = [||]; size = 0 } in
      Bucket.replace buckets key b;
      b

(* The buckets entry [e] reads its candidates from, and the ones it is filed
   in. *)
let buckets_of o buckets e =
  let read = ref [] and filed = ref [] in
  let specs =
    List.sort_uniq Int.compare
      (List.map specificity_code
         (Rule_graph.branch_specificities o.groups.(e.group).branches))
  in
  List.iter
    (fun spec ->
      let at kind = bucket buckets (kind, spec, e.important) in
      let keys = List.map (fun k -> at (Key k)) e.keys in
      if e.broad then begin
        read := at Every :: !read;
        filed := at Broad :: !filed
      end
      else begin
        read := at Broad :: List.rev_append keys !read;
        filed := List.rev_append keys !filed
      end;
      filed := at Every :: !filed;
      if e.unknown then begin
        read := at Unknown :: !read;
        filed := at Unknown :: !filed
      end)
    specs;
  (Array.of_list !read, !filed)

(* The entries of [read] not yet seen by [j], nearest first, merged across the
   buckets. *)
let rec next_candidate read cursor stamp j =
  let best = ref (-1) and at = ref (-1) in
  for k = 0 to Array.length read - 1 do
    let c = cursor.(k) in
    if c >= 0 && read.(k).items.(c) > !at then begin
      best := k;
      at := read.(k).items.(c)
    end
  done;
  if !best < 0 then -1
  else begin
    cursor.(!best) <- cursor.(!best) - 1;
    if stamp.(!at) = j then next_candidate read cursor stamp j
    else begin
      stamp.(!at) <- j;
      !at
    end
  end

(* The edges of the order, one per dependent pair unless a path already holds
   it: each entry reads the earlier entries filed where it would be, nearest
   first, and skips one that already reaches it through an entry it linked to.
   Only reachability reaches the output: {!emit} and {!between} read whether a
   path joins two entries, never which edges carry it.

   Up to [closure_limit] entries, each entry holds the set of its ancestors, so
   the skip is a bit test and the walk asks {!depends} once per candidate that
   is not already an ancestor. A dense run, where most writers of a slot meet,
   then costs a test per edge kept rather than several per candidate. Past the
   limit the sets would not fit, and a candidate is skipped when it depends on
   one of the last few entries linked, which then reaches it. *)
let closure_limit = 16384
let covered_limit = 8

let rec covered o i k = function
  | [] -> false
  | _ when k = 0 -> false
  | l :: rest -> depends o i l || covered o i (k - 1) rest

let order_graph o =
  let n = Array.length o.entries in
  let succ = Array.make n [] and preds = Array.make n 0 in
  let buckets = Bucket.create 64 and stamp = Array.make n (-1) in
  let ancestors =
    if n <= closure_limit then Array.make n Bytes.empty else [||]
  in
  Array.iteri
    (fun j e ->
      if not e.dead then begin
        let read, filed = buckets_of o buckets e in
        let cursor = Array.map (fun b -> b.size - 1) read in
        let link i =
          succ.(i) <- j :: succ.(i);
          preds.(j) <- preds.(j) + 1
        in
        (if Array.length ancestors > 0 then begin
           let reach = Reach.v n in
           ancestors.(j) <- reach;
           let rec walk () =
             let i = next_candidate read cursor stamp j in
             if i >= 0 then begin
               if (not (Reach.mem reach i)) && depends o i j then begin
                 link i;
                 Reach.union ~into:reach ancestors.(i);
                 Reach.add reach i
               end;
               walk ()
             end
           in
           walk ()
         end
         else
           let rec walk linked =
             let i = next_candidate read cursor stamp j in
             if i >= 0 then
               if (not (covered o i covered_limit linked)) && depends o i j then begin
                 link i;
                 walk (i :: linked)
               end
               else walk linked
           in
           walk []);
        List.iter (fun b -> push b j) filed
      end)
    o.entries;
  (succ, preds)

(* ===== The canonical linear extension ===== *)

(* The state of {!emit}: for each entry, its predecessors not yet taken, those
   outside its group, and whether a chain of edges leads from it out of its
   group; for each group, its entries left, those still waiting on another
   group, and its ready entries, all of them and those leading out, by text
   rank. [free] and [leading] are the groups by name rank that have every entry
   left free of other groups, and that have a ready entry leading out. *)
type emission = {
  o : order;
  succ : int list array;
  preds : int array;
  outside : int array;
  leads : bool array;
  by_text : int array;
  text_rank : int array;
  by_name : int array;
  name_rank : int array;
  remaining : int array;
  waiting : int array;
  ready : Int_set.t array;
  ready_leading : Int_set.t array;
  mutable free : Int_set.t;
  mutable leading : Int_set.t;
}

let ranked n compare =
  let order = Array.init n Fun.id in
  Array.sort
    (fun a b -> match compare a b with 0 -> Int.compare a b | c -> c)
    order;
  let rank = Array.make n 0 in
  Array.iteri (fun r i -> rank.(i) <- r) order;
  (order, rank)

let group_of_entry s i = s.o.entries.(i).group

let mark_ready s i =
  let g = group_of_entry s i in
  s.ready.(g) <- Int_set.add s.text_rank.(i) s.ready.(g);
  if s.leads.(i) then begin
    if Int_set.is_empty s.ready_leading.(g) then
      s.leading <- Int_set.add s.name_rank.(g) s.leading;
    s.ready_leading.(g) <- Int_set.add s.text_rank.(i) s.ready_leading.(g)
  end

let start o (succ, preds) =
  let n = Array.length o.entries and ng = Array.length o.groups in
  let group i = o.entries.(i).group in
  let by_text, text_rank =
    ranked n (fun a b -> String.compare o.entries.(a).text o.entries.(b).text)
  in
  let by_name, name_rank =
    ranked ng (fun a b -> String.compare o.groups.(a).name o.groups.(b).name)
  in
  let leads = Array.make n false and outside = Array.make n 0 in
  for i = n - 1 downto 0 do
    List.iter
      (fun s ->
        if group s <> group i then outside.(s) <- outside.(s) + 1;
        if group s <> group i || leads.(s) then leads.(i) <- true)
      succ.(i)
  done;
  let s =
    {
      o;
      succ;
      preds;
      outside;
      leads;
      by_text;
      text_rank;
      by_name;
      name_rank;
      remaining = Array.make ng 0;
      waiting = Array.make ng 0;
      ready = Array.make ng Int_set.empty;
      ready_leading = Array.make ng Int_set.empty;
      free = Int_set.empty;
      leading = Int_set.empty;
    }
  in
  Array.iteri
    (fun i e ->
      if not e.dead then begin
        s.remaining.(e.group) <- s.remaining.(e.group) + 1;
        if outside.(i) > 0 then s.waiting.(e.group) <- s.waiting.(e.group) + 1;
        if preds.(i) = 0 then mark_ready s i
      end)
    o.entries;
  Array.iteri
    (fun g left ->
      if left > 0 && s.waiting.(g) = 0 then
        s.free <- Int_set.add s.name_rank.(g) s.free)
    s.remaining;
  s

let release s g j =
  let h = group_of_entry s j in
  s.preds.(j) <- s.preds.(j) - 1;
  if h <> g then begin
    s.outside.(j) <- s.outside.(j) - 1;
    if s.outside.(j) = 0 then begin
      s.waiting.(h) <- s.waiting.(h) - 1;
      if s.waiting.(h) = 0 then s.free <- Int_set.add s.name_rank.(h) s.free
    end
  end;
  if s.preds.(j) = 0 then mark_ready s j

let take s i =
  let g = group_of_entry s i in
  s.remaining.(g) <- s.remaining.(g) - 1;
  if s.remaining.(g) = 0 then s.free <- Int_set.remove s.name_rank.(g) s.free;
  s.ready.(g) <- Int_set.remove s.text_rank.(i) s.ready.(g);
  if s.leads.(i) then begin
    s.ready_leading.(g) <- Int_set.remove s.text_rank.(i) s.ready_leading.(g);
    if Int_set.is_empty s.ready_leading.(g) then
      s.leading <- Int_set.remove s.name_rank.(g) s.leading
  end;
  List.iter (release s g) s.succ.(i)

(* Take the ready entries of group [g] that [from] holds, in text order, as long
   as it holds one. *)
let piece s g from =
  let rec go acc =
    match Int_set.min_elt_opt from.(g) with
    | None -> List.rev acc
    | Some r ->
        let i = s.by_text.(r) in
        take s i;
        go (s.o.entries.(i).decl :: acc)
  in
  go []

(* The entries in canonical order, cut into the runs of one group each that the
   canonical form writes as one rule.

   The choice reads the order and the entries' text alone. A group none of whose
   entries left waits on another group comes next, the first by name, all of it,
   its entries in text order as their own order allows. When no group is free,
   the first by name with a ready entry leading out of the group gives every
   such entry it can, and an entry nothing outside its group waits on stays for
   the group's later rule. One such group exists whenever an entry is left: a
   group waits on an entry that some ready entry reaches, and that ready entry
   leads out of its own group. *)
let emit o graph =
  let s = start o graph in
  let rec go pieces =
    let next =
      match Int_set.min_elt_opt s.free with
      | Some r -> Some (s.by_name.(r), s.ready)
      | None ->
          Option.map
            (fun r -> (s.by_name.(r), s.ready_leading))
            (Int_set.min_elt_opt s.leading)
    in
    match next with
    | None ->
        if Array.exists (fun left -> left > 0) s.remaining then
          invalid_arg "Rule_trace.emit: an entry left with none to take"
        else List.rev pieces
    | Some (g, from) -> (
        let decls = piece s g from in
        match pieces with
        | (h, before) :: rest when Int.equal h g ->
            go ((g, before @ decls) :: rest)
        | _ -> go ((g, decls) :: pieces))
  in
  go []

(* The statements of the canonical form: one rule per piece, and the consecutive
   pieces under one block prelude written in one block of it. *)
let rec statements_of o depth = function
  | [] -> []
  | (g, declarations) :: rest when Array.length o.groups.(g).chain = depth ->
      Rule { o.groups.(g).template with declarations }
      :: statements_of o depth rest
  | (g, _) :: _ as pieces ->
      let prelude, key = o.groups.(g).chain.(depth) in
      let under (h, _) =
        let chain = o.groups.(h).chain in
        Array.length chain > depth && Prelude_key.equal (snd chain.(depth)) key
      in
      let rec split acc = function
        | p :: rest when under p -> split (p :: acc) rest
        | rest -> (List.rev acc, rest)
      in
      let inside, rest = split [] pieces in
      block_statement prelude (statements_of o (depth + 1) inside)
      :: statements_of o depth rest

(* ===== Repeats ===== *)

(* The entries after [x] and before [y] in the order, by a walk from [x] that
   stops at [y]'s position, edges only going forward. [seen] is all [false]
   before and after. *)
let descendants succ seen x y =
  let visited = ref [] in
  let rec walk = function
    | [] -> ()
    | i :: rest ->
        if i >= y || seen.(i) then walk rest
        else begin
          seen.(i) <- true;
          visited := i :: !visited;
          walk (List.rev_append succ.(i) rest)
        end
  in
  walk (List.filter (fun s -> not (Int.equal s y)) succ.(x));
  List.iter (fun i -> seen.(i) <- false) !visited;
  !visited

(* Whether an entry [counts] stands after [x] and before [y] in the order. *)
let between ~counts succ seen x y =
  let leads_to_y i = List.exists (Int.equal y) succ.(i) in
  List.exists
    (fun z ->
      counts z
      && (leads_to_y z || List.exists leads_to_y (descendants succ seen z y)))
    (descendants succ seen x y)

(* Whether [y] is a twin of [x] the dedup drops after [before], as
   [-webkit-transform:none] after [transform:none]: one write of the same slots
   and the same value under another name, which names no slot of its own
   ({!Shorthand.declaration_slot_keys}). Every engine the dedup judges for that
   reads [y] reads it as the write [x] makes. [before] is [x] and what stands
   between the two in [y]'s group, read as one rule. *)
let dedup_twin ~ctx ex ey before =
  List.equal Shorthand.overlap_key_equal ex.keys ey.keys
  && (not (Int.equal ex.property ey.property))
  && String.equal (Lazy.force ex.value) (Lazy.force ey.value)
  && List.is_empty
       (Shorthand.declaration_slot_keys ~enforce_spec:(Ctx.enforce_spec ctx)
          ey.decl)
  && not
       (List.memq ey.decl
          (Shorthand.deduplicate_declarations_with ~ctx ~merge_box:false
             (Lazy.force before @ [ ey.decl ])))

(* The live declarations of [x]'s group from [x] up to, not including, [y].
   [members] lists the group in run order, so the walk starts at [x]. *)
let group_from o members x y =
  let m = members.(o.entries.(x).group) in
  let rec first lo hi =
    if lo >= hi then lo
    else
      let mid = (lo + hi) / 2 in
      if m.(mid) < x then first (mid + 1) hi else first lo mid
  in
  let rec collect k acc =
    if k >= Array.length m || m.(k) >= y then List.rev acc
    else
      let e = o.entries.(m.(k)) in
      collect (k + 1) (if e.dead then acc else e.decl :: acc)
  in
  collect (first 0 (Array.length m)) []

(* Whether [y] orders some other live entry [p] of its group, other than [x],
   that it meets, by a fact {!depends} reads off [y]: [y] rewrites slots of [p]
   ({!kill}) when it names a slot and overrides [p], being important where [p]
   is not, wherever the two stand, or later at [p]'s importance; and [y] is the
   later twin that frees [p] from an entry with [y]'s text in a group an element
   can match with [y]'s ({!twin}) when it comes after [p] at [p]'s importance.
   Without [y] the order between [p] and the rest would then be another one.
   [holders] gives the groups that hold each text. *)
let supports ~ctx o members holders x y =
  let ey = o.entries.(y) in
  let names_a_slot =
    not
      (List.is_empty
         (Shorthand.declaration_slot_keys ~enforce_spec:(Ctx.enforce_spec ctx)
            ey.decl))
  in
  let twinned =
    lazy
      (List.exists
         (fun g -> (not (Int.equal g ey.group)) && groups_meet o g ey.group)
         (Option.value ~default:[] (Int_table.find_opt holders ey.text_id)))
  in
  let orders p =
    let ep = o.entries.(p) in
    let later = p < y && Bool.equal ep.important ey.important in
    let overrides = later || (ey.important && not ep.important) in
    names_a_slot && overrides
    && Shorthand.overlap_keys_intersect ep.keys ey.keys
    || (later && overlap o ep ey && Lazy.force twinned)
  in
  Array.exists
    (fun p -> p <> y && p <> x && (not o.entries.(p).dead) && orders p)
    members.(ey.group)

(* The groups whose live entries hold each text. *)
let holders entries =
  let t = Int_table.create 256 in
  Array.iter
    (fun e ->
      if not e.dead then
        let gs = Option.value ~default:[] (Int_table.find_opt t e.text_id) in
        if not (List.exists (Int.equal e.group) gs) then
          Int_table.replace t e.text_id (e.group :: gs))
    entries;
  t

(* Whether [x] makes [y] change nothing. [x] applies wherever [y] does: it is of
   [y]'s group, or of the group of [y]'s selector under no condition. [y] is
   [x]'s text, or a twin of it ({!dedup_twin}). Where the order lets [x] stand
   right before [y], nothing between them, [y] then sets what [x] just set;
   where [y] always comes first, an unconditional [x] sets it again after [y] on
   every element [y] applies to. Either way the sheet without [y] is the sheet
   with it. A twin of [x]'s own group may have entries of that group between
   them, which the dedup reads with the two as one rule, as it reads a rule;
   only an entry of another group then stands between. The order is the run's
   own, every entry in it, and [y] must not be what orders any other entry of
   its group ({!supports}): the order of the run without [y] is then the order
   of the run less one entry, the same whichever sequence of it the run was
   written in. *)
let repeats ~ctx o members holders graph seen x y =
  let ex = o.entries.(x) and ey = o.entries.(y) in
  (not ex.dead)
  && (not (Int.equal x y))
  &&
  if Int.equal ex.group ey.group then
    x < y
    && dedup_twin ~ctx ex ey (lazy (group_from o members x y))
    && (not (supports ~ctx o members holders x y))
    && not
         (between
            ~counts:(fun z -> not (Int.equal o.entries.(z).group ey.group))
            (fst (Lazy.force graph))
            seen x y)
  else
    (Int.equal ex.text_id ey.text_id
    || dedup_twin ~ctx ex ey (Lazy.from_val [ ex.decl ]))
    && (not (supports ~ctx o members holders x y))
    && (x > y
       || not
            (between ~counts:(fun _ -> true) (fst (Lazy.force graph)) seen x y)
       )

(* Drop every entry an earlier entry or an unconditional one makes change
   nothing ({!repeats}). The groups under fewer conditions are read first, and
   in each group its normal entries before its important ones, each kind in run
   order, so the entry a decision leans on is decided before it and never
   dropped after: a witness is earlier or unconditional, an entry {!supports}
   reads is earlier at the same importance, which the order keeps where they
   meet, or normal under an important one. What is dropped depends on the order
   alone, whatever sequence of it the run was written in. Whether any was.
   [graph] is the run's order, worked out only once a pair could be a repeat. *)
let drop_repeats ~ctx o graph members unconditional =
  let seen = Array.make (Array.length o.entries) false in
  let holders = holders o.entries in
  let dropped = ref false in
  let by_depth = Array.init (Array.length o.groups) Fun.id in
  Array.stable_sort
    (fun a b ->
      Int.compare
        (Array.length o.groups.(a).chain)
        (Array.length o.groups.(b).chain))
    by_depth;
  Array.iter
    (fun g ->
      let witnesses =
        if Array.length o.groups.(g).chain = 0 then members.(g)
        else
          match Hashtbl.find_opt unconditional o.groups.(g).selector_key with
          | Some h -> Array.append members.(h) members.(g)
          | None -> members.(g)
      in
      let decide important y =
        if
          Bool.equal o.entries.(y).important important
          && (not o.entries.(y).dead)
          && Array.exists
               (fun x -> repeats ~ctx o members holders graph seen x y)
               witnesses
        then begin
          o.entries.(y).dead <- true;
          dropped := true
        end
      in
      Array.iter (decide false) members.(g);
      Array.iter (decide true) members.(g))
    by_depth;
  !dropped

(* The order a run's entries are read into, each group's overrides read. *)
let read ~ctx ?parent stmts =
  let r =
    {
      parent;
      by_name = Hashtbl.create 64;
      groups = [];
      group_count = 0;
      ids = Hashtbl.create 256;
      properties = Property_table.create 64;
      entries = [];
    }
  in
  List.iter (fun stmt -> element_chains [] stmt (read_rule r)) stmts;
  let entries = Array.of_list (List.rev r.entries) in
  let groups = Array.of_list (List.rev r.groups) in
  let members = Array.make (Array.length groups) [] in
  for i = Array.length entries - 1 downto 0 do
    let e = entries.(i) in
    members.(e.group) <- i :: members.(e.group)
  done;
  let members = Array.map Array.of_list members in
  Array.iter (kill ~ctx entries) members;
  ( members,
    {
      groups;
      entries;
      met_pairs = Array.make (pair_slots (Array.length groups)) (-1);
      met = Array.make (pair_slots (Array.length groups)) false;
      twins = twins entries members;
      unknown_meet = Int_table.create 16;
    } )

let settle ~ctx ?parent ?(drops = true) stmts =
  let members, o = read ~ctx ?parent stmts in
  let unconditional = Hashtbl.create 16 in
  Array.iteri
    (fun g group ->
      if Array.length group.chain = 0 then
        Hashtbl.replace unconditional group.selector_key g)
    o.groups;
  if drops && drop_repeats ~ctx o (lazy (order_graph o)) members unconditional
  then o.twins <- twins o.entries members;
  statements_of o 0 (emit o (order_graph o))
