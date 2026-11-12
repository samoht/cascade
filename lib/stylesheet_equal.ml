open Stylesheet_intf

(* Two layers are the same layer when they name the same idents: a [.] one of
   them carries is not the separator between two (CSS Cascade 5 sec. 6.4.1). *)
let equal_layer_name : layer_name -> layer_name -> bool =
  List.equal String.equal

(** {1 Statement Identity} *)

(* Every part is read through the equality its own module states, which is not
   the answer a structural walk gives: {!Media.equal} and {!Container.equal}
   normalise the query first, so two spellings that select the same media are
   one statement here and two under [Stdlib.compare]. The statements holding no
   condition, no selector and no block are the structural walk, the answer
   {!Declaration.equal_declaration} gives as well. *)

let equal_moz_document_condition (a : moz_document_condition) b =
  match (a, b) with
  | Url_exact a, Url_exact b
  | Domain a, Domain b
  | Media_document a, Media_document b
  | Regexp a, Regexp b ->
      String.equal a b
  | Url_prefix a, Url_prefix b -> Option.equal String.equal a b
  | (Url_exact _ | Url_prefix _ | Domain _ | Media_document _ | Regexp _), _ ->
      false

let rec equal_conditional (a : conditional) b =
  match (a, b) with
  | Media_condition a, Media_condition b -> Media.equal a b
  | Supports_condition_test a, Supports_condition_test b -> Supports.equal a b
  | And (a1, b1), And (a2, b2) | Or (a1, b1), Or (a2, b2) ->
      equal_conditional a1 a2 && equal_conditional b1 b2
  | (Media_condition _ | Supports_condition_test _ | And _ | Or _), _ -> false

let equal_import_rule (a : import_rule) b =
  String.equal a.url b.url
  && Option.equal equal_layer_name a.layer b.layer
  && Option.equal Supports.equal a.supports b.supports
  && Option.equal Media.equal a.media b.media

let rec equal_statement (a : statement) b =
  match (a, b) with
  | Rule r1, Rule r2 -> equal_rule r1 r2
  | Layer (n1, b1), Layer (n2, b2) ->
      Option.equal equal_layer_name n1 n2 && equal_block b1 b2
  | Media (m1, b1), Media (m2, b2) -> Media.equal m1 m2 && equal_block b1 b2
  | Container (n1, c1, b1), Container (n2, c2, b2) ->
      Option.equal String.equal n1 n2
      && Option.equal Container.equal c1 c2
      && equal_block b1 b2
  | Supports (c1, b1), Supports (c2, b2) ->
      Supports.equal c1 c2 && equal_block b1 b2
  | Moz_document (c1, b1), Moz_document (c2, b2) ->
      List.equal equal_moz_document_condition c1 c2 && equal_block b1 b2
  | Starting_style b1, Starting_style b2 -> equal_block b1 b2
  | When (c1, b1), When (c2, b2) -> equal_conditional c1 c2 && equal_block b1 b2
  | Else (c1, b1), Else (c2, b2) ->
      Option.equal equal_conditional c1 c2 && equal_block b1 b2
  | Origin (o1, b1), Origin (o2, b2) ->
      equal_cascade_origin o1 o2 && equal_block b1 b2
  | Scope (s1, e1, b1), Scope (s2, e2, b2) ->
      Option.equal Selector.equal s1 s2
      && Option.equal Selector.equal e1 e2
      && equal_block b1 b2
  | Import i1, Import i2 -> equal_import_rule i1 i2
  (* What is left holds no block, no at-rule condition and no selector: names,
     declarations and descriptor values, each of which the structural walk reads
     the way its own module does. It also answers every mismatched pair whose
     left side is one of these. *)
  | ( ( Declarations _ | Bang_comment _ | Charset _ | Namespace _ | Property _
      | Layer_decl _ | Supports_condition _ | Keyframes _ | Webkit_keyframes _
      | Moz_keyframes _ | Font_face _ | Counter_style _ | Page _
      | Page_with_margins _ | Font_palette_values _ | Font_feature_values _
      | View_transition _ | Position_try _ | Viewport _ | Unknown_at_rule _ ),
      _ ) ->
      Stdlib.compare a b = 0
  | ( ( Rule _ | Layer _ | Media _ | Container _ | Supports _ | Moz_document _
      | Starting_style _ | When _ | Else _ | Origin _ | Scope _ | Import _ ),
      _ ) ->
      false

and equal_block b1 b2 = List.equal equal_statement b1 b2

and equal_rule (a : rule) b =
  Option.equal String.equal a.merge_key b.merge_key
  && Selector.equal a.selector b.selector
  && List.equal Declaration.equal_declaration a.declarations b.declarations
  && equal_block a.nested b.nested

(* The fingerprint reads the statement's shape, {!Declaration.hash} of every
   declaration it holds, {!Selector.hash} of every selector, the names and
   descriptors it carries, and, recursively, the statements of its block.

   The conditions of the conditional at-rules are the exception. {!Media.equal}
   and {!Container.equal} answer on normalised queries, so no structural hash of
   one agrees with them, and neither they nor {!Supports} state a hash of their
   own. A fingerprint that disagreed with the equality it serves would file one
   statement under two keys, so a condition is left unread: two statements
   differing only there share a bucket, and [equal_statement] separates them. *)
let mix = Common.mix_int
let mix_string acc s = mix acc (Common.hash_string s)

(* Bounded like [Declaration.hash]: a descriptor list is one leaf here, and the
   cap keeps a long one out of the caller's inner loop. *)
let mix_leaf acc x = mix acc (Hashtbl.hash_param 30 100 x)

(* [None]/[Some] are shadowed here by the descriptor constructors of the same
   name, so the option is matched through its own path. *)
let mix_opt f acc = function
  | Option.None -> mix acc 0
  | Option.Some v -> f (mix acc 1) v

let mix_decls acc =
  List.fold_left (fun acc d -> mix acc (Declaration.hash d)) acc

let mix_layer_name acc = List.fold_left mix_string acc
let mix_selector acc s = mix acc (Selector.hash s)

let mix_frames acc frames =
  List.fold_left
    (fun acc (f : keyframe) ->
      mix_decls (mix_leaf acc f.selector) f.declarations)
    acc frames

let mix_namespace_url acc = function
  | (Url (url, _) : namespace_url) | Quoted url -> mix_string acc url

let rec mix_statement acc (stmt : statement) =
  match stmt with
  | Rule { selector; declarations; nested; merge_key } ->
      mix_block
        (mix_opt mix_string
           (mix_decls (mix_selector (mix acc 1) selector) declarations)
           merge_key)
        nested
  | Declarations decls -> mix_decls (mix acc 2) decls
  | Bang_comment body -> mix_string (mix acc 3) body
  | Charset name -> mix_string (mix acc 4) name
  | Import { url; layer; supports = _; media = _ } ->
      mix_opt mix_layer_name (mix_string (mix acc 5) url) layer
  | Namespace (prefix, url) ->
      mix_namespace_url (mix_opt mix_string (mix acc 6) prefix) url
  | Property { name; syntax; inherits; initial_value } ->
      mix_leaf (mix_string (mix acc 7) name) (syntax, inherits, initial_value)
  | Layer_decl names -> List.fold_left mix_layer_name (mix acc 8) names
  | Layer (name, block) ->
      mix_block (mix_opt mix_layer_name (mix acc 9) name) block
  | Media (_, block) -> mix_block (mix acc 10) block
  | Container (name, _, block) ->
      mix_block (mix_opt mix_string (mix acc 11) name) block
  | Supports (_, block) -> mix_block (mix acc 12) block
  | Moz_document (conditions, block) ->
      mix_block (mix_leaf (mix acc 13) conditions) block
  | Starting_style block -> mix_block (mix acc 14) block
  | When (_, block) -> mix_block (mix acc 15) block
  | Else (_, block) -> mix_block (mix acc 16) block
  | Supports_condition (name, decls) ->
      mix_decls (mix_string (mix acc 17) name) decls
  | Origin (origin, block) -> mix_block (mix_leaf (mix acc 18) origin) block
  | Scope (start_, end_, block) ->
      mix_block
        (mix_opt mix_selector (mix_opt mix_selector (mix acc 19) start_) end_)
        block
  | Keyframes (name, frames) -> mix_frames (mix_string (mix acc 20) name) frames
  | Webkit_keyframes (name, frames) ->
      mix_frames (mix_string (mix acc 21) name) frames
  | Moz_keyframes (name, frames) ->
      mix_frames (mix_string (mix acc 22) name) frames
  | Font_face descriptors -> mix_leaf (mix acc 23) descriptors
  | Counter_style (name, descriptors) ->
      mix_leaf (mix_string (mix acc 24) name) descriptors
  | Page (selectors, decls) -> mix_decls (mix_leaf (mix acc 25) selectors) decls
  | Page_with_margins (selectors, descriptors, margins) ->
      List.fold_left
        (fun acc (m : page_margin_rule) ->
          mix_decls (mix_string acc m.name) m.descriptors)
        (mix_decls (mix_leaf (mix acc 26) selectors) descriptors)
        margins
  | Font_palette_values (name, descriptors) ->
      mix_leaf (mix_string (mix acc 27) name) descriptors
  | Font_feature_values (families, blocks) ->
      mix_leaf (mix acc 28) (families, blocks)
  | View_transition descriptors -> mix_leaf (mix acc 29) descriptors
  | Position_try (name, decls) -> mix_decls (mix_string (mix acc 30) name) decls
  | Viewport (prefix, descriptors) -> mix_leaf (mix acc 31) (prefix, descriptors)
  | Unknown_at_rule { name; prelude; block } ->
      mix_opt mix_string
        (mix_string (mix_string (mix acc 32) name) prelude)
        block

and mix_block acc block = List.fold_left mix_statement acc block

let hash_statement stmt = mix_statement 0x811c9dc5 stmt

(* A cheap discriminating hash for a rule, folding the hash each declaration
   already caches. The stdlib structural hash reads only the first few nodes of
   a rule, so rules sharing a selector collide and any table keyed by one then
   compares whole rule subtrees on every probe. *)
let rule_hash (r : rule) =
  List.fold_left
    (fun acc d -> (acc * 31) + Declaration.hash d)
    (Selector.hash r.selector) r.declarations
