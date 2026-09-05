(** CSS stylesheet types and construction functions *)

include Stylesheet_intf

type t = stylesheet

(** {1 Construction Functions} *)

let rule ~selector ?(nested = []) ?merge_key declarations : rule =
  { selector; declarations; nested; merge_key }

let property ~syntax ?initial_value ?(inherits = false) name =
  Property { name; syntax; inherits; initial_value }

(* Two layers are the same layer when they name the same idents: a [.] one of
   them carries is not the separator between two (CSS Cascade 5 sec. 6.4.1). *)
let equal_layer_name : layer_name -> layer_name -> bool =
  List.equal String.equal

let layer_decl names = Layer_decl names
let layer ?name content = Layer (name, content)
let media ~condition content = Media (condition, content)

let media_nested ~condition declarations =
  Media (condition, [ Declarations declarations ])

let container ?name ?condition content = Container (name, condition, content)
let supports ~condition content = Supports (condition, content)
let starting_style content = Starting_style content
let with_origin cascade_origin content = Origin (cascade_origin, content)

let origin_importance_rank ~important = function
  | Transition -> 9
  | User_agent when important -> 8
  | User when important -> 7
  | Author when important -> 6
  | Animation -> 5
  | Author -> 4
  | Author_presentational_hint -> 3
  | User -> 2
  | User_agent -> 1

let import_layer_name ({ layer; _ } : import_rule) = layer

let layer_block_name = function
  | Layer (Some name, _) -> Some name
  | Layer (None, _) -> Some []
  | _ -> None

let layer_statement_name_list = function
  | Layer_decl names -> Some names
  | _ -> None

let rec index_of x i : string list -> int option = function
  | [] -> None
  | y :: ys -> if x = y then Some i else index_of x (i + 1) ys

let cascade_layer_precedence_rank ~layer_order ~important
    (layer : string option) =
  let layer_count = List.length layer_order in
  match (important, layer) with
  | false, None -> layer_count
  | false, Some name ->
      Option.value ~default:layer_count (index_of name 0 layer_order)
  | true, None -> 0
  | true, Some name ->
      let i = Option.value ~default:layer_count (index_of name 0 layer_order) in
      layer_count - i

let compare_cascade_layer_candidate ~layer_order (a : cascade_layer_candidate)
    (b : cascade_layer_candidate) =
  let key (c : cascade_layer_candidate) =
    ( (if c.important then 1 else 0),
      cascade_layer_precedence_rank ~layer_order ~important:c.important c.layer,
      c.source_order )
  in
  compare (key a) (key b)

let winning_cascade_layer_candidate ~layer_order
    (candidates : cascade_layer_candidate list) : cascade_layer_candidate option
    =
  match candidates with
  | [] -> None
  | first :: rest ->
      Some
        (List.fold_left
           (fun winner candidate ->
             if
               compare_cascade_layer_candidate ~layer_order winner candidate < 0
             then candidate
             else winner)
           first rest)

let cascade_revert_layer_candidates ~layer_order ~important ~current_layer
    (candidates : cascade_layer_candidate list) =
  let current_rank =
    cascade_layer_precedence_rank ~layer_order ~important current_layer
  in
  List.filter
    (fun (candidate : cascade_layer_candidate) ->
      candidate.important = important
      && cascade_layer_precedence_rank ~layer_order ~important candidate.layer
         < current_rank)
    candidates

let compare_cascade_origin_candidate (a : cascade_origin_candidate)
    (b : cascade_origin_candidate) =
  let key (c : cascade_origin_candidate) =
    (origin_importance_rank ~important:c.important c.origin, c.source_order)
  in
  compare (key a) (key b)

let winning_cascade_origin_candidate
    (candidates : cascade_origin_candidate list) :
    cascade_origin_candidate option =
  match candidates with
  | [] -> None
  | first :: rest ->
      Some
        (List.fold_left
           (fun winner candidate ->
             if compare_cascade_origin_candidate winner candidate < 0 then
               candidate
             else winner)
           first rest)

let revert_origin_rollback_origins = function
  | User_agent -> []
  | User -> [ User_agent ]
  | Author_presentational_hint | Author | Animation -> [ User_agent; User ]
  | Transition ->
      [ User_agent; User; Author_presentational_hint; Author; Animation ]

let cascade_revert_origin_candidates ~important ~current_origin
    (candidates : cascade_origin_candidate list) =
  let origins = revert_origin_rollback_origins current_origin in
  List.filter
    (fun (candidate : cascade_origin_candidate) ->
      candidate.important = important
      && List.exists (( = ) candidate.origin) origins)
    candidates

let declared_values ?property declarations =
  declarations
  |> List.mapi (fun source_order declaration ->
      let declaration_property = Declaration.property_name declaration in
      let value = Declaration.string_of_value ~minify:true declaration in
      {
        property = declaration_property;
        value;
        important = Declaration.is_important declaration;
        source_order;
      })
  |> List.filter (fun declared ->
      match property with
      | None -> true
      | Some property -> declared.property = property)

let cascaded_value candidates =
  winning_cascade_origin_candidate candidates
  |> Option.map (fun (c : cascade_origin_candidate) -> c.value)

let scope_proximity_rank : int option -> int = function
  | None -> min_int
  | Some hops -> -hops

let compare_cascade_candidate ~layer_order (a : cascade_candidate)
    (b : cascade_candidate) =
  let key (c : cascade_candidate) =
    ( origin_importance_rank ~important:c.important c.origin,
      cascade_layer_precedence_rank ~layer_order ~important:c.important c.layer,
      c.specificity,
      scope_proximity_rank c.scope_hops,
      c.source_order )
  in
  compare (key a) (key b)

let winning_cascade_candidate ~layer_order (candidates : cascade_candidate list)
    : cascade_candidate option =
  match candidates with
  | [] -> None
  | first :: rest ->
      Some
        (List.fold_left
           (fun winner candidate ->
             if compare_cascade_candidate ~layer_order winner candidate < 0 then
               candidate
             else winner)
           first rest)

let value ~inherits ~initial ~inherited ~cascaded =
  let inherited_or_initial () = Option.value ~default:initial inherited in
  match cascaded with
  | Some "initial" -> { value = initial; value_source = Initial_keyword }
  | Some "inherit" ->
      { value = inherited_or_initial (); value_source = Inherit_keyword }
  | Some "unset" when inherits ->
      { value = inherited_or_initial (); value_source = Unset_inherited }
  | Some "unset" -> { value = initial; value_source = Unset_initial }
  | Some value -> { value; value_source = Cascaded }
  | None when inherits ->
      { value = inherited_or_initial (); value_source = Inherited_default }
  | None -> { value = initial; value_source = Initial_default }

(* Resolve a chain of [revert] winners. Each revert exposes the next-lower
   origin's winning candidate; if THAT is also [revert], peel another origin
   off, and so on until a non-revert winner or no candidates remain. The
   rollback context comes from the winner itself so callers cannot pass a
   [current_origin] that disagrees with the candidate they exposed. *)
let rec resolve_revert_chain candidates =
  match winning_cascade_origin_candidate candidates with
  | Some winner when winner.value = "revert" ->
      cascade_revert_origin_candidates ~important:winner.important
        ~current_origin:winner.origin candidates
      |> resolve_revert_chain
  | other -> other

let specified_value_after_revert ~inherits ~initial ~inherited candidates =
  let cascaded =
    resolve_revert_chain candidates
    |> Option.map (fun (c : cascade_origin_candidate) -> c.value)
  in
  value ~inherits ~initial ~inherited ~cascaded

let rec resolve_revert_layer_chain ~layer_order candidates =
  match winning_cascade_layer_candidate ~layer_order candidates with
  | Some winner when winner.value = "revert-layer" ->
      cascade_revert_layer_candidates ~layer_order ~important:winner.important
        ~current_layer:winner.layer candidates
      |> resolve_revert_layer_chain ~layer_order
  | other -> other

let specified_value_after_revert_layer ~inherits ~initial ~inherited
    ~layer_order candidates =
  match resolve_revert_layer_chain ~layer_order candidates with
  | Some winner ->
      value ~inherits ~initial ~inherited ~cascaded:(Some winner.value)
  | None -> value ~inherits ~initial ~inherited ~cascaded:None

let value_processing_requires_document_context = function
  | Declared_value | Cascaded_value | Specified_value -> false
  | Computed_value | Used_value | Actual_value -> true

let starting_style_nested declarations =
  Starting_style [ Declarations declarations ]

let keyframes name frames = Keyframes (name, frames)
let v statements : stylesheet = statements
let empty : stylesheet = []

(** {1 Accessors} *)

let selector (rule : rule) = rule.selector
let declarations (rule : rule) = rule.declarations
let nested (rule : rule) = rule.nested

(* Listed one by one rather than closed with a wildcard: a traversal that
   descends through this function is only as complete as this match, and a
   wildcard would silently hide the next block at-rule from every caller. *)
let statement_children = function
  | Rule rule -> rule.nested
  | Layer (_, block)
  | Media (_, block)
  | Container (_, _, block)
  | Supports (_, block)
  | Moz_document (_, block)
  | When (_, block)
  | Else (_, block)
  | Starting_style block
  | Origin (_, block)
  | Scope (_, _, block) ->
      block
  | Property _ -> []
  | Declarations _ | Bang_comment _ | Charset _ | Import _ | Namespace _
  | Layer_decl _ | Supports_condition _ | Keyframes _ | Webkit_keyframes _
  | Moz_keyframes _ | Font_face _ | Counter_style _ | Page _
  | Page_with_margins _ | Font_palette_values _ | Font_feature_values _
  | View_transition _ | Position_try _ | Viewport _ | Unknown_at_rule _ ->
      []

(* The companion to [statement_children]: the declarations a statement holds
   itself. At-rules like [@keyframes] and [@page] hold declarations without
   wrapping them in a block, so a walk that only descends through
   [statement_children] never sees them. Exhaustive for the same reason: a
   wildcard would hide the next declaration-carrying at-rule from every caller.
   Descriptor at-rules ([@font-face], [@counter-style], [@viewport], ...) hold
   their own descriptor types rather than declarations. *)
let statement_declarations = function
  | Rule rule -> rule.declarations
  | Declarations decls
  | Supports_condition (_, decls)
  | Page (_, decls)
  | Position_try (_, decls) ->
      decls
  | Keyframes (_, frames)
  | Webkit_keyframes (_, frames)
  | Moz_keyframes (_, frames) ->
      List.concat_map (fun (frame : keyframe) -> frame.declarations) frames
  | Page_with_margins (_, descriptors, margins) ->
      descriptors
      @ List.concat_map
          (fun (margin : page_margin_rule) -> margin.descriptors)
          margins
  | Property _ -> []
  | Bang_comment _ | Charset _ | Import _ | Namespace _ | Layer_decl _ | Layer _
  | Media _ | Container _ | Supports _ | Moz_document _ | When _ | Else _
  | Starting_style _ | Origin _ | Scope _ | Font_face _ | Counter_style _
  | Font_palette_values _ | Font_feature_values _ | View_transition _
  | Viewport _ | Unknown_at_rule _ ->
      []

(* The rebuilding counterparts of the two readers above. A read-only walk can be
   written on [statement_children] and [statement_declarations] alone; one that
   rewrites the tree needs to put the result back, and hand-rolling that half is
   how a block at-rule ends up read but not written. Exhaustive for the same
   reason as the readers.

   Both preserve physical identity: a statement whose lists [f] returns
   physically unchanged is returned itself rather than rebuilt, so a pass built
   on them keeps the subtree sharing its fixed point converges on. That only
   pays off when [f] preserves identity too. *)
let map_statement_children f stmt =
  match stmt with
  | Rule rule ->
      let nested = f rule.nested in
      if nested == rule.nested then stmt else Rule { rule with nested }
  | Layer (name, block) ->
      let block' = f block in
      if block' == block then stmt else Layer (name, block')
  | Media (query, block) ->
      let block' = f block in
      if block' == block then stmt else Media (query, block')
  | Container (name, query, block) ->
      let block' = f block in
      if block' == block then stmt else Container (name, query, block')
  | Supports (condition, block) ->
      let block' = f block in
      if block' == block then stmt else Supports (condition, block')
  | Moz_document (condition, block) ->
      let block' = f block in
      if block' == block then stmt else Moz_document (condition, block')
  | When (condition, block) ->
      let block' = f block in
      if block' == block then stmt else When (condition, block')
  | Else (condition, block) ->
      let block' = f block in
      if block' == block then stmt else Else (condition, block')
  | Starting_style block ->
      let block' = f block in
      if block' == block then stmt else Starting_style block'
  | Origin (origin, block) ->
      let block' = f block in
      if block' == block then stmt else Origin (origin, block')
  | Scope (start, end_, block) ->
      let block' = f block in
      if block' == block then stmt else Scope (start, end_, block')
  | Property _ | Declarations _ | Bang_comment _ | Charset _ | Import _
  | Namespace _ | Layer_decl _ | Supports_condition _ | Keyframes _
  | Webkit_keyframes _ | Moz_keyframes _ | Font_face _ | Counter_style _
  | Page _ | Page_with_margins _ | Font_palette_values _ | Font_feature_values _
  | View_transition _ | Position_try _ | Viewport _ | Unknown_at_rule _ ->
      stmt

let map_frame_declarations f frames =
  Common.List.map_preserve
    (fun (frame : keyframe) ->
      let declarations = f frame.declarations in
      if declarations == frame.declarations then frame
      else { frame with declarations })
    frames

(* [f] sees each declaration list the statement holds as its own list, not the
   concatenation [statement_declarations] returns: the frames of [@keyframes]
   and the margin rules of [@page] each keep their own block. *)
let map_statement_declarations f stmt =
  match stmt with
  | Rule rule ->
      let declarations = f rule.declarations in
      if declarations == rule.declarations then stmt
      else Rule { rule with declarations }
  | Declarations decls ->
      let decls' = f decls in
      if decls' == decls then stmt else Declarations decls'
  | Supports_condition (name, decls) ->
      let decls' = f decls in
      if decls' == decls then stmt else Supports_condition (name, decls')
  | Page (selector, decls) ->
      let decls' = f decls in
      if decls' == decls then stmt else Page (selector, decls')
  | Position_try (name, decls) ->
      let decls' = f decls in
      if decls' == decls then stmt else Position_try (name, decls')
  | Keyframes (name, frames) ->
      let frames' = map_frame_declarations f frames in
      if frames' == frames then stmt else Keyframes (name, frames')
  | Webkit_keyframes (name, frames) ->
      let frames' = map_frame_declarations f frames in
      if frames' == frames then stmt else Webkit_keyframes (name, frames')
  | Moz_keyframes (name, frames) ->
      let frames' = map_frame_declarations f frames in
      if frames' == frames then stmt else Moz_keyframes (name, frames')
  | Page_with_margins (selector, descriptors, margins) ->
      let descriptors' = f descriptors in
      let margins' =
        Common.List.map_preserve
          (fun (margin : page_margin_rule) ->
            let descriptors = f margin.descriptors in
            if descriptors == margin.descriptors then margin
            else { margin with descriptors })
          margins
      in
      if descriptors' == descriptors && margins' == margins then stmt
      else Page_with_margins (selector, descriptors', margins')
  | Property _ | Bang_comment _ | Charset _ | Import _ | Namespace _
  | Layer_decl _ | Layer _ | Media _ | Container _ | Supports _ | Moz_document _
  | When _ | Else _ | Starting_style _ | Origin _ | Scope _ | Font_face _
  | Counter_style _ | Font_palette_values _ | Font_feature_values _
  | View_transition _ | Viewport _ | Unknown_at_rule _ ->
      stmt

(* Where in a stylesheet a declaration sits. This is the one classification of
   the declaration-carrying statements: [statement_declarations] extracts the
   lists, this says what they are, and the walks below filter on it. Exhaustive
   for the same reason as the extractors. *)
type declaration_site =
  | No_declarations
  | Element_rule
  | Animation_frame
  | Page_box
  | Position_fallback
  | Condition_test

let declaration_site = function
  | Rule _ | Declarations _ -> Element_rule
  | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _ -> Animation_frame
  | Page _ | Page_with_margins _ -> Page_box
  | Position_try _ -> Position_fallback
  | Supports_condition _ -> Condition_test
  | Property _ | Bang_comment _ | Charset _ | Import _ | Namespace _
  | Layer_decl _ | Layer _ | Media _ | Container _ | Supports _ | Moz_document _
  | When _ | Else _ | Starting_style _ | Origin _ | Scope _ | Font_face _
  | Counter_style _ | Font_palette_values _ | Font_feature_values _
  | View_transition _ | Viewport _ | Unknown_at_rule _ ->
      No_declarations

type declaration_sites = {
  element_rule : bool;
  animation_frame : bool;
  page_box : bool;
  position_fallback : bool;
  condition_test : bool;
}

let every_site =
  {
    element_rule = true;
    animation_frame = true;
    page_box = true;
    position_fallback = true;
    condition_test = true;
  }

let site_selected sites = function
  | No_declarations -> false
  | Element_rule -> sites.element_rule
  | Animation_frame -> sites.animation_frame
  | Page_box -> sites.page_box
  | Position_fallback -> sites.position_fallback
  | Condition_test -> sites.condition_test

let at_declaration_site sites stmt = site_selected sites (declaration_site stmt)

let rec fold_statements f acc block =
  List.fold_left
    (fun acc stmt -> fold_statements f (f acc stmt) (statement_children stmt))
    acc block

let iter_statements f block = fold_statements (fun () stmt -> f stmt) () block

(* The rewriting counterpart of [iter_statements]. [f] decides each statement's
   fate; the descent into what it holds is [map_statement_children]'s, so a
   caller that only cares about one statement kind does not enumerate the ones
   it nests inside. Applied to a statement before the statements it holds, and
   to the replacement rather than the original, so a rewrite and the walk below
   it compose. *)
let edit_statements f block =
  let rec statement stmt =
    match f stmt with
    | Drop -> Common.List.Drop
    | Keep -> descend stmt stmt
    | Replace stmt' -> descend stmt stmt'
  and descend stmt stmt' =
    let stmt' =
      map_statement_children (Common.List.edit_preserve statement) stmt'
    in
    if stmt' == stmt then Common.List.Keep else Common.List.Replace stmt'
  in
  Common.List.edit_preserve statement block

let fold_declarations ?(sites = every_site) f acc block =
  fold_statements
    (fun acc stmt ->
      if at_declaration_site sites stmt then f acc (statement_declarations stmt)
      else acc)
    acc block

let iter_declarations ?sites f block =
  fold_declarations ?sites (fun () decls -> f decls) () block

let map_declarations f block =
  let rec statement stmt =
    map_statement_children
      (Common.List.map_preserve statement)
      (map_statement_declarations f stmt)
  in
  Common.List.map_preserve statement block

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

(** {1 Pretty Printing} *)

let pp_property_rule : 'a property_rule Pp.t =
 fun ctx { name; syntax; inherits; initial_value } ->
  let is_empty_universal_initial : type a.
      a Variables.syntax -> a option -> bool =
   fun syntax initial_value ->
    match (syntax, initial_value) with
    | Variables.Universal, Some "" -> true
    | _ -> false
  in
  let pp_initial_value ctx v =
    Pp.semicolon ctx ();
    Pp.cut ctx ();
    Pp.indent
      (fun ctx () ->
        Pp.string ctx "initial-value:";
        Pp.space_if_pretty ctx ();
        Variables.pp_value syntax ctx v)
      ctx ()
  in
  let pp_initial_value_opt ctx =
    if Pp.minified ctx && is_empty_universal_initial syntax initial_value then
      ()
    else
      match initial_value with None -> () | Some v -> pp_initial_value ctx v
  in
  Pp.string ctx "@property ";
  (* CSS Syntax 3 (ED) sec. 4.3.7: the prelude names the custom property this
     rule registers, so it is written with the escapes that read the same name
     back (see [Properties.pp_property]). *)
  Pp.string ctx (Parser.escape_ident name);
  Pp.sp ctx ();
  Pp.braces
    (fun ctx () ->
      Pp.string ctx "syntax:";
      Pp.space_if_pretty ctx ();
      Variables.pp_syntax ctx syntax;
      Pp.string ctx ";";
      Pp.cut ctx ();
      Pp.indent
        (fun ctx () ->
          Pp.string ctx "inherits:";
          Pp.space_if_pretty ctx ();
          Pp.string ctx (if inherits then "true" else "false"))
        ctx ();
      pp_initial_value_opt ctx;
      if not ctx.Pp.minify then Pp.semicolon ctx ())
    ctx ()

(* CSS Animations 1 sec. 3 [<keyframes-name>] is [<custom-ident> | <string>].
   The reader normalizes either form to a plain OCaml string; on output we
   prefer the bare identifier when the value is a syntactically valid CSS ident
   (shorter than the quoted form), falling back to a double-quoted string when
   the name contains characters that would otherwise need escaping. *)
let pp_keyframes_name ctx name =
  let len = String.length name in
  let is_ident_continue c =
    (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9')
    || c = '-' || c = '_'
  in
  let is_ident_start c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
  in
  let is_safe_ident =
    len > 0
    && (is_ident_start name.[0]
       || name.[0] = '-'
          && len >= 2
          && (is_ident_start name.[1] || name.[1] = '-'))
    &&
    let ok = ref true in
    String.iter (fun c -> if not (is_ident_continue c) then ok := false) name;
    !ok
  in
  if is_safe_ident then Pp.string ctx name else Pp.quoted_string ctx name

let pp_keyframe_position : Keyframe.position Pp.t =
 fun ctx pos ->
  (* CSS Animations 1 7.1: [from] is equivalent to [0%] and [to] to [100%].
     Under minification, canonicalise to the shorter spelling. *)
  match (pos, Pp.minified ctx) with
  | Keyframe.From, true -> Pp.string ctx "0%"
  | Keyframe.Percent 100., true -> Pp.string ctx "to"
  | _ -> Pp.string ctx (Keyframe.string_of_position pos)

let pp_keyframe_selector : Keyframe.selector Pp.t =
 fun ctx -> function
  | Keyframe.Positions positions ->
      Pp.list ~sep:Pp.comma pp_keyframe_position ctx positions

let pp_keyframe : keyframe Pp.t =
 fun ctx kf ->
  pp_keyframe_selector ctx kf.selector;
  Pp.sp ctx ();
  Pp.braced_semicolon_list Declaration.pp ctx kf.declarations

let pp_keyframes_block_statement ctx header name frames =
  Pp.string ctx header;
  pp_keyframes_name ctx name;
  Pp.sp ctx ();
  (* Sibling frame blocks separate with a blank line, like statements in any
     other block (see [pp_block]). *)
  Pp.braced_list ~sep:Pp.(cut ++ cut) pp_keyframe ctx frames

let pp_page_pseudo ctx p =
  Pp.char ctx ':';
  Pp.string ctx
    (match p with
    | First -> "first"
    | Left -> "left"
    | Right -> "right"
    | Blank -> "blank")

let pp_page_selector_one ctx ({ name; pseudos } : page_selector) =
  Option.iter (Pp.string ctx) name;
  List.iter (pp_page_pseudo ctx) pseudos

let pp_page_selector ctx (selectors : page_selector list) =
  match selectors with
  | [] -> ()
  | first :: _ as selectors ->
      (* A named selector needs a hard space after [@page]; a pseudo-only one
         takes a collapsible space ([@page:first] minifies cleanly). *)
      (match (first : page_selector).name with
      | Some _ -> Pp.space ctx ()
      | None -> Pp.sp ctx ());
      Pp.list ~sep:Pp.comma pp_page_selector_one ctx selectors

let pp_page_margin_rule : page_margin_rule Pp.t =
 fun ctx rule ->
  Pp.string ctx "@";
  Pp.string ctx rule.name;
  Pp.sp ctx ();
  Pp.braced_semicolon_list Declaration.pp ctx rule.descriptors

let pp_page_with_margins_body ctx descriptors margins =
  Pp.braces
    (fun ctx () ->
      Pp.list ~sep:Pp.semicolon_cut
        (fun ctx (i, d) ->
          if i = 0 then Declaration.pp ctx d else Pp.indent Declaration.pp ctx d)
        ctx
        (List.mapi (fun i d -> (i, d)) descriptors);
      if descriptors <> [] && margins <> [] then (
        Pp.semicolon ctx ();
        Pp.cut ctx ())
      else if descriptors <> [] && not ctx.Pp.minify then Pp.semicolon ctx ();
      Pp.list ~sep:Pp.cut
        (fun ctx (i, m) ->
          if i = 0 && descriptors = [] then pp_page_margin_rule ctx m
          else Pp.indent pp_page_margin_rule ctx m)
        ctx
        (List.mapi (fun i m -> (i, m)) margins))
    ctx ()

let pp_scope_selector ctx s = Selector.pp ctx s

let pp_viewport_prefix_keyword = function
  | Standard -> "@viewport"
  | Ms_prefixed -> "@-ms-viewport"

let pp_viewport_descriptor ctx { name; value } =
  Pp.string ctx name;
  Pp.char ctx ':';
  Pp.string ctx value

let pp_viewport_statement ctx prefix descriptors =
  Pp.string ctx (pp_viewport_prefix_keyword prefix);
  Pp.sp ctx ();
  Pp.braced_semicolon_list pp_viewport_descriptor ctx descriptors

let trim_unknown_at_prelude prelude =
  let rec trim_end i =
    if i < 0 then 0
    else
      match prelude.[i] with
      | ';' | '}' | ' ' | '\t' | '\n' | '\r' -> trim_end (i - 1)
      | _ -> i + 1
  in
  let n = String.length prelude in
  String.sub prelude 0 (trim_end (n - 1))

(* CSS Syntax 3 (ED) sec. 4.3.1: a backslash starts an escape unless a newline
   follows it. A raw body ending on an odd run of backslashes therefore escapes
   the closer written straight after it, and the at-rule swallows whatever comes
   next instead of ending. *)
let body_escapes_its_closer body =
  let rec backslashes i n =
    if i < 0 || body.[i] <> '\\' then n else backslashes (i - 1) (n + 1)
  in
  backslashes (String.length body - 1) 0 land 1 = 1

let pp_unknown_at_rule_statement ctx name prelude (block : string option) =
  (* CSS Syntax 3 (ED) section 5.5.2: an at-rule terminates on [;], [}], or EOF.
     When the Parser captures an unterminated nested block ([(...], [[...],
     [{...]) its source slice can include the at-rule terminator or the
     enclosing block's close - don't echo them as part of the prelude or each
     round-trip stacks another [;]/[}]. *)
  let prelude = trim_unknown_at_prelude prelude in
  Pp.char ctx '@';
  (* CSS Syntax 3 (ED) section 9.1: an at-rule name with non-ident-continue code
     points (control chars, escapes, non-ASCII) must round-trip through
     [escape_ident] so the serialized [\6 T] re-tokenizes back to the same
     at-keyword token. *)
  Pp.string ctx (Parser.escape_ident name);
  (* CSS Syntax 3 (ED) sec. 4.3.1: the at-keyword consumes an ident sequence, so
     the whitespace before the prelude is the only thing keeping them apart. It
     is a hard space, not layout - minifying [@foo bar] to [@foobar] names a
     different at-rule. *)
  if prelude <> "" then (
    Pp.space ctx ();
    Pp.string ctx prelude);
  match block with
  | None -> Pp.semicolon ctx ()
  | Some body ->
      Pp.sp ctx ();
      Pp.char ctx '{';
      Pp.string ctx body;
      (* Sec. 4.3.7 reads a space or a hex digit as part of the escape, so a
         newline is the only separator that leaves the last backslash the delim
         token it was. *)
      if body_escapes_its_closer body then Pp.char ctx '\n';
      Pp.char ctx '}'

let font_face_participates descriptors =
  List.exists (function Font_family _ -> true | _ -> false) descriptors
  && List.exists (function Src _ -> true | _ -> false) descriptors

let should_print_statement ctx = function
  | Font_face descriptors when Pp.minified ctx ->
      font_face_participates descriptors
  | _ -> true

let printable_statements ctx statements =
  if Pp.minified ctx then List.filter (should_print_statement ctx) statements
  else statements

module String_set = Pp.String_set

let normalise_charset statements =
  (* CSS Syntax 3 (ED) sec. 8.3: [@charset] is an encoding-declaration byte
     pattern recognised before tokenization, not a stylesheet at-rule after
     parsing. In minified output the serializer emits UTF-8, so [@charset
     "UTF-8"] is redundant; keep at most the first non-UTF-8 declaration for
     byte-level compatibility. *)
  let is_utf8 encoding =
    String.equal (String.lowercase_ascii encoding) "utf-8"
  in
  let kept_one = ref false in
  List.filter
    (fun stmt ->
      match stmt with
      | Charset enc when is_utf8 enc -> false
      | Charset _ when !kept_one -> false
      | Charset _ ->
          kept_one := true;
          true
      | _ -> true)
    statements

let color_custom_property_names stylesheet =
  let var_name_of_custom_property = Custom_property_name.strip_prefix in
  let declaration names = function
    | Declaration.Declaration
        {
          property = Properties.Custom_property name;
          value =
            Properties.Custom_value
              { value = Properties.Typed { kind = Properties.Color; _ }; _ };
          _;
        } ->
        String_set.add (var_name_of_custom_property name) names
    | _ -> names
  in
  let rec statement names stmt =
    let names =
      List.fold_left declaration names (statement_declarations stmt)
    in
    List.fold_left statement names (statement_children stmt)
  in
  List.fold_left statement String_set.empty stylesheet

let color_fallback_of_length_fallback :
    Values.length Values.fallback -> Values.color Values.fallback = function
  | Values.None -> Values.None
  | Values.Empty -> Values.Empty
  | Values.Empty2 -> Values.Empty2
  | Values.Syntax_fallback components -> Values.Syntax_fallback components
  | Values.Var_fallback name -> Values.Var_fallback name
  | Values.Fallback value ->
      Values.Syntax_fallback
        (Cursor.remaining
           (Cursor.of_string (Pp.to_string ~minify:true Values.pp_length value)))

(* The shadow rewrite uses the [var(--ring)] reference as the colour slot of a
   [box-shadow]; the inline [default] (a length) doesn't apply when the var is
   consumed in colour position, so drop it. *)
let color_var_of_length_var (var : Values.length Values.var) :
    Values.color Values.var =
  {
    name = var.name;
    fallback = color_fallback_of_length_fallback var.fallback;
    default = None;
    layer = var.layer;
    meta = var.meta;
    runtime = var.runtime;
  }

let rec rewrite_shadow_value color_vars (value : Properties.shadow) :
    Properties.shadow =
  match value with
  | Properties.Shadow
      ({ blur = Some (Values.Var var); spread = None; color = None; _ } as
       shadow)
    when String_set.mem var.name color_vars ->
      Properties.Shadow
        {
          shadow with
          blur = Some Values.Zero;
          color = Some (Values.Var (color_var_of_length_var var));
        }
  | Properties.List shadows ->
      Properties.List (List.map (rewrite_shadow_value color_vars) shadows)
  | shadow -> shadow

let rewrite_shadow_decl color_vars = function
  | Declaration.Declaration
      { property = Properties.Box_shadow as property; value; important; _ } as
    decl ->
      let value' = rewrite_shadow_value color_vars value in
      if value' == value then decl else Declaration.v ~important property value'
  | Declaration.Declaration
      {
        property = Properties.Custom_property _ as property;
        value =
          Properties.Custom_value
            ({
               value =
                 Properties.Typed { kind = Properties.Shadow; value = shadow };
               _;
             } as custom_value);
        important;
        _;
      } as decl ->
      let shadow' = rewrite_shadow_value color_vars shadow in
      if shadow' == shadow then decl
      else
        Declaration.v ~important property
          (Properties.Custom_value
             {
               custom_value with
               value =
                 Properties.Typed { kind = Properties.Shadow; value = shadow' };
             })
  | decl -> decl

let normalise_shadows stylesheet =
  let color_vars = color_custom_property_names stylesheet in
  if String_set.is_empty color_vars then stylesheet
  else
    let declarations = List.map (rewrite_shadow_decl color_vars) in
    let rec statement stmt =
      stmt
      |> map_statement_declarations declarations
      |> map_statement_children (List.map statement)
    in
    List.map statement stylesheet

let normalise statements = statements |> normalise_charset |> normalise_shadows

let import_url_starts_with_url url len =
  len >= 4 && String.lowercase_ascii (String.sub url 0 4) = "url("

let import_url_unwrap_quoted url len =
  if len >= 2 && (url.[0] = '"' || url.[0] = '\'') && url.[len - 1] = url.[0]
  then Some (String.sub url 1 (len - 2))
  else None

let import_url_strip_inner_quotes inner : string option =
  let inner_len = String.length inner in
  if inner_len = 0 then None
  else if
    inner_len >= 2
    && (inner.[0] = '"' || inner.[0] = '\'')
    && inner.[inner_len - 1] = inner.[0]
  then Some (String.sub inner 1 (inner_len - 2))
  else Some inner

let import_url_inner_string url len : string option =
  (* Lift the inner string out of [url(...)] - either the quoted form
     [url("foo")] / [url('foo')] or the bare form [url(foo)]. Returns the
     unquoted body so the [@import] / [@namespace] minify path can re-emit it as
     a bare double-quoted string. *)
  if not (import_url_starts_with_url url len) then None
  else if len < 5 || url.[len - 1] <> ')' then None
  else
    let inner = String.sub url 4 (len - 5) |> String.trim in
    import_url_strip_inner_quotes inner

let pp_import_url_minified ctx url len =
  (* Canonicalise to the shortest spec-equivalent form: a double-quoted string
     (CSS Syntax 3 (ED) sec. 4.3.5 prefers double quotes). The [url()] wrapping
     is five characters of overhead that the bare-string form omits per CSS
     Conditional Rules 3, and a single-quoted source string re-emits with double
     quotes. *)
  match import_url_inner_string url len with
  | Some s -> Pp.quoted_string ctx s
  | None -> (
      match import_url_unwrap_quoted url len with
      | Some s -> Pp.quoted_string ctx s
      | None ->
          if len > 0 && import_url_starts_with_url url len then
            Pp.string ctx url
          else Pp.quoted_string ctx url)

let pp_import_url ctx url =
  let len = String.length url in
  if Pp.minified ctx then pp_import_url_minified ctx url len
  else if
    len > 0
    && (url.[0] = '"' || url.[0] = '\'' || import_url_starts_with_url url len)
  then Pp.string ctx url
  else Pp.quoted_string ctx url

(* CSS Cascade 5 sec. 6.4.1: a [<layer-name>] is [<ident> ['.' <ident>]*], so
   each ident takes the escapes CSS Syntax 3 (ED) sec. 4.3.7 needs (see
   [Properties.pp_property]) and the [.] separators stay bare. A [.] an ident
   carries is escaped, and so never reads back as a separator. *)
let string_of_layer_name name =
  String.concat "." (List.map Parser.escape_ident name)

let pp_layer_name : layer_name Pp.t =
 fun ctx name -> Pp.string ctx (string_of_layer_name name)

let pp_import_layer ctx layer =
  Pp.sp ctx ();
  if layer = [] then Pp.string ctx "layer"
  else (
    Pp.string ctx "layer(";
    pp_layer_name ctx layer;
    Pp.char ctx ')')

let pp_import_supports ctx supports =
  Pp.sp ctx ();
  Pp.string ctx "supports(";
  (match supports with
  | Supports.Property decl -> Supports.pp_declaration_feature ctx decl
  | _ -> Supports.pp ctx supports);
  Pp.char ctx ')'

let starts_with s prefix =
  let s_len = String.length s and prefix_len = String.length prefix in
  s_len >= prefix_len && String.sub s 0 prefix_len = prefix

let import_supports_media_needs_space supports media =
  match supports with
  | Supports.Property decl ->
      let supports =
        Pp.to_string ~minify:true Supports.pp_declaration_feature decl
      in
      let media = Media.to_string ~minify:true media in
      starts_with supports "--" && starts_with media "("
  | _ -> false

let pp_import_components ctx { url; layer; supports; media } =
  Pp.sp ctx ();
  pp_import_url ctx url;
  Option.iter (pp_import_layer ctx) layer;
  Option.iter (pp_import_supports ctx) supports;
  Option.iter
    (fun media ->
      (match supports with
      | Some supports when import_supports_media_needs_space supports media ->
          Pp.space ctx ()
      | None when layer = Some [] -> Pp.space ctx ()
      | _ -> Pp.sp ctx ());
      Media.pp ctx media)
    media

let strip_outer_parens s =
  let s = String.trim s in
  let len = String.length s in
  if len >= 2 && s.[0] = '(' && s.[len - 1] = ')' then String.sub s 1 (len - 2)
  else s

let pp_condition_function ctx name rendered =
  Pp.string ctx name;
  Pp.char ctx '(';
  Pp.string ctx (strip_outer_parens rendered);
  Pp.char ctx ')'

let rec pp_conditional : conditional Pp.t =
 fun ctx -> function
  | Media_condition condition ->
      pp_condition_function ctx "media"
        (Pp.to_string ~minify:ctx.Pp.minify ~lossless:ctx.Pp.lossless Media.pp
           condition)
  | Supports_condition_test condition ->
      pp_condition_function ctx "supports"
        (Pp.to_string ~minify:ctx.Pp.minify ~lossless:ctx.Pp.lossless
           Supports.pp condition)
  | And (a, b) ->
      pp_conditional ctx a;
      Pp.string ctx " and ";
      pp_conditional ctx b
  | Or (a, b) ->
      pp_conditional ctx a;
      Pp.string ctx " or ";
      pp_conditional ctx b

(* Every argument but the [<url>] one is a [<string>], so it stays quoted; the
   [<url>] takes the shortest spelling [Pp.url] gives it. *)
let pp_moz_document_condition ctx =
  let call name arg =
    Pp.string ctx name;
    Pp.char ctx '(';
    Pp.quoted_string ctx arg;
    Pp.char ctx ')'
  in
  function
  | Url_exact url -> Pp.url ctx url
  | Url_prefix None -> Pp.string ctx "url-prefix()"
  | Url_prefix (Some prefix) -> call "url-prefix" prefix
  | Domain domain -> call "domain" domain
  | Media_document media -> call "media-document" media
  | Regexp regexp -> call "regexp" regexp

let pp_declarations_statement ctx raw_decls =
  (* Bare declarations for CSS nesting - no selector/braces, just declarations.
     No extra indent since the containing block handles it *)
  let decls = raw_decls in
  Pp.list ~sep:Pp.semicolon_cut Declaration.pp ctx decls;
  if decls <> [] && not ctx.Pp.minify then Pp.semicolon ctx ()

let pp_namespace_uri ctx = function
  | Url (value, _) when Pp.minified ctx -> Pp.quoted_string ctx value
  | Url (value, Bare) ->
      Pp.string ctx "url(";
      Pp.string ctx value;
      Pp.char ctx ')'
  | Url (value, Quoted q) ->
      Pp.string ctx "url(";
      Pp.char ctx q;
      Pp.string ctx value;
      Pp.char ctx q;
      Pp.char ctx ')'
  | Quoted value -> Pp.quoted_string ctx value

let pp_namespace_statement ctx prefix uri =
  Pp.string ctx "@namespace ";
  (match prefix with
  | Some p ->
      Pp.string ctx p;
      Pp.sp ctx ()
  | None -> ());
  pp_namespace_uri ctx uri;
  Pp.semicolon ctx ()

let pp_supports_condition_value ctx condition =
  match condition with
  | Supports.And (_, Supports.Function _) | Supports.And (Supports.Function _, _)
    ->
      Pp.char ctx '(';
      Supports.pp ctx condition;
      Pp.char ctx ')'
  | _ -> Supports.pp ctx condition

let pp_font_variant_descriptor_value ctx = function
  | Ligature value -> Properties.pp_font_variant_ligature ctx value
  | Caps value -> Properties.pp_font_variant_caps ctx value
  | Numeric value -> Properties.pp_font_variant_numeric_token ctx value
  | East_asian value -> Properties.pp_east_asian_feature ctx value

let rec pp_font_tech_descriptor ctx : font_tech_descriptor -> unit = function
  | Tech tech -> Pp.string ctx (Supports.string_of_font_tech tech)
  | Var var -> Values.pp_var pp_font_tech_descriptor ctx var

let rec pp_font_variant_descriptor ctx = function
  | Normal -> Pp.string ctx "normal"
  | None -> Pp.string ctx "none"
  | Values values ->
      Pp.list ~sep:Pp.space pp_font_variant_descriptor_value ctx values
  | Var var -> Values.pp_var pp_font_variant_descriptor ctx var

let pp_font_face_descriptor : font_face_descriptor Pp.t =
 fun ctx desc ->
  let pp_descriptor name pp_value value =
    Pp.string ctx name;
    Pp.string ctx ":";
    Pp.space_if_pretty ctx ();
    pp_value ctx value
  in
  match desc with
  | Font_family families ->
      pp_descriptor "font-family"
        (fun ctx fams ->
          Pp.list ~sep:Pp.comma Properties.pp_font_family_name ctx fams)
        families
  | Src value -> pp_descriptor "src" Properties.pp_font_src value
  | Font_style style ->
      pp_descriptor "font-style" Properties.pp_font_style style
  | Font_style_range (min_style, max_style) ->
      pp_descriptor "font-style"
        (fun ctx (min_style, max_style) ->
          Properties.pp_font_style ctx min_style;
          Pp.space ctx ();
          Properties.pp_font_style ctx max_style)
        (min_style, max_style)
  | Font_weight weight ->
      pp_descriptor "font-weight" Properties.pp_font_weight weight
  | Font_weight_range (min_weight, max_weight) ->
      pp_descriptor "font-weight"
        (fun ctx (min_weight, max_weight) ->
          Properties.pp_font_weight ctx min_weight;
          Pp.space ctx ();
          Properties.pp_font_weight ctx max_weight)
        (min_weight, max_weight)
  | Font_stretch stretch ->
      pp_descriptor "font-stretch" Properties.pp_font_stretch stretch
  | Font_stretch_range (min_stretch, max_stretch) ->
      pp_descriptor "font-stretch"
        (fun ctx (min_stretch, max_stretch) ->
          Properties.pp_font_stretch ctx min_stretch;
          Pp.space ctx ();
          Properties.pp_font_stretch ctx max_stretch)
        (min_stretch, max_stretch)
  | Font_display value ->
      pp_descriptor "font-display" Properties.pp_font_display value
  | Unicode_range values ->
      pp_descriptor "unicode-range"
        (fun ctx vs -> Pp.list ~sep:Pp.comma Properties.pp_unicode_range ctx vs)
        values
  | Font_variant value ->
      pp_descriptor "font-variant" pp_font_variant_descriptor value
  | Font_feature_settings value ->
      pp_descriptor "font-feature-settings" Properties.pp_font_feature_settings
        value
  | Font_variation_settings value ->
      pp_descriptor "font-variation-settings"
        Properties.pp_font_variation_settings value
  | Font_tech value -> pp_descriptor "font-tech" pp_font_tech_descriptor value
  | Size_adjust value ->
      pp_descriptor "size-adjust" Font_face.pp_size_adjust value
  | Ascent_override value ->
      pp_descriptor "ascent-override" Font_face.pp_metric_override value
  | Descent_override value ->
      pp_descriptor "descent-override" Font_face.pp_metric_override value
  | Line_gap_override value ->
      pp_descriptor "line-gap-override" Font_face.pp_metric_override value

let pp_counter_style_system ctx = function
  | Cyclic -> Pp.string ctx "cyclic"
  | Numeric -> Pp.string ctx "numeric"
  | Alphabetic -> Pp.string ctx "alphabetic"
  | Symbolic -> Pp.string ctx "symbolic"
  | Fixed None -> Pp.string ctx "fixed"
  | Fixed (Some n) ->
      Pp.string ctx "fixed";
      Pp.space ctx ();
      Pp.int ctx n
  | Additive -> Pp.string ctx "additive"
  | Extends name ->
      Pp.string ctx "extends";
      Pp.space ctx ();
      Pp.string ctx (Parser.escape_ident name)

let pp_counter_symbol ctx symbol = Pp.quoted_string ctx symbol

let pp_counter_style_descriptor ctx =
  let pp_descriptor name pp_value value =
    Pp.string ctx name;
    Pp.char ctx ':';
    Pp.space_if_pretty ctx ();
    pp_value ctx value
  in
  function
  | System system -> pp_descriptor "system" pp_counter_style_system system
  | Symbols symbols ->
      (* Symbols are quoted strings; the [""] delimiter is unambiguous so the
         inter-symbol space is elidable in minified output. *)
      pp_descriptor "symbols"
        (Pp.list ~sep:Pp.space_if_pretty pp_counter_symbol)
        symbols
  | Suffix suffix -> pp_descriptor "suffix" pp_counter_symbol suffix
  | Prefix prefix -> pp_descriptor "prefix" pp_counter_symbol prefix
  | Fallback value -> pp_descriptor "fallback" Pp.string value
  | Range value -> pp_descriptor "range" Pp.string value
  | Pad value -> pp_descriptor "pad" Pp.string value
  | Negative value -> pp_descriptor "negative" Pp.string value
  | Additive_symbols value -> pp_descriptor "additive-symbols" Pp.string value
  | Speak_as value -> pp_descriptor "speak-as" Pp.string value

let pp_font_palette_base ctx = function
  | Light -> Pp.string ctx "light"
  | Dark -> Pp.string ctx "dark"
  | Index i -> Pp.string ctx (Int.to_string i)
  | Palette_ident s -> Pp.string ctx s

let pp_font_palette_descriptor : font_palette_descriptor Pp.t =
 fun ctx -> function
  | Palette_font_family families ->
      Pp.string ctx "font-family:";
      Pp.space_if_pretty ctx ();
      Pp.list ~sep:Pp.comma Properties.pp_font_family_name ctx families
  | Base_palette base ->
      Pp.string ctx "base-palette:";
      Pp.space_if_pretty ctx ();
      pp_font_palette_base ctx base
  | Override_colors entries ->
      Pp.string ctx "override-colors:";
      Pp.space_if_pretty ctx ();
      Pp.list ~sep:Pp.comma
        (fun ctx (index, color) ->
          Pp.string ctx (Int.to_string index);
          let rendered =
            Pp.to_string ~minify:(Pp.minified ctx) ~lossless:ctx.Pp.lossless
              Values.pp_color color
          in
          if (not (Pp.minified ctx)) || rendered = "" || rendered.[0] <> '#'
          then Pp.space ctx ();
          Pp.string ctx rendered)
        ctx entries

let pp_font_feature_value ctx (name, indexes) =
  Pp.string ctx (Parser.escape_ident name);
  Pp.char ctx ':';
  Pp.space_if_pretty ctx ();
  Pp.list ~sep:Pp.space Pp.int ctx indexes

let pp_font_feature_values_block ctx (name, values) =
  Pp.char ctx '@';
  Pp.string ctx name;
  Pp.sp ctx ();
  Pp.braced_semicolon_list pp_font_feature_value ctx values

let pp_view_transition_descriptor : view_transition_descriptor Pp.t =
 fun ctx -> function
  | Navigation `Auto -> Pp.string ctx "navigation:auto"
  | Navigation `None -> Pp.string ctx "navigation:none"
  | Types None -> Pp.string ctx "types:none"
  | Types (Some types) ->
      Pp.string ctx "types:";
      Pp.space_if_pretty ctx ();
      Pp.list ~sep:Pp.space Pp.string ctx types

(* Under minify a [Declarations] statement carries no trailing [;] of its own,
   so whoever sequences the statements owes it a separator: CSS Syntax 3 (ED)
   sec. 5.5.5 runs a declaration to the next [;] or to the block's [}], and a
   sibling that follows would otherwise be read as part of its value. *)
let pp_declarations_sep ctx = function
  | Declarations decls when decls <> [] && Pp.minified ctx ->
      Pp.semicolon ctx ()
  | _ -> ()

let rec pp_rule : rule Pp.t =
 fun ctx rule ->
  Selector.pp ctx rule.selector;
  Pp.sp ctx ();
  Pp.block_open ctx ();
  let decls = rule.declarations in
  (match (decls, rule.nested) with
  | [], [] -> ()
  | decls, nested ->
      let ctx = Pp.enter_style_rule { ctx with level = ctx.level + 1 } in
      let pp_declarations ctx () =
        Pp.list ~sep:Pp.semicolon_cut (Pp.indent Declaration.pp) ctx decls
      in
      let pp_nested ctx () =
        let rec loop = function
          | [] -> ()
          | [ stmt ] -> Pp.indent pp_statement ctx stmt
          | stmt :: rest ->
              Pp.indent pp_statement ctx stmt;
              pp_declarations_sep ctx stmt;
              Pp.cut ctx ();
              loop rest
        in
        loop nested
      in
      Pp.cut ctx ();
      (match (decls, nested) with
      | [], _ -> pp_nested ctx ()
      | _, [] ->
          pp_declarations ctx ();
          (* Trailing semicolon after last declaration to match Tailwind *)
          if not ctx.minify then Pp.semicolon ctx ()
      | _, _ ->
          pp_declarations ctx ();
          Pp.semicolon ctx ();
          Pp.cut ctx ();
          pp_nested ctx ());
      if not ctx.minify then (
        Pp.cut ctx ();
        match ctx.indent with
        | Some w when ctx.level > 1 ->
            Pp.string ctx (String.make (w * (ctx.level - 1)) ' ')
        | _ -> ()));
  Pp.block_close ctx ()

and pp_layer_statement ctx name content =
  Pp.string ctx "@layer";
  (match name with
  | Some n ->
      Pp.string ctx " ";
      pp_layer_name ctx n
  | None -> ());
  (* For empty layers: use statement form when minifying (more concise), but
     preserve block form otherwise for roundtrip fidelity. A style rule accepts
     no layer-order declaration, so inside one the block form is the only
     spelling that survives a re-read. *)
  if content = [] && Pp.minified ctx && not (Pp.in_style_rule ctx) then
    Pp.semicolon ctx ()
  else (
    Pp.sp ctx ();
    Pp.braces pp_block ctx content)

and pp_media_statement ctx condition content =
  Pp.string ctx "@media";
  (match condition with
  | Media.List [] -> ()
  | _ ->
      (* Per cascade's minify policy (README sec. "Minify policy"), elide
         whitespace at safe token boundaries: a leading [(] needs no space.
         Idents like [screen] still need it. *)
      let rendered =
        Pp.to_string ~minify:ctx.Pp.minify ~lossless:ctx.Pp.lossless Media.pp
          condition
      in
      if Pp.minified ctx && String.length rendered > 0 && rendered.[0] = '('
      then ()
      else Pp.string ctx " ";
      Pp.string ctx rendered);
  Pp.sp ctx ();
  Pp.braces pp_block ctx content

and pp_container_statement ctx name condition content =
  Pp.string ctx "@container";
  (match name with
  | Some n ->
      Pp.char ctx ' ';
      Pp.string ctx (Parser.escape_ident n)
  | None -> ());
  (match condition with
  | Some condition ->
      let condition_str =
        Container.to_stylesheet_string ~minify:ctx.Pp.minify condition
      in
      if condition_str <> "" then (
        (* Per cascade's minify policy: elide whitespace at safe token
           boundaries. A leading [(] needs no space; a name component (after
           [@container <name>] or a non-paren condition like [not (...)] /
           [style(...)]) does. *)
        if Pp.minified ctx && name = None && condition_str.[0] = '(' then ()
        else Pp.char ctx ' ';
        Pp.string ctx condition_str)
  | None -> ());
  Pp.sp ctx ();
  Pp.braces pp_block ctx content

and pp_supports_statement ctx condition content =
  (* Per cascade's minify policy (README sec. "Minify policy"): elide whitespace
     at safe token boundaries. The space after [@supports] is droppable only
     when the condition opens with [(] - any other start ([not],
     [selector(...)], etc.) needs the space to keep token shape. *)
  Pp.string ctx "@supports";
  let rendered =
    Pp.to_string ~minify:ctx.Pp.minify ~lossless:ctx.Pp.lossless
      pp_supports_condition_value condition
  in
  if Pp.minified ctx && String.length rendered > 0 && rendered.[0] = '(' then ()
  else Pp.string ctx " ";
  Pp.string ctx rendered;
  Pp.sp ctx ();
  Pp.braces pp_block ctx content

and pp_else_statement ctx (condition : conditional option) content =
  Pp.string ctx "@else";
  (match condition with
  | None -> ()
  | Some c ->
      Pp.string ctx " ";
      pp_conditional ctx c);
  Pp.sp ctx ();
  Pp.braces pp_block ctx content

and pp_scope_statement ctx start end_ content =
  Pp.string ctx "@scope";
  (match start with
  | Some s ->
      Pp.space_if_pretty ctx ();
      Pp.string ctx "(";
      pp_scope_selector ctx s;
      Pp.string ctx ")"
  | None -> ());
  (match end_ with
  | Some e ->
      Pp.string ctx (if Pp.minified ctx then "to (" else " to (");
      pp_scope_selector ctx e;
      Pp.string ctx ")"
  | None -> ());
  Pp.sp ctx ();
  Pp.braces pp_block ctx content

and pp_statement : statement Pp.t =
 fun ctx -> function
  | Rule rule -> pp_rule ctx rule
  | Declarations raw_decls -> pp_declarations_statement ctx raw_decls
  | Bang_comment body ->
      Pp.string ctx "/*";
      Pp.string ctx body;
      Pp.string ctx "*/"
  | Charset encoding ->
      Pp.string ctx "@charset \"";
      Pp.string ctx encoding;
      Pp.string ctx "\";"
  | Import { url; layer; supports; media } ->
      Pp.string ctx "@import";
      pp_import_components ctx { url; layer; supports; media };
      Pp.semicolon ctx ()
  | Namespace (prefix, uri) -> pp_namespace_statement ctx prefix uri
  | Property r -> pp_property_rule ctx r
  | Layer_decl names ->
      Pp.string ctx "@layer ";
      Pp.list ~sep:Pp.comma pp_layer_name ctx names;
      Pp.semicolon ctx ()
  | Layer (name, content) -> pp_layer_statement ctx name content
  | Media (condition, content) -> pp_media_statement ctx condition content
  | Container (name, condition, content) ->
      pp_container_statement ctx name condition content
  | Supports (condition, content) -> pp_supports_statement ctx condition content
  | Moz_document (conditions, content) ->
      Pp.string ctx "@-moz-document ";
      Pp.list ~sep:Pp.comma pp_moz_document_condition ctx conditions;
      Pp.sp ctx ();
      Pp.braces pp_block ctx content
  | Starting_style content ->
      Pp.string ctx "@starting-style";
      Pp.braces pp_block ctx content
  | When (condition, content) ->
      Pp.string ctx "@when ";
      pp_conditional ctx condition;
      Pp.sp ctx ();
      Pp.braces pp_block ctx content
  | Else (condition, content) -> pp_else_statement ctx condition content
  | Supports_condition (name, declarations) ->
      Pp.string ctx "@supports-condition ";
      Pp.string ctx name;
      Pp.sp ctx ();
      Pp.braced_semicolon_list Declaration.pp ctx declarations
  | Origin (_, content) -> pp_block ctx content
  | Scope (start, end_, content) -> pp_scope_statement ctx start end_ content
  | Keyframes (name, frames) ->
      pp_keyframes_block_statement ctx "@keyframes " name frames
  | Webkit_keyframes (name, frames) ->
      pp_keyframes_block_statement ctx "@-webkit-keyframes " name frames
  | Moz_keyframes (name, frames) ->
      pp_keyframes_block_statement ctx "@-moz-keyframes " name frames
  | Font_face descriptors ->
      Pp.string ctx "@font-face";
      Pp.sp ctx ();
      Pp.braced_semicolon_list pp_font_face_descriptor ctx descriptors
  | Counter_style (name, descriptors) ->
      Pp.string ctx "@counter-style ";
      Pp.string ctx (Parser.escape_ident name);
      Pp.sp ctx ();
      Pp.braced_semicolon_list pp_counter_style_descriptor ctx descriptors
  | Page (selector, raw_declarations) ->
      Pp.string ctx "@page";
      pp_page_selector ctx selector;
      Pp.sp ctx ();
      Pp.braced_semicolon_list Declaration.pp ctx raw_declarations
  | Page_with_margins (selector, descriptors, margins) ->
      Pp.string ctx "@page";
      pp_page_selector ctx selector;
      Pp.sp ctx ();
      pp_page_with_margins_body ctx descriptors margins
  | Font_palette_values (name, descriptors) ->
      Pp.string ctx "@font-palette-values ";
      Pp.string ctx (Parser.escape_ident name);
      Pp.sp ctx ();
      Pp.braced_semicolon_list pp_font_palette_descriptor ctx descriptors
  | Font_feature_values (families, blocks) ->
      Pp.string ctx "@font-feature-values ";
      Pp.list ~sep:Pp.comma Properties.pp_font_family ctx families;
      Pp.sp ctx ();
      Pp.braced_list
        ~sep:Pp.(cut ++ cut)
        pp_font_feature_values_block ctx blocks
  | View_transition descriptors ->
      Pp.string ctx "@view-transition";
      Pp.sp ctx ();
      Pp.braced_semicolon_list pp_view_transition_descriptor ctx descriptors
  | Position_try (name, declarations) ->
      Pp.string ctx "@position-try ";
      Pp.string ctx (Parser.escape_ident name);
      Pp.sp ctx ();
      Pp.braced_semicolon_list Declaration.pp ctx declarations
  | Viewport (prefix, descriptors) ->
      pp_viewport_statement ctx prefix descriptors
  | Unknown_at_rule { name; prelude; block } ->
      pp_unknown_at_rule_statement ctx name prelude block

and pp_block : block Pp.t =
 fun ctx statements ->
  let statements = printable_statements ctx statements in
  (* Block printing for at-rules (@media, @supports, etc.) The braces helper
     adds nest 1 and indent for the first item only. Subsequent items need
     explicit indentation and blank line separation to match Tailwind format. *)
  match statements with
  | [] -> ()
  | [ s ] -> pp_statement ctx s
  | s :: rest ->
      pp_statement ctx s;
      pp_declarations_sep ctx s;
      let rec pp_rest = function
        | [] -> ()
        | [ stmt ] ->
            Pp.cut ctx ();
            if not ctx.Pp.minify then Pp.cut ctx ();
            Pp.indent pp_statement ctx stmt
        | stmt :: rest ->
            Pp.cut ctx ();
            if not ctx.Pp.minify then Pp.cut ctx ();
            Pp.indent pp_statement ctx stmt;
            pp_declarations_sep ctx stmt;
            pp_rest rest
      in
      pp_rest rest

let is_layer_block = function Layer _ -> true | _ -> false

let pp_stylesheet : stylesheet Pp.t =
 fun ctx statements ->
  let statements =
    if Pp.minified ctx then normalise statements else statements
  in
  let statements = printable_statements ctx statements in
  let rec loop = function
    | [] -> ()
    | [ s ] -> pp_statement ctx s
    | s :: (next :: _ as rest) ->
        pp_statement ctx s;
        pp_declarations_sep ctx s;
        Pp.cut ctx ();
        (* Add blank line between consecutive @layer { } blocks *)
        if (not (Pp.minified ctx)) && is_layer_block s && is_layer_block next
        then Pp.cut ctx ();
        loop rest
  in
  loop statements

let pp = pp_stylesheet

(** {1 Rendering} *)

(* One walk of the tree. Presizing the buffer from a [Pp.size] prepass would
   cost a second full walk - the counter sink skips the output bytes, not
   [normalise], [printable_statements] or any printer below them - to save a
   [Buffer] growth that is amortised anyway. *)
let to_string ?(minify = false) ?indent ?lossless ?enforce_spec (statements : t)
    =
  let pp ctx () = pp_stylesheet ctx statements in
  Pp.to_string ~minify ?indent ?lossless ?enforce_spec pp ()

(** {1 Legacy Compatibility} *)

(* Helper functions to extract specific elements from a stylesheet *)
let rec extract_rules = function
  | [] -> []
  | Rule r :: rest -> r :: extract_rules rest
  | _ :: rest -> extract_rules rest

(* Every rule below a brace, not the ones that happen to sit directly under it:
   a rule nested in another rule and a rule inside an inner group are both under
   the query, and a walk that stopped at the direct children would report the
   query with a body it does not have. A nested rule keeps the relative selector
   it was written with. *)
let rules_below block =
  List.rev
    (fold_statements
       (fun acc stmt -> match stmt with Rule r -> r :: acc | _ -> acc)
       [] block)

(* CSS Cascade 5 sec. 6.4.2: a dotted layer name [foo.bar] is shorthand for the
   nested [@layer foo { @layer bar { ... } }]. Walk the sheet once through
   [statement_children], expanding dotted names and prefixing each name with its
   parent's path, so a layer is reachable under one canonical name whatever the
   input shape and an [@layer] inside a conditional group counts like any other:
   the group decides whether its contents apply, not whether the layer exists.
   Each entry carries the block the name opens, or [None] for a name declared by
   a layer statement that opens none, so a statement never shadows the block
   that fills the layer in. A sublayer of an anonymous layer has no name a
   caller could ask for, so the walk stops there. *)
let layer_declarations sheet =
  (* A [<layer-name>] is already its idents (CSS Cascade 5 sec. 6.4.1), so a
     path extends by appending them: a [.] one ident carries never splits it. *)
  let rec emit_parts parent segments (inner : block option) acc =
    match segments with
    | [] -> acc
    | [ leaf ] -> (
        let qualified = parent @ [ leaf ] in
        let acc = (qualified, inner) :: acc in
        match inner with None -> acc | Some block -> walk qualified acc block)
    | head :: tail ->
        (* An intermediate segment names a layer that exists but opens no block
           of its own. *)
        let qualified = parent @ [ head ] in
        let stub = Option.map (fun _ -> []) inner in
        emit_parts qualified tail inner ((qualified, stub) :: acc)
  and walk parent acc statements =
    List.fold_left
      (fun acc s ->
        match s with
        | Layer (Some name, inner) -> emit_parts parent name (Some inner) acc
        | Layer_decl names ->
            List.fold_left
              (fun acc name -> emit_parts parent name None acc)
              acc names
        | Layer (None, _) -> acc
        | s -> walk parent acc (statement_children s))
      acc statements
  in
  List.rev (walk [] [] sheet)

let layer_block name sheet =
  List.find_map
    (fun (declared, block) ->
      if equal_layer_name declared name then block else None)
    (layer_declarations sheet)

let layers sheet =
  let seen = Hashtbl.create 16 in
  List.filter_map
    (fun (name, _) ->
      if Hashtbl.mem seen name then None
      else (
        Hashtbl.add seen name ();
        Some name))
    (layer_declarations sheet)

(* A group at-rule above the query is not a reason to report nothing: it decides
   whether its contents apply, not whether the query exists. *)
let queries_of pick block =
  List.rev
    (fold_statements
       (fun acc stmt -> match pick stmt with Some q -> q :: acc | None -> acc)
       [] block)

(* Legacy compatibility functions *)
let rules t = extract_rules t

let media_queries t =
  queries_of
    (function
      | Media (condition, block) -> Some (condition, rules_below block)
      | _ -> None)
    t

let container_queries t =
  queries_of
    (function
      | Container (name, condition, block) ->
          Some (name, condition, rules_below block)
      | _ -> None)
    t

(** {1 Reading/Parsing} *)

let read_keyframe (r : Cursor.t) : keyframe =
  Cursor.ws r;
  let selector_str = Cursor.drain_until_block_as_string ~trim:true r in
  let selector =
    try Keyframe.selector_of_string selector_str
    with Invalid_argument msg -> Cursor.err_invalid r msg
  in
  let declarations =
    Cursor.braces
      (fun inner ->
        Declaration.read_declarations inner
        |> List.filter (fun decl -> not (Declaration.is_important decl)))
      r
  in
  { selector; declarations }

(* Helper functions for reading specific at-rules *)

(* CSS Syntax 3 (ED) sec. 8.3 reserves [@charset "UTF-8";] as the byte-stream
   decoder hint. The exact form -- uppercase label, double quotes, semicolon --
   is recognised; any other [@charset] (lowercase, single quotes, different
   label, no terminating ';') is a syntax error. *)
let read_charset (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "charset" r;
  Cursor.ws r;
  let encoding =
    match Cursor.string_with_quote_opt r with
    | Some (value, '"') when value = "UTF-8" -> value
    | _ ->
        Error.fail_bad_value (Cursor.position r) ~property:"@charset"
          ~reason:"@charset only recognises \"UTF-8\""
  in
  Cursor.ws r;
  if Cursor.peek_semicolon r then Cursor.skip r
  else
    Error.fail_bad_value (Cursor.position r) ~property:"@charset"
      ~reason:"@charset must end with ';'";
  Charset encoding

(* CSS Cascade section 6.4.2: a layer name is one or more idents joined by '.'
   with no whitespace around the dot. CSS-wide keywords are reserved. *)
let read_layer_name_component (r : Cursor.t) : layer_name =
  let reserved = function
    | "initial" | "inherit" | "unset" | "revert" | "revert-layer" -> true
    | _ -> false
  in
  let part p =
    if reserved (String.lowercase_ascii p) then
      Cursor.err_invalid r ("layer name reserves CSS-wide keyword: " ^ p);
    p
  in
  let first = part (Cursor.ident ~keep_case:true r) in
  (* Only a [.] delimiter separates two idents. One an ident carries arrives as
     part of that ident's own token, so it never reaches here. *)
  let rec extend acc =
    match Cursor.peek_raw r with
    | Some (Component.Preserved { kind = Token.Delim "."; _ }) ->
        Cursor.skip r;
        let next =
          match Cursor.peek_raw r with
          | Some (Component.Preserved { kind = Token.Ident s; _ }) ->
              Cursor.skip r;
              s
          | _ -> Cursor.err_expected r "ident after '.' in layer name"
        in
        extend (part next :: acc)
    | _ -> List.rev acc
  in
  extend [ first ]

(* CSS Cascade 5 sec. 2 [@import] prelude: [<url> [layer | layer(<layer-name>)]?
   [supports(<supports-condition>)]? <media-query-list>?]. The [~keep_url_repr]
   flavour preserves the original quote / [url(...)] form in [url] so the
   at-rule dispatch can round-trip author input verbatim; the canonical flavour
   ([keep_url_repr=false]) stores the decoded string for the top-level
   [read_import_rule] reader. *)
let read_import_url ~keep_url_repr (r : Cursor.t) =
  let pos = Cursor.position r in
  let value = Cursor.one_of [ Cursor.url; Cursor.string ] r in
  if keep_url_repr then
    let end_pos = Cursor.position r in
    match Cursor.source r with
    | Some source
      when end_pos.start_pos <= String.length source
           && pos.start_pos <= end_pos.start_pos
           && pos.start_pos >= 0 ->
        String.sub source pos.start_pos (end_pos.start_pos - pos.start_pos)
        |> String.trim
    | _ -> value
  else value

let read_import_layer (r : Cursor.t) =
  match
    Cursor.function_call "layer"
      (fun inner ->
        Cursor.ws inner;
        if Cursor.is_done inner then []
        else
          let name = read_layer_name_component inner in
          Cursor.ws inner;
          Cursor.expect_eof inner;
          name)
      r
  with
  | Some _ as some -> some
  | None -> (
      match Cursor.peek_ident r with
      | Some s when String.lowercase_ascii s = "layer" ->
          let _ = Cursor.ident r in
          Some []
      | _ -> None)

let read_import_supports (r : Cursor.t) =
  Cursor.function_call "supports"
    (fun inner -> Supports.read ~allow_unwrapped_decl:true inner)
    r

(* In the [@import] prelude [layer(...)] and [supports(...)] are structural, and
   the grammar allows each once, before the media query list. Reaching the media
   position still holding one means a duplicate or a misordered prelude. They
   are function tokens, so the media grammar would otherwise take them as a
   [<general-enclosed>] query and quietly accept the rule. *)
let starts_with_import_keyword components =
  let cursor = Cursor.of_components components in
  match Cursor.peek cursor with
  | Some (Component.Func { node = { name; _ }; _ }) ->
      let name = String.lowercase_ascii name in
      String.equal name "layer" || String.equal name "supports"
  | Some (Component.Preserved { kind = Token.Ident name; _ }) ->
      String.equal (String.lowercase_ascii name) "layer"
  | Some (Component.Block _ | Component.Preserved _) | None -> false

let drain_until_semicolon r =
  let rec loop acc =
    match Cursor.peek_raw r with
    | None | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
        List.rev acc
    | Some component ->
        ignore (Cursor.next_raw r);
        loop (component :: acc)
  in
  loop []

let read_import_media (r : Cursor.t) : Media.t option =
  if Cursor.peek_semicolon r || Cursor.is_done r then None
  else
    let loc = Cursor.position r in
    let components = drain_until_semicolon r in
    if starts_with_import_keyword components then
      Error.fail_bad_condition loc ~at_rule:"@media"
        ~reason:"layer()/supports() must precede the media query, and once only"
    else Some (Media.read ~recover:false (Cursor.sub r components))

let read_import_prelude ~keep_url_repr (r : Cursor.t) : import_rule =
  Cursor.expect_at_keyword "import" r;
  Cursor.ws r;
  let url = read_import_url ~keep_url_repr r in
  Cursor.ws r;
  let layer = read_import_layer r in
  Cursor.ws r;
  let supports = read_import_supports r in
  Cursor.ws r;
  let media = read_import_media r in
  if Cursor.peek_semicolon r then Cursor.skip r;
  { url; layer; supports; media }

let read_import (r : Cursor.t) : statement =
  Import (read_import_prelude ~keep_url_repr:true r)

let read_namespace (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "namespace" r;
  Cursor.ws r;
  (* Optional prefix ident; absent when the URL comes first. *)
  let prefix =
    match Cursor.peek r with
    | Some (Component.Preserved { kind = Token.Ident _; _ }) ->
        Some (Cursor.ident ~keep_case:true r)
    | _ -> None
  in
  Cursor.ws r;
  let uri =
    match Cursor.peek r with
    | Some (Component.Preserved { kind = Token.String { value; _ }; _ }) ->
        Cursor.skip r;
        Quoted value
    | _ ->
        let pos = Cursor.position r in
        let value = Cursor.url r in
        let end_pos = Cursor.position r in
        let form =
          match Cursor.source r with
          | Some source
            when end_pos.start_pos <= String.length source
                 && pos.start_pos <= end_pos.start_pos
                 && pos.start_pos >= 0 ->
              let raw =
                String.sub source pos.start_pos
                  (end_pos.start_pos - pos.start_pos)
              in
              if String.contains raw '\'' then (Quoted '\'' : url_form)
              else if String.contains raw '"' then (Quoted '"' : url_form)
              else (Bare : url_form)
          | _ -> (Bare : url_form)
        in
        Url (value, form)
  in
  Cursor.ws r;
  if Cursor.peek_semicolon r then Cursor.skip r;
  Namespace (prefix, uri)

(* Discard the rule the cursor sits on, stopping at its [{}] block or at a
   top-level [;]: CSS Syntax 3 (ED) sec. 5.5.2 ends an at-rule at whichever
   comes first, and sec. 5.5.3 ends a qualified rule at its block. Consuming
   exactly that far leaves what was written after the rule - the declarations
   around it, or the next rule - to be read on its own. A [(] or a [[] met
   before the block is a component value of the prelude being discarded, so
   stopping there would offer the tail of that prelude as a rule of its own. *)
let rec skip_past_rule r =
  match Cursor.next_raw r with
  | None -> ()
  | Some (Component.Block { node = { opening = Token.Curly; _ }; _ })
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
      ()
  | Some _ -> skip_past_rule r

(* Discard the item the cursor sits on. A body that holds declarations and
   at-rules alike has to pick by what the item starts with: the two ends differ,
   and taking an at-rule for a declaration would eat the item written after it.
   The caller rewinds first, so a reader that already consumed part of the item
   is skipped from its start. *)
let skip_invalid_item r =
  match Cursor.peek r with
  | Some (Component.Preserved { kind = Token.At_keyword _; _ }) ->
      skip_past_rule r
  | _ -> Cursor.skip_past_semicolon r

(* Read a body one item at a time, [step] committing each item to the
   accumulator before the next is read, so an item dropped in recovery costs
   only itself and the items around it are kept. CSS Syntax 3 (ED) sec. 5.5.5
   keeps what a block's contents already yielded when one item fails to parse,
   and CSS Paged Media 3 sec. 4.1 says as much of a page or a margin context in
   so many words: "valid declarations within the block are applied". Strict mode
   ([not (Cursor.recover r)]) still raises, so [~strict:true] rejects exactly
   what the lenient parse warns about. [skip] discards the item that failed, and
   which one it is depends on the body: {!skip_invalid_item} for a body of
   declarations, {!skip_past_rule} for a body of rules, where an item that opens
   a block ends at that block rather than at a [;] it does not have. [construct]
   names the same two bodies: the dropped item is a declaration in the first and
   a rule in the second. The warning is pushed after the skip, which is what
   fixes how far the loss reaches: the item is dropped from [start] to wherever
   [skip] leaves the cursor. *)
let read_items_with_recovery ~skip ~construct step r init =
  let rec loop state =
    if Cursor.recover r then recovering state else continue (step r state)
  and recovering state =
    let start = Cursor.save r in
    match step r state with
    | committed -> continue committed
    | exception Error.Parse_error e ->
        Cursor.restore r start;
        skip r;
        Cursor.push_warning r
          ~recovery:(Cursor.dropped_since r start construct)
          e;
        loop state
  and continue = function
    | `Done result -> result
    | `More state -> loop state
  in
  loop init

(* A selector that is no keyframe selector - the [to] of [entry to], which is no
   [<length-percentage>] - drops its rule and the block keeps parsing. *)
let read_keyframe_or_skip inner acc =
  let snap = Cursor.save inner in
  match read_keyframe inner with
  | frame -> frame :: acc
  | exception Cursor.Parse_error e ->
      Cursor.restore inner snap;
      Cursor.push_warning inner ~recovery:Error.Recovery.(dropped Rule) e;
      skip_past_rule inner;
      acc

(* One item of the body: a keyframe rule, or the end of it. CSS Animations 1
   sec. 3 fills a [@keyframes] body with keyframe rules, so an at-rule has no
   place there; rejecting it here rather than around the loop leaves the
   rejection inside the recovery the loop runs, which costs the at-rule alone
   instead of the animation and the sheet holding it. *)
let read_keyframes_step inner acc =
  Cursor.ws inner;
  if Cursor.is_done inner then `Done (List.rev acc)
  else
    match Cursor.peek inner with
    | Some (Component.Preserved { kind = Token.At_keyword name; _ }) ->
        Cursor.err_invalid inner ("@keyframes nested @" ^ name)
    | _ -> `More (read_keyframe_or_skip inner acc)

let read_keyframes_block inner =
  read_items_with_recovery ~skip:skip_past_rule ~construct:Error.Recovery.Rule
    read_keyframes_step inner []

(* CSS Animations 1 sec. 3: [@keyframes <keyframes-name>], [<keyframes-name> =
   <custom-ident> | <string>]. The reserved spellings ([none], CSS-wide
   keywords, [default]) are excluded from [<custom-ident>], but every mainstream
   minifier accepts them as [<string>], so cascade keeps them too rather than
   leak input that downstream tools preserve verbatim. *)
let read_keyframes_name r =
  Cursor.ws r;
  match Cursor.string_opt r with
  | Some s -> s
  | None -> Cursor.ident ~keep_case:true r

let read_keyframes_named at_keyword make_statement (r : Cursor.t) : statement =
  Cursor.with_context r ("@" ^ at_keyword) @@ fun () ->
  Cursor.expect_at_keyword at_keyword r;
  let name = read_keyframes_name r in
  Cursor.ws r;
  let frames = Cursor.braces read_keyframes_block r in
  make_statement name frames

let read_keyframes (r : Cursor.t) : statement =
  read_keyframes_named "keyframes"
    (fun name frames -> Keyframes (name, frames))
    r

let read_webkit_keyframes r =
  read_keyframes_named "-webkit-keyframes"
    (fun name frames -> Webkit_keyframes (name, frames))
    r

let read_moz_keyframes r =
  read_keyframes_named "-moz-keyframes"
    (fun name frames -> Moz_keyframes (name, frames))
    r

(* [url-prefix()] with no argument is the prelude that matches every document,
   so an empty argument list and an empty string both read as [None]. *)
let read_moz_url_prefix arg_cursor =
  match Cursor.string_opt arg_cursor with
  | Some "" -> Option.None
  | Some prefix -> Some prefix
  | None ->
      Cursor.ws arg_cursor;
      if Cursor.is_done arg_cursor then Option.None
      else Cursor.err_expected arg_cursor "url-prefix string argument"

let read_moz_document_function r name arguments : moz_document_condition =
  let arg_cursor = Cursor.of_components arguments in
  Cursor.ws arg_cursor;
  let finish condition =
    Cursor.ws arg_cursor;
    Cursor.expect_eof arg_cursor;
    condition
  in
  match name with
  | "url" -> finish (Url_exact (Cursor.string arg_cursor))
  | "url-prefix" -> finish (Url_prefix (read_moz_url_prefix arg_cursor))
  | "domain" -> finish (Domain (Cursor.string arg_cursor))
  | "media-document" -> finish (Media_document (Cursor.string arg_cursor))
  | "regexp" -> finish (Regexp (Cursor.string arg_cursor))
  | _ -> Cursor.err_expected r "a @-moz-document URL-matching function"

(* Gecko's [@document] prelude takes [<url>], [url-prefix(<string>)],
   [domain(<string>)], [media-document(<string>)] and [regexp(<string>)]. A
   prelude function outside that list has no grammar, so the at-rule goes down
   with it, which is what CSS Syntax 3 (ED) sec. 5.5.2 does with any prelude no
   grammar accepts. *)
let read_moz_document_condition r : moz_document_condition =
  Cursor.ws r;
  (* [url(x)] lexes as a [<url-token>] and [url("x")] as a function, so the
     [<url>] form is read before the function arms. *)
  match Cursor.url_opt r with
  | Some url -> Url_exact url
  | None -> (
      match Cursor.next r with
      | Some (Component.Func { node = { name; arguments; _ }; _ }) ->
          read_moz_document_function r name arguments
      | _ -> Cursor.err_expected r "a @-moz-document URL-matching function")

(* Read a font-face descriptor *)
(* Helper to read descriptor value after colon *)
let read_descriptor_value read_fn constructor r =
  Cursor.ws r;
  if not (Cursor.colon r) then Cursor.err_expected r "':'";
  Cursor.ws r;
  constructor (read_fn r)

(* One item of a descriptor body: a descriptor, a stray [;] that CSS Syntax 3
   (ED) sec. 5.5.5 discards with no declaration to validate, or the end of the
   body. [Declaration.read_declaration] answers [None] for an item that opens a
   block instead; a descriptor body holds no rules, so that item is invalid and
   is reported rather than left for the caller to loop on. *)
let read_descriptor_item (r : Cursor.t) =
  Cursor.ws r;
  if Cursor.is_done r then `Done
  else if Cursor.peek_semicolon r then (
    Cursor.skip r;
    `Skip)
  else
    match Declaration.read_declaration r with
    | Some desc -> `Descriptor desc
    | None -> Cursor.err_expected r "descriptor"

(* CSS Cascade 5 sec. 6.2 ranks an important author declaration above a normal
   one whatever their order, and sec. 6.4 breaks a tie between two of the same
   importance by order of appearance. A descriptor read later therefore takes
   the slot one already read holds, unless the one held is important and it is
   not: then the later declaration is the one that is dropped, and the survivor
   keeps the place it was written in. An opaque descriptor is the conservative
   exception: Cascade cannot know whether a browser accepts it, so neither it
   nor a same-name typed descriptor can erase the other. *)
let rec descriptor_has_opaque_value = function
  | Declaration.Declaration { property = Properties.Unknown_property _; _ } ->
      true
  | Declaration.Declaration _ -> false
  | Declaration.Theme_guarded { decl; _ } -> descriptor_has_opaque_value decl

let replace_descriptor desc acc =
  let same_slot held =
    (not (descriptor_has_opaque_value held || descriptor_has_opaque_value desc))
    && String.equal
         (Declaration.property_name held)
         (Declaration.property_name desc)
  in
  match List.find_opt same_slot acc with
  | Some held
    when Declaration.is_important held && not (Declaration.is_important desc) ->
      acc
  | Some _ -> desc :: List.filter (fun held -> not (same_slot held)) acc
  | None -> desc :: acc

let read_descriptor_step normalize inner acc =
  match read_descriptor_item inner with
  | `Done -> `Done (List.rev acc)
  | `Skip -> `More acc
  | `Descriptor desc -> `More (normalize desc acc)

let read_descriptor_block normalize inner =
  read_items_with_recovery ~skip:skip_invalid_item
    ~construct:Error.Recovery.Declaration
    (read_descriptor_step normalize)
    inner []

(* CSS Fonts 4 sec. 4.4: a descriptor range whose startpoint is larger than its
   endpoint is well defined, the user agent swapping the two endpoints for font
   matching. The swap is on the computed value, so the descriptor keeps the
   order it was written in. *)
let read_font_weight_descriptor r =
  read_descriptor_value Declaration.read_property_value
    (fun value ->
      let c = Cursor.of_string value in
      let first = Properties.read_font_weight c in
      Cursor.ws c;
      if Cursor.is_done c then Font_weight first
      else
        let second = Properties.read_font_weight c in
        Cursor.ws c;
        Cursor.expect_eof c;
        Font_weight_range (first, second))
    r

let read_font_style_descriptor r =
  read_descriptor_value Declaration.read_property_value
    (fun value ->
      let c = Cursor.of_string value in
      let first = Properties.read_font_style c in
      Cursor.ws c;
      if Cursor.is_done c then Font_style first
      else
        let second = Properties.read_font_style c in
        Cursor.ws c;
        Cursor.expect_eof c;
        Font_style_range (first, second))
    r

let validate_nonempty_descriptor r name value =
  if String.trim value = "" then
    Cursor.err_invalid r ("empty " ^ name ^ " descriptor")

let read_font_family_descriptor r =
  read_descriptor_value
    (fun r ->
      let family = Properties.read_font_family_name r in
      Cursor.ws r;
      if not (Cursor.is_done r || Cursor.peek_semicolon r) then
        Cursor.err_invalid r "trailing tokens after @font-face font-family";
      [ family ])
    (fun v -> Font_family v)
    r

let read_font_stretch_descriptor r =
  read_descriptor_value Declaration.read_property_value
    (fun value ->
      let c = Cursor.of_string value in
      let first = Properties.read_font_stretch c in
      Cursor.ws c;
      if Cursor.is_done c then Font_stretch first
      else
        let second = Properties.read_font_stretch c in
        Cursor.ws c;
        Cursor.expect_eof c;
        Font_stretch_range (first, second))
    r

let read_unicode_range_descriptor r =
  read_descriptor_value
    (fun cur ->
      Cursor.list ~at_least:1 ~sep:Cursor.comma Properties.read_unicode_range
        cur)
    (fun vs -> Unicode_range vs)
    r

let font_variant_desc_value = function
  | "common-ligatures" -> Some (Ligature Properties.Common_ligatures)
  | "no-common-ligatures" -> Some (Ligature Properties.No_common_ligatures)
  | "discretionary-ligatures" ->
      Some (Ligature Properties.Discretionary_ligatures)
  | "no-discretionary-ligatures" ->
      Some (Ligature Properties.No_discretionary_ligatures)
  | "historical-ligatures" -> Some (Ligature Properties.Historical_ligatures)
  | "no-historical-ligatures" ->
      Some (Ligature Properties.No_historical_ligatures)
  | "contextual" -> Some (Ligature Properties.Contextual)
  | "no-contextual" -> Some (Ligature Properties.No_contextual)
  | "small-caps" -> Some (Caps Properties.Small_caps)
  | "all-small-caps" -> Some (Caps Properties.All_small_caps)
  | "petite-caps" -> Some (Caps Properties.Petite_caps)
  | "all-petite-caps" -> Some (Caps Properties.All_petite_caps)
  | "unicase" -> Some (Caps Properties.Unicase)
  | "titling-caps" -> Some (Caps Properties.Titling_caps)
  | "lining-nums" -> Some (Numeric Properties.Lining_nums)
  | "oldstyle-nums" -> Some (Numeric Properties.Oldstyle_nums)
  | "proportional-nums" -> Some (Numeric Properties.Proportional_nums)
  | "tabular-nums" -> Some (Numeric Properties.Tabular_nums)
  | "diagonal-fractions" -> Some (Numeric Properties.Diagonal_fractions)
  | "stacked-fractions" -> Some (Numeric Properties.Stacked_fractions)
  | "ordinal" -> Some (Numeric Properties.Ordinal)
  | "slashed-zero" -> Some (Numeric Properties.Slashed_zero)
  | "jis78" -> Some (East_asian Properties.Jis78)
  | "jis83" -> Some (East_asian Properties.Jis83)
  | "jis90" -> Some (East_asian Properties.Jis90)
  | "jis04" -> Some (East_asian Properties.Jis04)
  | "simplified" -> Some (East_asian Properties.Simplified)
  | "traditional" -> Some (East_asian Properties.Traditional)
  | "full-width" -> Some (East_asian Properties.Full_width)
  | "proportional-width" -> Some (East_asian Properties.Proportional_width)
  | "ruby" -> Some (East_asian Properties.Ruby)
  | _ -> None

let font_variant_ligature_slot = function
  | Properties.Common_ligatures | Properties.No_common_ligatures -> `Common
  | Properties.Discretionary_ligatures | Properties.No_discretionary_ligatures
    ->
      `Discretionary
  | Properties.Historical_ligatures | Properties.No_historical_ligatures ->
      `Historical
  | Properties.Contextual | Properties.No_contextual -> `Contextual

let font_variant_numeric_slot = function
  | Properties.Lining_nums | Properties.Oldstyle_nums -> `Figure
  | Properties.Proportional_nums | Properties.Tabular_nums -> `Spacing
  | Properties.Diagonal_fractions | Properties.Stacked_fractions -> `Fraction
  | Properties.Ordinal -> `Ordinal
  | Properties.Slashed_zero -> `Slashed_zero
  | Properties.Normal | Properties.Var _ -> `Invalid

let font_variant_east_asian_slot = function
  | Properties.Jis78 | Properties.Jis83 | Properties.Jis90 | Properties.Jis04
  | Properties.Simplified | Properties.Traditional ->
      `Variant
  | Properties.Full_width | Properties.Proportional_width -> `Width
  | Properties.Ruby -> `Ruby

let validate_font_variant_descriptor_values r values =
  let seen_ligatures = ref [] in
  let seen_numeric = ref [] in
  let seen_east_asian = ref [] in
  let seen_caps = ref false in
  let duplicate slot seen =
    if List.mem slot !seen then true
    else (
      seen := slot :: !seen;
      false)
  in
  let invalid =
    List.exists
      (function
        | Ligature value ->
            duplicate (font_variant_ligature_slot value) seen_ligatures
        | Numeric value ->
            duplicate (font_variant_numeric_slot value) seen_numeric
        | East_asian value ->
            duplicate (font_variant_east_asian_slot value) seen_east_asian
        | Caps _ ->
            let duplicate = !seen_caps in
            seen_caps := true;
            duplicate)
      values
  in
  if invalid then Cursor.err_invalid r "font-variant descriptor"

let read_font_variant_descriptor_value r =
  let ident = Cursor.ident ~keep_case:false r in
  match font_variant_desc_value ident with
  | Some value -> value
  | None -> Cursor.err_invalid r ("font-variant descriptor value: " ^ ident)

(* CSS Fonts 4 sec. 11.1 spells [<font-tech>] as a keyword, so an unknown ident
   is a parse error rather than text to carry through. *)
let rec read_font_tech_descriptor r : font_tech_descriptor =
  match Cursor.peek r with
  | Some (Component.Func { node = { name; _ }; _ })
    when String.lowercase_ascii name = "var" ->
      Var (Values.read_var read_font_tech_descriptor r)
  | Some _ | Option.None -> (
      let ident = Cursor.ident r in
      match Supports.font_tech_of_string (String.lowercase_ascii ident) with
      | Some tech -> Tech tech
      | Option.None -> Cursor.err_invalid r ("font-tech descriptor: " ^ ident))

let read_font_variant_keywords r : font_variant_descriptor =
  let at_value_end () = Cursor.is_done r || Cursor.peek_semicolon r in
  let snap = Cursor.save r in
  let first = Cursor.ident ~keep_case:false r in
  Cursor.ws r;
  match (first, at_value_end ()) with
  | "normal", true -> Normal
  | "none", true -> None
  | _ ->
      Cursor.restore r snap;
      let rec loop acc =
        Cursor.ws r;
        if at_value_end () then List.rev acc
        else loop (read_font_variant_descriptor_value r :: acc)
      in
      let values = loop [] in
      if values = [] then Cursor.err_invalid r "font-variant descriptor";
      validate_font_variant_descriptor_values r values;
      Values values

let rec read_font_variant_descriptor r : font_variant_descriptor =
  Cursor.ws r;
  match Cursor.peek r with
  | Some (Component.Func { node = { name; _ }; _ })
    when String.lowercase_ascii name = "var" ->
      Var (Values.read_var read_font_variant_descriptor r)
  | Some _ | None -> read_font_variant_keywords r

let read_font_face_desc name r =
  match name with
  | "font-family" -> read_font_family_descriptor r
  | "src" -> read_descriptor_value Font_face.read_src (fun v -> Src v) r
  | "font-style" -> read_font_style_descriptor r
  | "font-weight" -> read_font_weight_descriptor r
  | "font-stretch" -> read_font_stretch_descriptor r
  | "font-display" ->
      read_descriptor_value Properties.read_font_display
        (fun v -> Font_display v)
        r
  | "unicode-range" -> read_unicode_range_descriptor r
  | "font-variant" ->
      read_descriptor_value read_font_variant_descriptor
        (fun v -> Font_variant v)
        r
  | "font-feature-settings" ->
      read_descriptor_value Properties.read_font_feature_settings
        (fun v -> Font_feature_settings v)
        r
  | "font-variation-settings" ->
      read_descriptor_value Properties.read_font_variation_settings
        (fun v -> Font_variation_settings v)
        r
  | "font-tech" ->
      read_descriptor_value read_font_tech_descriptor (fun v -> Font_tech v) r
  | "size-adjust" ->
      read_descriptor_value Font_face.read_size_adjust
        (fun v -> Size_adjust v)
        r
  | "ascent-override" ->
      read_descriptor_value Font_face.read_metric_override
        (fun v -> Ascent_override v)
        r
  | "descent-override" ->
      read_descriptor_value Font_face.read_metric_override
        (fun v -> Descent_override v)
        r
  | "line-gap-override" ->
      read_descriptor_value Font_face.read_metric_override
        (fun v -> Line_gap_override v)
        r
  | _ -> Cursor.err_invalid r ("unknown font-face descriptor: " ^ name)

let rec components_upto_semicolon = function
  | [] | Component.Preserved { kind = Token.Semicolon; _ } :: _ -> []
  | component :: rest -> component :: components_upto_semicolon rest

(* CSS Variables 1 sec. 3 substitutes [var()] in a property value; an @font-face
   descriptor is not a property, so no descriptor grammar accepts a var() and
   browsers drop the whole declaration. The descriptor readers delegate to the
   shared property readers, which accept var() legitimately in property
   position, so the rejection belongs at the descriptor boundary. *)
let descriptor_value_has_var r =
  Component.has_var (components_upto_semicolon (Cursor.remaining r))

(* Whether the descriptor called [name] keeps a var() for the inline pass, asked
   of the typed AST rather than of a second list of names: read a value only
   such a descriptor accepts, then put the descriptor to
   {!resolve_font_face_var}, the one table [Inline] fills in. A descriptor with
   no resolution path either refuses the probe or lands on an arm that table
   answers [None] for. Only a value carrying a var() asks, so an ordinary
   descriptor never pays for the probe. *)
let descriptor_resolves_var name =
  let probe = Cursor.of_string ":var(--x)" in
  match read_font_face_desc name probe with
  | descriptor ->
      Option.is_some
        (resolve_font_face_var ~src:Fun.id ~unicode_range:Fun.id
           ~font_family:Fun.id ~font_style:Fun.id ~font_weight:Fun.id
           ~font_stretch:Fun.id ~font_display:Fun.id ~font_variant:Fun.id
           ~font_feature_settings:Fun.id ~font_variation_settings:Fun.id
           ~metric_override:Fun.id ~font_tech:Fun.id ~size_adjust:Fun.id
           descriptor)
  | exception Error.Parse_error _ -> false

(* CSS Syntax 3 (ED) sec. 5.5.5 gives an [<at-keyword-token>] to "consume an
   at-rule" rather than to the declaration reader, and keeps the declarations
   written on either side of it. No @font-face descriptor is an at-rule, so the
   rule read here is dropped - and it alone, because sec. 5.5.2 ends an at-rule
   at its block or at its [;], never at the descriptor after it. A cursor that
   does not recover raises instead, as the other descriptor bodies do. *)
let skip_font_face_at_rule r name loc =
  let error = Error.unknown_at_rule loc name in
  if not (Cursor.recover r) then Error.fail error;
  let start = Cursor.save r in
  skip_past_rule r;
  Cursor.push_warning r
    ~recovery:(Cursor.dropped_since r start Error.Recovery.Rule)
    error

let read_font_face_descriptor (r : Cursor.t) : font_face_descriptor option =
  Cursor.ws r;
  if Cursor.is_done r then None
  else if Cursor.peek_semicolon r then (
    Cursor.skip r;
    None)
  else
    match Cursor.peek r with
    | Some (Component.Preserved { kind = Token.At_keyword name; loc; _ }) ->
        skip_font_face_at_rule r name loc;
        None
    | _ -> (
        let start = Cursor.save r in
        let name = Cursor.ident ~keep_case:false r in
        match
          if descriptor_value_has_var r && not (descriptor_resolves_var name)
          then Cursor.err_invalid r ("var() in @font-face descriptor: " ^ name)
          else read_font_face_desc name r
        with
        | descriptor ->
            Cursor.ws r;
            if Cursor.peek_semicolon r then Cursor.skip r;
            Some descriptor
        | exception Error.Parse_error e ->
            (* CSS Fonts 4 sec. 4.1 / CSS Syntax 3 (ED) sec. 5.5.5: a descriptor
               that does not parse - an unknown name (Fontsource's
               [font-named-instance]) or an invalid value of a known one
               ([font-display:maybe]) - is dropped and the rest of the
               @font-face is kept, matching browsers. *)
            Cursor.skip_past_semicolon r;
            Cursor.push_warning r
              ~recovery:
                (Cursor.dropped_since r start Error.Recovery.Declaration)
              e;
            None)

let read_font_face_block inner =
  let rec read_descriptors acc =
    match read_font_face_descriptor inner with
    | Some desc -> read_descriptors (desc :: acc)
    | None ->
        Cursor.ws inner;
        if Cursor.is_done inner then List.rev acc else read_descriptors acc
  in
  read_descriptors []

let read_font_face (r : Cursor.t) : statement =
  Cursor.with_context r "@font-face" @@ fun () ->
  Cursor.expect_at_keyword "font-face" r;
  Cursor.ws r;
  (* CSS Fonts 4 sec. 4.1: missing [font-family] / [src] is a semantic mismatch,
     not a syntax one. [validate_partial_statement] flags it; the syntactic
     reader accepts. *)
  let descriptors = Cursor.braces read_font_face_block r in
  Font_face descriptors

let read_counter_style_system_value r =
  let system = Cursor.ident ~keep_case:false r in
  match system with
  | "cyclic" -> Cyclic
  | "numeric" -> Numeric
  | "alphabetic" -> Alphabetic
  | "symbolic" -> Symbolic
  | "fixed" ->
      Cursor.ws r;
      Fixed (Cursor.integer_opt r)
  | "additive" -> Additive
  | "extends" ->
      Cursor.ws r;
      Extends (Cursor.ident ~keep_case:true r)
  | _ -> Cursor.err_invalid r ("unknown counter-style system: " ^ system)

let read_counter_style_system_descriptor r =
  read_descriptor_value Declaration.read_property_value
    (fun value ->
      let c = Cursor.of_string value in
      let system = read_counter_style_system_value c in
      Cursor.ws c;
      Cursor.expect_eof c;
      System system)
    r

let read_counter_symbol r =
  match Cursor.string_opt r with
  | Some symbol -> symbol
  | None -> Cursor.ident ~keep_case:true r

let read_counter_symbols_descriptor r =
  read_descriptor_value Declaration.read_property_value
    (fun value ->
      let c = Cursor.of_string value in
      let symbols =
        Cursor.list ~at_least:1 ~sep:Cursor.ws read_counter_symbol c
      in
      Cursor.ws c;
      Cursor.expect_eof c;
      Symbols symbols)
    r

let read_counter_symbol_descriptor constructor r =
  read_descriptor_value Declaration.read_property_value
    (fun value ->
      let c = Cursor.of_string value in
      let symbol = read_counter_symbol c in
      Cursor.ws c;
      Cursor.expect_eof c;
      constructor symbol)
    r

let read_counter_string_descriptor constructor r =
  read_descriptor_value
    (fun r ->
      let value = Declaration.read_property_value r in
      validate_nonempty_descriptor r "counter-style" value;
      value)
    constructor r

(* CSS Counter Styles 3 sec. 3: "unknown descriptors are invalid and ignored".
   One descriptor of the body, the caller looping over the rest, so a descriptor
   that fails leaves the ones already read untouched. The declaration is the
   whole of the descriptor's value: an [!important] flag, which no descriptor
   takes, invalidates the declaration it follows rather than being left over as
   an item of its own, as it is in Blink 146. *)
let read_counter_style_descriptor (r : Cursor.t) : counter_style_descriptor =
  let name = Cursor.ident ~keep_case:false r in
  let descriptor =
    match name with
    | "system" -> read_counter_style_system_descriptor r
    | "symbols" -> read_counter_symbols_descriptor r
    | "suffix" -> read_counter_symbol_descriptor (fun s -> Suffix s) r
    | "prefix" -> read_counter_symbol_descriptor (fun s -> Prefix s) r
    | "fallback" -> read_counter_string_descriptor (fun s -> Fallback s) r
    | "range" -> read_counter_string_descriptor (fun s -> Range s) r
    | "pad" -> read_counter_string_descriptor (fun s -> Pad s) r
    | "negative" -> read_counter_string_descriptor (fun s -> Negative s) r
    | "additive-symbols" ->
        read_counter_string_descriptor (fun s -> Additive_symbols s) r
    | "speak-as" -> read_counter_string_descriptor (fun s -> Speak_as s) r
    | _ -> Cursor.err_invalid r ("unknown counter-style descriptor: " ^ name)
  in
  Cursor.ws r;
  if not (Cursor.is_done r || Cursor.peek_semicolon r) then
    Cursor.err_invalid r ("trailing tokens after @counter-style " ^ name);
  if Cursor.peek_semicolon r then Cursor.skip r;
  descriptor

let counter_style_descriptor_rank = function
  | System _ -> 0
  | Symbols _ -> 1
  | Additive_symbols _ -> 2
  | Prefix _ -> 3
  | Suffix _ -> 4
  | Range _ -> 5
  | Pad _ -> 6
  | Negative _ -> 7
  | Fallback _ -> 8
  | Speak_as _ -> 9

let replace_counter_style_descriptor desc acc =
  desc
  :: List.filter
       (fun existing ->
         counter_style_descriptor_rank existing
         <> counter_style_descriptor_rank desc)
       acc

(* One item of a descriptor body: a descriptor, a stray [;] that CSS Syntax 3
   (ED) sec. 5.5.5 discards with no declaration to validate, or the end of the
   body. *)
let read_descriptor_body_step read_one replace inner acc =
  Cursor.ws inner;
  if Cursor.is_done inner then `Done (List.rev acc)
  else if Cursor.peek_semicolon inner then (
    Cursor.skip inner;
    `More acc)
  else `More (replace (read_one inner) acc)

let read_counter_style_descriptors r =
  Cursor.braces
    (fun inner ->
      read_items_with_recovery ~skip:skip_invalid_item
        ~construct:Error.Recovery.Declaration
        (read_descriptor_body_step read_counter_style_descriptor
           replace_counter_style_descriptor)
        inner [])
    r

let counter_style_system descriptors =
  List.find_map
    (function System system -> Some system | _ -> None)
    descriptors

let counter_style_has_symbols descriptors =
  List.exists (function Symbols _ -> true | _ -> false) descriptors

let counter_style_has_additive_symbols descriptors =
  List.exists (function Additive_symbols _ -> true | _ -> false) descriptors

let counter_style_validate r descriptors =
  let system =
    match counter_style_system descriptors with
    | Some system -> system
    | None -> Cursor.err_invalid r "@counter-style requires a system descriptor"
  in
  match system with
  | Cyclic | Numeric | Alphabetic | Symbolic | Fixed _ ->
      if not (counter_style_has_symbols descriptors) then
        Cursor.err_invalid r
          "@counter-style system requires a symbols descriptor"
  | Additive ->
      if not (counter_style_has_additive_symbols descriptors) then
        Cursor.err_invalid r
          "@counter-style additive system requires additive-symbols"
  | Extends _ -> ()

let read_counter_style (r : Cursor.t) : statement =
  Cursor.with_context r "@counter-style" @@ fun () ->
  Cursor.expect_at_keyword "counter-style" r;
  Cursor.ws r;
  let name = Cursor.ident ~keep_case:true r in
  Cursor.ws r;
  let descriptors =
    read_counter_style_descriptors r
    |> List.stable_sort (fun a b ->
        compare
          (counter_style_descriptor_rank a)
          (counter_style_descriptor_rank b))
  in
  counter_style_validate r descriptors;
  Counter_style (name, descriptors)

(* CSS Paged Media 3 sec. 4.2: a page selector is an optional page name followed
   by zero or more pseudo-pages from [:first | :left | :right | :blank]. *)
let page_selector_error r s =
  Cursor.err_invalid r ("invalid @page selector: " ^ s)

let page_selector_is_ident_char c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c = '-' || c = '_'

let page_selector_is_ws = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let rec page_selector_consume_ident s len i =
  if i < len && page_selector_is_ident_char s.[i] then
    page_selector_consume_ident s len (i + 1)
  else i

let rec page_selector_skip_ws s len i =
  if i < len && page_selector_is_ws s.[i] then
    page_selector_skip_ws s len (i + 1)
  else i

(* CSS Paged Media 3 section 4.3: [<page-selector-list> = <page-selector>#],
   [<page-selector> = <ident-token>? <pseudo-page>*], with each pseudo-page from
   the closed set [first | left | right | blank], so [@page invoice:blank:first]
   is well-formed. *)
let parse_page_selectors r selector =
  let s = String.trim selector in
  let len = String.length s in
  let consume_ident = page_selector_consume_ident s len in
  let skip_ws = page_selector_skip_ws s len in
  let pseudo_of_name name =
    (* CSS Paged Media 3 sec. 4.3 closes [<pseudo-page>] over four names, so
       each is a keyword and not the page name beside it. *)
    match Common.String.lowercase_ascii_preserve name with
    | "first" -> First
    | "left" -> Left
    | "right" -> Right
    | "blank" -> Blank
    | _ -> page_selector_error r s
  in
  let consume_pseudo_page i : (page_pseudo * int) option =
    if i >= len || s.[i] <> ':' then None
    else
      let start = i + 1 in
      let stop = consume_ident start in
      if stop = start then page_selector_error r s;
      Some (pseudo_of_name (String.sub s start (stop - start)), stop)
  in
  let rec consume_pseudo_pages acc i =
    match consume_pseudo_page i with
    | None -> (List.rev acc, i)
    | Some (p, stop) -> consume_pseudo_pages (p :: acc) stop
  in
  let rec consume_selectors acc i =
    let i = skip_ws i in
    if i >= len then List.rev acc
    else
      let after_ident = consume_ident i in
      let name =
        if after_ident = i then Option.None
        else Some (String.sub s i (after_ident - i))
      in
      let pseudos, after_pseudo = consume_pseudo_pages [] after_ident in
      let sel = { name; pseudos } in
      let after_pseudo = skip_ws after_pseudo in
      if after_pseudo >= len then List.rev (sel :: acc)
      else if s.[after_pseudo] = ',' then
        consume_selectors (sel :: acc) (after_pseudo + 1)
      else page_selector_error r s
  in
  consume_selectors [] 0

let allowed_page_margin_names =
  [
    "top-left";
    "top-center";
    "top-right";
    "right-top";
    "right-middle";
    "right-bottom";
    "bottom-right";
    "bottom-center";
    "bottom-left";
    "left-bottom";
    "left-middle";
    "left-top";
  ]

(* CSS Paged Media 3 sec. 6: Appendix A lists the CSS 2.1 properties that apply
   in a page and in a margin context, and behaviour for a property outside CSS
   2.1 is left undefined "to allow the gradual addition of appropriate CSS3
   properties as they emerge" - undefined, not invalid. Blink 146 duly keeps
   every property it knows in both contexts. A name filter here would drop text
   browsers keep, so the page contexts take the declarations a style rule takes:
   a value its property's grammar rejects is still rejected, and so is an item
   that is no declaration at all. *)
let read_page_margin_rule r =
  match Cursor.peek r with
  | Some (Component.Preserved { kind = Token.At_keyword raw; _ }) ->
      (* Sec. 5.2 names the margin boxes, so each name is a keyword. *)
      let name = Common.String.lowercase_ascii_preserve raw in
      if not (List.mem name allowed_page_margin_names) then
        Cursor.err_invalid r ("unknown page margin rule: @" ^ raw);
      Cursor.skip r;
      Cursor.ws r;
      (* Sec. 5 gives the margin at-rule a [<declaration-list>], which CSS
         Syntax 3 (ED) sec. 5.5.5 consumes even when it holds nothing, so an
         empty box is valid and Blink 146 keeps one. That it paints nothing is
         [Block.drop_empty_rules]'s call to make, not the reader's. *)
      let descriptors =
        Cursor.braces (read_descriptor_block replace_descriptor) r
      in
      { name; descriptors }
  | _ -> Cursor.err_expected r "page margin rule"

(* CSS Paged Media 3 sec. 2: a [@page] body holds page properties and margin
   at-rules, so its items end two different ways and are read apart. *)
let read_page_step inner (descriptors, margins) =
  match Cursor.peek inner with
  | Some (Component.Preserved { kind = Token.At_keyword _; _ }) ->
      `More (descriptors, read_page_margin_rule inner :: margins)
  | _ -> (
      match read_descriptor_item inner with
      | `Done -> `Done (List.rev descriptors, List.rev margins)
      | `Skip -> `More (descriptors, margins)
      | `Descriptor desc -> `More (replace_descriptor desc descriptors, margins)
      )

let read_page_body inner =
  read_items_with_recovery ~skip:skip_invalid_item
    ~construct:Error.Recovery.Declaration read_page_step inner ([], [])

let read_page (r : Cursor.t) : statement =
  Cursor.with_context r "@page" @@ fun () ->
  Cursor.expect_at_keyword "page" r;
  Cursor.ws r;
  let selector =
    let s = Cursor.drain_until_block_as_string ~trim:true r in
    if s = "" then [] else parse_page_selectors r s
  in
  let descriptors, margins = Cursor.braces read_page_body r in
  Page_with_margins (selector, descriptors, margins)

let validate_dashed_ident r name context =
  if not (Custom_property_name.is_valid name) then
    Cursor.err_invalid r (context ^ " name must be a dashed ident")

let font_palette_descriptor_kind = function
  | Palette_font_family _ -> "font-family"
  | Base_palette _ -> "base-palette"
  | Override_colors _ -> "override-colors"

let replace_font_palette_descriptor desc acc =
  desc
  :: List.filter
       (fun existing ->
         font_palette_descriptor_kind existing
         <> font_palette_descriptor_kind desc)
       acc

let read_base_palette_value inner =
  match Cursor.int inner with
  | n ->
      if n < 0 then
        Cursor.err_invalid inner "base-palette index must be non-negative";
      Index n
  | exception Cursor.Parse_error _ -> (
      (* [light] and [dark] are keywords, so CSS Values 4 sec. 4.1 reads them
         case-insensitively; any other ident is the author's own. *)
      let ident = Cursor.ident inner in
      match Common.String.lowercase_ascii_preserve ident with
      | "light" -> Light
      | "dark" -> Dark
      | _ when ident <> "" -> Palette_ident ident
      | _ -> Cursor.err_expected inner "base-palette value")

let read_override_color_entry c =
  let index = Cursor.int c in
  if index < 0 then
    Cursor.err_invalid c "override-colors index must be non-negative";
  Cursor.ws c;
  let color = Values.read_color c in
  Cursor.ws c;
  (index, color)

(* CSS Fonts 4 sec. 12.1 gives each [@font-palette-values] descriptor a grammar
   of its own, and the declaration is the whole of it: a trailing [!important]
   or a stray ident makes the declaration invalid rather than the leftover
   alone, as it does in Blink 146. *)
let read_font_palette_descriptor outer inner : font_palette_descriptor =
  let desc_name = Cursor.ident ~keep_case:false inner in
  Cursor.ws inner;
  if not (Cursor.colon inner) then Cursor.err_expected inner "':'";
  Cursor.ws inner;
  let desc =
    match desc_name with
    | "font-family" ->
        Palette_font_family
          (Cursor.list ~sep:Cursor.comma Properties.read_font_family_name inner)
    | "base-palette" -> Base_palette (read_base_palette_value inner)
    | "override-colors" ->
        Override_colors
          (Cursor.list ~sep:Cursor.comma ~at_least:1 read_override_color_entry
             inner)
    | name ->
        Cursor.err_invalid outer
          ("unknown font-palette-values descriptor: " ^ name)
  in
  Cursor.ws inner;
  if not (Cursor.is_done inner || Cursor.peek_semicolon inner) then
    Cursor.err_invalid outer
      ("trailing tokens after @font-palette-values " ^ desc_name);
  if Cursor.peek_semicolon inner then Cursor.skip inner;
  desc

let read_font_palette_descriptors outer =
  Cursor.braces
    (fun inner ->
      read_items_with_recovery ~skip:skip_invalid_item
        ~construct:Error.Recovery.Declaration
        (read_descriptor_body_step
           (read_font_palette_descriptor outer)
           replace_font_palette_descriptor)
        inner [])
    outer

let font_palette_descriptor_rank = function
  | Palette_font_family _ -> 0
  | Base_palette _ -> 1
  | Override_colors _ -> 2

let read_font_palette_values (r : Cursor.t) : statement =
  Cursor.with_context r "@font-palette-values" @@ fun () ->
  Cursor.expect_at_keyword "font-palette-values" r;
  Cursor.ws r;
  let name = Cursor.ident ~keep_case:true r in
  validate_dashed_ident r name "@font-palette-values";
  Cursor.ws r;
  let descriptors =
    read_font_palette_descriptors r
    |> List.stable_sort (fun a b ->
        compare
          (font_palette_descriptor_rank a)
          (font_palette_descriptor_rank b))
  in
  if descriptors = [] then
    Cursor.err_invalid r "@font-palette-values requires descriptors";
  Font_palette_values (name, descriptors)

let valid_font_feature_values_block = function
  | "styleset" | "character-variant" | "stylistic" | "swash" | "ornaments"
  | "annotation" ->
      true
  | _ -> false

let read_font_feature_value_entry outer inner : (string * int list) option =
  Cursor.ws inner;
  if Cursor.is_done inner then None
  else if Cursor.peek_semicolon inner then (
    Cursor.skip inner;
    None)
  else
    let name = Cursor.ident ~keep_case:true inner in
    Cursor.ws inner;
    if not (Cursor.colon inner) then Cursor.err_expected inner "':'";
    Cursor.ws inner;
    let indexes = Cursor.list ~at_least:1 Cursor.int inner in
    List.iter
      (fun index ->
        if index <= 0 then
          Cursor.err_invalid outer
            "@font-feature-values indexes must be positive")
      indexes;
    Cursor.ws inner;
    if Cursor.peek_semicolon inner then Cursor.skip inner;
    Some (name, indexes)

let replace_font_feature_value ((name, _) as entry) acc =
  entry :: List.filter (fun (existing, _) -> existing <> name) acc

let read_font_feature_values_entries outer =
  let rec loop inner acc =
    let start = Cursor.save inner in
    match read_font_feature_value_entry outer inner with
    | Some entry -> loop inner (replace_font_feature_value entry acc)
    | None ->
        Cursor.ws inner;
        if Cursor.is_done inner then List.rev acc else loop inner acc
    | exception Error.Parse_error e ->
        Cursor.skip_past_semicolon inner;
        Cursor.push_warning inner
          ~recovery:
            (Cursor.dropped_since inner start Error.Recovery.Declaration)
          e;
        loop inner acc
  in
  Cursor.braces (fun inner -> loop inner []) outer

let read_font_feature_values_block outer inner =
  match Cursor.at_keyword_opt inner with
  | None -> Cursor.err_expected inner "@font-feature-values nested at-rule"
  | Some name ->
      let name = String.lowercase_ascii name in
      if not (valid_font_feature_values_block name) then
        Cursor.err_invalid outer ("unknown @font-feature-values block: @" ^ name);
      (name, read_font_feature_values_entries inner)

(* CSS Fonts 4 sec. 11.1 fills the body with feature value blocks, so it is a
   list of rules rather than of declarations: a block the body rejects ends at
   its own [{}] or at a [;] (CSS Syntax 3 (ED) sec. 5.5.2), which leaves the
   blocks written around it in the rule. A [;] with nothing before it has no
   declaration to validate and is discarded (sec. 5.5.5). *)
let read_font_feature_values_step outer inner acc =
  Cursor.ws inner;
  if Cursor.is_done inner then `Done (List.rev acc)
  else if Cursor.peek_semicolon inner then (
    Cursor.skip inner;
    `More acc)
  else `More (read_font_feature_values_block outer inner :: acc)

let read_font_feature_values_blocks outer =
  Cursor.braces
    (fun inner ->
      read_items_with_recovery ~skip:skip_past_rule
        ~construct:Error.Recovery.Rule
        (read_font_feature_values_step outer)
        inner [])
    outer

let read_font_feature_values (r : Cursor.t) : statement =
  Cursor.with_context r "@font-feature-values" @@ fun () ->
  Cursor.expect_at_keyword "font-feature-values" r;
  Cursor.ws r;
  let families =
    Cursor.list ~sep:Cursor.comma ~at_least:1 Properties.read_font_family r
  in
  Cursor.ws r;
  let blocks = read_font_feature_values_blocks r in
  if blocks = [] then
    Cursor.err_invalid r "@font-feature-values requires feature blocks";
  Font_feature_values (families, blocks)

let expect_view_transition_block r =
  match Cursor.peek r with
  | Some (Component.Block { node = { opening = Token.Curly; _ }; _ }) -> ()
  | Some _ -> Cursor.err_invalid r "@view-transition does not take a prelude"
  | None -> Cursor.err_expected r "'{'"

let same_view_transition_descriptor a b =
  match (a, b) with
  | Navigation _, Navigation _ | Types _, Types _ -> true
  | _ -> false

let replace_view_transition_descriptor desc acc =
  desc
  :: List.filter
       (fun existing -> not (same_view_transition_descriptor existing desc))
       acc

let read_view_transition_navigation inner =
  match Cursor.ident ~keep_case:false inner with
  | "auto" -> Navigation `Auto
  | "none" -> Navigation `None
  | _ ->
      Cursor.err_invalid inner "invalid @view-transition navigation descriptor"

let read_view_transition_types inner =
  match Cursor.ident ~keep_case:true inner with
  | "none" -> Types None
  | first ->
      let rec names acc =
        Cursor.ws inner;
        match Cursor.ident_opt inner with
        | Some name -> names (name :: acc)
        | None -> List.rev acc
      in
      Types (Some (names [ first ]))

(* CSS View Transitions 2 sec. 2.4 gives each [@view-transition] descriptor a
   grammar of its own, and the declaration is the whole of it. *)
let read_view_transition_descriptor outer inner : view_transition_descriptor =
  let desc_name = Cursor.ident ~keep_case:false inner in
  Cursor.ws inner;
  if not (Cursor.colon inner) then Cursor.err_expected inner "':'";
  Cursor.ws inner;
  let desc =
    match desc_name with
    | "navigation" -> read_view_transition_navigation inner
    | "types" -> read_view_transition_types inner
    | name ->
        Cursor.err_invalid outer ("unknown @view-transition descriptor: " ^ name)
  in
  Cursor.ws inner;
  if not (Cursor.is_done inner || Cursor.peek_semicolon inner) then
    Cursor.err_invalid outer
      ("trailing tokens after @view-transition " ^ desc_name);
  if Cursor.peek_semicolon inner then Cursor.skip inner;
  desc

let read_view_transition_descriptors outer =
  Cursor.braces
    (fun inner ->
      read_items_with_recovery ~skip:skip_invalid_item
        ~construct:Error.Recovery.Declaration
        (read_descriptor_body_step
           (read_view_transition_descriptor outer)
           replace_view_transition_descriptor)
        inner [])
    outer

let read_view_transition (r : Cursor.t) : statement =
  Cursor.with_context r "@view-transition" @@ fun () ->
  Cursor.expect_at_keyword "view-transition" r;
  Cursor.ws r;
  expect_view_transition_block r;
  let descriptors = read_view_transition_descriptors r in
  if descriptors = [] then
    Cursor.err_invalid r "@view-transition requires descriptors";
  View_transition descriptors

let declaration_order_rank decl =
  match Declaration.property_name decl with
  | "top" -> 0
  | "right" -> 1
  | "bottom" -> 2
  | "left" -> 3
  | "width" -> 4
  | "height" -> 5
  | "inset-inline-start" -> 6
  | "inset-inline-end" -> 7
  | "inset-block-start" -> 8
  | "inset-block-end" -> 9
  | "margin-inline" -> 10
  | _ -> 100

let read_position_try (r : Cursor.t) : statement =
  Cursor.with_context r "@position-try" @@ fun () ->
  Cursor.expect_at_keyword "position-try" r;
  Cursor.ws r;
  let name = Cursor.ident ~keep_case:true r in
  validate_dashed_ident r name "@position-try";
  Cursor.ws r;
  let declarations =
    Cursor.braces
      (fun inner ->
        let declarations = Declaration.read_declarations inner in
        Cursor.ws inner;
        Cursor.expect_eof inner;
        declarations)
      r
    |> List.stable_sort (fun a b ->
        compare (declaration_order_rank a) (declaration_order_rank b))
  in
  if declarations = [] then
    Cursor.err_invalid r "@position-try requires descriptors";
  Position_try (name, declarations)

let rec read_viewport_descriptor (r : Cursor.t) : viewport_descriptor option =
  Cursor.ws r;
  match Cursor.peek r with
  | None -> None
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
      ignore (Cursor.next_raw r);
      read_viewport_descriptor r
  | Some (Component.Preserved { kind = Token.Ident name; _ }) ->
      ignore (Cursor.next_raw r);
      Cursor.ws r;
      if not (Cursor.colon r) then Cursor.err_expected r "':'";
      Cursor.ws r;
      let value = String.trim (Cursor.consume_until_semicolon ~trim:true r) in
      Some { name; value }
  | Some _ -> Cursor.err_expected r "<viewport-descriptor>"

let read_viewport_descriptors r =
  Cursor.braces
    (fun inner ->
      let rec loop acc =
        match read_viewport_descriptor inner with
        | None -> List.rev acc
        | Some d -> loop (d :: acc)
      in
      let descriptors = loop [] in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      descriptors)
    r

let read_viewport_with_prefix prefix at_keyword (r : Cursor.t) : statement =
  Cursor.with_context r ("@" ^ at_keyword) @@ fun () ->
  Cursor.expect_at_keyword at_keyword r;
  Cursor.ws r;
  Viewport (prefix, read_viewport_descriptors r)

let read_viewport = read_viewport_with_prefix Standard "viewport"
let read_ms_viewport = read_viewport_with_prefix Ms_prefixed "-ms-viewport"

(* CSS Syntax 3 (ED) sec. 4.3.5 closes a string at end of input, 4.3.6 a url,
   4.3.2 a comment, and 5.5.9 and 5.5.10 a simple block and a function. Raw text
   that stops mid-construct therefore means the closed form, but written back as
   it stands it hands the next reader an opener that eats the [;] or [}] the
   at-rule ends with. These close it, once, so the AST holds text that reads
   back as itself. *)
let bracket_closer = function
  | Token.Curly -> "}"
  | Token.Paren -> ")"
  | Token.Square -> "]"

(* One pass: the closers the open blocks and functions still want, innermost
   first, with the last token and the token count for [tail_closer]. *)
let scan_raw text =
  let lexer = Lexer.of_string text in
  let rec loop stack last n =
    let tok = Lexer.next lexer in
    match tok.Token.kind with
    | Token.Eof -> (stack, last, n)
    | Token.Function _ -> loop (")" :: stack) (Some tok) (n + 1)
    | Token.Open b -> loop (bracket_closer b :: stack) (Some tok) (n + 1)
    | Token.Close b ->
        let stack =
          match stack with
          | c :: rest when String.equal c (bracket_closer b) -> rest
          | _ -> stack
        in
        loop stack (Some tok) (n + 1)
    | _ -> loop stack (Some tok) (n + 1)
  in
  loop [] None 0

(* A string, a url and a comment run to end of input and close there without
   leaving an opener on the stack, so ask the tokenizer rather than re-deriving
   its state: text that swallows the probe gives back no token for it. Two probe
   code points, since a [\] at end of input eats one and returns it as an
   ident. *)
let tail_closer text last n =
  let lexer = Lexer.of_string (String.concat "" [ text; ";;" ]) in
  let rec count n =
    match (Lexer.next lexer).Token.kind with
    | Token.Eof -> n
    | _ -> count (n + 1)
  in
  if count 0 <> n then []
  else
    match last with
    | Some { Token.kind = Token.String { quote; terminated = false; _ }; _ } ->
        [ String.make 1 quote ]
    | Some { Token.kind = Token.Url _ | Token.Bad_url; loc }
      when loc.Loc.end_pos = String.length text ->
        [ ")" ]
    | _ -> [ "*/" ]

(* Closing one construct can uncover another: a [\] before the closing quote
   escapes it and leaves the string open, so re-read until the text settles. *)
let rec close_raw fuel text =
  let stack, last, n = scan_raw text in
  match List.append (tail_closer text last n) stack with
  | [] -> text
  | closers ->
      let closed = String.concat "" (text :: closers) in
      if fuel <= 1 then closed else close_raw (fuel - 1) closed

(* One pass closes what the stack holds; the other two are for a closer that a
   [\] at end of input escapes before it lands. *)
let close_open_constructs = close_raw 3

(* CSS Syntax 3 (ED) sec. 5.5.9: an unclosed block runs to EOF, so its slice has
   no closer to exclude and instead carries whatever [}] an unterminated nested
   construct swallowed on the way. The serializer supplies its own closer, so
   drop those or each round-trip stacks another one. *)
let trim_unknown_block_body body =
  let rec trim_end i =
    if i < 0 then 0
    else
      match body.[i] with
      | '}' | ' ' | '\t' | '\n' | '\r' -> trim_end (i - 1)
      | _ -> i + 1
  in
  String.sub body 0 (trim_end (String.length body - 1))

(* An unrecognised at-rule has no grammar to re-serialise its body from, so the
   body travels as the source text between its own braces. Slice the block's own
   span, not its child components: the children stop short of the closer
   whenever the body ends in a nested block ([@foo{a{b:c}}]). *)
let unknown_block_body slice (block : Component.block Component.node) =
  let start = block.loc.Loc.start_pos + 1 in
  if block.node.Component.closed then slice start (block.loc.Loc.end_pos - 1)
  else
    slice start block.loc.Loc.end_pos
    |> trim_unknown_block_body |> close_open_constructs

(* CSS Syntax 3 (ED) sec. 5.5.2 "consume an at-rule": after the at-keyword has
   been consumed, walk components until we hit [;] (no block) or [{...}]
   (block). Raw prelude/block strings are sliced from the original source so the
   at-rule round-trips byte-for-byte even when its grammar is unknown. *)
let read_unknown_at_rule name (r : Cursor.t) : statement =
  let source = Option.value (Cursor.source r) ~default:"" in
  let source_len = String.length source in
  let slice start stop =
    if start < 0 || stop > source_len || start > stop then ""
    else String.sub source start (stop - start)
  in
  let prelude_start = ref (-1) in
  let prelude_end = ref (-1) in
  let block = ref Option.None in
  let rec gather () =
    match Cursor.peek r with
    | None -> ()
    | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
        ignore (Cursor.next_raw r)
    | Some (Component.Block ({ node = { opening = Token.Curly; _ }; _ } as b))
      ->
        block := Option.Some (unknown_block_body slice b);
        ignore (Cursor.next_raw r)
    | Some comp ->
        let loc = Component.source_loc comp in
        if !prelude_start < 0 then prelude_start := loc.Loc.start_pos;
        prelude_end := loc.Loc.end_pos;
        ignore (Cursor.next_raw r);
        gather ()
  in
  gather ();
  (* The block form knows it ran to EOF from [closed] and pays for the scan only
     then; a statement one cannot, because [cursor_of_rule] gives the at-rule a
     [;] whether the source ended in one or in EOF. A prelude that is already
     closed comes back unchanged, so close it either way - after the trim the
     printer applies, so the closer lands where the trim would have ended the
     text rather than after the run of whitespace it takes off. *)
  let prelude =
    if !prelude_start < 0 then ""
    else
      slice !prelude_start !prelude_end
      |> trim_unknown_at_prelude |> close_open_constructs
  in
  Unknown_at_rule { name; prelude; block = !block }

type property_reader_state = {
  syntax : Variables.any_syntax option;
  inherits : bool option;
  initial_value : string option;
}

let css_wide_keyword s =
  match String.lowercase_ascii (String.trim s) with
  | "initial" | "inherit" | "unset" | "revert" | "revert-layer" -> true
  | _ -> false

let read_property_initial_value r syntax str =
  if css_wide_keyword str then
    Cursor.err_invalid r "@property: initial-value cannot be CSS-wide keyword";
  let value_reader = Cursor.of_string str in
  let value = Variables.read_value value_reader syntax in
  Cursor.ws value_reader;
  Cursor.expect_eof value_reader;
  value

(* Every failure below is a failure of the [\@when] / [\@else] prelude, so it is
   raised on the cursor the prelude's own components sit in and the caret lands
   on the one that failed rather than on the block that follows it. *)
let conditional_terminated r ~at_rule (fn : Component.func Component.node) =
  if not fn.node.terminated then
    Cursor.err_condition r ~at_rule "unterminated condition function"

let conditional_atom r ~at_rule (fn : Component.func Component.node) =
  conditional_terminated r ~at_rule fn;
  match String.lowercase_ascii fn.Component.node.name with
  | "media" -> Media_condition (Media.of_function_components fn.node.arguments)
  | "supports" ->
      Supports_condition_test
        (Supports.of_string ~allow_unwrapped_decl:true
           (Cursor.string_of_components ~trim:true fn.node.arguments))
  | name ->
      Cursor.err_condition r ~at_rule ("unknown condition function: " ^ name)

let conditional_components ~at_rule cursor =
  let peek_ident () =
    match Cursor.peek cursor with
    | Some (Component.Preserved { kind = Token.Ident name; _ }) ->
        Some (String.lowercase_ascii name)
    | _ -> None
  in
  let read_atom () =
    Cursor.ws cursor;
    match Cursor.peek cursor with
    | Some (Component.Func fn) ->
        let atom = conditional_atom cursor ~at_rule fn in
        Cursor.skip cursor;
        atom
    | _ -> Cursor.err_condition cursor ~at_rule "expected a condition function"
  in
  let mixed op = Cursor.err_condition cursor ~at_rule ("cannot mix " ^ op) in
  let rec chain op acc =
    Cursor.ws cursor;
    match peek_ident () with
    | Some "and" ->
        (match op with Some `Or -> mixed "or and and" | _ -> ());
        Cursor.skip cursor;
        chain (Some `And) (And (acc, read_atom ()))
    | Some "or" ->
        (match op with Some `And -> mixed "and and or" | _ -> ());
        Cursor.skip cursor;
        chain (Some `Or) (Or (acc, read_atom ()))
    | _ -> acc
  in
  let condition = chain None (read_atom ()) in
  Cursor.ws cursor;
  if not (Cursor.is_done cursor) then
    Cursor.err_condition cursor ~at_rule "trailing content";
  condition

let follows_conditional = function When _ | Else _ -> true | _ -> false

let scope_prelude r prelude_components : Selector.t option * Selector.t option =
  let prelude = Cursor.string_of_components ~trim:true prelude_components in
  let rec split_at_to seen rest =
    match rest with
    | [] -> (List.rev seen, [])
    | Component.Preserved { kind = Token.Ident s; _ } :: tail
      when String.lowercase_ascii s = "to" ->
        (List.rev seen, tail)
    | hd :: tail -> split_at_to (hd :: seen) tail
  in
  let strip_parens cvs =
    match Cursor.string_of_components ~trim:true cvs with
    | s
      when String.length s >= 2 && s.[0] = '(' && s.[String.length s - 1] = ')'
      ->
        (true, String.sub s 1 (String.length s - 2) |> String.trim)
    | s -> (false, s)
  in
  if prelude = "" then (Option.None, Option.None)
  else
    let start_cvs, end_cvs = split_at_to [] prelude_components in
    let start_parens, start = strip_parens start_cvs in
    let end_parens, end_ = strip_parens end_cvs in
    if start_parens && start = "" then
      Cursor.err_invalid r "@scope start selector cannot be empty";
    if end_cvs <> [] && end_parens && end_ = "" then
      Cursor.err_invalid r "@scope end selector cannot be empty";
    (* Parse each bound into a selector; an unparseable bound is
       [Selector.Invalid]. *)
    let opt s : Selector.t option =
      if s = "" then None
      else
        match Selector.of_string s with
        | sel -> Some sel
        | exception (Cursor.Parse_error _ | Invalid_argument _) ->
            Some Selector.Invalid
    in
    (opt start, opt end_)

let read_supports_condition (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "supports-condition" r;
  Cursor.ws r;
  let name = Cursor.ident ~keep_case:true r in
  if not (Custom_property_name.is_valid name) then
    Cursor.err_invalid r "@supports-condition: invalid custom-property name";
  Cursor.ws r;
  let declarations = Cursor.braces Declaration.read_declarations r in
  Supports_condition (name, declarations)

let read_layer_name (r : Cursor.t) : layer_name = read_layer_name_component r

let read_nested_media_condition t =
  if Cursor.is_done t then Media.List []
  else
    try Media.read ~recover:false t
    with Error.Parse_error _ -> Media.of_string "not all"

let read_rule_selector ?(nested = false) r =
  let prelude = Cursor.drain_until_block r in
  (* Anchor selector-level EOF errors at the block's opening delimiter so a
     prelude like [.a >] reports its error just before the [{], not at the end
     of the whole stylesheet. The block itself stays in the parent cursor. *)
  let eof_loc =
    match Cursor.peek r with
    | Some (Component.Block { loc; _ }) ->
        Some (Loc.v ~start_pos:loc.start_pos ~end_pos:loc.start_pos)
    | _ -> None
  in
  let c = Cursor.sub ?eof_loc r prelude in
  (* Re-raise the original [Parse_error] so its loc/kind/path/snippet reach the
     caller intact, rather than rewrapping (which erases the structured error
     and relocates it). CSS Nesting 1 sec. 3: a nested rule's prelude is a
     [<relative-selector-list>], so it may start with a combinator ([> .bar])
     implicitly relative to the parent [&]. *)
  Cursor.with_context c "selector" (fun () ->
      if nested then Selector.read_relative c
      else Selector.read_strict_selector_list c)

let is_bare_nesting_selector : Selector.t -> bool = function
  | Selector.Nesting -> true
  | _ -> false

let validate_nested_rule_selector inner selector nested_selector =
  if
    is_bare_nesting_selector selector
    && is_bare_nesting_selector nested_selector
  then
    Cursor.err_invalid inner
      "bare nesting selector cannot directly nest another bare nesting selector"

(* CSS Nesting 1 sec. 3.3: "any at-rule whose body contains style rules can be
   nested inside of a style rule as well, unless otherwise specified". A
   descriptor rule, a keyframe list and a declaration-list rule contain none, so
   none of them nests, and Blink 146 drops each one. [\@view-transition] is a
   descriptor rule too, yet Blink keeps it inside a style rule; dropping what a
   shipping engine still reads is the one lossy direction, so it stays. *)
let nests_in_style_rule = function
  | "keyframes" | "-webkit-keyframes" | "-moz-keyframes" | "font-face"
  | "counter-style" | "page" | "font-palette-values" | "font-feature-values"
  | "position-try" | "viewport" | "-ms-viewport" | "property"
  | "supports-condition" ->
      false
  | _ -> true

(* One descriptor of an [@property] body. The state is returned rather than
   assigned as the value is read, so a descriptor that fails leaves the ones
   before it untouched. CSS Properties and Values API 1 sec. 2 gives each
   descriptor a grammar of its own, and the declaration is the whole of it: a
   trailing [!important] or a stray ident makes the declaration invalid rather
   than the leftover alone, as it does in Blink 146. *)
let read_property_descriptor (r : Cursor.t) state =
  let key = Cursor.ident ~keep_case:false r in
  Cursor.ws r;
  if not (Cursor.colon r) then Cursor.err_expected r "':'";
  Cursor.ws r;
  let state =
    match key with
    | "syntax" -> { state with syntax = Some (Variables.read_syntax r) }
    | "inherits" -> { state with inherits = Some (Cursor.bool r) }
    | "initial-value" ->
        {
          state with
          initial_value = Some (Cursor.consume_until_semicolon ~trim:true r);
        }
    | _ -> Cursor.err_invalid r "unknown property descriptor"
  in
  Cursor.ws r;
  if not (Cursor.is_done r || Cursor.peek_semicolon r) then
    Cursor.err_invalid r ("trailing tokens after @property " ^ key);
  state

let read_property_step r state =
  Cursor.ws r;
  if Cursor.is_done r then `Done state
  else if Cursor.peek_semicolon r then (
    Cursor.skip r;
    `More state)
  else `More (read_property_descriptor r state)

let read_property_descriptors (r : Cursor.t) : property_reader_state =
  read_items_with_recovery ~skip:skip_invalid_item
    ~construct:Error.Recovery.Declaration read_property_step r
    { syntax = None; inherits = None; initial_value = None }

let read_property_rule (r : Cursor.t) : statement =
  (* Read @property descriptors as a separate helper to keep the reader tidy. *)
  Cursor.expect_at_keyword "property" r;
  Cursor.ws r;
  let name = Cursor.ident ~keep_case:true r in
  if not (Custom_property_name.is_valid name) then
    Cursor.err_invalid r ("@property: invalid custom-property name: " ^ name);
  Cursor.ws r;
  let state = Cursor.braces (fun inner -> read_property_descriptors inner) r in
  match (state.syntax, state.inherits) with
  | None, _ ->
      Cursor.err_invalid r "@property: missing required 'syntax' descriptor"
  | _, None ->
      Cursor.err_invalid r "@property: missing required 'inherits' descriptor"
  | Some (Variables.Syntax syntax), Some inherits ->
      let is_universal_syntax =
        match syntax with Universal -> true | _ -> false
      in
      let initial_value =
        match state.initial_value with
        | None when not is_universal_syntax ->
            Cursor.err_invalid r
              "@property: initial-value is required for non-universal syntax"
        | None -> Option.None
        | Some str -> Some (read_property_initial_value r syntax str)
      in
      Property { name; syntax; inherits; initial_value }

(* Does the item ahead hold a curly block before its terminating [;]? Then it is
   a nested rule, not a declaration, however much its prelude looks like one.
   Leaves the cursor where it found it. *)
(* CSS Syntax 3 (ED) sec. 5.5.3 consumes a qualified rule whole, block and all,
   before deciding it is invalid, so sec. 5.5.5 resumes right after that block.
   Recovering a rule to the next semicolon, which is what sec. 5.5.6 does for a
   bad declaration, would take the items written after it. The caller rewinds
   first so the item is skipped from its start. *)
let item_opens_block inner =
  let start = Cursor.save inner in
  let rec scan () =
    match Cursor.peek inner with
    | None -> false
    | Some (Component.Preserved { kind = Token.Semicolon; _ }) -> false
    | Some (Component.Block { node = { opening = Token.Curly; _ }; _ }) -> true
    | Some _ ->
        Cursor.skip inner;
        scan ()
  in
  let found = scan () in
  Cursor.restore inner start;
  found

let skip_invalid_nesting_item r =
  if item_opens_block r then (
    skip_past_rule r;
    Error.Recovery.Rule)
  else (
    Cursor.skip_past_semicolon r;
    Error.Recovery.Declaration)

(* Discard an at-rule that is invalid in a style rule and resume at the next
   item, the recovery CSS Syntax 3 (ED) sec. 5.5.5 describes. The warning is
   what [~strict:true] turns into an error. *)
let drop_nested_at_rule r ~loc reason : statement option =
  skip_past_rule r;
  Cursor.push_warning r
    ~recovery:Error.Recovery.(dropped Rule)
    (Error.bad_value loc ~property:"rule" ~reason);
  None

(* CSS Nesting 1 sec. 3.4 wraps a run of declarations written after a nested
   statement in a nested declarations rule, so it keeps its place among them.
   The run a rule body is reading is the head of its [nested] accumulator and
   holds its declarations reversed, like [decls]; sealing it puts them back in
   source order. *)
let seal_declaration_run = function
  | Declarations run :: rest -> Declarations (List.rev run) :: rest
  | nested -> nested

(* Add one declaration to the run being read, which is the head of the
   accumulator and holds its declarations reversed until it is sealed. *)
let add_to_declaration_run decl = function
  | Declarations run :: rest -> Declarations (decl :: run) :: rest
  | nested -> Declarations [ decl ] :: nested

let read_starting_style ~body (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "starting-style" r;
  Cursor.ws r;
  Starting_style (Cursor.braces body r)

let read_when ~body (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "when" r;
  Cursor.ws r;
  let prelude = Cursor.drain_until_block r in
  if Cursor.string_of_components ~trim:true prelude = "" then
    Cursor.err_condition r ~at_rule:"@when" "missing condition";
  let condition : conditional =
    conditional_components ~at_rule:"@when" (Cursor.sub r prelude)
  in
  When (condition, Cursor.braces body r)

let read_else ~body (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "else" r;
  Cursor.ws r;
  let prelude = Cursor.drain_until_block r in
  let condition =
    match
      List.filter
        (function
          | Component.Preserved { kind = Token.Whitespace; _ } -> false
          | _ -> true)
        prelude
    with
    | [] -> Option.None
    | _ -> Some (conditional_components ~at_rule:"@else" (Cursor.sub r prelude))
  in
  Else (condition, Cursor.braces body r)

let read_moz_document ~body (r : Cursor.t) : statement =
  Cursor.with_context r "@-moz-document" @@ fun () ->
  Cursor.expect_at_keyword "-moz-document" r;
  Cursor.ws r;
  let prelude = Cursor.drain_until_block_as_string ~trim:true r in
  let prelude_cursor = Cursor.of_string prelude in
  let conditions =
    Cursor.list ~sep:Cursor.comma ~at_least:1 read_moz_document_condition
      prelude_cursor
  in
  Cursor.ws prelude_cursor;
  Cursor.expect_eof prelude_cursor;
  Moz_document (conditions, Cursor.braces body r)

(* CSS Values 4 sec. 4.1 reads an at-rule name as a keyword, so its case does
   not pick between two grammars. CSS Syntax 3 (ED) sec. 8.3 is the one
   exception: it matches [@charset] and the quote after it as an exact byte
   sequence, so any other spelling of that name is a name with no grammar behind
   it. *)
let at_rule_keyword name =
  match Common.String.lowercase_ascii_preserve name with
  | "charset" when name <> "charset" -> name
  | folded -> folded

let rec read_statement (r : Cursor.t) : statement =
  Cursor.ws r;
  let table : (string * (Cursor.t -> statement)) list =
    [
      ("charset", read_charset);
      ("import", read_import);
      ("namespace", read_namespace);
      ("layer", read_layer);
      ("media", read_media);
      ("container", read_container);
      ("supports", read_supports);
      ("-moz-document", read_moz_document ~body:read_block);
      ("when", read_when ~body:read_block);
      ("else", read_else ~body:read_block);
      ("supports-condition", read_supports_condition);
      ("starting-style", read_starting_style ~body:read_block);
      ("scope", read_scope);
      ("keyframes", read_keyframes);
      ("-webkit-keyframes", read_webkit_keyframes);
      ("-moz-keyframes", read_moz_keyframes);
      ("font-face", read_font_face);
      ("counter-style", read_counter_style);
      ("page", read_page);
      ("font-palette-values", read_font_palette_values);
      ("font-feature-values", read_font_feature_values);
      ("view-transition", read_view_transition);
      ("position-try", read_position_try);
      ("viewport", read_viewport);
      ("-ms-viewport", read_ms_viewport);
      ("property", read_property_rule);
    ]
  in
  match Cursor.peek r with
  | Some (Component.Preserved { kind = Token.At_keyword name; loc; _ }) -> (
      match List.assoc_opt (at_rule_keyword name) table with
      | Some p -> p r
      | None ->
          (* CSS Syntax 3 (ED) sec. 5.5.2 consumes an at-rule whatever its name,
             so the prelude and block stay in the AST as [Unknown_at_rule] and
             reach the output. The typed warning tells the caller cascade could
             not interpret it; [Optimize.drop_unknown_at_rules] is there for a
             caller that wants it gone. *)
          ignore (Cursor.next_raw r);
          let stmt = read_unknown_at_rule name r in
          Cursor.push_warning r ~recovery:Error.Recovery.Recovered
            (Error.unknown_at_rule loc name);
          stmt)
  | _ -> Rule (read_rule r)

and read_block (r : Cursor.t) : block =
  let rec read_statements acc =
    Cursor.ws r;
    if Cursor.is_done r then List.rev acc
    else
      let loc = Cursor.position r in
      let snap = Cursor.save r in
      match read_statement r with
      (* CSS Syntax 3 (ED) sec. 5.5.1: a rule that fails to parse (e.g. an
         invalid selector) is dropped, and parsing resumes at the next rule -
         one bad rule must not take the rest of the [@layer] / [@media] block
         with it. Strict mode ([not (Cursor.recover r)]) still raises. *)
      | exception Error.Parse_error e when Cursor.recover r ->
          Cursor.restore r snap;
          Cursor.push_warning r ~recovery:Error.Recovery.(dropped Rule) e;
          skip_past_rule r;
          read_statements acc
      | Import _ ->
          (* CSS Cascade L6 sec. 2: @import is only valid at the top of the
             stylesheet. Drop a misplaced one rather than emitting it. *)
          Cursor.push_warning r
            ~recovery:Error.Recovery.(dropped Rule)
            (Error.bad_value loc ~property:"stylesheet"
               ~reason:"@import is only valid at the top of a stylesheet");
          read_statements acc
      | Else _ when not (List.exists follows_conditional acc) ->
          Cursor.err_invalid r "@else without preceding @when"
      | stmt -> read_statements (stmt :: acc)
  in
  read_statements []

and read_media (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "media" r;
  Cursor.ws r;
  let condition_components = Cursor.drain_until_block r in
  let query = Cursor.sub r condition_components in
  let content = Cursor.braces (fun inner -> read_block inner) r in
  let condition =
    if Cursor.is_done query then Media.List []
    else Media.read ~recover:false query
  in
  Media (condition, content)

and read_supports (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "supports" r;
  Cursor.ws r;
  let query = Cursor.sub r (Cursor.drain_until_block r) in
  if Cursor.is_done query then
    Cursor.err r "@supports rule requires a condition";
  let content = Cursor.braces (fun inner -> read_block inner) r in
  Supports (Supports.read query, content)

and read_scope (r : Cursor.t) : statement =
  (* CSS Cascade 6 sec. 3.5.2: [@scope <start> to <end> { ... }]. The two
     selectors are kept as raw strings; the block is consumed normally. *)
  Cursor.expect_at_keyword "scope" r;
  Cursor.ws r;
  let prelude_components = Cursor.drain_until_block r in
  let scope_start, scope_end = scope_prelude r prelude_components in
  let content = Cursor.braces (fun inner -> read_block inner) r in
  Scope (scope_start, scope_end, content)

and read_container (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "container" r;
  Cursor.ws r;
  (* CSS Containment 3 section 4: [<container-name>] is a [<custom-ident>] that
     excludes the keywords [none], [and], [not], [or]. Without this filter,
     [@container not (width)] would parse [not] as the name and accept the
     malformed [(width)] as the query. *)
  let container_name : string option =
    let snap = Cursor.save r in
    match Cursor.option Cursor.ident r with
    | Some name
      when List.mem (String.lowercase_ascii name) [ "none"; "and"; "not"; "or" ]
      ->
        Cursor.restore r snap;
        Option.None
    | other -> other
  in
  Cursor.ws r;
  let query = Cursor.sub r (Cursor.drain_until_block r) in
  let content = Cursor.braces (fun inner -> read_block inner) r in
  let condition : Container.t option =
    if Cursor.is_done query then Option.None else Some (Container.read query)
  in
  (* CSS Containment 3 section 4: [@container] requires a query (with an
     optional [<container-name>] in front). Bare [@container { ... }] is a parse
     error. *)
  if container_name = None && condition = None then
    Cursor.err_invalid r "@container requires a container query";
  Container (container_name, condition, content)

and read_layer (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "layer" r;
  Cursor.ws r;
  match Cursor.peek r with
  | Some (Component.Block { node = { opening = Token.Curly; _ }; _ }) ->
      let content = Cursor.braces (fun inner -> read_block inner) r in
      Layer (None, content)
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
      Cursor.err_invalid r "@layer requires at least one name"
  | _ -> (
      let first = read_layer_name r in
      Cursor.ws r;
      match Cursor.peek r with
      | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
          Cursor.skip r;
          Layer_decl [ first ]
      | Some (Component.Preserved { kind = Token.Comma; _ }) ->
          Cursor.skip r;
          Cursor.ws r;
          let rest =
            Cursor.list ~sep:Cursor.comma ~at_least:1
              (fun r ->
                Cursor.ws r;
                read_layer_name r)
              r
          in
          Cursor.ws r;
          (match Cursor.peek r with
          | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
              Cursor.skip r
          | Some (Component.Block { node = { opening = Token.Curly; _ }; _ }) ->
              Cursor.err_invalid r
                "@layer with multiple names cannot have a block"
          | _ -> ());
          Layer_decl (first :: rest)
      | Some (Component.Block { node = { opening = Token.Curly; _ }; _ }) ->
          let content = Cursor.braces (fun inner -> read_block inner) r in
          Layer (Some first, content)
      | _ -> Cursor.err_invalid r "expected ';' or '{' after @layer name")

(* Helper: Read a block that can contain either bare declarations or statements.
   Used for CSS nesting contexts where content inside @media/@supports/etc can
   be either bare declarations (inheriting the parent selector) or nested
   rules. *)
and read_nesting_block (r : Cursor.t) : block =
  let rec read_items acc =
    Cursor.ws r;
    if Cursor.recover r then read_recovering_item acc
    else add_item acc (read_nesting_item ~prev:acc r)
  (* CSS Syntax 3 (ED) sec. 5.5.5: a declaration that fails to parse is dropped
     and reading resumes past the next top-level [;], a [{}] met on the way
     counting as one component value of the value being skipped. A nested
     at-rule's body is <block-contents> like a style rule's, so it recovers the
     same way and one bad declaration takes neither the group rule holding it
     nor the rest of the sheet. Strict mode ([not (Cursor.recover r)]) still
     raises. *)
  and read_recovering_item acc =
    let start = Cursor.save r in
    match read_nesting_item ~prev:acc r with
    | item -> add_item acc item
    | exception Error.Parse_error e ->
        Cursor.restore r start;
        let construct = skip_invalid_nesting_item r in
        Cursor.push_warning r
          ~recovery:(Cursor.dropped_since r start construct)
          e;
        read_items acc
  and add_item acc = function
    | `Done -> List.rev (seal_declaration_run acc)
    | `Skip -> read_items acc
    | `Decl decl -> read_items (add_to_declaration_run decl acc)
    | `Stmt stmt -> read_items (stmt :: seal_declaration_run acc)
  in
  read_items []

and read_nesting_item ~prev r =
  match Cursor.peek r with
  | None -> `Done
  | Some (Component.Preserved { kind = Token.At_keyword _; _ }) -> (
      match read_nested_at_within_rule ~prev r with
      | Some stmt -> `Stmt stmt
      | None -> `Skip)
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
      Cursor.skip r;
      `Skip
  | _ -> read_nesting_declaration_or_statement r

and read_nesting_declaration_or_statement r =
  let start = Cursor.save r in
  match Declaration.read_declaration r with
  | Some decl ->
      Cursor.ws r;
      if Cursor.peek_semicolon r then Cursor.skip r;
      `Decl decl
  | None -> `Stmt (read_statement r)
  (* CSS Nesting 1 sec. 3 lets a nested rule start with an identifier, so
     [h2:where(...) { ... }] reads as a declaration up to the [{]. Rewind and
     take it as a rule; a genuine bad declaration has no block and still reports
     as one. *)
  | exception Error.Parse_error _
    when Cursor.restore r start;
         item_opens_block r ->
      `Stmt (read_statement r)

(* Helper: Read nested at-rule with declarations content *)
and read_nested_at_rule (r : Cursor.t) (at_rule : string) : statement =
  Cursor.with_context r at_rule @@ fun () ->
  let name = String.sub at_rule 1 (String.length at_rule - 1) in
  Cursor.expect_at_keyword name r;
  Cursor.ws r;
  match at_rule with
  | "@container" -> read_nested_container_rule r
  | "@supports" -> read_nested_supports_rule r
  | "@media" -> read_nested_media_rule r
  | "@scope" -> read_nested_scope_rule r
  | _ -> Cursor.err_invalid r ("Unexpected nested at-rule: " ^ at_rule)

and read_nested_container_rule r =
  let container_name : string option = Cursor.option Cursor.ident r in
  Cursor.ws r;
  let query = Cursor.sub r (Cursor.drain_until_block r) in
  let content = Cursor.braces (fun inner -> read_nesting_block inner) r in
  let condition : Container.t option =
    if Cursor.is_done query then Option.None else Some (Container.read query)
  in
  Container (container_name, condition, content)

and read_nested_supports_rule r =
  let query = Cursor.sub r (Cursor.drain_until_block r) in
  let content = Cursor.braces (fun inner -> read_nesting_block inner) r in
  Supports (Supports.read query, content)

and read_nested_media_rule r =
  let query = Cursor.sub r (Cursor.drain_until_block r) in
  let content = Cursor.braces (fun inner -> read_nesting_block inner) r in
  Media (read_nested_media_condition query, content)

and read_nested_scope_rule r =
  let prelude_components = Cursor.drain_until_block r in
  let scope_start, scope_end = scope_prelude r prelude_components in
  let content = Cursor.braces (fun inner -> read_nesting_block inner) r in
  Scope (scope_start, scope_end, content)

and read_nested_layer_rule r =
  Cursor.expect_at_keyword "layer" r;
  Cursor.ws r;
  match Cursor.peek r with
  | Some (Component.Block { node = { opening = Token.Curly; _ }; _ }) ->
      Layer (None, Cursor.braces (fun inner -> read_nesting_block inner) r)
  | _ ->
      let name = read_layer_name r in
      Cursor.ws r;
      Layer (Some name, Cursor.braces (fun inner -> read_nesting_block inner) r)

and read_nested_at_within_rule ~prev (r : Cursor.t) : statement option =
  match Cursor.peek r with
  | Some (Component.Preserved { kind = Token.At_keyword name; loc; _ }) ->
      read_nested_at_keyword r ~loc ~prev (at_rule_keyword name)
  | _ -> Some (read_statement r)

(* CSS Nesting 1 sec. 3.3: "any at-rule whose body contains style rules can be
   nested inside of a style rule as well", which is every group rule cascade
   models - the conditional group rules @media/@supports/@container, @when and
   @else that generalise them (CSS Conditional 5 sec. 3, sec. 4),
   @-moz-document, @layer, @scope, and @starting-style (CSS Transitions 2 sec.
   3.3). Nested, the body is <block-contents>, so a bare declaration run in it
   belongs to the parent selector. An at-rule with no style rule in its body is
   invalid here and dropped; a top-of-sheet rule is invalid here outright. *)
and read_nested_at_keyword r ~loc ~prev = function
  | ("supports" | "media" | "container" | "scope") as name ->
      Some (read_nested_at_rule r ("@" ^ name))
  | "layer" -> Some (read_nested_layer_rule r)
  | "starting-style" -> Some (read_starting_style ~body:read_nesting_block r)
  | "-moz-document" -> Some (read_moz_document ~body:read_nesting_block r)
  | "when" -> Some (read_when ~body:read_nesting_block r)
  | "else" when List.exists follows_conditional prev ->
      Some (read_else ~body:read_nesting_block r)
  | "else" -> drop_nested_at_rule r ~loc "@else without preceding @when"
  | ("charset" | "import" | "namespace") as name ->
      drop_nested_at_rule r ~loc
        ("@" ^ name ^ " is only valid at the top of a stylesheet")
  | name when not (nests_in_style_rule name) ->
      drop_nested_at_rule r ~loc
        ("@" ^ name ^ " has no style rule in it, so it does not nest")
  | _ -> Some (read_statement r)

and read_rule_item selector inner decls nested =
  match Cursor.peek inner with
  | None -> `Done (List.rev decls, List.rev (seal_declaration_run nested))
  | Some (Component.Preserved { kind = Token.At_keyword _; _ }) -> (
      match read_nested_at_within_rule ~prev:nested inner with
      | Some stmt -> `Continue (decls, stmt :: seal_declaration_run nested)
      (* A dropped at-rule leaves no gap: the declaration run written around it
         is one run, as it is in Blink. *)
      | None -> `Continue (decls, nested))
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
      Cursor.skip inner;
      `Continue (decls, nested)
  | _ -> read_rule_decl_or_nested selector inner decls nested

and read_rule_decl_or_nested selector inner decls nested =
  let start = Cursor.save inner in
  match Declaration.read_declaration inner with
  | Some d -> (
      Cursor.ws inner;
      if Cursor.peek_semicolon inner then Cursor.skip inner;
      (* Only the run written before the first nested statement is the rule's
         own; a later one joins the block it follows. *)
      match nested with
      | [] -> `Continue (d :: decls, nested)
      | Declarations run :: rest ->
          `Continue (decls, Declarations (d :: run) :: rest)
      | _ -> `Continue (decls, Declarations [ d ] :: nested))
  | None -> read_nested_rule_or_done selector inner decls nested
  (* CSS Nesting 1 sec. 3 lets a nested rule start with an identifier, so
     [h2:where(...) { ... }] reads as a declaration up to the [{]. Rewind and
     take it as a rule; a genuine bad declaration has no block and still reports
     as one. *)
  | exception Error.Parse_error _
    when Cursor.restore inner start;
         item_opens_block inner ->
      read_nested_rule_or_done selector inner decls nested

and read_nested_rule_or_done selector inner decls nested =
  if Cursor.is_done inner then
    `Done (List.rev decls, List.rev (seal_declaration_run nested))
  else
    let nr = read_rule ~nested:true inner in
    validate_nested_rule_selector inner selector nr.selector;
    `Continue (decls, Rule nr :: seal_declaration_run nested)

and read_rule_body selector inner =
  let rec loop decls nested =
    Cursor.ws inner;
    if Cursor.recover inner then
      read_recovering_rule_item selector inner loop decls nested
    else
      match read_rule_item selector inner decls nested with
      | `Done result -> result
      | `Continue (decls, nested) -> loop decls nested
  in
  loop [] []

and read_recovering_rule_item selector inner loop decls nested =
  let start = Cursor.save inner in
  match read_rule_item selector inner decls nested with
  | `Done result -> result
  | `Continue (decls, nested) -> loop decls nested
  | exception Error.Parse_error e ->
      Cursor.restore inner start;
      let construct = skip_invalid_nesting_item inner in
      Cursor.push_warning inner
        ~recovery:(Cursor.dropped_since inner start construct)
        e;
      loop decls nested

and read_rule ?(nested = false) (r : Cursor.t) : rule =
  Cursor.with_context r "rule" @@ fun () ->
  let selector = read_rule_selector ~nested r in
  let declarations, nested = Cursor.braces (read_rule_body selector) r in
  { selector; declarations; nested; merge_key = None }

type prelude_seen = {
  mutable charset_seen : bool;
  mutable import_seen : bool;
  mutable namespace_seen : bool;
  mutable body_seen : bool;
}

let new_prelude_seen () =
  {
    charset_seen = false;
    import_seen = false;
    namespace_seen = false;
    body_seen = false;
  }

let validate_stylesheet_prelude r state stmt =
  match stmt with
  | Charset _ ->
      if
        state.charset_seen || state.import_seen || state.namespace_seen
        || state.body_seen
      then Cursor.err_invalid r "@charset must precede all rules";
      state.charset_seen <- true
  | Layer_decl _ ->
      if state.import_seen || state.namespace_seen then state.body_seen <- true
  | Import _ ->
      if state.namespace_seen || state.body_seen then
        Cursor.err_invalid r "@import must precede style rules";
      state.import_seen <- true
  | Namespace _ ->
      if state.body_seen then
        Cursor.err_invalid r "@namespace must precede style rules";
      state.namespace_seen <- true
  | _ -> state.body_seen <- true

let validate_stylesheet_else r acc stmt =
  match stmt with
  | Else _ when not (List.exists follows_conditional acc) ->
      Cursor.err_invalid r "@else without preceding @when"
  | _ -> ()

let rec read_stylesheet_statements r state acc =
  Cursor.ws r;
  if Cursor.is_done r then List.rev acc
  else
    let stmt = read_statement r in
    validate_stylesheet_else r acc stmt;
    validate_stylesheet_prelude r state stmt;
    read_stylesheet_statements r state (stmt :: acc)

let read (r : Cursor.t) : stylesheet =
  Cursor.with_context r "stylesheet" (fun () ->
      read_stylesheet_statements r (new_prelude_seen ()) [])

(* Replay a Parser-recovered rule as a Cursor by flattening its components.
   [Component.at_rule] stores the at-keyword as a bare [name] string; the
   original [At_keyword] token was already consumed during section 5.3, so
   synthesize one at the rule's location so dispatch in [read_statement] still
   sees the opening [\@name]. [?source] flows through so errors raised by
   validators downstream pick up source-context snippets. *)
let cursor_of_rule ?source ?meta : Component.rule -> Cursor.t = function
  | Qualified { node = { prelude; block }; _ } ->
      Cursor.of_components ?source ?meta ~recover:true
        (prelude @ [ Component.Block block ])
  | At { node = { name; prelude; block }; loc } ->
      let at_kw = Token.v ~kind:(Token.At_keyword name) ~loc in
      let at_cv = Component.Preserved at_kw in
      let tail_cv =
        match block with
        | Some b -> [ Component.Block b ]
        | None ->
            (* Section 5.5.2 "consume an at-rule" terminates on [;] or EOF and
               drops the terminator. Readers like [read_layer] and
               [read_charset] then expect a trailing semicolon; synthesize one
               so the replayed cursor matches the original token shape
               regardless of whether [;] or EOF ended the rule. *)
            [ Component.Preserved (Token.v ~kind:Token.Semicolon ~loc) ]
      in
      Cursor.of_components ?source ?meta ~recover:true
        ((at_cv :: prelude) @ tail_cv)

(* Validate one Parser-recovered rule to a typed statement, or convert the
   validator's [Parse_error] into a rule-level error and drop the rule. Per-
   declaration warnings accumulated on the cursor are drained separately by the
   caller. *)
let read_statement_of_rule ?source ?meta (rule : Component.rule) :
    Cursor.t * (statement, Error.t) result =
  let r = cursor_of_rule ?source ?meta rule in
  let result =
    match read_statement r with
    | stmt -> Ok stmt
    | exception Error.Parse_error e -> Error e
  in
  (r, result)

let rule_loc : Component.rule -> Loc.t = function
  | Qualified { loc; _ } | At { loc; _ } -> loc

let validate_partial_prelude () =
  let charset_seen = ref false in
  let import_seen = ref false in
  let namespace_seen = ref false in
  let body_seen = ref false in
  fun loc stmt ->
    let warning reason =
      Some (Error.bad_value loc ~property:"stylesheet" ~reason)
    in
    match stmt with
    | Charset _ ->
        let error =
          if !charset_seen || !import_seen || !namespace_seen || !body_seen then
            warning "@charset must precede all rules"
          else None
        in
        charset_seen := true;
        error
    | Layer_decl _ ->
        if !import_seen || !namespace_seen then body_seen := true;
        None
    | Import _ ->
        let error =
          if !namespace_seen || !body_seen then
            warning "@import must precede style rules"
          else None
        in
        import_seen := true;
        error
    | Namespace _ ->
        let error =
          if !body_seen then warning "@namespace must precede style rules"
          else None
        in
        namespace_seen := true;
        error
    | _ ->
        body_seen := true;
        None

let validate_partial_statement loc = function
  | Rule { selector; _ } when Selector.matches_nothing selector ->
      Some (Error.bad_selector loc "selector matches nothing")
  | Font_face descriptors when not (font_face_participates descriptors) ->
      Some
        (Error.bad_value loc ~property:"@font-face"
           ~reason:"missing font-family or src descriptor")
  (* CSS Fonts 4 sec. 9.2 makes font-family the one mandatory descriptor; sec.
     9.2.2 defaults a missing base-palette to 0. *)
  | Font_palette_values (_, descriptors)
    when not
           (List.exists
              (function Palette_font_family _ -> true | _ -> false)
              descriptors) ->
      Some
        (Error.bad_value loc ~property:"@font-palette-values"
           ~reason:"missing font-family descriptor")
  | (Keyframes (name, _) | Webkit_keyframes (name, _) | Moz_keyframes (name, _))
    when List.mem
           (String.lowercase_ascii name)
           [ "none"; "initial"; "inherit"; "unset"; "revert"; "revert-layer" ]
    ->
      Some
        (Error.bad_value loc ~property:"@keyframes"
           ~reason:"forbidden keyframes name")
  | _ -> None

let rec statement_has_invalid_declaration = function
  | Rule { declarations; nested; _ } ->
      List.exists Declaration.is_invalid declarations
      || List.exists statement_has_invalid_declaration nested
  | Declarations decls -> List.exists Declaration.is_invalid decls
  | Media (_, block)
  | Container (_, _, block)
  | Supports (_, block)
  | Moz_document (_, block)
  | When (_, block)
  | Else (_, block)
  | Layer (_, block)
  | Starting_style block
  | Origin (_, block)
  | Scope (_, _, block) ->
      List.exists statement_has_invalid_declaration block
  | Page (_, decls) -> List.exists Declaration.is_invalid decls
  | Page_with_margins (_, descs, margins) ->
      List.exists Declaration.is_invalid descs
      || List.exists
           (fun { descriptors; _ } ->
             List.exists Declaration.is_invalid descriptors)
           margins
  | Position_try (_, decls) | Supports_condition (_, decls) ->
      List.exists Declaration.is_invalid decls
  | _ -> false

let validate_partial_invalid_declarations loc stmt =
  if statement_has_invalid_declaration stmt then
    Some
      (Error.bad_value loc ~property:"stylesheet"
         ~reason:"invalid declaration value")
  else None

let rec declaration_has_strict_warning = function
  | Declaration.Declaration { property = Properties.Color; value; _ } ->
      Values.color_has_specified_hue value
  | Declaration.Theme_guarded { decl; _ } -> declaration_has_strict_warning decl
  | _ -> false

let rec statement_has_strict_warning = function
  | Rule { declarations; nested; _ } ->
      List.exists declaration_has_strict_warning declarations
      || List.exists statement_has_strict_warning nested
  | Declarations decls -> List.exists declaration_has_strict_warning decls
  | Media (_, block)
  | Container (_, _, block)
  | Supports (_, block)
  | Moz_document (_, block)
  | When (_, block)
  | Else (_, block)
  | Layer (_, block)
  | Starting_style block
  | Origin (_, block)
  | Scope (_, _, block) ->
      List.exists statement_has_strict_warning block
  | Page (_, decls) -> List.exists declaration_has_strict_warning decls
  | Page_with_margins (_, descs, margins) ->
      List.exists declaration_has_strict_warning descs
      || List.exists
           (fun { descriptors; _ } ->
             List.exists declaration_has_strict_warning descriptors)
           margins
  | Position_try (_, decls) | Supports_condition (_, decls) ->
      List.exists declaration_has_strict_warning decls
  | _ -> false

let validate_partial_strict_warnings loc stmt =
  if statement_has_strict_warning stmt then
    Some
      (Error.bad_value loc ~property:"stylesheet"
         ~reason:"strict stylesheet warning")
  else None

(* A warning drained from a cursor is stamped where the cursor recorded it. *)
let add_warning warnings warning = warnings := warning :: !warnings

(* A warning raised beside the statement rather than inside it, so this site
   says what became of the statement. *)
let add_statement_warning warnings ~recovery warning =
  add_warning warnings (Error.with_recovery recovery warning)

let drain_statement_warnings warnings cursor =
  List.iter (add_warning warnings) (Cursor.drain_warnings cursor)

let validate_else_orphan previous rule stmt =
  match stmt with
  | Else _
    when not (Option.fold ~none:false ~some:follows_conditional !previous) ->
      Some
        (Error.bad_value (rule_loc rule) ~property:"stylesheet"
           ~reason:"@else without preceding @when")
  | _ -> None

(* These four run on a statement the reader read whole and the sheet keeps, so
   the material each reports on reaches the output. *)
let validate_partial_statement_warnings warnings validate_prelude rule stmt =
  let loc = rule_loc rule in
  let add =
    Option.iter
      (add_statement_warning warnings ~recovery:Error.Recovery.Recovered)
  in
  add (validate_prelude loc stmt);
  add (validate_partial_statement loc stmt);
  add (validate_partial_invalid_declarations loc stmt);
  add (validate_partial_strict_warnings loc stmt)

let read_stylesheet_of_rules ?source ?meta (rules : Component.rule list) :
    stylesheet * Error.t list =
  let warnings = ref [] in
  let validate_prelude = validate_partial_prelude () in
  (* CSS Conditional Rules 5 section 4: [@else] is a continuation rule and must
     follow [@when] or another [@else]. A bare top-level [@else] is a parse
     error. *)
  let previous : statement option ref = ref Option.None in
  let statements =
    List.filter_map
      (fun rule ->
        let cursor, result = read_statement_of_rule ?source ?meta rule in
        (* [rule_loc] spans the rule the parser handed over, prelude and block
           alike, so it names what a drop costs the sheet. The error's own
           location marks where the read gave up inside it. *)
        let lost () =
          Error.Recovery.(dropped ?source ~loc:(rule_loc rule) Rule)
        in
        (* Drain declaration-level warnings first so source order is preserved:
           decl warnings come from inside the rule, the rule-level error (if
           any) comes after them. *)
        drain_statement_warnings warnings cursor;
        match result with
        | Ok stmt -> (
            validate_partial_statement_warnings warnings validate_prelude rule
              stmt;
            match validate_else_orphan previous rule stmt with
            | Some w ->
                add_statement_warning warnings ~recovery:(lost ()) w;
                previous := Some stmt;
                None
            | None ->
                previous := Some stmt;
                Some stmt)
        | Error e ->
            (* [read_statement] refused the rule, so the sheet loses it whole:
               its text reaches no caller reading the parse. *)
            add_statement_warning warnings ~recovery:(lost ()) e;
            None)
      rules
  in
  (statements, List.rev !warnings)

module Source = struct
  type comment = { loc : Loc.t; terminated : bool }
  type rule = { syntax : Component.rule; loc : Loc.t; owned_loc : Loc.t }
  type position = { byte : int; line : int; column : int }
  type span = { start : position; end_ : position }

  type t = {
    original_source : string;
    preprocessed_source : string;
    comments : comment list;
    rules : rule list;
    trailing_loc : Loc.t;
    original_offsets : int array option;
    line_starts : int array;
  }

  let contents t = t.original_source
  let preprocessed t = t.preprocessed_source
  let comments t = t.comments
  let rules t = t.rules
  let trailing_loc t = t.trailing_loc

  let check_loc t ({ Loc.start_pos; end_pos } as loc) =
    let length = String.length t.preprocessed_source in
    if start_pos < 0 || end_pos < start_pos || end_pos > length then
      invalid_arg
        (String.concat ""
           [
             "Css.Source: location ";
             Loc.to_string loc;
             " is outside [0-";
             string_of_int length;
             "]";
           ])

  let slice t ({ Loc.start_pos; end_pos } as loc) =
    check_loc t loc;
    String.sub t.preprocessed_source start_pos (end_pos - start_pos)

  let original_offset t offset =
    if offset < 0 || offset > String.length t.preprocessed_source then
      invalid_arg
        (String.concat ""
           [
             "Css.Source: offset ";
             string_of_int offset;
             " is outside [0-";
             string_of_int (String.length t.preprocessed_source);
             "]";
           ]);
    match t.original_offsets with
    | Option.None -> offset
    | Option.Some map -> map.(offset)

  let original_loc t ({ Loc.start_pos; end_pos } as loc) =
    check_loc t loc;
    Loc.v
      ~start_pos:(original_offset t start_pos)
      ~end_pos:(original_offset t end_pos)

  let original_slice t loc =
    let { Loc.start_pos; end_pos } = original_loc t loc in
    String.sub t.original_source start_pos (end_pos - start_pos)

  let position t offset =
    let byte = original_offset t offset in
    let starts = t.line_starts in
    let low = ref 0 in
    let high = ref (Array.length starts) in
    while !low + 1 < !high do
      let middle = (!low + !high) / 2 in
      if starts.(middle) <= offset then low := middle else high := middle
    done;
    let line_start = starts.(!low) in
    let column =
      Common.String.utf8_length ~pos:line_start ~len:(offset - line_start)
        t.preprocessed_source
      + 1
    in
    { byte; line = !low + 1; column }

  let span t ({ Loc.start_pos; end_pos } as loc) =
    check_loc t loc;
    { start = position t start_pos; end_ = position t end_pos }

  let has_bom input =
    String.length input >= 3
    && input.[0] = '\xEF'
    && input.[1] = '\xBB'
    && input.[2] = '\xBF'

  (* Boundary map from the CSS-preprocessed buffer back to caller bytes. The two
     interior byte boundaries of a U+FFFD replacement map to the original NUL's
     start; lexer/component locations only occur at code-point boundaries, where
     the map is exact. *)
  let build_original_offsets original preprocessed =
    if String.equal original preprocessed then Option.None
    else
      let original_length = String.length original in
      let preprocessed_length = String.length preprocessed in
      let map = Array.make (preprocessed_length + 1) 0 in
      let original_pos = ref (if has_bom original then 3 else 0) in
      let preprocessed_pos = ref 0 in
      map.(0) <- !original_pos;
      let emit_byte original_end =
        incr preprocessed_pos;
        if !preprocessed_pos > preprocessed_length then
          invalid_arg "Css.Source: preprocessing map exceeds parsed source";
        map.(!preprocessed_pos) <- original_end
      in
      while !original_pos < original_length do
        match original.[!original_pos] with
        | '\x00' ->
            let start = !original_pos in
            incr original_pos;
            emit_byte start;
            emit_byte start;
            emit_byte !original_pos
        | '\r' ->
            incr original_pos;
            if
              !original_pos < original_length && original.[!original_pos] = '\n'
            then incr original_pos;
            emit_byte !original_pos
        | '\x0C' ->
            incr original_pos;
            emit_byte !original_pos
        | _ ->
            incr original_pos;
            emit_byte !original_pos
      done;
      if !preprocessed_pos <> preprocessed_length then
        invalid_arg "Css.Source: preprocessing map disagrees with parsed source";
      Option.Some map

  let line_starts source =
    let starts = ref [ 0 ] in
    String.iteri
      (fun index byte -> if byte = '\n' then starts := (index + 1) :: !starts)
      source;
    Array.of_list (List.rev !starts)

  let v ~original ~preprocessed ~comments syntax_rules =
    let previous_end = ref 0 in
    let rules =
      List.map
        (fun syntax ->
          let loc = rule_loc syntax in
          let owned_loc =
            Loc.v ~start_pos:!previous_end ~end_pos:loc.Loc.end_pos
          in
          previous_end := loc.Loc.end_pos;
          { syntax; loc; owned_loc })
        syntax_rules
    in
    let trailing_loc =
      Loc.v ~start_pos:!previous_end ~end_pos:(String.length preprocessed)
    in
    {
      original_source = original;
      preprocessed_source = preprocessed;
      comments =
        List.map
          (fun ({ Lexer.loc; terminated } : Lexer.comment) ->
            { loc; terminated })
          comments;
      rules;
      trailing_loc;
      original_offsets = build_original_offsets original preprocessed;
      line_starts = line_starts preprocessed;
    }
end

(* Scan [source] for top-level [/*! ... */] bang comments. The lexer drops
   ordinary comments per CSS Syntax 3 (ED) sec. 4.3.2; bang comments are the
   minifier convention for license headers and need to round-trip. Returns pairs
   [(start_offset, body)] in source order; nested comments inside strings or
   other comments are not handled because CSS comments don't nest. *)
let extract_bang_comments (source : string) : (int * string) list =
  let len = String.length source in
  let acc = ref [] in
  let i = ref 0 in
  while !i + 2 < len do
    if
      String.unsafe_get source !i = '/'
      && String.unsafe_get source (!i + 1) = '*'
      && String.unsafe_get source (!i + 2) = '!'
    then (
      let start_offset = !i in
      let body_start = !i + 2 in
      let j = ref (!i + 3) in
      let stop : int option ref = ref Option.None in
      while !stop = Option.None && !j + 1 < len do
        if
          String.unsafe_get source !j = '*'
          && String.unsafe_get source (!j + 1) = '/'
        then stop := Some !j
        else incr j
      done;
      match !stop with
      | Some end_ ->
          let body = String.sub source body_start (end_ - body_start) in
          acc := (start_offset, body) :: !acc;
          i := end_ + 2
      | None -> i := len)
    else incr i
  done;
  List.rev !acc

let interleave_bang_comments source rules sheet =
  let bangs = extract_bang_comments source in
  let rule_ends = List.map (fun r -> (rule_loc r).Loc.end_pos) rules in
  let rec interleave bangs rule_ends sheet =
    match (bangs, rule_ends, sheet) with
    | [], _, _ -> sheet
    | (_, body) :: rest_b, [], _ ->
        Bang_comment body :: interleave rest_b [] sheet
    | (offset, body) :: rest_b, end_ :: _, _ when offset < end_ ->
        Bang_comment body :: interleave rest_b rule_ends sheet
    | _, _ :: rest_s, [] -> interleave bangs rest_s []
    | _, _ :: rest_s, stmt :: rest_sheet ->
        stmt :: interleave bangs rest_s rest_sheet
  in
  interleave bangs rule_ends sheet

(* Top-level partial-recovery entry point: combine section 5.3 syntax warnings
   from [Parser.stylesheet] with per-rule typed-validation warnings. *)
let parse_stylesheet_partial ?(meta = Loc.default_meta_level)
    ?(enforce_spec = false) ?on_source (original : string) :
    stylesheet * Error.t list =
  let comments, on_comment =
    match on_source with
    | Option.None -> (Option.None, Option.None)
    | Option.Some _ ->
        let comments = ref [] in
        ( Option.Some comments,
          Option.Some (fun comment -> comments := comment :: !comments) )
  in
  let reader = Reader.of_string ~enforce_spec original in
  let out = Parser.stylesheet ~meta ?on_comment reader in
  let preprocessed = Reader.source reader in
  let sheet, typed_warnings =
    read_stylesheet_of_rules ~source:preprocessed ~meta out.value
  in
  (* Interleave preserved [/*! ... */] bang comments at their source position by
     walking the original rules and the bang-comment list in parallel; any bang
     comments after the last rule are appended at the end. The typed [sheet] may
     be shorter than [out.value] when validation drops a rule, but each typed
     statement still corresponds to the next unconsumed rule, so the position
     mapping survives. *)
  let sheet = interleave_bang_comments preprocessed out.value sheet in
  (match (on_source, comments) with
  | Option.Some emit, Option.Some comments ->
      emit
        (Source.v ~original ~preprocessed ~comments:(List.rev !comments)
           out.value)
  | Option.None, Option.None -> ()
  | _ -> assert false);
  (sheet, out.warnings @ typed_warnings)

(** {1 Unknown At-Rule Construction} *)

(* An at-rule cascade has no grammar for carries its parts as raw text, so the
   constructor has to answer for text that ends the at-rule before its parts do:
   CSS Syntax 3 (ED) sec. 5.5.2 ends the prelude at the first top-level [;] or
   [{], sec. 5.5.9 ends the block at the closer matching its opener, sec. 4.3.2
   runs an unclosed [/*] to EOF, and sec. 4.3.7 lets a trailing backslash escape
   the closer written after it. Enumerating those boundaries re-derives the
   tokenizer and misses whichever one is not on the list, so read the parts back
   instead: this sits after the parser because that read is the check.

   A statement is whole when the text it prints to reads back as one unknown
   at-rule of the same name printing that same text. Nothing else is required of
   it - the printer's own separator before an escaped closer moves a byte the
   caller did not write, and that byte is what keeps the at-rule closing where
   the caller meant it to. *)
let unknown_at_rule ~name ~prelude ?block () =
  let printed statement = to_string ~minify:true [ statement ] in
  let text = printed (Unknown_at_rule { name; prelude; block }) in
  let reject reason =
    let at_rule = String.concat "" [ "@"; name ] in
    Error (Error.bad_condition Loc.dummy ~at_rule ~reason)
  in
  let read_back =
    (* A name cascade does have a grammar for reads back as that at-rule and not
       as this one, so [~name:"media"] is refused here rather than printed as a
       statement whose type disagrees with the sheet it prints to. *)
    match fst (parse_stylesheet_partial text) with
    | [ (Unknown_at_rule at as statement) ] when String.equal at.name name ->
        Option.Some statement
    | _ | (exception Error.Parse_error _) -> Option.None
  in
  match read_back with
  | Option.None -> reject "the parts do not read back as one at-rule"
  | Option.Some statement ->
      if String.equal (printed statement) text then Ok statement
      else reject "a part carries text that ends the at-rule early"

(** {1 Variable extraction from stylesheets} *)

(* A [var()] is a reference wherever the declaration holding it sits, so this is
   [fold_declarations] over every site rather than a list of the at-rules that
   came to mind: an animation frame and a page margin box read a variable like a
   style rule does. Gathering the declarations first and extracting once dedupes
   across statements, which per-statement extraction cannot. *)
let vars_of_stylesheet (ss : stylesheet) : Variables.any_var list =
  fold_declarations (fun acc decls -> List.rev_append decls acc) [] ss
  |> List.rev |> Variables.vars_of_declarations

(* Pretty-printer for import_rule *)
let pp_import_rule : import_rule Pp.t =
 fun ctx rule ->
  Pp.string ctx "@import";
  pp_import_components ctx rule;
  Pp.string ctx ";"

(* Reader for import_rule *)
let read_import_rule (r : Cursor.t) : import_rule =
  Cursor.ws r;
  read_import_prelude ~keep_url_repr:false r

(* A cheap discriminating hash for a rule, folding the hash each declaration
   already caches. The stdlib structural hash reads only the first few nodes of
   a rule, so rules sharing a selector collide and any table keyed by one then
   compares whole rule subtrees on every probe. *)
let rule_hash (r : rule) =
  List.fold_left
    (fun acc d -> (acc * 31) + Declaration.hash d)
    (Selector.hash r.selector) r.declarations
