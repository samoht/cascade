open Stylesheet_intf
open Stylesheet_equal

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

(** {1 Variable extraction from stylesheets} *)

(* A [var()] is a reference wherever the declaration holding it sits, so this is
   [fold_declarations] over every site rather than a list of the at-rules that
   came to mind: an animation frame and a page margin box read a variable like a
   style rule does. Gathering the declarations first and extracting once dedupes
   across statements, which per-statement extraction cannot. *)
let vars_of_stylesheet (ss : stylesheet) : Variables.any_var list =
  fold_declarations (fun acc decls -> List.rev_append decls acc) [] ss
  |> List.rev |> Variables.vars_of_declarations
