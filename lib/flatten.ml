(** CSS nesting flattening. *)

open Stylesheet

let concat_map_preserve f xs =
  let changed = ref false in
  let ys =
    List.concat_map
      (fun x ->
        match f x with
        | [ y ] when y == x -> [ x ]
        | r ->
            changed := true;
            r)
      xs
  in
  if !changed then ys else xs

let contains_nesting = Nest.contains
let substitute_nesting = Nest.substitute
let combine_with_parent = Nest.combine
let keep_readable_branches = Nest.keep_readable_branches

let scope_selector_in_context (parent : Selector.t) selector =
  if contains_nesting selector then substitute_nesting ~parent selector
  else selector

let rec flat_rule ?(parent : Selector.t option) (rule : rule) : statement list =
  let selector =
    match parent with
    | None -> Option.Some rule.selector
    | Some p -> keep_readable_branches (combine_with_parent p rule.selector)
  in
  match selector with
  | Option.None -> []
  | Option.Some selector -> flat_rule_with ~selector rule

and flat_rule_with ~selector (rule : rule) : statement list =
  let direct =
    if rule.declarations = [] then []
    else
      [
        Rule
          {
            selector;
            declarations = rule.declarations;
            nested = [];
            merge_key = rule.merge_key;
          };
      ]
  in
  let nested_flat = List.concat_map (in_rule_context selector) rule.nested in
  direct @ nested_flat

and in_rule_context (parent : Selector.t) : statement -> statement list =
  function
  | Rule child -> flat_rule ~parent child
  | Declarations decls ->
      [
        Rule
          {
            selector = parent;
            declarations = decls;
            nested = [];
            merge_key = None;
          };
      ]
  | Media (cond, block) ->
      [ Media (cond, List.concat_map (in_rule_context parent) block) ]
  | Moz_document (cond, block) ->
      [ Moz_document (cond, List.concat_map (in_rule_context parent) block) ]
  | Container (name, cond, block) ->
      [ Container (name, cond, List.concat_map (in_rule_context parent) block) ]
  | Supports (cond, block) ->
      [ Supports (cond, List.concat_map (in_rule_context parent) block) ]
  | Layer (name, block) ->
      [ Layer (name, List.concat_map (in_rule_context parent) block) ]
  | Origin (origin, block) ->
      [ Origin (origin, List.concat_map (in_rule_context parent) block) ]
  | Starting_style block ->
      [ Starting_style (List.concat_map (in_rule_context parent) block) ]
  | When (cond, block) ->
      [ When (cond, List.concat_map (in_rule_context parent) block) ]
  | Else (cond, block) ->
      [ Else (cond, List.concat_map (in_rule_context parent) block) ]
  | Scope (s, e, block) ->
      [
        Scope
          ( Option.map (scope_selector_in_context parent) s,
            Option.map (scope_selector_in_context parent) e,
            List.concat_map (in_rule_context parent) block );
      ]
  (* Listed rather than closed with a wildcard: a statement that grows a block
     later has to be classified above before it compiles, or it escapes the
     parent selector it was written under. Everything below carries no selector
     context of its own, so it lifts out verbatim. *)
  | ( Property _ | Bang_comment _ | Charset _ | Import _ | Namespace _
    | Layer_decl _ | Supports_condition _ | Keyframes _ | Webkit_keyframes _
    | Moz_keyframes _ | Font_face _ | Counter_style _ | Page _
    | Page_with_margins _ | Font_palette_values _ | Font_feature_values _
    | View_transition _ | Position_try _ | Viewport _ | Unknown_at_rule _ ) as
    other ->
      [ other ]

let rec top_statement (stmt : statement) : statement list =
  (* A flat rule with declarations is already in final form; a wrapper whose
     block does not change keeps its node. Both let an already-flat sheet stay
     physically shared. *)
  let wrap stmts rebuild =
    let stmts' = block stmts in
    if stmts' == stmts then [ stmt ] else [ rebuild stmts' ]
  in
  match stmt with
  | Rule { nested = []; declarations = _ :: _; _ } -> [ stmt ]
  | Rule rule -> flat_rule rule
  | Media (cond, block) -> wrap block (fun b -> Media (cond, b))
  | Moz_document (cond, block) -> wrap block (fun b -> Moz_document (cond, b))
  | Container (name, cond, block) ->
      wrap block (fun b -> Container (name, cond, b))
  | Supports (cond, block) -> wrap block (fun b -> Supports (cond, b))
  | Layer (name, block) -> wrap block (fun b -> Layer (name, b))
  | Origin (origin, block) -> wrap block (fun b -> Origin (origin, b))
  | Starting_style block -> wrap block (fun b -> Starting_style b)
  | When (cond, block) -> wrap block (fun b -> When (cond, b))
  | Else (cond, block) -> wrap block (fun b -> Else (cond, b))
  | Scope (s, e, block) -> wrap block (fun b -> Scope (s, e, b))
  | ( Declarations _ | Property _ | Bang_comment _ | Charset _ | Import _
    | Namespace _ | Layer_decl _ | Supports_condition _ | Keyframes _
    | Webkit_keyframes _ | Moz_keyframes _ | Font_face _ | Counter_style _
    | Page _ | Page_with_margins _ | Font_palette_values _
    | Font_feature_values _ | View_transition _ | Position_try _ | Viewport _
    | Unknown_at_rule _ ) as other ->
      [ other ]

and block (block : statement list) : statement list =
  concat_map_preserve top_statement block
