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

let scope_selector_in_context (parent : Selector.t) selector =
  if Nest.contains selector then Nest.substitute ~parent selector else selector

(* CSS Cascade 6 sec. 3.5.3: a selector of a style rule in an [@scope] block
   that holds neither [&] nor [:scope] is relative, with the scoping root and a
   descendant combinator implied at its start, and sec. 3.5.1 makes [&] there
   behave as [:where(:scope)]. So [&] spelled in front of such a selector writes
   out what is implied, at no weight. *)
let names_scope sel =
  Nest.contains sel
  || Selector.any (function Selector.Scope -> true | _ -> false) sel

let rec scoped (sel : Selector.t) =
  match sel with
  | Selector.List branches -> Selector.List (List.map scoped branches)
  | _ when names_scope sel -> sel
  | Selector.Relative (comb, right) ->
      Selector.combine Selector.Nesting comb right
  | _ -> Selector.combine Selector.Nesting Selector.Descendant sel

let branch_count = function Selector.List bs -> List.length bs | _ -> 1

(* What [&] stands for in a rule's body, written out in [selector], and what a
   flat rule matching the same elements prints as, [printed]. They differ only
   in an [@scope] block, where [printed] leaves the scoping root implied. *)
type parent = { selector : Selector.t; printed : Selector.t }

let compose (parent : parent) child =
  match Nest.keep_readable_branches (Nest.combine parent.selector child) with
  | Option.None -> Option.None
  | Option.Some selector ->
      let printed =
        if parent.printed == parent.selector || not (Nest.parent_first child)
        then selector
        else
          match
            Nest.keep_readable_branches (Nest.combine parent.printed child)
          with
          | Option.Some printed
            when branch_count printed = branch_count selector ->
              printed
          | _ -> selector
      in
      Option.Some { selector; printed }

let rec flat_rule ~in_scope ?(parent : parent option) (rule : rule) :
    statement list =
  let at =
    match parent with
    | None ->
        let selector =
          if in_scope then scoped rule.selector else rule.selector
        in
        Some { selector; printed = rule.selector }
    | Some p -> compose p rule.selector
  in
  match at with None -> [] | Some at -> flat_rule_with ~in_scope ~at rule

and flat_rule_with ~in_scope ~(at : parent) (rule : rule) : statement list =
  let direct =
    if rule.declarations = [] then []
    else
      [
        Rule
          {
            selector = at.printed;
            declarations = rule.declarations;
            nested = [];
            merge_key = rule.merge_key;
          };
      ]
  in
  let nested_flat =
    List.concat_map (in_rule_context ~in_scope at) rule.nested
  in
  direct @ nested_flat

and in_rule_context ~in_scope (parent : parent) : statement -> statement list =
  let under block = List.concat_map (in_rule_context ~in_scope parent) block in
  function
  | Rule child -> flat_rule ~in_scope ~parent child
  | Declarations decls ->
      [
        Rule
          {
            selector = parent.printed;
            declarations = decls;
            nested = [];
            merge_key = None;
          };
      ]
  | Media (cond, block) -> [ Media (cond, under block) ]
  | Moz_document (cond, block) -> [ Moz_document (cond, under block) ]
  | Container (name, cond, block) -> [ Container (name, cond, under block) ]
  | Supports (cond, block) -> [ Supports (cond, under block) ]
  | Layer (name, block) -> [ Layer (name, under block) ]
  | Origin (origin, block) -> [ Origin (origin, under block) ]
  | Starting_style block -> [ Starting_style (under block) ]
  | When (cond, block) -> [ When (cond, under block) ]
  | Else (cond, block) -> [ Else (cond, under block) ]
  | Scope (s, e, block) -> [ nested_scope parent s e block ]
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

(* Chromium 153 reads the prelude of an [@scope] nested in a style rule relative
   to that rule, as a nested selector: [.a{@scope (.b){...}}] roots at a [.b]
   inside an [.a], not at an [.a .b] element nor at a [.b] outside one. Its
   block is an [@scope] block like any other, whose declarations match the
   scoping root (CSS Cascade 6 sec. 3.5.6) and whose rules are relative to it,
   not to the parent. *)
and nested_scope (parent : parent) s e block =
  let start =
    Option.map
      (fun s ->
        match compose parent s with
        | Some at -> at.printed
        | None -> Nest.combine parent.selector s)
      s
  in
  Scope
    ( start,
      Option.map (scope_selector_in_context parent.selector) e,
      block_in ~flattens:(fun _ -> true) ~in_scope:true block )

and top_statement ~flattens ~in_scope (stmt : statement) : statement list =
  (* A flat rule with declarations is already in final form; a wrapper whose
     block does not change keeps its node. Both let an already-flat sheet stay
     physically shared. *)
  let wrap ?(in_scope = in_scope) stmts rebuild =
    let stmts' = block_in ~flattens ~in_scope stmts in
    if stmts' == stmts then [ stmt ] else [ rebuild stmts' ]
  in
  match stmt with
  | Rule { nested = []; declarations = _ :: _; _ } -> [ stmt ]
  | Rule rule when flattens rule -> flat_rule ~in_scope rule
  | Rule _ -> [ stmt ]
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
  | Scope (s, e, block) -> wrap ~in_scope:true block (fun b -> Scope (s, e, b))
  | ( Declarations _ | Property _ | Bang_comment _ | Charset _ | Import _
    | Namespace _ | Layer_decl _ | Supports_condition _ | Keyframes _
    | Webkit_keyframes _ | Moz_keyframes _ | Font_face _ | Counter_style _
    | Page _ | Page_with_margins _ | Font_palette_values _
    | Font_feature_values _ | View_transition _ | Position_try _ | Viewport _
    | Unknown_at_rule _ ) as other ->
      [ other ]

and block_in ~flattens ~in_scope (block : statement list) : statement list =
  concat_map_preserve (top_statement ~flattens ~in_scope) block

let block block = block_in ~flattens:(fun _ -> true) ~in_scope:false block

(* CSS Nesting 1 sec. 3.4 and 5: a declaration written after a nested rule, or
   directly in a conditional group rule nested in a style rule, is a nested
   declarations rule. Releases before it moved such a declaration ahead of every
   nested rule and read one in a group rule as [& {...}], which matches no
   pseudo-element and weighs a list parent as [:is()]. *)
let rec holds_nested_declarations (stmts : statement list) =
  List.exists
    (function
      | Declarations _ -> true
      | Rule r -> holds_nested_declarations r.nested
      | stmt -> holds_nested_declarations (statement_children stmt))
    stmts

let rules_with_nested_declarations block =
  block_in
    ~flattens:(fun (r : rule) -> holds_nested_declarations r.nested)
    ~in_scope:false block
