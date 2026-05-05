(** Closed-world inlining transforms (var() and \@import).

    The [vars] entry point implements a typed substitution pass layered on top
    of {!Context} (typed evaluator) and {!Selector}. Visibility of a custom
    property is computed from the rule structure: a custom property defined on a
    rule applies to itself and to any rule whose effective selector descends
    from it, and only if the consumer is inside the same chain of [\@media],
    [\@layer], [\@supports] blocks.

    The [imports] entry point inlines [\@import] rules from a closed
    [Context.loader] table. Layer / supports / media guards on the [\@import]
    prelude are evaluated against [?query] and [?layer_order] when supplied;
    rejected imports are dropped, accepted ones lose the matched guard from
    their wrapping. Cyclic imports terminate by dropping the repeat visit. *)

open Stylesheet

(** {1 Selector cover} *)

let contains_nesting sel =
  Selector.any (function Selector.Nesting -> true | _ -> false) sel

let substitute_nesting ~parent sel =
  Selector.map (function Selector.Nesting -> parent | s -> s) sel

let combine_with_parent (parent : Selector.t) (child : Selector.t) : Selector.t
    =
  if contains_nesting child then substitute_nesting ~parent child
  else Selector.Combined (parent, Selector.Descendant, child)

(* Effective selector after substituting [&] / nesting against any parent
   selectors on the recursion stack. *)
let effective_selector ~parents sel =
  match parents with
  | [] -> sel
  | _ ->
      List.fold_left
        (fun child parent -> combine_with_parent parent child)
        sel parents

let universal_selector_text s = s = ":root" || s = "html" || s = "*"

(* [.theme] is an ancestor of [.theme .descendant] (descendant-prefix); not of
   [.other]. Universal selectors always cover. The text comparison is
   conservative - exact rather than structural - which mirrors the "static
   prefix" semantics expected by the cram suite. *)
let selector_covers ~ancestor ~consumer =
  let a = Selector.to_string ~minify:true ancestor in
  let c = Selector.to_string ~minify:true consumer in
  if universal_selector_text a then true
  else if a = c then true
  else
    let prefix = a ^ " " in
    String.length c >= String.length prefix
    && String.sub c 0 (String.length prefix) = prefix

(** {1 At-rule path} *)

type at_node =
  | A_media of Media.t
  | A_layer of string option
  | A_supports of Supports.t
  | A_container of string option * Container.t
  | A_starting_style
  | A_when of conditional
  | A_else of conditional option
  | A_origin of cascade_origin
  | A_scope of string option * string option

(* Visibility through at-rule wrappers: a custom property defined outside
   (shorter path) is visible to consumers further inside (longer path). *)
let rec at_path_prefix ~outer ~inner =
  match (outer, inner) with
  | [], _ -> true
  | _, [] -> false
  | a :: outer, b :: inner -> a = b && at_path_prefix ~outer ~inner

let at_wrapper : statement -> (at_node * t * (t -> statement)) option = function
  | Layer (n, b) -> Some (A_layer n, b, fun b -> Layer (n, b))
  | Media (q, b) -> Some (A_media q, b, fun b -> Media (q, b))
  | Supports (q, b) -> Some (A_supports q, b, fun b -> Supports (q, b))
  | Container (n, q, b) ->
      Some (A_container (n, q), b, fun b -> Container (n, q, b))
  | Starting_style b -> Some (A_starting_style, b, fun b -> Starting_style b)
  | When (c, b) -> Some (A_when c, b, fun b -> When (c, b))
  | Else (c, b) -> Some (A_else c, b, fun b -> Else (c, b))
  | Origin (o, b) -> Some (A_origin o, b, fun b -> Origin (o, b))
  | Scope (a, b, body) ->
      Some (A_scope (a, b), body, fun body -> Scope (a, b, body))
  | _ -> None

(** {1 Scope record} *)

type scope = {
  at_path : at_node list;
  selector : Selector.t;
  customs : Declaration.declaration list;
}

let custom_name = Variables.custom_declaration_name

let local_customs ~kept decls =
  List.filter
    (fun d ->
      match custom_name d with None -> false | Some n -> not (List.mem n kept))
    decls

(** {1 Pass 1 - collect every rule's scope} *)

let collect_scopes ~kept stylesheet =
  let acc = ref [] in
  let record at_path selector customs =
    if customs <> [] then acc := { at_path; selector; customs } :: !acc
  in
  let rec walk_stmt ~parents ~at_path stmt =
    match at_wrapper stmt with
    | Some (node, body, _) ->
        List.iter (walk_stmt ~parents ~at_path:(at_path @ [ node ])) body
    | None -> (
        match stmt with
        | Rule rule ->
            let eff = effective_selector ~parents rule.selector in
            record at_path eff (local_customs ~kept rule.declarations);
            List.iter (walk_stmt ~parents:(eff :: parents) ~at_path) rule.nested
        | Declarations decls ->
            let sel =
              match parents with p :: _ -> p | [] -> Selector.Universal None
            in
            record at_path sel (local_customs ~kept decls)
        | _ -> ())
  in
  List.iter (walk_stmt ~parents:[] ~at_path:[]) stylesheet;
  List.rev !acc

(** {1 Pass 2 - substitute var() in every declaration} *)

let visible_customs ~scopes ~at_path ~selector =
  List.concat_map
    (fun s ->
      if
        at_path_prefix ~outer:s.at_path ~inner:at_path
        && selector_covers ~ancestor:s.selector ~consumer:selector
      then s.customs
      else [])
    scopes

let context_for visible = Context.v ~custom_properties:(List.rev visible) ()

let map_keyframe_decls f frames =
  List.map
    (fun frame ->
      {
        frame with
        keyframe_declarations = List.map f frame.keyframe_declarations;
      })
    frames

let rec substitute ~scopes ~parents ~at_path stmts =
  List.map (substitute_stmt ~scopes ~parents ~at_path) stmts

and substitute_stmt ~scopes ~parents ~at_path stmt =
  match at_wrapper stmt with
  | Some (node, body, rebuild) ->
      rebuild (substitute ~scopes ~parents ~at_path:(at_path @ [ node ]) body)
  | None -> (
      let eval_with sel decls =
        List.map
          (Context.eval
             (context_for (visible_customs ~scopes ~at_path ~selector:sel)))
          decls
      in
      let unscoped decls = List.map (Context.eval (context_for [])) decls in
      let unscoped_frame = Context.eval (context_for []) in
      match stmt with
      | Rule rule ->
          let eff = effective_selector ~parents rule.selector in
          Rule
            {
              rule with
              declarations = eval_with eff rule.declarations;
              nested =
                substitute ~scopes ~parents:(eff :: parents) ~at_path
                  rule.nested;
            }
      | Declarations decls ->
          let sel =
            match parents with p :: _ -> p | [] -> Selector.Universal None
          in
          Declarations (eval_with sel decls)
      | Page (sel, decls) -> Page (sel, unscoped decls)
      | Position_try (n, decls) -> Position_try (n, unscoped decls)
      | Keyframes (n, frames) ->
          Keyframes (n, map_keyframe_decls unscoped_frame frames)
      | Webkit_keyframes (n, frames) ->
          Webkit_keyframes (n, map_keyframe_decls unscoped_frame frames)
      | other -> other)

(** {1 Pass 3 - dead-code elimination} *)

let scan_referenced_names s =
  let acc = ref [] in
  let len = String.length s in
  let in_string = ref None in
  let rec scan i =
    if i >= len then ()
    else
      match !in_string with
      | Some q ->
          if s.[i] = q && (i = 0 || s.[i - 1] <> '\\') then in_string := None;
          scan (i + 1)
      | None ->
          if s.[i] = '"' || s.[i] = '\'' then begin
            in_string := Some s.[i];
            scan (i + 1)
          end
          else if i + 4 <= len && String.sub s i 4 = "var(" then begin
            let j = ref (i + 4) in
            while !j < len && (s.[!j] = ' ' || s.[!j] = '\t') do
              incr j
            done;
            let start = !j in
            while
              !j < len
              &&
              let c = s.[!j] in
              c = '-'
              || (c >= 'a' && c <= 'z')
              || (c >= 'A' && c <= 'Z')
              || (c >= '0' && c <= '9')
              || c = '_'
            do
              incr j
            done;
            if !j > start then begin
              let name = String.sub s start (!j - start) in
              if String.length name >= 2 && String.sub name 0 2 = "--" then
                acc := name :: !acc
            end;
            scan (i + 4)
          end
          else scan (i + 1)
  in
  scan 0;
  !acc

let referenced_names stylesheet =
  let acc = ref [] in
  let consider_decl decl =
    acc :=
      scan_referenced_names (Declaration.string_of_value ~minify:true decl)
      @ !acc
  in
  let rec walk_stmt stmt =
    match at_wrapper stmt with
    | Some (_, body, _) -> List.iter walk_stmt body
    | None -> (
        match stmt with
        | Rule rule ->
            List.iter consider_decl rule.declarations;
            List.iter walk_stmt rule.nested
        | Declarations decls -> List.iter consider_decl decls
        | Page (_, decls) | Position_try (_, decls) ->
            List.iter consider_decl decls
        | Keyframes (_, frames) | Webkit_keyframes (_, frames) ->
            List.iter
              (fun fr -> List.iter consider_decl fr.keyframe_declarations)
              frames
        | _ -> ())
  in
  List.iter walk_stmt stylesheet;
  !acc

let strip_dead ~keep ~referenced stmts =
  let alive name = List.mem name keep || List.mem name referenced in
  let filter_decls =
    List.filter (fun d ->
        match custom_name d with None -> true | Some name -> alive name)
  in
  let rec map_stmts stmts = List.filter_map map_stmt stmts
  and map_stmt stmt =
    match at_wrapper stmt with
    | Some (_, body, rebuild) -> (
        match map_stmts body with [] -> None | b -> Some (rebuild b))
    | None -> (
        match stmt with
        | Rule rule ->
            let nested = map_stmts rule.nested in
            let decls = filter_decls rule.declarations in
            if decls = [] && nested = [] then None
            else Some (Rule { rule with declarations = decls; nested })
        | Declarations decls -> (
            match filter_decls decls with
            | [] -> None
            | decls -> Some (Declarations decls))
        | Page (sel, decls) -> Some (Page (sel, filter_decls decls))
        | other -> Some other)
  in
  map_stmts stmts

let normalise_var_name name =
  if String.length name >= 2 && String.sub name 0 2 = "--" then name
  else String.concat "" [ "--"; name ]

let vars ?(keep_vars = []) stylesheet =
  let keep = List.map normalise_var_name keep_vars in
  let scopes = collect_scopes ~kept:keep stylesheet in
  let substituted = substitute ~scopes ~parents:[] ~at_path:[] stylesheet in
  let referenced = referenced_names substituted in
  strip_dead ~keep ~referenced substituted

(** {1 [@import] inlining helpers} *)

let decode_import_url s =
  let trimmed = String.trim s in
  if trimmed = "" then trimmed
  else
    try
      let r = Cursor.of_string trimmed in
      Cursor.one_of [ Cursor.url; Cursor.string ] r
    with Cursor.Parse_error _ -> trimmed

let strip_url_suffix url =
  let cut_at c s =
    match String.index_opt s c with Some i -> String.sub s 0 i | None -> s
  in
  url |> cut_at '?' |> cut_at '#'

let wrap_import_body (ir : import_rule) body =
  let body =
    match ir.media with None -> body | Some m -> [ Media (m, body) ]
  in
  let body =
    match ir.supports with None -> body | Some s -> [ Supports (s, body) ]
  in
  match ir.layer with
  | None -> body
  | Some "" -> [ Layer (None, body) ]
  | Some n -> [ Layer (Some n, body) ]

(* Layer/supports/media guard checks. When [layer_order] is empty, every layer
   name is treated as known (the caller hasn't declared an order, so we defer to
   the runtime cascade). When [query] is [None], supports/media guards pass
   through and survive as wrapping at-rules in the inlined body; when a query is
   supplied the guard is evaluated and the import is dropped on rejection. *)
let layer_guard_passes ~layer_order (rule : import_rule) =
  match (rule.layer, layer_order) with
  | None, _ | _, [] -> true
  | Some name, order -> List.mem name order

let supports_guard_passes ~query (rule : import_rule) =
  match (rule.supports, query) with
  | None, _ | Some _, None -> true
  | Some cond, Some q -> Context.matches_supports q cond

let media_guard_passes ~query (rule : import_rule) =
  match (rule.media, query) with
  | None, _ | Some _, None -> true
  | Some m, Some q -> Context.matches_media q m

(* When [query] is provided, the matching at-rule wrapper is no longer needed:
   we have already evaluated the guard and decided to load. Strip the matched
   wrapper from the rule so [wrap_import_body] doesn't re-emit it. *)
let strip_evaluated_guards ~query (rule : import_rule) =
  match query with
  | None -> rule
  | Some _ -> { rule with media = None; supports = None }

let imports ?query ?(layer_order = []) (loader : Context.loader) stylesheet =
  let imports = loader.imports in
  let resolve ~base url =
    let l = Context.loader ?base_url:base ~imports () in
    let url = strip_url_suffix url in
    match Context.resolve_url l url with
    | Error _ -> None
    | Ok resolved ->
        if List.mem_assoc resolved imports then Some resolved else None
  in
  let guards_pass rule =
    layer_guard_passes ~layer_order rule
    && supports_guard_passes ~query rule
    && media_guard_passes ~query rule
  in
  (* Track URLs currently on the recursion stack to break cyclic imports (a -> b
     -> a drops the second visit). *)
  let rec replace_stmts ~base ~stack stmts =
    List.concat_map (replace ~base ~stack) stmts
  and replace ~base ~stack = function
    | Import import_rule when not (guards_pass import_rule) -> []
    | Import import_rule -> (
        let url = decode_import_url import_rule.url in
        match resolve ~base url with
        | None -> [ Import import_rule ]
        | Some resolved -> (
            if List.mem resolved stack then []
            else
              let content = List.assoc resolved imports in
              let parsed =
                let cursor = Cursor.of_string content in
                try Some (read_stylesheet cursor)
                with Cursor.Parse_error _ -> (
                  try
                    let inner, _warnings = parse_stylesheet_partial content in
                    Some inner
                  with Invalid_argument _ -> None)
              in
              match parsed with
              | None -> [ Import import_rule ]
              | Some inner ->
                  let inner =
                    List.filter
                      (function Charset _ -> false | _ -> true)
                      inner
                  in
                  let processed =
                    replace_stmts ~base:(Some resolved)
                      ~stack:(resolved :: stack) inner
                  in
                  wrap_import_body
                    (strip_evaluated_guards ~query import_rule)
                    processed))
    | stmt -> (
        match at_wrapper stmt with
        | Some (_, body, rebuild) ->
            [ rebuild (replace_stmts ~base ~stack body) ]
        | None -> (
            match stmt with
            | Rule rule ->
                [
                  Rule
                    {
                      rule with
                      nested = replace_stmts ~base ~stack rule.nested;
                    };
                ]
            | other -> [ other ]))
  in
  let initial_stack =
    match loader.base_url with Some b -> [ b ] | None -> []
  in
  replace_stmts ~base:loader.base_url ~stack:initial_stack stylesheet
