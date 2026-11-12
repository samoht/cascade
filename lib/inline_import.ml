open Stylesheet
open Syntax
open Inline_scope

(** {1 [@import] inlining helpers} *)

let decode_import_url s =
  let trimmed = String.trim s in
  if trimmed = "" then trimmed
  else
    try
      let r = Cursor.of_string trimmed in
      Cursor.one_of [ Cursor.url; Cursor.string ] r
    with Cursor.Parse_error _ -> trimmed

let wrap_import_body (ir : import_rule) body =
  let body =
    match ir.media with
    | None -> body
    | Some m -> [ Stylesheet.Media (m, body) ]
  in
  let body =
    match ir.supports with
    | None -> body
    | Some s -> [ Stylesheet.Supports (s, body) ]
  in
  match ir.layer with
  | None -> body
  | Some [] -> [ Stylesheet.Layer (None, body) ]
  | Some n -> [ Stylesheet.Layer (Some n, body) ]

(* Layer/supports/media guard checks. When [layer_order] is empty, every layer
   name is treated as known (the caller hasn't declared an order, so we defer to
   the runtime cascade). When [query] is [None], supports/media guards pass
   through and survive as wrapping at-rules in the inlined body; when a query is
   supplied the guard is evaluated and the import is dropped on rejection. *)
let layer_guard_passes ~(layer_order : string list) (rule : import_rule) =
  match ((rule.layer : Stylesheet.layer_name option), layer_order) with
  | None, _ | _, [] -> true
  | Some name, order -> List.mem (Stylesheet.string_of_layer_name name) order

let supports_guard_passes ~(query : Context.query option) (rule : import_rule) =
  match ((rule.supports : Supports.t option), query) with
  | None, _ | Some _, None -> true
  | Some cond, Some q -> Context.matches_supports q cond

let media_guard_passes ~(query : Context.query option) (rule : import_rule) =
  match ((rule.media : Media.t option), query) with
  | None, _ | Some _, None -> true
  | Some m, Some q -> Context.matches_media q m

(* When [query] is provided, the matching at-rule wrapper is no longer needed:
   we have already evaluated the guard and decided to load. Strip the matched
   wrapper from the rule so [wrap_import_body] doesn't re-emit it. *)
let strip_evaluated_guards ~(query : Context.query option) (rule : import_rule)
    : import_rule =
  match query with
  | None -> rule
  | Some _ -> { rule with media = Option.None; supports = Option.None }

let parse_import_content content =
  let cursor = Cursor.of_string content in
  match read cursor with
  | stylesheet -> Some stylesheet
  | exception Cursor.Parse_error _ -> (
      match
        let inner, _warnings = parse_stylesheet_partial content in
        inner
      with
      | inner -> Some inner
      | exception Invalid_argument _ -> None)

let strip_import_charset =
  List.filter (function Charset _ -> false | _ -> true)

let imports ?query ?(layer_order = []) (loader : Context.loader) stylesheet =
  let imports = loader.imports in
  let resolve ~base url : string option =
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
    | Import import_rule -> replace_import ~base ~stack import_rule
    | stmt -> replace_non_import ~base ~stack stmt
  and replace_import ~base ~stack import_rule =
    let url = decode_import_url import_rule.url in
    match resolve ~base url with
    | None -> [ Import import_rule ]
    | Some resolved -> replace_resolved_import ~stack import_rule resolved
  and replace_resolved_import ~stack import_rule resolved =
    if List.mem resolved stack then []
    else
      let content = List.assoc resolved imports in
      match parse_import_content content with
      | None -> [ Import import_rule ]
      | Some inner -> inline_parsed_import ~stack import_rule resolved inner
  and inline_parsed_import ~stack import_rule resolved inner =
    let inner = strip_import_charset inner in
    let processed =
      replace_stmts ~base:(Some resolved) ~stack:(resolved :: stack) inner
    in
    wrap_import_body (strip_evaluated_guards ~query import_rule) processed
  and replace_non_import ~base ~stack stmt =
    match at_wrapper stmt with
    | Some (_, body, rebuild) -> [ rebuild (replace_stmts ~base ~stack body) ]
    | None -> replace_plain_stmt ~base ~stack stmt
  and replace_plain_stmt ~base ~stack = function
    | Rule rule ->
        [ Rule { rule with nested = replace_stmts ~base ~stack rule.nested } ]
    | other -> [ other ]
  in
  let initial_stack =
    match loader.base_url with Some b -> [ b ] | None -> []
  in
  replace_stmts ~base:loader.base_url ~stack:initial_stack stylesheet
