(** CSS generation utilities - Pure re-export module *)

(* Module re-exports *)

module Pp = Pp
module Reader = Reader
module Values = Values
module Properties = Properties
module Declaration = Declaration
module Selector = Selector
module Stylesheet = Stylesheet
module Variables = Variables
module Optimize = Optimize

(* CSS Parsing *)

type parse_error = Reader.parse_error

let pp_parse_error = Reader.pp_parse_error

(* Include all public APIs except Stylesheet *)

include Values
include Declaration
include Properties
include Variables
include Optimize
include Stylesheet

(* Declaration accessor functions *)
let declaration_is_important = Declaration.is_important
let declaration_name = Declaration.property_name

let declaration_value ?(minify = false) decl =
  Declaration.string_of_value ~minify decl

(* Override rule function to return statement directly *)
let rule ~selector ?nested declarations =
  Rule (Stylesheet.rule ~selector ?nested declarations)

(* Query functions for statements *)
let statement_selector = function
  | Rule r -> Some (Stylesheet.selector r)
  | _ -> None

let statement_declarations = function
  | Rule r -> Some (Stylesheet.declarations r)
  | _ -> None

let as_rule = function
  | Rule r ->
      Some
        (Stylesheet.selector r, Stylesheet.declarations r, Stylesheet.nested r)
  | _ -> None

let as_layer = function
  | Layer (name, content) -> Some (name, content)
  | _ -> None

let as_media = function
  | Media (condition, content) -> Some (condition, content)
  | _ -> None

let as_container = function
  | Container (name, condition, content) -> Some (name, condition, content)
  | _ -> None

let as_supports = function
  | Supports (condition, content) -> Some (condition, content)
  | _ -> None

let rec map f stmts =
  List.map
    (fun stmt ->
      match as_rule stmt with
      | Some (sel, decls, _nested) -> f sel decls
      | None -> (
          match as_media stmt with
          | Some (condition, content) -> media ~condition (map f content)
          | None -> (
              match as_supports stmt with
              | Some (condition, content) -> supports ~condition (map f content)
              | None -> (
                  match as_layer stmt with
                  | Some (name, content) -> layer ?name (map f content)
                  | None -> (
                      match as_container stmt with
                      | Some (name, condition, content) ->
                          container ?name ~condition (map f content)
                      | None -> stmt)))))
    stmts

let rec sort cmp stmts =
  (* First, recursively sort within containers *)
  let stmts_with_sorted_contents =
    List.map
      (fun stmt ->
        match as_media stmt with
        | Some (condition, content) -> media ~condition (sort cmp content)
        | None -> (
            match as_supports stmt with
            | Some (condition, content) ->
                supports ~condition (sort cmp content)
            | None -> (
                match as_layer stmt with
                | Some (name, content) -> layer ?name (sort cmp content)
                | None -> (
                    match as_container stmt with
                    | Some (name, condition, content) ->
                        container ?name ~condition (sort cmp content)
                    | None -> stmt))))
      stmts
  in

  (* Now sort the rules at this level *)
  List.sort
    (fun stmt1 stmt2 ->
      match (as_rule stmt1, as_rule stmt2) with
      | Some (sel1, decls1, _), Some (sel2, decls2, _) ->
          cmp (sel1, decls1) (sel2, decls2)
      | Some _, None -> -1 (* Rules before non-rules *)
      | None, Some _ -> 1 (* Non-rules after rules *)
      | None, None -> 0 (* Preserve order of non-rules *))
    stmts_with_sorted_contents

(* Existential type for property information *)
type property_info =
  | Property_info : {
      name : string;
      syntax : 'a Variables.syntax;
      inherits : bool;
      initial_value : 'a option;
    }
      -> property_info

let as_property = function
  | Property { name; syntax; inherits; initial_value } ->
      Some (Property_info { name; syntax; inherits; initial_value })
  | _ -> None

let as_keyframes = function
  | Keyframes (name, frames) -> Some (name, frames)
  | _ -> None

let as_font_face = function
  | Font_face descriptors -> Some descriptors
  | _ -> None

let concat = List.concat
let empty = []
let v = Stylesheet.v

(* Override to return statements instead of rules *)
let rule_statements t =
  let raw_rules = Stylesheet.rules t in
  List.map (fun r -> Rule r) raw_rules

(* Function to extract all statements, not just rules *)
let statements t = t

(* Fold over all statements recursively, descending into nested structures *)
let rec fold f acc t =
  List.fold_left
    (fun acc stmt ->
      let acc' = f acc stmt in
      (* Recursively fold over nested statements *)
      let nested =
        match as_layer stmt with
        | Some (_, nested) -> nested
        | None -> (
            match as_media stmt with
            | Some (_, nested) -> nested
            | None -> (
                match as_container stmt with
                | Some (_, _, nested) -> nested
                | None -> (
                    match as_supports stmt with
                    | Some (_, nested) -> nested
                    | None -> [])))
      in
      fold f acc' nested)
    acc t

let media_queries t =
  let raw_media = Stylesheet.media_queries t in
  List.map
    (fun (condition, rules) -> (condition, List.map (fun r -> Rule r) rules))
    raw_media

let layers t = Stylesheet.layers t

(* AST Introspection Helpers *)
let layer_block name sheet =
  List.find_map
    (fun s ->
      match as_layer s with
      | Some (Some n, inner) when n = name -> Some inner
      | _ -> None)
    sheet

let rules_from_statements stmts =
  List.filter_map
    (fun stmt ->
      match as_rule stmt with
      | Some (sel, decls, _) -> Some (sel, decls)
      | None -> None)
    stmts

let custom_prop_names decls = List.filter_map custom_declaration_name decls

let custom_props_from_rules rules =
  List.concat_map (fun (_, decls) -> custom_prop_names decls) rules

let custom_props ?layer sheet =
  (* Use fold to recursively traverse all statements *)
  let in_target_layer = ref (layer = None) in
  let props =
    fold
      (fun acc stmt ->
        (* Track if we're inside the target layer *)
        (match (layer, as_layer stmt) with
        | Some target, Some (Some name, _) when name = target ->
            in_target_layer := true
        | Some _, Some (Some _, _) ->
            (* Entering a different layer *)
            in_target_layer := false
        | _ -> ());
        (* Extract custom props from rules if we're in the right context *)
        if !in_target_layer then
          match as_rule stmt with
          | Some (_, decls, _) -> custom_prop_names decls @ acc
          | None -> acc
        else acc)
      [] sheet
  in
  List.rev props

let media ~condition statements = Media (condition, statements)
let layer ?name statements = Layer (name, statements)
let layer_decl names = Layer_decl names

let layer_of ?name stylesheet =
  (* Wrap the stylesheet statements in a layer *)
  [ Layer (name, stylesheet) ]

let container ?name ~condition statements =
  Container (name, condition, statements)

let supports ~condition statements = Supports (condition, statements)

let property ~name syntax ?initial_value ?(inherits = false) () =
  [ property ~syntax ?initial_value ~inherits name ]

(* Top-level convenience helpers for non-calc values *)

let vars_of_declarations = Variables.vars_of_declarations

let vars_of_rules statements =
  let decls =
    List.concat_map
      (fun stmt -> match stmt with Rule r -> r.declarations | _ -> [])
      statements
  in
  vars_of_declarations decls

let of_string ?(filename = "<string>") css =
  let reader = Reader.of_string css in
  try Ok (read_stylesheet reader)
  with Reader.Parse_error error -> Error (Reader.with_filename error filename)

let to_string ?(minify = false) ?(optimize = false) ?(mode = Variables)
    ?(newline = true) stylesheet =
  let stylesheet =
    if optimize then Optimize.stylesheet stylesheet
    else if minify then Optimize.apply_property_duplication stylesheet
    else stylesheet
  in
  Stylesheet.to_string ~minify ~mode ~newline stylesheet

let pp = to_string

let inline_style_of_declarations ?(optimize = false) ?minify ?mode ?newline
    declarations =
  let declarations =
    if optimize then Optimize.deduplicate_declarations declarations
    else declarations
  in
  inline_style_of_declarations ?minify ?mode ?newline declarations

(* Keep Css.optimize alias for convenience *)
let optimize = Optimize.stylesheet
