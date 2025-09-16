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

let statement_nested = function
  | Rule r -> Some (Stylesheet.nested r)
  | _ -> None

let is_rule = function Rule _ -> true | _ -> false

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

let concat = List.concat
let empty = []
let of_statements = Stylesheet.v
let v statements = of_statements statements

(* Override to return statements instead of rules *)
let rules t =
  let raw_rules = Stylesheet.rules t in
  List.map (fun r -> Rule r) raw_rules

let media_queries t =
  let raw_media = Stylesheet.media_queries t in
  List.map
    (fun (condition, rules) -> (condition, List.map (fun r -> Rule r) rules))
    raw_media

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

let vars_of_rules statements =
  let decls =
    List.concat_map
      (fun stmt -> match stmt with Rule r -> r.declarations | _ -> [])
      statements
  in
  Variables.vars_of_declarations decls

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
