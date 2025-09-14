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

let v rules = List.map (fun r -> Rule r) rules
let concat = List.concat
let empty = []
let stylesheet_rules = Stylesheet.rules

let media ~condition rules =
  [ Media (condition, List.map (fun r -> Rule r) rules) ]

let layer ?name rules = [ Layer (name, List.map (fun r -> Rule r) rules) ]

let container ?name ~condition rules =
  [ Container (name, condition, List.map (fun r -> Rule r) rules) ]

let supports ~condition rules =
  [ Supports (condition, List.map (fun r -> Rule r) rules) ]

let property ~name syntax ?initial_value ?(inherits = false) () =
  [ property ~syntax ?initial_value ~inherits name ]

let vars_of_rules rules =
  Variables.vars_of_declarations
    (List.concat_map (fun r -> r.declarations) rules)

let of_string ?(filename = "<string>") css =
  let reader = Reader.of_string css in
  try Ok (read_stylesheet reader)
  with Reader.Parse_error error -> Error (Reader.with_filename error filename)

let to_string ?(minify = false) ?(optimize = false) ?(mode = Variables)
    ?(newline = true) stylesheet =
  let stylesheet =
    if optimize then Optimize.stylesheet stylesheet else stylesheet
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
