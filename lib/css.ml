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

let of_string ?(filename = "<string>") css =
  let reader = Reader.of_string css in
  try Ok (Stylesheet.read_stylesheet reader)
  with Reader.Parse_error error -> Error (Reader.with_filename error filename)

(* Include all public APIs *)
include Values
include Declaration
include Properties
include Stylesheet
include Variables
include Optimize

(* Wrapper functions that handle optimization *)
let to_string ?(minify = false) ?(optimize = false) ?(mode = Variables)
    ?(newline = true) stylesheet =
  let stylesheet =
    if optimize then Optimize.optimize stylesheet else stylesheet
  in
  Stylesheet.to_string ~minify ~mode ~newline stylesheet

let pp ?minify ?optimize ?mode ?newline stylesheet =
  to_string ?minify ?optimize ?mode ?newline stylesheet

let inline_style_of_declarations ?(optimize = false) ?minify ?mode ?newline
    declarations =
  let declarations =
    if optimize then Optimize.deduplicate_declarations declarations
    else declarations
  in
  Stylesheet.inline_style_of_declarations ?minify ?mode ?newline declarations
