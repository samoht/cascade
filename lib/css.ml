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
module Render = Render

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
include Render
