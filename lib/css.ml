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
let of_string ?(filename = "<string>") css =
  let reader = Reader.of_string css in
  try Ok (Stylesheet.read_stylesheet reader)
  with Reader.Parse_error (msg, _, reader) ->
    let pos = Reader.position reader in
    let context, marker_pos = Reader.context_window reader in
    let error_msg =
      Printf.sprintf "%s at %s:%d\n%s\n%s^" msg filename pos context
        (String.make marker_pos ' ')
    in
    Error error_msg

(* Include all public APIs *)
include Values
include Declaration
include Properties
include Stylesheet
include Variables
include Optimize
include Render
