(** Main CSS parser module that coordinates all section parsers *)

type error = Parse_error of string * Reader.t

(** Parse a CSS stylesheet from a string *)
let of_string s =
  let t = Reader.of_string s in
  try Ok (Rule.stylesheet t)
  with Reader.Parse_error (msg, reader) -> Error (Parse_error (msg, reader))

(** Parse a CSS stylesheet from a string, raising on error *)
let of_string_exn s =
  match of_string s with
  | Ok css -> css
  | Error (Parse_error (msg, reader)) ->
      raise (Reader.Parse_error (msg, reader))

module Reader = Reader
(** Re-export all parser submodules *)

module Values = Values
module Property = Property
module Custom_property = Custom_property
module Selector = Selector
module Declaration = Declaration
module Rule = Rule
