(** Main CSS parser module that coordinates all section parsers *)

(** Parse a CSS stylesheet from a string *)
let of_string s =
  let t = Reader.of_string s in
  try Ok (Rule.stylesheet t) with Reader.Parse_error msg -> Error msg

(** Parse a CSS stylesheet from a string, raising on error *)
let of_string_exn s =
  match of_string s with Ok css -> css | Error msg -> failwith msg

module Reader = Reader
(** Re-export all parser submodules *)

module Values = Values
module Property = Property
module Custom_property = Custom_property
module Selector = Selector
module Declaration = Declaration
module Rule = Rule
