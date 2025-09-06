(** Main CSS parser module that coordinates all section parsers *)

type error = Parse_error of string * Css.Reader.t

(** Parse a CSS stylesheet from a string *)
let of_string s =
  let t = Css.Reader.of_string s in
  try Ok (Rule.stylesheet t)
  with Css.Reader.Parse_error (msg, reader) ->
    Error (Parse_error (msg, reader))

(** Parse a CSS stylesheet from a string, raising on error *)
let of_string_exn s =
  match of_string s with
  | Ok css -> css
  | Error (Parse_error (msg, reader)) ->
      raise (Css.Reader.Parse_error (msg, reader))

module Custom_property = Custom_property
module Rule = Rule
