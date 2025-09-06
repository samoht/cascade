(** Main CSS parser module that coordinates all section parsers. *)

type error = Parse_error of string * Css.Reader.t

val of_string : string -> (Css.t, error) result
(** [of_string css] parses a CSS string and returns a stylesheet or a parse
    error. *)

val of_string_exn : string -> Css.t
(** [of_string_exn css] parses a CSS string and returns a stylesheet or raises
    an exception. *)

(**/**)

module Custom_property = Custom_property
module Rule = Rule
