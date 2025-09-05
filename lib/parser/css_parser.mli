(** Main CSS parser module that coordinates all section parsers. *)

val of_string : string -> (Css.t, string) result
(** [of_string css] parses a CSS string and returns a stylesheet or an error. *)

val of_string_exn : string -> Css.t
(** [of_string_exn css] parses a CSS string and returns a stylesheet or raises
    an exception. *)

(**/**)

module Reader = Reader
(** Re-export all parser submodules *)

module Values = Values
module Property = Property
module Custom_property = Custom_property
module Selector = Selector
module Declaration = Declaration
module Rule = Rule
module Rendering = Rendering
