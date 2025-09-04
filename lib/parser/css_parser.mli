(** Minimal CSS parser that produces Css.t.

   Scope (initial): - Rulesets: selector { property: value; ... } -
   Declarations: parsed as custom properties (name, raw value string) - Comments
   and whitespace handling - Strings and balanced parentheses inside values

   Notes: - !important is preserved as part of the raw value (custom properties
   in Css are rendered exactly as given). - At-rules (e.g., @media) are
   currently skipped; can be added later by plumbing into
   Css.media/Supports/Container. *)

module Reader = Reader
(** The Reader module for position-aware character consumption *)

module Declaration = Declaration
(** The Declaration module for parsing CSS declarations *)

module Selector = Selector
(** The Selector module for parsing CSS selectors *)

module Rule = Rule
(** The Rule module for parsing CSS rules and stylesheets *)

val of_string : string -> (Css.t, string) result
(** [of_string css] parses a CSS string and returns a stylesheet or an error. *)

val of_string_exn : string -> Css.t
(** [of_string_exn css] parses a CSS string and returns a stylesheet or raises
    an exception. *)
