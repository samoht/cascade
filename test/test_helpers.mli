(** Common helpers for CSS tests to reduce duplication and inconsistencies *)

val neg : (Css.Reader.t -> 'a) -> string -> unit
(** Generic negative test combinator for parsers that raise Parse_error.

    Tests that parsing should fail by attempting to parse the input and
    expecting either a Parse_error exception or incomplete consumption of input.

    @param reader The parser function that raises Parse_error on failure
    @param input The CSS input string that should fail to parse *)

val none : (Css.Reader.t -> 'a option) -> string -> unit
(** Test that an option-returning parser rejects invalid input.

    Tests that parsing should fail by expecting None result. Use this for
    parsers that return Some/None instead of raising.

    @param reader The parser function that returns 'a option
    @param input The CSS input string that should fail to parse *)

val test_css_wide_keywords_mixing :
  (Css.Reader.t -> 'a) -> string list -> string -> unit
(** Test that CSS-wide keywords cannot be mixed with other values.

    CSS-wide keywords (inherit, unset, etc.) should be standalone values and
    cannot be combined with other tokens in the same declaration.

    @param reader The parser function to test
    @param css_wide_keywords List of CSS-wide keywords to test
    @param prop_name Property name for error messages *)

val css_wide_keywords : string list
(** Common CSS-wide keywords used across CSS specifications *)
