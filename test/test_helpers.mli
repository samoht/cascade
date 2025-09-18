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

val check_value :
  string ->
  (Css.Reader.t -> 'a) ->
  'a Css.Pp.t ->
  ?minify:bool ->
  ?roundtrip:bool ->
  ?expected:string ->
  string ->
  unit
(** Generic check function for CSS value types - handles parse/print testing.

    Tests that parsing input produces the expected output when pretty-printed.
    Optionally tests roundtrip stability (parse -> print -> parse -> print).

    @param type_name Name of the type being tested (for error messages)
    @param reader Parser function that reads the value
    @param pp_func Pretty-printer function for the value
    @param minify Whether to minify output (default: true)
    @param roundtrip Whether to test roundtrip stability (default: false)
    @param expected Expected output string (default: input string)
    @param input Input CSS string to parse *)

val check_parse_error_fields :
  string -> Css.Reader.parse_error -> Css.Reader.parse_error -> unit
(** Check that two Parse_error records have matching fields.

    Compares message and got fields between expected and actual parse errors.
    Fails the test with a descriptive message if they don't match.

    @param name Test name for error messages
    @param expected Expected parse error
    @param actual Actual parse error *)

val check_raises : string -> exn -> (unit -> unit) -> unit
(** Check that a function raises a specific exception.

    Tests that calling the function raises the expected exception. Handles
    Parse_error comparison using check_parse_error_fields.

    @param name Test name for error messages
    @param expected_exn Expected exception
    @param f Function that should raise the exception *)

val check_construct : string -> ('a -> string) -> string -> 'a -> unit
(** Check that a constructor produces the expected string representation.

    Generic helper for testing that constructed values serialize to expected
    strings.

    @param name Test name for error messages
    @param to_string Function to convert value to string
    @param expected Expected string representation
    @param value Constructed value to test *)

val css_wide_keywords : string list
(** Common CSS-wide keywords used across CSS specifications *)
