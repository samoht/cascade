(** Common helpers for CSS tests to reduce duplication and inconsistencies *)

val neg : (Css.Reader.t -> 'a) -> string -> unit
(** [neg reader input] tests that parsing should fail by attempting to parse the
    input and expecting either a Parse_error exception or incomplete consumption
    of input. *)

val none : (Css.Reader.t -> 'a option) -> string -> unit
(** [none reader input] tests that parsing should fail by expecting None result.
    Use this for parsers that return Some/None instead of raising. *)

val test_css_wide_keywords_mixing :
  (Css.Reader.t -> 'a) -> string list -> string -> unit
(** [test_css_wide_keywords_mixing reader keywords prop_name] tests that
    CSS-wide keywords (inherit, unset, etc.) cannot be mixed with other values.
    They must be standalone values and cannot be combined with other tokens in
    the same declaration. *)

val check_value :
  string ->
  (Css.Reader.t -> 'a) ->
  'a Css.Pp.t ->
  ?minify:bool ->
  ?roundtrip:bool ->
  ?expected:string ->
  string ->
  unit
(** [check_value type_name reader pp_func ?minify ?roundtrip ?expected input]
    tests that parsing input produces the expected output when pretty-printed.
    Optionally tests roundtrip stability (parse -> print -> parse -> print). *)

val check_parse_error_fields :
  string -> Css.Reader.parse_error -> Css.Reader.parse_error -> unit
(** [check_parse_error_fields name expected actual] compares message and got
    fields between expected and actual parse errors. Fails the test with a
    descriptive message if they don't match. *)

val check_raises : string -> exn -> (unit -> unit) -> unit
(** [check_raises name expected_exn f] tests that calling the function raises
    the expected exception. Handles Parse_error comparison using
    check_parse_error_fields. *)

val check_construct : string -> ('a -> string) -> string -> 'a -> unit
(** [check_construct name to_string expected value] tests that constructed
    values serialize to expected strings. *)

val css_wide_keywords : string list
(** [css_wide_keywords] is the list of common CSS-wide keywords used across CSS
    specifications. *)
