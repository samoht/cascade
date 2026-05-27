(** Common helpers for CSS tests to reduce duplication and inconsistencies.

    Expected strings passed to these helpers are spec oracles. They must come
    from CSS specifications or from documented Cascade canonicalization that
    preserves required CSS syntax, not from current implementation output. *)

open Cascade

val neg : (Reader.t -> 'a) -> string -> unit
(** [neg reader input] tests that parsing should fail by attempting to parse the
    input and expecting either a Parse_error exception or incomplete consumption
    of input. *)

val none : (Reader.t -> 'a option) -> string -> unit
(** [none reader input] tests that parsing should fail by expecting None result.
    Use this for parsers that return Some/None instead of raising. *)

val test_css_wide_keywords_mixing :
  (Reader.t -> 'a) -> string list -> string -> unit
(** [test_css_wide_keywords_mixing reader keywords prop_name] tests that
    CSS-wide keywords (inherit, unset, etc.) cannot be mixed with other values.
    They must be standalone values and cannot be combined with other tokens in
    the same declaration. *)

val check_value :
  string ->
  (Reader.t -> 'a) ->
  'a Css.Pp.t ->
  ?minify:bool ->
  ?roundtrip:bool ->
  ?expected:string ->
  string ->
  unit
(** [check_value type_name reader pp_func ?minify ?roundtrip ?expected input]
    tests that parsing input produces the expected output when pretty-printed.
    [expected] must be spec-derived. Optionally tests roundtrip stability (parse
    -> print -> parse -> print). *)

val neg_cursor : (Cursor.t -> 'a) -> string -> unit
(** Cursor-based variant of {!neg}. *)

val none_cursor : (Cursor.t -> 'a option) -> string -> unit
(** Cursor-based variant of {!none}. *)

val check_error : (Cursor.t -> 'a) -> string -> string -> unit
(** [check_error parse input expected] asserts that [parse] applied to a cursor
    over [input] raises {!Error.Parse_error} whose rendered message equals
    [expected]. Use to pin down error shape exactly. *)

val check_value_cursor :
  string ->
  (Cursor.t -> 'a) ->
  'a Css.Pp.t ->
  ?minify:bool ->
  ?roundtrip:bool ->
  ?expected:string ->
  string ->
  unit
(** [check_value_cursor type_name parse pp_func ?minify ?expected input] is the
    {!Cursor.t}-based variant of {!check_value}: lexes [input] into a cursor,
    applies [parse], pretty-prints with [pp_func], and asserts the result equals
    spec-derived [input] or [expected]. *)

val decl_optimizes :
  prop:string -> ?held:string -> into:string -> string -> unit
(** [decl_optimizes ~prop ?held ~into input] checks that normalization is done
    in optimize, not pp. With [held], the just-minify (pp only) form of
    [prop:input] must equal [prop:held] - pp does not normalize. The
    minify+optimize form must equal [prop:into] - the optimizer does the
    cross-node fold (unit conversion, zero-strip, named<->hex, calc folding).
    [into] is the spec-canonical shortest spelling, not a snapshot of current
    output. *)

val decl_optimizes_to : ?held:string -> into:string -> string -> unit
(** [decl_optimizes_to ?held ~into input] is the full-declaration variant of
    {!decl_optimizes}: [input], [held] and [into] include the property name. *)

val decl_lossless : prop:string -> into:string -> string -> unit
(** [decl_lossless ~prop ~into input] checks the minify+optimize path with
    [~lossless:true] on both optimization and printing. *)

val check_parse_error_fields :
  string -> Reader.parse_error -> Reader.parse_error -> unit
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
