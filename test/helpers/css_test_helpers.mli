(** Common helpers for CSS tests to reduce duplication and inconsistencies.

    Expected strings passed to these helpers are spec oracles. They must come
    from CSS specifications or from documented Cascade canonicalization that
    preserves required CSS syntax, not from current implementation output. *)

open Cascade

val ok : ('a, Error.t) result -> 'a
(** [ok r] is the value of [Ok v], and fails the test with the error printed on
    [Error]. *)

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

val neg_cursor : ?allow_partial:bool -> (Cursor.t -> 'a) -> string -> unit
(** [neg_cursor parse input] tests that [parse] rejects [input], by raising
    {!Error.Parse_error} or by returning without having taken the whole of it.
    Returning a value and leaving input behind is an acceptance, not a
    rejection: a reader that takes the prefix of [margin: inherit 10px] has
    accepted a value the case calls invalid. Pass [~allow_partial:true] for a
    reader whose contract is to stop early, where leftover input is the
    rejection. *)

val none_cursor : (Cursor.t -> 'a option) -> string -> unit
(** [none_cursor parse input] tests that [parse] yields [None] for [input], or
    raises {!Error.Parse_error}. *)

val check_error : (Cursor.t -> 'a) -> string -> string -> unit
(** [check_error parse input expected] asserts that [parse] applied to a cursor
    over [input] raises {!Error.Parse_error} whose rendered message equals
    [expected]. Use to pin down error shape exactly. *)

val check_value_cursor :
  ?unicode_ranges:bool ->
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

val decl_optimizes_to :
  ?held:string -> ?scope:Css.Optimize.scope -> into:string -> string -> unit
(** [decl_optimizes_to ?held ?scope ~into input] is the full-declaration variant
    of {!decl_optimizes}: [input], [held] and [into] include the property name.
    [scope] tells the optimizer how much surrounding CSS the input may be
    embedded in, and defaults to the fragment the optimizer itself assumes. *)

val decl_lossless : prop:string -> into:string -> string -> unit
(** [decl_lossless ~prop ~into input] checks the minify+optimize path with
    bounded approximation disabled on both optimization and printing. *)

val minify_reads_back : string -> expected:string -> string -> unit
(** [minify_reads_back name ~expected source] checks that [source] minifies to
    [expected] and that re-reading [expected] gives back the statements [source]
    parsed to.

    Comparing the text a second pass prints is weaker: CSS Syntax 3 (ED) sec. 4
    tokenises greedily, so an elided separator can turn one construct into
    another that prints back byte for byte for ever - [@scope to (.a)] minified
    to [@scopeto (.a)] is a stable unknown at-rule. *)

val sheets_agree : string -> string -> string -> unit
(** [sheets_agree name a b] checks that two spellings of one sheet parse to the
    same statements. Use it where the two differ only in whitespace a grammar
    allows but does not require. *)

val check_raises : string -> exn -> (unit -> unit) -> unit
(** [check_raises name expected_exn f] tests that [f ()] raises an exception
    equal to [expected_exn]. *)

val check_construct : string -> ('a -> string) -> string -> 'a -> unit
(** [check_construct name to_string expected value] tests that constructed
    values serialize to expected strings. *)

val css_wide_keywords : string list
(** [css_wide_keywords] is the list of common CSS-wide keywords used across CSS
    specifications. *)

val measure : (unit -> 'a) -> float
(** [measure f] is the minor words [f] allocates, read as the difference of a
    counter that only grows, so a collection inside [f] does not disturb it.
    [Gc.full_major] starts every reading from the same heap state, so collection
    and finalisation owed by an earlier case is not charged to whichever thunk
    runs next; the allocation guards compare two readings as a ratio, which
    holds only if both were taken alike. [Sys.opaque_identity] keeps the result
    live to the end of the call, so the compiler cannot satisfy a guard by
    deleting the allocation it counts. *)
