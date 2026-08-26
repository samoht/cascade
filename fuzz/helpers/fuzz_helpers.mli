(** Shared assertion helpers used across the fuzz entry points. *)

val assert_invalid_declaration_contract : string -> string -> unit
(** [assert_invalid_declaration_contract label input] asserts that [input] is
    rejected as a declaration under [~strict:true] and recovers (with a warning)
    under [~strict:false]. Used by fuzz tests to pin the strict-rejects /
    lenient-recovers contract for invalid declarations. *)

val shapes_with_rule_runs :
  boundary_shape:(Cascade.Css.Stylesheet.statement -> string list) ->
  Cascade.Css.Stylesheet.statement list ->
  string list
(** [shapes_with_rule_runs ~boundary_shape ss] maps each statement of [ss] with
    [boundary_shape] and collapses every contiguous run of [Rule]s into a single
    ["rules"] token, so a boundary-shape invariant tracks the at-rule skeleton
    without forcing the optimizer to keep every individual rule. Each fuzz entry
    point supplies its own [boundary_shape] (they differ in baseline-[@supports]
    handling). *)

val unicodish : string -> string
(** [unicodish buf] builds CSS text out of the byte shapes an ASCII-only
    generator never reaches: a BOM, well-formed multi-byte UTF-8, malformed
    UTF-8 (a lone continuation byte, a truncated sequence, an overlong form, a
    surrogate encoding), a NUL, and [U+] ranges written next to an ident. CSS
    Syntax 3 sec. 3.3 decodes the input as UTF-8 and turns every malformed
    sequence into U+FFFD, and sec. 4.3.1 admits any code point at or above
    U+0080 into an ident, so these are live paths that an alphabet of ASCII
    never exercises. The fragments are placed in real syntactic positions -
    selector, property name, value, at-rule prelude - rather than concatenated,
    so the parser reaches them the way a stylesheet would. *)
