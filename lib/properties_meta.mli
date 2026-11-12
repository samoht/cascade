(** Property names and per-property metadata: inheritance, spec-invalid values
    and the value kind a property carries. *)

open Properties_intf

val pp_property : 'a property Pp.t
(** [pp_property] is the pretty-printer for property names. *)

val property_is_inherited : 'a property -> bool
(** [property_is_inherited property] is whether [property] inherits by default.
    Shorthands return [true] when every longhand they reset inherits. The
    classification is exhaustive over the property GADT, so adding a property
    requires an explicit decision here. *)

val minified_name_carries : 'a property -> 'a -> bool
(** [minified_name_carries property value] is whether the name {!pp_property}
    gives [property] under minify can carry [value]. A [page-break-*] property
    minifies to the CSS Fragmentation 3 sec. 3.4 [break-*] property it aliases,
    and that alias is defined by a value mapping table, so it is a spelling the
    declaration can use only for a value the table names. A [var()] is not one:
    substitution happens at computed-value time, and [always], the only mapping
    that is not the identity, has no [break-*] spelling. Every other property
    names itself the same whatever it carries. *)

val pp_any_property : any_property Pp.t
(** [pp_any_property] pretty-prints any CSS property. *)

val is_invalid_value : 'a property -> 'a -> bool
(** [is_invalid_value prop value] is [true] when [value] contains an [Invalid]
    arm cascade detected at parse time (CSS spec violations preserved verbatim
    for round-trip). [Optimize.drop_invalid], which every serialisation runs,
    removes declarations that satisfy this predicate. The classification is
    exhaustive over the property GADT, so adding a property requires an explicit
    validity decision here. *)

val property_value_kind : 'a property -> 'a property_value_kind option
(** [property_value_kind property] returns the optional property-specific
    evaluator for a value shape. Generic context evaluation, including direct
    CSS-wide keywords, does not depend on this specialization. *)

val pp_property_value_kind : 'a property_value_kind Pp.t
(** [pp_property_value_kind] pretty-prints the kind label of a typed property
    value. *)

val read_property_value_kind : Cursor.t -> 'a property_value_kind
(** [read_property_value_kind t] always raises. The {!val-property_value_kind}
    GADT classifies typed values for dispatch and is not addressable as a
    standalone CSS value; the function exists only so the API surface stays
    symmetric. *)
