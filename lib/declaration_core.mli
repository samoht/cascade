(** Declaration values: the smart constructor and its hash, importance, the
    property identity, and the queries the optimizer asks of a value. *)

open Declaration_intf

val pp_property : 'a Properties.property Pp.t
(** [pp_property] is the pretty-printer for CSS property names. *)

val meta_of_declaration : declaration -> Values.meta option
(** [meta_of_declaration d] is the metadata of [d], if any. *)

val v : ?important:bool -> 'a Properties.property -> 'a -> declaration
(** [v ?important property value] creates a typed declaration. *)

val theme_guarded : var_name:string -> declaration -> declaration
(** [theme_guarded ~var_name decl] wraps [decl] in a theme guard. *)

val hash : declaration -> int
(** [hash decl] returns the structural fingerprint cached at construction. Two
    declarations that are equal under [=] always return the same value; the
    converse may fail on hash collisions, so use it only as a cheap pre-filter
    before falling back to structural equality. *)

val canonical_prop_name : 'a Properties.property -> string
(** [canonical_prop_name prop] is the unminified name of [prop], the spelling it
    parses back from and a diagnostic names. *)

val pp : declaration Pp.t
(** [pp] is the pretty-printer for declarations. *)

val size : declaration -> int
(** [size decl] is the length of [decl]'s minified rendering under {!pp}, cached
    at construction. *)

val unknown_property : ?important:bool -> string -> string -> declaration
(** [unknown_property ?important name value] is [name: value] kept as the
    component values of [value], for a property cascade does not type. *)

val important : declaration -> declaration
(** [important d] is [d] marked as [!important]. *)

val is_substitution_call : string -> Component.t list -> bool
(** [is_substitution_call name arguments] is whether a function [name] with
    [arguments] is an arbitrary substitution call that substitutes a token
    stream ({!Component.is_substitution_function}): one with no argument
    substitutes nothing, bar a [<dashed-function>]. *)

val substitution_takes_a_slot : Component.t list -> bool
(** [substitution_takes_a_slot cvs] is whether the value [cvs] holds a
    substitution that stands in a slot of the property's grammar, beside other
    components or inside a function other than a math function. Such a call may
    supply any number of components (CSS Variables 1 sec. 3). A substitution
    that is the whole value, with a fallback that takes no slot either, or that
    is an operand of a math function takes none. *)

val slots_kept : Component.t list -> Component.t list -> bool
(** [slots_kept source print] is whether [print] writes every substitution call
    of [source] as written, in its place among the same separators and inside
    the same functions, with no component added, dropped or moved. A component
    holding no substitution may be respelled where it stands. *)

val value_components : declaration -> Component.t list
(** [value_components d] is the pretty print of [d]'s value, as components. *)

val normalize :
  ?lossless:bool -> ?ctx:Values.calc_ctx -> declaration -> declaration
(** [normalize ?lossless d] applies AST-level semantic value canonicalisation so
    the optimizer holds a canonical declaration and the pretty-printer stays a
    pure serialiser. [lossless] disables bounded colour and numeric
    approximation. *)

val is_important : declaration -> bool
(** [is_important decl] returns true if the declaration has !important. *)

val is_decl_unknown_property_name : string -> bool
(** [is_decl_unknown_property_name name] is whether [name] is a well-formed
    property name that cascade does not type. *)

val is_invalid : declaration -> bool
(** [is_invalid decl] is [true] when [decl]'s typed value is a CSS
    spec-violation cascade detected at parse time. [Optimize.drop_invalid],
    which every serialisation runs, uses this predicate to remove the
    declaration. *)

val value_uses_color : (Values.color -> bool) -> declaration -> bool
(** [value_uses_color p decl] is [true] when a colour in [decl]'s typed value
    satisfies [p]. [var()] returns [false]. *)

val value_uses_color_4 : declaration -> bool
(** [value_uses_color_4 decl] is [true] when [decl]'s typed value contains any
    CSS Color 4 / 5 construct. [var()] returns [false]. *)

val value_uses_runtime_subst : declaration -> bool
(** [value_uses_runtime_subst decl] is [true] when [decl]'s typed length value
    contains a [var()] / [env()] / [attr()] / anchor query, possibly through
    [calc()]. Covers length / length-list / length-percentage properties used in
    cross-tier fallback patterns. *)

val property_name : declaration -> string
(** [property_name decl] returns the property name as a string. *)

(** A property identity comparable with stdlib structural equality. *)
type prop_key = Key : 'a Properties.property -> prop_key [@@unboxed]

val equal_declaration : declaration -> declaration -> bool
(** [equal_declaration a b] tests declarations for structural equality. A NaN
    equals itself here, as it does under {!hash}: CSS has one NaN, a keyword of
    the [<number>] grammar (Values 4 sec. 10.7.2) serialised as [calc(NaN)]
    (sec. 10.13). *)

val same_minified : declaration -> declaration -> bool
(** [same_minified a b] is [true] when [a] and [b] render to the same minified
    text. Two declarations do so exactly when their canonical ASTs agree on
    property, value and importance, so this is {!equal_declaration} behind the
    {!hash} pre-filter rather than a comparison of rendered strings. *)

val equal_prop_key : prop_key -> prop_key -> bool
(** [equal_prop_key a b] tests property identities for equality. *)

val hash_prop_key : prop_key -> int
(** [hash_prop_key key] returns a hash consistent with {!equal_prop_key}. *)

val compare_prop_key : prop_key -> prop_key -> int
(** [compare_prop_key a b] is a total order on property identities, [0] exactly
    where {!equal_prop_key} is [true]. It orders the property tag rather than
    the runtime representation, so an ordered container keyed on a property
    stays off [Stdlib.compare]. *)

val property_key : declaration -> prop_key
(** [property_key decl] is the identity of [decl]'s property. Two declarations
    have the same property name iff their keys are structurally equal. *)

val same_property : declaration -> declaration -> bool
(** [same_property d1 d2] is [property_key d1 = property_key d2]: the two
    declarations target the same property name. *)

val pp_value : ('a Properties.kind * 'a) Pp.t
(** [pp_value] is the pretty-printer for typed values. *)

val value_of : declaration -> 'a Properties.property -> 'a option
(** [value_of decl property] is [decl]'s value when {!property_key} names
    [property], and [None] otherwise. The witness ties the result to the type
    that property carries, so a caller reads the typed value without naming the
    existential {!Declaration_intf.constructor-Declaration} binds, which no
    constructor name resolves against. A theme guard is read through, as
    {!property_key} reads through it. *)

val string_of_value : ?minify:bool -> ?inline:bool -> declaration -> string
(** [string_of_value ?minify decl] returns the value as a string. *)

val first_release :
  floor:Support.version -> Support.engine -> declaration -> Support.release
(** [first_release ~floor engine decl] is the first release of [engine] at or
    past [floor] that parses [decl], as the web-features dataset records the
    productions [decl] uses: its property, a colour construct, or a keyword or
    function of its value. A production the dataset has no key for counts as
    parsed, except those cascade knows a recorded release drops, which make the
    answer {!Support.constructor-Unrecorded}: a [clamp()] bound of [none] and a
    CSS-wide keyword the dataset does not name. A custom property, or a
    declaration holding [var()], waits on no value. *)

val fallback_for :
  floors:Support.targets -> later:declaration -> declaration -> bool
(** [fallback_for ~floors ~later earlier] is [true] when some release of an
    engine [floors] names, from its floor to the newest the web-features dataset
    records, parses [earlier] and drops [later] at parse time, so paints
    [earlier] where [later] covers it. Which release parses a declaration is
    {!first_release}. A vendor-prefixed or legacy keyword the dataset does not
    model, such as [-webkit-box], counts as dropped by some release: an earlier
    value of the property is a fallback for a later one using such keywords the
    earlier does not. [false] means every such release that parses [earlier]
    parses [later], so [earlier] is dead wherever [later] covers it. *)

val legacy_pair : declaration -> declaration -> bool
(** [legacy_pair later earlier] is [true] when [later] and [earlier] set one
    property and differ in which vendor-prefixed or legacy keywords they use,
    one of them using some, such as [display:-webkit-box] and [display:flex].
    Deduplication inside one rule keeps both, in either order, for a browser
    older than the floors that reads only the prefixed one. *)

val map_custom_value : (string -> string) -> declaration -> declaration
(** [map_custom_value f d] is [d] with its custom-property value replaced by
    [f]'s rewrite of the minified serialisation. The importance, layer, metadata
    and theme guard of [d] are kept; any other declaration passes through
    unchanged. *)

val value_size : ?minify:bool -> ?inline:bool -> declaration -> int
(** [value_size ?minify decl] is the byte length of
    [string_of_value ?minify decl], computed without allocating the string. *)

val value_has_css_wide_mix : declaration -> bool
(** [value_has_css_wide_mix decl] is [true] when [decl]'s value holds a CSS-wide
    keyword beside other components. CSS Cascade 5 sec. 7.3 makes such a keyword
    the whole value of a declaration or nothing, so the reader rejects one and a
    browser drops the declaration. This is that rule read off an emission, for
    the composer deciding whether a contraction it built can be written out. *)

val rolls_back_rule : declaration -> bool
(** [rolls_back_rule decl] is [true] when [decl]'s value holds [revert-rule],
    whole or as a substitution fallback. CSS Cascade 5 sec. 7.3.6 rolls such a
    value back to what the property would be without the style rule holding
    [decl], so the rule's identity is part of its meaning: no rewrite merges
    that rule with another, moves [decl] into another rule, or lets [decl] cover
    a declaration of another rule. *)
