(** Property identity: the order on property constructors and the type-equality
    proof between two properties. *)

open Properties_intf

val compare_property : 'a property -> 'b property -> int
(** [compare_property a b] is a total order on property identities. It is [0]
    exactly when [a] and [b] are the same property, so an ordered container
    keyed on a property agrees with equality on it. *)

val eq_property : 'a property -> 'b property -> ('a, 'b) Type.eq option
(** [eq_property a b] is [Some Equal] when [a] and [b] are the same property,
    and [None] otherwise. It answers {!compare_property}'s question and carries
    the proof that the two properties value the same type, which the comparison
    on its own cannot express. *)
