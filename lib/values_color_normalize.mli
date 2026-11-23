(** Colour canonicalisation: the value-changing folds of the optimize pass. *)

open Values_intf

val normalize_color : ?lossless:bool -> ?enforce_spec:bool -> color -> color
(** [normalize_color ?lossless ?enforce_spec c] canonicalises a colour to its
    shortest spelling within its interpolation class: a legacy sRGB colour folds
    to hex/named, any other colour stays in its own notation, and a [none]
    component stays. [enforce_spec] also folds a [color-mix()] of one item,
    which a target may drop. *)
