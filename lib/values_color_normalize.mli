(** Colour canonicalisation: the value-changing folds of the optimize pass. *)

open Values_intf

val normalize_color : ?lossless:bool -> color -> color
(** [normalize_color ?lossless c] canonicalises a colour to its shortest
    spelling within its interpolation class: a legacy sRGB colour folds to
    hex/named, any other colour stays in its own notation, and a [none]
    component stays. *)
