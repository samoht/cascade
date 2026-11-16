(** Colour canonicalisation: the value-changing folds of the optimize pass. *)

open Values_intf

val normalize_color :
  ?lossless:bool -> ?exact_srgb:bool -> ?resolve_missing:bool -> color -> color
(** [normalize_color ?lossless c] canonicalises a color to its shortest
    spelling: a static colour in any space folds through sRGB to hex/named, hex
    shortens, and named<->hex picks the shorter. *)
