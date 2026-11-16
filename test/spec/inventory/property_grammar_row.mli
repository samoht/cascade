(** The property grammar row and the value vectors several tables share. *)

type row = {
  property : string;
  positives : string list;
  negatives : string list;
}
(** A row: a property with values valid and invalid as its complete value. *)

val omitted_filters : string list
(** [omitted_filters] is every filter function written with no argument, which
    Filter Effects 1 sec. 6.1 accepts for all but [drop-shadow()]. *)

val blur_lengths : string list
(** [blur_lengths] is [blur()] calls whose calculated length is valid. *)

val invalid_blur_lengths : string list
(** [invalid_blur_lengths] is [blur()] calls whose argument is no length. *)

val rows_for : string list -> string list -> string list -> row list
(** [rows_for properties positives negatives] is one row per property, all
    sharing [positives] and [negatives]. *)
