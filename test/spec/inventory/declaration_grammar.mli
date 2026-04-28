(** Shared declaration-level grammar vectors for CSS Cascade behavior. *)

type serialization_row = { branch : string; input : string; expected : string }
type invalid_row = { branch : string; input : string }

val css_wide_keywords : string list
(** [css_wide_keywords] contains the CSS-wide keyword set. *)

val css_wide_positive : serialization_row list
(** [css_wide_positive] contains valid CSS-wide declaration vectors. *)

val css_wide_negative : invalid_row list
(** [css_wide_negative] contains invalid CSS-wide declaration vectors. *)

val alias_positive : serialization_row list
(** [alias_positive] contains valid legacy property alias vectors. *)

val alias_negative : invalid_row list
(** [alias_negative] contains invalid legacy property alias vectors. *)
