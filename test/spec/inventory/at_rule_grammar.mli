(** Shared stylesheet at-rule and descriptor grammar vectors. *)

type row = {
  feature : string;
  branch : string;
  input : string;
  expected : string;
}

type invalid_row = { feature : string; branch : string; input : string }

val positive : row list
(** [positive] contains valid at-rule/descriptor grammar branches. *)

val negative : invalid_row list
(** [negative] contains invalid at-rule/descriptor grammar branches. *)

val features : row list -> string list
(** [features rows] returns unique feature names represented by [rows]. *)
