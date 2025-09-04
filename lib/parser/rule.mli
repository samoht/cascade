(** CSS rule parser. *)

val one : Reader.t -> Css.sheet_item option
(** [one t] parses a single CSS rule (selector and declarations). *)

val rules : Reader.t -> Css.sheet_item list
(** [rules t] parses multiple CSS rules. *)

val stylesheet : Reader.t -> Css.t
(** [stylesheet t] parses a complete CSS stylesheet (rules and at-rules). *)
