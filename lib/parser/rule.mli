(** CSS rule parser. *)

val one : Css.Reader.t -> Css.sheet_item option
(** [one t] parses a single CSS rule (selector and declarations). *)

val rules : Css.Reader.t -> Css.sheet_item list
(** [rules t] parses multiple CSS rules. *)

val stylesheet : Css.Reader.t -> Css.t
(** [stylesheet t] parses a complete CSS stylesheet (rules and at-rules). *)
