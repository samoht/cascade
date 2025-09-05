(** CSS selector parser. *)

val one : Css.Reader.t -> Css.Selector.t
(** [one t] parses a CSS selector. *)

val one_opt : Css.Reader.t -> Css.Selector.t option
(** [one_opt t] parses a CSS selector and returns [None] on failure. *)
