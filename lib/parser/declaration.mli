(** CSS declaration parser. *)

val one : Css.Reader.t -> (string * string * bool) option
(** [one t] parses a single declaration and returns
    [(name, value, is_important)]. *)

val declarations : Css.Reader.t -> (string * string * bool) list
(** [declarations t] parses all declarations in a block (without the braces). *)

val block : Css.Reader.t -> (string * string * bool) list
(** [block t] parses a declaration block including braces. *)
