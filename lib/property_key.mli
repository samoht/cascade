(** A property's typed identity and importance.

    Keyed on {!Declaration.prop_key} rather than on the property's name, which
    exists only once it is printed: a key is built for every declaration of
    every rule the candidate search touches. *)

type t = { prop : Declaration.prop_key; important : bool; hash : int }

val of_declaration : Declaration.declaration -> t
(** [of_declaration decl] is the key [decl] writes. *)

val equal : t -> t -> bool
(** [equal a b] is [true] when [a] and [b] name one property at one importance.
*)

val hash : t -> int
(** [hash key] is a hash consistent with {!equal}. *)

val pp : t Pp.t
(** [pp] prints the property's name, followed by [!important] when the key is.
*)

val has : t -> Declaration.declaration -> bool
(** [has key decl] is [true] when [decl] writes [key]. *)

val has_any : t list -> Declaration.declaration -> bool
(** [has_any keys decl] is [true] when [decl] writes one of [keys]. *)

val single : t -> Declaration.declaration list -> Declaration.declaration option
(** [single key decls] is the one declaration of [decls] that writes [key], or
    [None] when none or several do. *)

val of_declarations : Declaration.declaration list -> t list
(** [of_declarations decls] is the keys [decls] write, each once. *)

val written : t -> Declaration.declaration list -> bool
(** [written key decls] is [true] when one of [decls] writes [key]. *)

val remove :
  t list -> Declaration.declaration list -> Declaration.declaration list
(** [remove keys decls] is [decls] without the declarations writing one of
    [keys]. *)
