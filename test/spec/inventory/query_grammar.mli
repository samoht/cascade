(** Shared CSS Media Queries and Container Queries grammar vectors. *)

type row = { branch : string; input : string; expected : string }
type invalid_row = { branch : string; input : string }

type mutation = { input : string; recovery : string }
(** A derived invalid query and the serialization it recovers to. *)

val media_positive : row list
(** [media_positive] contains valid CSS Media Queries branches. *)

val media_negative : invalid_row list
(** [media_negative] contains invalid CSS Media Queries branches that recover to
    [not all] under Media Queries error handling. *)

val media_general_enclosed : row list
(** [media_general_enclosed] contains unknown enclosed conditions retained by
    the forward-compatible grammar in MQ4 section 3. *)

val media_recovery : row list
(** [media_recovery] contains invalid-in-part media query lists with their
    spec-mandated recovered serialization. *)

val container_positive : row list
(** [container_positive] contains valid CSS Container Queries branches. *)

val container_negative : invalid_row list
(** [container_negative] contains invalid CSS Container Queries branches. *)

val mutate_invalid : row -> int -> mutation
(** [mutate_invalid row salt] derives invalid query syntax from [row], with the
    serialization Media Queries 4 sec. 3.2 recovers it to. *)
