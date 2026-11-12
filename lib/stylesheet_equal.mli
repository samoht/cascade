(** Statement identity: equality and the hashes consistent with it. *)

open Stylesheet_intf

val equal_layer_name : layer_name -> layer_name -> bool
(** [equal_layer_name a b] is whether [a] and [b] name the same idents in the
    same order. *)

val equal_statement : statement -> statement -> bool
(** [equal_statement a b] is whether [a] and [b] are the same statement, each
    part compared through the equality its own module states. *)

val hash_statement : statement -> int
(** [hash_statement stmt] is a fingerprint of [stmt] consistent with
    {!equal_statement}. *)

val rule_hash : rule -> int
(** [rule_hash r] folds the cached hash of each declaration of [r] over the hash
    of its selector. *)
