(** Shared assertion helpers used across the fuzz entry points. *)

val assert_invalid_declaration_contract : string -> string -> unit
(** [assert_invalid_declaration_contract label input] asserts that [input] is
    rejected as a declaration under [~strict:true] and recovers (with a warning)
    under [~strict:false]. Used by fuzz tests to pin the strict-rejects /
    lenient-recovers contract for invalid declarations. *)
