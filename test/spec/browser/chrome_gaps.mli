(** What a headless Chrome cannot arbitrate, and the vectors where it and the
    specifications disagree. Shared by the browser-backed harnesses here, so a
    browser that catches up is recorded once rather than in each of them. *)

val unimplemented : string list
(** [unimplemented] is the property names Chrome implements no grammar for, so
    it accepts no vector for them and cannot answer about them. Each harness
    checks the list against its own population in both directions. *)

val unimplemented_property : string -> bool
(** [unimplemented_property name] is [true] when [name] is in {!unimplemented}.
*)

type excuse = { properties : string list; value : string; why : string }
(** One property-value pair the browser and the specifications disagree about,
    with the spec text that decides it. *)

val spec_ahead : excuse list
(** [spec_ahead] is grammar a specification defines and Chrome rejects, so a
    Chrome rejection says nothing about the value. *)

val lenient : excuse list
(** [lenient] is a value Chrome accepts that no specification grants, so a
    Chrome acceptance says nothing about the value. *)

val find : excuse list -> property:string -> value:string -> excuse option
(** [find table ~property ~value] is the entry of [table] covering that pair. *)
