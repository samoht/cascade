(** Helpers shared by the declaration test suites. *)

open Cascade

val check_declaration :
  ?minify:bool ->
  ?roundtrip:bool ->
  ?expected:string ->
  ?optimized:string ->
  string ->
  unit
(** [check_declaration input] reads [input] as a declaration and checks how it
    prints; [optimized] is what the optimizer folds it into. *)

val check :
  ?minify:bool ->
  ?roundtrip:bool ->
  ?expected:string ->
  ?optimized:string ->
  string ->
  unit
(** [check] is {!check_declaration}. *)

val check_specified_value : string -> string -> string -> unit
(** [check_specified_value name css expected] checks that the declaration [css]
    prints its value as [expected]. *)

val check_declarations : string -> int -> Css.Declaration.declaration list
(** [check_declarations input n] reads the declarations of [input], checks there
    are [n] of them and returns them. *)
