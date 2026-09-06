(** Shared CSS Conditional Rules supports-condition grammar vectors. *)

type expected =
  | Property of string * string
  | Func of string * string
  | Not of expected
  | And of expected * expected
  | Or of expected * expected

type row = { name : string; input : string; expected : expected }

val rows : row list
(** [rows] contains valid supports-condition branches with expected ASTs. *)

val invalid : string list
(** [invalid] contains invalid supports-condition grammar branches. *)

val general_enclosed_functions : string list
(** Functions with unsupported arguments, preserved as general-enclosed. *)

val mutate_invalid : row -> int -> string
(** [mutate_invalid row salt] returns an invalid condition derived from [row].
*)
