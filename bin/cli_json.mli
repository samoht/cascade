(** A JSON writer for the CLI's machine-readable output.

    Cascade has no JSON dependency and this is the only place that needs one, so
    the writer lives here rather than in the library. It writes, never reads:
    the only values it has to render are the ones a command builds. *)

(** A JSON value. Numbers are integers because every number the CLI reports is a
    count or a position. *)
type t =
  | Bool of bool
  | Int of int
  | String of string
  | List of t list
  | Obj of (string * t) list

val to_string : t -> string
(** [to_string t] renders [t] as an indented JSON document with no trailing
    newline. Strings are escaped so that a CSS selector or value carrying a
    quote, a backslash or a control byte survives the round trip; bytes at or
    above [0x80] pass through, so UTF-8 input stays UTF-8 output. *)

val pp : t Fmt.t
(** [pp] prints a value as a JSON document, as {!to_string} renders it. *)
