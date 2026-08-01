(** Minimal JSON writer for the browser driver's input file. *)

type t =
  | Str of string  (** JSON string. *)
  | Int of int  (** JSON number. *)
  | Arr of t list  (** JSON array. *)
  | Obj of (string * t) list  (** JSON object. *)

val to_string : t -> string
(** [to_string json] serialises [json]. Every code point below U+0020 and the
    characters [<], [>] and [&] are escaped, so the result is safe to embed
    verbatim in a [<script>] element. *)

val pp : t Fmt.t
(** [pp] prints {!to_string}. *)
