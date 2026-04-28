(** Shared CSS Syntax tokenizer and component parser branch vectors. *)

type token_row = { branch : string; input : string; expected : string list }
type parser_row = { branch : string; input : string }

val token_rows : token_row list
(** [token_rows] contains tokenizer branch vectors and expected token strings.
*)

val parser_rows : parser_row list
(** [parser_rows] contains component parser branch vectors. *)

val mutate_parser_input : parser_row -> int -> string
(** [mutate_parser_input row salt] derives parser recovery input from [row]. *)
