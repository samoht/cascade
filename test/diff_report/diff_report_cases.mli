(** Constructed pairs: the neutral arm, the one-site changes the model cannot
    write, and the controls that must differ. *)

val neutral_pairs : (string * string * string) list
(** [neutral_pairs] is [(name, expected, actual)] pairs that write one
    stylesheet two ways, so canonical mode must report no difference. *)

type text_case = {
  case : string;
  before : string;
  after : string;
  names : string list;
  under : string list;
  holds : (string * string * string * string) option;
}
(** A one-site change written as text, with what a report must name for it. *)

val text_cases : text_case list
(** [text_cases] is the one-site changes the model generator cannot write. *)

val differing_pairs : (string * string * string) list
(** [differing_pairs] is [(name, expected, actual)] pairs that do differ. *)
