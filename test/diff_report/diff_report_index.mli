(** A parsed stylesheet indexed the way a [cascade diff] report names it. *)

val contains : string -> string -> bool
(** [contains haystack needle] is [true] iff [needle] occurs in [haystack]. *)

val shown_value : Cascade.Css.declaration -> string
(** [shown_value d] is the value of [d] as a report prints it, with [!important]
    when [d] carries it. *)

type rule_entry = { selector : string; props : (string * string) list }
(** A rule or keyframe: its selector and its property, value pairs. *)

type sheet_index = {
  rules : rule_entry list;
  heads : string list;
  source : string;  (** the file as written, for a head the printer drops *)
}
(** The rules of a sheet at any depth and the heads of its statements. *)

val rule_index : Cascade.Css.t -> rule_entry list
(** [rule_index sheet] is every rule of [sheet] at any depth, in document order.
*)

val index : source:string -> Cascade.Css.t -> sheet_index
(** [index ~source sheet] is the index of [sheet], parsed from [source]. *)

val parse : string -> Cascade.Css.t option
(** [parse text] is the stylesheet [text] reads as leniently, if any. *)

val index_of_text : string -> sheet_index option
(** [index_of_text text] is the index of [text], if it parses. *)

val tight : string -> string
(** [tight s] is [s] without spaces, tabs and newlines. *)

val index_for : Cascade_diff.Css_compare.mode -> string -> sheet_index option
(** [index_for mode text] is the index a claim about [text] is checked against
    under [mode]: under [`Canonical], [text] and its canonical form together. *)

val has_pair : string * string -> (string * string) list -> bool
(** [has_pair (s, p) l] is [true] iff [l] holds the pair [(s, p)]. *)

val has_selector : sheet_index -> string -> bool
(** [has_selector idx s] is [true] iff [idx] names [s], as a rule, a statement
    head or, for an at-rule, a span of the source. *)

val has_decl : sheet_index -> string -> string -> string -> bool
(** [has_decl idx s p v] is [true] iff a rule of [idx] with selector [s] sets
    [p] to [v]. *)
