(** A seeded model of a stylesheet, the one-site mutations it admits, and the
    site each mutation touched. *)

type mdecl = { prop : string; value : string; important : bool }
(** A modelled declaration. *)

type mrule = { sel : string; decls : mdecl list }
(** A modelled rule. *)

(** A top-level block: one rule, or rules under a condition. *)
type mblock = Flat of mrule | Grouped of { cond : string; rules : mrule list }

val model : seed:int -> mblock list
(** [model ~seed] is the stylesheet model [seed] generates. *)

val render : mblock list -> string
(** [render m] is [m] written as CSS. *)

val model_rules : mblock list -> mrule list
(** [model_rules m] is the rules of [m] in document order. *)

(** A one-site change, addressed by rule and declaration index. *)
type mutation =
  | Change_value of { rule : int; decl : int; value : string }
  | Drop_decl of { rule : int; decl : int }
  | Add_decl of { rule : int; prop : string; value : string }
  | Drop_rule of int
  | Add_rule of { sel : string; prop : string; value : string }
  | Swap_rules of int  (** top-level block [i] with block [i+1] *)
  | Rename_selector of { rule : int; sel : string }
  | Swap_decls of { rule : int; decl : int }
      (** two writes of one property swapped, so the cascade changes *)
  | Toggle_important of { rule : int; decl : int }
  | Drop_container of int  (** a whole conditional block *)

val apply : mblock list -> mutation -> mblock list
(** [apply m mutation] is [m] with [mutation] applied. *)

val label : mutation -> string
(** [label mutation] names [mutation] in a finding. *)

val mutations : mblock list -> mutation list
(** [mutations m] is every mutation [m] admits. *)

type site = {
  selectors : string list;  (** every selector the change touched *)
  containers : string list;  (** the container conditions it reached *)
  value_change : (string * string * string * string) option;
      (** selector, property, expected value, actual value *)
  gone : (string * string) option;  (** selector, property removed *)
  came : (string * string) option;  (** selector, property added *)
}
(** What a mutation touched, read from the two parsed sheets. *)

val empty_site : site
(** [empty_site] touches nothing. *)

val site_of :
  before:Diff_report_index.rule_entry list ->
  after:Diff_report_index.rule_entry list ->
  mblock list ->
  mutation ->
  site option
(** [site_of ~before ~after m mutation] is the site [mutation] touched in [m],
    or [None] when the parsed sheets do not line up with the model. *)
