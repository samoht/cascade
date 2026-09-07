(** Spec-derived vectors for the at-rule descriptors.

    A descriptor is not a property, so {!Property_grammar} cannot carry these:
    [src] and [unicode-range] have no property of the same name, and the ones
    that do ([font-style], [font-weight]) are given a narrower grammar inside
    the rule than outside it. Every row names the CSS Fonts section that grants
    or refuses the vector, and a browser arbitrates the row rather than cascade.

    A positive is a complete descriptor value the specification grants. A
    negative is one it does not, and it may still be a valid fragment: [10deg]
    is no [font-style] alone and a good one after [oblique]. *)

type row = {
  at_rule : string;  (** the at-rule the descriptor belongs to, without [@] *)
  descriptor : string;  (** the descriptor name, as written in the rule *)
  positives : string list;
  negatives : string list;
  why : string;  (** the section that spells the grammar *)
}
(** A row is the citation. Where a browser and a row disagree, the row decides
    and the divergence is reported as the browser's: it is the section that says
    whether [font-weight: bold 400] is a value, not the engine in front of it.
*)

val rows : row list
(** Every [@font-face] descriptor a current specification defines. A descriptor
    a specification has REMOVED has no row: there is no grammar left to check a
    browser against, and writing one from the section that used to define it
    would put a harness in the position of testing a browser's leftovers. *)

val descriptors : string list
(** [descriptors] is the descriptor names {!rows} covers. *)

val row_for : at_rule:string -> string -> row option
(** [row_for ~at_rule descriptor] is the row for that descriptor of that
    at-rule. The name alone is not enough: [font-family] is a descriptor of
    [\@font-face] and of [\@font-palette-values], with different grammars. *)
