(** Spec-derived vectors for the [@font-face] descriptors.

    A descriptor is not a property, so {!Property_grammar} cannot carry these:
    [src] and [unicode-range] have no property of the same name, and the ones
    that do ([font-style], [font-weight]) are given a narrower grammar inside
    the rule than outside it. Every row names the CSS Fonts section that grants
    or refuses the vector, and a browser arbitrates the row rather than cascade.

    A positive is a complete descriptor value the specification grants. A
    negative is one it does not, and it may still be a valid fragment: [10deg]
    is no [font-style] alone and a good one after [oblique]. *)

type row = {
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
(** Every [@font-face] descriptor this project has spec vectors for. *)

val descriptors : string list
(** [descriptors] is the descriptor names {!rows} covers. *)

val row_for : string -> row option
(** [row_for descriptor] is the row named [descriptor]. *)
