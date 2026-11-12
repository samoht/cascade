(** Four-side box shorthands ([margin], [padding], [inset], [border-width] and
    the like, [grid-area]) and the [overflow] pair: composing longhand runs and
    folding later longhands into an earlier shorthand. *)

type sides = Values.length * Values.length * Values.length * Values.length
(** The four sides of a length box, top first, clockwise. *)

(** One side of a box. *)
type box_side = Top | Right | Bottom | Left

val merge_overflow_longhands :
  (int * Declaration.declaration) list -> (int * Declaration.declaration) list
(** [merge_overflow_longhands decls] folds [overflow-x]/[overflow-y] into
    [overflow]. *)

val border_width_of :
  Declaration.declaration -> (box_side * Properties.border_width * bool) option
(** [border_width_of d] is the side, value and importance of a
    [border-<side>-width] longhand [d]. *)

val border_style_of :
  Declaration.declaration -> (box_side * Properties.border_style * bool) option
(** [border_style_of d] is the side, value and importance of a
    [border-<side>-style] longhand [d]. *)

val border_color_of :
  Declaration.declaration -> (box_side * Values.color * bool) option
(** [border_color_of d] is the side, value and importance of a
    [border-<side>-color] longhand [d]. *)

val same_border_width :
  Properties.border_width -> Properties.border_width -> bool
(** [same_border_width a b] is typed equality on canonical border widths. *)

val same_border_style :
  Properties.border_style -> Properties.border_style -> bool
(** [same_border_style a b] is typed equality on border styles. *)

val foldable_length_strict : Values.length -> bool
(** [foldable_length_strict v] is [true] when [v] holds no runtime substitution.
*)

val foldable_border_width : ctx:Ctx.t -> Properties.border_width -> bool
(** [foldable_border_width ~ctx w] is [true] when [w] may move into a positional
    shorthand: no [var()], or one [ctx] registers. *)

val foldable_border_style : ctx:Ctx.t -> Properties.border_style -> bool
(** [foldable_border_style ~ctx s] is {!val-foldable_border_width} for a style.
*)

val foldable_border_color : ctx:Ctx.t -> Values.color -> bool
(** [foldable_border_color ~ctx c] is {!val-foldable_border_width} for a colour.
*)

val foldable_grid_line : Properties.grid_line -> bool
(** [foldable_grid_line l] is [true] when [l] is resolved, so it stands for one
    line rather than a whole [<start> / <end>]. *)

val compose_box_via_index : ctx:Ctx.t -> Rule_index.t -> unit
(** [compose_box_via_index ~ctx idx] absorbs every four-side longhand run of
    [idx] into its box shorthand. *)

val merge_box_shorthand_longhands :
  ('a * Declaration.declaration) list ->
  ('a * Declaration.declaration) list ->
  ('a * Declaration.declaration) list
(** [merge_box_shorthand_longhands source decls] folds later [margin] and
    [padding] longhands of [decls] into a preceding shorthand that [source]
    shows no earlier longhand for. *)
