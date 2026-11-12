(** SVG paint, stroke and vector-effect value types. *)

open Values

(* SVG Types *)
(* SVG paint servers allow url(#id) with optional fallback (none/currentcolor/color). *)

(** SVG paint values for fill and stroke properties *)
type svg_paint =
  | None  (** No paint *)
  | Inherit  (** Inherited value *)
  | Current_color  (** Current color value *)
  | Color of color  (** Specific color value *)
  | Url of string * svg_paint option  (** url(#id) with optional fallback *)
  | Context_fill
      (** SVG2 sec. 13.2 [context-fill] - inherits the fill paint of the context
          element, used in marker / pattern / use trees. *)
  | Context_stroke
      (** SVG2 sec. 13.2 [context-stroke] - mirror of [Context_fill]. *)
  | Var of svg_paint var

(** SVG 2 sec. 13.4.2 and CSS Masking 1 sec. 6.2 [<fill-rule>]: which points
    count as inside a shape when its subpaths overlap. Shared by [fill-rule] and
    [clip-rule]; the argument form inside [polygon()] is {!clip_path_fill_rule},
    which carries no CSS-wide keywords. *)
type fill_rule =
  | Nonzero
  | Evenodd
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of fill_rule var

(** SVG 2 sec. 13.5.3 [stroke-width]: [<length-percentage> | <number>], where a
    bare [<number>] is a width in user units. That number is not a CSS
    [<length>], which is why this is not plain [length_percentage]; sec. 13.5.6
    gives the dash lengths the same shape. A negative width is invalid. *)
type stroke_width =
  | Number of float  (** A width in user units *)
  | Calc of stroke_width calc
      (** A math function answering the [<number>] branch. CSS Values 4 sec.
          10.12 checks the [0,inf] range on what it resolves to, so a call the
          literal grammar refuses is still a declaration. *)
  | Length of length_percentage
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of stroke_width var

(** SVG 2 sec. 13.5.4 [stroke-linecap]: the shape drawn at the ends of an open
    subpath and at the ends of each dash. *)
type stroke_linecap =
  | Butt
  | Round
  | Square
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of stroke_linecap var

(** SVG 2 sec. 13.5.5 [stroke-linejoin]: the shape drawn where two path segments
    meet. [miter_clip] and [arcs] are the Level 2 additions. *)
type stroke_linejoin =
  | Miter
  | Miter_clip
  | Round
  | Bevel
  | Arcs
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of stroke_linejoin var

(** SVG 2 sec. 13.5.5 [stroke-miterlimit]: the ratio at which a miter join is
    converted to a bevel. Only a negative value is illegal. *)
type stroke_miterlimit =
  | Number of float
  | Calc of stroke_miterlimit calc
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of stroke_miterlimit var

(** SVG 2 sec. 13.5.6: one dash length. A bare [<number>] is in user units,
    which is why this is not plain [length_percentage]. CSS Values 4 sec. 10.1
    puts a math function wherever a number is, so that half is a {!type-number}.
*)
type dash_length = Number of number | Length of length_percentage

(** SVG 2 sec. 13.5.6 [stroke-dashoffset]. *)
type stroke_dashoffset =
  | Dash of dash_length
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of stroke_dashoffset var

(** SVG 2 sec. 13.5.6 [stroke-dasharray]: [none] or a dash pattern. The grammar
    separates entries by comma and/or whitespace and the rendered pattern is the
    flat sequence either way, so both spellings read to one list. *)
type stroke_dasharray =
  | None
  | Dashes of dash_length list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of stroke_dasharray var

(** SVG 2 sec. 13.8 [paint-order] operand. *)
type paint_order_keyword = Fill | Stroke | Markers

(** SVG 2 sec. 13.8 [paint-order]: [normal | [ fill || stroke || markers ]]. The
    written order is the paint order, and any keyword left out is painted last
    in the order [normal] would use, so a trailing run that already matches
    [normal] is redundant. Entries are distinct: [||] takes each operand at most
    once. *)
type paint_order =
  | Normal
  | Order of paint_order_keyword list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of paint_order var

(** SVG 2 sec. 8.13 [vector-effect] operand. *)
type vector_effect_keyword =
  | Non_scaling_stroke
  | Non_scaling_size
  | Non_rotation
  | Fixed_position

(** SVG 2 sec. 8.13 host coordinate space for a vector effect. [viewport] is
    what an omitted space means. *)
type vector_effect_space = Viewport | Screen

(** SVG 2 sec. 8.13 [vector-effect]:
    [none | [ non-scaling-stroke | non-scaling-size | non-rotation |
     fixed-position ]+ [ viewport | screen ]?]. *)
type vector_effect =
  | None
  | Effects of vector_effect_keyword list * vector_effect_space option
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of vector_effect var
