(** CSS Property value parsing using Reader API. *)

val read_display : Css.Reader.t -> Css.display
(** [read_display t] reads a display value. *)

val read_position : Css.Reader.t -> Css.position
(** [read_position t] reads a position value. *)

val read_flex_direction : Css.Reader.t -> Css.flex_direction
(** [read_flex_direction t] reads a flex-direction value. *)

val read_align_items : Css.Reader.t -> Css.align_items
(** [read_align_items t] reads an align-items value. *)

val read_justify_content : Css.Reader.t -> Css.justify_content
(** [read_justify_content t] reads a justify-content value. *)

val read_font_weight : Css.Reader.t -> Css.font_weight
(** [read_font_weight t] reads a font-weight value. *)

val read_font_style : Css.Reader.t -> Css.font_style
(** [read_font_style t] reads a font-style value. *)

val read_text_align : Css.Reader.t -> Css.text_align
(** [read_text_align t] reads a text-align value. *)

val read_text_decoration : Css.Reader.t -> Css.text_decoration
(** [read_text_decoration t] reads a text-decoration value. *)

val read_text_transform : Css.Reader.t -> Css.text_transform
(** [read_text_transform t] reads a text-transform value. *)

val read_overflow : Css.Reader.t -> Css.overflow
(** [read_overflow t] reads an overflow value. *)

val read_cursor : Css.Reader.t -> Css.cursor
(** [read_cursor t] reads a cursor value. *)

val read_box_shadow : Css.Reader.t -> Css.box_shadow
(** [read_box_shadow t] reads a box-shadow value. *)

val read_transform : Css.Reader.t -> Css.transform
(** [read_transform t] reads a single transform function. *)

val read_transform_list : Css.Reader.t -> Css.transform list
(** [read_transform_list t] reads a list of transform functions. *)

val parse_by_name : string -> string -> Css.declaration option
(** [parse_by_name name value] parses [value] based on CSS property [name]. *)
