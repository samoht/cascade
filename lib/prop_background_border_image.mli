(** The [border-image] and [mask-border] shorthands and their slice, width,
    outset, repeat and source longhands. *)

val pp_border_image_slice_item : Properties_intf.border_image_slice_item Pp.t
(** [pp_border_image_slice_item] prints a [border-image-slice-item] value. *)

val pp_border_image_slice_offsets :
  Pp.ctx -> Properties_intf.border_image_slice_offsets -> unit
(** [pp_border_image_slice_offsets] prints a [border-image-slice-offsets] value.
*)

val pp_border_image_slice : Properties_intf.border_image_slice Pp.t
(** [pp_border_image_slice] prints a [border-image-slice] value. *)

val pp_border_image_width_item :
  Pp.ctx -> Properties_intf.border_image_width_item -> unit
(** [pp_border_image_width_item] prints a [border-image-width-item] value. *)

val pp_border_image_outset_item :
  Pp.ctx -> Properties_intf.border_image_outset_item -> unit
(** [pp_border_image_outset_item] prints a [border-image-outset-item] value. *)

val pp_border_image_repeat_keyword :
  Pp.ctx -> Properties_intf.border_image_repeat_keyword -> unit
(** [pp_border_image_repeat_keyword] prints a [border-image-repeat-keyword]
    value. *)

val pp_border_image_repeat : Properties_intf.border_image_repeat Pp.t
(** [pp_border_image_repeat] prints a [border-image-repeat] value. *)

val pp_border_image_width : Properties_intf.border_image_width Pp.t
(** [pp_border_image_width] prints a [border-image-width] value. *)

val pp_border_image_outset : Properties_intf.border_image_outset Pp.t
(** [pp_border_image_outset] prints a [border-image-outset] value. *)

val pp_mask_border_mode : Pp.ctx -> Properties_intf.mask_border_mode -> unit
(** [pp_mask_border_mode] prints a [mask-border-mode] value. *)

val normalize_border_image :
  Properties_intf.border_image -> Properties_intf.border_image
(** [normalize_border_image v] is [v] in canonical form. *)

val pp_border_image : Properties_intf.border_image Pp.t
(** [pp_border_image] prints a [border-image] value. *)

val normalize_mask_border :
  Properties_intf.border_image -> Properties_intf.border_image
(** [normalize_mask_border v] is [v] in canonical form. *)

val read_border_image_repeat_keyword :
  Cursor.t -> Properties_intf.border_image_repeat_keyword
(** [read_border_image_repeat_keyword t] reads a [border-image-repeat-keyword]
    value from [t]. *)

val read_border_image_slice_item :
  Cursor.t -> Properties_intf.border_image_slice_item
(** [read_border_image_slice_item t] reads a [border-image-slice-item] value
    from [t]. *)

val read_border_image_slice_offsets :
  Cursor.t -> Properties_intf.border_image_slice_offsets
(** [read_border_image_slice_offsets t] reads a [border-image-slice-offsets]
    value from [t]. *)

val read_border_image_slice : Cursor.t -> Properties_intf.border_image_slice
(** [read_border_image_slice t] reads a [border-image-slice] value from [t]. *)

val read_border_image_width_item :
  Cursor.t -> Properties_intf.border_image_width_item
(** [read_border_image_width_item t] reads a [border-image-width-item] value
    from [t]. *)

val read_border_image_outset_item :
  Cursor.t -> Properties_intf.border_image_outset_item
(** [read_border_image_outset_item t] reads a [border-image-outset-item] value
    from [t]. *)

val read_border_image_repeat : Cursor.t -> Properties_intf.border_image_repeat
(** [read_border_image_repeat t] reads a [border-image-repeat] value from [t].
*)

val read_border_image_width : Cursor.t -> Properties_intf.border_image_width
(** [read_border_image_width t] reads a [border-image-width] value from [t]. *)

val read_border_image_outset : Cursor.t -> Properties_intf.border_image_outset
(** [read_border_image_outset t] reads a [border-image-outset] value from [t].
*)

val read_mask_border_mode : Cursor.t -> Properties_intf.mask_border_mode
(** [read_mask_border_mode t] reads a [mask-border-mode] value from [t]. *)

val read_border_image_source : Cursor.t -> Properties_intf.background_image
(** [read_border_image_source t] reads a [border-image-source] value from [t].
*)

val read_border_image : Cursor.t -> Properties_intf.border_image
(** [read_border_image t] reads a [border-image] value from [t]. *)

val read_mask_border : Cursor.t -> Properties_intf.border_image
(** [read_mask_border t] reads a [mask-border] value from [t]. *)
