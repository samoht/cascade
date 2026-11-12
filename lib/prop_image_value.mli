(** The CSS Images 4 [<image>] value ([url()], [image-set()], [cross-fade()] and
    the gradient functions), and the image-orientation, image-rendering and
    image-resolution properties. *)

open Properties_intf

val pp_background_image : background_image Pp.t
(** [pp_background_image] prints a [background_image]. *)

val normalize_background_image :
  ?lossless:bool -> background_image -> background_image
(** [normalize_background_image ?lossless i] is [i] in its canonical spelling,
    gradient stops folded; [lossless] keeps colour channels unrounded. *)

val pp_image_orientation : image_orientation Pp.t
(** [pp_image_orientation] prints an [image_orientation]. *)

val pp_image_rendering : image_rendering Pp.t
(** [pp_image_rendering] prints an [image_rendering]. *)

val pp_resolution : Pp.ctx -> resolution -> unit
(** [pp_resolution] prints a [<resolution>]. *)

val pp_image_resolution : image_resolution Pp.t
(** [pp_image_resolution] prints an [image_resolution]. *)

val read_image_orientation : Cursor.t -> image_orientation
(** [read_image_orientation t] reads an [image_orientation] from [t]. *)

val read_image_rendering : Cursor.t -> image_rendering
(** [read_image_rendering t] reads an [image_rendering] from [t]. *)

val read_resolution : Cursor.t -> resolution
(** [read_resolution t] reads a [resolution] from [t]. *)

val read_image_resolution : Cursor.t -> image_resolution
(** [read_image_resolution t] reads an [image_resolution] from [t]. *)

val read_bg_image : Cursor.t -> background_image
(** [read_bg_image t] reads a [background_image] from [t]. *)

val read_background_image : Cursor.t -> background_image
(** [read_background_image t] reads a [background_image] from [t]. *)

val read_background_images : Cursor.t -> background_image list
(** [read_background_images t] reads a comma-separated list of images from [t].
*)

val minify_background_image : background_image -> background_image
(** [minify_background_image i] is [i] with its gradient stop colours minified.
*)
