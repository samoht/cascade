(** Static colour-space conversion arithmetic for the optimiser.

    Implements the matrices and gamma-correction functions in CSS Color 4
    sections 8-16. All functions operate on [float] triples where each component
    is the canonical floating-point representation for the named space (sRGB /
    Display-P3 in [[0, 1]], XYZ in tristimulus units, Lab L* in [[0, 100]],
    OKLab L in [[0, 1]], hue in degrees, ...).

    The arithmetic is pure: it has no opinion about CSS syntax, parsing, or
    pretty-printing. [Values.ml] is the only consumer; the optimiser folds
    static colours by routing them through this module. *)

type rgb = float * float * float
type xyz = float * float * float
type lab = float * float * float
type lch = float * float * float

(** {1 sRGB} *)

val linear_of_srgb : float -> float
(** CSS Color 4 sec. 11.5.1: [<R G B>] in [[0, 1]] linearised via the standard
    [(v + 0.055) / 1.055)^2.4] curve below the toe at [0.04045]. *)

val srgb_of_linear : float -> float
(** Inverse of {!val-linear_of_srgb}. *)

val linear_rgb_of_rgb : rgb -> rgb
(** Applies {!val-linear_of_srgb} to each channel. *)

val rgb_of_linear_rgb : rgb -> rgb
(** Applies {!val-srgb_of_linear} to each channel. *)

(** {1 Predefined RGB spaces} *)

val xyz65_of_linear_srgb : rgb -> xyz
(** Converts linear sRGB to XYZ with the D65 white point. *)

val linear_srgb_of_xyz65 : xyz -> rgb
(** Converts XYZ-D65 to linear sRGB. *)

val xyz65_of_linear_p3 : rgb -> xyz
(** Converts linear Display-P3 to XYZ-D65. *)

val linear_p3_of_xyz65 : xyz -> rgb
(** Converts XYZ-D65 to linear Display-P3. *)

val linear_of_display_p3 : float -> float
(** Linearises a Display-P3 channel. *)

val display_p3_of_linear : float -> float
(** Encodes a linear Display-P3 channel. *)

val xyz65_of_linear_a98 : rgb -> xyz
(** Converts linear A98-RGB to XYZ-D65. *)

val linear_a98_of_xyz65 : xyz -> rgb
(** Converts XYZ-D65 to linear A98-RGB. *)

val linear_of_a98_rgb : float -> float
(** Linearises an A98-RGB channel. *)

val a98_rgb_of_linear : float -> float
(** Encodes a linear A98-RGB channel. *)

val xyz50_of_linear_prophoto : rgb -> xyz
(** Converts linear ProPhoto-RGB to XYZ-D50. *)

val linear_prophoto_of_xyz50 : xyz -> rgb
(** Converts XYZ-D50 to linear ProPhoto-RGB. *)

val linear_of_prophoto_rgb : float -> float
(** Linearises a ProPhoto-RGB channel. *)

val prophoto_rgb_of_linear : float -> float
(** Encodes a linear ProPhoto-RGB channel. *)

val xyz65_of_linear_rec2020 : rgb -> xyz
(** Converts linear Rec.2020 to XYZ-D65. *)

val linear_rec2020_of_xyz65 : xyz -> rgb
(** Converts XYZ-D65 to linear Rec.2020. *)

val linear_of_rec2020 : float -> float
(** Linearises a Rec.2020 channel. *)

val rec2020_of_linear : float -> float
(** Encodes a linear Rec.2020 channel. *)

(** {1 Chromatic adaptation (Bradford)} *)

val d50_of_xyz65 : xyz -> xyz
(** Adapts XYZ-D65 coordinates to XYZ-D50. *)

val d65_of_xyz50 : xyz -> xyz
(** Adapts XYZ-D50 coordinates to XYZ-D65. *)

(** {1 CIE Lab / LCH} *)

val lab_of_xyz50 : xyz -> lab
(** Converts XYZ-D50 to CIE Lab. *)

val xyz50_of_lab : lab -> xyz
(** Converts CIE Lab to XYZ-D50. *)

val lch_of_lab : lab -> lch
(** Converts CIE Lab to polar LCH. *)

val lab_of_lch : lch -> lab
(** Converts polar LCH to CIE Lab. *)

(** {1 OKLab / OKLCH} *)

val oklab_of_linear_srgb : rgb -> lab
(** Converts linear sRGB to OKLab. *)

val linear_srgb_of_oklab : lab -> rgb
(** Converts OKLab to linear sRGB. *)

val oklch_of_oklab : lab -> lch
(** Converts OKLab to OKLCH. *)

val oklab_of_oklch : lch -> lab
(** Converts OKLCH to OKLab. *)

val oklab_distance : lab -> lab -> float
(** [oklab_distance a b] is the Euclidean distance between two OKLab colours,
    the perceptual difference metric used by the colour-folding budget. *)

val srgb_bytes_of_linear : ?budget:float -> rgb -> (int * int * int) option
(** [srgb_bytes_of_linear linear] is the nearest 8-bit sRGB byte triple for the
    linear-sRGB colour [linear], or [None] when it is out of the sRGB gamut or
    its 8-bit quantisation lies more than [budget] (default [0.002]) in OKLab
    distance from the source. Alpha is not considered. *)

(** {1 Hue interpolation} *)

type hue_interpolation =
  | Shorter  (** Default per CSS Color 4 sec. 12.4. *)
  | Longer
  | Increasing
  | Decreasing

val interpolate_hue : hue_interpolation -> float -> float -> float -> float
(** [interpolate_hue method h1 h2 t] returns the hue at parameter [t] in
    [[0, 1]] along the path from [h1] to [h2] using the given interpolation
    [method]. The result is normalised to [\[0, 360)]. *)
