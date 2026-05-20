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

val srgb_to_linear : float -> float
(** CSS Color 4 sec. 11.5.1: [<R G B>] in [[0, 1]] linearised via the standard
    [(v + 0.055) / 1.055)^2.4] curve below the toe at [0.04045]. *)

val linear_to_srgb : float -> float
(** Inverse of [srgb_to_linear]. *)

val rgb_to_linear_rgb : rgb -> rgb
val linear_rgb_to_rgb : rgb -> rgb

(** {1 Predefined RGB spaces} *)

val linear_srgb_to_xyz_d65 : rgb -> xyz
val xyz_d65_to_linear_srgb : xyz -> rgb
val linear_display_p3_to_xyz_d65 : rgb -> xyz
val xyz_d65_to_linear_display_p3 : xyz -> rgb
val display_p3_to_linear : float -> float
val linear_to_display_p3 : float -> float
val linear_a98_rgb_to_xyz_d65 : rgb -> xyz
val xyz_d65_to_linear_a98_rgb : xyz -> rgb
val a98_rgb_to_linear : float -> float
val linear_to_a98_rgb : float -> float
val linear_prophoto_rgb_to_xyz_d50 : rgb -> xyz
val xyz_d50_to_linear_prophoto_rgb : xyz -> rgb
val prophoto_rgb_to_linear : float -> float
val linear_to_prophoto_rgb : float -> float
val linear_rec2020_to_xyz_d65 : rgb -> xyz
val xyz_d65_to_linear_rec2020 : xyz -> rgb
val rec2020_to_linear : float -> float
val linear_to_rec2020 : float -> float

(** {1 Chromatic adaptation (Bradford)} *)

val xyz_d65_to_d50 : xyz -> xyz
val xyz_d50_to_d65 : xyz -> xyz

(** {1 CIE Lab / LCH} *)

val xyz_d50_to_lab : xyz -> lab
val lab_to_xyz_d50 : lab -> xyz
val lab_to_lch : lab -> lch
val lch_to_lab : lch -> lab

(** {1 OKLab / OKLCH} *)

val linear_srgb_to_oklab : rgb -> lab
val oklab_to_linear_srgb : lab -> rgb
val oklab_to_oklch : lab -> lch
val oklch_to_oklab : lch -> lab

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
