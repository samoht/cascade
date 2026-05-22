(** Static colour-space conversion arithmetic for the optimiser.

    All matrices and constants are the CSS Color 4 sections 8-16 reference
    values. The conversions are pure floating-point arithmetic; no CSS syntax
    knowledge lives here. *)

type rgb = float * float * float
type xyz = float * float * float
type lab = float * float * float
type lch = float * float * float

let matrix_mul ((a, b, c), (d, e, f), (g, h, i)) (x, y, z) =
  ( (a *. x) +. (b *. y) +. (c *. z),
    (d *. x) +. (e *. y) +. (f *. z),
    (g *. x) +. (h *. y) +. (i *. z) )

(* sRGB ↔ linear-sRGB. CSS Color 4 sec. 11.5.1. *)

let srgb_to_linear v =
  let s = Float.copy_sign 1.0 v in
  let v = Float.abs v in
  if v <= 0.04045 then s *. (v /. 12.92)
  else s *. (((v +. 0.055) /. 1.055) ** 2.4)

let linear_to_srgb v =
  let s = Float.copy_sign 1.0 v in
  let v = Float.abs v in
  if v <= 0.0031308 then s *. (v *. 12.92)
  else s *. ((1.055 *. (v ** (1.0 /. 2.4))) -. 0.055)

let rgb_to_linear_rgb (r, g, b) =
  (srgb_to_linear r, srgb_to_linear g, srgb_to_linear b)

let linear_rgb_to_rgb (r, g, b) =
  (linear_to_srgb r, linear_to_srgb g, linear_to_srgb b)

(* Linear sRGB ↔ XYZ-D65. CSS Color 4 sec. 9.1 reference matrix. *)

let m_linear_srgb_to_xyz_d65 =
  ( (0.4123907992659593, 0.357584339383878, 0.1804807884018343),
    (0.21263900587151025, 0.715168678767756, 0.07219231536073371),
    (0.01933081871559185, 0.11919477979462598, 0.9505321522496607) )

let m_xyz_d65_to_linear_srgb =
  ( (3.2409699419045226, -1.537383177570094, -0.4986107602930034),
    (-0.9692436362808796, 1.8759675015077202, 0.04155505740717561),
    (0.05563007969699366, -0.20397695888897652, 1.0569715142428786) )

let linear_srgb_to_xyz_d65 rgb = matrix_mul m_linear_srgb_to_xyz_d65 rgb
let xyz_d65_to_linear_srgb xyz = matrix_mul m_xyz_d65_to_linear_srgb xyz

(* Display-P3 ↔ XYZ-D65. CSS Color 4 sec. 10.4 reference matrix. The
   non-linearity is identical to sRGB. *)

let display_p3_to_linear = srgb_to_linear
let linear_to_display_p3 = linear_to_srgb

let m_linear_p3_to_xyz_d65 =
  ( (0.4865709486482162, 0.26566769316909306, 0.1982172852343625),
    (0.2289745640697488, 0.6917385218365064, 0.079286914093745),
    (0.0, 0.04511338185890264, 1.043944368900976) )

let m_xyz_d65_to_linear_p3 =
  ( (2.4934969119414255, -0.9313836179191242, -0.40271078445071684),
    (-0.8294889695615747, 1.7626640603183465, 0.023624685841943573),
    (0.03584583024378445, -0.07617238926804184, 0.9568845240076872) )

let linear_display_p3_to_xyz_d65 rgb = matrix_mul m_linear_p3_to_xyz_d65 rgb
let xyz_d65_to_linear_display_p3 xyz = matrix_mul m_xyz_d65_to_linear_p3 xyz

(* A98-RGB ↔ XYZ-D65. CSS Color 4 sec. 10.2: the non-linearity is a fixed
   [256/563] power, equivalent to a gamma of [1/(563/256)] = [1/2.1992...]. *)

let a98_rgb_gamma = 563.0 /. 256.0

let a98_rgb_to_linear v =
  let s = Float.copy_sign 1.0 v in
  s *. (Float.abs v ** a98_rgb_gamma)

let linear_to_a98_rgb v =
  let s = Float.copy_sign 1.0 v in
  s *. (Float.abs v ** (1.0 /. a98_rgb_gamma))

let m_linear_a98_rgb_to_xyz_d65 =
  ( (0.5766690429101305, 0.18555823790654632, 0.1882286462349947),
    (0.29734497525053605, 0.6273635662554661, 0.07529145849399788),
    (0.02703136138641234, 0.07068885253582723, 0.9913375368376388) )

let m_xyz_d65_to_linear_a98_rgb =
  ( (2.0415879038107465, -0.5650069742788596, -0.34473135077832957),
    (-0.9692436362808795, 1.8759675015077202, 0.04155505740717561),
    (0.013444280632031142, -0.11836239223101838, 1.0151749943912054) )

let linear_a98_rgb_to_xyz_d65 rgb = matrix_mul m_linear_a98_rgb_to_xyz_d65 rgb
let xyz_d65_to_linear_a98_rgb xyz = matrix_mul m_xyz_d65_to_linear_a98_rgb xyz

(* ProPhoto-RGB ↔ XYZ-D50. CSS Color 4 sec. 10.3: piecewise gamma with toe at
   [1/512] and a fixed 1.8 exponent above. *)

let prophoto_rgb_to_linear v =
  let s = Float.copy_sign 1.0 v in
  let av = Float.abs v in
  if av <= 16.0 /. 512.0 then s *. (av /. 16.0) else s *. (av ** 1.8)

let linear_to_prophoto_rgb v =
  let s = Float.copy_sign 1.0 v in
  let av = Float.abs v in
  if av >= 1.0 /. 512.0 then s *. (av ** (1.0 /. 1.8)) else s *. (16.0 *. av)

let m_linear_prophoto_to_xyz_d50 =
  ( (0.7977666449006423, 0.13518129740053308, 0.0313477341283042),
    (0.2880748288194013, 0.711835234241873, 0.00008993693872564),
    (0.0, 0.0, 0.8251046025104602) )

let m_xyz_d50_to_linear_prophoto =
  ( (1.3457868816471585, -0.2555720873797946, -0.05110186497554868),
    (-0.5446307051249019, 1.5082477428451437, 0.020533836549400985),
    (0.0, 0.0, 1.2119675456389452) )

let linear_prophoto_rgb_to_xyz_d50 rgb =
  matrix_mul m_linear_prophoto_to_xyz_d50 rgb

let xyz_d50_to_linear_prophoto_rgb xyz =
  matrix_mul m_xyz_d50_to_linear_prophoto xyz

(* Rec2020 ↔ XYZ-D65. CSS Color 4 sec. 10.5: piecewise gamma with a low-end
   linear segment and a 1/0.45 power for the upper segment. *)

let rec2020_alpha = 1.09929682680944
let rec2020_beta = 0.018053968510807

let rec2020_to_linear v =
  let s = Float.copy_sign 1.0 v in
  let av = Float.abs v in
  if av < rec2020_beta *. 4.5 then s *. (av /. 4.5)
  else s *. (((av +. rec2020_alpha -. 1.0) /. rec2020_alpha) ** (1.0 /. 0.45))

let linear_to_rec2020 v =
  let s = Float.copy_sign 1.0 v in
  let av = Float.abs v in
  if av < rec2020_beta then s *. (4.5 *. av)
  else s *. ((rec2020_alpha *. (av ** 0.45)) -. (rec2020_alpha -. 1.0))

let m_linear_rec2020_to_xyz_d65 =
  ( (0.6369580483012914, 0.14461690358620832, 0.1688809751641721),
    (0.2627002120112671, 0.6779980715188708, 0.05930171646986196),
    (0.0, 0.028072693049087428, 1.060985057710791) )

let m_xyz_d65_to_linear_rec2020 =
  ( (1.7166511879712674, -0.35567078377639233, -0.25336628137365974),
    (-0.6666843518324892, 1.6164812366349395, 0.01576854581391113),
    (0.017639857445310783, -0.042770613257808524, 0.9421031212354738) )

let linear_rec2020_to_xyz_d65 rgb = matrix_mul m_linear_rec2020_to_xyz_d65 rgb
let xyz_d65_to_linear_rec2020 xyz = matrix_mul m_xyz_d65_to_linear_rec2020 xyz

(* XYZ chromatic adaptation, Bradford method. CSS Color 4 sec. 16.4. *)

let m_xyz_d65_to_d50 =
  ( (1.0479298208405488, 0.022946793341019088, -0.05019222954313557),
    (0.029627815688159, 0.990434484573249, -0.01707382502938514),
    (-0.009243058152591178, 0.015055144896577895, 0.7518742899580008) )

let m_xyz_d50_to_d65 =
  ( (0.9554734527042182, -0.023098536874261423, 0.0632593086610217),
    (-0.028369706963208136, 1.0099954580058226, 0.021041398966943008),
    (0.012314001688319899, -0.020507696433477912, 1.3303659366080753) )

let xyz_d65_to_d50 xyz = matrix_mul m_xyz_d65_to_d50 xyz
let xyz_d50_to_d65 xyz = matrix_mul m_xyz_d50_to_d65 xyz

(* CIE Lab ↔ XYZ-D50. CSS Color 4 sec. 8.2 with D50 white point. *)

let lab_kappa = 24389.0 /. 27.0
let lab_epsilon = 216.0 /. 24389.0
let lab_d50_white = (0.96422, 1.0, 0.82521)

let f_lab v =
  if v > lab_epsilon then v ** (1.0 /. 3.0)
  else ((lab_kappa *. v) +. 16.0) /. 116.0

let f_lab_inv v =
  let v3 = v ** 3.0 in
  if v3 > lab_epsilon then v3 else ((116.0 *. v) -. 16.0) /. lab_kappa

let xyz_d50_to_lab (x, y, z) =
  let wx, wy, wz = lab_d50_white in
  let fx = f_lab (x /. wx) in
  let fy = f_lab (y /. wy) in
  let fz = f_lab (z /. wz) in
  ((116.0 *. fy) -. 16.0, 500.0 *. (fx -. fy), 200.0 *. (fy -. fz))

let lab_to_xyz_d50 (l, a, b) =
  let fy = (l +. 16.0) /. 116.0 in
  let fx = (a /. 500.0) +. fy in
  let fz = fy -. (b /. 200.0) in
  let wx, wy, wz = lab_d50_white in
  (wx *. f_lab_inv fx, wy *. f_lab_inv fy, wz *. f_lab_inv fz)

(* OKLab ↔ linear sRGB. CSS Color 4 sec. 8.3: a two-stage matrix with a
   cube-root non-linearity sandwiched between the two stages. *)

let m_linear_srgb_to_lms =
  ( (0.4122214708, 0.5363325363, 0.0514459929),
    (0.2119034982, 0.6806995451, 0.1073969566),
    (0.0883024619, 0.2817188376, 0.6299787005) )

let m_lms_to_oklab =
  ( (0.2104542553, 0.793617785, -0.0040720468),
    (1.9779984951, -2.428592205, 0.4505937099),
    (0.0259040371, 0.7827717662, -0.808675766) )

let m_oklab_to_lms =
  ( (1.0, 0.3963377774, 0.2158037573),
    (1.0, -0.1055613458, -0.0638541728),
    (1.0, -0.0894841775, -1.291485548) )

let m_lms_to_linear_srgb =
  ( (4.0767416621, -3.3077115913, 0.2309699292),
    (-1.2684380046, 2.6097574011, -0.3413193965),
    (-0.0041960863, -0.7034186147, 1.707614701) )

let cbrt v =
  let s = Float.copy_sign 1.0 v in
  s *. (Float.abs v ** (1.0 /. 3.0))

let linear_srgb_to_oklab rgb =
  let l, m, s = matrix_mul m_linear_srgb_to_lms rgb in
  matrix_mul m_lms_to_oklab (cbrt l, cbrt m, cbrt s)

let oklab_to_linear_srgb lab =
  let l, m, s = matrix_mul m_oklab_to_lms lab in
  matrix_mul m_lms_to_linear_srgb (l *. l *. l, m *. m *. m, s *. s *. s)

(* Lab / LCH polar form. CSS Color 4 sec. 8.4 (CIE Lab) and sec. 8.6 (OKLab)
   share the same polar transform. *)

let lab_to_lch (l, a, b) =
  let c = Float.sqrt ((a *. a) +. (b *. b)) in
  let h = Float.atan2 b a *. 180.0 /. Float.pi in
  let h = if h < 0.0 then h +. 360.0 else h in
  (l, c, h)

let lch_to_lab (l, c, h) =
  let h_rad = h *. Float.pi /. 180.0 in
  (l, c *. Float.cos h_rad, c *. Float.sin h_rad)

let oklab_to_oklch = lab_to_lch
let oklch_to_oklab = lch_to_lab

(* CSS Color 4 perceptual difference: Euclidean distance in OKLab. *)
let oklab_distance (l1, a1, b1) (l2, a2, b2) =
  let dl = l1 -. l2 and da = a1 -. a2 and db = b1 -. b2 in
  Float.sqrt ((dl *. dl) +. (da *. da) +. (db *. db))

(* Fold a colour given as linear sRGB to its nearest 8-bit sRGB byte triple,
   when that is sound. Returns [None] when the colour is out of the sRGB gamut,
   or when the 8-bit quantisation drifts past [budget] (default the documented
   0.001) in OKLab distance - a wide-gamut / high-precision source whose nearest
   byte triple is perceptibly off keeps its functional spelling. Alpha is not
   considered here: 8-bit alpha is the canonical hex form and is handled by the
   caller. *)
let fold_linear_srgb_to_bytes ?(budget = 0.001) (linear : rgb) :
    (int * int * int) option =
  let r, g, b = linear_rgb_to_rgb linear in
  let in_gamut v = v >= -1e-3 && v <= 1.0 +. 1e-3 in
  if not (in_gamut r && in_gamut g && in_gamut b) then None
  else
    let clamp01 v = Float.max 0.0 (Float.min 1.0 v) in
    let to_byte v = Float.to_int (Float.round (clamp01 v *. 255.0)) in
    let rb = to_byte r and gb = to_byte g and bb = to_byte b in
    let byte_srgb x = Float.of_int x /. 255.0 in
    let folded = rgb_to_linear_rgb (byte_srgb rb, byte_srgb gb, byte_srgb bb) in
    if
      oklab_distance (linear_srgb_to_oklab linear) (linear_srgb_to_oklab folded)
      <= budget
    then Some (rb, gb, bb)
    else None

(* Hue interpolation. CSS Color 4 sec. 12.4. *)

type hue_interpolation = Shorter | Longer | Increasing | Decreasing

let normalise_hue h =
  let h = Float.rem h 360.0 in
  if h < 0.0 then h +. 360.0 else h

let interpolate_hue method_ h1 h2 t =
  let h1 = normalise_hue h1 in
  let h2 = normalise_hue h2 in
  let diff = h2 -. h1 in
  let diff =
    match method_ with
    | Shorter ->
        if diff > 180.0 then diff -. 360.0
        else if diff < -180.0 then diff +. 360.0
        else diff
    | Longer ->
        if diff > 0.0 && diff < 180.0 then diff -. 360.0
        else if diff < 0.0 && diff > -180.0 then diff +. 360.0
        else diff
    | Increasing -> if diff < 0.0 then diff +. 360.0 else diff
    | Decreasing -> if diff > 0.0 then diff -. 360.0 else diff
  in
  normalise_hue (h1 +. (t *. diff))
