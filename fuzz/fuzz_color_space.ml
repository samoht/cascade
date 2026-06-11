(** Fuzz tests for [Color_space]: roundtrip identities and bounded-output
    invariants under arbitrary input. *)

open Cascade
open Alcobar

let float_of_buf buf i =
  if String.length buf = 0 then 0.0
  else
    let b = Char.code buf.[i mod String.length buf] in
    Float.of_int b /. 255.0

let triple_of_buf buf =
  (float_of_buf buf 0, float_of_buf buf 1, float_of_buf buf 2)

let approx_eq a b = Float.abs (a -. b) < 1e-3

let approx_eq_triple (a1, b1, c1) (a2, b2, c2) =
  approx_eq a1 a2 && approx_eq b1 b2 && approx_eq c1 c2

let pp_triple ppf (a, b, c) = Fmt.pf ppf "(%.5f %.5f %.5f)" a b c

(* sRGB <-> linear sRGB is a continuous monotone bijection on [0, 1]; any input
   in that interval must roundtrip to itself within float epsilon. *)
let test_srgb_roundtrip buf =
  let v = float_of_buf buf 0 in
  let back = Color_space.srgb_of_linear (Color_space.linear_of_srgb v) in
  if not (approx_eq v back) then failf "sRGB roundtrip %f -> %f" v back

let test_rgb_oklab_roundtrip buf =
  let rgb = triple_of_buf buf in
  let linear = Color_space.linear_rgb_of_rgb rgb in
  let back =
    linear |> Color_space.oklab_of_linear_srgb
    |> Color_space.linear_srgb_of_oklab
  in
  if not (approx_eq_triple linear back) then
    failf "linear sRGB <-> OKLab roundtrip %a -> %a" pp_triple linear pp_triple
      back

let test_lab_lch_roundtrip buf =
  let l = float_of_buf buf 0 *. 100.0 in
  let a = (float_of_buf buf 1 *. 256.0) -. 128.0 in
  let b = (float_of_buf buf 2 *. 256.0) -. 128.0 in
  let back = (l, a, b) |> Color_space.lch_of_lab |> Color_space.lab_of_lch in
  if not (approx_eq_triple (l, a, b) back) then
    failf "Lab <-> LCH roundtrip %a -> %a" pp_triple (l, a, b) pp_triple back

let test_xyz_bradford_roundtrip buf =
  let xyz = triple_of_buf buf in
  let back = Color_space.d65_of_xyz50 (Color_space.d50_of_xyz65 xyz) in
  if not (approx_eq_triple xyz back) then
    failf "XYZ D65 -> D50 -> D65 roundtrip %a -> %a" pp_triple xyz pp_triple
      back

let test_display_p3_roundtrip buf =
  let rgb = triple_of_buf buf in
  let linear = Color_space.linear_rgb_of_rgb rgb in
  let back =
    linear |> Color_space.xyz65_of_linear_p3 |> Color_space.linear_p3_of_xyz65
  in
  if not (approx_eq_triple linear back) then
    failf "Display-P3 <-> XYZ roundtrip %a -> %a" pp_triple linear pp_triple
      back

(* Hue interpolation must always land in [0, 360) regardless of inputs and
   regardless of interpolation method. *)
let test_hue_interpolation_in_range buf =
  let h1 = (float_of_buf buf 0 *. 720.0) -. 360.0 in
  let h2 = (float_of_buf buf 1 *. 720.0) -. 360.0 in
  let t = float_of_buf buf 2 in
  let method_ =
    match
      Char.code (if String.length buf > 3 then buf.[3] else '\000') mod 4
    with
    | 0 -> Color_space.Shorter
    | 1 -> Color_space.Longer
    | 2 -> Color_space.Increasing
    | _ -> Color_space.Decreasing
  in
  let h = Color_space.interpolate_hue method_ h1 h2 t in
  if h < 0.0 || h >= 360.0 then
    failf "hue interpolation out of range: %f (inputs %f -> %f t=%f)" h h1 h2 t

let suite =
  ( "color_space",
    [
      test_case "sRGB <-> linear roundtrip" [ bytes ] test_srgb_roundtrip;
      test_case "linear sRGB <-> OKLab roundtrip" [ bytes ]
        test_rgb_oklab_roundtrip;
      test_case "Lab <-> LCH roundtrip" [ bytes ] test_lab_lch_roundtrip;
      test_case "XYZ D65/D50 Bradford roundtrip" [ bytes ]
        test_xyz_bradford_roundtrip;
      test_case "Display-P3 <-> XYZ roundtrip" [ bytes ]
        test_display_p3_roundtrip;
      test_case "hue interpolation in [0, 360)" [ bytes ]
        test_hue_interpolation_in_range;
    ] )
