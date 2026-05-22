(** Unit tests for [Color_space]: known CSS Color 4 reference values plus
    roundtrip identities. *)

open Cascade

let eps = 1e-3
let tight_eps = 1e-12

let triplet =
  Alcotest.testable
    (fun ppf (a, b, c) -> Fmt.pf ppf "(%.4f %.4f %.4f)" a b c)
    (fun (a1, b1, c1) (a2, b2, c2) ->
      Float.abs (a1 -. a2) < eps
      && Float.abs (b1 -. b2) < eps
      && Float.abs (c1 -. c2) < eps)

let approx_float =
  Alcotest.testable Fmt.float (fun a b -> Float.abs (a -. b) < eps)

let approx_float_tight =
  Alcotest.testable Fmt.float (fun a b -> Float.abs (a -. b) < tight_eps)

let srgb_red = (1.0, 0.0, 0.0)
let srgb_blue = (0.0, 0.0, 1.0)
let srgb_green = (0.0, 1.0, 0.0)
let srgb_white = (1.0, 1.0, 1.0)
let srgb_black = (0.0, 0.0, 0.0)
let srgb_grey_50 = (0.5, 0.5, 0.5)

(* CSS Color 4 sec. 11.5.1 reference values for sRGB / linear sRGB. *)
let test_srgb_linearise () =
  Alcotest.(check approx_float) "0 -> 0" 0.0 (Color_space.srgb_to_linear 0.0);
  Alcotest.(check approx_float) "1 -> 1" 1.0 (Color_space.srgb_to_linear 1.0);
  Alcotest.(check approx_float)
    "0.5 (mid-grey) is ~0.2140 in linear" 0.21404
    (Color_space.srgb_to_linear 0.5);
  Alcotest.(check approx_float)
    "below toe scales by 1/12.92" (0.03 /. 12.92)
    (Color_space.srgb_to_linear 0.03)

let test_srgb_roundtrip () =
  let roundtrip v = Color_space.linear_to_srgb (Color_space.srgb_to_linear v) in
  Alcotest.(check approx_float) "0" 0.0 (roundtrip 0.0);
  Alcotest.(check approx_float) "0.04" 0.04 (roundtrip 0.04);
  Alcotest.(check approx_float) "0.5" 0.5 (roundtrip 0.5);
  Alcotest.(check approx_float) "1" 1.0 (roundtrip 1.0)

(* CSS Color 4 sec. 9.1: D65 white in XYZ should land at [(0.9505, 1.0, 1.0890)]
   (CIE 1931 D65 with Y = 1). *)
let test_linear_srgb_to_xyz_d65 () =
  let xyz_white = Color_space.linear_srgb_to_xyz_d65 (1.0, 1.0, 1.0) in
  Alcotest.(check triplet)
    "linear-srgb white -> XYZ-D65" (0.95047, 1.0, 1.08883) xyz_white;
  let xyz_red = Color_space.linear_srgb_to_xyz_d65 (1.0, 0.0, 0.0) in
  Alcotest.(check triplet)
    "linear-srgb red -> XYZ-D65" (0.4124, 0.2126, 0.0193) xyz_red

let test_linear_srgb_to_oklab_known () =
  (* Reference values from the OKLab paper (Ottosson) and confirmed by web.dev /
     WebKit fixtures: linear-sRGB red -> OKLab ≈ (0.628 0.226 0.126); blue ->
     (0.452 -0.032 -0.312); white -> (1.0 0 0). *)
  let red = Color_space.linear_srgb_to_oklab (1.0, 0.0, 0.0) in
  Alcotest.(check triplet)
    "linear-srgb red -> OKLab"
    (0.6279554, 0.2248631, 0.1258462)
    red;
  let blue = Color_space.linear_srgb_to_oklab (0.0, 0.0, 1.0) in
  Alcotest.(check triplet)
    "linear-srgb blue -> OKLab"
    (0.4520137, -0.0324569, -0.3115459)
    blue;
  let white = Color_space.linear_srgb_to_oklab (1.0, 1.0, 1.0) in
  Alcotest.(check triplet)
    "linear-srgb white -> OKLab (L=1, a=b=0)" (1.0, 0.0, 0.0) white

let test_oklab_roundtrip () =
  let roundtrip rgb =
    rgb |> Color_space.linear_srgb_to_oklab |> Color_space.oklab_to_linear_srgb
  in
  Alcotest.(check triplet) "red roundtrip" srgb_red (roundtrip srgb_red);
  Alcotest.(check triplet) "blue roundtrip" srgb_blue (roundtrip srgb_blue);
  Alcotest.(check triplet) "green roundtrip" srgb_green (roundtrip srgb_green);
  Alcotest.(check triplet) "white roundtrip" srgb_white (roundtrip srgb_white)

let test_oklab_distance_formula () =
  (* CSS Color 4 defines OKLab as rectangular [L a b] coordinates; Cascade's
     approximation budget uses ordinary Euclidean distance in that space. *)
  let distance = Color_space.oklab_distance in
  Alcotest.(check approx_float_tight)
    "zero distance" 0.0
    (distance (0.5, 0.1, -0.05) (0.5, 0.1, -0.05));
  Alcotest.(check approx_float_tight)
    "single-axis L delta" 0.001
    (distance (0.5, 0.0, 0.0) (0.501, 0.0, 0.0));
  Alcotest.(check approx_float_tight)
    "single-axis a delta" 0.001
    (distance (0.5, 0.0, 0.0) (0.5, 0.001, 0.0));
  Alcotest.(check approx_float_tight)
    "3-4-12 millistep triangle" 0.013
    (distance (0.0, 0.0, 0.0) (0.003, 0.004, 0.012));
  Alcotest.(check approx_float_tight)
    "symmetric"
    (distance (0.2, -0.1, 0.3) (0.7, 0.4, -0.2))
    (distance (0.7, 0.4, -0.2) (0.2, -0.1, 0.3))

let test_oklab_distance_budget_edges () =
  let distance = Color_space.oklab_distance in
  Alcotest.(check approx_float_tight)
    "nearest 3-decimal rounding in all channels stays under 0.001"
    0.000866025403784
    (distance (0.5, 0.1, -0.05) (0.5005, 0.1005, -0.0495));
  Alcotest.(check bool)
    "nearest 3-decimal rounding is inside budget" true
    (distance (0.5, 0.1, -0.05) (0.5005, 0.1005, -0.0495) < 0.001);
  Alcotest.(check bool)
    "two millisteps on one channel exceed budget" true
    (distance (0.5, 0.1, -0.05) (0.502, 0.1, -0.05) > 0.001)

let test_oklab_distance_reference_colours () =
  (* OKLab reference coordinates for linear-sRGB primaries, generated from the
     CSS Color 4 OKLab matrices. These tests pin [oklab_distance] on realistic
     colour coordinates, independently of the conversion functions above. *)
  let red = (0.6279553606145516, 0.224863061065974, 0.1258462985307351) in
  let green = (0.8664396115356694, -0.2338875741879082, 0.1794984798967299) in
  let blue = (0.4520137183853429, -0.032456984168764, -0.3115281476783751) in
  let white = (0.9999999934735462, 0.0000000000809529, 0.0000000372739076) in
  let black = (0.0, 0.0, 0.0) in
  Alcotest.(check approx_float_tight)
    "red-blue OKLab distance" 0.53708981869576
    (Color_space.oklab_distance red blue);
  Alcotest.(check approx_float_tight)
    "red-green OKLab distance" 0.519812889267452
    (Color_space.oklab_distance red green);
  Alcotest.(check approx_float_tight)
    "green-blue OKLab distance" 0.673372298581318
    (Color_space.oklab_distance green blue);
  Alcotest.(check approx_float_tight)
    "black-white OKLab distance" 0.999999993473547
    (Color_space.oklab_distance black white)

let test_xyz_d65_d50_roundtrip () =
  let roundtrip xyz =
    Color_space.xyz_d50_to_d65 (Color_space.xyz_d65_to_d50 xyz)
  in
  let xyz_d65 = (0.41246, 0.21267, 0.01933) in
  Alcotest.(check triplet) "D65 red roundtrip" xyz_d65 (roundtrip xyz_d65);
  Alcotest.(check triplet)
    "D65 white roundtrip" (0.95047, 1.0, 1.08883)
    (roundtrip (0.95047, 1.0, 1.08883))

let test_lab_roundtrip () =
  let roundtrip lab =
    Color_space.xyz_d50_to_lab (Color_space.lab_to_xyz_d50 lab)
  in
  Alcotest.(check triplet)
    "(50 20 -30)" (50.0, 20.0, -30.0)
    (roundtrip (50.0, 20.0, -30.0));
  Alcotest.(check triplet)
    "(80 0 0)" (80.0, 0.0, 0.0)
    (roundtrip (80.0, 0.0, 0.0));
  Alcotest.(check triplet) "L=0" (0.0, 0.0, 0.0) (roundtrip (0.0, 0.0, 0.0));
  Alcotest.(check triplet)
    "L=100" (100.0, 0.0, 0.0)
    (roundtrip (100.0, 0.0, 0.0))

let test_lab_lch_roundtrip () =
  let lch1 = Color_space.lab_to_lch (50.0, 20.0, -30.0) in
  let back = Color_space.lch_to_lab lch1 in
  Alcotest.(check triplet) "(50 20 -30) lab<->lch" (50.0, 20.0, -30.0) back;
  let lch2 = Color_space.lab_to_lch (60.0, 0.0, 0.0) in
  Alcotest.(check approx_float)
    "C=0 when a=b=0" 0.0
    (let _, c, _ = lch2 in
     c)

let test_display_p3_to_xyz () =
  (* Display-P3 with sRGB-identical gamma. linear (1, 1, 1) sums to a D65 white
     near (0.9505, 1.0, 1.0890) as well, since all wide-gamut RGB spaces share
     that whitepoint. *)
  let white = Color_space.linear_display_p3_to_xyz_d65 (1.0, 1.0, 1.0) in
  Alcotest.(check triplet)
    "linear-P3 white -> XYZ-D65" (0.95047, 1.0, 1.08883) white

let test_display_p3_grey_to_srgb () =
  (* Display-P3 50% grey (0.5, 0.5, 0.5) lands at a 50% grey in sRGB too: both
     spaces share the same whitepoint and gamma curve. *)
  let linear_p3 = Color_space.rgb_to_linear_rgb (0.5, 0.5, 0.5) in
  let xyz = Color_space.linear_display_p3_to_xyz_d65 linear_p3 in
  let linear_srgb = Color_space.xyz_d65_to_linear_srgb xyz in
  let srgb = Color_space.linear_rgb_to_rgb linear_srgb in
  Alcotest.(check triplet)
    "Display-P3 0.5 grey -> sRGB grey" (0.5, 0.5, 0.5) srgb

let test_fold_linear_srgb_to_bytes () =
  let fold = Color_space.fold_linear_srgb_to_bytes in
  let of_oklch (l, c, h) =
    Color_space.oklab_to_linear_srgb (Color_space.oklch_to_oklab (l, c, h))
  in
  let of_lab (l, a, b) =
    Color_space.xyz_d65_to_linear_srgb
      (Color_space.xyz_d50_to_d65 (Color_space.lab_to_xyz_d50 (l, a, b)))
  in
  Alcotest.(check bool)
    "in-gamut sRGB red folds to its bytes" true
    (fold (Color_space.rgb_to_linear_rgb (1.0, 0.0, 0.0)) = Some (255, 0, 0));
  Alcotest.(check bool)
    "oklch(50% .2 30) is within budget and folds" true
    (fold (of_oklch (0.5, 0.2, 30.0)) <> None);
  Alcotest.(check bool)
    "oklch(50% .1 20) is past budget and is preserved" true
    (fold (of_oklch (0.5, 0.1, 20.0)) = None);
  Alcotest.(check bool)
    "lab(50% 20 30) is past budget and is preserved" true
    (fold (of_lab (50.0, 20.0, 30.0)) = None);
  Alcotest.(check bool)
    "out-of-sRGB-gamut colour is preserved" true
    (fold (2.0, 0.0, 0.0) = None)

let test_hue_interpolation () =
  let hue m h1 h2 t = Color_space.interpolate_hue m h1 h2 t in
  Alcotest.(check approx_float)
    "shorter 350 -> 10 @ 0.5" 0.0
    (hue Shorter 350.0 10.0 0.5);
  Alcotest.(check approx_float)
    "longer 350 -> 10 @ 0.5" 180.0
    (hue Longer 350.0 10.0 0.5);
  Alcotest.(check approx_float)
    "increasing 350 -> 10 @ 0.5" 0.0
    (hue Increasing 350.0 10.0 0.5);
  Alcotest.(check approx_float)
    "decreasing 10 -> 350 @ 0.5" 0.0
    (hue Decreasing 10.0 350.0 0.5);
  Alcotest.(check approx_float)
    "shorter 0 -> 90 @ 0" 0.0 (hue Shorter 0.0 90.0 0.0);
  Alcotest.(check approx_float)
    "shorter 0 -> 90 @ 1" 90.0 (hue Shorter 0.0 90.0 1.0)

let suite =
  ( "color_space",
    [
      Alcotest.test_case "sRGB linearise reference values" `Quick
        test_srgb_linearise;
      Alcotest.test_case "sRGB linear roundtrip" `Quick test_srgb_roundtrip;
      Alcotest.test_case "linear sRGB to XYZ-D65" `Quick
        test_linear_srgb_to_xyz_d65;
      Alcotest.test_case "linear sRGB to OKLab reference values" `Quick
        test_linear_srgb_to_oklab_known;
      Alcotest.test_case "OKLab roundtrip" `Quick test_oklab_roundtrip;
      Alcotest.test_case "OKLab distance formula" `Quick
        test_oklab_distance_formula;
      Alcotest.test_case "OKLab distance budget edges" `Quick
        test_oklab_distance_budget_edges;
      Alcotest.test_case "OKLab distance reference colours" `Quick
        test_oklab_distance_reference_colours;
      Alcotest.test_case "XYZ D65/D50 Bradford roundtrip" `Quick
        test_xyz_d65_d50_roundtrip;
      Alcotest.test_case "Lab roundtrip" `Quick test_lab_roundtrip;
      Alcotest.test_case "Lab/LCH roundtrip" `Quick test_lab_lch_roundtrip;
      Alcotest.test_case "Display-P3 white -> XYZ-D65" `Quick
        test_display_p3_to_xyz;
      Alcotest.test_case "Display-P3 grey -> sRGB grey" `Quick
        test_display_p3_grey_to_srgb;
      Alcotest.test_case "fold linear sRGB to bytes within budget" `Quick
        test_fold_linear_srgb_to_bytes;
      Alcotest.test_case "Hue interpolation methods" `Quick
        test_hue_interpolation;
    ] )
