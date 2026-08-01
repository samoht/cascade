(* Filter Effects 1 and Compositing and Blending 1: blend modes, [isolation] and
   the [blur()] filter function.

   The rest of the filter family cannot move yet. [pp_filter],
   [normalize_filter], [module Filter] and [read_filter_item]/[read_filter] all
   go through the [Shadow] machinery that [box-shadow] owns (drop-shadow()
   reuses it), and that machinery belongs to a later family. They stay in
   properties.ml until [Shadow] lands in Prop_common or Prop_filter is ordered
   after Prop_background.

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name here stays visible under [Css.Properties] exactly as when
   it lived in properties.ml. *)

open Values
open Properties_intf

let rec pp_blend_mode : blend_mode Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Multiply -> Pp.string ctx "multiply"
  | Screen -> Pp.string ctx "screen"
  | Overlay -> Pp.string ctx "overlay"
  | Darken -> Pp.string ctx "darken"
  | Lighten -> Pp.string ctx "lighten"
  | Color_dodge -> Pp.string ctx "color-dodge"
  | Color_burn -> Pp.string ctx "color-burn"
  | Hard_light -> Pp.string ctx "hard-light"
  | Soft_light -> Pp.string ctx "soft-light"
  | Difference -> Pp.string ctx "difference"
  | Exclusion -> Pp.string ctx "exclusion"
  | Hue -> Pp.string ctx "hue"
  | Saturation -> Pp.string ctx "saturation"
  | Color -> Pp.string ctx "color"
  | Luminosity -> Pp.string ctx "luminosity"
  | Plus_darker -> Pp.string ctx "plus-darker"
  | Plus_lighter -> Pp.string ctx "plus-lighter"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_blend_mode ctx v

let rec pp_isolation : isolation Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_isolation ctx v
  | Auto -> Pp.string ctx "auto"
  | Isolate -> Pp.string ctx "isolate"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec read_isolation t : isolation =
  Cursor.enum_or_var "isolation"
    [
      ("auto", (Auto : isolation));
      ("isolate", Isolate);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_isolation t))
    t

let rec read_blend_mode t : blend_mode =
  let read_var t : blend_mode = Var (read_var read_blend_mode t) in
  Cursor.enum_or_calls "blend-mode"
    [
      ("normal", (Normal : blend_mode));
      ("multiply", Multiply);
      ("screen", Screen);
      ("overlay", Overlay);
      ("darken", Darken);
      ("lighten", Lighten);
      ("color-dodge", Color_dodge);
      ("color-burn", Color_burn);
      ("hard-light", Hard_light);
      ("soft-light", Soft_light);
      ("difference", Difference);
      ("exclusion", Exclusion);
      ("hue", Hue);
      ("saturation", Saturation);
      ("color", Color);
      ("luminosity", Luminosity);
      ("plus-darker", Plus_darker);
      ("plus-lighter", Plus_lighter);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    t

let read_blur t : filter = Cursor.call "blur" t (fun t -> Blur (read_length t))
