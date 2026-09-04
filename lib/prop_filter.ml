(* Filter Effects 1 and Compositing and Blending 1: the filter functions and the
   [filter] / [backdrop-filter] value grammar, blend modes and [isolation].

   [drop-shadow()] reuses the [Shadow] machinery that [box-shadow] owns, so this
   module comes after Prop_background in the Properties include chain.

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name here stays visible under [Css.Properties] exactly as when
   it lived in properties.ml. *)

open Values
open Properties_intf
open Prop_common
open Prop_background

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

let read_blur t : filter =
  Cursor.call "blur" t (fun t ->
      if Cursor.is_done t then Omitted Blur_function
      else
        Blur
          (read_length ~length_only:true ~with_keywords:false
             ~allow_negative:false t))

let filter_function_name = function
  | Blur_function -> "blur"
  | Brightness_function -> "brightness"
  | Contrast_function -> "contrast"
  | Grayscale_function -> "grayscale"
  | Hue_rotate_function -> "hue-rotate"
  | Invert_function -> "invert"
  | Opacity_function -> "opacity"
  | Saturate_function -> "saturate"
  | Sepia_function -> "sepia"

let pp_filter_function ctx fn = Pp.string ctx (filter_function_name fn)

let read_filter_function t =
  Cursor.enum "optional filter function"
    (List.map
       (fun fn -> (filter_function_name fn, fn))
       [
         Blur_function;
         Brightness_function;
         Contrast_function;
         Grayscale_function;
         Hue_rotate_function;
         Invert_function;
         Opacity_function;
         Saturate_function;
         Sepia_function;
       ])
    t

let pp_blur_length ctx (length : length) =
  match length with
  | Calc (Val value) -> (
      match Values.calc_length_unit value with
      | Some (_, n) when n < 0. ->
          (* The calculation clamps to zero; a negative literal is invalid. *)
          Pp.call "calc" pp_length { ctx with in_calc = true } value
      | _ -> pp_length ctx length)
  | _ -> pp_length ctx length

let rec pp_filter : filter Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Omitted fn -> Pp.call (filter_function_name fn) Pp.nop ctx ()
  | Blur l -> Pp.call "blur" pp_blur_length ctx l
  | Brightness n ->
      Pp.call "brightness" (pp_number_percentage ~always:true) ctx n
  | Contrast n -> Pp.call "contrast" (pp_number_percentage ~always:true) ctx n
  | Drop_shadow s -> Pp.call "drop-shadow" pp_shadow ctx s
  | Grayscale n -> Pp.call "grayscale" (pp_number_percentage ~always:true) ctx n
  | Hue_rotate (Deg 0.) when Pp.minified ctx -> Pp.string ctx "hue-rotate()"
  | Hue_rotate a -> Pp.call "hue-rotate" pp_angle ctx a
  | Invert n -> Pp.call "invert" (pp_number_percentage ~always:true) ctx n
  | Opacity n -> Pp.call "opacity" (pp_number_percentage ~always:true) ctx n
  | Saturate n -> Pp.call "saturate" (pp_number_percentage ~always:true) ctx n
  | Sepia n -> Pp.call "sepia" (pp_number_percentage ~always:true) ctx n
  | Url url -> Pp.url ctx url
  | List filters ->
      let sep = if Pp.minified ctx then Pp.nop else Pp.space in
      Pp.list ~sep pp_filter ctx filters
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_filter ctx v

let rec normalize_filter ?(lossless = false) : filter -> filter =
 fun value ->
  let amount fn make n =
    match Values.normalize_number_percentage n with
    | Num 1. | Pct 100. -> Omitted fn
    | n -> preserve_if_equal value (make n)
  in
  match value with
  | Blur l ->
      let omitted =
        match l with Pct _ -> false | l -> Values.length_is_zero l
      in
      if omitted then Omitted Blur_function else value
  | Drop_shadow s ->
      preserve_if_equal value (Drop_shadow (normalize_shadow ~lossless s))
  | Hue_rotate a ->
      let a = Values.normalize_angle a in
      if a = Deg 0. then Omitted Hue_rotate_function
      else preserve_if_equal value (Hue_rotate a)
  | Brightness x -> amount Brightness_function (fun n -> Brightness n) x
  | Contrast x -> amount Contrast_function (fun n -> Contrast n) x
  | Grayscale x -> amount Grayscale_function (fun n -> Grayscale n) x
  | Invert x -> amount Invert_function (fun n -> Invert n) x
  | Opacity x -> amount Opacity_function (fun n -> Opacity n) x
  | Saturate x -> amount Saturate_function (fun n -> Saturate n) x
  | Sepia x -> amount Sepia_function (fun n -> Sepia n) x
  | List filters ->
      preserve_if_equal value
        (List (map_preserve (normalize_filter ~lossless) filters))
  | other -> other

module Filter = struct
  let read_amount fn make t : filter =
    Cursor.call (filter_function_name fn) t (fun t ->
        if Cursor.is_done t then Omitted fn
        else make (Values.read_number_percentage t))

  let read_brightness t : filter =
    read_amount Brightness_function (fun n -> Brightness n) t

  let read_contrast t : filter =
    read_amount Contrast_function (fun n -> Contrast n) t

  let read_grayscale t : filter =
    read_amount Grayscale_function (fun n -> Grayscale n) t

  let read_hue_rotate t : filter =
    Cursor.call "hue-rotate" t (fun t ->
        if Cursor.is_done t then Omitted Hue_rotate_function
        else Hue_rotate (read_angle t))

  let read_invert t : filter = read_amount Invert_function (fun n -> Invert n) t

  let read_opacity t : filter =
    read_amount Opacity_function (fun n -> Opacity n) t

  let read_saturate t : filter =
    read_amount Saturate_function (fun n -> Saturate n) t

  let read_sepia t : filter = read_amount Sepia_function (fun n -> Sepia n) t

  let read_drop_shadow t : filter =
    Cursor.call "drop-shadow" t (fun t ->
        let read_var t : filter = Drop_shadow (Var (read_var Shadow.read t)) in
        let read_shadow t : filter = Drop_shadow (Shadow.read t) in
        Cursor.one_of [ read_var; read_shadow ] t)
end

let rec read_filter_item t : filter =
  let read_var t : filter = Var (read_var read_filter t) in
  (* [<filter-value-list>] mixes filter functions with a bare [<url>] reference
     to an SVG filter. [url(#id)] tokenises as a url-token, not a [url(]
     function, so it is read via [Cursor.url] (which handles both that and the
     quoted [url("#id")] form) and backtracks to the function dispatch. *)
  let read_url t = (Url (Cursor.url t) : filter) in
  Cursor.one_of
    [
      read_url;
      (fun t ->
        Cursor.enum_or_calls "filter"
          [ ("none", (None : filter)) ]
          ~calls:
            [
              ("blur", read_blur);
              ("brightness", Filter.read_brightness);
              ("contrast", Filter.read_contrast);
              ("grayscale", Filter.read_grayscale);
              ("hue-rotate", Filter.read_hue_rotate);
              ("invert", Filter.read_invert);
              ("opacity", Filter.read_opacity);
              ("saturate", Filter.read_saturate);
              ("sepia", Filter.read_sepia);
              ("drop-shadow", Filter.read_drop_shadow);
              ("var", read_var);
            ]
          t);
    ]
    t

and read_filter t : filter =
  let read_filter_list t =
    let filters, _ = Cursor.many read_filter_item t in
    match filters with
    | [] -> err_invalid_value t "filter" "expected filter function(s)"
    | [ f ] -> f
    | fs
      when List.exists
             (fun (value : filter) ->
               match value with None -> true | _ -> false)
             fs ->
        err_invalid_value t "filter" "none cannot be combined"
    | fs -> List fs
  in
  Cursor.enum "filter"
    [
      ("none", (None : filter));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~default:read_filter_list t
