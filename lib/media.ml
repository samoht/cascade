(** Structured media conditions for type-safe media query construction. *)

type t =
  | Min_width of float
  | Max_width of float
  | Not_min_width of float (* @media not all and (min-width: Xpx) *)
  | Min_width_rem of float (* @media (min-width: Xrem) *)
  | Not_min_width_rem of float (* @media not all and (min-width: Xrem) *)
  | Prefers_reduced_motion of [ `No_preference | `Reduce ]
  | Prefers_contrast of [ `More | `Less ]
  | Prefers_color_scheme of [ `Dark | `Light ]
  | Forced_colors of [ `Active | `None ]
  | Inverted_colors of [ `Inverted | `None ]
  | Pointer of [ `None | `Coarse | `Fine ]
  | Any_pointer of [ `None | `Coarse | `Fine ]
  | Scripting of [ `None | `Initial_only | `Enabled ]
  | Hover
  | Print
  | Orientation of [ `Portrait | `Landscape ]
  | Raw of string

let format_px px =
  if Float.is_integer px then Int.to_string (Float.to_int px)
  else Float.to_string px

let format_rem rem =
  if Float.is_integer rem then Int.to_string (Float.to_int rem)
  else Float.to_string rem

let to_string = function
  | Min_width px -> "(min-width: " ^ format_px px ^ "px)"
  | Max_width px -> "(max-width: " ^ format_px px ^ "px)"
  | Not_min_width px -> "not all and (min-width: " ^ format_px px ^ "px)"
  | Min_width_rem rem -> "(min-width:" ^ format_rem rem ^ "rem)"
  | Not_min_width_rem rem -> "not all and (min-width:" ^ format_rem rem ^ "rem)"
  | Prefers_reduced_motion `No_preference ->
      "(prefers-reduced-motion: no-preference)"
  | Prefers_reduced_motion `Reduce -> "(prefers-reduced-motion: reduce)"
  | Prefers_contrast `More -> "(prefers-contrast: more)"
  | Prefers_contrast `Less -> "(prefers-contrast: less)"
  | Prefers_color_scheme `Dark -> "(prefers-color-scheme: dark)"
  | Prefers_color_scheme `Light -> "(prefers-color-scheme: light)"
  | Forced_colors `Active -> "(forced-colors: active)"
  | Forced_colors `None -> "(forced-colors: none)"
  | Inverted_colors `Inverted -> "(inverted-colors: inverted)"
  | Inverted_colors `None -> "(inverted-colors: none)"
  | Pointer `None -> "(pointer: none)"
  | Pointer `Coarse -> "(pointer: coarse)"
  | Pointer `Fine -> "(pointer: fine)"
  | Any_pointer `None -> "(any-pointer: none)"
  | Any_pointer `Coarse -> "(any-pointer: coarse)"
  | Any_pointer `Fine -> "(any-pointer: fine)"
  | Scripting `None -> "(scripting: none)"
  | Scripting `Initial_only -> "(scripting: initial-only)"
  | Scripting `Enabled -> "(scripting: enabled)"
  | Hover -> "(hover: hover)"
  | Print -> "print"
  | Orientation `Portrait -> "(orientation: portrait)"
  | Orientation `Landscape -> "(orientation: landscape)"
  | Raw s -> s

type kind =
  | Kind_hover
  | Kind_responsive of float
  | Kind_preference_accessibility
  | Kind_preference_appearance
  | Kind_other

let kind = function
  | Hover -> Kind_hover
  | Min_width px | Max_width px | Not_min_width px -> Kind_responsive px
  | Min_width_rem rem | Not_min_width_rem rem -> Kind_responsive (rem *. 16.)
  | Prefers_reduced_motion _ | Prefers_contrast _ | Forced_colors _
  | Inverted_colors _ | Pointer _ | Any_pointer _ | Scripting _ ->
      Kind_preference_accessibility
  | Prefers_color_scheme _ -> Kind_preference_appearance
  | Print | Orientation _ -> Kind_other
  | Raw _ -> Kind_other

(* For backward compatibility with string-based code *)
let contains s sub =
  let ls = String.length s and lsub = String.length sub in
  let rec loop i =
    if i > ls - lsub then false
    else if String.sub s i lsub = sub then true
    else loop (i + 1)
  in
  lsub <> 0 && loop 0

let kind_of_string cond =
  if String.length cond > 10 && String.sub cond 0 10 = "(min-width" then
    try
      let start = String.index cond ':' + 1 in
      let end_pos = String.index_from cond start 'r' in
      let value_str = String.sub cond start (end_pos - start) |> String.trim in
      Kind_responsive (float_of_string value_str)
    with Not_found | Failure _ | Invalid_argument _ -> Kind_responsive 0.
  else if String.length cond > 8 && String.sub cond 1 7 = "prefers" then
    if contains cond "color-scheme" then Kind_preference_appearance
    else Kind_preference_accessibility
  else if
    String.equal cond "(hover: hover)" || String.equal cond "(hover:hover)"
  then Kind_hover
  else Kind_other

let group_order = function
  | Kind_hover -> (0, 0.)
  | Kind_other -> (500, 0.)
  | Kind_preference_accessibility -> (1000, 0.)
  | Kind_responsive rem -> (2000, rem)
  | Kind_preference_appearance -> (3000, 0.)

let preference_order = function
  | Prefers_reduced_motion `No_preference -> 0
  | Prefers_reduced_motion `Reduce -> 1
  | Prefers_contrast `More -> 2
  | Prefers_contrast `Less -> 3
  | Prefers_color_scheme _ -> 4
  | Forced_colors _ -> 5
  | Inverted_colors _ -> 6
  | Pointer `None -> 7
  | Pointer `Coarse -> 8
  | Pointer `Fine -> 9
  | Any_pointer `None -> 10
  | Any_pointer `Coarse -> 11
  | Any_pointer `Fine -> 12
  | Scripting `None -> 13
  | Scripting `Initial_only -> 14
  | Scripting `Enabled -> 15
  | Raw s ->
      (* Fallback to string matching for Raw conditions *)
      if contains s "reduced-motion" && contains s "no-preference" then 0
      else if contains s "reduced-motion" && contains s "reduce" then 1
      else if contains s "contrast" && contains s "more" then 2
      else if contains s "contrast" && contains s "less" then 3
      else if contains s "color-scheme" then 4
      else 20
  | _ -> 20

let compare a b =
  let ka = kind a and kb = kind b in
  let ga, va = group_order ka and gb, vb = group_order kb in
  let group_cmp = Int.compare ga gb in
  if group_cmp <> 0 then group_cmp
  else
    let value_cmp = Float.compare va vb in
    if value_cmp <> 0 then value_cmp
    else
      (* Within same group, use preference_order for fine-grained sorting *)
      Int.compare (preference_order a) (preference_order b)

let equal a b = compare a b = 0
