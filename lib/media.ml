(** Structured media conditions for type-safe media query construction. *)

type t =
  | Min_width of float
  | Max_width of float
  | Not_min_width of float (* @media not all and (min-width: Xpx) *)
  | Min_width_rem of float (* @media (min-width: Xrem) *)
  | Not_min_width_rem of float (* @media not all and (min-width: Xrem) *)
  | Min_width_length of Values_intf.length
    (* [@media (min-width: <length>)] for arbitrary CSS lengths *)
  | Not_min_width_length of Values_intf.length
    (* [@media not all and (min-width: <length>)] *)
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
  | Negated of t

(** Format a float as a compact string: integer form when possible, otherwise
    the standard float representation. Used for media query values. *)
let format_float f =
  if Float.is_integer f then Int.to_string (Float.to_int f)
  else Float.to_string f

let format_px = format_float
let format_rem = format_float
let render_length l = Pp.to_string (Values.pp_length ~always:true) l

let rec to_string = function
  | Min_width px -> "(min-width: " ^ format_px px ^ "px)"
  | Max_width px -> "(max-width: " ^ format_px px ^ "px)"
  | Not_min_width px -> "not all and (min-width: " ^ format_px px ^ "px)"
  | Min_width_rem rem -> "(min-width: " ^ format_rem rem ^ "rem)"
  | Not_min_width_rem rem ->
      "not all and (min-width: " ^ format_rem rem ^ "rem)"
  | Min_width_length l -> "(min-width: " ^ render_length l ^ ")"
  | Not_min_width_length l -> "not all and (min-width: " ^ render_length l ^ ")"
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
  | Negated Print -> "not print"
  | Negated inner -> "not all and " ^ to_string inner

let pp_feature ctx name value =
  Pp.char ctx '(';
  Pp.string ctx name;
  Pp.char ctx ':';
  Pp.sp ctx ();
  Pp.string ctx value;
  Pp.char ctx ')'

let pp_length_value ctx l =
  Pp.char ctx '(';
  Pp.string ctx "min-width";
  Pp.char ctx ':';
  Pp.sp ctx ();
  Values.pp_length ~always:true ctx l;
  Pp.char ctx ')'

let rec pp ctx = function
  | Min_width px -> pp_feature ctx "min-width" (format_px px ^ "px")
  | Max_width px -> pp_feature ctx "max-width" (format_px px ^ "px")
  | Not_min_width px ->
      Pp.string ctx "not all and ";
      pp_feature ctx "min-width" (format_px px ^ "px")
  | Min_width_rem rem -> pp_feature ctx "min-width" (format_rem rem ^ "rem")
  | Not_min_width_rem rem ->
      Pp.string ctx "not all and ";
      pp_feature ctx "min-width" (format_rem rem ^ "rem")
  | Min_width_length l -> pp_length_value ctx l
  | Not_min_width_length l ->
      Pp.string ctx "not all and ";
      pp_length_value ctx l
  | Prefers_reduced_motion `No_preference ->
      pp_feature ctx "prefers-reduced-motion" "no-preference"
  | Prefers_reduced_motion `Reduce ->
      pp_feature ctx "prefers-reduced-motion" "reduce"
  | Prefers_contrast `More -> pp_feature ctx "prefers-contrast" "more"
  | Prefers_contrast `Less -> pp_feature ctx "prefers-contrast" "less"
  | Prefers_color_scheme `Dark -> pp_feature ctx "prefers-color-scheme" "dark"
  | Prefers_color_scheme `Light -> pp_feature ctx "prefers-color-scheme" "light"
  | Forced_colors `Active -> pp_feature ctx "forced-colors" "active"
  | Forced_colors `None -> pp_feature ctx "forced-colors" "none"
  | Inverted_colors `Inverted -> pp_feature ctx "inverted-colors" "inverted"
  | Inverted_colors `None -> pp_feature ctx "inverted-colors" "none"
  | Pointer `None -> pp_feature ctx "pointer" "none"
  | Pointer `Coarse -> pp_feature ctx "pointer" "coarse"
  | Pointer `Fine -> pp_feature ctx "pointer" "fine"
  | Any_pointer `None -> pp_feature ctx "any-pointer" "none"
  | Any_pointer `Coarse -> pp_feature ctx "any-pointer" "coarse"
  | Any_pointer `Fine -> pp_feature ctx "any-pointer" "fine"
  | Scripting `None -> pp_feature ctx "scripting" "none"
  | Scripting `Initial_only -> pp_feature ctx "scripting" "initial-only"
  | Scripting `Enabled -> pp_feature ctx "scripting" "enabled"
  | Hover -> pp_feature ctx "hover" "hover"
  | Print -> Pp.string ctx "print"
  | Orientation `Portrait -> pp_feature ctx "orientation" "portrait"
  | Orientation `Landscape -> pp_feature ctx "orientation" "landscape"
  | Raw s -> Pp.string ctx s
  | Negated Print -> Pp.string ctx "not print"
  | Negated inner ->
      Pp.string ctx "not all and ";
      pp ctx inner

type kind =
  | Kind_hover
  | Kind_responsive of int * float
      (** (unit_order, value) — unit_order: -2=calc, -1=em, 0=px, 1=rem, 2=vh *)
  | Kind_responsive_max of int * float
  | Kind_preference_accessibility
  | Kind_preference_appearance
  | Kind_other

(** Map a CSS length to (unit_order, numeric_value) for sorting. Unit order
    follows alphabetical unit names: calc, em, px, rem, vh. *)
let length_sort_key (l : Values_intf.length) =
  match l with
  | Calc _ -> (-2, 0.)
  | Em v -> (-1, v)
  | Px v -> (0, v)
  | Rem v -> (1, v)
  | Vh v -> (2, v)
  | Vw v -> (3, v)
  | Cm v -> (4, v)
  | Mm v -> (5, v)
  | In v -> (6, v)
  | Pt v -> (7, v)
  | _ -> (100, 0.)

let rec kind = function
  | Hover -> Kind_hover
  | Min_width px | Max_width px -> Kind_responsive (0, px)
  | Not_min_width px -> Kind_responsive_max (0, px)
  | Min_width_rem rem -> Kind_responsive (0, rem *. 16.)
  | Not_min_width_rem rem -> Kind_responsive_max (0, rem *. 16.)
  | Min_width_length l ->
      let u, v = length_sort_key l in
      Kind_responsive (u, v)
  | Not_min_width_length l ->
      let u, v = length_sort_key l in
      Kind_responsive_max (u, v)
  | Prefers_reduced_motion _ | Prefers_contrast _ | Forced_colors _
  | Inverted_colors _ | Pointer _ | Any_pointer _ | Scripting _ ->
      Kind_preference_accessibility
  | Prefers_color_scheme _ -> Kind_preference_appearance
  | Print | Orientation _ -> Kind_other
  | Raw _ -> Kind_other
  | Negated inner -> kind inner

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
      Kind_responsive (0, float_of_string value_str)
    with Not_found | Failure _ | Invalid_argument _ -> Kind_responsive (0, 0.)
  else if String.length cond > 8 && String.sub cond 1 7 = "prefers" then
    if contains cond "color-scheme" then Kind_preference_appearance
    else Kind_preference_accessibility
  else if contains cond "(hover" && contains cond "hover)" then Kind_hover
  else Kind_other

let group_order = function
  | Kind_hover -> (0, 0.)
  | Kind_other -> (500, 0.)
  | Kind_preference_accessibility -> (1000, 0.)
  | Kind_responsive_max (unit_ord, value) ->
      (1999, (Float.of_int unit_ord *. 1e9) +. (1e6 -. value))
  | Kind_responsive (unit_ord, value) ->
      (2000, (Float.of_int unit_ord *. 1e9) +. value)
  | Kind_preference_appearance -> (3000, 0.)

let rec preference_order = function
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
  | Negated inner -> preference_order inner
  | _ -> 20

(* Distinguish responsive sub-types: not-min-width comes before min-width at the
   same breakpoint value. *)
let rec responsive_subkind = function
  | Not_min_width _ | Not_min_width_rem _ | Not_min_width_length _ -> 0
  | Max_width _ -> 1
  | Min_width _ | Min_width_rem _ | Min_width_length _ -> 2
  | Negated inner -> responsive_subkind inner
  | _ -> 2

let compare a b =
  let ka = kind a and kb = kind b in
  let ga, va = group_order ka and gb, vb = group_order kb in
  let group_cmp = Int.compare ga gb in
  if group_cmp <> 0 then group_cmp
  else
    let value_cmp = Float.compare va vb in
    if value_cmp <> 0 then value_cmp
    else
      let sub_cmp = Int.compare (responsive_subkind a) (responsive_subkind b) in
      if sub_cmp <> 0 then sub_cmp
      else
        let pref_cmp = Int.compare (preference_order a) (preference_order b) in
        if pref_cmp <> 0 then pref_cmp
        else
          (* Final tiebreaker: string comparison for calc expressions or other
             conditions that have the same sort key but different content *)
          String.compare (to_string a) (to_string b)

let equal a b = compare a b = 0
