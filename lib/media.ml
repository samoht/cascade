(** Structured media conditions for type-safe media query construction. *)

type t =
  | Min_width of float
  | Max_width of float
  | Prefers_reduced_motion of [ `No_preference | `Reduce ]
  | Prefers_contrast of [ `More | `Less ]
  | Prefers_color_scheme of [ `Dark | `Light ]
  | Forced_colors of [ `Active | `None ]
  | Hover
  | Raw of string

let to_string = function
  | Min_width rem -> "(min-width:" ^ Float.to_string rem ^ "rem)"
  | Max_width rem -> "(max-width:" ^ Float.to_string rem ^ "rem)"
  | Prefers_reduced_motion `No_preference ->
      "(prefers-reduced-motion:no-preference)"
  | Prefers_reduced_motion `Reduce -> "(prefers-reduced-motion:reduce)"
  | Prefers_contrast `More -> "(prefers-contrast:more)"
  | Prefers_contrast `Less -> "(prefers-contrast:less)"
  | Prefers_color_scheme `Dark -> "(prefers-color-scheme:dark)"
  | Prefers_color_scheme `Light -> "(prefers-color-scheme:light)"
  | Forced_colors `Active -> "(forced-colors:active)"
  | Forced_colors `None -> "(forced-colors:none)"
  | Hover -> "(hover:hover)"
  | Raw s -> s

type kind =
  | Kind_hover
  | Kind_responsive of float
  | Kind_preference_accessibility
  | Kind_preference_appearance
  | Kind_other

let kind = function
  | Hover -> Kind_hover
  | Min_width rem | Max_width rem -> Kind_responsive rem
  | Prefers_reduced_motion _ | Prefers_contrast _ | Forced_colors _ ->
      Kind_preference_accessibility
  | Prefers_color_scheme _ -> Kind_preference_appearance
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
  else if String.equal cond "(hover:hover)" then Kind_hover
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
  | Raw s ->
      (* Fallback to string matching for Raw conditions *)
      if contains s "reduced-motion:no-preference" then 0
      else if contains s "reduced-motion:reduce" then 1
      else if contains s "contrast:more" then 2
      else if contains s "contrast:less" then 3
      else if contains s "color-scheme" then 4
      else 10
  | _ -> 10

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
