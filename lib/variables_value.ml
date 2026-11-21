open Values
open Properties
open Variables_intf

let rec vars_of_calc : type a. a calc -> any_var list = function
  | Val _ -> []
  | Var v -> [ V v ]
  | Num _ -> []
  | Math_const _ -> []
  | Math_fn _ -> []
  | Sibling_index -> []
  | Sibling_count -> []
  | Expr (left, _, right) -> vars_of_calc left @ vars_of_calc right
  | Nested inner -> vars_of_calc inner
  | Parens inner -> vars_of_calc inner

let vars_of_percentage (value : Values.percentage) : any_var list =
  match value with Var v -> [ V v ] | Calc calc -> vars_of_calc calc | _ -> []

let rec vars_of_length (value : Values.length) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Calc calc -> vars_of_calc calc
  | Round (_, value, step) -> vars_of_length value @ vars_of_length step
  | Mod (a, b) | Rem_fn (a, b) -> vars_of_length a @ vars_of_length b
  | Hypot values -> List.concat_map vars_of_length values
  | Abs value -> vars_of_length value
  | Calc_size (basis, calc) -> vars_of_length basis @ vars_of_calc calc
  | Anchor (_, side, fallback) ->
      let side =
        match side with Percentage_side p -> vars_of_percentage p | _ -> []
      in
      side @ Option.fold ~none:[] ~some:vars_of_length fallback
  | Anchor_size (_, _, fallback) ->
      Option.fold ~none:[] ~some:vars_of_length fallback
  | _ -> []

let vars_of_length_list (values : Values.length list) : any_var list =
  List.concat_map vars_of_length values

(** Helper for types that share Length/Var/Calc constructors with a wildcard
    fallback (e.g., length_percentage, font_size). The caller decomposes the
    value into one of three cases. *)
let vars_of_lvc = function
  | `Length l -> vars_of_length l
  | `Var v -> [ v ]
  | `Calc calc -> vars_of_calc calc
  | `Other -> []

let vars_of_length_percentage (value : Values.length_percentage) : any_var list
    =
  vars_of_lvc
    (match value with
    | Length l -> `Length l
    | Var v -> `Var (V v)
    | Calc calc -> `Calc calc
    | _ -> `Other)

let vars_of_angle (value : Values.angle) : any_var list =
  match value with Var v -> [ V v ] | Calc c -> vars_of_calc c | _ -> []

let vars_of_channel (value : Values.channel) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_rgb (value : Values.rgb) : any_var list =
  match value with
  | Channels { r; g; b } ->
      vars_of_channel r @ vars_of_channel g @ vars_of_channel b
  | Var v -> [ V v ]

let vars_of_alpha (value : Values.alpha) : any_var list =
  match value with Var v -> [ V v ] | Calc c -> vars_of_calc c | _ -> []

let vars_of_hue (value : Values.hue) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Angle angle -> vars_of_angle angle
  | _ -> []

let vars_of_component (value : Values.component) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Calc calc -> vars_of_calc calc
  | Angle hue -> vars_of_hue hue
  | Component_none -> []
  | _ -> []

let rec vars_of_color (value : Values.color) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Authored_color { color; _ } -> vars_of_color color
  | Rgb rgb -> vars_of_rgb rgb
  | Rgba { rgb; a } -> vars_of_rgb rgb @ vars_of_alpha a
  | Hsl { h; s; l; a } ->
      vars_of_hue h @ vars_of_percentage s @ vars_of_percentage l
      @ vars_of_alpha a
  | Hwb { h; w; b; a } ->
      vars_of_hue h @ vars_of_percentage w @ vars_of_percentage b
      @ vars_of_alpha a
  | Color { components; alpha; _ } ->
      List.concat_map vars_of_component components @ vars_of_alpha alpha
  | Relative_rgb (origin, _) -> vars_of_color origin
  | Relative_color (_, origin, _) -> vars_of_color origin
  | Contrast_color color -> vars_of_color color
  | Light_dark (light, dark) -> vars_of_color light @ vars_of_color dark
  | Attribute (_, fallback) -> Option.fold ~none:[] ~some:vars_of_color fallback
  | Lab { l; alpha; _ } ->
      Option.fold ~none:[] ~some:vars_of_percentage l @ vars_of_alpha alpha
  | Oklch { l; h; alpha; _ } ->
      Option.fold ~none:[] ~some:vars_of_percentage l
      @ vars_of_hue h @ vars_of_alpha alpha
  | Oklab { l; alpha; _ } ->
      Option.fold ~none:[] ~some:vars_of_percentage l @ vars_of_alpha alpha
  | Lch { l; h; alpha; _ } ->
      Option.fold ~none:[] ~some:vars_of_percentage l
      @ vars_of_hue h @ vars_of_alpha alpha
  | Mix { color1; percent1; rest; _ } -> (
      let vars_of_pct = function
        | Some p -> vars_of_percentage p
        | None -> []
      in
      let item (c, p) = vars_of_color c @ vars_of_pct p in
      (* The first two items' colours, then their percentages, then the rest
         item by item. *)
      match rest with
      | (color2, percent2) :: more ->
          vars_of_color color1 @ vars_of_color color2 @ vars_of_pct percent1
          @ vars_of_pct percent2 @ List.concat_map item more
      | [] -> item (color1, percent1))
  | _ -> []

let rec vars_of_duration (value : Values.duration) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Calc calc -> vars_of_calc calc
  | Durations values -> List.concat_map vars_of_duration values
  | _ -> []

(* Helper for optional length properties in Gap *)
let vars_of_optional_length : Values.length option -> any_var list = function
  | Some (Var v) -> [ V v ]
  | Some (Calc calc) -> vars_of_calc calc
  | _ -> []

let rec vars_of_number_value (value : Values.number) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Calc calc -> vars_of_number_calc calc
  | Round (_, value, step)
  | Mod (value, step)
  | Rem (value, step)
  | Hypot (value, step)
  | Pow (value, step) ->
      vars_of_number_value value @ vars_of_number_value step
  | Sqrt value | Abs value | Sign value -> vars_of_number_value value
  | Sin angle -> vars_of_angle angle
  | Num _ -> []

and vars_of_number_calc (calc : Values.number calc) : any_var list =
  match calc with
  | Var v -> [ V v ]
  | Val value -> vars_of_number_value value
  | Expr (left, _, right) ->
      vars_of_number_calc left @ vars_of_number_calc right
  | Nested inner | Parens inner -> vars_of_number_calc inner
  | Num _ | Math_const _ | Math_fn _ | Sibling_index | Sibling_count -> []

(** Extract variables from timing function *)
let rec vars_of_timing_function = function
  | Ease | Linear | Ease_in | Ease_out | Ease_in_out | Step_start | Step_end
  | Steps _ | Cubic_bezier _ | Linear_function _ | Inherit | Initial | Unset
  | Revert | Revert_layer ->
      []
  | Timing_functions values -> List.concat_map vars_of_timing_function values
  | Var v -> [ V v ]

let vars_of_css_wide (value : Properties.css_wide) =
  match value with Var v -> [ V v ] | _ -> []
