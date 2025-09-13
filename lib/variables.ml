(** CSS variables and variable extraction utilities *)

open Values
open Properties
open Declaration
include Variables_intf

(** {1 Meta handling} *)

let meta (type t) () =
  let module M = struct
    type meta += V : t -> meta
  end in
  let inj x = M.V x in
  let proj = function M.V v -> Some v | _ -> None in
  (inj, proj)

(** {1 Variable creation} *)

let var : type a.
    ?fallback:a ->
    ?layer:string ->
    ?meta:meta ->
    string ->
    a kind ->
    a ->
    declaration * a var =
 fun ?fallback ?layer ?meta name kind value ->
  let declaration =
    Declaration.custom_declaration ?layer ?meta
      (String.concat "" [ "--"; name ])
      kind value
  in
  let fallback =
    match fallback with None -> No_fallback | Some v -> Fallback v
  in
  let var_handle = { name; fallback; default = Some value; layer; meta } in
  (declaration, var_handle)

(** {1 Variable extraction} *)

let rec vars_of_calc : type a. a calc -> any_var list = function
  | Val _ -> []
  | Var v -> [ V v ]
  | Num _ -> []
  | Expr (left, _, right) -> vars_of_calc left @ vars_of_calc right

let vars_of_length_list (values : Values.length list) : any_var list =
  List.concat_map
    (fun (value : Values.length) ->
      match value with
      | Values.Var v -> [ V v ]
      | Values.Calc calc -> vars_of_calc calc
      | _ -> [])
    values

let vars_of_property : type a. a property -> a -> any_var list =
 fun prop value ->
  match (prop, value) with
  | Width, Var v -> [ V v ]
  | Width, Calc calc -> vars_of_calc calc
  | Height, Var v -> [ V v ]
  | Height, Calc calc -> vars_of_calc calc
  | Min_width, Var v -> [ V v ]
  | Min_width, Calc calc -> vars_of_calc calc
  | Min_height, Var v -> [ V v ]
  | Min_height, Calc calc -> vars_of_calc calc
  | Max_width, Var v -> [ V v ]
  | Max_width, Calc calc -> vars_of_calc calc
  | Max_height, Var v -> [ V v ]
  | Max_height, Calc calc -> vars_of_calc calc
  | Padding, values -> vars_of_length_list values
  | Padding_top, Var v -> [ V v ]
  | Padding_top, Calc calc -> vars_of_calc calc
  | Padding_right, Var v -> [ V v ]
  | Padding_right, Calc calc -> vars_of_calc calc
  | Padding_bottom, Var v -> [ V v ]
  | Padding_bottom, Calc calc -> vars_of_calc calc
  | Padding_left, Var v -> [ V v ]
  | Padding_left, Calc calc -> vars_of_calc calc
  | Padding_inline, Var v -> [ V v ]
  | Padding_inline, Calc calc -> vars_of_calc calc
  | Padding_inline_start, Var v -> [ V v ]
  | Padding_inline_start, Calc calc -> vars_of_calc calc
  | Padding_inline_end, Var v -> [ V v ]
  | Padding_inline_end, Calc calc -> vars_of_calc calc
  | Padding_block, Var v -> [ V v ]
  | Padding_block, Calc calc -> vars_of_calc calc
  | Margin, values -> vars_of_length_list values
  | Margin_top, Var v -> [ V v ]
  | Margin_top, Calc calc -> vars_of_calc calc
  | Margin_right, Var v -> [ V v ]
  | Margin_right, Calc calc -> vars_of_calc calc
  | Margin_bottom, Var v -> [ V v ]
  | Margin_bottom, Calc calc -> vars_of_calc calc
  | Margin_left, Var v -> [ V v ]
  | Margin_left, Calc calc -> vars_of_calc calc
  | Margin_inline, Var v -> [ V v ]
  | Margin_inline, Calc calc -> vars_of_calc calc
  | Margin_block, Var v -> [ V v ]
  | Margin_block, Calc calc -> vars_of_calc calc
  | Top, Var v -> [ V v ]
  | Top, Calc calc -> vars_of_calc calc
  | Right, Var v -> [ V v ]
  | Right, Calc calc -> vars_of_calc calc
  | Bottom, Var v -> [ V v ]
  | Bottom, Calc calc -> vars_of_calc calc
  | Left, Var v -> [ V v ]
  | Left, Calc calc -> vars_of_calc calc
  | Font_size, Var v -> [ V v ]
  | Font_size, Calc calc -> vars_of_calc calc
  | Letter_spacing, Var v -> [ V v ]
  | Letter_spacing, Calc calc -> vars_of_calc calc
  | Line_height, Normal -> []
  | Line_height, Px _ -> []
  | Line_height, Rem _ -> []
  | Line_height, Em _ -> []
  | Line_height, Pct _ -> []
  | Line_height, Num _ -> []
  | Line_height, Inherit -> []
  | Line_height, Var v -> [ V v ]
  | Border_width, Var v -> [ V v ]
  | Border_width, Calc calc -> vars_of_calc calc
  | Border_top_width, Var v -> [ V v ]
  | Border_top_width, Calc calc -> vars_of_calc calc
  | Border_right_width, Var v -> [ V v ]
  | Border_right_width, Calc calc -> vars_of_calc calc
  | Border_bottom_width, Var v -> [ V v ]
  | Border_bottom_width, Calc calc -> vars_of_calc calc
  | Border_left_width, Var v -> [ V v ]
  | Border_left_width, Calc calc -> vars_of_calc calc
  | Border_inline_start_width, Var v -> [ V v ]
  | Border_inline_start_width, Calc calc -> vars_of_calc calc
  | Border_inline_end_width, Var v -> [ V v ]
  | Border_inline_end_width, Calc calc -> vars_of_calc calc
  | Outline_width, Var v -> [ V v ]
  | Outline_width, Calc calc -> vars_of_calc calc
  | Column_gap, Var v -> [ V v ]
  | Column_gap, Calc calc -> vars_of_calc calc
  | Row_gap, Var v -> [ V v ]
  | Row_gap, Calc calc -> vars_of_calc calc
  | Gap, { row_gap; column_gap } ->
      let row_vars =
        match row_gap with
        | Some (Var v) -> [ V v ]
        | Some (Calc calc) -> vars_of_calc calc
        | _ -> []
      in
      let col_vars =
        match column_gap with
        | Some (Var v) -> [ V v ]
        | Some (Calc calc) -> vars_of_calc calc
        | _ -> []
      in
      row_vars @ col_vars
  (* Color properties *)
  | Background_color, Var v -> [ V v ]
  | Color, Var v -> [ V v ]
  | Border_color, Var v -> [ V v ]
  | Border_top_color, Var v -> [ V v ]
  | Border_right_color, Var v -> [ V v ]
  | Border_bottom_color, Var v -> [ V v ]
  | Border_left_color, Var v -> [ V v ]
  | Border_inline_start_color, Var v -> [ V v ]
  | Border_inline_end_color, Var v -> [ V v ]
  | Text_decoration_color, Var v -> [ V v ]
  | Outline_color, Var v -> [ V v ]
  (* Border radius *)
  | Border_radius, Var v -> [ V v ]
  | Border_radius, Calc calc -> vars_of_calc calc
  (* Outline offset *)
  | Outline_offset, Var v -> [ V v ]
  | Outline_offset, Calc calc -> vars_of_calc calc
  (* Other properties don't support Var *)
  (* All other cases *)
  | _ -> []

let rec vars_of_value : type a. a kind -> a -> any_var list =
 fun kind value ->
  match (kind, value) with
  | Length, Var v -> [ V v ]
  | Color, Var v -> [ V v ]
  | Duration, Var v -> [ V v ]
  | Blend_mode, _ -> []
  | Scroll_snap_strictness, _ -> []
  | Angle, Var v -> [ V v ]
  | Angle, _ -> []
  | Length, Calc calc -> vars_of_calc calc
  | Color, Mix _ -> [] (* TODO: extend to extract from color mix *)
  | Int, _ -> []
  | Float, _ -> []
  | Aspect_ratio, _ -> []
  | Border_style, _ -> []
  | Font_weight, _ -> []
  | String, _ -> []
  | Font_variant_numeric, Var v -> [ V v ]
  | ( Font_variant_numeric,
      Composed
        {
          ordinal;
          slashed_zero;
          numeric_figure;
          numeric_spacing;
          numeric_fraction;
        } ) ->
      vars_of_values_opt
        [
          ordinal;
          slashed_zero;
          numeric_figure;
          numeric_spacing;
          numeric_fraction;
        ]
  | Font_variant_numeric, _ -> []
  | Font_variant_numeric_token, Var v -> [ V v ]
  | Font_variant_numeric_token, _ -> []
  | Box_shadow, _ -> []
  | _ -> []

and vars_of_values_opt values =
  let collect_vars (opt_fv : font_variant_numeric_token option) =
    match opt_fv with None -> [] | Some (Var v) -> [ V v ] | Some _token -> []
  in
  List.concat_map collect_vars values

let compare_vars_by_name (V x) (V y) = String.compare x.name y.name

(** Extract all CSS variables referenced in properties (for theme layer) *)
let vars_of_declarations properties =
  List.concat_map
    (function
      | Declaration { property; value; _ } -> vars_of_property property value
      | Custom_declaration { kind; value; _ } -> vars_of_value kind value)
    properties
  |> List.sort_uniq compare_vars_by_name

(** {1 Variable name utilities} *)

let any_var_name (V v) = String.concat "" [ "--"; v.name ]

(** {1 Advanced variable extraction} *)

(* Extract variables from a typed value - needs to handle each property type *)
let extract_vars_from_prop_value : type a. a property -> a -> any_var list =
 fun prop value ->
  match (prop, value) with
  | Background_color, Var v -> [ V v ]
  | Color, Var v -> [ V v ]
  | Border_color, Var v -> [ V v ]
  | Border_top_color, Var v -> [ V v ]
  | Border_right_color, Var v -> [ V v ]
  | Border_bottom_color, Var v -> [ V v ]
  | Border_left_color, Var v -> [ V v ]
  | Border_inline_start_color, Var v -> [ V v ]
  | Border_inline_end_color, Var v -> [ V v ]
  | Text_decoration_color, Var v -> [ V v ]
  | Webkit_text_decoration_color, Var v -> [ V v ]
  | Webkit_tap_highlight_color, Var v -> [ V v ]
  | Gap, { row_gap; column_gap } ->
      let row_vars =
        match row_gap with
        | Some (Var v) -> [ V v ]
        | Some (Calc calc) -> vars_of_calc calc
        | _ -> []
      in
      let col_vars =
        match column_gap with
        | Some (Var v) -> [ V v ]
        | Some (Calc calc) -> vars_of_calc calc
        | _ -> []
      in
      row_vars @ col_vars
  | Column_gap, Var v -> [ V v ]
  | Row_gap, Var v -> [ V v ]
  | Width, Var v -> [ V v ]
  | Height, Var v -> [ V v ]
  | Min_width, Var v -> [ V v ]
  | Min_height, Var v -> [ V v ]
  | Max_width, Var v -> [ V v ]
  | Max_height, Var v -> [ V v ]
  | Font_size, Var v -> [ V v ]
  | Line_height, Var v -> [ V v ]
  | Letter_spacing, Var v -> [ V v ]
  | Top, Var v -> [ V v ]
  | Right, Var v -> [ V v ]
  | Bottom, Var v -> [ V v ]
  | Left, Var v -> [ V v ]
  | Border_radius, Var v -> [ V v ]
  | Border_width, Var v -> [ V v ]
  | Outline_offset, Var v -> [ V v ]
  | _ -> [] (* No variables in this value *)

let extract_vars_from_declaration : declaration -> any_var list = function
  | Custom_declaration _ -> [] (* Custom properties don't have typed vars *)
  | Declaration { property; value; _ } ->
      extract_vars_from_prop_value property value

(* Analyze declarations to find all variable references *)
let analyze_declarations (decls : declaration list) : any_var list =
  List.concat_map extract_vars_from_declaration decls

(* Extract only custom property declarations (variable definitions) *)
let extract_custom_declarations (decls : declaration list) : declaration list =
  List.filter (function Custom_declaration _ -> true | _ -> false) decls

(* Extract the variable name from a custom declaration *)
let custom_declaration_name (decl : declaration) : string option =
  match decl with Custom_declaration { name; _ } -> Some name | _ -> None

(** {1 Stylesheet variable extraction} *)

let vars_of_rules rules =
  List.concat_map
    (fun (rule : Stylesheet.rule) -> vars_of_declarations rule.declarations)
    rules

let vars_of_media_queries media_queries =
  List.concat_map
    (fun (mq : Stylesheet.media_rule) -> vars_of_rules mq.media_rules)
    media_queries

let vars_of_container_queries container_queries =
  List.concat_map
    (fun (cq : Stylesheet.container_rule) -> vars_of_rules cq.container_rules)
    container_queries

let vars_of_stylesheet (ss : Stylesheet.t) =
  vars_of_rules ss.rules
  @ vars_of_media_queries ss.media_queries
  @ vars_of_container_queries ss.container_queries
