(** CSS variables and variable extraction utilities *)

open Values
open Properties
open Declaration
include Variables_intf

(** {1 Custom Property Support} *)

(** Pretty-print a syntax descriptor to CSS syntax string *)
let rec pp_syntax_inner : type a. a syntax Pp.t =
 fun ctx syn ->
  match syn with
  | Length -> Pp.string ctx "<length>"
  | Color -> Pp.string ctx "<color>"
  | Number -> Pp.string ctx "<number>"
  | Integer -> Pp.string ctx "<integer>"
  | Percentage -> Pp.string ctx "<percentage>"
  | Length_percentage -> Pp.string ctx "<length-percentage>"
  | Angle -> Pp.string ctx "<angle>"
  | Time -> Pp.string ctx "<time>"
  | Custom_ident -> Pp.string ctx "<custom-ident>"
  | String -> Pp.string ctx "<string>"
  | Url -> Pp.string ctx "<url>"
  | Image -> Pp.string ctx "<image>"
  | Transform_function -> Pp.string ctx "<transform-function>"
  | Universal -> Pp.string ctx "*"
  | Or (syn1, syn2) ->
      pp_syntax_inner ctx syn1;
      Pp.string ctx " | ";
      pp_syntax_inner ctx syn2
  | Plus syn ->
      pp_syntax_inner ctx syn;
      Pp.string ctx "+"
  | Hash syn ->
      pp_syntax_inner ctx syn;
      Pp.string ctx "#"
  | Question syn ->
      pp_syntax_inner ctx syn;
      Pp.string ctx "?"
  | Brackets s ->
      Pp.string ctx "[";
      Pp.string ctx s;
      Pp.string ctx "]"

and pp_syntax : type a. a syntax Pp.t =
 fun ctx syn ->
  (* Syntax descriptors should be printed with quotes per CSS spec *)
  Pp.char ctx '"';
  pp_syntax_inner ctx syn;
  Pp.char ctx '"'

(** Pretty-print a value according to its syntax type *)
let rec pp_value : type a. a syntax -> a Pp.t =
 fun syntax ctx value ->
  match syntax with
  | Length -> Values.pp_length ~always:true ctx value
  | Color -> Values.pp_color ctx value
  | Number -> Pp.float ctx value
  | Integer -> Pp.int ctx value
  | Percentage -> Values.pp_percentage ~always:true ctx value
  | Length_percentage -> Values.pp_length_percentage ~always:true ctx value
  | Angle -> Values.pp_angle ctx value
  | Time -> Values.pp_duration ctx value
  | Custom_ident -> Pp.string ctx value
  | String -> Pp.quoted ctx value
  | Url ->
      Pp.string ctx "url(";
      Pp.quoted ctx value;
      Pp.string ctx ")"
  | Image -> Pp.string ctx value
  | Transform_function -> Pp.string ctx value
  | Universal -> Pp.string ctx value
  | Or (syn1, syn2) -> (
      match value with
      | Left v -> pp_value syn1 ctx v
      | Right v -> pp_value syn2 ctx v)
  | Plus syn ->
      List.iteri
        (fun i v ->
          if i > 0 then Pp.sp ctx ();
          pp_value syn ctx v)
        value
  | Hash syn ->
      List.iteri
        (fun i v ->
          if i > 0 then Pp.string ctx ", ";
          pp_value syn ctx v)
        value
  | Question syn -> (
      match value with None -> () | Some v -> pp_value syn ctx v)
  | Brackets _ -> Pp.string ctx value

(** Read a CSS syntax descriptor from input *)
let read_syntax (r : Reader.t) : any_syntax =
  (* CSS @property syntax values must be quoted strings per spec *)
  let s = Reader.string r in
  match s with
  | "<length>" -> Syntax Length
  | "<color>" -> Syntax Color
  | "<number>" -> Syntax Number
  | "<integer>" -> Syntax Integer
  | "<percentage>" -> Syntax Percentage
  | "<length-percentage>" -> Syntax Length_percentage
  | "<angle>" -> Syntax Angle
  | "<time>" -> Syntax Time
  | "<custom-ident>" -> Syntax Custom_ident
  | "<string>" -> Syntax String
  | "<url>" -> Syntax Url
  | "<image>" -> Syntax Image
  | "<transform-function>" -> Syntax Transform_function
  | "*" -> Syntax Universal
  | s when String.length s > 2 && s.[0] = '[' && s.[String.length s - 1] = ']'
    ->
      Syntax (Brackets (String.sub s 1 (String.length s - 2)))
  | s when String.contains s '|' -> (
      (* Handle composite syntax like "<length> | <percentage>" *)
      match List.map String.trim (String.split_on_char '|' s) with
      | [ "<length>"; "<percentage>" ] | [ "<percentage>"; "<length>" ] ->
          Syntax (Or (Length, Percentage))
      | _ -> Reader.err_invalid r ("Unsupported CSS composite syntax: " ^ s))
  | s -> Reader.err_invalid r ("Unsupported CSS syntax: " ^ s)

(** Read a value according to its syntax type *)
let rec read_value : type a. Reader.t -> a syntax -> a =
 fun reader syntax ->
  match syntax with
  | Universal ->
      (* For universal syntax "*", accept any CSS value - not just quoted
         strings *)
      Reader.ws reader;
      Reader.token reader
  | String -> Reader.string ~trim:true reader
  | Custom_ident -> Reader.string ~trim:true reader
  | Url -> Reader.string ~trim:true reader
  | Image -> Reader.string ~trim:true reader
  | Transform_function -> Reader.string ~trim:true reader
  | Brackets _desc -> Reader.string ~trim:true reader
  | Length -> Values.read_length reader
  | Color -> Values.read_color reader
  | Number -> Reader.number reader
  | Integer -> int_of_float (Reader.number reader)
  | Percentage -> Values.read_percentage reader
  | Length_percentage -> Values.read_length_percentage reader
  | Angle -> Values.read_angle reader
  | Time -> Values.read_duration reader
  | Or (syn1, _syn2) ->
      (* For now, only try the first syntax - proper backtracking would require
         a seekable reader *)
      Either.Left (read_value reader syn1)
  | Plus syn ->
      (* Read space-separated list - use Reader.many for proper error
         handling *)
      let values, _error_opt = Reader.many (fun r -> read_value r syn) reader in
      if values = [] then
        Reader.err_invalid reader "expected at least one value for '+' syntax"
      else values
  | Hash syn ->
      (* Read comma-separated list - use Reader.list for proper parsing *)
      let values =
        Reader.list ~sep:Reader.comma ~at_least:1
          (fun r -> read_value r syn)
          reader
      in
      values
  | Question syn ->
      (* Optional value - use Reader.option for safe parsing *)
      Reader.option (fun r -> read_value r syn) reader

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
    ?default:a ->
    ?fallback:a fallback ->
    ?layer:string ->
    ?meta:meta ->
    string ->
    a kind ->
    a ->
    declaration * a var =
 fun ?default ?fallback ?layer ?meta name kind value ->
  (* Create the declaration directly with the value *)
  let decl =
    Declaration.custom_declaration ?layer ?meta
      (String.concat "" [ "--"; name ])
      kind value
  in
  let fallback : a fallback =
    match fallback with None -> None | Some v -> v
  in
  (* Use the value as default if no explicit default provided *)
  let default_value =
    match default with Some d -> Some d | None -> Some value
  in
  let var_handle = { name; fallback; default = default_value; layer; meta } in
  (decl, var_handle)

(** {1 Variable extraction} *)

let rec vars_of_calc : type a. a calc -> any_var list = function
  | Val _ -> []
  | Var v -> [ V v ]
  | Num _ -> []
  | Expr (left, _, right) -> vars_of_calc left @ vars_of_calc right

let vars_of_length (value : Values.length) : any_var list =
  match value with Var v -> [ V v ] | Calc calc -> vars_of_calc calc | _ -> []

let vars_of_length_list (values : Values.length list) : any_var list =
  List.concat_map vars_of_length values

let vars_of_length_percentage (value : Values.length_percentage) : any_var list
    =
  match value with
  | Length l -> vars_of_length l
  | Var v -> [ V v ]
  | Calc calc -> vars_of_calc calc
  | _ -> []

let vars_of_angle (value : Values.angle) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_channel (value : Values.channel) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_alpha (value : Values.alpha) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

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
  | _ -> []

let vars_of_percentage (value : Values.percentage) : any_var list =
  match value with Var v -> [ V v ] | Calc calc -> vars_of_calc calc | _ -> []

let vars_of_number (value : Values.number) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_number_percentage (value : Values.number_percentage) : any_var list
    =
  match value with Var v -> [ V v ] | Calc calc -> vars_of_calc calc | _ -> []

let rec vars_of_color (value : Values.color) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Rgb { r; g; b } -> vars_of_channel r @ vars_of_channel g @ vars_of_channel b
  | Rgba { r; g; b; a } ->
      vars_of_channel r @ vars_of_channel g @ vars_of_channel b
      @ vars_of_alpha a
  | Hsl { h; s; l; a } ->
      vars_of_hue h @ vars_of_percentage s @ vars_of_percentage l
      @ vars_of_alpha a
  | Hwb { h; w; b; a } ->
      vars_of_hue h @ vars_of_percentage w @ vars_of_percentage b
      @ vars_of_alpha a
  | Color { components; alpha; _ } ->
      List.concat_map vars_of_component components @ vars_of_alpha alpha
  | Oklch { l; h; alpha; _ } ->
      vars_of_percentage l @ vars_of_hue h @ vars_of_alpha alpha
  | Oklab { l; alpha; _ } -> vars_of_percentage l @ vars_of_alpha alpha
  | Lch { l; h; alpha; _ } ->
      vars_of_percentage l @ vars_of_hue h @ vars_of_alpha alpha
  | Mix { color1; percent1; color2; percent2; _ } ->
      let c1_vars = vars_of_color color1 in
      let c2_vars = vars_of_color color2 in
      let p1_vars =
        match percent1 with Some p -> vars_of_percentage p | None -> []
      in
      let p2_vars =
        match percent2 with Some p -> vars_of_percentage p | None -> []
      in
      c1_vars @ c2_vars @ p1_vars @ p2_vars
  | _ -> []

let vars_of_duration (value : Values.duration) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_border_width (value : Properties.border_width) : any_var list =
  match value with Var v -> [ V v ] | Calc calc -> vars_of_calc calc | _ -> []

let vars_of_line_height (value : Properties.line_height) : any_var list =
  match value with Var v -> [ V v ] | Calc calc -> vars_of_calc calc | _ -> []

(* Helper for optional length properties in Gap *)
let vars_of_optional_length : Values.length option -> any_var list = function
  | Some (Var v) -> [ V v ]
  | Some (Calc calc) -> vars_of_calc calc
  | _ -> []

(* moved below, after [extract_vars_from_prop_value] definition *)
(* vars_of_property definition moved below to avoid forward reference *)

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
  | Font_weight, Var v -> [ V v ]
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
      | Custom_declaration { kind; value; _ } ->
          (* Only extract variables being referenced in the value, not the
             variable being defined *)
          vars_of_value kind value)
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

(* Pretty-printer for any_syntax *)
let pp_any_syntax : any_syntax Pp.t = fun ctx (Syntax syn) -> pp_syntax ctx syn

(* Reader for any_syntax *)
let read_any_syntax (r : Reader.t) : any_syntax =
  (* Reuse the main read_syntax function *)
  read_syntax r

(* Pretty-printer for any_var *)
let pp_any_var : any_var Pp.t = fun ctx (V v) -> pp_var (fun _ _ -> ()) ctx v

(** Parse a CSS variable reference with optional fallback value. This creates a
    variable handle for parsing purposes only - it doesn't have type or layer
    information which would need to be resolved from a variable registry or
    context. *)
let parse_var_reference (r : Reader.t) : string * string option =
  if Reader.looking_at r "var(" then (
    for _ = 1 to 4 do
      Reader.skip r
    done;
    (* skip "var(" *)
    Reader.expect '-' r;
    Reader.expect '-' r;
    let content = Reader.until r ')' in
    let name, fallback =
      match String.index_opt content ',' with
      | Some i ->
          let name = String.sub content 0 i |> String.trim in
          let fallback =
            String.sub content (i + 1) (String.length content - i - 1)
            |> String.trim
          in
          (name, Some fallback)
      | None -> (String.trim content, None)
    in
    (* CSS spec requires variable names to have at least one character after
       -- *)
    if name = "" then Reader.err_invalid r "CSS variable name";
    Reader.expect ')' r;
    (name, fallback))
  else Reader.err_invalid r "Expected var() function"
