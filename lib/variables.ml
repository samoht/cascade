(** CSS variables and variable extraction utilities *)

open Values
open Properties
open Declaration
open Syntax
include Variables_intf

(** {1 Custom Property Support} *)

let custom_value_ident name =
  [ Component.Preserved (Token.synthetic (Token.Ident name)) ]

let custom_value_var_empty_fallback name =
  let loc = Loc.dummy in
  let ident =
    Component.Preserved (Token.synthetic (Token.Ident ("--" ^ name)))
  in
  let comma = Component.Preserved (Token.synthetic Token.Comma) in
  [
    Component.Func
      {
        node = { name = "var"; arguments = [ ident; comma ]; terminated = true };
        loc;
      };
  ]

let string_of_custom_value = Parser.string_of_components

(* The first dashed-ident argument of a [var()] is the referenced custom
   property; leading whitespace is skipped. *)
let rec first_var_ref_ident = function
  | [] -> Option.None
  | Component.Preserved { Token.kind = Token.Ident n; _ } :: _
    when Custom_property_name.is_valid n ->
      Option.Some n
  | _ :: rest -> first_var_ref_ident rest

(* Custom-property names referenced through a real [var()] function anywhere in
   a component stream, recursing into function arguments and bracketed blocks. A
   [var(] inside a string or url is an atomic [Preserved] token, never a [Func],
   so it is correctly ignored - the false positive a text scan would produce. *)
let rec var_refs_in_components acc (components : Component.t list) =
  List.fold_left
    (fun acc (c : Component.t) ->
      match c with
      | Component.Func { node = { name; arguments; _ }; _ } ->
          let acc =
            if String.lowercase_ascii name = "var" then
              match first_var_ref_ident arguments with
              | Option.Some n -> n :: acc
              | Option.None -> acc
            else acc
          in
          var_refs_in_components acc arguments
      | Component.Block { node = { value; _ }; _ } ->
          var_refs_in_components acc value
      | Component.Preserved _ -> acc)
    acc components

let var_refs_in_value_string value =
  let p = Parser.of_string value in
  let rec collect acc =
    match Parser.next p with
    | Component.Preserved { Token.kind = Token.Eof; _ } -> List.rev acc
    | c -> collect (c :: acc)
  in
  var_refs_in_components [] (collect [])

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
  | Resolution -> Pp.string ctx "<resolution>"
  | Custom_ident -> Pp.string ctx "<custom-ident>"
  | String -> Pp.string ctx "<string>"
  | Url -> Pp.string ctx "<url>"
  | Image -> Pp.string ctx "<image>"
  | Transform_function -> Pp.string ctx "<transform-function>"
  | Transform_list -> Pp.string ctx "<transform-list>"
  | Universal -> Pp.string ctx "*"
  | Or (syn1, syn2) ->
      pp_syntax_inner ctx syn1;
      if Pp.minified ctx then Pp.char ctx '|' else Pp.string ctx " | ";
      pp_syntax_inner ctx syn2
  | Plus syn ->
      pp_syntax_inner ctx syn;
      Pp.string ctx "+"
  | Hash syn ->
      pp_syntax_inner ctx syn;
      Pp.string ctx "#"
  | Ident_keyword name -> Pp.string ctx name

let pp_syntax : type a. a syntax Pp.t =
 fun ctx syn ->
  (* Syntax descriptors should be printed with quotes per CSS spec *)
  Pp.char ctx '"';
  pp_syntax_inner ctx syn;
  Pp.char ctx '"'

(** Pretty-print a value according to its syntax type *)
let pp_registered_angle ctx = function
  | Deg 0. when Pp.minified ctx -> Pp.string ctx "0deg"
  | angle -> Values.pp_angle ctx angle

let rec pp_value : type a. a syntax -> a Pp.t =
 fun syntax ctx value ->
  match syntax with
  | Length -> Values.pp_length ~always:true ctx value
  | Color -> Values.pp_color ctx value
  | Number -> Pp.float ctx value
  | Integer -> Pp.int ctx value
  | Percentage -> Values.pp_percentage ~always:true ctx value
  | Length_percentage -> Values.pp_length_percentage ~always:true ctx value
  | Angle -> pp_registered_angle ctx value
  | Time -> Values.pp_duration ctx value
  | Resolution -> Pp.string ctx value
  | Custom_ident -> Pp.string ctx value
  | String -> Pp.quoted ctx value
  | Url -> Pp.url ctx value
  | Image ->
      let value =
        if Pp.minified ctx then Properties.minify_background_image value
        else value
      in
      Properties.pp_background_image ctx value
  | Transform_function -> Pp.string ctx value
  | Transform_list -> Pp.string ctx value
  | Universal -> Pp.string ctx value
  | Or (syn1, syn2) -> (
      match value with
      | Left v -> pp_value syn1 ctx v
      | Right v -> pp_value syn2 ctx v)
  | Plus syn ->
      (* The [+] separator is the whole of it: drop the space and [10px 20px] is
         one dimension whose unit is [px20px]. *)
      List.iteri
        (fun i v ->
          if i > 0 then Pp.space ctx ();
          pp_value syn ctx v)
        value
  | Hash syn ->
      List.iteri
        (fun i v ->
          if i > 0 then Pp.comma ctx ();
          pp_value syn ctx v)
        value
  | Ident_keyword name -> Pp.string ctx name

(* A value already typed by [syntax] is already spelled for the substitution
   site (CSS Variables 1 sec. 2); [custom_property] still checks the name and
   parses the printed text into the token stream a declaration holds. *)
let typed_custom_property ?layer name syntax value =
  custom_property ?layer name
    (Pp.to_string ~minify:true (pp_value syntax) value)

(* CSS Properties and Values API 1 (ED) sec. 5.1 lists the named [<...>] type
   references. Bare ident keywords match the [<custom-ident>] shape so a leading
   letter followed by ident-continue characters counts; this rejects stray
   punctuation. *)
let is_ident_keyword s =
  String.length s > 0
  && is_ascii_ident_start s.[0]
  &&
  let rec loop i =
    if i = String.length s then true
    else if is_ascii_ident_continue s.[i] then loop (i + 1)
    else false
  in
  loop 1

(** Read a CSS syntax descriptor from input *)
let read_simple_syntax_component r s : any_syntax =
  match s with
  | "<length>" -> Syntax Length
  | "<color>" -> Syntax Color
  | "<number>" -> Syntax Number
  | "<integer>" -> Syntax Integer
  | "<percentage>" -> Syntax Percentage
  | "<length-percentage>" -> Syntax Length_percentage
  | "<angle>" -> Syntax Angle
  | "<time>" -> Syntax Time
  | "<resolution>" -> Syntax Resolution
  | "<custom-ident>" -> Syntax Custom_ident
  | "<string>" -> Syntax String
  | "<url>" -> Syntax Url
  | "<image>" -> Syntax Image
  | "<transform-function>" -> Syntax Transform_function
  | "<transform-list>" -> Syntax Transform_list
  | s when is_ident_keyword s -> Syntax (Ident_keyword s)
  | s -> Cursor.err_invalid r ("Unsupported CSS syntax: " ^ s)

(* CSS Properties and Values API 1 (ED) sec. 5.2: only [+] and [#] are valid
   syntax multipliers. *)
let apply_syntax_modifier r (Syntax inner) (modifier : char option) : any_syntax
    =
  match (inner, modifier) with
  | Transform_list, Some ('+' | '#') ->
      Cursor.err_invalid r
        "a pre-multiplied CSS syntax component cannot take a multiplier"
  | inner, None -> Syntax inner
  | inner, Some '+' -> Syntax (Plus inner)
  | inner, Some '#' -> Syntax (Hash inner)
  | _, Some c ->
      Cursor.err_invalid r
        (String.concat ""
           [ "Unsupported CSS syntax modifier: '"; String.make 1 c; "'" ])

(* CSS Properties and Values API 1 (ED) sec. 5.4.3 sets a component's multiplier
   from a single [+] or [#] and returns, so a component carries at most one.
   Sec. 5.4.2 accepts only EOF or [|] after a component, which fails the whole
   syntax definition on a second multiplier; here that one is left in the body
   for [read_simple_syntax_component] to reject, the route [++], [##] and [#+]
   already take. *)
let split_syntax_modifier s : string * char option =
  let n = String.length s in
  if n = 0 then (s, None)
  else
    let last = s.[n - 1] in
    if last <> '+' && last <> '#' then (s, None)
    else (String.sub s 0 (n - 1), Some last)

let read_syntax (r : Cursor.t) : any_syntax =
  (* CSS @property syntax values must be quoted strings per spec *)
  let s = String.trim (Cursor.string r) in
  let read_component part =
    let body, modifier = split_syntax_modifier (String.trim part) in
    if body = "" then Cursor.err_invalid r "empty CSS syntax component";
    apply_syntax_modifier r (read_simple_syntax_component r body) modifier
  in
  (* Sec. 5.4.2 strips the surrounding whitespace, then returns the universal
     syntax definition only for a string that is [*] and nothing else. As a
     component [*] is none of the shapes sec. 5.4.3 accepts, so it carries no
     multiplier and joins no alternation. *)
  if s = "*" then Syntax Universal
  else if String.contains s '|' then
    let parts = String.split_on_char '|' s in
    let components = List.map read_component parts in
    match components with
    | [] | [ _ ] ->
        (* split_on_char never returns empty list; the single-element case
           cannot reach here because '|' is in [s]. *)
        Cursor.err_invalid r "invalid CSS syntax disjunction"
    | first :: rest ->
        List.fold_left
          (fun (Syntax left) (Syntax right) -> Syntax (Or (left, right)))
          first rest
  else read_component s

(* These types have no reader of their own and take whatever is left. None of
   them spells a value as nothing, and a reader that succeeded on an empty
   stream would leave the [+] and [#] repetitions below with no way to stop. *)
let read_remaining_typed reader what =
  let value = Cursor.consume_remaining_as_string ~trim:true reader in
  if value = "" then Cursor.err_expected reader what;
  value

(** Read a value according to its syntax type *)
let rec read_value : type a. Cursor.t -> a syntax -> a =
 fun reader syntax ->
  match syntax with
  | Universal ->
      (* For universal syntax "*", accept any CSS value -- consume the remaining
         components and serialise them back to source text so the surrounding
         [expect_eof] sees an empty cursor. *)
      Cursor.consume_remaining_as_string ~trim:true reader
  | String -> Cursor.string ~trim:true reader
  | Custom_ident -> Cursor.ident ~keep_case:true reader
  | Url -> Cursor.url reader
  | Image -> Properties.read_background_image reader
  | Transform_function -> read_remaining_typed reader "<transform-function>"
  | Transform_list -> read_remaining_typed reader "<transform-list>"
  | Resolution -> read_remaining_typed reader "<resolution>"
  | Length -> Values.read_length reader
  | Color -> Values.read_color reader
  | Number -> Cursor.number reader
  | Integer -> Cursor.int reader
  | Percentage -> Values.read_percentage reader
  | Length_percentage -> Values.read_length_percentage reader
  | Angle -> Values.read_angle_unit_required reader
  | Time -> Values.read_duration reader
  | Or (syn1, syn2) -> (
      (* Try the left branch with backtracking; fall back to the right. *)
      match Cursor.option (fun r -> read_value r syn1) reader with
      | Some v -> Either.Left v
      | None -> Either.Right (read_value reader syn2))
  | Plus syn ->
      (* Read space-separated list - use Cursor.many for proper error
         handling *)
      let values, _error_opt = Cursor.many (fun r -> read_value r syn) reader in
      if values = [] then
        Cursor.err_invalid reader "expected at least one value for '+' syntax"
      else values
  | Hash syn ->
      (* Read comma-separated list - use Cursor.list for proper parsing *)
      let values =
        Cursor.list ~sep:Cursor.comma ~at_least:1
          (fun r -> read_value r syn)
          reader
      in
      values
  | Ident_keyword name ->
      let got = Cursor.ident reader in
      if got <> name then
        Cursor.err_invalid reader
          (String.concat ""
             [ "expected keyword '"; name; "', got '"; got; "'" ])

open Common

let list_map_preserve = List.map_preserve

let rec normalize_value : type a. ?lossless:bool -> a syntax -> a -> a =
 fun ?(lossless = false) syntax value ->
  match syntax with
  | Color -> Values.normalize_color ~lossless value
  | Or (left, right) -> (
      match value with
      | Either.Left v ->
          let v' = normalize_value ~lossless left v in
          if v' == v then value else Either.Left v'
      | Either.Right v ->
          let v' = normalize_value ~lossless right v in
          if v' == v then value else Either.Right v')
  | Plus syntax -> list_map_preserve (normalize_value ~lossless syntax) value
  | Hash syntax -> list_map_preserve (normalize_value ~lossless syntax) value
  | Length | Number | Integer | Percentage | Length_percentage | Angle | Time
  | Resolution | Custom_ident | String | Url | Image | Transform_function
  | Transform_list | Universal | Ident_keyword _ ->
      value

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
    ?runtime:bool ->
    string ->
    a kind ->
    a ->
    declaration * a var =
 fun ?default ?fallback ?layer ?meta ?(runtime = false) name kind value ->
  (* Create the declaration directly with the value *)
  let decl =
    Declaration.v
      (Custom_property (String.concat "" [ "--"; name ]))
      (Custom_value { value = Typed { kind; value }; layer; meta })
  in
  let fallback : a fallback =
    match fallback with None -> None | Some v -> v
  in
  (* Use the value as default if no explicit default provided *)
  let default_value =
    match default with Some d -> Some d | None -> Some value
  in
  let var_handle =
    { name; fallback; default = default_value; layer; meta; runtime }
  in
  (decl, var_handle)

(** {1 Variable extraction} *)

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

let rec vars_of_length (value : Values.length) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Calc calc -> vars_of_calc calc
  | Round (_, value, step) -> vars_of_length value @ vars_of_length step
  | Mod (a, b) | Rem_fn (a, b) -> vars_of_length a @ vars_of_length b
  | Hypot values -> List.concat_map vars_of_length values
  | Abs value | Sign value -> vars_of_length value
  | Calc_size (basis, calc) -> vars_of_length basis @ vars_of_calc calc
  | Anchor (_, _, Some fallback) -> vars_of_length fallback
  | _ -> []

let vars_of_length_list (values : Values.length list) : any_var list =
  List.concat_map vars_of_length values

let vars_of_border_spacing (value : Properties.border_spacing) : any_var list =
  match value with
  | Var v -> [ V v ]
  | (Lengths values : Properties.border_spacing) -> vars_of_length_list values

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

let vars_of_text_indent_value (value : Properties.text_indent_value) :
    any_var list =
  match value with
  | Var v -> [ V v ]
  | Indent { length; _ } -> vars_of_length_percentage length
  | Inherit | Initial | Unset | Revert | Revert_layer -> []

let vars_of_font_size (value : Properties.font_size) : any_var list =
  vars_of_lvc
    (match value with
    | Length l -> `Length l
    | Var v -> `Var (V v)
    | Calc calc -> `Calc calc
    | _ -> `Other)

let vars_of_angle (value : Values.angle) : any_var list =
  match value with Var v -> [ V v ] | Calc c -> vars_of_calc c | _ -> []

let vars_of_rotate_value (value : Properties.rotate_value) : any_var list =
  match value with
  | Angle a | X a | Y a | Z a -> vars_of_angle a
  | Axis (_, _, _, a) -> vars_of_angle a
  | Var v -> [ V v ]
  | None | Inherit | Initial | Unset | Revert | Revert_layer -> []

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

let vars_of_percentage (value : Values.percentage) : any_var list =
  match value with Var v -> [ V v ] | Calc calc -> vars_of_calc calc | _ -> []

let rec vars_of_color (value : Values.color) : any_var list =
  match value with
  | Var v -> [ V v ]
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

let rec vars_of_duration (value : Values.duration) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Calc calc -> vars_of_calc calc
  | Durations values -> List.concat_map vars_of_duration values
  | _ -> []

let vars_of_border_width (value : Properties.border_width) : any_var list =
  match value with Var v -> [ V v ] | Calc calc -> vars_of_calc calc | _ -> []

let vars_of_border_width_list values =
  List.concat_map vars_of_border_width values

let vars_of_line_height (value : Properties.line_height) : any_var list =
  match value with Var v -> [ V v ] | Calc calc -> vars_of_calc calc | _ -> []

(* Helper for optional length properties in Gap *)
let vars_of_optional_length : Values.length option -> any_var list = function
  | Some (Var v) -> [ V v ]
  | Some (Calc calc) -> vars_of_calc calc
  | _ -> []

(* Complex type extractors *)
let vars_of_font_weight (value : Properties.font_weight) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_family (value : Properties.font_family) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_transform (value : Properties.transform) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_transform_list (value : Properties.transform list) : any_var list =
  List.concat_map vars_of_transform value

let rec vars_of_shadow (value : Properties.shadow) : any_var list =
  match value with
  | Var v -> [ V v ]
  | List shadows -> List.concat_map vars_of_shadow shadows
  | Shadow { color; _ } -> (
      match color with Some c -> vars_of_color c | None -> [])
  | _ -> []

let vars_of_content (value : Properties.content) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_counter_set (value : Properties.counter_set) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_blend_mode (value : Properties.blend_mode) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_border_style (value : Properties.border_style) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_decoration (value : Properties.text_decoration) : any_var list
    =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_emphasis_style (value : Properties.text_emphasis_style) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_emphasis (value : Properties.text_emphasis) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Emphasis (style, color) ->
      Option.value ~default:[] (Option.map vars_of_text_emphasis_style style)
      @ Option.value ~default:[] (Option.map vars_of_color color)
  | _ -> []

let vars_of_text_emphasis_position (value : Properties.text_emphasis_position) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_orientation (value : Properties.text_orientation) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_transform (value : Properties.text_transform) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_scale (value : Properties.scale) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_translate_value (value : Properties.translate_value) : any_var list
    =
  match value with
  | Var v -> [ V v ]
  | X len -> vars_of_length len
  | XY (len1, len2) -> vars_of_length len1 @ vars_of_length len2
  | XYZ (len1, len2, len3) ->
      vars_of_length len1 @ vars_of_length len2 @ vars_of_length len3
  | None | Inherit | Initial | Unset | Revert | Revert_layer -> []

let vars_of_quotes (value : Properties.quotes) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_animation (value : Properties.animation) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_transition_property_value
    (value : Properties.transition_property_value) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_transition_property_list (value : Properties.transition_property) :
    any_var list =
  List.concat_map vars_of_transition_property_value value

let vars_of_transition (value : Properties.transition) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_filter (value : Properties.filter) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_background (value : Properties.background) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Vars vars -> List.map (fun v -> V v) vars
  | _ -> []

let vars_of_content_visibility (value : Properties.content_visibility) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let rec vars_of_animation_name (value : Properties.animation_name) :
    any_var list =
  match value with
  | Var v -> [ V v ]
  | Names names -> List.concat_map vars_of_animation_name names
  | _ -> []

let vars_of_font_feature_settings (value : Properties.font_feature_settings) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_variation_settings (value : Properties.font_variation_settings)
    : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_variant_numeric (value : Properties.font_variant_numeric) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_scroll_snap_strictness (value : Properties.scroll_snap_strictness) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_scroll_snap_axis (value : Properties.scroll_snap_axis) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_scroll_snap_type (value : Properties.scroll_snap_type) :
    any_var list =
  match value with
  | Var v -> [ V v ]
  | Axis axis -> vars_of_scroll_snap_axis axis
  | Axis_with_strictness (axis, strictness) ->
      vars_of_scroll_snap_axis axis @ vars_of_scroll_snap_strictness strictness
  | Inherit | Initial | Unset | Revert | Revert_layer -> []

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

let vars_of_aspect_ratio (value : Properties.aspect_ratio) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Auto_ratio_calc (width, height) | Ratio_calc (width, height) ->
      vars_of_number_value width @ vars_of_number_value height
  | _ -> []

let vars_of_webkit_gradient_stop = function
  | Properties.Webkit_gradient.From color | Properties.Webkit_gradient.To color
    ->
      vars_of_color color
  | Properties.Webkit_gradient.Color_stop (position, color) ->
      vars_of_percentage position @ vars_of_color color

let vars_of_color_interpolation (value : Properties.color_interpolation) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_position_value (value : Properties.position_value) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Single l -> vars_of_length l
  | XY (l1, l2) -> vars_of_length l1 @ vars_of_length l2
  | Edge_offset_axis (_, lp, _) -> vars_of_length_percentage lp
  | Axis_edge_offset (_, _, offset) -> vars_of_length_percentage offset
  | Edge_offset_edge_offset (_, lp1, _, lp2) ->
      vars_of_length_percentage lp1 @ vars_of_length_percentage lp2
  | _ -> []

let vars_of_background_position_axis
    (value : Properties.background_position_axis) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Offset lp | Edge_offset (_, lp) -> vars_of_length_percentage lp
  | _ -> []

let rec vars_of_gradient_direction (value : Properties.gradient_direction) :
    any_var list =
  match value with
  | Angle angle -> vars_of_angle angle
  | With_interpolation (direction, interpolation) ->
      vars_of_gradient_direction direction
      @ vars_of_color_interpolation interpolation
  | Var v -> [ V v ]
  | _ -> []

let vars_of_radial_shape (value : Properties.radial_shape) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_radial_size (value : Properties.radial_size) : any_var list =
  match value with
  | Circle_radius length -> vars_of_length length
  | Ellipse_radii (lp1, lp2) ->
      vars_of_length_percentage lp1 @ vars_of_length_percentage lp2
  | Var v -> [ V v ]
  | _ -> []

let vars_of_radial_gradient_config (value : Properties.radial_gradient_config) :
    any_var list =
  Option.fold ~none:[] ~some:vars_of_radial_shape value.shape
  @ Option.fold ~none:[] ~some:vars_of_radial_size value.size
  @ Option.fold ~none:[] ~some:vars_of_position_value value.position
  @ Option.fold ~none:[] ~some:vars_of_color_interpolation value.interpolation

let vars_of_conic_gradient_config (value : Properties.conic_gradient_config) :
    any_var list =
  Option.fold ~none:[] ~some:vars_of_angle value.angle
  @ Option.fold ~none:[] ~some:vars_of_position_value value.position
  @ Option.fold ~none:[] ~some:vars_of_color_interpolation value.interpolation

let vars_of_gradient_position (value : Properties.gradient_position) :
    any_var list =
  match value with
  | Linear_position direction -> vars_of_gradient_direction direction
  | Radial_position config -> vars_of_radial_gradient_config config
  | Conic_position config -> vars_of_conic_gradient_config config
  | Var v -> [ V v ]

let rec vars_of_gradient_stop (value : Properties.gradient_stop) : any_var list
    =
  match value with
  | Var v -> [ V v ]
  | Color_percentage (color, first, second) ->
      vars_of_color color
      @ Option.fold ~none:[] ~some:vars_of_length_percentage first
      @ Option.fold ~none:[] ~some:vars_of_length_percentage second
  | Color_length (color, first, second) ->
      vars_of_color color
      @ Option.fold ~none:[] ~some:vars_of_length first
      @ Option.fold ~none:[] ~some:vars_of_length second
  | Length length -> vars_of_length length
  | Channel channel -> vars_of_channel channel
  | Percentage percentage -> vars_of_percentage percentage
  | List stops -> List.concat_map vars_of_gradient_stop stops
  | Position position -> vars_of_gradient_position position
  | Direction direction -> vars_of_gradient_direction direction

let vars_of_background_image (value : Properties.background_image) :
    any_var list =
  match value with
  | Var v -> [ V v ]
  | Linear_gradient (direction, stops)
  | Repeating_linear_gradient (direction, stops)
  | Webkit_linear_gradient (direction, stops)
  | Webkit_repeating_linear_gradient (direction, stops)
  | Moz_linear_gradient (direction, stops)
  | Moz_repeating_linear_gradient (direction, stops)
  | O_linear_gradient (direction, stops)
  | O_repeating_linear_gradient (direction, stops) ->
      vars_of_gradient_direction direction
      @ List.concat_map vars_of_gradient_stop stops
  | Radial_gradient (_, stops)
  | Repeating_radial_gradient (_, stops)
  | Webkit_radial_gradient (_, stops)
  | Webkit_repeating_radial_gradient (_, stops)
  | Moz_radial_gradient (_, stops)
  | Moz_repeating_radial_gradient (_, stops)
  | O_radial_gradient (_, stops)
  | O_repeating_radial_gradient (_, stops)
  | Conic_gradient (_, stops)
  | Repeating_conic_gradient (_, stops) ->
      List.concat_map vars_of_gradient_stop stops
  | Webkit_gradient (Webkit_gradient.Linear { stops; _ })
  | Webkit_gradient (Webkit_gradient.Radial { stops; _ }) ->
      List.concat_map vars_of_webkit_gradient_stop stops
  | _ -> []

let vars_of_background_size (value : Properties.background_size) : any_var list
    =
  match value with
  | Var v -> [ V v ]
  | Length l -> vars_of_length l
  | Size (w, h) -> vars_of_length w @ vars_of_length h
  | _ -> []

let vars_of_columns_value (value : Properties.columns_value) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Width l -> vars_of_length l
  | Both (l, _) -> vars_of_length l
  | _ -> []

let vars_of_border_image_repeat (value : Properties.border_image_repeat) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_border_image_width (value : Properties.border_image_width) :
    any_var list =
  match value with
  | Var v -> [ V v ]
  | Widths values ->
      List.concat_map
        (fun (item : Properties.border_image_width_item) ->
          match item with
          | Length length -> vars_of_length length
          | Number n -> vars_of_number_value n
          | _ -> [])
        values
  | _ -> []

let vars_of_border_image_outset (value : Properties.border_image_outset) :
    any_var list =
  match value with
  | Var v -> [ V v ]
  | Outsets values ->
      List.concat_map
        (fun (item : Properties.border_image_outset_item) ->
          match item with
          | Length length -> vars_of_length length
          | Number n -> vars_of_number_value n)
        values
  | _ -> []

let vars_of_column_width (value : Properties.column_width) : any_var list =
  match value with Var v -> [ V v ] | Width l -> vars_of_length l | _ -> []

let vars_of_column_height (value : Properties.column_height) : any_var list =
  match value with Var v -> [ V v ] | Height l -> vars_of_length l | _ -> []

let vars_of_column_wrap (value : Properties.column_wrap) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_column_count (value : Properties.column_count) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_column_span (value : Properties.column_span) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_contain (value : Properties.contain) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let rec vars_of_cursor (value : Properties.cursor) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Url (_, _, fallback) -> vars_of_cursor fallback
  | _ -> []

let vars_of_interactivity (value : Properties.interactivity) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_caret_animation (value : Properties.caret_animation) : any_var list
    =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_caret_shape (value : Properties.caret_shape) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_caret (value : Properties.caret) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Caret (color, _, _) -> Option.fold ~none:[] ~some:vars_of_color color
  | _ -> []

let vars_of_interest_delay (value : Properties.interest_delay) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_nav (value : Properties.nav) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let rec vars_of_grid_template (value : Properties.grid_template) : any_var list
    =
  match value with
  | Var v -> [ V v ]
  | Fit_content l -> vars_of_length l
  | Min_max (a, b) -> vars_of_grid_template a @ vars_of_grid_template b
  | Repeat (_, ts) -> List.concat_map vars_of_grid_template ts
  | Tracks ts -> List.concat_map vars_of_grid_template ts
  | Split (rows, columns) ->
      vars_of_grid_template rows @ vars_of_grid_template columns
  | Auto_flow_columns (rows, _, auto_columns) ->
      vars_of_grid_template rows
      @ Option.fold ~none:[] ~some:vars_of_grid_template auto_columns
  | Auto_flow_rows (_, auto_rows, columns) ->
      Option.fold ~none:[] ~some:vars_of_grid_template auto_rows
      @ vars_of_grid_template columns
  | Named_tracks ts ->
      List.concat_map (fun (_, t) -> vars_of_grid_template t) ts
  | _ -> []

let vars_of_grid_line (value : Properties.grid_line) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_grid_line_pair (value : Properties.grid_line_pair) : any_var list =
  match value with
  | Var v -> [ V v ]
  | (Lines (start, end_) : Properties.grid_line_pair) ->
      vars_of_grid_line start @ vars_of_grid_line end_

let vars_of_list_style_image (value : Properties.list_style_image) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_list_style_type (value : Properties.list_style_type) : any_var list
    =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_outline_style (value : Properties.outline_style) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_shadow (value : Properties.text_shadow) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Text_shadow { h_offset; v_offset; blur; color; _ } -> (
      vars_of_length h_offset @ vars_of_length v_offset
      @ (match blur with Some l -> vars_of_length l | None -> [])
      @ match color with Some c -> vars_of_color c | None -> [])
  | _ -> []

let vars_of_text_shadow_list (values : Properties.text_shadow list) :
    any_var list =
  List.concat_map vars_of_text_shadow values

let vars_of_transform_origin (value : Properties.transform_origin) :
    any_var list =
  match value with
  | Var v -> [ V v ]
  | Position position -> vars_of_position_value position
  | X l -> vars_of_length l
  | XY (l1, l2) -> vars_of_length l1 @ vars_of_length l2
  | XYZ (l1, l2, l3) ->
      vars_of_length l1 @ vars_of_length l2 @ vars_of_length l3
  | Position_z (position, z) ->
      vars_of_position_value position @ vars_of_length z
  | _ -> []

let vars_of_vertical_align (value : Properties.vertical_align) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_will_change (value : Properties.will_change) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let rec vars_of_opacity (value : Properties.opacity) : any_var list =
  match value with
  | Opacity_number _ -> []
  | Calc calc -> vars_of_calc calc
  | Abs v | Sign v -> vars_of_opacity v
  | Var v -> [ V v ]
  | Inherit | Initial | Unset | Revert | Revert_layer -> []

let vars_of_shape_image_threshold (value : Properties.shape_image_threshold) :
    any_var list =
  match value with
  | Var v -> [ V v ]
  | Number _ | Inherit | Initial | Unset | Revert | Revert_layer -> []

let vars_of_overflow_clip_margin (value : Properties.overflow_clip_margin) :
    any_var list =
  match value with
  | Var v -> [ V v ]
  | Clip_margin (_, Some length) -> vars_of_length length
  | Clip_margin (_, None) | Initial | Inherit | Unset | Revert | Revert_layer ->
      []

let vars_of_tab_size (value : Properties.tab_size) : any_var list =
  match value with
  | Int _ -> []
  | Length len -> vars_of_length len
  | Var v -> [ V v ]
  | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_zoom (value : Properties.zoom) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Normal | Reset | Num _ | Pct _ | Initial | Inherit | Unset | Revert
  | Revert_layer ->
      []

let compare_vars_by_name (V x) (V y) = String.compare x.name y.name

(** {1 Variable name utilities} *)

let any_var_name (V v) = String.concat "" [ "--"; v.name ]

(** Extract variables from timing function *)
let rec vars_of_timing_function = function
  | Ease | Linear | Ease_in | Ease_out | Ease_in_out | Step_start | Step_end
  | Steps _ | Cubic_bezier _ | Linear_function _ | Inherit | Initial | Unset
  | Revert | Revert_layer ->
      []
  | Timing_functions values -> List.concat_map vars_of_timing_function values
  | Var v -> [ V v ]

let vars_of_display (value : Properties.display) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_position (value : Properties.position) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_overflow (value : Properties.overflow) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_flex_direction (value : Properties.flex_direction) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_flex_wrap (value : Properties.flex_wrap) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_flex_flow (value : Properties.flex_flow) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_flex_factor (value : Properties.flex_factor) =
  match value with Var v -> [ V v ] | Calc c -> vars_of_calc c | _ -> []

let vars_of_align_content (value : Properties.align_content) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_align_items (value : Properties.align_items) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_align_self (value : Properties.align_self) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_justify_content (value : Properties.justify_content) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_justify_items (value : Properties.justify_items) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_justify_self (value : Properties.justify_self) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_place_content (value : Properties.place_content) =
  match value with
  | Var v -> [ V v ]
  | Align_justify (align, justify) ->
      vars_of_align_content align @ vars_of_justify_content justify
  | _ -> []

let vars_of_place_items (value : Properties.place_items) =
  match value with
  | Var v -> [ V v ]
  | Align_justify (align, justify) ->
      vars_of_align_items align @ vars_of_justify_items justify
  | _ -> []

let vars_of_grid_flow_component (value : Properties.grid_auto_flow_component) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_grid_auto_flow (value : Properties.grid_auto_flow) =
  match value with
  | Var v -> [ V v ]
  | Components components ->
      List.concat_map vars_of_grid_flow_component components
  | _ -> []

let vars_of_container_name (value : Properties.container_name) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_anchor_name (value : Properties.anchor_name) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_position_anchor (value : Properties.position_anchor) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_position_try_fallbacks (value : Properties.position_try_fallbacks) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_position_try_order (value : Properties.position_try_order) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_position_try (value : Properties.position_try) =
  match value with
  | Var v -> [ V v ]
  | Try (order, fallbacks) ->
      vars_of_position_try_order order
      @ vars_of_position_try_fallbacks fallbacks
  | _ -> []

let vars_of_position_visibility (value : Properties.position_visibility) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_position_area (value : Properties.position_area) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_overflow_anchor (value : Properties.overflow_anchor) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_scrollbar_width (value : Properties.scrollbar_width) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_scrollbar_color (value : Properties.scrollbar_color) =
  match value with
  | Var v -> [ V v ]
  | Colors (thumb, track) -> vars_of_color thumb @ vars_of_color track
  | Auto | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_scrollbar_gutter (value : Properties.scrollbar_gutter) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_palette (value : Properties.font_palette) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_synthesis (value : Properties.font_synthesis) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_animation_timeline (value : Properties.animation_timeline) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_animation_range_item (value : Properties.animation_range_item) =
  match value with
  | Var v -> [ V v ]
  | Normal -> []
  | Offset lp | Named (_, Some lp) -> vars_of_length_percentage lp
  | Named (_, None) -> []
  | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_animation_range (value : Properties.animation_range) =
  match value with
  | Var v -> [ V v ]
  | Range (first, second) ->
      vars_of_animation_range_item first
      @ Option.fold ~none:[] ~some:vars_of_animation_range_item second
  | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_view_transition_name (value : Properties.view_transition_name) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_view_transition_class (value : Properties.view_transition_class) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_image_orientation (value : Properties.image_orientation) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_image_rendering (value : Properties.image_rendering) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_image_resolution (value : Properties.image_resolution) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_intrinsic_size_item (value : Properties.contain_intrinsic_size_item)
    =
  match value with Length len | Auto len -> vars_of_length len

let vars_of_contain_intrinsic_size (value : Properties.contain_intrinsic_size) =
  match value with
  | Var v -> [ V v ]
  | Intrinsic (first, second) ->
      vars_of_intrinsic_size_item first
      @ Option.fold ~none:[] ~some:vars_of_intrinsic_size_item second
  | None | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_contain_intrinsic_longhand
    (value : Properties.contain_intrinsic_longhand) =
  match value with
  | Var v -> [ V v ]
  | Size size -> vars_of_intrinsic_size_item size
  | None | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_margin_trim (value : Properties.margin_trim) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_border_radius (value : Properties.border_radius) =
  let from_list = List.concat_map vars_of_length_percentage in
  match value with
  | Var v -> [ V v ]
  | Radius { horizontal; vertical } ->
      from_list horizontal
      @ Option.value ~default:[] (Option.map from_list vertical)
  | Inherit | Initial | Unset | Revert | Revert_layer -> []

let vars_of_clip_path_extent (value : Properties.clip_path_extent) =
  match value with Extent_length l -> vars_of_length l | _ -> []

let rec vars_of_clip_path (value : Properties.clip_path) =
  match value with
  | Var v -> [ V v ]
  | Clip_path_inset { top; right; bottom; left; rounded } ->
      vars_of_length_percentage top
      @ Option.value ~default:[] (Option.map vars_of_length_percentage right)
      @ Option.value ~default:[] (Option.map vars_of_length_percentage bottom)
      @ Option.value ~default:[] (Option.map vars_of_length_percentage left)
      @ Option.value ~default:[] (Option.map vars_of_border_radius rounded)
  | Clip_path_circle { radius; _ } ->
      Option.value ~default:[] (Option.map vars_of_clip_path_extent radius)
  | Clip_path_ellipse { rx; ry; _ } ->
      Option.value ~default:[] (Option.map vars_of_clip_path_extent rx)
      @ Option.value ~default:[] (Option.map vars_of_clip_path_extent ry)
  | Clip_path_polygon { points; _ } ->
      List.concat_map (fun (a, b) -> vars_of_length a @ vars_of_length b) points
  | Clip_path_box _ -> []
  | Clip_path_with_box { shape; _ } -> vars_of_clip_path shape
  | Clip_path_xywh { x; y; width; height; rounded }
  | Clip_path_rect
      { top = x; right = y; bottom = width; left = height; rounded } ->
      vars_of_length_percentage x
      @ vars_of_length_percentage y
      @ vars_of_length_percentage width
      @ vars_of_length_percentage height
      @ Option.value ~default:[] (Option.map vars_of_border_radius rounded)
  | _ -> []

let vars_of_ray (value : Properties.ray) =
  vars_of_angle value.angle
  @ Option.fold ~none:[] ~some:vars_of_position_value value.position

let vars_of_offset_path (value : Properties.offset_path) =
  match value with
  | Var v -> [ V v ]
  | Ray ray -> vars_of_ray ray
  | Shape shape -> vars_of_clip_path shape
  | None | Url _ | Path _ | Initial | Inherit | Unset | Revert | Revert_layer ->
      []

let vars_of_offset_anchor (value : Properties.offset_anchor) =
  match value with
  | Var var -> [ V var ]
  | Position position -> vars_of_position_value position
  | Auto | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_offset_position (value : Properties.offset_position) =
  match value with
  | Var var -> [ V var ]
  | Position position -> vars_of_position_value position
  | Normal | Auto | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_offset_rotate (value : Properties.offset_rotate) =
  match value with
  | Var v -> [ V v ]
  | Angle angle | With_angle (_, angle) -> vars_of_angle angle
  | Auto | Reverse | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_offset_target (value : Properties.offset_target) =
  match value with
  | Position_only position -> vars_of_offset_position position
  | With_path { position; path; distance; rotate } ->
      List.concat
        [
          (match position with
          | Some position -> vars_of_offset_position position
          | None -> []);
          vars_of_offset_path path;
          (match distance with
          | Some distance -> vars_of_length_percentage distance
          | None -> []);
          (match rotate with
          | Some rotate -> vars_of_offset_rotate rotate
          | None -> []);
        ]

let vars_of_offset (value : Properties.offset) =
  match value with
  | Var var -> [ V var ]
  | Shorthand { target; anchor } ->
      List.append
        (vars_of_offset_target target)
        (match anchor with
        | Some anchor -> vars_of_offset_anchor anchor
        | None -> [])
  | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_flex_basis (value : Properties.flex_basis) =
  match value with Var v -> [ V v ] | Calc c -> vars_of_calc c | _ -> []

let vars_of_flex (value : Properties.flex) =
  match value with
  | Var v -> [ V v ]
  | Grow g -> vars_of_flex_factor g
  | Grow_shrink (g, s) -> vars_of_flex_factor g @ vars_of_flex_factor s
  | Full (g, s, b) ->
      vars_of_flex_factor g @ vars_of_flex_factor s @ vars_of_flex_basis b
  | Basis b -> vars_of_flex_basis b
  | _ -> []

let vars_of_font_style (value : Properties.font_style) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_align (value : Properties.text_align) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_visibility (value : Properties.visibility) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_baseline_source (value : Properties.baseline_source) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_alignment_baseline (value : Properties.alignment_baseline) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_baseline_shift (value : Properties.baseline_shift) =
  match value with
  | Var v -> [ V v ]
  | Shift value -> vars_of_length_percentage value
  | _ -> []

let vars_of_text_decoration_line (value : Properties.text_decoration_line) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_decoration_style (value : Properties.text_decoration_style) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_decoration_skip (value : Properties.text_decoration_skip) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_decoration_skip_self (value : Properties.text_decoration_skip_self)
    =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_decoration_skip_box (value : Properties.text_decoration_skip_box) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_decoration_skip_inset
    (value : Properties.text_decoration_skip_inset) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_decoration_skip_spaces
    (value : Properties.text_decoration_skip_spaces) =
  match value with Var v -> [ V v ] | _ -> []

let rec vars_of_text_overflow (value : Properties.text_overflow) =
  match value with
  | Var v -> [ V v ]
  | Pair (first, second) ->
      vars_of_text_overflow first @ vars_of_text_overflow second
  | _ -> []

let vars_of_text_wrap (value : Properties.text_wrap) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_wrap_mode (value : Properties.text_wrap_mode) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_wrap_style (value : Properties.text_wrap_style) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_glyph_orientation_vertical
    (value : Properties.glyph_orientation_vertical) =
  match value with
  | Var v -> [ V v ]
  | Angle angle -> vars_of_angle angle
  | _ -> []

let vars_of_text_box_trim (value : Properties.text_box_trim) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_underline_position (value : Properties.text_underline_position)
    =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_box_edge (value : Properties.text_box_edge) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_box (value : Properties.text_box) =
  match value with
  | Var v -> [ V v ]
  | Box (trim, edge) ->
      Option.value ~default:[] (Option.map vars_of_text_box_trim trim)
      @ Option.value ~default:[] (Option.map vars_of_text_box_edge edge)
  | Normal | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_inline_sizing (value : Properties.inline_sizing) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_line_fit_edge (value : Properties.line_fit_edge) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_interpolate_size (value : Properties.interpolate_size) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_min_intrinsic_sizing (value : Properties.min_intrinsic_sizing) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_ruby_align (value : Properties.ruby_align) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_ruby_merge (value : Properties.ruby_merge) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_ruby_overhang (value : Properties.ruby_overhang) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_ruby_position (value : Properties.ruby_position) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_spacing_trim (value : Properties.text_spacing_trim) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_hyphenate_limit_chars (value : Properties.hyphenate_limit_chars) =
  let slot (item : Properties.hyphenate_limit_chars_item) =
    match item with Auto -> [] | Chars n -> vars_of_number_value n
  in
  match value with
  | Var v -> [ V v ]
  | One a -> slot a
  | Two (a, b) -> slot a @ slot b
  | Three (a, b, c) -> slot a @ slot b @ slot c
  | _ -> []

let vars_of_initial_letter (value : Properties.initial_letter) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_white_space (value : Properties.white_space) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_variant (value : Properties.font_variant) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_variant_alternates (value : Properties.font_variant_alternates)
    =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_white_space_collapse (value : Properties.white_space_collapse) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_word_break (value : Properties.word_break) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_overflow_wrap (value : Properties.overflow_wrap) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_line_break (value : Properties.line_break) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_list_style_position (value : Properties.list_style_position) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_border (value : Properties.border) =
  match value with
  | Var v -> [ V v ]
  | Shorthand { width; style; color } ->
      Option.value ~default:[] (Option.map vars_of_border_width width)
      @ Option.value ~default:[] (Option.map vars_of_border_style style)
      @ Option.value ~default:[] (Option.map vars_of_color color)
  | _ -> []

let vars_of_logical_border_color (value : Properties.logical_border_color) =
  match value with
  | Var v -> [ V v ]
  | Single color -> vars_of_color color
  | Pair (start_, end_) -> vars_of_color start_ @ vars_of_color end_
  | _ -> []

let vars_of_logical_border_width (value : Properties.logical_border_width) =
  match value with
  | Var v -> [ V v ]
  | Single w -> vars_of_border_width w
  | Pair (start_, end_) ->
      vars_of_border_width start_ @ vars_of_border_width end_
  | _ -> []

let vars_of_logical_border_style (value : Properties.logical_border_style) =
  match value with
  | Var v -> [ V v ]
  | Single s -> vars_of_border_style s
  | Pair (start_, end_) ->
      vars_of_border_style start_ @ vars_of_border_style end_
  | _ -> []

let vars_of_outline (value : Properties.outline) =
  match value with
  | Var v -> [ V v ]
  | Shorthand { width; style; color } ->
      Option.value ~default:[] (Option.map vars_of_border_width width)
      @ Option.value ~default:[] (Option.map vars_of_outline_style style)
      @ Option.value ~default:[] (Option.map vars_of_color color)
  | _ -> []

let rec vars_of_background_attachment (value : Properties.background_attachment)
    =
  match value with
  | Var v -> [ V v ]
  | Layers layers -> List.concat_map vars_of_background_attachment layers
  | Scroll | Fixed | Local | Initial | Inherit | Unset | Revert | Revert_layer
    ->
      []

let vars_of_background_box (value : Properties.background_box) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_background_repeat (value : Properties.background_repeat) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_border_collapse (value : Properties.border_collapse) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_box_sizing (value : Properties.box_sizing) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_transform_style (value : Properties.transform_style) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_transform_box (value : Properties.transform_box) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_backface_visibility (value : Properties.backface_visibility) =
  match value with Var v -> [ V v ] | _ -> []

let rec vars_of_animation_direction (value : Properties.animation_direction) =
  match value with
  | Var v -> [ V v ]
  | Directions values -> List.concat_map vars_of_animation_direction values
  | _ -> []

let rec vars_of_animation_fill_mode (value : Properties.animation_fill_mode) =
  match value with
  | Var v -> [ V v ]
  | Fill_modes values -> List.concat_map vars_of_animation_fill_mode values
  | _ -> []

let rec vars_of_animation_play_state (value : Properties.animation_play_state) =
  match value with
  | Var v -> [ V v ]
  | States states -> List.concat_map vars_of_animation_play_state states
  | _ -> []

let rec vars_of_animation_iteration_count
    (value : Properties.animation_iteration_count) =
  match value with
  | Var v -> [ V v ]
  | Counts counts -> List.concat_map vars_of_animation_iteration_count counts
  | Count n -> vars_of_number_value n
  | _ -> []

let vars_of_transition_behavior (value : Properties.transition_behavior) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_overlay (value : Properties.overlay) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_appearance (value : Properties.appearance) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_webkit_appearance (value : Properties.webkit_appearance) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_color_scheme (value : Properties.color_scheme) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_print_color_adjust (value : Properties.print_color_adjust) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_box_decoration_break (value : Properties.box_decoration_break) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_clear (value : Properties.clear) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_float_side (value : Properties.float_side) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_decoration_skip_ink (value : Properties.text_decoration_skip_ink) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_forced_color_adjust (value : Properties.forced_color_adjust) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_pointer_events (value : Properties.pointer_events) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_resize (value : Properties.resize) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_object_fit (value : Properties.object_fit) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_container_type (value : Properties.container_type) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_container_shorthand (value : Properties.container_shorthand) =
  match value with
  | Var v -> [ V v ]
  | Shorthand { ctype = Some ctype; _ } -> vars_of_container_type ctype
  | _ -> []

let vars_of_break_value (value : Properties.break_value) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_break_inside_value (value : Properties.break_inside_value) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_page_break_value (value : Properties.page_break_value) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_page_break_inside (value : Properties.page_break_inside_value) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_page_size_name (value : Properties.page_size_name) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_page_size_orientation (value : Properties.page_size_orientation) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_page_size (value : Properties.page_size) =
  match value with
  | Var v -> [ V v ]
  | Single l -> vars_of_length l
  | Pair (a, b) -> vars_of_length a @ vars_of_length b
  | Named name -> vars_of_page_size_name name
  | Named_oriented (name, orientation) ->
      vars_of_page_size_name name @ vars_of_page_size_orientation orientation
  | Oriented orientation -> vars_of_page_size_orientation orientation
  | _ -> []

let rec vars_of_scroll_snap_align (value : Properties.scroll_snap_align) =
  match value with
  | Var v -> [ V v ]
  | Snap_align_pair (a, b) ->
      vars_of_scroll_snap_align a @ vars_of_scroll_snap_align b
  | _ -> []

let vars_of_scroll_snap_stop (value : Properties.scroll_snap_stop) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_overscroll_behavior (value : Properties.overscroll_behavior) =
  match value with Var v -> [ V v ] | _ -> []

let rec vars_of_svg_paint (value : Properties.svg_paint) =
  match value with
  | Var v -> [ V v ]
  | Color c -> vars_of_color c
  | Url (_, fallback) ->
      Option.value ~default:[] (Option.map vars_of_svg_paint fallback)
  | _ -> []

let vars_of_unicode_bidi (value : Properties.unicode_bidi) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_writing_mode (value : Properties.writing_mode) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_combine_upright (value : Properties.text_combine_upright) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_webkit_font_smoothing (value : Properties.webkit_font_smoothing) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_osx_font_smoothing (value : Properties.moz_osx_font_smoothing) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_webkit_box_orient (value : Properties.webkit_box_orient) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_moz_orient (value : Properties.moz_orient) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_stretch (value : Properties.font_stretch) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_optical_sizing (value : Properties.font_optical_sizing) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_kerning (value : Properties.font_kerning) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_language_override (value : Properties.font_language_override) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_synthesis_style (value : Properties.font_synthesis_style) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_synthesis_weight (value : Properties.font_synthesis_weight) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_synthesis_small_caps (value : Properties.font_synthesis_small_caps)
    =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_synthesis_position (value : Properties.font_synthesis_position)
    =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_variant_ligatures (value : Properties.font_variant_ligatures) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_variant_caps (value : Properties.font_variant_caps) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_variant_position (value : Properties.font_variant_position) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_east_asian (value : Properties.font_variant_east_asian) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_size_adjust (value : Properties.font_size_adjust) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_variant_emoji (value : Properties.font_variant_emoji) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_size_adjust (value : Properties.text_size_adjust) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_touch_action (value : Properties.touch_action) =
  match value with
  | Var v -> [ V v ]
  | Vars vars -> List.map (fun v -> V v) vars
  | _ -> []

let vars_of_clip (value : Properties.clip) =
  match value with
  | Var v -> [ V v ]
  | Clip_rect (a, b, c, d) ->
      vars_of_length a @ vars_of_length b @ vars_of_length c @ vars_of_length d
  | _ -> []

let vars_of_perspective_origin (value : Properties.perspective_origin) =
  vars_of_position_value value

let vars_of_object_view_box (value : Properties.object_view_box) =
  match value with
  | Var v -> [ V v ]
  | Inset (a, b, c, d) ->
      vars_of_length a
      @ Option.value ~default:[] (Option.map vars_of_length b)
      @ Option.value ~default:[] (Option.map vars_of_length c)
      @ Option.value ~default:[] (Option.map vars_of_length d)
  | Xywh { x; y; width; height; rounded }
  | Rect { top = x; right = y; bottom = width; left = height; rounded } ->
      vars_of_length_percentage x
      @ vars_of_length_percentage y
      @ vars_of_length_percentage width
      @ vars_of_length_percentage height
      @ Option.value ~default:[] (Option.map vars_of_border_radius rounded)
  | None | Inherit | Initial | Unset | Revert | Revert_layer -> []

let vars_of_mask_box (value : Properties.mask_box) =
  match value with Var v -> [ V v ] | _ -> []

let rec vars_of_webkit_mask_composite (value : Properties.webkit_mask_composite)
    =
  match value with
  | Var v -> [ V v ]
  | Composites composites ->
      List.concat_map vars_of_webkit_mask_composite composites
  | _ -> []

let vars_of_mask_composite (value : Properties.mask_composite) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_mask_source_type (value : Properties.webkit_mask_source_type) =
  match value with Var v -> [ V v ] | _ -> []

let rec vars_of_mask_mode (value : Properties.mask_mode) =
  match value with
  | Var v -> [ V v ]
  | Modes modes -> List.concat_map vars_of_mask_mode modes
  | Alpha | Luminance | Match_source | Initial | Inherit | Unset | Revert
  | Revert_layer ->
      []

let vars_of_mask_layer (layer : Properties.mask_layer) : any_var list =
  Option.fold ~none:[] ~some:vars_of_background_image layer.image
  @ Option.fold ~none:[] ~some:vars_of_position_value layer.position
  @ Option.fold ~none:[] ~some:vars_of_background_size layer.size
  @ Option.fold ~none:[] ~some:vars_of_background_repeat layer.repeat
  @ Option.fold ~none:[] ~some:vars_of_mask_box layer.origin
  @ Option.fold ~none:[] ~some:vars_of_mask_box layer.clip
  @ Option.fold ~none:[] ~some:vars_of_mask_mode layer.mode
  @ Option.fold ~none:[] ~some:vars_of_mask_composite layer.composite

let vars_of_mask (value : Properties.mask) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Layer layer -> vars_of_mask_layer layer
  | Layers layers -> List.concat_map vars_of_mask_layer layers
  | _ -> []

let vars_of_border_image (value : Properties.border_image) : any_var list =
  Option.fold ~none:[] ~some:vars_of_background_image value.source

let vars_of_user_select (value : Properties.user_select) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_timeline_axis (value : Properties.timeline_axis) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_timeline_name (value : Properties.timeline_name) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_timeline_shorthand (value : Properties.timeline_shorthand) =
  match value with
  | Var v -> [ V v ]
  | (Timelines items : Properties.timeline_shorthand) ->
      List.concat_map
        (fun ({ Properties.axis; _ } : Properties.timeline_shorthand_item) ->
          Option.fold ~none:[] ~some:vars_of_timeline_axis axis)
        items
  | _ -> []

let vars_of_timeline_inset_item (value : Properties.timeline_inset_item) =
  match value with Auto -> [] | Length lp -> vars_of_length_percentage lp

let rec vars_of_timeline_inset (value : Properties.timeline_inset) =
  match value with
  | Var v -> [ V v ]
  | Inset (first, second) ->
      vars_of_timeline_inset_item first
      @ Option.value ~default:[] (Option.map vars_of_timeline_inset_item second)
  | Insets insets -> List.concat_map vars_of_timeline_inset insets
  | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_view_timeline_shorthand (value : Properties.view_timeline_shorthand)
    =
  match value with
  | Var v -> [ V v ]
  | Timelines items ->
      List.concat_map
        (fun { Properties.axis; inset; _ } ->
          Option.fold ~none:[] ~some:vars_of_timeline_axis axis
          @ Option.fold ~none:[] ~some:vars_of_timeline_inset inset)
        items
  | _ -> []

let vars_of_direction (value : Properties.direction) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_fill_rule (value : Properties.fill_rule) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_stroke_linecap (value : Properties.stroke_linecap) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_stroke_linejoin (value : Properties.stroke_linejoin) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_stroke_miterlimit (value : Properties.stroke_miterlimit) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_stroke_width (value : Properties.stroke_width) =
  match value with
  | Var v -> [ V v ]
  | Length lp -> vars_of_length_percentage lp
  | _ -> []

let vars_of_dash_length (value : Properties.dash_length) =
  match value with
  | Number n -> vars_of_number_value n
  | Length lp -> vars_of_length_percentage lp

let vars_of_stroke_dashoffset (value : Properties.stroke_dashoffset) =
  match value with
  | Var v -> [ V v ]
  | Dash d -> vars_of_dash_length d
  | _ -> []

let vars_of_stroke_dasharray (value : Properties.stroke_dasharray) =
  match value with
  | Var v -> [ V v ]
  | Dashes ds -> List.concat_map vars_of_dash_length ds
  | _ -> []

let vars_of_paint_order (value : Properties.paint_order) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_vector_effect (value : Properties.vector_effect) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_css_wide (value : Properties.css_wide) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_scroll_behavior (value : Properties.scroll_behavior) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_caption_side (value : Properties.caption_side) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_dominant_baseline (value : Properties.dominant_baseline) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_field_sizing (value : Properties.field_sizing) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_grid_template_areas (value : Properties.grid_template_areas) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_hyphens (value : Properties.hyphens) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_initial_letter_align (value : Properties.initial_letter_align) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_initial_letter_wrap (value : Properties.initial_letter_wrap) =
  match value with
  | Var v -> [ V v ]
  | Length lp -> vars_of_length_percentage lp
  | _ -> []

let vars_of_isolation (value : Properties.isolation) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_mask_type (value : Properties.mask_type) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_order (value : Properties.order) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_table_layout (value : Properties.table_layout) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_emphasis_skip (value : Properties.text_emphasis_skip) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_webkit_line_clamp (value : Properties.webkit_line_clamp) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_z_index (value : Properties.z_index) =
  match value with Var v -> [ V v ] | _ -> []

(** {1 Advanced variable extraction} *)

let vars_of_font (value : Properties.font) : any_var list =
  let opt f = function Some x -> f x | None -> [] in
  match value with
  | Var v -> [ V v ]
  | Shorthand { style; variant = _; weight; stretch; size; line_height; family }
    ->
      opt vars_of_font_style style
      @ opt vars_of_font_weight weight
      @ opt vars_of_font_stretch stretch
      @ vars_of_font_size size
      @ opt vars_of_line_height line_height
      @ vars_of_font_family family
  | _ -> []

let vars_of_grid_area (value : Properties.grid_area) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Lines { row_start; column_start; row_end; column_end } ->
      vars_of_grid_line row_start
      @ vars_of_grid_line column_start
      @ vars_of_grid_line row_end
      @ vars_of_grid_line column_end
  | _ -> []

let vars_of_list_style (value : Properties.list_style) : any_var list =
  let opt f = function Some x -> f x | None -> [] in
  match value with
  | Var v -> [ V v ]
  | Shorthand { type_; position; image } ->
      opt vars_of_list_style_type type_
      @ opt vars_of_list_style_position position
      @ opt vars_of_list_style_image image
  | _ -> []

(* CSS Variables L1 sec. 3: a custom property's value can itself reference other
   custom properties. A [Typed] value embeds real [var] handles, so recurse via
   the kind; a raw [Tokens] value carries refs as [var()] functions in the
   stream, recovered structurally by {!vars_of_token_stream}. *)
let vars_of_kind : type a. a kind -> a -> any_var list =
 fun kind value ->
  match kind with
  | Length -> vars_of_length value
  | Color -> vars_of_color value
  | Rgb -> vars_of_rgb value
  | Number -> vars_of_number_value value
  | Int -> []
  | Float -> []
  | Percentage -> vars_of_percentage value
  | Length_percentage -> vars_of_length_percentage value
  | Number_percentage -> []
  | Opacity -> []
  | Value -> []
  | Duration -> vars_of_duration value
  | Aspect_ratio -> vars_of_aspect_ratio value
  | Border_style -> vars_of_border_style value
  | Outline_style -> []
  | Border -> vars_of_border value
  | Font_weight -> vars_of_font_weight value
  | Font_size -> vars_of_font_size value
  | Line_height -> vars_of_line_height value
  | Font_family -> vars_of_font_family value
  | Font_feature_settings -> vars_of_font_feature_settings value
  | Font_variation_settings -> vars_of_font_variation_settings value
  | Numeric -> vars_of_font_variant_numeric value
  | Font_variant_numeric_token -> []
  | Blend_mode -> vars_of_blend_mode value
  | Scroll_snap_strictness -> vars_of_scroll_snap_strictness value
  | Angle -> vars_of_angle value
  | Rotate -> vars_of_rotate_value value
  | Scale -> vars_of_scale value
  | Shadow -> vars_of_shadow value
  | Content -> vars_of_content value
  | Gradient_stop -> vars_of_gradient_stop value
  | Gradient_direction -> vars_of_gradient_direction value
  | Gradient_position -> vars_of_gradient_position value
  | Radial_shape -> vars_of_radial_shape value
  | Radial_size -> vars_of_radial_size value
  | Position_value -> vars_of_position_value value
  | Animation -> vars_of_animation value
  | Timing_function -> []
  | Transform -> vars_of_transform value
  | Touch_action -> []
  | Transition_property_value -> vars_of_transition_property_value value
  | Background_image -> vars_of_background_image value
  | Z_index -> []
  | Filter -> vars_of_filter value
  | Font_src -> []

(* The structural scans return each name with its leading [--]; [Values.var_ref]
   re-adds it, so strip the prefix before rebuilding the handle. *)
let vars_of_ref_names names : any_var list =
  List.rev_map
    (fun name ->
      let bare = Custom_property_name.strip_prefix name in
      V (Values.var_ref bare))
    names

(* Names referenced via real [var()] functions in an opaque token stream. *)
let vars_of_token_stream (components : Component.t list) : any_var list =
  vars_of_ref_names (var_refs_in_components [] components)

(* Names referenced by a property whose value the reader keeps as raw text. The
   text never went through a typed parser, so a [var()] in it is recoverable
   only by re-tokenising, exactly as for an opaque custom-property stream. *)
let vars_of_value_string (value : string) : any_var list =
  vars_of_ref_names (var_refs_in_value_string value)

let vars_of_custom_property_value : custom_property_value -> any_var list =
  function
  | Typed { kind; value } -> vars_of_kind kind value
  | Tokens components -> vars_of_token_stream components

(* Extract variables from CSS property values using type-specific extraction
   functions *)
let vars_of_property : type a. a property -> a -> any_var list =
 fun prop value ->
  match (prop, value) with
  | Width, value -> vars_of_length_percentage value
  | Height, value -> vars_of_length_percentage value
  | Min_width, value -> vars_of_length_percentage value
  | Min_height, value -> vars_of_length_percentage value
  | Max_width, value -> vars_of_length_percentage value
  | Max_height, value -> vars_of_length_percentage value
  | Padding, values -> vars_of_length_list values
  | Padding_top, value -> vars_of_length value
  | Padding_right, value -> vars_of_length value
  | Padding_bottom, value -> vars_of_length value
  | Padding_left, value -> vars_of_length value
  | Padding_inline, values -> vars_of_length_list values
  | Padding_inline_start, value -> vars_of_length value
  | Padding_inline_end, value -> vars_of_length value
  | Padding_block, values -> vars_of_length_list values
  | Padding_block_start, value -> vars_of_length value
  | Padding_block_end, value -> vars_of_length value
  | Margin, values -> vars_of_length_list values
  | Margin_top, value -> vars_of_length value
  | Margin_right, value -> vars_of_length value
  | Margin_bottom, value -> vars_of_length value
  | Margin_left, value -> vars_of_length value
  | Margin_inline, value -> vars_of_length_list value
  | Margin_inline_start, value -> vars_of_length value
  | Margin_inline_end, value -> vars_of_length value
  | Margin_block, value -> vars_of_length_list value
  | Margin_block_start, value -> vars_of_length value
  | Margin_block_end, value -> vars_of_length value
  | Top, value -> vars_of_length_list value
  | Right, value -> vars_of_length_list value
  | Bottom, value -> vars_of_length_list value
  | Left, value -> vars_of_length_list value
  | Font_size, value -> vars_of_font_size value
  | Letter_spacing, value -> vars_of_length value
  | Line_height, value -> vars_of_line_height value
  | Border_width, value -> vars_of_border_width_list value
  | Border_top_width, value -> vars_of_border_width value
  | Border_right_width, value -> vars_of_border_width value
  | Border_bottom_width, value -> vars_of_border_width value
  | Border_left_width, value -> vars_of_border_width value
  | Border_inline_start_width, value -> vars_of_border_width value
  | Border_inline_end_width, value -> vars_of_border_width value
  | Border_block_start_width, value -> vars_of_border_width value
  | Border_block_end_width, value -> vars_of_border_width value
  | Outline_width, value -> vars_of_border_width value
  | Column_gap, value -> vars_of_length value
  | Row_gap, value -> vars_of_length value
  | Gap, Lengths { row_gap; column_gap } ->
      vars_of_optional_length row_gap @ vars_of_optional_length column_gap
  | Gap, Var v -> [ V v ]
  | Gap, (Inherit | Initial | Unset | Revert | Revert_layer) -> []
  (* Color properties *)
  | Background_color, value -> vars_of_color value
  | Color, value -> vars_of_color value
  | Border_color, value -> List.concat_map vars_of_color value
  | Border_top_color, value -> vars_of_color value
  | Border_right_color, value -> vars_of_color value
  | Border_bottom_color, value -> vars_of_color value
  | Border_left_color, value -> vars_of_color value
  | Border_inline_start_color, value -> vars_of_color value
  | Border_inline_end_color, value -> vars_of_color value
  | Border_block_start_color, value -> vars_of_color value
  | Border_block_end_color, value -> vars_of_color value
  | Border_inline_color, value -> vars_of_logical_border_color value
  | Border_block_color, value -> vars_of_logical_border_color value
  | Border_inline_width, value -> vars_of_logical_border_width value
  | Border_block_width, value -> vars_of_logical_border_width value
  | Border_inline_style, value -> vars_of_logical_border_style value
  | Border_block_style, value -> vars_of_logical_border_style value
  | Border_start_start_radius, value -> vars_of_length_list value
  | Border_start_end_radius, value -> vars_of_length_list value
  | Border_end_start_radius, value -> vars_of_length_list value
  | Border_end_end_radius, value -> vars_of_length_list value
  | Text_decoration_color, value -> vars_of_color value
  | Webkit_text_decoration_color, value -> vars_of_color value
  | Webkit_tap_highlight_color, value -> vars_of_color value
  | Outline_color, value -> vars_of_color value
  (* Border radius *)
  | Border_radius, value -> vars_of_border_radius value
  | Border_top_left_radius, value -> vars_of_length_list value
  | Border_top_right_radius, value -> vars_of_length_list value
  | Border_bottom_left_radius, value -> vars_of_length_list value
  | Border_bottom_right_radius, value -> vars_of_length_list value
  | Border_image, value -> vars_of_border_image value
  (* Outline offset *)
  | Outline_offset, value -> vars_of_length value
  | Flex_basis, value -> vars_of_flex_basis value
  (* Text and font properties *)
  | Text_indent, value -> vars_of_text_indent_value value
  | Text_decoration_thickness, value -> vars_of_length value
  | Word_spacing, value -> vars_of_length value
  (* Other length properties *)
  | Border_spacing, value -> vars_of_border_spacing value
  | Perspective, value -> vars_of_length value
  | Stroke_width, value -> vars_of_stroke_width value
  | Scroll_margin, value -> List.concat_map vars_of_length value
  | Scroll_margin_top, value -> vars_of_length value
  | Scroll_margin_right, value -> vars_of_length value
  | Scroll_margin_bottom, value -> vars_of_length value
  | Scroll_margin_left, value -> vars_of_length value
  | Scroll_padding, value -> List.concat_map vars_of_length value
  | Scroll_padding_top, value -> vars_of_length value
  | Scroll_padding_right, value -> vars_of_length value
  | Scroll_padding_bottom, value -> vars_of_length value
  | Scroll_padding_left, value -> vars_of_length value
  (* Color properties *)
  | Accent_color, value -> vars_of_color value
  | Caret_color, value -> vars_of_color value
  | Stop_color, value -> vars_of_color value
  | Flood_color, value -> vars_of_color value
  | Lighting_color, value -> vars_of_color value
  (* Rotate property *)
  | Rotate, value -> vars_of_rotate_value value
  (* Duration properties *)
  | Transition_duration, value -> vars_of_duration value
  | Transition_delay, value -> vars_of_duration value
  | Animation_duration, value -> vars_of_duration value
  | Animation_delay, value -> vars_of_duration value
  | Animation_name, value -> vars_of_animation_name value
  (* Transform properties *)
  | Transform, value -> vars_of_transform_list value
  | Webkit_transform, value -> vars_of_transform_list value
  | Moz_transform, value -> vars_of_transform_list value
  | Ms_transform, value -> vars_of_transform_list value
  | O_transform, value -> vars_of_transform_list value
  | Translate, value -> vars_of_translate_value value
  (* Border style properties *)
  | Border_style, value -> List.concat_map vars_of_border_style value
  | Border_top_style, value -> vars_of_border_style value
  | Border_right_style, value -> vars_of_border_style value
  | Border_bottom_style, value -> vars_of_border_style value
  | Border_left_style, value -> vars_of_border_style value
  | Border_inline_start_style, value -> vars_of_border_style value
  | Border_inline_end_style, value -> vars_of_border_style value
  | Border_block_start_style, value -> vars_of_border_style value
  | Border_block_end_style, value -> vars_of_border_style value
  (* Font properties *)
  | Font_weight, value -> vars_of_font_weight value
  | Font_family, value -> vars_of_font_family value
  | Font_feature_settings, value -> vars_of_font_feature_settings value
  | Font_size_adjust, value -> vars_of_font_size_adjust value
  | Font_stretch, value -> vars_of_font_stretch value
  | Font_optical_sizing, value -> vars_of_font_optical_sizing value
  | Font_kerning, value -> vars_of_font_kerning value
  | Font_language_override, value -> vars_of_font_language_override value
  | Font_synthesis_style, value -> vars_of_font_synthesis_style value
  | Font_synthesis_weight, value -> vars_of_font_synthesis_weight value
  | Font_synthesis_small_caps, value -> vars_of_synthesis_small_caps value
  | Font_synthesis_position, value -> vars_of_font_synthesis_position value
  | Font_variant_ligatures, value -> vars_of_font_variant_ligatures value
  | Caps, value -> vars_of_font_variant_caps value
  | Font_variant_emoji, value -> vars_of_font_variant_emoji value
  | Font_variation_settings, value -> vars_of_font_variation_settings value
  | Numeric, value -> vars_of_font_variant_numeric value
  | Font_variant_position, value -> vars_of_font_variant_position value
  | East_asian, value -> vars_of_east_asian value
  (* Text properties *)
  | Text_decoration, value -> vars_of_text_decoration value
  | Text_emphasis, value -> vars_of_text_emphasis value
  | Text_emphasis_position, value -> vars_of_text_emphasis_position value
  | Text_emphasis_style, value -> vars_of_text_emphasis_style value
  | Text_orientation, value -> vars_of_text_orientation value
  | Webkit_text_decoration, value -> vars_of_text_decoration value
  | Text_transform, value -> vars_of_text_transform value
  (* Content and visibility *)
  | Content, value -> vars_of_content value
  | Counter_reset, value -> vars_of_counter_set value
  | Counter_increment, value -> vars_of_counter_set value
  | Content_visibility, value -> vars_of_content_visibility value
  (* Blend mode properties *)
  | Mix_blend_mode, value -> vars_of_blend_mode value
  | Background_blend_mode, values -> List.concat_map vars_of_blend_mode values
  (* Filter properties *)
  | Filter, value -> vars_of_filter value
  | Backdrop_filter, value -> vars_of_filter value
  | Webkit_backdrop_filter, value -> vars_of_filter value
  | Webkit_filter, value -> vars_of_filter value
  | Ms_filter, value -> vars_of_filter value
  (* Transition properties *)
  | Transition, values -> List.concat_map vars_of_transition values
  | Webkit_transition, values -> List.concat_map vars_of_transition values
  | O_transition, values -> List.concat_map vars_of_transition values
  (* Animation properties *)
  | Animation, values -> List.concat_map vars_of_animation values
  (* Background properties *)
  | Background, values -> List.concat_map vars_of_background values
  (* Shadow properties *)
  | Box_shadow, value -> vars_of_shadow value
  (* Scale properties *)
  | Scale, value -> vars_of_scale value
  (* Scroll snap properties *)
  | Scroll_snap_type, value -> vars_of_scroll_snap_type value
  (* Timing function properties *)
  | Animation_timing_function, value -> vars_of_timing_function value
  | Transition_timing_function, value -> vars_of_timing_function value
  (* Quotes property *)
  | Quotes, value -> vars_of_quotes value
  (* Aspect ratio *)
  | Aspect_ratio, value -> vars_of_aspect_ratio value
  (* Background image/size *)
  | Background_image, values -> List.concat_map vars_of_background_image values
  | Background_size, value -> vars_of_background_size value
  (* Columns *)
  | Columns, value -> vars_of_columns_value value
  | Column_width, value -> vars_of_column_width value
  | Column_height, value -> vars_of_column_height value
  | Column_wrap, value -> vars_of_column_wrap value
  | Column_count, value -> vars_of_column_count value
  | Column_rule, value -> vars_of_border value
  | Column_rule_color, value -> List.concat_map vars_of_color value
  | Column_rule_width, value -> List.concat_map vars_of_border_width value
  | Column_rule_style, value -> List.concat_map vars_of_border_style value
  | Column_span, value -> vars_of_column_span value
  (* Contain *)
  | Contain, value -> vars_of_contain value
  (* Cursor *)
  | Cursor, value -> vars_of_cursor value
  | Interactivity, value -> vars_of_interactivity value
  | Caret_animation, value -> vars_of_caret_animation value
  | Caret_shape, value -> vars_of_caret_shape value
  | Caret, value -> vars_of_caret value
  | Interest_delay, value -> vars_of_interest_delay value
  | Interest_delay_start, value -> vars_of_interest_delay value
  | Interest_delay_end, value -> vars_of_interest_delay value
  | Nav_up, value -> vars_of_nav value
  | Nav_right, value -> vars_of_nav value
  | Nav_down, value -> vars_of_nav value
  | Nav_left, value -> vars_of_nav value
  (* Grid template *)
  | Grid_auto_columns, value -> vars_of_grid_template value
  | Grid_auto_rows, value -> vars_of_grid_template value
  | Grid, value -> vars_of_grid_template value
  | Grid_template, value -> vars_of_grid_template value
  | Grid_template_columns, value -> vars_of_grid_template value
  | Grid_template_rows, value -> vars_of_grid_template value
  (* Grid line *)
  | Grid_column_end, value -> vars_of_grid_line value
  | Grid_column_start, value -> vars_of_grid_line value
  | Grid_column, value -> vars_of_grid_line_pair value
  | Grid_row, value -> vars_of_grid_line_pair value
  | Grid_row_end, value -> vars_of_grid_line value
  | Grid_row_start, value -> vars_of_grid_line value
  (* List style *)
  | List_style_image, value -> vars_of_list_style_image value
  | List_style_type, value -> vars_of_list_style_type value
  (* Mask image/size *)
  | Border_image_source, value -> vars_of_background_image value
  | Border_image_repeat, value -> vars_of_border_image_repeat value
  | Border_image_width, value -> vars_of_border_image_width value
  | Border_image_outset, value -> vars_of_border_image_outset value
  | Mask_image, value -> vars_of_background_image value
  | Mask_size, value -> vars_of_background_size value
  | Webkit_mask_image, value -> vars_of_background_image value
  | Webkit_mask_size, value -> vars_of_background_size value
  (* Object position *)
  | Object_position, value -> vars_of_position_value value
  (* Outline style *)
  | Outline_style, value -> vars_of_outline_style value
  (* Text shadow *)
  | Text_shadow, value -> vars_of_text_shadow_list value
  (* Transform origin *)
  | Transform_origin, value -> vars_of_transform_origin value
  (* Vertical align *)
  | Vertical_align, value -> vars_of_vertical_align value
  (* Will change *)
  | Will_change, value -> vars_of_will_change value
  (* Opacity (typed math) *)
  | Opacity, value -> vars_of_opacity value
  | Fill_opacity, value -> vars_of_opacity value
  | Stroke_opacity, value -> vars_of_opacity value
  | Stop_opacity, value -> vars_of_opacity value
  | Flood_opacity, value -> vars_of_opacity value
  | Tab_size, value -> vars_of_tab_size value
  | Zoom, value -> vars_of_zoom value
  | Align_content, value -> vars_of_align_content value
  | Align_items, value -> vars_of_align_items value
  | Align_self, value -> vars_of_align_self value
  | Animation_direction, value -> vars_of_animation_direction value
  | Animation_fill_mode, value -> vars_of_animation_fill_mode value
  | Animation_iteration_count, value -> vars_of_animation_iteration_count value
  | Animation_play_state, value -> vars_of_animation_play_state value
  | Animation_composition, value -> (
      match value with Var v -> [ V v ] | _ -> [])
  | Appearance, value -> vars_of_appearance value
  | Backface_visibility, value -> vars_of_backface_visibility value
  | Background_attachment, value -> vars_of_background_attachment value
  | Background_clip, value -> vars_of_background_box value
  | Background_origin, value -> vars_of_background_box value
  | Background_repeat, value -> vars_of_background_repeat value
  | Border, value -> vars_of_border value
  | Border_block, value -> vars_of_border value
  | Border_top, value -> vars_of_border value
  | Border_right, value -> vars_of_border value
  | Border_bottom, value -> vars_of_border value
  | Border_left, value -> vars_of_border value
  | Border_collapse, value -> vars_of_border_collapse value
  | Box_sizing, value -> vars_of_box_sizing value
  | Webkit_box_sizing, value -> vars_of_box_sizing value
  | Moz_box_sizing, value -> vars_of_box_sizing value
  | Box_decoration_break, value -> vars_of_box_decoration_break value
  | Break_after, value -> vars_of_break_value value
  | Break_before, value -> vars_of_break_value value
  | Break_inside, value -> vars_of_break_inside_value value
  | Page_break_after, value -> vars_of_page_break_value value
  | Page_break_before, value -> vars_of_page_break_value value
  | Page_break_inside, value -> vars_of_page_break_inside value
  | Caption_side, value -> vars_of_caption_side value
  | Clear, value -> vars_of_clear value
  | Clip, value -> vars_of_clip value
  | Clip_path, value -> vars_of_clip_path value
  | Color_scheme, value -> vars_of_color_scheme value
  | Container_type, value -> vars_of_container_type value
  | Container, value -> vars_of_container_shorthand value
  | Container_name, value -> vars_of_container_name value
  | Anchor_name, value -> vars_of_anchor_name value
  | Position_anchor, value -> vars_of_position_anchor value
  | Position_try_fallbacks, value -> vars_of_position_try_fallbacks value
  | Position_try_order, value -> vars_of_position_try_order value
  | Position_try, value -> vars_of_position_try value
  | Position_visibility, value -> vars_of_position_visibility value
  | Position_area, value -> vars_of_position_area value
  | Shape_image_threshold, value -> vars_of_shape_image_threshold value
  | Overflow_clip_margin, value -> vars_of_overflow_clip_margin value
  | Overflow_anchor, value -> vars_of_overflow_anchor value
  | Scrollbar_width, value -> vars_of_scrollbar_width value
  | Scrollbar_color, value -> vars_of_scrollbar_color value
  | Scrollbar_gutter, value -> vars_of_scrollbar_gutter value
  | Font_palette, value -> vars_of_font_palette value
  | Font_synthesis, value -> vars_of_font_synthesis value
  | Animation_timeline, value -> vars_of_animation_timeline value
  | Animation_range, value -> vars_of_animation_range value
  | Animation_range_start, value -> vars_of_animation_range_item value
  | Animation_range_end, value -> vars_of_animation_range_item value
  | View_transition_name, value -> vars_of_view_transition_name value
  | View_transition_class, value -> vars_of_view_transition_class value
  | Image_orientation, value -> vars_of_image_orientation value
  | Image_rendering, value -> vars_of_image_rendering value
  | Image_resolution, value -> vars_of_image_resolution value
  | Contain_intrinsic_size, value -> vars_of_contain_intrinsic_size value
  | Contain_intrinsic_width, value -> vars_of_contain_intrinsic_longhand value
  | Contain_intrinsic_height, value -> vars_of_contain_intrinsic_longhand value
  | Contain_intrinsic_block_size, value ->
      vars_of_contain_intrinsic_longhand value
  | Contain_intrinsic_inline_size, value ->
      vars_of_contain_intrinsic_longhand value
  | Margin_trim, value -> vars_of_margin_trim value
  | Offset_path, value -> vars_of_offset_path value
  | Offset, value -> vars_of_offset value
  | Offset_anchor, value -> vars_of_offset_anchor value
  | Offset_position, value -> vars_of_offset_position value
  | Offset_rotate, value -> vars_of_offset_rotate value
  | All, value -> vars_of_css_wide value
  | Direction, value -> vars_of_direction value
  | Fill_rule, value -> vars_of_fill_rule value
  | Clip_rule, value -> vars_of_fill_rule value
  | Stroke_linecap, value -> vars_of_stroke_linecap value
  | Stroke_linejoin, value -> vars_of_stroke_linejoin value
  | Stroke_miterlimit, value -> vars_of_stroke_miterlimit value
  | Stroke_dashoffset, value -> vars_of_stroke_dashoffset value
  | Stroke_dasharray, value -> vars_of_stroke_dasharray value
  | Paint_order, value -> vars_of_paint_order value
  | Vector_effect, value -> vars_of_vector_effect value
  | Display, value -> vars_of_display value
  | Fill, value -> vars_of_svg_paint value
  | Flex, value -> vars_of_flex value
  | Flex_direction, value -> vars_of_flex_direction value
  | Flex_wrap, value -> vars_of_flex_wrap value
  | Flex_flow, value -> vars_of_flex_flow value
  | Flex_grow, value -> vars_of_flex_factor value
  | Flex_shrink, value -> vars_of_flex_factor value
  | Float, value -> vars_of_float_side value
  | Font_style, value -> vars_of_font_style value
  | Forced_color_adjust, value -> vars_of_forced_color_adjust value
  | Field_sizing, value -> vars_of_field_sizing value
  | Grid_auto_flow, value -> vars_of_grid_auto_flow value
  | Grid_template_areas, value -> vars_of_grid_template_areas value
  | Justify_content, value -> vars_of_justify_content value
  | Justify_items, value -> vars_of_justify_items value
  | Justify_self, value -> vars_of_justify_self value
  | List_style_position, value -> vars_of_list_style_position value
  | Mask_clip, value -> vars_of_mask_box value
  | Mask_composite, value -> vars_of_mask_composite value
  | Mask_type, value -> vars_of_mask_type value
  | Mask, value -> vars_of_mask value
  | Mask_border, value -> vars_of_border_image value
  | Mask_mode, value -> vars_of_mask_mode value
  | Mask_origin, value -> vars_of_mask_box value
  | Mask_repeat, value -> vars_of_background_repeat value
  | Moz_appearance, value -> vars_of_appearance value
  | Moz_orient, value -> vars_of_moz_orient value
  | Moz_osx_font_smoothing, value -> vars_of_osx_font_smoothing value
  | Object_fit, value -> vars_of_object_fit value
  | Object_view_box, value -> vars_of_object_view_box value
  | Order, value -> vars_of_order value
  | Outline, value -> vars_of_outline value
  | Overflow, value -> vars_of_overflow value
  | Overflow_wrap, value -> vars_of_overflow_wrap value
  | Line_break, value -> vars_of_line_break value
  | Overflow_x, value -> vars_of_overflow value
  | Overflow_y, value -> vars_of_overflow value
  | Overflow_block, value -> vars_of_overflow value
  | Overflow_inline, value -> vars_of_overflow value
  | Overscroll_behavior, values ->
      List.concat_map vars_of_overscroll_behavior values
  | Overscroll_behavior_x, value -> vars_of_overscroll_behavior value
  | Overscroll_behavior_y, value -> vars_of_overscroll_behavior value
  | Overscroll_behavior_block, value -> vars_of_overscroll_behavior value
  | Overscroll_behavior_inline, value -> vars_of_overscroll_behavior value
  | Page_size, value -> vars_of_page_size value
  | Perspective_origin, value -> vars_of_perspective_origin value
  | Place_content, value -> vars_of_place_content value
  | Place_items, value -> vars_of_place_items value
  | Pointer_events, value -> vars_of_pointer_events value
  | Position, value -> vars_of_position value
  | Print_color_adjust, value -> vars_of_print_color_adjust value
  | Resize, value -> vars_of_resize value
  | Scroll_snap_align, value -> vars_of_scroll_snap_align value
  | Scroll_snap_stop, value -> vars_of_scroll_snap_stop value
  | Scroll_behavior, value -> vars_of_scroll_behavior value
  | Scroll_timeline, value -> vars_of_timeline_shorthand value
  | Scroll_timeline_name, value -> vars_of_timeline_name value
  | Scroll_timeline_axis, value -> vars_of_timeline_axis value
  | Stroke, value -> vars_of_svg_paint value
  | Source, _ -> []
  | Table_layout, value -> vars_of_table_layout value
  | Text_align, value -> vars_of_text_align value
  | Text_decoration_line, values ->
      List.concat_map vars_of_text_decoration_line values
  | Text_decoration_skip, value -> vars_of_text_decoration_skip value
  | Text_decoration_skip_self, value -> vars_of_decoration_skip_self value
  | Text_decoration_skip_box, value -> vars_of_decoration_skip_box value
  | Text_decoration_skip_inset, value -> vars_of_decoration_skip_inset value
  | Text_decoration_skip_spaces, value -> vars_of_decoration_skip_spaces value
  | Text_decoration_skip_ink, value -> vars_of_decoration_skip_ink value
  | Text_decoration_style, value -> vars_of_text_decoration_style value
  | Text_overflow, value -> vars_of_text_overflow value
  | Text_size_adjust, value -> vars_of_text_size_adjust value
  | Text_wrap, value -> vars_of_text_wrap value
  | Text_wrap_mode, value -> vars_of_text_wrap_mode value
  | Text_wrap_style, value -> vars_of_text_wrap_style value
  | Text_box_trim, value -> vars_of_text_box_trim value
  | Text_underline_position, value -> vars_of_text_underline_position value
  | Text_box_edge, value -> vars_of_text_box_edge value
  | Text_box, value -> vars_of_text_box value
  | Text_emphasis_skip, value -> vars_of_text_emphasis_skip value
  | Inline_sizing, value -> vars_of_inline_sizing value
  | Line_fit_edge, value -> vars_of_line_fit_edge value
  | Interpolate_size, value -> vars_of_interpolate_size value
  | Min_intrinsic_sizing, value -> vars_of_min_intrinsic_sizing value
  | Ruby_align, value -> vars_of_ruby_align value
  | Ruby_merge, value -> vars_of_ruby_merge value
  | Ruby_overhang, value -> vars_of_ruby_overhang value
  | Ruby_position, value -> vars_of_ruby_position value
  | Glyph_orientation_vertical, value ->
      vars_of_glyph_orientation_vertical value
  | Text_spacing_trim, value -> vars_of_text_spacing_trim value
  | Hyphenate_limit_chars, value -> vars_of_hyphenate_limit_chars value
  | Initial_letter, value -> vars_of_initial_letter value
  | Initial_letter_align, value -> vars_of_initial_letter_align value
  | Initial_letter_wrap, value -> vars_of_initial_letter_wrap value
  | Dominant_baseline, value -> vars_of_dominant_baseline value
  | Hyphens, value -> vars_of_hyphens value
  | Webkit_hyphens, value -> vars_of_hyphens value
  | Isolation, value -> vars_of_isolation value
  | Touch_action, value -> vars_of_touch_action value
  | Transform_box, value -> vars_of_transform_box value
  | Transform_style, value -> vars_of_transform_style value
  | Transition_behavior, value -> vars_of_transition_behavior value
  | Overlay, value -> vars_of_overlay value
  | Unicode_bidi, value -> vars_of_unicode_bidi value
  | User_select, value -> vars_of_user_select value
  | Visibility, value -> vars_of_visibility value
  | Baseline_source, value -> vars_of_baseline_source value
  | Alignment_baseline, value -> vars_of_alignment_baseline value
  | Baseline_shift, value -> vars_of_baseline_shift value
  | View_timeline_name, value -> vars_of_timeline_name value
  | View_timeline_axis, value -> vars_of_timeline_axis value
  | View_timeline_inset, value -> vars_of_timeline_inset value
  | View_timeline, value -> vars_of_view_timeline_shorthand value
  | Timeline_scope, value -> vars_of_timeline_name value
  | Webkit_appearance, value -> vars_of_webkit_appearance value
  | Webkit_background_clip, value -> vars_of_background_box value
  | Webkit_box_decoration_break, value -> vars_of_box_decoration_break value
  | Webkit_print_color_adjust, value -> vars_of_print_color_adjust value
  | Webkit_box_orient, value -> vars_of_webkit_box_orient value
  | Webkit_font_smoothing, value -> vars_of_webkit_font_smoothing value
  | Webkit_line_clamp, value -> vars_of_webkit_line_clamp value
  | Webkit_mask_clip, value -> vars_of_mask_box value
  | Webkit_mask_composite, value -> vars_of_webkit_mask_composite value
  | Webkit_mask_origin, value -> vars_of_mask_box value
  | Webkit_mask_repeat, value -> vars_of_background_repeat value
  | Webkit_mask_source_type, value -> vars_of_mask_source_type value
  | Webkit_text_size_adjust, value -> vars_of_text_size_adjust value
  | Webkit_user_select, value -> vars_of_user_select value
  | Ms_user_select, value -> vars_of_user_select value
  | Webkit_text_fill_color, value -> vars_of_color value
  | Webkit_text_stroke_color, value -> vars_of_color value
  | Webkit_text_stroke_width, value -> vars_of_border_width value
  | Webkit_text_stroke, value ->
      Option.fold ~none:[] ~some:vars_of_border_width value.width
      @ Option.fold ~none:[] ~some:vars_of_color value.color
  | Moz_user_select, value -> vars_of_user_select value
  | White_space, value -> vars_of_white_space value
  | White_space_collapse, value -> vars_of_white_space_collapse value
  | Font_variant_alternates, value -> vars_of_font_variant_alternates value
  | Font_variant, value -> vars_of_font_variant value
  | Word_break, value -> vars_of_word_break value
  | Writing_mode, value -> vars_of_writing_mode value
  | Z_index, value -> vars_of_z_index value
  | Text_combine_upright, value -> vars_of_text_combine_upright value
  | Font, value -> vars_of_font value
  | Grid_area, value -> vars_of_grid_area value
  | List_style, value -> vars_of_list_style value
  | Webkit_animation, value -> List.concat_map vars_of_animation value
  (* Modern vendor-prefixed longhands: same value type as the unprefixed modern
     property, so reuse the same [vars_of_*] extractor. *)
  | Webkit_animation_delay, value -> vars_of_duration value
  | Webkit_animation_duration, value -> vars_of_duration value
  | Webkit_animation_direction, value -> vars_of_animation_direction value
  | Webkit_animation_iteration_count, value ->
      vars_of_animation_iteration_count value
  | Webkit_animation_name, value -> vars_of_animation_name value
  | Webkit_animation_timing_function, value -> vars_of_timing_function value
  | Webkit_animation_fill_mode, value -> vars_of_animation_fill_mode value
  | Webkit_animation_play_state, value -> vars_of_animation_play_state value
  | Moz_animation, value -> List.concat_map vars_of_animation value
  | Moz_animation_delay, value -> vars_of_duration value
  | Moz_animation_duration, value -> vars_of_duration value
  | Moz_animation_direction, value -> vars_of_animation_direction value
  | Moz_animation_iteration_count, value ->
      vars_of_animation_iteration_count value
  | Moz_animation_name, value -> vars_of_animation_name value
  | Moz_animation_timing_function, value -> vars_of_timing_function value
  | Moz_animation_fill_mode, value -> vars_of_animation_fill_mode value
  | Moz_animation_play_state, value -> vars_of_animation_play_state value
  | Webkit_transition_delay, value -> vars_of_duration value
  | Webkit_transition_duration, value -> vars_of_duration value
  | Webkit_transition_property, value -> vars_of_transition_property_list value
  | Webkit_transition_timing_function, value -> vars_of_timing_function value
  | Moz_transition, value -> List.concat_map vars_of_transition value
  | Moz_transition_delay, value -> vars_of_duration value
  | Moz_transition_duration, value -> vars_of_duration value
  | Moz_transition_property, value -> vars_of_transition_property_list value
  | Moz_transition_timing_function, value -> vars_of_timing_function value
  | Webkit_flex_direction, value -> vars_of_flex_direction value
  | Webkit_flex_wrap, value -> vars_of_flex_wrap value
  | Webkit_flex_flow, value -> vars_of_flex_flow value
  | Webkit_justify_content, value -> vars_of_justify_content value
  | Webkit_align_items, value -> vars_of_align_items value
  | Webkit_align_content, value -> vars_of_align_content value
  | Webkit_align_self, value -> vars_of_align_self value
  | Webkit_border_radius, value -> vars_of_border_radius value
  | Webkit_box_shadow, value -> vars_of_shadow value
  | Webkit_background_size, value -> vars_of_background_size value
  | Moz_border_radius, value -> vars_of_border_radius value
  | Moz_box_shadow, value -> vars_of_shadow value
  (* Logical-border shorthand middle tier. *)
  | Border_block_start, value -> vars_of_border value
  | Border_block_end, value -> vars_of_border value
  | Border_inline, value -> vars_of_border value
  | Border_inline_start, value -> vars_of_border value
  | Border_inline_end, value -> vars_of_border value
  (* Logical sizing longhands (CSS Logical 1 sec. 4.1): the same
     <length-percentage> as the physical property each maps to. *)
  | Inline_size, value -> vars_of_length_percentage value
  | Min_inline_size, value -> vars_of_length_percentage value
  | Max_inline_size, value -> vars_of_length_percentage value
  | Block_size, value -> vars_of_length_percentage value
  | Min_block_size, value -> vars_of_length_percentage value
  | Max_block_size, value -> vars_of_length_percentage value
  (* [inset] and its logical longhands hold a length list, like [top]. *)
  | Inset, value -> vars_of_length_list value
  | Inset_inline, value -> vars_of_length_list value
  | Inset_inline_start, value -> vars_of_length_list value
  | Inset_inline_end, value -> vars_of_length_list value
  | Inset_block, value -> vars_of_length_list value
  | Inset_block_start, value -> vars_of_length_list value
  | Inset_block_end, value -> vars_of_length_list value
  (* Logical scroll-margin / scroll-padding, matching the physical arms: the
     two-value forms hold a list, the single-side longhands one length. *)
  | Scroll_margin_inline, value -> vars_of_length_list value
  | Scroll_margin_inline_start, value -> vars_of_length value
  | Scroll_margin_inline_end, value -> vars_of_length value
  | Scroll_margin_block, value -> vars_of_length_list value
  | Scroll_margin_block_start, value -> vars_of_length value
  | Scroll_margin_block_end, value -> vars_of_length value
  | Scroll_padding_inline, value -> vars_of_length_list value
  | Scroll_padding_inline_start, value -> vars_of_length value
  | Scroll_padding_inline_end, value -> vars_of_length value
  | Scroll_padding_block, value -> vars_of_length_list value
  | Scroll_padding_block_start, value -> vars_of_length value
  | Scroll_padding_block_end, value -> vars_of_length value
  (* One position per background / mask layer, as for [object-position]. *)
  | Background_position, value -> List.concat_map vars_of_position_value value
  | Background_position_x, value -> vars_of_background_position_axis value
  | Background_position_y, value -> vars_of_background_position_axis value
  | Webkit_mask_position_x, value -> vars_of_background_position_axis value
  | Webkit_mask_position_y, value -> vars_of_background_position_axis value
  | Mask_position, value -> List.concat_map vars_of_position_value value
  | Webkit_mask_position, value -> List.concat_map vars_of_position_value value
  (* Remaining typed longhands. *)
  | Text_emphasis_color, value -> vars_of_color value
  | Text_underline_offset, value -> vars_of_length value
  | Shape_margin, value -> vars_of_length_percentage value
  | Line_height_step, value -> vars_of_length value
  | Offset_distance, value -> vars_of_length_percentage value
  | Transition_property, value -> vars_of_transition_property_list value
  (* CSS Box Alignment 3 sec. 6.3: a one-value [place-self] sets both, so the
     reader stores the [var()] in each half; the dedup collapses them. *)
  | Place_self, (align, justify) ->
      vars_of_align_self align @ vars_of_justify_self justify
  (* CSS Backgrounds 3 sec. 5.2: [border-image-slice] is numbers, percentages
     and the [fill] keyword, none of which carries a var handle. *)
  | Border_image_slice, _ -> []
  (* [shape-outside] and an unrecognised property are held as raw text and an
     opaque token stream respectively; scan both structurally. *)
  | Shape_outside, value -> vars_of_value_string value
  | Unknown_property _, value -> vars_of_token_stream value
  | Custom_property _, Custom_value { value; _ } ->
      vars_of_custom_property_value value

let rec extract_vars_of_declaration : declaration -> any_var list = function
  | Declaration
      { property = Custom_property _; value = Custom_value { value; _ }; _ } ->
      vars_of_custom_property_value value
  | Declaration { property; value; _ } -> vars_of_property property value
  | Theme_guarded { decl; _ } -> extract_vars_of_declaration decl

(* Stable dedup: preserves first occurrence of each var, removes later
   duplicates. For the (very common) zero / one / two element cases skip the
   hashtable entirely - the work is cheaper than the allocation. *)
let stable_dedup_vars = function
  | [] -> []
  | [ _ ] as xs -> xs
  | [ (V v1 as a); (V v2 as b) ] ->
      if v1.name = v2.name then [ a ] else [ a; b ]
  | vars ->
      let seen = Hashtbl.create (List.length vars) in
      List.filter
        (fun (V v) ->
          if Hashtbl.mem seen v.name then false
          else (
            Hashtbl.add seen v.name ();
            true))
        vars

let vars_of_declarations properties =
  List.concat_map extract_vars_of_declaration properties |> stable_dedup_vars

(* Cheaper than [vars_of_declarations [d] <> []]: skip the dedup hashtable and
   short-circuit on the first matching var. *)
let declaration_uses_var d =
  match extract_vars_of_declaration d with [] -> false | _ -> true

(* Extract only custom property declarations (variable definitions) *)
let custom_declarations ?layer (decls : declaration list) : declaration list =
  List.filter
    (function
      | Declaration
          {
            property = Custom_property _;
            value = Custom_value { layer = decl_layer; _ };
            _;
          } -> (
          match layer with None -> true | Some l -> decl_layer = Some l)
      | _ -> false)
    decls

(* Extract the variable name from a custom declaration *)
let rec custom_declaration_name (decl : declaration) : string option =
  match decl with
  | Declaration { property = Custom_property name; _ } -> Some name
  | Theme_guarded { decl; _ } -> custom_declaration_name decl
  | _ -> None

(* Pretty-printer for any_syntax *)
let pp_any_syntax : any_syntax Pp.t = fun ctx (Syntax syn) -> pp_syntax ctx syn

(* Reader for any_syntax *)
let read_any_syntax (r : Cursor.t) : any_syntax =
  (* Reuse the main read_syntax function *)
  read_syntax r

(* CSS Custom Properties sec. 3 requires [var()] fallbacks to round-trip
   including author-written comments. The token stream silently drops comments,
   so slice the original source between the first and last fallback component
   instead of re-serialising tokens. Falls back to component-based serialisation
   when the source is not retained on the cursor. *)
let string_of_fallback inner =
  let cvs = Cursor.consume_remaining inner in
  match (cvs, Cursor.source inner) with
  | [], _ -> ""
  | _, None -> Cursor.string_of_components ~trim:true cvs
  | first :: _, Some src ->
      let start_pos = (Component.source_loc first).start_pos in
      let last = List.nth cvs (List.length cvs - 1) in
      let end_pos = (Component.source_loc last).end_pos in
      let len = max 0 (end_pos - start_pos) in
      let slice =
        String.sub src start_pos (min len (String.length src - start_pos))
      in
      String.trim slice

(* CSS Custom Properties 1 sec. 3: [var( <custom-property-name> ,
   <declaration-value>? )], read from a cursor already positioned at the
   argument list. The string-returning counterpart of [read_var_body], for a
   caller with no value type to read the fallback at. *)
let read_reference_body_as_string (r : Cursor.t) : string * string option =
  Cursor.ws r;
  let raw_name = Cursor.ident ~keep_case:true r in
  (* css-variables-1: a <custom-property-name> is a <dashed-ident> other than
     the bare reserved [--] keyword. A further leading dash is part of the
     name. *)
  if not (Custom_property_name.is_valid raw_name) then
    Cursor.err_invalid r ("not a custom property: " ^ raw_name);
  let name = Custom_property_name.strip_prefix raw_name in
  Cursor.ws r;
  let fallback =
    if Cursor.comma_opt r then Some (string_of_fallback r) else None
  in
  (name, fallback)

(** Parse a CSS variable reference with optional fallback value. This creates a
    variable handle for parsing purposes only - it doesn't have type or layer
    information which would need to be resolved from a variable registry or
    context. *)
let read_reference (r : Cursor.t) : string * string option =
  (* CSS Syntax 3 (ED) sec. 4.3.6: EOF inside a function is a parse error,
     tolerated only when the fallback list opened with a comma (a sec. 4.3.5
     [<string-token>] may have eaten the closing [)]) so a recoverable name +
     fallback survives. Without a fallback there is no recovery signal, so it is
     rejected. *)
  let terminated =
    match Cursor.peek r with
    | Some (Component.Func fn) -> fn.node.terminated
    | _ -> true
  in
  let result = Cursor.call "var" r read_reference_body_as_string in
  (match (terminated, snd result) with
  | false, None -> Cursor.err_invalid r "unterminated var()"
  | _ -> ());
  Cursor.ws r;
  if not (Cursor.is_done r) then
    Cursor.err_invalid r "trailing tokens after var()";
  result

let read_reference_body = read_var_body
