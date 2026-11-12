(* Custom-property values: typed re-reading, colour and math folding, token
   canonicalisation and printing. *)

open Common
open Values
open Properties_intf
open Prop_common
open Prop_image
open Prop_background
open Prop_filter
open Prop_transform
open Prop_ui
open Prop_layout
open Prop_box
open Prop_font
open Prop_animation
open Prop_content

let read_custom_value_as kind read components =
  match
    let cursor = Cursor.of_components components in
    let parsed = read cursor in
    Cursor.ws cursor;
    Cursor.expect_eof cursor;
    Some (Typed { kind; value = parsed })
  with
  | result -> result
  | exception Cursor.Parse_error _ -> None

(* CSS Custom Properties for Cascading Variables 1 sec. 2: an unregistered
   custom property is an opaque token stream that [var()] later substitutes
   wholesale into whichever consumer site invokes it. Canonical typed rewrites
   like [rgb(0 0 0) -> #000] assume a consumer type that only [@property --foo {
   syntax: "<color>"; ... }] can promise, so the parser leaves the tokens alone
   here. *)
let read_custom_property_value ?font_family:_ cursor =
  Tokens (Cursor.remaining cursor)

(* A registered [<color>] custom property carries a typed colour once promoted,
   so canonicalise it the same way a real colour property would. *)
let is_color_function name =
  match String.lowercase_ascii name with
  | "rgb" | "rgba" | "hsl" | "hsla" | "hwb" | "lab" | "lch" | "oklab" | "oklch"
  | "color" | "color-mix" | "light-dark" ->
      true
  | _ -> false

(* A construct whose type is fixed by its own syntax - a complete colour
   function ([oklab(...)], [color-mix(...)], ...) or a hex colour ([#abc]) - is
   unconditionally a colour in every [var()] substitution site, so folding it to
   its canonical spelling inside an opaque custom-property token stream
   preserves every rendered result while collapsing two spellings of the same
   colour. The only observable change is the exact token string a script reads
   back via [getPropertyValue]; that readback is the optimizer's domain (it
   already folds insignificant math whitespace here), so the canonical diff
   inherits this fold rather than shimming it. Bare keywords are left untouched
   - they may be a [<custom-ident>] in a non-colour context, whereas a hex token
   never can. *)
(* [c] is one component whose syntax fixes it as a colour; fold it to the
   shortest non-keyword spelling, falling back to [fallback ()] when it does not
   actually parse as a complete colour. [resolve_missing] is
   {!Values.normalize_color}'s flag: a missing axis reads as the zero CSS Color
   4 sec. 4.4 makes it, which holds wherever the stream substitutes as it holds
   for a longhand, and the caller decides where the stream is interpolated. *)
let fold_custom_color ~lossless ~resolve_missing (c : Component.t) ~fallback =
  let text = Parser.string_of_components [ c ] in
  let cur = Cursor.of_string text in
  match
    try Some (Values.read_color cur) with Cursor.Parse_error _ -> None
  with
  | Some col when Cursor.is_done cur -> (
      let canon =
        Pp.to_string ~minify:true Values.pp_color
          (Values.nonkeyword_color
             (Values.normalize_color ~lossless ~resolve_missing col))
      in
      match read_custom_property_value (Cursor.of_string canon) with
      | Tokens cs -> cs
      | Typed _ -> [ c ])
  | _ -> fallback ()

let is_math_function = Parser.is_math_function

(* A math function is unconditionally a math expression whose type is fixed by
   its operands' units, so when it reduces to a single constant it has that
   value in every [var()] substitution site - fold it inside an opaque
   custom-property stream like a complete colour. [<number>] and the
   unit-unambiguous dimensions ([<angle>] / [<time>]) qualify; [<percentage>] is
   ambiguous (length vs number percentage) so it stays verbatim, as does a
   function that still references a [var()] (it does not reduce to a leaf). *)
let fold_custom_calc (c : Component.t) ~fallback =
  let text = Parser.string_of_components [ c ] in
  (* Parse the whole token as one typed value and fold only when it reduces to a
     single concrete leaf (not a [calc()] that still carries a [var()]). *)
  let try_typed : type a.
      (Cursor.t -> a) ->
      (a -> a) ->
      a Pp.t ->
      (a -> bool) ->
      Component.t list option =
   fun reader normalize pp reduced ->
    let cur = Cursor.of_string text in
    match try Some (reader cur) with Cursor.Parse_error _ -> None with
    | Some v when Cursor.is_done cur ->
        let folded = normalize v in
        if reduced folded then
          match
            read_custom_property_value
              (Cursor.of_string (Pp.to_string ~minify:true pp folded))
          with
          | Tokens cs -> Some cs
          | Typed _ -> None
        else None
    | _ -> None
  in
  let number_reduced = function (Num _ : number) -> true | _ -> false in
  let angle_reduced = function
    | (Deg _ | Rad _ | Turn _ | Grad _ : angle) -> true
    | _ -> false
  in
  let time_reduced = function (S _ | Ms _ : duration) -> true | _ -> false in
  match
    List.find_map Fun.id
      [
        try_typed read_number
          (fun n -> normalize_number n)
          pp_number number_reduced;
        try_typed read_angle_unit_required
          (fun a -> normalize_angle a)
          pp_angle angle_reduced;
        try_typed read_duration
          (fun d -> normalize_duration d)
          pp_duration time_reduced;
      ]
  with
  | Some cs -> cs
  | None -> fallback ()

(* Filter Effects 1 sec. 6.1 gives [hue-rotate()] the argument [[ <angle> |
   <zero> ]?] and 0 when omitted, so a zero argument is redundant. [hue-rotate]
   names a filter function and nothing else, so this holds wherever the stream
   is substituted, which is the same argument that lets a colour function fold
   here. *)
let hue_rotate_zero_argument (func : Component.func) =
  String.lowercase_ascii func.name = "hue-rotate"
  && func.arguments <> []
  &&
  let cur = Cursor.of_string (Parser.string_of_components func.arguments) in
  match read_angle_unit_required cur with
  | exception Cursor.Parse_error _ -> false
  | angle ->
      Cursor.is_done cur
      && Values.angle_degrees_opt (normalize_angle angle) = Some 0.

let drop_function_arguments wrapped =
  Component.Func
    { wrapped with node = { wrapped.Component.node with arguments = [] } }

let rec normalize_shadow_colors ~lossless (shadow : shadow) =
  let normalize_body (body : shadow_body) : shadow_body =
    {
      body with
      color = option_map_preserve (Values.normalize_color ~lossless) body.color;
    }
  in
  match shadow with
  | Shadow body -> preserve_if_equal shadow (Shadow (normalize_body body))
  | Inset (Body body) ->
      preserve_if_equal shadow (Inset (Body (normalize_body body)) : shadow)
  | Inset (Toggle ({ body; _ } as toggle)) ->
      preserve_if_equal shadow
        (Inset (Toggle { toggle with body = normalize_body body }) : shadow)
  | List shadows ->
      preserve_if_equal shadow
        (List (map_preserve (normalize_shadow_colors ~lossless) shadows))
  | other -> other

(* A complete shadow token stream is self-typing: its length sequence and
   optional final <color> are parsed together, so a bare colour keyword cannot
   be mistaken for a custom-ident at another substitution site. Canonicalise
   only that typed colour while preserving the authored shadow shape; applying
   the full shadow optimiser here would also drop explicit default lengths from
   an otherwise opaque custom-property token stream. *)
let canonicalize_custom_shadow_components ~lossless components =
  let t = Cursor.of_string (Parser.string_of_components components) in
  match try Some (read_shadow t) with Cursor.Parse_error _ -> None with
  | Some shadow when Cursor.is_done t -> (
      let shadow = normalize_shadow_colors ~lossless shadow in
      let canonical = Pp.to_string ~minify:true pp_shadow shadow in
      match read_custom_property_value (Cursor.of_string canonical) with
      | Tokens components -> components
      | Typed _ -> components)
  | _ -> components

let rec canonicalize_custom_colors_components ~lossless
    ?(resolve_missing = false) comps =
  let fold_color c ~fallback =
    fold_custom_color ~lossless ~resolve_missing c ~fallback
  in
  let inner =
    canonicalize_custom_colors_components ~lossless ~resolve_missing
  in
  List.concat_map
    (fun (c : Component.t) ->
      match c with
      | Component.Func wrapped
        when is_color_function wrapped.Component.node.name ->
          fold_color c ~fallback:(fun () ->
              let func = wrapped.Component.node in
              let args = inner func.arguments in
              [
                Component.Func
                  { wrapped with node = { func with arguments = args } };
              ])
      | Component.Preserved { kind = Token.Hash _; _ } ->
          fold_color c ~fallback:(fun () -> [ c ])
      | Component.Func wrapped when is_math_function wrapped.Component.node.name
        ->
          fold_custom_calc c ~fallback:(fun () ->
              let func = wrapped.Component.node in
              let args = inner func.arguments in
              [
                Component.Func
                  { wrapped with node = { func with arguments = args } };
              ])
      | Component.Func wrapped
        when hue_rotate_zero_argument wrapped.Component.node ->
          [ drop_function_arguments wrapped ]
      | Component.Func wrapped ->
          let func = wrapped.Component.node in
          let args = inner func.arguments in
          [
            Component.Func
              { wrapped with node = { func with arguments = args } };
          ]
      | Component.Block wrapped ->
          let block = wrapped.Component.node in
          let value = inner block.value in
          [ Component.Block { wrapped with node = { block with value } } ]
      | Component.Preserved _ -> [ c ])
    comps

(* Typed re-readers exposed for the registry pass that consumes [@property]
   declarations. Each reader takes a token stream and tries to parse it as the
   matching typed kind, returning [None] when the stream doesn't match. The
   unregistered path stays opaque; the registry pass is what flips a value to
   [Typed]. *)
let try_read_custom_color components =
  read_custom_value_as Color read_color components

let try_read_custom_length components =
  read_custom_value_as Length (read_length ~with_keywords:false) components

let try_read_custom_length_percentage components =
  read_custom_value_as Length_percentage
    (read_length_percentage ~with_keywords:false)
    components

let try_read_custom_number components =
  read_custom_value_as Number read_number components

let try_read_custom_percentage components =
  read_custom_value_as Percentage read_percentage components

let try_read_custom_angle components =
  read_custom_value_as Angle read_angle_unit_required components

let try_read_custom_time components =
  read_custom_value_as Duration read_duration components

let pp_number_value ctx (value : number) =
  match value with
  | Calc c when Pp.minified ctx -> (
      (* [eval_numeric_calc] is Cascade's own arithmetic, so its result takes
         the six significant figures Cascade commits to in serialised output. An
         authored coefficient is the author's digits and keeps every one of
         them, so it falls through to [pp_number]. *)
      match eval_numeric_calc c with
      | Some f -> Pp.float ctx (Pp.round_sig 6 f)
      | None -> pp_number ctx (Calc (eval_calc c)))
  | _ -> pp_number ctx value

let pp_value : type a. (a kind * a) Pp.t =
 fun ctx (kind, value) ->
  let pp pp_a = pp_a ctx value in
  match kind with
  | Length -> pp (pp_length ~always:true)
  | Color -> pp pp_specified_color
  | Rgb ->
      let rec pp_rgb_type : rgb Pp.t =
       fun ctx rgb ->
        match rgb with
        | Channels { r; g; b } ->
            pp_channel ctx r;
            Pp.space ctx ();
            pp_channel ctx g;
            Pp.space ctx ();
            pp_channel ctx b
        | Var v -> pp_var pp_rgb_type ctx v
      in
      pp pp_rgb_type
  | Number -> pp pp_number_value
  | Int -> pp Pp.int
  | Float -> pp Pp.float
  | Percentage -> pp pp_percentage
  | Length_percentage -> pp (pp_length_percentage ~always:true)
  | Number_percentage -> pp pp_number_percentage
  | Opacity -> pp pp_opacity
  | Value ->
      (* CSS Custom Properties 1 (ED) sec. 4.1 forbids normalizing the
         whitespace of the stream a [var()] substitutes, so the runs come back
         as they were read. *)
      let rendered =
        if Pp.minified ctx then
          Parser.to_string_custom_minified
            ~fold_ident:Values.fold_custom_value_ident value
        else Parser.to_string_verbatim value
      in
      Pp.string ctx rendered
  | Shadow -> pp pp_shadow
  | Duration -> pp pp_duration
  | Aspect_ratio -> pp pp_aspect_ratio
  | Border_style -> pp pp_border_style
  | Outline_style -> pp pp_outline_style
  | Border -> pp pp_border
  | Font_weight -> pp pp_font_weight
  | Font_size -> pp pp_font_size
  | Line_height -> pp pp_line_height
  | Font_family -> pp pp_font_family
  | Font_feature_settings -> pp pp_font_feature_settings
  | Font_variation_settings -> pp pp_font_variation_settings
  | Numeric -> pp pp_font_variant_numeric
  | Font_variant_numeric_token -> pp pp_font_variant_numeric_token
  | Blend_mode -> pp pp_blend_mode
  | Scroll_snap_strictness -> pp pp_scroll_snap_strictness
  | Angle -> pp pp_angle
  | Rotate -> pp pp_rotate_value
  | Scale -> pp pp_scale
  | Content -> pp pp_content
  | Gradient_stop -> pp pp_gradient_stop
  | Gradient_direction -> pp pp_gradient_direction
  | Gradient_position -> pp pp_gradient_position
  | Radial_shape -> pp pp_radial_shape
  | Radial_size -> pp pp_radial_size
  | Position_value -> pp pp_position_value
  | Animation -> pp pp_animation
  | Timing_function -> pp pp_timing_function
  | Transform -> pp pp_transform
  | Touch_action -> pp pp_touch_action
  | Transition_property_value -> pp pp_transition_property_value
  | Background_image -> pp pp_background_image
  | Z_index -> pp pp_z_index
  | Filter -> pp pp_filter
  | Font_src -> pp pp_font_src

let string_of_channel : channel -> string = function
  | Int i -> string_of_int i
  | Num f -> Pp.string_of_float f
  | Pct p -> Pp.string_of_float p ^ "%"
  | Var _ -> "0"
  | None -> "none"

let string_of_kind_value : type a. a kind -> a -> string =
 fun kind value ->
  match kind with
  | Length -> Pp.to_string (pp_length ~always:false) value
  | Color -> Pp.to_string pp_color value
  | Angle -> Pp.to_string pp_angle value
  | Duration -> Pp.to_string pp_duration value
  | Float -> Pp.string_of_float value
  | Percentage -> (
      match value with Pct f -> Pp.string_of_float f | _ -> "initial")
  | Number_percentage -> Values.string_of_number_percentage value
  | Number -> Pp.to_string pp_number value
  | Int -> string_of_int value
  | Value -> Parser.string_of_components value
  | Content -> (
      match value with
      | String "" -> "\"\""
      | String s -> "\"" ^ s ^ "\""
      | Quoted { value; quote; repr = _ } ->
          String.make 1 quote ^ value ^ String.make 1 quote
      | None -> "none"
      | Normal -> "normal"
      | Open_quote -> "open-quote"
      | Close_quote -> "close-quote"
      | Image _ | Attr _ | Counter _ | Counters _ | String_ref _
      | Content_list _ | Inherit | Initial | Unset | Revert | Revert_layer
      | Var _ ->
          "initial")
  | Font_weight -> Pp.to_string pp_font_weight value
  | Shadow -> "0 0 #0000"
  | Border_style -> Pp.to_string pp_border_style value
  | Outline_style -> Pp.to_string pp_outline_style value
  | Scroll_snap_strictness -> Pp.to_string pp_scroll_snap_strictness value
  | Rgb -> (
      match value with
      | Channels { r; g; b } ->
          string_of_channel r ^ " " ^ string_of_channel g ^ " "
          ^ string_of_channel b
      | Var _ -> "initial")
  | Animation -> Pp.to_string pp_animation value
  | Gradient_direction -> Pp.to_string pp_gradient_direction value
  | Gradient_position -> Pp.to_string pp_gradient_position value
  | _ -> "initial"

let pp_custom_property_value ctx = function
  | Typed { kind; value } -> pp_value ctx (kind, value)
  | Tokens value -> pp_value ctx (Value, value)

let components_of_custom_property_value = function
  | Tokens components -> components
  | Typed { kind; value } ->
      Cursor.remaining
        (Cursor.of_string (Pp.to_string ~minify:true pp_value (kind, value)))

let pp_custom_property ctx (Custom_value { value; _ }) =
  pp_custom_property_value ctx value

(* CSS Values 4 (ED) sec. 10.8 ("Syntax"): inside a math function whitespace is
   *required* around binary [+] and [-] (sign-token disambiguation - stripping
   it changes [100% - var(--a)] to [100%-var(--a)], where [-var] is one
   ident-like function token) but *optional* around [*], [/], [(], [)], [,].
   Strip the optional whitespace from math-function arguments so two
   custom-property token streams that differ only there have the same canonical
   AST. Typed math is already minified by [pp_calc]; this matters for opaque
   [Tokens _] custom-property values where cascade preserves the author's
   whitespace verbatim by design. Nested non-math functions ([var()] etc.) get a
   recursive component walk but no whitespace stripping; nested math functions
   get their own. *)
let strip_math_whitespace comps =
  let rec aux acc = function
    | [] -> List.rev acc
    | (Component.Preserved { kind = Token.Whitespace _; _ } as ws) :: rest ->
        let prev_pm =
          match acc with
          | [] -> false
          | p :: _ -> Parser.is_plus_or_minus_delim p
        in
        let next_pm =
          match rest with
          | [] -> false
          | n :: _ -> Parser.is_plus_or_minus_delim n
        in
        if prev_pm || next_pm then aux (ws :: acc) rest else aux acc rest
    | other :: rest -> aux (other :: acc) rest
  in
  aux [] comps

let is_mul_or_div_delim = function
  | Component.Preserved { kind = Token.Delim ("*" | "/"); _ } -> true
  | _ -> false

(* Outside a math function only the whitespace adjacent to a [*] or [/] delim is
   insignificant (CSS Values 4 sec. 10.8): [16 / 9] and [16/9] re-tokenise
   identically wherever the stream is substituted. Every other separator stays
   (a whitespace token between two values is part of the stream). *)
let strip_mul_div_whitespace comps =
  let rec aux acc = function
    | [] -> List.rev acc
    | (Component.Preserved { kind = Token.Whitespace _; _ } as ws) :: rest ->
        let prev_md =
          match acc with [] -> false | p :: _ -> is_mul_or_div_delim p
        in
        let next_md =
          match rest with [] -> false | n :: _ -> is_mul_or_div_delim n
        in
        if prev_md || next_md then aux acc rest else aux (ws :: acc) rest
    | other :: rest -> aux (other :: acc) rest
  in
  aux [] comps

(* CSS Values 4 sec. 2.5: [var()] / [env()] / [attr()] substitute a token stream
   textually, so the whitespace next to one is significant - dropping it lets
   the substituted values merge ([var(--a) var(--b)] could become [1px2px]).
   Every other function and every block closes with a hard token boundary ([)],
   []], [}]) that no neighbour can merge across. *)
let is_substitution_func_name name =
  match String.lowercase_ascii name with
  | "var" | "env" | "attr" -> true
  | _ -> false

(* Whitespace immediately after a function or block that closes with a hard
   token boundary is insignificant: [drop-shadow(a) drop-shadow(b)] and
   [calc(45deg*-1) in oklab] re-tokenise identically without it, wherever the
   stream is substituted. Whitespace after a substitution function stays, and so
   does the whitespace before a math operator, which sec. 10.8 requires however
   hard the boundary on its left: [calc(min(1px,2px) - 3px)] keeps both spaces
   or the browser drops the declaration. *)
let strip_after_close_paren ~in_math comps =
  let closes_hard = function
    | Component.Func wrapped ->
        not (is_substitution_func_name wrapped.Component.node.name)
    | Component.Block _ -> true
    | _ -> false
  in
  let rec aux acc = function
    | [] -> List.rev acc
    | (Component.Preserved { kind = Token.Whitespace _; _ } as ws) :: rest ->
        let prev_hard =
          match acc with [] -> false | p :: _ -> closes_hard p
        in
        let next_pm =
          match rest with
          | [] -> false
          | n :: _ -> in_math && Parser.is_plus_or_minus_delim n
        in
        if prev_hard && not next_pm then aux acc rest else aux (ws :: acc) rest
    | other :: rest -> aux (other :: acc) rest
  in
  aux [] comps

(* [in_math] tracks whether the current component list is inside a math
   function's grammar. It enters at the args of a [calc()] / [min()] / ... call,
   propagates through grouping paren [Block]s since those are math operands, and
   turns off in square or curly blocks and nested non-math functions like
   [var()], which have their own grammars. *)
let rec canonicalize_math_whitespace ~in_math comps =
  let comps' =
    List.map
      (fun c ->
        match c with
        | Component.Func wrapped ->
            let func = wrapped.Component.node in
            let nested_in_math = is_math_function func.name in
            let args =
              canonicalize_math_whitespace ~in_math:nested_in_math
                func.arguments
            in
            Component.Func
              { wrapped with node = { func with arguments = args } }
        | Component.Block wrapped ->
            let block = wrapped.Component.node in
            let nested_in_math =
              match block.opening with
              | Token.Paren -> in_math
              | Token.Square | Token.Curly -> false
            in
            let value =
              canonicalize_math_whitespace ~in_math:nested_in_math block.value
            in
            Component.Block { wrapped with node = { block with value } }
        | Component.Preserved _ -> c)
      comps
  in
  let comps' =
    if in_math then strip_math_whitespace comps'
    else strip_mul_div_whitespace comps'
  in
  strip_after_close_paren ~in_math comps'

let canonicalize_math_whitespace_components comps =
  canonicalize_math_whitespace ~in_math:false comps

(* CSS Values 4 sec. 7.2: [s] and [ms] name one quantity, so a [<time>] token
   spells the same duration under either unit. Emission keeps the unit the
   author wrote, and the projection spells the token the way the typed printer
   spells a minified [<time>], the shorter of the two: [100ms] is [.1s] and
   [1500ms] is [1.5s]. A token inside a function or a block is a time where it
   lands just as a top-level one is. *)
let canonical_time_token number unit_ =
  let ms =
    match String.lowercase_ascii unit_ with
    | "ms" -> Option.Some number.Token.value
    | "s" -> Option.Some (number.Token.value *. 1000.)
    | _ -> Option.None
  in
  match ms with
  | Option.None -> Option.None
  | Option.Some ms ->
      let s = ms /. 1000. in
      let spell f = Pp.string_of_float ~drop_leading_zero:true f in
      let value, unit_ =
        if String.length (spell s) + 1 <= String.length (spell ms) + 2 then
          (s, "s")
        else (ms, "ms")
      in
      Option.Some
        (Token.Dimension
           {
             number = { value; repr = spell value; number_flag = Token.Number };
             unit_;
           })

let rec canonicalize_time_components comps =
  List.map
    (fun c ->
      match c with
      | Component.Preserved
          ({ kind = Token.Dimension { number; unit_ }; _ } as token) -> (
          match canonical_time_token number unit_ with
          | Option.Some kind ->
              Component.Preserved { token with kind; repr = Option.None }
          | Option.None -> c)
      | Component.Func wrapped ->
          let func = wrapped.Component.node in
          let arguments = canonicalize_time_components func.arguments in
          Component.Func { wrapped with node = { func with arguments } }
      | Component.Block wrapped ->
          let block = wrapped.Component.node in
          let value = canonicalize_time_components block.value in
          Component.Block { wrapped with node = { block with value } }
      | Component.Preserved _ -> c)
    comps

(* CSS Values 4 sec. 6.1: [deg], [grad], [rad] and [turn] are units of one
   dimension, and a dimension token with one of them is an [<angle>] wherever
   the stream substitutes. The projection spells the token as the degrees
   {!Values.normalize_angle} folds it to under the budget, which is where the
   typed path spells [rotate: 1.5rad], so the two meet. *)
let canonical_angle_token number unit_ =
  let v = number.Token.value in
  let angle : Values.angle option =
    match String.lowercase_ascii unit_ with
    | "deg" -> Option.Some (Deg v)
    | "rad" -> Option.Some (Rad v)
    | "turn" -> Option.Some (Turn v)
    | "grad" -> Option.Some (Grad v)
    | _ -> Option.None
  in
  match angle with
  | Option.None -> Option.None
  | Option.Some angle -> (
      let ctx = { Values.default_calc_ctx with budget = true } in
      match Values.normalize_angle ~ctx angle with
      | Deg value ->
          let repr = Pp.string_of_float ~drop_leading_zero:true value in
          Option.Some
            (Token.Dimension
               {
                 number = { value; repr; number_flag = Token.Number };
                 unit_ = "deg";
               })
      | _ -> Option.None)

let rec canonicalize_angle_components comps =
  List.map
    (fun c ->
      match c with
      | Component.Preserved
          ({ kind = Token.Dimension { number; unit_ }; _ } as token) -> (
          match canonical_angle_token number unit_ with
          | Option.Some kind ->
              Component.Preserved { token with kind; repr = Option.None }
          | Option.None -> c)
      | Component.Func wrapped ->
          let func = wrapped.Component.node in
          let arguments = canonicalize_angle_components func.arguments in
          Component.Func { wrapped with node = { func with arguments } }
      | Component.Block wrapped ->
          let block = wrapped.Component.node in
          let value = canonicalize_angle_components block.value in
          Component.Block { wrapped with node = { block with value } }
      | Component.Preserved _ -> c)
    comps

let normalize_custom_property_value ?(lossless = false)
    ?(ctx = Values.default_calc_ctx) :
    custom_property_value -> custom_property_value = function
  | Typed { kind = Length; value } ->
      Typed { kind = Length; value = Values.normalize_length ~ctx value }
  | Typed { kind = Color; value } ->
      Typed { kind = Color; value = Values.normalize_color ~lossless value }
  | Typed { kind = Number; value } ->
      Typed { kind = Number; value = Values.normalize_number ~ctx value }
  | Typed { kind = Percentage; value } ->
      Typed
        { kind = Percentage; value = Values.normalize_percentage ~ctx value }
  | Typed { kind = Length_percentage; value } ->
      Typed
        {
          kind = Length_percentage;
          value = Values.normalize_length_percentage ~ctx value;
        }
  | Typed { kind = Angle; value } ->
      Typed { kind = Angle; value = Values.normalize_angle ~ctx value }
  | Typed { kind = Duration; value } ->
      Typed { kind = Duration; value = Values.normalize_duration ~ctx value }
  | Typed { kind = Gradient_direction; value } ->
      Typed
        {
          kind = Gradient_direction;
          value = normalize_gradient_direction value;
        }
  | Tokens components ->
      Tokens
        (canonicalize_math_whitespace_components
           (canonicalize_custom_colors_components ~lossless
              (canonicalize_custom_shadow_components ~lossless components)))
  | Typed _ as other -> other
