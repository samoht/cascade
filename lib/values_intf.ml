type meta = ..

type component_values = Component.t list
(** Parsed CSS component values preserved for fallback and invalid-value
    round-tripping. *)

type invalid_value = component_values
(** Spec-invalid value fragments preserved until optimization decides whether to
    drop the containing declaration. *)

type custom_value = component_values
(** CSS custom-property token stream. *)

type 'a fallback =
  | Empty (* Empty fallback: var(--name,) *)
  | Empty2
    (* 2-char empty fallback: var(--name, ) -- matches tailwindcss output,
       likely a bug in tailwindcss *)
  | None (* No fallback: var(--name) *)
  | Fallback of 'a (* Value fallback: var(--name, value) *)
  | Syntax_fallback of component_values
    (* Syntactic declaration-value fallback when it is not a typed value. *)
  | Var_fallback of
      string (* Nested var fallback: var(--name, var(--fallback)) *)

type 'a var = {
  name : string;
  fallback : 'a fallback;
  default : 'a option;
  layer : string option;
  meta : meta option;
  runtime : bool;
      (** When [true], a context never folds this var to its [default]: the
          [var()] reference is kept so a runtime stylesheet or script can still
          override the custom property. The [default] then acts only as a
          browser-time fallback hint. *)
}

type 'a env = { name : string; indices : int list; fallback : 'a option }
type calc_op = Add | Sub | Mul | Div

(** CSS Values 4 sec. 10.7 math constants - emitted at the source byte sequence
    (e.g. [pi], [e]) rather than their floating-point evaluation, so pretty pp
    preserves [calc(2 * pi)] instead of writing [calc(6.28318530718)]. *)
type math_const = Pi | E | Infinity | Neg_infinity | Nan

(** CSS Values 4 (ED) sec. 9.1 numeric math function arguments. Self-recursive
    so nested calls round-trip ([pow(2, sqrt(100))]) and arithmetic with
    constants stays as a tree ([(e - exp(1))]). *)
type math_arg =
  | Lit of float
  | Dim of float * string
      (** A dimension argument (e.g. [1vw], [1%]). [sign(<dimension>)] only
          cares about the numeric coefficient; the unit is preserved for pretty
          pp. *)
  | Const of math_const
  | Var_arg of math_arg var
  | Op of math_arg * calc_op * math_arg
  | Parens_arg of math_arg
  | Math_call of math_fn

(** CSS Values 4 (ED) sec. 9.1 numeric math functions. Each arm preserves its
    source arg shape so pretty pp re-emits [name(args)]; the optimizer evaluates
    to [Num] under minify. *)
and math_fn =
  | Sin of angle_arg
  | Cos of angle_arg
  | Tan of angle_arg
  | Asin of math_arg
  | Acos of math_arg
  | Atan of math_arg
  | Atan2 of math_arg * math_arg
  | Sqrt of math_arg
  | Exp of math_arg
  | Log of math_arg * math_arg option
  | Pow of math_arg * math_arg
  | Hypot of math_arg list
  | Sign_n of math_arg
  | Abs_n of math_arg

(** [sin] / [cos] / [tan] accept an [<angle>] or unitless [<number>] expression
    (treated as radians). Arithmetic over angles ([22deg + 23deg]) and
    parentheses round-trip via [Operation] / [Grouped]. *)
and angle_arg =
  | Deg of float
  | Rad of float
  | Turn of float
  | Grad of float
  | Numeric_arg of math_arg
  | Operation of angle_arg * calc_op * angle_arg
  | Grouped of angle_arg

type 'a calc =
  | Var of 'a var
  | Val of 'a
  | Num of float
  | Math_const of math_const
  | Sibling_index
  | Sibling_count
  | Expr of 'a calc * calc_op * 'a calc
  | Nested of 'a calc (* Explicitly nested calc(), rendered as calc(inner) *)
  | Parens of 'a calc (* Parenthesized expression, rendered as (inner) *)
  | Math_fn of math_fn
(* CSS Values 4 (ED) sec. 9.1 numeric math functions ([sin], [cos], [hypot],
   ...). Pretty pp emits [name(args)] preserving source shape; minify pp /
   optimizer evaluates to [Num]. *)

type attr_syntax = Length | Length_percentage | Color | Number | Percentage

type attr_type =
  | Type of attr_syntax
  | Unit of string
  | Raw_string
  | Number_type

type 'a attr_fallback = No_fallback | Empty_fallback | Attr_fallback of 'a

type 'a attr_call = {
  name : string;
  type_ : attr_type option;
  fallback : 'a attr_fallback;
}

type length =
  | Px of float
  | Cm of float
  | Mm of float
  | Q of float
  | In of float
  | Pt of float
  | Pc of float
  | Rem of float
  | Em of float
  | Ex of float
  | Cap of float
  | Ic of float
  | Ric of float
  | Rlh of float
  | Pct of float
  | Vw of float
  | Vh of float
  | Vmin of float
  | Vmax of float
  | Vi of float
  | Vb of float
  | Dvh of float
  | Dvw of float
  | Dvmin of float
  | Dvmax of float
  | Lvh of float
  | Lvw of float
  | Lvmin of float
  | Lvmax of float
  | Svh of float
  | Svw of float
  | Svmin of float
  | Svmax of float
  | Cqw of float
  | Cqh of float
  | Cqi of float
  | Cqb of float
  | Cqmin of float
  | Cqmax of float
  | Ch of float
  | Lh of float
  | Dimension of { value : float; unit : string; repr : string }
  | Size
  | Auto
  | None
  | Normal
      (** [normal] keyword used by [letter-spacing], [word-spacing],
          [line-height], etc. when their value falls back to the user-agent
          default. *)
  | Zero
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Fit_content
  | Fit_content_arg of length
      (** [fit-content(<length-percentage>)] - CSS Sizing 3 sec. 5.1. The
          argument is a [<length-percentage>]; we store it via the [length] type
          because [length] already has a [Pct of float] case for the percentage
          form (a separate [length_percentage] would force a mutually-recursive
          type). *)
  | Content
  | Contain
  | Max_content
  | Min_content
  | Webkit_max_content
  | Webkit_min_content
  | Webkit_fit_content
  | Moz_max_content
  | Moz_min_content
  | Moz_fit_content
  | From_font
  | Hairline
  | Thin
  | Medium
  | Thick
  | Stretch
  | Clamp of length * length * length
  | Min of length list
  | Max of length list
  | Minmax of length * length
  | Round of string * length * length
  | Mod of length * length
  | Rem_fn of length * length
  | Hypot of length list
  | Abs of length
  | Sign of length
  | Calc_size of length * length calc
  | Anchor_size of string
  | Anchor of string option * string * length option
  | Attr of length attr_call
      (** CSS Values 5 sec. 8.7 [attr(<attr-name> <attr-type>?, <fallback>?)]
          for typed-value contexts. *)
  | Env of length env
  | Var of length var
  | Calc of length calc

type color_space =
  | Srgb
  | Srgb_linear
  | Display_p3
  | A98_rgb
  | Prophoto_rgb
  | Rec2020
  | Lab
  | Oklab
  | Xyz
  | Xyz_d50
  | Xyz_d65
  | Lch
  | Oklch
  | Hsl
  | Hwb

type color_name =
  | Red
  | Blue
  | Green
  | White
  | Black
  | Yellow
  | Cyan
  | Magenta
  | Gray
  | Grey
  | Orange
  | Purple
  | Pink
  | Silver
  | Maroon
  | Fuchsia
  | Lime
  | Olive
  | Navy
  | Teal
  | Aqua
  | Alice_blue
  | Antique_white
  | Aquamarine
  | Azure
  | Beige
  | Bisque
  | Blanched_almond
  | Blue_violet
  | Brown
  | Burlywood
  | Cadet_blue
  | Chartreuse
  | Chocolate
  | Coral
  | Cornflower_blue
  | Cornsilk
  | Crimson
  | Dark_blue
  | Dark_cyan
  | Dark_goldenrod
  | Dark_gray
  | Dark_green
  | Dark_grey
  | Dark_khaki
  | Dark_magenta
  | Dark_olive_green
  | Dark_orange
  | Dark_orchid
  | Dark_red
  | Dark_salmon
  | Dark_sea_green
  | Dark_slate_blue
  | Dark_slate_gray
  | Dark_slate_grey
  | Dark_turquoise
  | Dark_violet
  | Deep_pink
  | Deep_sky_blue
  | Dim_gray
  | Dim_grey
  | Dodger_blue
  | Firebrick
  | Floral_white
  | Forest_green
  | Gainsboro
  | Ghost_white
  | Gold
  | Goldenrod
  | Green_yellow
  | Honeydew
  | Hot_pink
  | Indian_red
  | Indigo
  | Ivory
  | Khaki
  | Lavender
  | Lavender_blush
  | Lawn_green
  | Lemon_chiffon
  | Light_blue
  | Light_coral
  | Light_cyan
  | Light_goldenrod_yellow
  | Light_gray
  | Light_green
  | Light_grey
  | Light_pink
  | Light_salmon
  | Light_sea_green
  | Light_sky_blue
  | Light_slate_gray
  | Light_slate_grey
  | Light_steel_blue
  | Light_yellow
  | Lime_green
  | Linen
  | Medium_aquamarine
  | Medium_blue
  | Medium_orchid
  | Medium_purple
  | Medium_sea_green
  | Medium_slate_blue
  | Medium_spring_green
  | Medium_turquoise
  | Medium_violet_red
  | Midnight_blue
  | Mint_cream
  | Misty_rose
  | Moccasin
  | Navajo_white
  | Old_lace
  | Olive_drab
  | Orange_red
  | Orchid
  | Pale_goldenrod
  | Pale_green
  | Pale_turquoise
  | Pale_violet_red
  | Papaya_whip
  | Peach_puff
  | Peru
  | Plum
  | Powder_blue
  | Rebecca_purple
  | Rosy_brown
  | Royal_blue
  | Saddle_brown
  | Salmon
  | Sandy_brown
  | Sea_green
  | Sea_shell
  | Sienna
  | Sky_blue
  | Slate_blue
  | Slate_gray
  | Slate_grey
  | Snow
  | Spring_green
  | Steel_blue
  | Tan
  | Thistle
  | Tomato
  | Turquoise
  | Violet
  | Wheat
  | White_smoke
  | Yellow_green

type channel =
  | Int of int
  | Num of float
  | Pct of float
  | Var of channel var
  | None
      (** CSS Color 4 sec. 4.4 [none] sentinel. Carries through to the
          serialised output as the [none] keyword and participates in
          [color-mix] / relative-color substitution by adopting the other
          operand's analogous channel instead of being treated as a zero. *)

type rgb =
  | Channels of { r : channel; g : channel; b : channel }
  | Var of rgb var

type angle =
  | Deg of float
  | Rad of float
  | Turn of float
  | Grad of float
  | Round of string * angle * angle
  | Mod of angle * angle
  | Rem of angle * angle
  | Calc of angle calc
  | Var of angle var
  | Invalid of invalid_value
      (** Spec-invalid [<angle>] input (e.g. [asin(<angle>)] - inverse trig
          takes [<number>], not [<angle>]) that upstream tools preserve
          verbatim. The pretty-printer emits the tokens unchanged; the
          [Optimize.drop_invalid] pass, which every serialisation runs, removes
          any declaration whose typed value reduces to this arm. *)

type alpha =
  | None
  | Num of float
  | Pct of float
  | Var of alpha var
  | Calc of alpha calc

type hue = Unitless of float | Angle of angle | Var of hue var | Hue_none

type component =
  | Num of float
  | Pct of float
  | Angle of hue
  | Var of component var
  | Calc of component calc
  | Component_none

type percentage =
  | Pct of float
  | Num of float
  | Var of percentage var
  | Calc of percentage calc

type length_percentage =
  | Length of length
  | Pct of float
  | Env of length_percentage env
  | Var of length_percentage var
  | Calc of length_percentage calc
  | Invalid of invalid_value
      (** Spec-invalid input cascade detected (e.g. [width: asin(sin(45deg))]
          - the inner reduction yields an angle, but [<length-percentage>]
            doesn't accept angles). Pretty-printer emits the tokens verbatim;
            [Optimize.drop_invalid] removes the declaration on every
            serialisation. *)

type number_percentage =
  | Num of float
  | Pct of float
  | Var of number_percentage var
  | Calc of number_percentage calc

type hue_interpolation =
  | Shorter
  | Longer
  | Increasing
  | Decreasing
  | Specified
      (** CSS Color 5 sec. 13 [specified hue] - keep authored hue values without
          interpolation rotation. *)
  | Default

(** CSS system colors - case-insensitive keywords that map to OS/browser colors
*)
type system_color =
  | Accent_color
  | Accent_color_text
  | Active_text
  | Button_border
  | Button_face
  | Button_text
  | Canvas
  | Canvas_text
  | Field
  | Field_text
  | Gray_text
  | Highlight
  | Highlight_text
  | Link_text
  | Mark
  | Mark_text
  | Selected_item
  | Selected_item_text
  | Visited_text
  (* WebKit-specific system colors *)
  | Webkit_focus_ring_color

type color =
  | Hex of { r : int; g : int; b : int; a : int }
      (** A hex colour decoded to its sRGB byte components ([a = 255] when
          opaque). Every spelling ([#fff] / [#ffffff] / [#FFFFFF]) decodes to
          one node, so the printer picking the shortest spelling round-trips. *)
  | Authored_hex of { value : string; r : int; g : int; b : int; a : int }
      (** A parsed hex colour preserving the source spelling without the leading
          [#]. Optimisation folds this to the canonical semantic colour. *)
  | Rgb of rgb
  | Rgba of { rgb : rgb; a : alpha }
  | Hsl of { h : hue; s : percentage; l : percentage; a : alpha }
  | Hwb of { h : hue; w : percentage; b : percentage; a : alpha }
  | Color of { space : color_space; components : component list; alpha : alpha }
  | Relative_rgb of color * string
      (** [rgb(from <origin> <channels> [/ <alpha>]?)] with a parsed origin and
          an opaque channel-expression tail. *)
  | Relative_color of string * color * string
      (** [<fn>(from <origin> <c1> <c2> <c3> [/ <alpha>]?)] for any color
          function other than [rgb()] (CSS Color 5 sec. 4.2). The first string
          is the function name ([lab], [lch], [oklab], [oklch], [hsl], [hwb],
          [color]), the [color] is the parsed origin, and the final string is
          the channel-expression tail. *)
  | Contrast_color of color
  | Light_dark of color * color
  | Attribute of string * color option
  | Lab of {
      l : percentage option;
      a : float option;
      b : float option;
      alpha : alpha;
    }
  | Oklch of { l : percentage option; c : float option; h : hue; alpha : alpha }
  | Oklab of {
      l : percentage option;
      a : float option;
      b : float option;
      alpha : alpha;
    }
  | Lch of { l : percentage option; c : float option; h : hue; alpha : alpha }
  | Named of color_name
  | System of system_color
  | Var of color var
  | Current
  | Transparent
  | Auto
      (** [auto] keyword (e.g., [accent-color: auto], [caret-color: auto]) *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Mix of {
      in_space : color_space option;
      hue : hue_interpolation;
      color1 : color;
      percent1 : percentage option;
      color2 : color;
      percent2 : percentage option;
    }

type duration =
  | Ms of float
  | S of float
  (* CSS Animations 2 sec. 4.1: [animation-duration] alone has an [auto] arm,
     and it is that property's initial value. *)
  | Auto
  | Durations of duration list
  | Round of string * duration * duration
  | Mod of duration * duration
  | Rem of duration * duration
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of duration var
  | Calc of duration calc

type number =
  | Num of float
  | Var of number var
  | Calc of number calc
  | Round of string * number * number
  | Mod of number * number
  | Rem of number * number
  | Hypot of number * number
  | Pow of number * number
  | Sqrt of number
  | Abs of number
  | Sign of number
  | Sin of angle

type transition_behavior = Normal | Allow_discrete

let equal_length (a : length) b = a = b
let equal_length_percentage (a : length_percentage) b = a = b
let equal_number_percentage (a : number_percentage) b = a = b
let equal_color (a : color) b = a = b

(* Bounded like [Declaration.hash], for the same reason: the depth cap keeps the
   cost of fingerprinting a deep [color-mix()] tree off the caller's inner
   loop. *)
let hash_color (c : color) = Hashtbl.hash_param 30 100 c
