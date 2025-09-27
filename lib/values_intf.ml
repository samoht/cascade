type meta = ..

type 'a fallback =
  | Empty (* Empty fallback: var(--name,) *)
  | None (* No fallback: var(--name) *)
  | Fallback of 'a (* Value fallback: var(--name, value) *)

type 'a var = {
  name : string;
  fallback : 'a fallback;
  default : 'a option;
  layer : string option;
  meta : meta option;
}

type calc_op = Add | Sub | Mul | Div

type 'a calc =
  | Var of 'a var
  | Val of 'a
  | Num of float
  | Expr of 'a calc * calc_op * 'a calc

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
  | Ch of float
  | Lh of float
  | Auto
  | Zero
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Fit_content
  | Content
  | Max_content
  | Min_content
  | From_font
  | Function of string
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

type channel = Int of int | Num of float | Pct of float | Var of channel var

type rgb =
  | Channels of { r : channel; g : channel; b : channel }
  | Var of rgb var

type angle =
  | Deg of float
  | Rad of float
  | Turn of float
  | Grad of float
  | Var of angle var

type alpha = None | Num of float | Pct of float | Var of alpha var
type hue = Unitless of float | Angle of angle | Var of hue var

type component =
  | Num of float
  | Pct of float
  | Angle of hue
  | Var of component var
  | Calc of component calc

type percentage =
  | Pct of float
  | Var of percentage var
  | Calc of percentage calc

type length_percentage =
  | Length of length
  | Pct of float
  | Var of length_percentage var
  | Calc of length_percentage calc

type number_percentage =
  | Num of float
  | Pct of float
  | Var of number_percentage var
  | Calc of number_percentage calc

type hue_interpolation = Shorter | Longer | Increasing | Decreasing | Default

type color =
  | Hex of { hash : bool; value : string }
  | Rgb of rgb
  | Rgba of { rgb : rgb; a : alpha }
  | Hsl of { h : hue; s : percentage; l : percentage; a : alpha }
  | Hwb of { h : hue; w : percentage; b : percentage; a : alpha }
  | Color of { space : color_space; components : component list; alpha : alpha }
  | Oklch of { l : percentage; c : float; h : hue; alpha : alpha }
  | Oklab of { l : percentage; a : float; b : float; alpha : alpha }
  | Lch of { l : percentage; c : float; h : hue; alpha : alpha }
  | Named of color_name
  | Var of color var
  | Current
  | Transparent
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

type duration = Ms of float | S of float | Var of duration var
type number = Num of float | Var of number var
