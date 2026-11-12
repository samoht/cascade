(** Static colour evaluation: hex decoding, sRGB and linear-sRGB conversion,
    [color-mix()] interpolation and gamut mapping. *)

open Values_intf

val pp_color_name : color_name Pp.t
(** [pp_color_name] pretty-prints {!type-color_name} values. *)

val minify_relative_color_alpha : string -> string
(** [minify_relative_color_alpha body] rewrites a trailing percentage alpha in a
    relative colour tail as a number. *)

val minify_relative_color_numbers : string -> string
(** [minify_relative_color_numbers body] respells every number in a relative
    colour tail at its shortest. *)

val relative_color_splits_tokens : char -> string -> int -> bool
(** [relative_color_splits_tokens prev body i] is [true] when the reader splits
    [body] at [i] after the character [prev] without a space. *)

val minify_relative_color_spaces : string -> string
(** [minify_relative_color_spaces body] drops the spaces a relative colour tail
    reads the same without. *)

val fold_relative_color_pass_through : string -> color -> string -> color option
(** [fold_relative_color_pass_through name origin tail] is [origin] when the
    relative colour [name(from origin tail)] passes its channels through
    unchanged. *)

val deg_of_hue : hue -> float option
(** [deg_of_hue h] is [h] in degrees when it is a plain number or a static
    angle. *)

val alpha_is_full : alpha -> bool
(** [alpha_is_full a] is [true] when [a] is absent or fully opaque. *)

val alpha_value_byte : alpha -> int option
(** [alpha_value_byte a] is the rounded byte of the static alpha [a]. *)

val exact_alpha_value_byte : alpha -> int option
(** [exact_alpha_value_byte a] is the byte of the static alpha [a] when it lands
    on a whole byte. *)

val rgba_of_hex : string -> (int * int * int * int) option
(** [rgba_of_hex s] is the [(r, g, b, a)] bytes of the hex digits [s]. *)

val hex_of_byte : int -> string
(** [hex_of_byte i] is the two lower-case hex digits of the byte [i]. *)

val hex_opt : string -> color option
(** [hex_opt s] is {!val-hex} without the exception: the colour when [s] is a
    hex spelling, and nothing otherwise. *)

val hex : string -> color
(** [hex s] is the colour of the hex spelling [s], with or without the leading
    [#]. *)

val static_color_to_srgb_channels :
  color -> (int option * int option * int option * int option) option
(** [static_color_to_srgb_channels c] is the sRGB bytes of the static colour
    [c], channel by channel, with [None] for a [none] channel. *)

val static_color_to_srgb_bytes : color -> (int * int * int * int) option
(** [static_color_to_srgb_bytes c] is the [(r, g, b, a)] sRGB bytes of the
    static colour [c]. *)

val nonkeyword_color : color -> color
(** [nonkeyword_color c] re-spells a colour written as a bare keyword (a named
    colour, or transparent) as its hex form, leaving every other colour
    unchanged. *)

val exact_rgb_to_srgb_bytes : color -> (int * int * int * int) option
(** [exact_rgb_to_srgb_bytes c] is the sRGB bytes of the [rgb()] colour [c] when
    every channel lands on a whole byte. *)

val exact_srgb_function_channels : color -> (int * int * int * alpha) option
(** [exact_srgb_function_channels c] is the bytes and alpha of the
    [color(srgb ...)] colour [c] when every channel lands on a whole byte. *)

val mix_weights : float -> float -> (float * float * float) option
(** [mix_weights p1 p2] is the two [color-mix()] component weights and the alpha
    multiplier for the percentages [p1] and [p2]. *)

val mix_srgb_bytes :
  color -> color -> p1:float -> p2:float -> (int * int * int * int) option
(** [mix_srgb_bytes c1 c2 ~p1 ~p2] is the sRGB bytes of [c1] and [c2] mixed in
    sRGB. *)

val mix_lab_family :
  color_space option -> color -> color -> p1:float -> p2:float -> color option
(** [mix_lab_family in_space c1 c2 ~p1 ~p2] mixes two colours already written in
    the lab-family space [in_space]. *)

val static_color_to_linear_srgb :
  color -> ((float * float * float) * float) option
(** [static_color_to_linear_srgb c] is the linear-sRGB coordinates and alpha of
    the static colour [c]. *)

val gamut_map_color : color -> color
(** [gamut_map_color c] is the sRGB colour a display renders for the static
    colour [c], as a hex colour: CSS Color 4 sec. 14.2 reduces the chroma of a
    colour sRGB cannot hold until it fits, holding lightness and hue. *)

val alpha_of_unit_float : float -> alpha
(** [alpha_of_unit_float a] is the alpha [a], absent when opaque. *)

val premult_mix3 :
  w1:float ->
  w2:float ->
  a1:float ->
  a2:float ->
  float * float * float ->
  float * float * float ->
  (float * float * float) * float
(** [premult_mix3 ~w1 ~w2 ~a1 ~a2 c1 c2] interpolates two coordinate triples
    with premultiplied alpha. *)

val mix_in_oklab_space : color -> color -> p1:float -> p2:float -> color option
(** [mix_in_oklab_space c1 c2 ~p1 ~p2] mixes two static colours in Oklab. *)

val mix_in_oklch_space :
  color -> color -> p1:float -> p2:float -> hue_interpolation -> color option
(** [mix_in_oklch_space c1 c2 ~p1 ~p2 hue] mixes two static colours in OKLCh
    along [hue]. *)

val mix_in_lab_space : color -> color -> p1:float -> p2:float -> color option
(** [mix_in_lab_space c1 c2 ~p1 ~p2] mixes two static colours in CIE Lab. *)

val mix_in_lch_space :
  color -> color -> p1:float -> p2:float -> hue_interpolation -> color option
(** [mix_in_lch_space c1 c2 ~p1 ~p2 hue] mixes two static colours in CIE LCh
    along [hue]. *)

val mix_in_rectangular_space :
  color_space -> color -> color -> p1:float -> p2:float -> color option
(** [mix_in_rectangular_space space c1 c2 ~p1 ~p2] mixes two static colours in
    the rectangular [color()] space [space]. *)

val color_mix_percentages :
  percentage option -> percentage option -> (float * float) option
(** [color_mix_percentages p1 p2] is the two [color-mix()] percentages after CSS
    Color 5 sec. 3 normalisation. *)

val hex_string_of_bytes : int -> int -> int -> int -> string
(** [hex_string_of_bytes r g b a] is the shortest hex spelling, without [#], of
    the bytes. *)

val with_alpha : color -> alpha -> color
(** [with_alpha c a] is [c] carrying the alpha [a], replacing whatever alpha [c]
    already spelled. *)

val minify_color : color -> color
(** [minify_color c] converts named colors to hex when the hex form is shorter
    or equal length, matching Lightning CSS behavior. *)
