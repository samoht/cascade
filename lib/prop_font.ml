(* css-fonts-4: the font shorthand and every font-* longhand, font-variant and
   font-synthesis, feature and variation settings, font-family, line-height,
   unicode-range, @font-face src and display, and the vendor font-smoothing
   properties.

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name here stays visible under [Css.Properties] exactly as when
   it lived in properties.ml. *)

include Prop_font_variant
include Prop_font_family
include Prop_font_face
include Prop_font_shorthand
