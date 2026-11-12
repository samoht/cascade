(* CSS Backgrounds 3: the background and border families. The background
   longhands and shorthand, border style / width / radius / image,
   border-collapse and border-spacing, and box-shadow with its [Shadow] reader.

   [Shadow], [pp_shadow] and [normalize_shadow] are shared with the filter
   family: [drop-shadow()] reuses them. Prop_filter must therefore be ordered
   after this module, or the filter family stays in properties.ml.

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name here stays visible under [Css.Properties] exactly as when
   it lived in properties.ml. *)

include Prop_background_shadow
include Prop_background_border
include Prop_background_layer
include Prop_background_border_image
