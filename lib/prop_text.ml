(* css-text-4, css-text-decor-4 and css-inline-3: text alignment, indent,
   transform, overflow and wrapping, the text-decoration and text-emphasis
   families, text-shadow, white-space, word-break, overflow-wrap, hyphens, ruby,
   initial-letter, text-box, tab-size and vertical-align.

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name its parts export stays visible under [Css.Properties]. *)

include Prop_text_layout
include Prop_text_inline
include Prop_text_decor
