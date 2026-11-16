(* CSS Images 4: the shared <image> value layer. Gradients and their vendor
   variants, image-set(), cross-fade(), <position>, url() and colour
   interpolation, plus image-orientation, image-rendering and image-resolution.
   Every family that embeds an <image> or a <position> (backgrounds, masks,
   border-image, transform-origin, offset-path, clip-path) reads and prints
   through the definitions here.

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name its parts export stays visible under [Css.Properties]. *)

include Prop_image_position
include Prop_image_gradient
include Prop_image_value
