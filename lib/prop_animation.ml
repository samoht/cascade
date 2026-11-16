(* css-animations-2, css-transitions-2, scroll-animations-1 and
   view-transitions-2: the animation and transition shorthands and their
   longhands, easing functions, scroll and view timelines, animation-range,
   view-transition-name / -class and overlay.

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name its parts export stays visible under [Css.Properties]. *)

include Prop_animation_timeline
include Prop_animation_transition
include Prop_animation_shorthand
