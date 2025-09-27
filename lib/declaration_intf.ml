(** Declaration interface types *)

open Values
open Properties_intf

type _ kind =
  | Length : length kind
  | Color : color kind
  | Rgb : rgb kind
  | Int : int kind
  | Float : float kind
  | Percentage : percentage kind
  | Number_percentage : number_percentage kind
  | String : string kind
  | Duration : duration kind
  | Aspect_ratio : aspect_ratio kind
  | Border_style : border_style kind
  | Border : border kind
  | Font_weight : font_weight kind
  | Line_height : line_height kind
  | Font_family : font_family kind
  | Font_feature_settings : font_feature_settings kind
  | Font_variation_settings : font_variation_settings kind
  | Font_variant_numeric : font_variant_numeric kind
  | Font_variant_numeric_token : font_variant_numeric_token kind
  | Blend_mode : blend_mode kind
  | Scroll_snap_strictness : scroll_snap_strictness kind
  | Angle : angle kind
  | Shadow : shadow kind
  | Box_shadow : shadow kind
  | Content : content kind
  | Gradient_stop : gradient_stop kind

type declaration =
  | Declaration : {
      property : 'a property;
      value : 'a;
      important : bool;
    }
      -> declaration
  | Custom_declaration : {
      name : string;
      kind : 'a kind;
      value : 'a;
      layer : string option;
      meta : Values.meta option;
      important : bool;
    }
      -> declaration
