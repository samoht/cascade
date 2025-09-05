(** Common test helpers for CSS parser tests *)

(** Module for converting CSS values to strings using the pretty-printers *)
module To_string = struct
  (* Display and positioning *)
  let display v = Css.Pp.to_string Css.pp_display v
  let position v = Css.Pp.to_string Css.pp_position v

  (* Typography *)
  let font_weight v = Css.Pp.to_string Css.pp_font_weight v
  let font_style v = Css.Pp.to_string Css.pp_font_style v
  let text_align v = Css.Pp.to_string Css.pp_text_align v
  let text_decoration v = Css.Pp.to_string Css.pp_text_decoration v
  let text_transform v = Css.Pp.to_string Css.pp_text_transform v

  (* Box model *)
  let length v = Css.Pp.to_string Css.pp_length v
  let overflow v = Css.Pp.to_string Css.pp_overflow v
  let border_style v = Css.Pp.to_string Css.pp_border_style v

  (* Colors and backgrounds *)
  let color v = Css.Pp.to_string Css.pp_color v

  (* User interaction *)
  let cursor v = Css.Pp.to_string Css.pp_cursor v

  (* Transforms and effects *)
  let transform v = Css.Pp.to_string Css.pp_transform v
  let box_shadow v = Css.Pp.to_string Css.pp_box_shadow v

  (* Values *)
  let angle v = Css.Pp.to_string Css.pp_angle v
  let duration v = Css.Pp.to_string Css.pp_duration v
  let calc pp_value v = Css.Pp.to_string (Css.pp_calc pp_value) v

  (* Flexbox *)
  let flex_direction v = Css.Pp.to_string Css.pp_flex_direction v
  let align_items v = Css.Pp.to_string Css.pp_align_items v
  let justify_content v = Css.Pp.to_string Css.pp_justify_content v
end

(** Helper to create a Reader from a string *)
let reader_of_string s = Css_parser.Reader.of_string s

(** Helper to parse and pretty-print a value *)
let parse_and_pp parser pp s =
  let t = reader_of_string s in
  pp (parser t)
