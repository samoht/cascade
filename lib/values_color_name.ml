(** Named and system colour keywords, and the colour constructors. *)

open Values_intf

let read_system_color_of_string keyword : color option =
  (* System colors are case-insensitive per CSS spec *)
  match String.lowercase_ascii keyword with
  | "accentcolor" -> Some (System Accent_color)
  | "accentcolortext" -> Some (System Accent_color_text)
  | "activetext" -> Some (System Active_text)
  | "buttonborder" -> Some (System Button_border)
  | "buttonface" -> Some (System Button_face)
  | "buttontext" -> Some (System Button_text)
  | "canvas" -> Some (System Canvas)
  | "canvastext" -> Some (System Canvas_text)
  | "field" -> Some (System Field)
  | "fieldtext" -> Some (System Field_text)
  | "graytext" -> Some (System Gray_text)
  | "highlight" -> Some (System Highlight)
  | "highlighttext" -> Some (System Highlight_text)
  | "linktext" -> Some (System Link_text)
  | "mark" -> Some (System Mark)
  | "marktext" -> Some (System Mark_text)
  | "selecteditem" -> Some (System Selected_item)
  | "selecteditemtext" -> Some (System Selected_item_text)
  | "visitedtext" -> Some (System Visited_text)
  (* WebKit-specific system colors *)
  | "-webkit-focus-ring-color" -> Some (System Webkit_focus_ring_color)
  | _ -> None

(* CSS Color 4 sec. 6.1: the named colours, each with its canonical CSS spelling
   and its sRGB bytes. This list is the only place either is written down.
   [pp_color_name], [color_name_hex], the colour-keyword reader,
   [read_color_name] and the hex inversion all derive from it, so a name and its
   bytes cannot drift apart and every view covers the same 148 colours. Hex is
   stored in its shortest spelling ([rrggbb] folded to [rgb]), which is what
   [hex_string_of_bytes] produces, so the inversion keys on it directly.

   A colour with two names lists the canonical spelling first ([gray] before
   [grey], [cyan] before [aqua]); the hex inversion keeps the first row. *)
let color_table : (color_name * string * string) list =
  [
    (Red, "red", "f00");
    (Blue, "blue", "00f");
    (Green, "green", "008000");
    (White, "white", "fff");
    (Black, "black", "000");
    (Yellow, "yellow", "ff0");
    (Cyan, "cyan", "0ff");
    (Magenta, "magenta", "f0f");
    (Gray, "gray", "808080");
    (Grey, "grey", "808080");
    (Orange, "orange", "ffa500");
    (Purple, "purple", "800080");
    (Pink, "pink", "ffc0cb");
    (Silver, "silver", "c0c0c0");
    (Maroon, "maroon", "800000");
    (Fuchsia, "fuchsia", "f0f");
    (Lime, "lime", "0f0");
    (Olive, "olive", "808000");
    (Navy, "navy", "000080");
    (Teal, "teal", "008080");
    (Aqua, "aqua", "0ff");
    (Alice_blue, "aliceblue", "f0f8ff");
    (Antique_white, "antiquewhite", "faebd7");
    (Aquamarine, "aquamarine", "7fffd4");
    (Azure, "azure", "f0ffff");
    (Beige, "beige", "f5f5dc");
    (Bisque, "bisque", "ffe4c4");
    (Blanched_almond, "blanchedalmond", "ffebcd");
    (Blue_violet, "blueviolet", "8a2be2");
    (Brown, "brown", "a52a2a");
    (Burlywood, "burlywood", "deb887");
    (Cadet_blue, "cadetblue", "5f9ea0");
    (Chartreuse, "chartreuse", "7fff00");
    (Chocolate, "chocolate", "d2691e");
    (Coral, "coral", "ff7f50");
    (Cornflower_blue, "cornflowerblue", "6495ed");
    (Cornsilk, "cornsilk", "fff8dc");
    (Crimson, "crimson", "dc143c");
    (Dark_blue, "darkblue", "00008b");
    (Dark_cyan, "darkcyan", "008b8b");
    (Dark_goldenrod, "darkgoldenrod", "b8860b");
    (Dark_gray, "darkgray", "a9a9a9");
    (Dark_green, "darkgreen", "006400");
    (Dark_grey, "darkgrey", "a9a9a9");
    (Dark_khaki, "darkkhaki", "bdb76b");
    (Dark_magenta, "darkmagenta", "8b008b");
    (Dark_olive_green, "darkolivegreen", "556b2f");
    (Dark_orange, "darkorange", "ff8c00");
    (Dark_orchid, "darkorchid", "9932cc");
    (Dark_red, "darkred", "8b0000");
    (Dark_salmon, "darksalmon", "e9967a");
    (Dark_sea_green, "darkseagreen", "8fbc8f");
    (Dark_slate_blue, "darkslateblue", "483d8b");
    (Dark_slate_gray, "darkslategray", "2f4f4f");
    (Dark_slate_grey, "darkslategrey", "2f4f4f");
    (Dark_turquoise, "darkturquoise", "00ced1");
    (Dark_violet, "darkviolet", "9400d3");
    (Deep_pink, "deeppink", "ff1493");
    (Deep_sky_blue, "deepskyblue", "00bfff");
    (Dim_gray, "dimgray", "696969");
    (Dim_grey, "dimgrey", "696969");
    (Dodger_blue, "dodgerblue", "1e90ff");
    (Firebrick, "firebrick", "b22222");
    (Floral_white, "floralwhite", "fffaf0");
    (Forest_green, "forestgreen", "228b22");
    (Gainsboro, "gainsboro", "dcdcdc");
    (Ghost_white, "ghostwhite", "f8f8ff");
    (Gold, "gold", "ffd700");
    (Goldenrod, "goldenrod", "daa520");
    (Green_yellow, "greenyellow", "adff2f");
    (Honeydew, "honeydew", "f0fff0");
    (Hot_pink, "hotpink", "ff69b4");
    (Indian_red, "indianred", "cd5c5c");
    (Indigo, "indigo", "4b0082");
    (Ivory, "ivory", "fffff0");
    (Khaki, "khaki", "f0e68c");
    (Lavender, "lavender", "e6e6fa");
    (Lavender_blush, "lavenderblush", "fff0f5");
    (Lawn_green, "lawngreen", "7cfc00");
    (Lemon_chiffon, "lemonchiffon", "fffacd");
    (Light_blue, "lightblue", "add8e6");
    (Light_coral, "lightcoral", "f08080");
    (Light_cyan, "lightcyan", "e0ffff");
    (Light_goldenrod_yellow, "lightgoldenrodyellow", "fafad2");
    (Light_gray, "lightgray", "d3d3d3");
    (Light_green, "lightgreen", "90ee90");
    (Light_grey, "lightgrey", "d3d3d3");
    (Light_pink, "lightpink", "ffb6c1");
    (Light_salmon, "lightsalmon", "ffa07a");
    (Light_sea_green, "lightseagreen", "20b2aa");
    (Light_sky_blue, "lightskyblue", "87cefa");
    (Light_slate_gray, "lightslategray", "789");
    (Light_slate_grey, "lightslategrey", "789");
    (Light_steel_blue, "lightsteelblue", "b0c4de");
    (Light_yellow, "lightyellow", "ffffe0");
    (Lime_green, "limegreen", "32cd32");
    (Linen, "linen", "faf0e6");
    (Medium_aquamarine, "mediumaquamarine", "66cdaa");
    (Medium_blue, "mediumblue", "0000cd");
    (Medium_orchid, "mediumorchid", "ba55d3");
    (Medium_purple, "mediumpurple", "9370db");
    (Medium_sea_green, "mediumseagreen", "3cb371");
    (Medium_slate_blue, "mediumslateblue", "7b68ee");
    (Medium_spring_green, "mediumspringgreen", "00fa9a");
    (Medium_turquoise, "mediumturquoise", "48d1cc");
    (Medium_violet_red, "mediumvioletred", "c71585");
    (Midnight_blue, "midnightblue", "191970");
    (Mint_cream, "mintcream", "f5fffa");
    (Misty_rose, "mistyrose", "ffe4e1");
    (Moccasin, "moccasin", "ffe4b5");
    (Navajo_white, "navajowhite", "ffdead");
    (Old_lace, "oldlace", "fdf5e6");
    (Olive_drab, "olivedrab", "6b8e23");
    (Orange_red, "orangered", "ff4500");
    (Orchid, "orchid", "da70d6");
    (Pale_goldenrod, "palegoldenrod", "eee8aa");
    (Pale_green, "palegreen", "98fb98");
    (Pale_turquoise, "paleturquoise", "afeeee");
    (Pale_violet_red, "palevioletred", "db7093");
    (Papaya_whip, "papayawhip", "ffefd5");
    (Peach_puff, "peachpuff", "ffdab9");
    (Peru, "peru", "cd853f");
    (Plum, "plum", "dda0dd");
    (Powder_blue, "powderblue", "b0e0e6");
    (Rebecca_purple, "rebeccapurple", "663399");
    (Rosy_brown, "rosybrown", "bc8f8f");
    (Royal_blue, "royalblue", "4169e1");
    (Saddle_brown, "saddlebrown", "8b4513");
    (Salmon, "salmon", "fa8072");
    (Sandy_brown, "sandybrown", "f4a460");
    (Sea_green, "seagreen", "2e8b57");
    (Sea_shell, "seashell", "fff5ee");
    (Sienna, "sienna", "a0522d");
    (Sky_blue, "skyblue", "87ceeb");
    (Slate_blue, "slateblue", "6a5acd");
    (Slate_gray, "slategray", "708090");
    (Slate_grey, "slategrey", "708090");
    (Snow, "snow", "fffafa");
    (Spring_green, "springgreen", "00ff7f");
    (Steel_blue, "steelblue", "4682b4");
    (Tan, "tan", "d2b48c");
    (Thistle, "thistle", "d8bfd8");
    (Tomato, "tomato", "ff6347");
    (Turquoise, "turquoise", "40e0d0");
    (Violet, "violet", "ee82ee");
    (Wheat, "wheat", "f5deb3");
    (White_smoke, "whitesmoke", "f5f5f5");
    (Yellow_green, "yellowgreen", "9acd32");
  ]

(* Not called. [color_table] is a list, so a constructor with no row would be a
   lookup miss at run time; this match turns adding a [color_name] into a build
   error here, beside the table that needs the new row. *)
let _color_table_covers_color_name : color_name -> unit = function
  | Red | Blue | Green | White | Black | Yellow | Cyan | Magenta | Gray | Grey
  | Orange | Purple | Pink | Silver | Maroon | Fuchsia | Lime | Olive | Navy
  | Teal | Aqua | Alice_blue | Antique_white | Aquamarine | Azure | Beige
  | Bisque | Blanched_almond | Blue_violet | Brown | Burlywood | Cadet_blue
  | Chartreuse | Chocolate | Coral | Cornflower_blue | Cornsilk | Crimson
  | Dark_blue | Dark_cyan | Dark_goldenrod | Dark_gray | Dark_green | Dark_grey
  | Dark_khaki | Dark_magenta | Dark_olive_green | Dark_orange | Dark_orchid
  | Dark_red | Dark_salmon | Dark_sea_green | Dark_slate_blue | Dark_slate_gray
  | Dark_slate_grey | Dark_turquoise | Dark_violet | Deep_pink | Deep_sky_blue
  | Dim_gray | Dim_grey | Dodger_blue | Firebrick | Floral_white | Forest_green
  | Gainsboro | Ghost_white | Gold | Goldenrod | Green_yellow | Honeydew
  | Hot_pink | Indian_red | Indigo | Ivory | Khaki | Lavender | Lavender_blush
  | Lawn_green | Lemon_chiffon | Light_blue | Light_coral | Light_cyan
  | Light_goldenrod_yellow | Light_gray | Light_green | Light_grey | Light_pink
  | Light_salmon | Light_sea_green | Light_sky_blue | Light_slate_gray
  | Light_slate_grey | Light_steel_blue | Light_yellow | Lime_green | Linen
  | Medium_aquamarine | Medium_blue | Medium_orchid | Medium_purple
  | Medium_sea_green | Medium_slate_blue | Medium_spring_green
  | Medium_turquoise | Medium_violet_red | Midnight_blue | Mint_cream
  | Misty_rose | Moccasin | Navajo_white | Old_lace | Olive_drab | Orange_red
  | Orchid | Pale_goldenrod | Pale_green | Pale_turquoise | Pale_violet_red
  | Papaya_whip | Peach_puff | Peru | Plum | Powder_blue | Rebecca_purple
  | Rosy_brown | Royal_blue | Saddle_brown | Salmon | Sandy_brown | Sea_green
  | Sea_shell | Sienna | Sky_blue | Slate_blue | Slate_gray | Slate_grey | Snow
  | Spring_green | Steel_blue | Tan | Thistle | Tomato | Turquoise | Violet
  | Wheat | White_smoke | Yellow_green ->
      ()

(* Shortest spelling of one colour. The name wins only when it is no longer than
   the [#hex] beside it, so [bisque] beats [#ffe4c4] while [blue] loses to
   [#00f]. [minify_color] and the hex inversion are the two directions of this
   single choice, which is why both read it from here. *)
let color_name_is_shortest ~name ~hex =
  String.length name < String.length hex + 1

let color_table_index : (color_name, string * string) Hashtbl.t =
  let tbl = Hashtbl.create 211 in
  List.iter
    (fun (c, name, hex) -> Hashtbl.replace tbl c (name, hex))
    color_table;
  tbl

(** Convert a named color to its hex equivalent (name, hex_value). Returns the
    shortest representation matching Lightning CSS behavior. *)
let color_name_hex (c : color_name) : string * string =
  match Hashtbl.find_opt color_table_index c with
  | Some row -> row
  | None -> ("", "")

let color_name_by_spelling : (string, color_name) Hashtbl.t =
  let tbl = Hashtbl.create 211 in
  List.iter (fun (c, name, _) -> Hashtbl.replace tbl name c) color_table;
  tbl

(* [name] must already be lower-cased: a colour keyword is
   ASCII-case-insensitive, but every caller folds the case before it gets
   here. *)
let color_name_of_string name = Hashtbl.find_opt color_name_by_spelling name

(* Reverse of [color_name_hex], restricted to the colours whose name is the
   shortest spelling: inverting any of the others would hand back a longer
   answer than the hex it was given. *)
let color_name_by_hex : (string, color_name) Hashtbl.t =
  let tbl = Hashtbl.create 61 in
  List.iter
    (fun (c, name, hex) ->
      if color_name_is_shortest ~name ~hex && not (Hashtbl.mem tbl hex) then
        Hashtbl.add tbl hex c)
    color_table;
  tbl

let color_name_of_hex hex =
  Hashtbl.find_opt color_name_by_hex (String.lowercase_ascii hex)

let read_color_keyword_of_string keyword : color option =
  match keyword with
  | "transparent" -> Some Transparent
  | "currentcolor" -> Some Current
  | "auto" -> Some Auto
  | "inherit" -> Some Inherit
  | "initial" -> Some Initial
  | "unset" -> Some Unset
  | "revert" -> Some Revert
  | "revert-layer" -> Some Revert_layer
  | _ -> (
      match color_name_of_string keyword with
      | Some name -> Some (Named name)
      (* CSS system colors - case-insensitive matching *)
      | None -> read_system_color_of_string keyword)

(* Inside a custom-property body, colour keywords ([transparent], named colours,
   ...) are ASCII-case-insensitive idents whose canonical spelling is
   lower-case. System colours are left alone (canonical spelling is mixed-case);
   non-colour idents pass through so a class-name token keeps its source
   case. *)
let fold_custom_value_ident s =
  let lower = String.lowercase_ascii s in
  if
    Option.is_some (read_color_keyword_of_string lower)
    && Option.is_none (read_system_color_of_string lower)
  then lower
  else Parser.fold_value_ident s

(** Color constructors *)
let rgb ?alpha r g b =
  match alpha with
  | None -> Rgb (Channels { r = Int r; g = Int g; b = Int b })
  | Some a ->
      Rgba { rgb = Channels { r = Int r; g = Int g; b = Int b }; a = Num a }

let hsl h s l = Hsl { h = Unitless h; s = Pct s; l = Pct l; a = None }
let hsla h s l a = Hsl { h = Unitless h; s = Pct s; l = Pct l; a = Num a }
let hwb h w b = Hwb { h = Unitless h; w = Pct w; b = Pct b; a = None }
let hwba h w b a = Hwb { h = Unitless h; w = Pct w; b = Pct b; a = Num a }

let oklch l c h =
  Oklch { l = Some (Pct l); c = Some c; h = Unitless h; alpha = None }

let oklcha l c h a =
  Oklch { l = Some (Pct l); c = Some c; h = Unitless h; alpha = Num a }

let oklch_none_hue l c =
  Oklch { l = Some (Pct l); c = Some c; h = Hue_none; alpha = None }

let oklab l a b =
  Oklab { l = Some (Pct l); a = Some a; b = Some b; alpha = None }

let oklaba l a b alpha =
  Oklab { l = Some (Pct l); a = Some a; b = Some b; alpha = Num alpha }

let oklaba_none_zeros l a b alpha =
  let a = if a = 0.0 then Stdlib.Option.None else Stdlib.Option.Some a in
  let b = if b = 0.0 then Stdlib.Option.None else Stdlib.Option.Some b in
  Oklab { l = Some (Pct l); a; b; alpha = Num alpha }

let lch l c h =
  Lch { l = Some (Pct l); c = Some c; h = Unitless h; alpha = None }

let lcha l c h a =
  Lch { l = Some (Pct l); c = Some c; h = Unitless h; alpha = Num a }

let color_name n = Named n
let current_color = Current
let transparent = Transparent

let color_mix ?in_space ?(hue = Default) ?percent1 ?percent2 color1 color2 =
  let percent1 : percentage option =
    match percent1 with Some p -> Some (Pct p) | None -> None
  in
  let percent2 : percentage option =
    match percent2 with Some p -> Some (Pct p) | None -> None
  in
  Mix { in_space; hue; color1; percent1; color2; percent2 }

let color_mix_var_percent ?in_space ?(hue = Default) ~var_name color1 color2 =
  let percent1 : percentage option =
    Some
      (Var
         {
           name = var_name;
           fallback = None;
           default = None;
           layer = None;
           meta = None;
           runtime = false;
         })
  in
  Mix { in_space; hue; color1; percent1; color2; percent2 = None }

let color_mix_var_pct_fallback ?in_space ?(hue = Default) ~var_name ~fallback
    color1 color2 =
  let percent1 : percentage option =
    Some
      (Var
         {
           name = var_name;
           fallback;
           default = None;
           layer = None;
           meta = None;
           runtime = false;
         })
  in
  Mix { in_space; hue; color1; percent1; color2; percent2 = None }
