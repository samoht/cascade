(** CSS Values & Units parsing using Reader API *)

include Values_intf

let var_ref ?fallback ?default ?layer ?meta ?(runtime = false) name =
  let fallback : _ fallback =
    match fallback with None -> None | Some x -> x
  in
  { name; fallback; default; layer; meta; runtime }

let syntax_fallback s = Syntax_fallback (Cursor.remaining (Cursor.of_string s))

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

let string_of_number_percentage (np : number_percentage) =
  match np with Num f | Pct f -> Pp.string_of_float f | _ -> "initial"

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

(** Comparison functions *)

(** Pretty-printing functions *)

(* Opens [var(] and writes the referenced name. [name] is the custom property's
   name without its [--] prefix, and CSS Syntax 3 (ED) sec. 4.3.7 lets an escape
   carry a [;] or a [}] into it, so the tail is written with the escapes that
   read the same name back. *)
let pp_var_open ctx name =
  Pp.string ctx "var(--";
  Pp.string ctx (Parser.escape_name name)

(* Prints the [var(--fallback_name)] used as another var's fallback, then the
   outer var's closing paren. Theme resolution is a transform, not a print
   concern, so the reference is emitted structurally. *)
let pp_var_fallback ctx fallback_name =
  pp_var_open ctx fallback_name;
  Pp.string ctx "))"

let pp_syntax_fallback ctx value =
  Pp.string ctx
    (if Pp.minified ctx then
       Parser.to_string_custom_minified ~fold_ident:fold_custom_value_ident
         value
     else Parser.string_of_components value)

let pp_var_ref ctx name =
  pp_var_open ctx name;
  Pp.char ctx ')'

let pp_empty_var ctx name =
  pp_var_open ctx name;
  Pp.char ctx ',';
  Pp.char ctx ')'

let pp_empty2_var ctx name =
  pp_var_open ctx name;
  Pp.string ctx ",  )"

let pp_typed_var_fallback pp_value ctx name value =
  pp_var_open ctx name;
  Pp.comma ctx ();
  pp_value { ctx with in_function = true } value;
  Pp.char ctx ')'

let pp_syntax_var_fallback ctx name value =
  pp_var_open ctx name;
  Pp.comma ctx ();
  pp_syntax_fallback { ctx with in_function = true } value;
  Pp.char ctx ')'

let first_top_level_comma_segment s =
  let len = String.length s in
  let quote_after_char i quote =
    if s.[i] = quote && (i = 0 || s.[i - 1] <> '\\') then Option.None
    else Some quote
  in
  let rec loop i depth (quote : char option) =
    if i >= len then s
    else
      match (quote, s.[i]) with
      | Some quote, _ -> loop (i + 1) depth (quote_after_char i quote)
      | Option.None, ('"' | '\'') -> loop (i + 1) depth (Some s.[i])
      | Option.None, '(' -> loop (i + 1) (depth + 1) Option.None
      | Option.None, ')' when depth > 0 -> loop (i + 1) (depth - 1) Option.None
      | Option.None, ',' when depth = 0 -> String.sub s 0 i
      | Option.None, _ -> loop (i + 1) depth Option.None
  in
  loop 0 0 Option.None

let pp_inline_var : type a. a Pp.t -> a var Pp.t =
 fun pp_value ctx v ->
  match v.default with
  | Some value -> pp_value ctx value
  | Option.None -> (
      match v.fallback with
      | Fallback value -> pp_value ctx value
      | Var_fallback _ -> pp_var_ref ctx v.name
      | Syntax_fallback value ->
          pp_syntax_fallback { ctx with in_function = true } value
      | Empty | Empty2 -> ()
      | None -> pp_var_ref ctx v.name)

(* Print the [var()] reference structurally. Theme resolution (keep / inline a
   default) is an AST transform that runs before printing, so the printer never
   consults a theme set or resolver. *)
let pp_stylesheet_var : type a. a Pp.t -> a var Pp.t =
 fun pp_value ctx v ->
  match v.fallback with
  | None -> pp_var_ref ctx v.name
  | Empty -> pp_empty_var ctx v.name
  | Empty2 -> pp_empty2_var ctx v.name
  | Fallback value -> pp_typed_var_fallback pp_value ctx v.name value
  | Syntax_fallback value -> pp_syntax_var_fallback ctx v.name value
  | Var_fallback fallback_name ->
      pp_var_open ctx v.name;
      Pp.comma ctx ();
      pp_var_fallback ctx fallback_name

let pp_var : type a. a Pp.t -> a var Pp.t =
 fun pp_value ctx v ->
  if ctx.inline then pp_inline_var pp_value ctx v
  else pp_stylesheet_var pp_value ctx v

let pp_env : type a. a Pp.t -> a env Pp.t =
 fun pp_value ctx (env : a env) ->
  Pp.string ctx "env(";
  Pp.string ctx env.name;
  List.iter
    (fun i ->
      Pp.sp ctx ();
      Pp.string ctx (string_of_int i))
    env.indices;
  (match env.fallback with
  | Some fallback ->
      Pp.comma ctx ();
      pp_value ctx fallback
  | Option.None -> ());
  Pp.char ctx ')'

(* Function call formatting now provided by Pp.call and Pp.call_list *)

(** Pretty print calc_op *)
let pp_calc_op : calc_op Pp.t =
 fun ctx op ->
  match op with
  | Add -> Pp.string ctx " + "
  | Sub -> Pp.string ctx " - "
  | Mul ->
      Pp.space_if_pretty ctx ();
      Pp.string ctx "*";
      Pp.space_if_pretty ctx ()
  | Div ->
      Pp.space_if_pretty ctx ();
      Pp.string ctx "/";
      Pp.space_if_pretty ctx ()

let pp_component_values ctx values =
  let value =
    if Pp.minified ctx then
      Parser.to_string_custom_minified ~fold_ident:fold_custom_value_ident
        values
    else Parser.string_of_components values
  in
  Pp.string ctx value

let read_component_values t = Cursor.remaining t
let pp_invalid_value = pp_component_values
let read_invalid_value = read_component_values

let string_of_attr_syntax : attr_syntax -> string = function
  | Length -> "<length>"
  | Length_percentage -> "<length-percentage>"
  | Color -> "<color>"
  | Number -> "<number>"
  | Percentage -> "<percentage>"

let pp_attr_syntax ctx syntax = Pp.string ctx (string_of_attr_syntax syntax)

let pp_attr_type ctx (type_ : attr_type) =
  match type_ with
  | Type syntax ->
      Pp.string ctx "type(";
      pp_attr_syntax ctx syntax;
      Pp.char ctx ')'
  | Unit unit_ -> Pp.string ctx unit_
  | Raw_string -> Pp.string ctx "raw-string"
  | Number_type -> Pp.string ctx "number"

let pp_attr_call pp_value ctx (attr : _ attr_call) =
  Pp.string ctx attr.name;
  Option.iter
    (fun type_ ->
      Pp.space ctx ();
      pp_attr_type ctx type_)
    attr.type_;
  match attr.fallback with
  | No_fallback -> ()
  | Empty_fallback -> Pp.comma ctx ()
  | Attr_fallback value ->
      Pp.comma ctx ();
      pp_value ctx value

let attr_syntax_of_string : string -> attr_syntax option = function
  | "<length>" -> Some Length
  | "<length-percentage>" -> Some Length_percentage
  | "<color>" -> Some Color
  | "<number>" -> Some Number
  | "<percentage>" -> Some Percentage
  | _ -> None

let read_attr_syntax t : attr_syntax =
  Cursor.ws t;
  let body = Cursor.consume_remaining_as_string ~trim:true t in
  match attr_syntax_of_string body with
  | Some syntax -> syntax
  | None -> Cursor.err_invalid t ("attr() type: " ^ body)

let read_attr_type t : attr_type =
  Cursor.ws t;
  if Cursor.looking_at_func "type" t then
    Type (Cursor.call "type" t read_attr_syntax)
  else
    match Cursor.ident_opt t with
    | Some "raw-string" -> Raw_string
    | Some "number" -> Number_type
    | Some unit_ -> Unit unit_
    | None when Cursor.peek_delim t = Some '%' ->
        Cursor.expect '%' t;
        Unit "%"
    | None -> Cursor.err_expected t "attr() type"

let pp_math_const ctx = function
  | Pi -> Pp.string ctx "pi"
  | E -> Pp.string ctx "e"
  | Infinity -> Pp.string ctx "infinity"
  | Neg_infinity -> Pp.string ctx "-infinity"
  | Nan -> Pp.string ctx "NaN"

(* Inside a math function [NaN] is a keyword of the grammar (CSS Values 4 sec.
   10.7.2), so an operand spells a NaN with it; the [calc()] wrapper
   {!Pp.nan_value} adds is for the positions outside one. *)
let pp_calc_number ctx f =
  if Float.is_nan f then pp_math_const ctx Nan else Pp.float ctx f

let read_math_const t =
  match Cursor.ident_opt t with
  | Some name -> (
      match String.lowercase_ascii name with
      | "pi" -> Pi
      | "e" -> E
      | "infinity" -> Infinity
      | "-infinity" -> Neg_infinity
      | "nan" -> Nan
      | _ -> Cursor.err_expected t "math constant")
  | None -> Cursor.err_expected t "math constant"

let math_const_value = function
  | Pi -> Float.pi
  | E -> Float.exp 1.
  | Infinity -> Float.infinity
  | Neg_infinity -> Float.neg_infinity
  | Nan -> Float.nan

(* Inside a [calc()] [Expr], a math constant participates as its numeric value
   so the surrounding fold can run ([calc(pi * 100px)] -> [314.159px]). A
   standalone [calc(pi)] keeps the named constant, which serialises shorter than
   the decimal, so this is applied only to [Expr] operands. *)
let calc_operand_value : type a. a calc -> a calc = function
  | Math_const c -> Num (math_const_value c)
  | other -> other

(* The other direction, for a fold that lands on a NaN. Sec. 10.7.2 resolves
   [NaN] at parse time into a keyword of the [<number>] grammar and sec. 10.13
   serialises every NaN-valued calculation through that keyword, so CSS has one
   NaN and one node for it: a [Num] carrying the float would print [calc(NaN)]
   too and hash apart from the keyword the same text parses to. [infinity] /
   [-infinity] are constants of their own in that list and keep their values. *)
let calc_num : type a. float -> a calc =
 fun f -> if Float.is_nan f then Math_const Nan else Num f

let rec minify_angle_arg : angle_arg -> angle_arg = function
  | Operation (l, op, r) -> (
      let l = minify_angle_arg l in
      let r = minify_angle_arg r in
      match (l, op, r) with
      | Deg a, Add, Deg b -> Deg (a +. b)
      | Deg a, Sub, Deg b -> Deg (a -. b)
      | Rad a, Add, Rad b -> Rad (a +. b)
      | Rad a, Sub, Rad b -> Rad (a -. b)
      | Turn a, Add, Turn b -> Turn (a +. b)
      | Turn a, Sub, Turn b -> Turn (a -. b)
      | Grad a, Add, Grad b -> Grad (a +. b)
      | Grad a, Sub, Grad b -> Grad (a -. b)
      | _ -> Operation (l, op, r))
  | Grouped inner -> (
      match minify_angle_arg inner with
      | Operation _ as inner -> Grouped inner
      | inner -> inner)
  | arg -> arg

let rec pp_math_arg ctx = function
  | Lit f -> pp_calc_number ctx f
  | Dim (f, unit_) -> Pp.unit ctx f unit_
  | Const c -> pp_math_const ctx c
  | Var_arg v -> pp_var pp_math_arg ctx v
  | Op (l, op, r) ->
      pp_math_arg ctx l;
      pp_calc_op ctx op;
      pp_math_arg ctx r
  | Parens_arg inner ->
      Pp.char ctx '(';
      pp_math_arg ctx inner;
      Pp.char ctx ')'
  | Math_call fn -> pp_math_fn ctx fn

and pp_math_fn ctx fn =
  let call name args =
    Pp.string ctx name;
    Pp.char ctx '(';
    Pp.list ~sep:Pp.comma pp_math_arg ctx args;
    Pp.char ctx ')'
  in
  match fn with
  | Sin a ->
      Pp.string ctx "sin(";
      pp_angle_arg ctx a;
      Pp.char ctx ')'
  | Cos a ->
      Pp.string ctx "cos(";
      pp_angle_arg ctx a;
      Pp.char ctx ')'
  | Tan a ->
      Pp.string ctx "tan(";
      pp_angle_arg ctx a;
      Pp.char ctx ')'
  | Asin a -> call "asin" [ a ]
  | Acos a -> call "acos" [ a ]
  | Atan a -> call "atan" [ a ]
  | Atan2 (y, x) -> call "atan2" [ y; x ]
  | Sqrt a -> call "sqrt" [ a ]
  | Exp a -> call "exp" [ a ]
  | Log (a, None) -> call "log" [ a ]
  | Log (a, Some b) -> call "log" [ a; b ]
  | Pow (a, b) -> call "pow" [ a; b ]
  | Hypot args -> call "hypot" args
  | Sign_n a -> call "sign" [ a ]
  | Abs_n a -> call "abs" [ a ]

and pp_angle_arg ctx arg =
  match if Pp.minified ctx then minify_angle_arg arg else arg with
  | Deg f -> Pp.unit ctx f "deg"
  | Rad f -> Pp.unit ctx f "rad"
  | Turn f -> Pp.unit ctx f "turn"
  | Grad f -> Pp.unit ctx f "grad"
  | Numeric_arg arg -> pp_math_arg ctx arg
  | Operation (l, op, r) ->
      pp_angle_arg ctx l;
      pp_calc_op ctx op;
      pp_angle_arg ctx r
  | Grouped inner ->
      Pp.char ctx '(';
      pp_angle_arg ctx inner;
      Pp.char ctx ')'

let rec eval_math_arg = function
  | Lit f -> Some f
  | Dim (f, _) -> Some f
  | Const c -> Some (math_const_value c)
  | Var_arg _ -> None
  | Op (l, op, r) -> (
      match (eval_math_arg l, eval_math_arg r) with
      | Some lv, Some rv -> (
          match op with
          | Add -> Some (lv +. rv)
          | Sub -> Some (lv -. rv)
          | Mul -> Some (lv *. rv)
          | Div when rv <> 0. -> Some (lv /. rv)
          | Div -> None)
      | _ -> None)
  | Parens_arg inner -> eval_math_arg inner
  | Math_call fn -> eval_math_fn fn

and eval_angle_arg : angle_arg -> float option = function
  | Deg f -> Some (f *. Float.pi /. 180.)
  | Rad f -> Some f
  | Turn f -> Some (f *. 2. *. Float.pi)
  | Grad f -> Some (f *. Float.pi /. 200.)
  | Numeric_arg arg -> eval_math_arg arg
  | Operation (l, op, r) -> (
      match (eval_angle_arg l, eval_angle_arg r) with
      | Some lv, Some rv -> (
          match op with
          | Add -> Some (lv +. rv)
          | Sub -> Some (lv -. rv)
          | Mul -> Some (lv *. rv)
          | Div when rv <> 0. -> Some (lv /. rv)
          | Div -> None)
      | _ -> None)
  | Grouped inner -> eval_angle_arg inner

and eval_math_fn fn =
  let unary f arg = Option.map f (eval_math_arg arg) in
  let binary f a b =
    match (eval_math_arg a, eval_math_arg b) with
    | Some av, Some bv -> Some (f av bv)
    | _ -> None
  in
  match fn with
  | Sin a -> Option.map Float.sin (eval_angle_arg a)
  | Cos a -> Option.map Float.cos (eval_angle_arg a)
  | Tan a -> Option.map Float.tan (eval_angle_arg a)
  | Asin a -> unary (fun v -> Float.asin v *. 180. /. Float.pi) a
  | Acos a -> unary (fun v -> Float.acos v *. 180. /. Float.pi) a
  | Atan a -> unary (fun v -> Float.atan v *. 180. /. Float.pi) a
  | Atan2 (y, x) -> binary (fun y x -> Float.atan2 y x *. 180. /. Float.pi) y x
  | Sqrt a -> unary Float.sqrt a
  | Exp a -> unary Float.exp a
  | Log (a, None) -> unary Float.log a
  | Log (a, Some b) -> (
      match (eval_math_arg a, eval_math_arg b) with
      | Some av, Some bv when bv > 0. && bv <> 1. ->
          Some (Float.log av /. Float.log bv)
      | _ -> None)
  | Pow (a, b) -> binary Float.pow a b
  | Hypot args ->
      let vs = List.filter_map eval_math_arg args in
      if List.length vs <> List.length args then None
      else
        let sum_sq = List.fold_left (fun acc v -> acc +. (v *. v)) 0. vs in
        Some (Float.sqrt sum_sq)
  | Sign_n a ->
      unary
        (fun v ->
          if Float.is_nan v then Float.nan
          else if v > 0. then 1.
          else if v < 0. then -1.
          else v)
        a
  | Abs_n a -> unary Float.abs a

(* What a static math function reduces to: a plain coefficient, or one that
   carries a unit. CSS Values 4 sec. 10.7 gives [abs()] and [hypot()] the type
   of their arguments, so a typed [calc()] that reduces one has to learn the
   unit here; [eval_math_fn] answers with the coefficient alone, which is how
   [calc(hypot(3px, 4px))] came out as a bare [5]. *)
type math_result = Scalar of float | United of float * string

let math_result_unit = function
  | Scalar _ -> Option.none
  | United (_, unit) -> Option.some unit

let math_result_value = function Scalar v | United (v, _) -> v

let math_result_op op a b =
  match op with
  | Add -> Option.some (a +. b)
  | Sub -> Option.some (a -. b)
  | Mul -> Option.some (a *. b)
  | Div when b <> 0. -> Option.some (a /. b)
  | Div -> Option.none

let combine_math_result op l r =
  let united unit = Option.map (fun v -> United (v, unit)) in
  let scalar = Option.map (fun v -> Scalar v) in
  match (l, op, r) with
  | Scalar a, _, Scalar b -> scalar (math_result_op op a b)
  | United (a, u), (Add | Sub), United (b, v) when String.equal u v ->
      united u (math_result_op op a b)
  | United (a, u), (Mul | Div), Scalar b -> united u (math_result_op op a b)
  | Scalar a, Mul, United (b, u) -> united u (math_result_op Mul a b)
  | _ -> Option.none

(* CSS Values 4 sec. 10.11 types an operand tree: [+] and [-] need matching
   units, [*] takes at most one dimensioned operand and [/] a unitless divisor.
   Anything else is not an operand Cascade can reduce to a single value. *)
let rec math_arg_result (arg : math_arg) : math_result option =
  match arg with
  | Lit f -> Option.some (Scalar f)
  | Dim (f, unit) -> Option.some (United (f, String.lowercase_ascii unit))
  | Const c -> Option.some (Scalar (math_const_value c))
  | Var_arg _ -> Option.none
  | Parens_arg inner -> math_arg_result inner
  | Math_call fn -> math_fn_result fn
  | Op (l, op, r) -> (
      match (math_arg_result l, math_arg_result r) with
      | Some l, Some r -> combine_math_result op l r
      | _ -> Option.none)

and math_fn_result (fn : math_fn) : math_result option =
  match fn with
  | Abs_n a -> (
      match math_arg_result a with
      | Some (Scalar v) -> Option.some (Scalar (Float.abs v))
      | Some (United (v, unit)) -> Option.some (United (Float.abs v, unit))
      | None -> Option.none)
  | Hypot args -> (
      let results = List.filter_map math_arg_result args in
      match results with
      | first :: rest
        when List.compare_lengths results args = 0
             && List.for_all
                  (fun r ->
                    Option.equal String.equal (math_result_unit r)
                      (math_result_unit first))
                  rest ->
          let square acc r =
            let v = math_result_value r in
            acc +. (v *. v)
          in
          let root = Float.sqrt (List.fold_left square 0. results) in
          Option.some
            (match math_result_unit first with
            | Some unit -> United (root, unit)
            | None -> Scalar root)
      | _ -> Option.none)
  (* Every other math function is typed [<number>] in, [<number>] out, bar the
     inverse trig functions, whose [<angle>] result only the angle evaluator can
     place. *)
  | _ -> Option.map (fun v -> Scalar v) (eval_math_fn fn)

(* CSS Values 4 (ED) sec. 10.9.2: NaN belongs to a calculation tree and has no
   leaf spelling outside one, so folding a math function down to a NaN would
   lose the only form the value can take. Leave the function in place. *)
let fold_math_result = function
  | Some r when Float.is_nan (math_result_value r) -> Option.none
  | result -> result

(* A NaN coefficient may stay: it lands in the tree as a [Num], which sec.
   10.7.2 spells [NaN]. One that has picked up a unit would rebuild a leaf
   instead, and that leaf has no spelling, so the call keeps its own. *)
let fold_united_nan = function
  | Some (United (v, _)) when Float.is_nan v -> Option.none
  | result -> result

(* Reduce a math function inside a typed [calc()]: a result CSS types as the
   arguments' own type rebuilds this type's leaf, a [<number>] result stays a
   [Num] operand, and a call that does not reduce - or reduces to a unit this
   type cannot hold - keeps its spelling rather than shedding the unit. *)
let typed_math_fn : type a. (string -> float -> a option) -> math_fn -> a calc =
 fun of_unit fn ->
  match fold_math_result (math_fn_result fn) with
  | Some (United (v, unit)) -> (
      match of_unit unit v with Some leaf -> Val leaf | None -> Math_fn fn)
  | Some (Scalar v) -> Num v
  | None -> Math_fn fn

let rec math_arg_contains_var = function
  | Lit _ | Dim _ | Const _ -> false
  | Var_arg _ -> true
  | Op (l, _, r) -> math_arg_contains_var l || math_arg_contains_var r
  | Parens_arg inner -> math_arg_contains_var inner
  | Math_call fn -> math_fn_contains_var fn

and angle_arg_contains_var : angle_arg -> bool = function
  | Deg _ | Rad _ | Turn _ | Grad _ -> false
  | Numeric_arg arg -> math_arg_contains_var arg
  | Operation (l, _, r) -> angle_arg_contains_var l || angle_arg_contains_var r
  | Grouped inner -> angle_arg_contains_var inner

and math_fn_contains_var = function
  | Sin a | Cos a | Tan a -> angle_arg_contains_var a
  | Asin a | Acos a | Atan a | Sqrt a | Exp a | Sign_n a | Abs_n a ->
      math_arg_contains_var a
  | Atan2 (a, b) | Log (a, Some b) | Pow (a, b) ->
      math_arg_contains_var a || math_arg_contains_var b
  | Log (a, None) -> math_arg_contains_var a
  | Hypot args -> List.exists math_arg_contains_var args

let rec calc_contains_var : type a. a calc -> bool = function
  | Var _ -> true
  | Val _ -> false
  | Num _ | Math_const _ | Sibling_index | Sibling_count -> false
  | Math_fn fn -> math_fn_contains_var fn
  | Nested inner | Parens inner -> calc_contains_var inner
  | Expr (l, _, r) -> calc_contains_var l || calc_contains_var r

(* Drop authored grouping nodes while retaining the expression tree that gives
   them their meaning. The printer's precedence rules reconstruct parentheses
   exactly where serialization still needs them. This is an AST normalization,
   so a non-normalized value keeps every authored [Parens] node. *)
let rec normalize_calc_parens : type a. a calc -> a calc =
 fun calc ->
  match calc with
  | Parens inner -> normalize_calc_parens inner
  | Nested inner ->
      let inner' = normalize_calc_parens inner in
      if inner' == inner then calc else Nested inner'
  | Expr (left, op, right) ->
      let left' = normalize_calc_parens left in
      let right' = normalize_calc_parens right in
      if left' == left && right' == right then calc else Expr (left', op, right')
  | Val _ | Var _ | Num _ | Math_const _ | Math_fn _ | Sibling_index
  | Sibling_count ->
      calc

let pp_calc_contents : type a. a Pp.t -> a calc Pp.t =
 fun pp_value ctx calc ->
  let precedence = function Add | Sub -> 1 | Mul | Div -> 2 in
  (* Print a calc expression, tracking parent precedence and whether we're on
     the right side of a non-commutative operator (Sub/Div) *)
  let rec pp_calc_inner ~parent_prec ~right_of_noncommut ctx = function
    | Val v -> pp_value ctx v
    | Var v -> pp_var pp_value ctx v
    | Num n -> pp_calc_number ctx n
    | Math_const c -> pp_math_const ctx c
    | Math_fn fn -> pp_math_fn ctx fn
    | Sibling_index -> Pp.string ctx "sibling-index()"
    | Sibling_count -> Pp.string ctx "sibling-count()"
    | Nested inner ->
        (* Preserve nested calc() exactly as written to match Tailwind's output
           format, e.g. calc(4px * calc(1 - var(--tw-reverse))) *)
        Pp.call "calc"
          (fun ctx inner ->
            pp_calc_inner ~parent_prec:0 ~right_of_noncommut:false ctx inner)
          ctx inner
    | Parens inner ->
        Pp.char ctx '(';
        pp_calc_inner ~parent_prec:0 ~right_of_noncommut:false ctx inner;
        Pp.char ctx ')'
    | Expr (left, op, right) ->
        let op_prec = precedence op in
        (* Need parens if: - Our precedence is lower than parent (standard) - OR
           we're on the right of a non-commutative op (Sub/Div) and our op has
           same or lower precedence *)
        let needs_parens =
          op_prec < parent_prec || (right_of_noncommut && op_prec <= parent_prec)
        in
        if needs_parens then Pp.char ctx '(';
        pp_calc_inner ~parent_prec:op_prec ~right_of_noncommut:false ctx left;
        pp_calc_op ctx op;
        (* Right side of Sub/Div needs special handling *)
        let is_noncommut = match op with Sub | Div -> true | _ -> false in
        pp_calc_inner ~parent_prec:op_prec ~right_of_noncommut:is_noncommut ctx
          right;
        if needs_parens then Pp.char ctx ')'
  in
  pp_calc_inner ~parent_prec:0 ~right_of_noncommut:false ctx calc

(* Six significant figures is the precision Cascade commits to in serialised
   output, and CSS Values 4 sec. 10.13 leaves the choice to the implementation.
   It applies to a coefficient Cascade computed itself - an irrational folded
   out of [pi] or [sqrt()], a conversion between absolute units - which carries
   digits that will never be printed. An authored coefficient is the author's
   own digits and keeps every one of them: at a [14px] font [.4285714em] is
   [6px] and [.428571em] is not.

   The budget buys a fractional tail, and six significant figures reach one only
   while the magnitude stays under [10^6]. Wider than that the only digits left
   to drop are integer ones the arithmetic got right: [1in] is exactly [96px]
   and [1pt] exactly [4/3px] (CSS Values 4 sec. 6.2), so an inch added to
   [999999999px] is [1000000095px], and [1000000000px] is 95px away from it. *)
let round_computed (f : float) : float =
  if Float.abs f < 1e6 then Pp.round_sig 6 f else f

(* Whether every digit of [f] already prints within the budget. *)
let fits_precision (f : float) : bool = Pp.round_sig 6 f = f

(* Fold [a / b] only when the float quotient round-trips through multiplication:
   exact divisions like [100/4 = 25] survive but [100/3 = 33.333...] does not,
   so the calc wrapper stays and CSS Values 4 sec. 10.13's precision requirement
   holds. *)
let exact_div (a : float) (b : float) : float option =
  if b = 0. then Option.none
  else
    let r = a /. b in
    (* Round-trip alone is too lax under IEEE 754 (33.333... * 3 = 100.0 due to
       multiplication rounding). Also require the quotient to fit the serialised
       precision, whatever its magnitude: an unfolded [calc()] keeps the digits
       the quotient would otherwise drop. *)
    if Float.is_finite r && r *. b = a && fits_precision r then Option.some r
    else Option.none

(* Scale the coefficient [v] of a typed leaf by [n], rebuilding the leaf in its
   own type. Multiplication folds whenever the product stays finite; division
   folds only on an exact quotient, unless [~exact:false] accepts the rounded
   one. *)
let scale_leaf ~exact (op : calc_op) n rebuild v =
  match op with
  | Mul ->
      let r = v *. n in
      if Float.is_finite r then Option.some (rebuild r) else Option.none
  | Div ->
      if exact then Option.map rebuild (exact_div v n)
      else if n = 0. then Option.none
      else
        let r = v /. n in
        if Float.is_finite r then Option.some (rebuild r) else Option.none
  | Add | Sub -> Option.none

(* Combine the coefficients of two same-unit typed leaves: [*] and [/] of two
   typed operands is not a typed value, so only [+] and [-] fold. *)
let combine_leaf (op : calc_op) x y =
  match op with
  | Add -> Option.some (x +. y)
  | Sub -> Option.some (x -. y)
  | Mul | Div -> Option.none

let numeric_leaf_value : type a. a calc -> float option = function
  | Num n -> Some n
  | Math_const c -> Some (math_const_value c)
  | _ -> None

let fold_zero_numeric_expr : type a.
    a calc -> calc_op -> a calc -> a calc option =
 fun l op r ->
  match (numeric_leaf_value l, numeric_leaf_value r) with
  | Some a, Some b -> (
      let value =
        match op with
        | Add -> Some (a +. b)
        | Sub -> Some (a -. b)
        | Mul -> Some (a *. b)
        | Div when b <> 0. -> Some (a /. b)
        | Div -> None
      in
      match value with Some 0. -> Some (Num 0.) | _ -> None)
  | _ -> None

(* CSS Values 4 sec. 10.10.1 value-independent multiplicative calc identities:
   these fold even across a kept [var()] ([var * 1 = var]). A zero sum term is
   deliberately not an identity here: the spec only combines sum children with
   identical units and warns that removing any other zero can change the calc's
   type. Zero-producing products take the type's canonical zero from [~zero],
   and [~is_zero] recognises a typed zero leaf ([0px], not just [0]). Operands
   are already evaluated; numeric op numeric is the caller's job. *)
let calc_identity : type a.
    zero:a calc ->
    is_zero:(a -> bool) ->
    a calc ->
    calc_op ->
    a calc ->
    a calc option =
 fun ~zero ~is_zero l op r ->
  match (l, op, r) with
  | _, Mul, Num 1. | _, Div, Num 1. -> Some l
  | Num 1., Mul, _ -> Some r
  | _, Mul, Num 0. | Num 0., Mul, _ -> Some zero
  (* A typed zero ([0px]) keeps its own spelling here; the optimizer's
     zero-length strip canonicalises it, so a non-optimised serialisation still
     reads [0px] rather than a bare [0]. *)
  | (Val v as z), Mul, _ when is_zero v -> Some z
  | _, Mul, (Val v as z) when is_zero v -> Some z
  | _ -> None

(* CSS Values 4 10.7 structural simplification of a typed calc AST. Folds [Expr
   (Num _, op, Num _)] subtrees and the value-independent identities
   ([calc_identity]). The per-type evaluators ([eval_length_calc] /
   [eval_lp_calc]) add the typed [Val] combinations ([1px + 2px], [2px * 3]) and
   pass their own [~zero] / [~is_zero] so a typed zero collapses too. *)
(* Context threaded through the calc simplifier. [var_is_single_valued n] reports
   whether [--n] has a single-component [@property] syntax, so [calc(var(--n))]
   substitutes one term and its redundant grouping may be dropped. The default
   knows nothing, so every such rewrite is a no-op. *)
type calc_ctx = { var_is_single_valued : string -> bool }

let default_calc_ctx = { var_is_single_valued = (fun _ -> false) }

(* A nested [calc()] or a parenthesised group around a single leaf is redundant
   grouping: drop it. A [var()] leaf is only safe to unwrap when the var is
   single-valued; otherwise its substitution could be a multi-term expression
   that needs the grouping (CSS Values 4 sec. 10.10). *)
let unwrap_grouping : type a.
    ctx:calc_ctx -> rewrap:(a calc -> a calc) -> a calc -> a calc =
 fun ~ctx ~rewrap reduced ->
  match reduced with
  | (Val _ | Num _ | Math_const _) as leaf -> leaf
  | Var v as leaf when ctx.var_is_single_valued v.name -> leaf
  | reduced -> rewrap reduced

let rec eval_calc_inner : type a. ctx:calc_ctx -> a calc -> a calc =
 fun ~ctx -> function
  | (Num _ | Val _ | Var _ | Sibling_index | Sibling_count) as leaf -> leaf
  | Math_const _ as leaf -> leaf
  | Math_fn fn when math_fn_contains_var fn -> Math_fn fn
  | Math_fn fn -> (
      (* This fold carries no type to rebuild a unit with, so a call CSS types
         as its arguments ([abs()], [hypot()]) keeps its spelling rather than
         collapsing to a coefficient the unit has fallen off. *)
      match fold_math_result (math_fn_result fn) with
      | Some (Scalar v) -> Num v
      | Some (United _) | None -> Math_fn fn)
  | Nested inner ->
      unwrap_grouping ~ctx
        ~rewrap:(fun r -> Nested r)
        (eval_calc_inner ~ctx inner)
  | Parens inner ->
      unwrap_grouping ~ctx
        ~rewrap:(fun r -> Parens r)
        (eval_calc_inner ~ctx inner)
  | Expr (l, op, r) -> (
      let l = eval_calc_inner ~ctx l in
      let r = eval_calc_inner ~ctx r in
      (* Match on the const-folded operands but keep [l] / [r] in the no-fold
         results, so an unreduced expression keeps the short [calc(.. pi ..)].
         Identity rules need a type-aware [Val] inspection (runtime subst?);
         per-type evaluators own them. *)
      let lc = calc_operand_value l in
      let rc = calc_operand_value r in
      match (lc, op, rc) with
      | Num a, Add, Num b -> calc_num (a +. b)
      | Num a, Sub, Num b -> calc_num (a -. b)
      | Num a, Mul, Num b -> calc_num (a *. b)
      | Num a, Div, Num b -> (
          match exact_div a b with
          | Some q -> calc_num q
          | None -> Expr (l, op, r))
      | _ -> (
          match
            calc_identity ~zero:(Num 0.) ~is_zero:(fun _ -> false) l op r
          with
          | Some folded -> folded
          | None -> (
              match fold_zero_numeric_expr lc op rc with
              | Some zero -> zero
              | None -> Expr (l, op, r))))

let eval_calc ?(ctx = default_calc_ctx) calc =
  normalize_calc_parens (eval_calc_inner ~ctx calc)

(* A subtree Cascade has to evaluate to a float of its own: a math constant or a
   math function. A fold that consumes one lands a computed coefficient in the
   typed leaf, so that leaf takes [round_computed]; a fold over authored
   operands alone keeps their digits. *)
let rec calc_is_computed : type a. a calc -> bool = function
  | Math_const _ | Math_fn _ -> true
  | Nested inner | Parens inner -> calc_is_computed inner
  | Expr (left, _, right) -> calc_is_computed left || calc_is_computed right
  | Num _ | Val _ | Var _ | Sibling_index | Sibling_count -> false

(* Spend the [round] budget on a typed value Cascade computed, once nothing more
   will fold it. A value the author wrote keeps its digits. *)
let settle_computed : type a. (a -> a) -> a calc * bool -> a calc =
 fun round (reduced, computed) ->
  match reduced with Val v when computed -> Val (round v) | reduced -> reduced

(* Fold one [Expr] over operands that are already reduced. [lf] / [rf] pair each
   operand with its own provenance flag; the result carries the flag of the
   typed value it produces, and an operand a kept [calc()] will print settles
   here because nothing further will fold it. *)
let fold_typed_expr : type a.
    scale:(exact:bool -> calc_op -> a -> float -> a option) ->
    combine:(calc_op -> a -> a -> a option) ->
    round:(a -> a) ->
    is_zero:(a -> bool) ->
    zero:a calc ->
    computed:bool ->
    a calc * bool ->
    calc_op ->
    a calc * bool ->
    a calc * bool =
 fun ~scale ~combine ~round ~is_zero ~zero ~computed lf op rf ->
  let l = fst lf and r = fst rf in
  let lc = calc_operand_value l in
  let rc = calc_operand_value r in
  let keep () =
    (Expr (settle_computed round lf, op, settle_computed round rf), false)
  in
  let value v = (Val v, computed) in
  let zero_fold = fold_zero_numeric_expr lc op rc in
  match (lc, op, rc, zero_fold) with
  | Num a, Add, Num b, _ -> (calc_num (a +. b), false)
  | Num a, Sub, Num b, _ -> (calc_num (a -. b), false)
  | Num a, Mul, Num b, _ -> (calc_num (a *. b), false)
  | Num a, Div, Num b, _ -> (
      match exact_div a b with Some q -> (calc_num q, false) | None -> keep ())
  | _, _, _, Some folded -> (folded, false)
  | Val a, op, Val b, None -> (
      match combine op a b with Some v -> value v | None -> keep ())
  | Val _, Div, Num 0., None -> keep ()
  | Val a, ((Mul | Div) as op), Num n, None -> (
      let exact = match r with Math_const _ -> false | _ -> true in
      match scale ~exact op a n with Some v -> value v | None -> keep ())
  | Num n, Mul, Val a, None -> (
      match scale ~exact:true Mul a n with Some v -> value v | None -> keep ())
  (* CSS Values 4 sec. 10.10.1: [1 / (1 / x)] cancels the double inversion. *)
  | Num 1., Div, Parens (Expr (Num 1., Div, x)), None -> (x, false)
  | Num 1., Div, Expr (Num 1., Div, x), None -> (x, false)
  | _, _, _, None -> (
      (* An identity returns one operand verbatim, so the surviving value keeps
         that operand's provenance rather than the expression's. *)
      match calc_identity ~zero ~is_zero l op r with
      | Some folded -> (folded, snd lf || snd rf)
      | None -> keep ())

(* CSS Values 4 sec. 10.10.1 typed [calc()] reduction, shared by every
   dimensioned type: the [Expr] dispatch is identical, only the per-type leaves
   differ and are passed in ([math_fn], [scale], [combine], [round], [zero] /
   [is_zero] for the identities). A shape with no [scale] / [combine] returns
   [None] and the [calc()] is kept verbatim. *)
let eval_typed_calc : type a.
    math_fn:(math_fn -> a calc) ->
    scale:(exact:bool -> calc_op -> a -> float -> a option) ->
    combine:(calc_op -> a -> a -> a option) ->
    round:(a -> a) ->
    is_zero:(a -> bool) ->
    zero:a calc ->
    ctx:calc_ctx ->
    a calc ->
    a calc =
 fun ~math_fn ~scale ~combine ~round ~is_zero ~zero ~ctx calc ->
  (* [reduce] pairs the reduced tree with a flag: its root is a typed value
     Cascade computed rather than one the author wrote. The budget is spent once
     nothing more will fold that root - at the top, or where it is embedded in a
     [calc()] that stays. Rounding an intermediate product instead feeds the
     shortened coefficient back into the next fold and compounds the error:
     [calc(100px * sqrt(2) * sqrt(2))] is still [200px]. *)
  let rec reduce (calc : a calc) : a calc * bool =
    match calc with
    | (Num _ | Val _ | Var _ | Sibling_index | Sibling_count) as leaf ->
        (leaf, false)
    | Math_const _ as leaf -> (leaf, false)
    | Math_fn fn when math_fn_contains_var fn -> (Math_fn fn, false)
    | Math_fn fn -> (
        match math_fn fn with
        | Val _ as reduced -> (reduced, true)
        | reduced -> (reduced, false))
    | Nested inner -> wrap (fun r -> Nested r) inner
    | Parens inner -> wrap (fun r -> Parens r) inner
    | Expr (left, op, right) ->
        let computed = calc_is_computed left || calc_is_computed right in
        fold_typed_expr ~scale ~combine ~round ~is_zero ~zero ~computed
          (reduce left) op (reduce right)
  and wrap rewrap inner =
    let reduced, computed = reduce inner in
    (unwrap_grouping ~ctx ~rewrap reduced, computed)
  in
  normalize_calc_parens (settle_computed round (reduce calc))

let pp_calc_with : type a. ?unwrap_num:bool -> a Pp.t -> a calc Pp.t =
 fun ?(unwrap_num = true) pp_value ctx calc ->
  match calc with
  (* CSS Values 4 sec. 10.10: a [var()] inside [calc()] is a runtime
     substitution boundary - the substituted tokens go through calc's typed
     grammar, not the surrounding property's grammar. Unwrapping
     [calc(var(--x))] to bare [var(--x)] would change which substitution shape
     is valid. *)
  | Val v when Pp.minified ctx -> pp_value ctx v
  | Num n when Pp.minified ctx && unwrap_num -> Pp.float ctx n
  | _ ->
      let ctx = { ctx with in_calc = true } in
      Pp.call "calc" (pp_calc_contents pp_value) ctx calc

let pp_calc pp_value ctx calc = pp_calc_with pp_value ctx calc

(* Small helpers *)

let pp_unit ?always:_ ctx f suffix =
  (* Pure serialiser: the unit is always kept, even on a zero. Dropping it
     ([0px] -> [0]) changes the node ([Px 0.] -> [Zero]) and is gated to
     top-level (a calc / function operand keeps its unit), so it is an AST
     rewrite in the optimize pass, not a printer choice. *)
  (* CSSOM serialization (CSS Values 4 6.7.2) drops a leading zero on fractional
     values ([.25rem], not [0.25rem]) in both modes. The coefficient itself
     prints in full, like [Pp.pct]: rounding it changes the value, so the
     6-significant-figure budget ([round_computed]) belongs to the fold that
     computes a coefficient, not to the printer that serialises one. *)
  if Float.is_nan f then Pp.nan_value ctx suffix
  else (
    Pp.string ctx (Pp.string_of_float ~drop_leading_zero:true f);
    Pp.string ctx suffix)

(** Try to evaluate a calc expression containing only numbers to a float.
    Returns None if the expression contains variables or non-numeric values. *)
let rec eval_numeric_calc : type a. a calc -> float option = function
  | Num f -> Some f
  | Math_const c -> Some (math_const_value c)
  | Math_fn fn -> eval_math_fn fn
  | Sibling_index -> None
  | Sibling_count -> None
  | Val _ -> None (* Can't evaluate typed values *)
  | Var _ -> None (* Can't evaluate variables *)
  | Nested inner -> eval_numeric_calc inner
  | Parens inner -> eval_numeric_calc inner
  | Expr (left, op, right) -> (
      match (eval_numeric_calc left, eval_numeric_calc right) with
      | Some l, Some r -> (
          match op with
          | Add -> Some (l +. r)
          | Sub -> Some (l -. r)
          | Mul -> Some (l *. r)
          | Div when r <> 0.0 -> Some (l /. r)
          | Div -> None)
      | _ -> None)

(** Map [f] over every [Val] leaf, preserving the calc structure. Useful when
    the caller wants to simplify a typed calc by applying a per-type rewrite
    (e.g., [Length.simplify]) at every leaf without re-tokenising. *)
let rec map_calc : type a b. (a -> b) -> a calc -> b calc =
 fun f calc ->
  match calc with
  | Val v -> Val (f v)
  | Var v ->
      (* Fallbacks carry an [a]-typed value too. *)
      let fallback : b fallback =
        match v.fallback with
        | Empty -> Empty
        | Empty2 -> Empty2
        | None -> None
        | Fallback x -> Fallback (f x)
        | Syntax_fallback value -> Syntax_fallback value
        | Var_fallback s -> Var_fallback s
      in
      let default = Option.map f v.default in
      Var
        {
          name = v.name;
          fallback;
          default;
          layer = v.layer;
          meta = v.meta;
          runtime = v.runtime;
        }
  | Num f -> Num f
  | Sibling_index -> Sibling_index
  | Sibling_count -> Sibling_count
  | Math_const c -> Math_const c
  | Math_fn fn -> Math_fn fn
  | Nested inner -> Nested (map_calc f inner)
  | Parens inner -> Parens (map_calc f inner)
  | Expr (l, op, r) -> Expr (map_calc f l, op, map_calc f r)

(** Partial sibling of {!map_calc}: [Val] leaves go through [f] and the whole
    tree fails when one of them does. A [Var] / [Sibling_index] /
    [Sibling_count] node carries no leaf to retype, so it fails too. *)
let rec map_calc_opt : type a b. (a -> b option) -> a calc -> b calc option =
 fun f calc ->
  match calc with
  | Val v -> Option.map (fun v -> Val v) (f v)
  | Num n -> Some (Num n)
  | Math_const c -> Some (Math_const c)
  | Math_fn fn -> Some (Math_fn fn)
  | Var _ | Sibling_index | Sibling_count -> None
  | Nested inner -> Option.map (fun i -> Nested i) (map_calc_opt f inner)
  | Parens inner -> Option.map (fun i -> Parens i) (map_calc_opt f inner)
  | Expr (left, op, right) -> (
      match (map_calc_opt f left, map_calc_opt f right) with
      | Some left, Some right -> Some (Expr (left, op, right))
      | _ -> None)

(* Top-level commas in math-function args round-trip differently in pretty vs
   minified mode: minified strips space after comma, pretty inserts ", ". Walk
   the raw arg string with a paren-depth counter so commas inside nested calls
   are left untouched. *)
(* Recognise literal zeros for top-level canonicalisation and typed products.
   This does not make them interchangeable in a calc sum: CSS Values 4 sec.
   10.10.1 only combines identical units and requires other zero terms to stay. *)
let length_is_zero = function
  | Zero -> true
  | Px f | Cm f | Mm f | Q f | In f | Pt f | Pc f -> f = 0.
  | Em f | Rem f | Ex f | Cap f | Ic f | Ric f | Rlh f -> f = 0.
  | Ch f | Lh f -> f = 0.
  | Pct f -> f = 0.
  | Vw f | Vh f | Vmin f | Vmax f | Vi f | Vb f -> f = 0.
  | Dvh f | Dvw f | Dvmin f | Dvmax f -> f = 0.
  | Lvh f | Lvw f | Lvmin f | Lvmax f -> f = 0.
  | Svh f | Svw f | Svmin f | Svmax f -> f = 0.
  | Cqw f | Cqh f | Cqi f | Cqb f | Cqmin f | Cqmax f -> f = 0.
  | Dimension { value; _ } -> value = 0.
  | _ -> false

let calc_length_unit = function
  | Zero -> Some ("px", 0.)
  | Px n -> Some ("px", n)
  | Cm n -> Some ("cm", n)
  | Mm n -> Some ("mm", n)
  | Q n -> Some ("q", n)
  | In n -> Some ("in", n)
  | Pt n -> Some ("pt", n)
  | Pc n -> Some ("pc", n)
  | Rem n -> Some ("rem", n)
  | Em n -> Some ("em", n)
  | Ex n -> Some ("ex", n)
  | Cap n -> Some ("cap", n)
  | Ic n -> Some ("ic", n)
  | Ric n -> Some ("ric", n)
  | Rlh n -> Some ("rlh", n)
  | Ch n -> Some ("ch", n)
  | Lh n -> Some ("lh", n)
  | Pct n -> Some ("%", n)
  | Vw n -> Some ("vw", n)
  | Vh n -> Some ("vh", n)
  | Vmin n -> Some ("vmin", n)
  | Vmax n -> Some ("vmax", n)
  | Vi n -> Some ("vi", n)
  | Vb n -> Some ("vb", n)
  | Dvh n -> Some ("dvh", n)
  | Dvw n -> Some ("dvw", n)
  | Dvmin n -> Some ("dvmin", n)
  | Dvmax n -> Some ("dvmax", n)
  | Lvh n -> Some ("lvh", n)
  | Lvw n -> Some ("lvw", n)
  | Lvmin n -> Some ("lvmin", n)
  | Lvmax n -> Some ("lvmax", n)
  | Svh n -> Some ("svh", n)
  | Svw n -> Some ("svw", n)
  | Svmin n -> Some ("svmin", n)
  | Svmax n -> Some ("svmax", n)
  | Cqw n -> Some ("cqw", n)
  | Cqh n -> Some ("cqh", n)
  | Cqi n -> Some ("cqi", n)
  | Cqb n -> Some ("cqb", n)
  | Cqmin n -> Some ("cqmin", n)
  | Cqmax n -> Some ("cqmax", n)
  | Dimension { value; unit; _ } -> Some (String.lowercase_ascii unit, value)
  | _ -> None

let length_from_calc_unit unit value =
  if String.equal unit "px" && value = 0. then Zero
  else Dimension { value; unit; repr = Pp.string_of_float value }

let length_of_default_string value : length option =
  match
    let t = Cursor.of_string value in
    let value, unit = Cursor.number_with_unit t in
    Cursor.ws t;
    Cursor.expect_eof t;
    match unit with
    | Option.Some unit ->
        Option.Some (length_from_calc_unit (String.lowercase_ascii unit) value)
    | Option.None when value = 0. -> Option.Some Zero
    | Option.None -> Option.None
  with
  | value -> value
  | exception Cursor.Parse_error _ -> None

let fallback_of_var_resolution _parse_default : 'a fallback -> 'a option =
  function
  | Fallback value -> Option.Some value
  | Var_fallback _ | None | Empty | Empty2 | Syntax_fallback _ -> Option.None

(* Var resolution at print time covers only [inline] mode, which reads the
   already-resolved [v.default] from the AST. Theme resolution is a transform
   that runs earlier, so the printer never consults a resolver. *)
let value_of_var_resolution ctx parse_default (v : 'a var) : 'a option =
  if ctx.Pp.inline then
    Option.fold
      ~none:(fallback_of_var_resolution parse_default v.fallback)
      ~some:(fun value -> Option.Some value)
      v.default
  else Option.None

let length_of_var_resolution ctx (v : length var) =
  value_of_var_resolution ctx length_of_default_string v

let rec resolve_length_calc_vars ctx : length calc -> length calc = function
  | Var v -> (
      match length_of_var_resolution ctx v with
      | Option.Some value -> Val value
      | Option.None -> Var v)
  | Nested inner -> Nested (resolve_length_calc_vars ctx inner)
  | Parens inner -> Parens (resolve_length_calc_vars ctx inner)
  | Expr (left, op, right) ->
      Expr
        ( resolve_length_calc_vars ctx left,
          op,
          resolve_length_calc_vars ctx right )
  | (Val _ | Num _ | Math_const _ | Sibling_index | Sibling_count | Math_fn _)
    as leaf ->
      leaf

(* CSS Values 4 sec. 6.2: the absolute lengths share px as a canonical unit. *)
let absolute_unit_px_ratio = function
  | "px" -> Some 1.
  | "in" -> Some 96.
  | "cm" -> Some (96. /. 2.54)
  | "mm" -> Some (96. /. 2.54 /. 10.)
  | "q" -> Some (96. /. 2.54 /. 40.)
  | "pt" -> Some (96. /. 72.)
  | "pc" -> Some (96. /. 6.)
  | _ -> None

(* CSS Values 4 sec. 10.10.1: same-unit add/sub of two typed lengths reduces to
   a single length. Mixed cases reduce when both operands are absolute units
   (px-compatible per sec. 6.2) by combining in the canonical px form; relative
   units (em/rem/vw/...) still require cascade context and stay unfolded. *)
let length_combine op v1 v2 =
  let combine a b =
    match op with Add -> a +. b | Sub -> a -. b | Mul | Div -> nan
  in
  match (op, calc_length_unit v1, calc_length_unit v2) with
  | (Add | Sub), Some (unit1, a), Some (unit2, b) when String.equal unit1 unit2
    ->
      Some (length_from_calc_unit unit1 (combine a b))
  | (Add | Sub), Some (unit1, a), Some (unit2, b) -> (
      match (absolute_unit_px_ratio unit1, absolute_unit_px_ratio unit2) with
      | Some r1, Some r2 ->
          (* The px form is Cascade's own arithmetic, not the authored one. *)
          Some
            (length_from_calc_unit "px"
               (round_computed (combine (a *. r1) (b *. r2))))
      | _ -> None)
  | _ -> None

(* CSS Values 4 sec. 10.10.1: a unitless factor scales a typed length, unit
   unchanged. Division folds only when [exact_div] is exact, else the [calc()]
   is kept to avoid precision loss. Exception: an irrational divisor (a math
   constant [pi] / [e] / ...) can never be exact, and keeping [calc()] preserves
   no more precision than the browser computes, so fold to the rounded value. *)
let length_scale ?(exact = true) op v n =
  if not (Float.is_finite n) then Option.none
  else
    match (op, calc_length_unit v) with
    | Mul, Some (unit, value) ->
        let r = value *. n in
        if Float.is_finite r then Option.some (length_from_calc_unit unit r)
        else Option.none
    | Div, Some (unit, value) ->
        if exact then
          Option.map (length_from_calc_unit unit) (exact_div value n)
        else if n = 0. then Option.none
        else
          let r = value /. n in
          if Float.is_finite r then Option.some (length_from_calc_unit unit r)
          else Option.none
    | _ -> Option.none

(* CSS Values 4 sec. 10.10: identity-rule simplifications around a runtime
   substitution would change the substituted-grammar context. *)
(* A [-webkit-] / [-moz-] intrinsic sizing keyword, the legacy fallback an
   author pairs with the unprefixed form ([width:-webkit-max-content;
   width:max-content]). *)
let length_is_vendor_prefixed_sizing : length -> bool = function
  | Webkit_max_content | Webkit_min_content | Webkit_fit_content
  | Moz_max_content | Moz_min_content | Moz_fit_content ->
      true
  | _ -> false

let length_percentage_is_vendor_prefixed : length_percentage -> bool = function
  | Length l -> length_is_vendor_prefixed_sizing l
  | _ -> false

let rec length_has_runtime_subst : length -> bool = function
  | Var _ | Env _ | Attr _ | Anchor _ | Anchor_size _ -> true
  | Calc c -> length_calc_has_runtime_subst c
  | _ -> false

and length_calc_has_runtime_subst : length calc -> bool = function
  | Var _ -> true
  | Val v -> length_has_runtime_subst v
  | Num _ | Math_const _ | Sibling_index | Sibling_count -> false
  | Math_fn fn -> math_fn_contains_var fn
  | Nested inner | Parens inner -> length_calc_has_runtime_subst inner
  | Expr (l, _, r) ->
      length_calc_has_runtime_subst l || length_calc_has_runtime_subst r

(* Reduce a computed coefficient to the serialised precision, returning the leaf
   unchanged when every digit already prints. A [<percentage>] is left alone:
   [Pp.pct] prints its coefficient in full in both modes, so there is no budget
   to apply. *)
let length_round (l : length) : length =
  match l with
  | Pct _ -> l
  | _ -> (
      match calc_length_unit l with
      | Some (unit, value) ->
          let rounded = round_computed value in
          if rounded = value then l else length_from_calc_unit unit rounded
      | None -> l)

(* CSS Values 4 sec. 10.6: [abs()] and [hypot()] preserve the input's type, so
   [hypot(<length>, <length>)] returns a [<length>] and the [Val] is rebuilt
   from the unit [math_fn_result] carries out. *)
let length_calc_math_fn (fn : math_fn) : length calc =
  match (fn, fold_united_nan (math_fn_result fn)) with
  (* [sin()] / [cos()] reduce to a coefficient no shorter than the call, and it
     is not a [<length>] either way, so the call stays. *)
  | (Sin _ | Cos _), _ -> Math_fn fn
  | _, Some (United (v, unit)) -> Val (length_from_calc_unit unit v)
  | _, Some (Scalar v) -> Num v
  | _, None -> Math_fn fn

let eval_length_calc ?(ctx = default_calc_ctx) (c : length calc) : length calc =
  eval_typed_calc ~math_fn:length_calc_math_fn
    ~scale:(fun ~exact op v n -> length_scale ~exact op v n)
    ~combine:length_combine ~round:length_round ~is_zero:length_is_zero
    ~zero:(Val Zero) ~ctx c

type length_unit =
  | Px
  | Cm
  | Mm
  | Q
  | In
  | Pt
  | Pc
  | Rem
  | Em
  | Ex
  | Cap
  | Ic
  | Ric
  | Rlh
  | Ch
  | Lh
  | Pct
  | Vw
  | Vh
  | Vmin
  | Vmax
  | Vi
  | Vb
  | Dvh
  | Dvw
  | Dvmin
  | Dvmax
  | Lvh
  | Lvw
  | Lvmin
  | Lvmax
  | Svh
  | Svw
  | Svmin
  | Svmax
  | Cqw
  | Cqh
  | Cqi
  | Cqb
  | Cqmin
  | Cqmax

let length_unit_is_pct = function Pct -> true | _ -> false

let length_unit_is_viewport = function
  | Vw | Vh | Vmin | Vmax | Vi | Vb | Dvh | Dvw | Dvmin | Dvmax | Lvh | Lvw
  | Lvmin | Lvmax | Svh | Svw | Svmin | Svmax ->
      true
  | _ -> false

let length_unit_is_font_relative = function
  | Rem | Em | Ex | Cap | Ic | Ric | Rlh | Ch | Lh -> true
  | _ -> false

let length_unit_negative_rank = function
  | unit when length_unit_is_font_relative unit -> 0
  | _ -> 1

let unit_of_string = function
  | "px" -> Some Px
  | "cm" -> Some Cm
  | "mm" -> Some Mm
  | "q" -> Some Q
  | "in" -> Some In
  | "pt" -> Some Pt
  | "pc" -> Some Pc
  | "rem" -> Some Rem
  | "em" -> Some Em
  | "ex" -> Some Ex
  | "cap" -> Some Cap
  | "ic" -> Some Ic
  | "ric" -> Some Ric
  | "rlh" -> Some Rlh
  | "ch" -> Some Ch
  | "lh" -> Some Lh
  | "%" -> Some Pct
  | "vw" -> Some Vw
  | "vh" -> Some Vh
  | "vmin" -> Some Vmin
  | "vmax" -> Some Vmax
  | "vi" -> Some Vi
  | "vb" -> Some Vb
  | "dvh" -> Some Dvh
  | "dvw" -> Some Dvw
  | "dvmin" -> Some Dvmin
  | "dvmax" -> Some Dvmax
  | "lvh" -> Some Lvh
  | "lvw" -> Some Lvw
  | "lvmin" -> Some Lvmin
  | "lvmax" -> Some Lvmax
  | "svh" -> Some Svh
  | "svw" -> Some Svw
  | "svmin" -> Some Svmin
  | "svmax" -> Some Svmax
  | "cqw" -> Some Cqw
  | "cqh" -> Some Cqh
  | "cqi" -> Some Cqi
  | "cqb" -> Some Cqb
  | "cqmin" -> Some Cqmin
  | "cqmax" -> Some Cqmax
  | _ -> None

let unit_of_length = function
  | Zero -> Some (Px, 0.)
  | Px n -> Some (Px, n)
  | Cm n -> Some (Cm, n)
  | Mm n -> Some (Mm, n)
  | Q n -> Some (Q, n)
  | In n -> Some (In, n)
  | Pt n -> Some (Pt, n)
  | Pc n -> Some (Pc, n)
  | Rem n -> Some (Rem, n)
  | Em n -> Some (Em, n)
  | Ex n -> Some (Ex, n)
  | Cap n -> Some (Cap, n)
  | Ic n -> Some (Ic, n)
  | Ric n -> Some (Ric, n)
  | Rlh n -> Some (Rlh, n)
  | Ch n -> Some (Ch, n)
  | Lh n -> Some (Lh, n)
  | Pct n -> Some (Pct, n)
  | Vw n -> Some (Vw, n)
  | Vh n -> Some (Vh, n)
  | Vmin n -> Some (Vmin, n)
  | Vmax n -> Some (Vmax, n)
  | Vi n -> Some (Vi, n)
  | Vb n -> Some (Vb, n)
  | Dvh n -> Some (Dvh, n)
  | Dvw n -> Some (Dvw, n)
  | Dvmin n -> Some (Dvmin, n)
  | Dvmax n -> Some (Dvmax, n)
  | Lvh n -> Some (Lvh, n)
  | Lvw n -> Some (Lvw, n)
  | Lvmin n -> Some (Lvmin, n)
  | Lvmax n -> Some (Lvmax, n)
  | Svh n -> Some (Svh, n)
  | Svw n -> Some (Svw, n)
  | Svmin n -> Some (Svmin, n)
  | Svmax n -> Some (Svmax, n)
  | Cqw n -> Some (Cqw, n)
  | Cqh n -> Some (Cqh, n)
  | Cqi n -> Some (Cqi, n)
  | Cqb n -> Some (Cqb, n)
  | Cqmin n -> Some (Cqmin, n)
  | Cqmax n -> Some (Cqmax, n)
  | Dimension { value; unit; _ } -> (
      match unit_of_string (String.lowercase_ascii unit) with
      | Some unit -> Some (unit, value)
      | None -> None)
  | _ -> None

let length_of_unit unit n =
  match unit with
  | Px -> if n = 0. then Zero else Px n
  | Cm -> Cm n
  | Mm -> Mm n
  | Q -> Q n
  | In -> In n
  | Pt -> Pt n
  | Pc -> Pc n
  | Rem -> Rem n
  | Em -> Em n
  | Ex -> Ex n
  | Cap -> Cap n
  | Ic -> Ic n
  | Ric -> Ric n
  | Rlh -> Rlh n
  | Ch -> Ch n
  | Lh -> Lh n
  | Pct -> Pct n
  | Vw -> Vw n
  | Vh -> Vh n
  | Vmin -> Vmin n
  | Vmax -> Vmax n
  | Vi -> Vi n
  | Vb -> Vb n
  | Dvh -> Dvh n
  | Dvw -> Dvw n
  | Dvmin -> Dvmin n
  | Dvmax -> Dvmax n
  | Lvh -> Lvh n
  | Lvw -> Lvw n
  | Lvmin -> Lvmin n
  | Lvmax -> Lvmax n
  | Svh -> Svh n
  | Svw -> Svw n
  | Svmin -> Svmin n
  | Svmax -> Svmax n
  | Cqw -> Cqw n
  | Cqh -> Cqh n
  | Cqi -> Cqi n
  | Cqb -> Cqb n
  | Cqmin -> Cqmin n
  | Cqmax -> Cqmax n

(* A zero [px] term inside calc() must retain its unit. Only a completed
   top-level [<length>] can use the shorter unitless-zero representation. *)
let length_of_calc_unit (unit : length_unit) n : length =
  match unit with Px -> Px n | _ -> length_of_unit unit n

type linear_term = {
  unit : length_unit;
  value : float;
  first_pos : int;
  count : int;
}

let linear_term_priority ~first_pos term =
  match (term.value > 0., length_unit_is_pct term.unit) with
  | true, true -> 0
  | true, false when term.first_pos = first_pos && term.count = 1 -> 1
  | false, false when not (length_unit_is_viewport term.unit) -> 2
  | true, false when term.count = 1 -> 3
  | true, false -> 4
  | false, false -> 5
  | false, true -> 6

let compare_negative_linear_term a b =
  let c =
    compare
      (length_unit_negative_rank a.unit)
      (length_unit_negative_rank b.unit)
  in
  if c <> 0 then c else compare a.first_pos b.first_pos

let compare_linear_term ~first_pos a b =
  let a_priority = linear_term_priority ~first_pos a in
  let b_priority = linear_term_priority ~first_pos b in
  let c = compare a_priority b_priority in
  if c <> 0 then c
  else if a_priority = 2 then compare_negative_linear_term a b
  else compare a.first_pos b.first_pos

let ordered_linear_terms terms =
  let table = Hashtbl.create 8 in
  List.iteri
    (fun pos (unit, n) ->
      match Hashtbl.find_opt table unit with
      | None ->
          Hashtbl.add table unit { unit; value = n; first_pos = pos; count = 1 }
      | Some term ->
          Hashtbl.replace table unit
            { term with value = term.value +. n; count = term.count + 1 })
    terms;
  (* CSS Values 4 sec. 10.10.1 combines identical units, but explicitly keeps
     zero-valued sum children otherwise: their unit contributes to the calc's
     type. The table has already combined each identical-unit set, so retain
     every resulting term, including zero. *)
  let terms = Hashtbl.to_seq_values table |> List.of_seq in
  let first_pos =
    List.fold_left (fun acc term -> min acc term.first_pos) max_int terms
  in
  List.sort (compare_linear_term ~first_pos) terms

let linear_calc_op first_unit first_value unit n =
  if
    n < 0. && first_value > 0. && first_unit = Px
    && length_unit_is_font_relative unit
  then (Add, n)
  else if n < 0. then (Sub, -.n)
  else (Add, n)

let linear_terms_with unit_of_value calc =
  let scale factor terms =
    List.map (fun (unit, n) -> (unit, factor *. n)) terms
  in
  let rec aux = function
    | Val v -> Option.map (fun term -> [ term ]) (unit_of_value v)
    (* A unitless zero is still a [<number>] inside calc(), not a typed zero. *)
    | Num _ | Math_const _ | Var _ | Sibling_index | Sibling_count | Math_fn _
      ->
        None
    | Nested inner | Parens inner -> aux inner
    | Expr (left, Add, right) -> (
        match (aux left, aux right) with
        | Some l, Some r -> Some (l @ r)
        | _ -> None)
    | Expr (left, Sub, right) -> (
        match (aux left, aux right) with
        | Some l, Some r -> Some (l @ scale (-1.) r)
        | _ -> None)
    | Expr (left, Mul, Num n) -> Option.map (scale n) (aux left)
    | Expr (Num n, Mul, right) -> Option.map (scale n) (aux right)
    | Expr (left, Div, Num n) -> (
        match exact_div 1. n with
        | Some inv -> Option.map (scale inv) (aux left)
        | None -> None)
    | Expr _ -> None
  in
  aux calc

let linear_length_terms calc = linear_terms_with unit_of_length calc

let linear_length_calc calc =
  match linear_length_terms calc with
  | None -> calc
  | Some terms -> (
      let terms = ordered_linear_terms terms in
      match terms with
      | [] -> Val Zero
      | { unit; value = n; _ } :: rest ->
          let first_unit = unit in
          let first_value = n in
          List.fold_left
            (fun acc { unit; value = n; _ } ->
              let op, n = linear_calc_op first_unit first_value unit n in
              Expr (acc, op, Val (length_of_calc_unit unit n)))
            (Val (length_of_calc_unit unit n))
            rest)

let lp_of_default_string value : length_percentage option =
  match
    let t = Cursor.of_string value in
    let value, unit = Cursor.number_with_unit t in
    Cursor.ws t;
    Cursor.expect_eof t;
    match unit with
    | Option.Some "%" -> Option.Some (Pct value : length_percentage)
    | Option.Some unit ->
        Option.Some
          (Length (length_from_calc_unit (String.lowercase_ascii unit) value)
            : length_percentage)
    | Option.None when value = 0. ->
        Option.Some (Length Zero : length_percentage)
    | Option.None -> Option.None
  with
  | value -> value
  | exception Cursor.Parse_error _ -> Option.None

let lp_of_var_resolution ctx (v : length_percentage var) =
  value_of_var_resolution ctx lp_of_default_string v

let rec resolve_lp_calc_vars ctx :
    length_percentage calc -> length_percentage calc = function
  | Var v -> (
      match lp_of_var_resolution ctx v with
      | Option.Some value -> Val value
      | Option.None -> Var v)
  | Nested inner -> Nested (resolve_lp_calc_vars ctx inner)
  | Parens inner -> Parens (resolve_lp_calc_vars ctx inner)
  | Expr (left, op, right) ->
      Expr (resolve_lp_calc_vars ctx left, op, resolve_lp_calc_vars ctx right)
  | (Val _ | Num _ | Math_const _ | Sibling_index | Sibling_count | Math_fn _)
    as leaf ->
      leaf

let lp_combine op (v1 : length_percentage) (v2 : length_percentage) :
    length_percentage option =
  let combine a b =
    match op with Add -> a +. b | Sub -> a -. b | Mul | Div -> nan
  in
  match (op, v1, v2) with
  | (Add | Sub), Length a, Length b -> (
      match length_combine op a b with
      | Some v -> Some (Length v)
      | None -> None)
  | (Add | Sub), Pct a, Pct b -> Some (Pct (combine a b))
  | _ -> None

let lp_scale ?(exact = true) op (v : length_percentage) n :
    length_percentage option =
  match (op, v) with
  | (Mul | Div), Length a -> (
      match length_scale ~exact op a n with
      | Some lv -> Some (Length lv)
      | None -> None)
  | Mul, Pct a -> Some (Pct (a *. n) : length_percentage)
  | Div, Pct a ->
      if exact then
        Option.map (fun r -> (Pct r : length_percentage)) (exact_div a n)
      else if n = 0. then None
      else Some (Pct (a /. n) : length_percentage)
  | _ -> None

let lp_is_zero : length_percentage -> bool = function
  | Length l -> length_is_zero l
  | Pct f -> f = 0.
  | _ -> false

let lp_round : length_percentage -> length_percentage = function
  | Length l -> Length (length_round l)
  | lp -> lp

(* Same unit-preserving rule as [length_calc_math_fn], plus the percentage shape
   [abs(-5%)] keeps. *)
let lp_calc_math_fn (fn : math_fn) : length_percentage calc =
  match fold_united_nan (math_fn_result fn) with
  | Some (United (v, "%")) -> Val (Pct v : length_percentage)
  | _ -> (
      match length_calc_math_fn fn with
      | Val l -> Val (Length l : length_percentage)
      | Num n -> Num n
      | _ -> Math_fn fn)

let eval_lp_calc ?(ctx = default_calc_ctx) (c : length_percentage calc) :
    length_percentage calc =
  eval_typed_calc ~math_fn:lp_calc_math_fn
    ~scale:(fun ~exact op v n -> lp_scale ~exact op v n)
    ~combine:lp_combine ~round:lp_round ~is_zero:lp_is_zero
    ~zero:(Val (Length Zero)) ~ctx c

let unit_of_lp : length_percentage -> (length_unit * float) option = function
  | Pct n -> Some (Pct, n)
  | Length l -> unit_of_length l
  | Env _ | Var _ | Calc _ | Invalid _ -> None

let lp_of_unit unit n : length_percentage =
  match unit with Pct -> Pct n | unit -> Length (length_of_calc_unit unit n)

let linear_lp_terms calc = linear_terms_with unit_of_lp calc

let linear_lp_calc calc =
  match linear_lp_terms calc with
  | None -> calc
  | Some terms -> (
      let terms = ordered_linear_terms terms in
      match terms with
      | [] -> Val (Length Zero)
      | { unit; value = n; _ } :: rest ->
          let first_unit = unit in
          let first_value = n in
          List.fold_left
            (fun acc { unit; value = n; _ } ->
              let op, n = linear_calc_op first_unit first_value unit n in
              Expr (acc, op, Val (lp_of_unit unit n)))
            (Val (lp_of_unit unit n))
            rest)

let round_length_step strategy value step =
  match strategy with
  | "up" -> Float.ceil (value /. step) *. step
  | "down" -> Float.floor (value /. step) *. step
  | "to-zero" -> Float.trunc (value /. step) *. step
  | _ -> Float.round (value /. step) *. step

(* Typed math-call printer: emit [name(arg1,arg2,...)] from a typed list of
   length values, deferring to [pp_length] for each component (so nested
   [calc()] / [var()] / [min()] / [clamp()] argument shapes Just Work). *)
let pp_typed_math_call ctx name pp_arg args =
  Pp.string ctx name;
  Pp.char ctx '(';
  (* A calc-family math function's arguments are a typed-boundary calc context:
     keep the unit on a zero so [clamp(0px, 0em, 0vh)] stays unit-tagged rather
     than collapsing to the foldable-looking [clamp(0, 0, 0)] (which would lose
     round-trip stability). *)
  let ctx = { ctx with Pp.in_calc = true } in
  let sep ctx () =
    Pp.char ctx ',';
    if not (Pp.minified ctx) then Pp.char ctx ' '
  in
  Pp.list ~sep pp_arg ctx args;
  Pp.char ctx ')'

(* CSS Values 4 sec. 10.2 [min()] / [max()] reduce when every argument is a
   plain dimension of the same unit (so [min(1px, 2px)] -> [1px], [max(1em,
   .5em, 2em)] -> [2em]). Mixed units or any non-literal argument (variables,
   calc(), nested math) bail out and keep the function call. *)
let length_simple_dimension : length -> (float * length) option = function
  | Zero -> Some (0., Zero)
  | Px f -> Some (f, Px 0.)
  | Cm f -> Some (f, Cm 0.)
  | Mm f -> Some (f, Mm 0.)
  | Q f -> Some (f, Q 0.)
  | In f -> Some (f, In 0.)
  | Pt f -> Some (f, Pt 0.)
  | Pc f -> Some (f, Pc 0.)
  | Em f -> Some (f, Em 0.)
  | Rem f -> Some (f, Rem 0.)
  | Ex f -> Some (f, Ex 0.)
  | Cap f -> Some (f, Cap 0.)
  | Ic f -> Some (f, Ic 0.)
  | Ric f -> Some (f, Ric 0.)
  | Rlh f -> Some (f, Rlh 0.)
  | Vw f -> Some (f, Vw 0.)
  | Vh f -> Some (f, Vh 0.)
  | Vmin f -> Some (f, Vmin 0.)
  | Vmax f -> Some (f, Vmax 0.)
  | Vi f -> Some (f, Vi 0.)
  | Vb f -> Some (f, Vb 0.)
  | Ch f -> Some (f, Ch 0.)
  | Lh f -> Some (f, Lh 0.)
  | Pct f -> Some (f, Pct 0.)
  | _ -> None

let with_length_value l v : length =
  match l with
  | Zero | Px _ -> Px v
  | Cm _ -> Cm v
  | Mm _ -> Mm v
  | Q _ -> Q v
  | In _ -> In v
  | Pt _ -> Pt v
  | Pc _ -> Pc v
  | Em _ -> Em v
  | Rem _ -> Rem v
  | Ex _ -> Ex v
  | Cap _ -> Cap v
  | Ic _ -> Ic v
  | Ric _ -> Ric v
  | Rlh _ -> Rlh v
  | Vw _ -> Vw v
  | Vh _ -> Vh v
  | Vmin _ -> Vmin v
  | Vmax _ -> Vmax v
  | Vi _ -> Vi v
  | Vb _ -> Vb v
  | Ch _ -> Ch v
  | Lh _ -> Lh v
  | Pct _ -> Pct v
  | _ -> Px v

let rec reduce_length_min_max : length -> length = function
  | Min xs as orig -> (
      let xs = List.map reduce_length_min_max xs in
      match reduce_typed_min_max_simple xs Float.min with
      | Some v -> v
      | None ->
          if xs = match orig with Min ys -> ys | _ -> [] then orig else Min xs)
  | Max xs as orig -> (
      let xs = List.map reduce_length_min_max xs in
      match reduce_typed_min_max_simple xs Float.max with
      | Some v -> v
      | None ->
          if xs = match orig with Max ys -> ys | _ -> [] then orig else Max xs)
  | other -> other

and reduce_typed_min_max_simple (xs : length list) reduce : length option =
  let pairs = List.map length_simple_dimension xs in
  if List.exists Option.is_none pairs then None
  else
    let pairs = List.filter_map (fun x -> x) pairs in
    match pairs with
    | [] -> None
    | (_, sample) :: _ when List.for_all (fun (_, s) -> s = sample) pairs ->
        let values = List.map fst pairs in
        let reduced = List.fold_left reduce (List.hd values) (List.tl values) in
        Some (with_length_value sample reduced)
    | _ -> None

let try_reduce_typed_min_max xs reduce =
  let xs = List.map reduce_length_min_max xs in
  reduce_typed_min_max_simple xs reduce

let px_values values =
  List.fold_right
    (fun (value : length) acc ->
      match (value, acc) with
      | Px f, Some values -> Some (f :: values)
      | _ -> None)
    values (Some [])

let rec pp_length ?(always = false) : length Pp.t =
 fun ctx v ->
  let pp_unit_fn = pp_unit ~always ctx in
  match v with
  | Zero -> Pp.char ctx '0'
  | Px f -> pp_unit_fn f "px"
  | Cm f -> pp_unit_fn f "cm"
  | Mm f -> pp_unit_fn f "mm"
  | Q f -> pp_unit_fn f "q"
  | In f -> pp_unit_fn f "in"
  | Pt f -> pp_unit_fn f "pt"
  | Pc f -> pp_unit_fn f "pc"
  | Rem f -> pp_unit_fn f "rem"
  | Em f -> pp_unit_fn f "em"
  | Ex f -> pp_unit_fn f "ex"
  | Cap f -> pp_unit_fn f "cap"
  | Ic f -> pp_unit_fn f "ic"
  | Ric f -> pp_unit_fn f "ric"
  | Rlh f -> pp_unit_fn f "rlh"
  (* CSS Values 4 sec. 6: the unit-drop on a zero is for [<length>] only; [Pct
     0.] is a [<percentage>] zero and must keep [%] - dropping it would change
     the value's type from percentage to length. *)
  | Pct f -> Pp.pct ctx f
  | Vw f -> pp_unit_fn f "vw"
  | Vh f -> pp_unit_fn f "vh"
  | Vmin f -> pp_unit_fn f "vmin"
  | Vmax f -> pp_unit_fn f "vmax"
  | Vi f -> pp_unit_fn f "vi"
  | Vb f -> pp_unit_fn f "vb"
  | Dvh f -> pp_unit_fn f "dvh"
  | Dvw f -> pp_unit_fn f "dvw"
  | Dvmin f -> pp_unit_fn f "dvmin"
  | Dvmax f -> pp_unit_fn f "dvmax"
  | Lvh f -> pp_unit_fn f "lvh"
  | Lvw f -> pp_unit_fn f "lvw"
  | Lvmin f -> pp_unit_fn f "lvmin"
  | Lvmax f -> pp_unit_fn f "lvmax"
  | Svh f -> pp_unit_fn f "svh"
  | Svw f -> pp_unit_fn f "svw"
  | Svmin f -> pp_unit_fn f "svmin"
  | Svmax f -> pp_unit_fn f "svmax"
  | Cqw f -> pp_unit_fn f "cqw"
  | Cqh f -> pp_unit_fn f "cqh"
  | Cqi f -> pp_unit_fn f "cqi"
  | Cqb f -> pp_unit_fn f "cqb"
  | Cqmin f -> pp_unit_fn f "cqmin"
  | Cqmax f -> pp_unit_fn f "cqmax"
  | Ch f -> pp_unit_fn f "ch"
  | Lh f -> pp_unit_fn f "lh"
  | Dimension { value; unit; repr } ->
      if ctx.minify then pp_unit_fn value unit
      else (
        Pp.string ctx repr;
        Pp.string ctx unit)
  | Size -> Pp.string ctx "size"
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Normal -> Pp.string ctx "normal"
  | Inherit -> Pp.string ctx "inherit"
  | Fit_content -> Pp.string ctx "fit-content"
  | Fit_content_arg arg ->
      Pp.string ctx "fit-content(";
      pp_length ~always ctx arg;
      Pp.char ctx ')'
  | Contain -> Pp.string ctx "contain"
  | Max_content -> Pp.string ctx "max-content"
  | Min_content -> Pp.string ctx "min-content"
  | Webkit_max_content -> Pp.string ctx "-webkit-max-content"
  | Webkit_min_content -> Pp.string ctx "-webkit-min-content"
  | Webkit_fit_content -> Pp.string ctx "-webkit-fit-content"
  | Moz_max_content -> Pp.string ctx "-moz-max-content"
  | Moz_min_content -> Pp.string ctx "-moz-min-content"
  | Moz_fit_content -> Pp.string ctx "-moz-fit-content"
  | From_font -> Pp.string ctx "from-font"
  | Hairline -> Pp.string ctx "hairline"
  | Thin -> Pp.string ctx "thin"
  | Medium -> Pp.string ctx "medium"
  | Thick -> Pp.string ctx "thick"
  | Stretch -> Pp.string ctx "stretch"
  | Clamp (mn, v, mx) ->
      pp_typed_math_call ctx "clamp" (pp_length_math_arg ~always) [ mn; v; mx ]
  | Min xs -> pp_typed_math_call ctx "min" (pp_length_math_arg ~always) xs
  | Max xs -> pp_typed_math_call ctx "max" (pp_length_math_arg ~always) xs
  | Minmax (mn, mx) ->
      pp_typed_math_call ctx "minmax" (pp_length_math_arg ~always) [ mn; mx ]
  | Round (strategy, value, step) ->
      pp_round_length ~always ctx strategy value step
  | Mod (a, b) -> pp_mod_length ~always ctx a b
  | Rem_fn (a, b) -> pp_rem_length ~always ctx a b
  | Hypot values -> pp_hypot_length ~always ctx values
  | Abs v -> Pp.call "abs" (pp_length ~always) ctx v
  | Sign v ->
      (* CSS Values 4 10.7: [sign(<length>)] returns a [<number>], not a
         [<length>], so we cannot reduce to a single dimension without breaking
         the length round-trip. Keep [sign()] verbatim. *)
      Pp.call "sign" (pp_length ~always) ctx v
  | Calc_size (basis, calc) ->
      Pp.call "calc-size"
        (fun ctx (basis, calc) ->
          pp_length ~always ctx basis;
          Pp.comma ctx ();
          pp_calc_contents (pp_length ~always) ctx calc)
        ctx (basis, calc)
  | Anchor_size size ->
      Pp.call "anchor-size" (fun ctx size -> Pp.string ctx size) ctx size
  | Anchor (name, side, fallback) ->
      pp_anchor_length ~always ctx name side fallback
  | Attr attr -> Pp.call "attr" (pp_attr_call (pp_length ~always)) ctx attr
  | Env env -> pp_env (pp_length ~always) ctx env
  | Var v -> pp_var (pp_length ~always) ctx v
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Content -> Pp.string ctx "content"
  | Calc cv -> pp_length_calc ~always ctx cv

and pp_calc_wrapped_length ~always ctx length =
  Pp.string ctx "calc(";
  pp_length ~always ctx length;
  Pp.char ctx ')'

(* CSS Values 4 sec. 10.2: arguments to math functions ([clamp()], [min()],
   [max()], [minmax()]) are implicit calc expressions. A top-level [Calc] node
   is emitted as its bare expression body (no surrounding [calc(...)]). Nested
   [Calc] inside other length shapes still serializes through [pp_length]. *)
and pp_length_math_arg ~always ctx (length : length) =
  match length with
  | Calc cv ->
      let ctx = { ctx with in_calc = true } in
      pp_calc_contents (pp_length ~always) ctx cv
  | _ -> pp_length ~always ctx length

and pp_anchor_length ~always ctx name side fallback =
  Pp.call "anchor"
    (fun ctx (name, side, fallback) ->
      Option.iter
        (fun name ->
          Pp.string ctx name;
          Pp.space ctx ())
        name;
      Pp.string ctx side;
      Option.iter
        (fun fallback ->
          Pp.comma ctx ();
          pp_length ~always ctx fallback)
        fallback)
    ctx (name, side, fallback)

and pp_round_length ~always ctx strategy value step =
  Pp.call "round"
    (fun ctx (strategy, value, step) ->
      (* [round()] is a math function (CSS Values 4 10.x): its value/step
         operands are a typed calc context, so keep their units. The [strategy]
         keyword is not a math operand and is emitted as-is. *)
      if strategy <> "nearest" then (
        Pp.string ctx strategy;
        Pp.comma ctx ());
      let ctx = { ctx with in_calc = true } in
      pp_length ~always ctx value;
      Pp.comma ctx ();
      pp_length ~always ctx step)
    ctx (strategy, value, step)

and pp_mod_length ~always ctx a b =
  Pp.call "mod"
    (fun ctx (a, b) ->
      let ctx = { ctx with in_calc = true } in
      pp_length ~always ctx a;
      Pp.comma ctx ();
      pp_length ~always ctx b)
    ctx (a, b)

and pp_rem_length ~always ctx a b =
  Pp.call "rem"
    (fun ctx (a, b) ->
      let ctx = { ctx with in_calc = true } in
      pp_length ~always ctx a;
      Pp.comma ctx ();
      pp_length ~always ctx b)
    ctx (a, b)

and pp_hypot_length ~always ctx values =
  Pp.call_list "hypot" (pp_length ~always) { ctx with in_calc = true } values

and pp_length_calc ~always ctx cv = pp_generic_length_calc ~always ctx cv

and pp_generic_length_calc ~always ctx cv =
  let cv = if Pp.minified ctx then resolve_length_calc_vars ctx cv else cv in
  match cv with
  | Val (Var _ as length) when Pp.minified ctx ->
      pp_calc_wrapped_length ~always ctx length
  | _ ->
      let always = always || calc_contains_var cv in
      pp_calc_with ~unwrap_num:false (pp_length ~always) ctx cv

let pp_color_name : color_name Pp.t =
 fun ctx name -> Pp.string ctx (fst (color_name_hex name))

(* Relative-colour channel and alpha expressions remain an opaque tail because
   they are [<calc>]-derived. To match the typed-color path's alpha
   canonicalisation under [~minify:true], rewrite a trailing ["/<n>%"] alpha
   suffix to its decimal equivalent. *)
let minify_relative_color_alpha body =
  let len = String.length body in
  if len = 0 || body.[len - 1] <> '%' then body
  else
    match String.rindex_opt body '/' with
    | None -> body
    | Some slash -> (
        let pct = String.sub body (slash + 1) (len - slash - 2) in
        match Float.of_string_opt pct with
        | Some f ->
            String.concat ""
              [
                String.sub body 0 (slash + 1);
                Pp.string_of_float ~drop_leading_zero:true (f /. 100.);
              ]
        | None -> body)

let body_is_digit body i =
  let len = String.length body in
  i < len && body.[i] >= '0' && body.[i] <= '9'

let body_is_number_boundary body i =
  if i = 0 then true
  else
    match body.[i - 1] with
    | ' ' | '/' | ',' | '(' | '+' | '*' -> true
    | _ -> false

let rec body_starts_number body i =
  let len = String.length body in
  body_is_digit body i
  || (i < len && body.[i] = '.' && body_is_digit body (i + 1))
  || i + 1 < len
     && (body.[i] = '+' || body.[i] = '-')
     && body_starts_number body (i + 1)

let rec body_scan_digits body j =
  if body_is_digit body j then body_scan_digits body (j + 1) else j

let body_scan_fraction body after_int =
  let len = String.length body in
  if after_int < len && body.[after_int] = '.' then
    body_scan_digits body (after_int + 1)
  else after_int

let body_scan_exponent body after_frac =
  let len = String.length body in
  let is_e c = c = 'e' || c = 'E' in
  if after_frac < len && is_e body.[after_frac] then
    let signed = after_frac + 1 in
    let signed =
      if signed < len && (body.[signed] = '+' || body.[signed] = '-') then
        signed + 1
      else signed
    in
    let after_digits = body_scan_digits body signed in
    if after_digits = signed then after_frac else after_digits
  else after_frac

let body_scan_number body start =
  let after_exp =
    body_scan_exponent body
      (body_scan_fraction body (body_scan_digits body start))
  in
  let lone_dot =
    after_exp = start + 1 && start < String.length body && body.[start] = '.'
  in
  if after_exp = start || lone_dot then Option.None
  else
    let raw = String.sub body start (after_exp - start) in
    match Float.of_string_opt raw with
    | Option.Some f ->
        Option.Some (Pp.string_of_float ~drop_leading_zero:true f, after_exp)
    | Option.None -> Option.None

(* Reserialise any number literal in [body] through [Pp.string_of_float] so
   forms like [0.2] collapse to [.2]. The body is a normalised token sequence
   with single-space separators and tight [/] joins, so number boundaries are
   one of: start-of-string, space, [/], [(], [,], or another operator delim. *)
let minify_relative_color_numbers body =
  let len = String.length body in
  let buf = Buffer.create len in
  let rec loop i =
    if i >= len then ()
    else if body_is_number_boundary body i && body_starts_number body i then (
      match body_scan_number body i with
      | Option.Some (rendered, next) ->
          Buffer.add_string buf rendered;
          loop next
      | Option.None ->
          Buffer.add_char buf body.[i];
          loop (i + 1))
    else (
      Buffer.add_char buf body.[i];
      loop (i + 1))
  in
  loop 0;
  Buffer.contents buf

let relative_color_space_elidable body i =
  let len = String.length body in
  if i <= 0 || i + 1 >= len || body.[i] <> ' ' then false
  else
    match (body.[i - 1], body.[i + 1]) with
    | ')', next -> body_starts_number body (i + 1) || next = '.'
    | '%', next ->
        body_starts_number body (i + 1)
        || (next >= 'A' && next <= 'Z')
        || (next >= 'a' && next <= 'z')
    | _ -> false

let minify_relative_color_spaces body =
  let len = String.length body in
  let buf = Buffer.create len in
  let rec loop i =
    if i >= len then ()
    else if relative_color_space_elidable body i then loop (i + 1)
    else (
      Buffer.add_char buf body.[i];
      loop (i + 1))
  in
  loop 0;
  Buffer.contents buf

let zero_percentage : percentage option = Some (Pct 0.)

let zero_float_of_none (v : float option) : float option =
  match v with None -> Some 0. | Some f -> Some f

let zero_hue_of_none = function Hue_none -> Unitless 0. | h -> h

let fold_relative_color_pass_through name origin tail =
  match (name, origin, tail) with
  | "lab", Lab { l; a; b; alpha }, ("l a b" | "l a b/alpha") ->
      Some
        (Lab
           {
             l = (match l with None -> zero_percentage | Some _ -> l);
             a = zero_float_of_none a;
             b = zero_float_of_none b;
             alpha;
           })
  | "oklab", Oklab { l; a; b; alpha }, ("l a b" | "l a b/alpha") ->
      Some
        (Oklab
           {
             l = (match l with None -> zero_percentage | Some _ -> l);
             a = zero_float_of_none a;
             b = zero_float_of_none b;
             alpha;
           })
  | "lch", Lch { l; c; h; alpha }, ("l c h" | "l c h/alpha") ->
      Some
        (Lch
           {
             l = (match l with None -> zero_percentage | Some _ -> l);
             c = zero_float_of_none c;
             h = zero_hue_of_none h;
             alpha;
           })
  | "oklch", Oklch { l; c; h; alpha }, ("l c h" | "l c h/alpha") ->
      Some
        (Oklch
           {
             l = (match l with None -> zero_percentage | Some _ -> l);
             c = zero_float_of_none c;
             h = zero_hue_of_none h;
             alpha;
           })
  | _ -> None

(* Hue value of an [Hsl] colour as a float in degrees, when the input is a plain
   numeric hue (number / [deg] angle). Other forms ([rad]/[turn]/
   [grad]/[var]/[calc]/[none]) return [None] so the printer keeps the authored
   representation. *)
let deg_of_hue = function
  | Unitless f -> Some f
  | Angle (Deg f) -> Some f
  | Angle (Turn f) -> Some (f *. 360.)
  | Angle (Grad f) -> Some (f *. 0.9)
  | Angle (Rad f) -> Some (f *. 180. /. Float.pi)
  | _ -> None

let float_of_percentage = function
  | (Pct f : percentage) -> Some f
  | (Num f : percentage) -> Some (f *. 100.)
  | _ -> None

let alpha_is_full = function
  | (None : alpha) -> true
  | Num 1.0 -> true
  | Pct 100.0 -> true
  | _ -> false

let round_to_step strategy value step =
  if step = 0. then value
  else
    let q = value /. step in
    let q =
      match strategy with
      | "up" -> Float.ceil q
      | "down" -> Float.floor q
      | "to-zero" -> Float.trunc q
      | _ -> Float.round q
    in
    q *. step

let mod_value a b =
  if b = 0. then a
  else
    let q = Float.floor (a /. b) in
    a -. (q *. b)

(* Byte value [0..255] for an alpha component, when the alpha is a static number
   or percentage. Returns [None] for symbolic forms ([Var] / [Calc]) that can't
   fold to a fixed byte. *)
let alpha_value_byte = function
  | (None : alpha) -> Some 255
  | Num f when f >= 0. && f <= 1. ->
      Some (Float.to_int (Float.round (f *. 255.)))
  | Pct f when f >= 0. && f <= 100. ->
      Some (Float.to_int (Float.round (f *. 255. /. 100.)))
  | _ -> Option.None

let exact_alpha_value_byte = function
  | (None : alpha) -> Some 255
  | Num f when f >= 0. && f <= 1. ->
      let byte = f *. 255. in
      if Float.is_integer byte then Some (Float.to_int byte) else None
  | Pct f when f >= 0. && f <= 100. ->
      let byte = f *. 255. /. 100. in
      if Float.is_integer byte then Some (Float.to_int byte) else None
  | _ -> None

(* CSS Color 4 3: the hue is interpreted modulo 360 degrees. *)
let normalize_hue f =
  let m = Float.rem f 360. in
  if m < 0. then m +. 360. else m

(* CSS Color 4 4.2.4: convert HSL components to sRGB byte triples. [hue] is in
   degrees [0..360), [saturation] and [lightness] are percentages [0..100].
   Returns [(r, g, b)] each in [0..255]. *)
let hsl_to_rgb_float ~hue ~saturation ~lightness =
  let s = saturation /. 100. in
  let l = lightness /. 100. in
  let a = s *. Float.min l (1. -. l) in
  let f n =
    let k = Float.rem (n +. (hue /. 30.)) 12. in
    let k = if k < 0. then k +. 12. else k in
    let t = Float.max (Float.min (Float.min (k -. 3.) (9. -. k)) 1.) (-1.) in
    l -. (a *. t)
  in
  (f 0., f 8., f 4.)

let hsl_to_rgb_bytes ~hue ~saturation ~lightness =
  let r, g, b = hsl_to_rgb_float ~hue ~saturation ~lightness in
  let to_byte v = Float.to_int (Float.round ((v *. 255.) +. 1e-9)) in
  (to_byte r, to_byte g, to_byte b)

(* CSS Color 4 4.2.5: convert HWB components to sRGB byte triples. [hue] in
   degrees, [whiteness]/[blackness] as percentages. *)
let hwb_to_rgb_bytes ~hue ~whiteness ~blackness =
  let w = whiteness /. 100. in
  let bl = blackness /. 100. in
  if w +. bl >= 1. then
    let g = w /. (w +. bl) in
    let v = Float.to_int (Float.round (g *. 255.)) in
    (v, v, v)
  else if w = 0. && bl = 0. then
    hsl_to_rgb_bytes ~hue ~saturation:100. ~lightness:50.
  else
    let r, g, b = hsl_to_rgb_float ~hue ~saturation:100. ~lightness:50. in
    let scale c =
      let v = (c *. (1. -. w -. bl)) +. w in
      Float.to_int (Float.round (v *. 255.))
    in
    (scale r, scale g, scale b)

(* Map a fully-saturated, mid-lightness HSL hue onto its CSS named colour. Only
   the six primary/secondary hues are addressed, since they are the only ones
   whose name is shorter than the equivalent [#hex] form. *)

(* Parse a single hex digit. *)
let hex_digit c =
  match c with
  | '0' .. '9' -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
  | 'A' .. 'F' -> Some (Char.code c - Char.code 'A' + 10)
  | _ -> Option.None

(* Parse a [Hex.value] string ([RGB], [RGBA], [RRGGBB], [RRGGBBAA]) into (r, g,
   b, a) bytes. Returns [None] for malformed lengths. *)
let rgba_of_hex s =
  let pair a b =
    Option.bind (hex_digit a) (fun ah ->
        Option.map (fun bh -> (ah lsl 4) lor bh) (hex_digit b))
  in
  let single c = Option.map (fun h -> (h lsl 4) lor h) (hex_digit c) in
  match String.length s with
  | 3 -> (
      match (single s.[0], single s.[1], single s.[2]) with
      | Some r, Some g, Some b -> Some (r, g, b, 255)
      | _ -> Option.None)
  | 4 -> (
      match (single s.[0], single s.[1], single s.[2], single s.[3]) with
      | Some r, Some g, Some b, Some a -> Some (r, g, b, a)
      | _ -> Option.None)
  | 6 -> (
      match (pair s.[0] s.[1], pair s.[2] s.[3], pair s.[4] s.[5]) with
      | Some r, Some g, Some b -> Some (r, g, b, 255)
      | _ -> Option.None)
  | 8 -> (
      match
        (pair s.[0] s.[1], pair s.[2] s.[3], pair s.[4] s.[5], pair s.[6] s.[7])
      with
      | Some r, Some g, Some b, Some a -> Some (r, g, b, a)
      | _ -> Option.None)
  | _ -> Option.None

(* The numeric value of a channel when it is a plain [Int] or integer-valued
   [Num]/[Pct]. Returns [None] for [Var]/[Calc] inputs which the printer cannot
   canonicalise. *)
let channel_byte_value (c : channel) =
  match c with
  | Int i when i >= 0 && i <= 255 -> Some i
  | Num f when Float.is_integer f && f >= 0. && f <= 255. ->
      Some (Float.to_int f)
  | Pct f when f >= 0. && f <= 100. ->
      Some (Float.to_int (Float.round (f *. 255. /. 100.)))
  | _ -> None

(* Classify a channel for static sRGB folding. [Some (Some byte)] is numeric;
   [Some Option.None] is the [none] keyword (foldable, no byte); the outer
   [Option.None] is a [Var] which makes the whole colour unfoldable. Conflating
   the two [None]s would fold [rgb(var(--r) var(--g) var(--b))] to black. *)
let channel_srgb_slot (c : channel) : int option option =
  match c with
  | None -> Some Option.None
  | Var _ -> Option.None
  | _ -> Option.map (fun b -> Some b) (channel_byte_value c)

let exact_channel_byte_value (c : channel) =
  match c with
  | Int i when i >= 0 && i <= 255 -> Some i
  | Num f when Float.is_integer f && f >= 0. && f <= 255. ->
      Some (Float.to_int f)
  | Pct f when f >= 0. && f <= 100. ->
      let byte = f *. 255. /. 100. in
      if Float.is_integer byte then Some (Float.to_int byte) else None
  | _ -> None

let hex_of_byte i =
  let hex_digits = "0123456789abcdef" in
  let s = Bytes.create 2 in
  Bytes.set s 0 hex_digits.[(i lsr 4) land 0xF];
  Bytes.set s 1 hex_digits.[i land 0xF];
  Bytes.to_string s

(* Decode a hex spelling ([#rgb] / [#rrggbb] / [#rgba] / [#rrggbbaa], with or
   without the leading [#]) to its sRGB byte components. Every equivalent
   spelling decodes to the same node. Malformed input raises: there is no colour
   it denotes, and a caller that guessed one would emit a plausible wrong colour
   rather than a failure. Use [hex_opt] to decide. *)
let strip_hash s =
  if String.length s > 0 && s.[0] = '#' then String.sub s 1 (String.length s - 1)
  else s

let hex_opt s : color option =
  match rgba_of_hex (strip_hash s) with
  | Some (r, g, b, a) -> Some (Hex { r; g; b; a })
  | None -> Option.None

let hex s : color =
  match hex_opt s with
  | Some c -> c
  | Option.None ->
      invalid_arg (String.concat "" [ "Values.hex: not a hex colour: "; s ])

(* CSS Color 4 sec. 4.4 [none] sentinel: per-channel folding of a static colour
   to sRGB. Each channel is [Some byte] when the colour resolves statically and
   the channel is a numeric value, [Option.None] when the channel is the [none]
   keyword. The outer [Option.None] means the whole colour can't be folded (e.g.
   contains [Var] / [Calc] / unsupported space). *)
let static_color_to_srgb_channels :
    color -> (int option * int option * int option * int option) option =
  function
  | Hex { r; g; b; a } | Authored_hex { r; g; b; a; _ } ->
      Some (Some r, Some g, Some b, Some a)
  | Rgb (Channels { r; g; b }) -> (
      match (channel_srgb_slot r, channel_srgb_slot g, channel_srgb_slot b) with
      | Some r, Some g, Some b -> Some (r, g, b, Some 255)
      | _ -> Option.None)
  | Rgba { rgb = Channels { r; g; b }; a } -> (
      match (channel_srgb_slot r, channel_srgb_slot g, channel_srgb_slot b) with
      | Some r, Some g, Some b -> Some (r, g, b, alpha_value_byte a)
      | _ -> Option.None)
  | Hsl { h; s; l; a } -> (
      match (deg_of_hue h, float_of_percentage s, float_of_percentage l) with
      | Some hue, Some saturation, Some lightness ->
          let hue = normalize_hue hue in
          let r, g, b = hsl_to_rgb_bytes ~hue ~saturation ~lightness in
          Some (Some r, Some g, Some b, alpha_value_byte a)
      | _ -> Option.None)
  | Hwb { h; w; b; a } -> (
      match (deg_of_hue h, float_of_percentage w, float_of_percentage b) with
      | Some hue, Some whiteness, Some blackness ->
          let hue = normalize_hue hue in
          let r, g, blue = hwb_to_rgb_bytes ~hue ~whiteness ~blackness in
          Some (Some r, Some g, Some blue, alpha_value_byte a)
      | _ -> Option.None)
  | Named name -> (
      let _, hex = color_name_hex name in
      if hex = "" then Option.None
      else
        match rgba_of_hex hex with
        | Some (r, g, b, a) -> Some (Some r, Some g, Some b, Some a)
        | None -> None)
  | Transparent -> Some (Some 0, Some 0, Some 0, Some 0)
  | _ -> Option.None

(* Reduce any all-static colour to its sRGB byte channels and alpha. Returns
   [None] for colours that contain a [Var] / [Calc] / [Current] / [none]
   component or that can't be folded statically. *)
let static_color_to_srgb_bytes c : (int * int * int * int) option =
  match static_color_to_srgb_channels c with
  | Some (r, g, b, Some a) ->
      let component = function Some v -> v | None -> 0 in
      Some (component r, component g, component b, a)
  | _ -> Option.None

(* A bare colour keyword ([red], [transparent]) is also a valid [<custom-ident>]
   in a non-colour context, so it must not be introduced when folding a colour
   inside an opaque custom-property token stream: re-spell a named/transparent
   colour as its hex form, which is a colour or nothing in every context. A
   hex/function/currentcolor colour already carries no keyword spelling. *)
let nonkeyword_color (c : color) : color =
  match c with
  | Named _ | Transparent -> (
      match static_color_to_srgb_bytes c with
      | Some (r, g, b, a) -> Hex { r; g; b; a }
      | None -> c)
  | _ -> c

let exact_rgb_to_srgb_bytes c : (int * int * int * int) option =
  match c with
  | Rgb (Channels { r; g; b }) -> (
      match
        ( exact_channel_byte_value r,
          exact_channel_byte_value g,
          exact_channel_byte_value b )
      with
      | Some r, Some g, Some b -> Some (r, g, b, 255)
      | _ -> None)
  | Rgba { rgb = Channels { r; g; b }; a } -> (
      match
        ( exact_channel_byte_value r,
          exact_channel_byte_value g,
          exact_channel_byte_value b,
          exact_alpha_value_byte a )
      with
      | Some r, Some g, Some b, Some a -> Some (r, g, b, a)
      | _ -> None)
  | Transparent -> Some (0, 0, 0, 0)
  | _ -> None

(* CSS Color 4 sec. 10.2: [color(srgb r g b)] is [rgb()] with its channels
   scaled to [0..1], so the two functions spell one colour exactly when every
   channel lands on a whole byte. No other [color()] space converts to sRGB
   without a transfer function or a matrix, so nothing else here is exact; a
   channel outside the gamut, a [none] and a [var()] are all left alone. *)
let exact_srgb_function_channels c : (int * int * int * alpha) option =
  let byte : component -> int option = function
    | Num f when f >= 0. && f <= 1. ->
        let b = f *. 255. in
        if Float.is_integer b then Some (Float.to_int b) else None
    | Pct f when f >= 0. && f <= 100. ->
        let b = f *. 255. /. 100. in
        if Float.is_integer b then Some (Float.to_int b) else None
    | _ -> None
  in
  match c with
  | Color { space = Srgb; components = [ r; g; b ]; alpha } -> (
      match (byte r, byte g, byte b) with
      | Some r, Some g, Some b -> Some (r, g, b, alpha)
      | _ -> None)
  | _ -> None

(* CSS Color 5 5: combine two [color-mix] percentages into the final
   per-component weights and an [alpha] multiplier. [p1] / [p2] are in [0..100];
   missing values are normalised by the caller. *)
let mix_weights p1 p2 : (float * float * float) option =
  let sum = p1 +. p2 in
  if sum <= 0. then None
  else
    let w1 = p1 /. sum in
    let w2 = p2 /. sum in
    let alpha_mult = if sum >= 100. then 1. else sum /. 100. in
    Some (w1, w2, alpha_mult)

(* Linearly interpolate two byte channels to a byte. *)
let lerp_byte b1 b2 w1 w2 =
  Float.to_int
    (Float.round ((Float.of_int b1 *. w1) +. (Float.of_int b2 *. w2)))

(* Mix two static colours in sRGB per CSS Color 5 sec. 3. [None] if either
   operand can't fold statically ([Var] / [Calc]) or the weights reduce to zero.
   CSS Color 4 sec. 4.4 [none] sentinel: a [none] channel inherits the other
   operand's channel rather than averaging in a zero; both [none] yields [none]
   (returned as zero, since the caller routes through [Hex] and [#000000] is the
   shortest fully-[none] spelling). *)
let mix_srgb_bytes c1 c2 ~p1 ~p2 =
  match
    (static_color_to_srgb_channels c1, static_color_to_srgb_channels c2)
  with
  | Some (r1, g1, b1, a1), Some (r2, g2, b2, a2) -> (
      match mix_weights p1 p2 with
      | None -> Option.None
      | Some (w1, w2, alpha_mult) ->
          (* CSS Color 4 sec. 13.3: colours with alpha interpolate
             premultiplied, then un-premultiply by the result alpha - otherwise
             a transparent operand ([#0000]) bleeds its zero channels in and
             darkens the result. A [none] (or absent) alpha contributes as fully
             opaque to its own channels. *)
          let alpha_f = function
            | Some a -> Float.of_int a /. 255.
            | None -> 1.
          in
          let af1 = alpha_f a1 and af2 = alpha_f a2 in
          (* Un-premultiply by the interpolated alpha only; the [alpha_mult]
             (<100%-sum) scaling applies to the final alpha, not the
             channels. *)
          let interp_alpha = (af1 *. w1) +. (af2 *. w2) in
          let mix_channel x y =
            match (x, y) with
            | Some bx, Some by ->
                let pre =
                  (Float.of_int bx *. af1 *. w1)
                  +. (Float.of_int by *. af2 *. w2)
                in
                let v =
                  if interp_alpha <= 0. then 0. else pre /. interp_alpha
                in
                Some (Float.to_int (Float.round v))
            | Some b, None | None, Some b -> Some b
            | None, None -> None
          in
          let r = mix_channel r1 r2 in
          let g = mix_channel g1 g2 in
          let b = mix_channel b1 b2 in
          let a =
            match (a1, a2) with
            | Some av1, Some av2 ->
                let alpha_pre = lerp_byte av1 av2 w1 w2 in
                Some
                  (Float.to_int
                     (Float.round (Float.of_int alpha_pre *. alpha_mult)))
            | Some av, None | None, Some av ->
                Some
                  (Float.to_int (Float.round (Float.of_int av *. alpha_mult)))
            | None, None -> None
          in
          let unwrap = function Some v -> v | None -> 0 in
          Some (unwrap r, unwrap g, unwrap b, unwrap a))
  | _ -> Option.None

let static_alpha_value = function
  | (None : alpha) -> Some 1.
  | Num f when f >= 0. && f <= 1. -> Some f
  | Pct f when f >= 0. && f <= 100. -> Some (f /. 100.)
  | _ -> Option.None

let alpha_of_mixed_value f = if f >= 1. then (None : alpha) else Num f

let mix_optional_float ~w1 ~w2 v1 v2 =
  match (v1, v2) with
  | Some f1, Some f2 -> Some ((f1 *. w1) +. (f2 *. w2))
  | Some f, None | None, Some f -> Some f
  | None, None -> Option.None

(* Mix the lightness channel of a lab-family colour. [l_num_max] is the L value
   that 100% denotes: 100. for lab/lch (a bare number is already on the 0-100
   scale) and 1. for oklab/oklch (a bare number is on 0-1). The result is stored
   as a bare number on that scale, matching the [mix_in_*_space] fallback. A
   [none] L channel carries the other operand's value over verbatim. *)
let mix_lightness ~l_num_max ~w1 ~w2 l1 l2 =
  let to_float = function
    | (Num f : percentage) -> Some f
    | Pct f -> Some (f /. 100. *. l_num_max)
    | _ -> None
  in
  match (l1, l2) with
  | Some p1, Some p2 -> (
      match (to_float p1, to_float p2) with
      | Some f1, Some f2 ->
          Some (Some (Num ((f1 *. w1) +. (f2 *. w2)) : percentage))
      | _ -> Option.None)
  | Some p, None | None, Some p -> Some (Some p)
  | None, None -> Some None

let mix_alpha ~w1 ~w2 ~alpha_mult a1 a2 =
  match (static_alpha_value a1, static_alpha_value a2) with
  | Some f1, Some f2 ->
      Some (alpha_of_mixed_value (((f1 *. w1) +. (f2 *. w2)) *. alpha_mult))
  | _ -> Option.None

(* Mix two hues along the shorter arc (the default interpolation method, the
   only one the typed lab-family fold handles; specified methods bail out to
   [keep]). A [none] hue carries the other operand's hue over verbatim. *)
let mix_hue_shorter ~w2 h1 h2 =
  match (h1, h2) with
  | Hue_none, Hue_none -> Some Hue_none
  | Hue_none, h | h, Hue_none -> Some h
  | h1, h2 -> (
      match (deg_of_hue h1, deg_of_hue h2) with
      | Some f1, Some f2 ->
          Some
            (Unitless (Color_space.interpolate_hue Color_space.Shorter f1 f2 w2))
      | _ -> Option.None)

let mix_lab_like_result ~l_num_max ~p1 ~p2 build l1 a1 b1 alpha1 l2 a2 b2 alpha2
    =
  match mix_weights p1 p2 with
  | None -> Option.None
  | Some (w1, w2, alpha_mult) -> (
      match
        ( mix_lightness ~l_num_max ~w1 ~w2 l1 l2,
          Some (mix_optional_float ~w1 ~w2 a1 a2),
          Some (mix_optional_float ~w1 ~w2 b1 b2),
          mix_alpha ~w1 ~w2 ~alpha_mult alpha1 alpha2 )
      with
      | Some l, Some a, Some b, Some alpha -> Some (build l a b alpha)
      | _ -> Option.None)

let mix_lch_like_result ~l_num_max ~p1 ~p2 build l1 c1 h1 alpha1 l2 c2 h2 alpha2
    =
  match mix_weights p1 p2 with
  | None -> Option.None
  | Some (w1, w2, alpha_mult) -> (
      match
        ( mix_lightness ~l_num_max ~w1 ~w2 l1 l2,
          Some (mix_optional_float ~w1 ~w2 c1 c2),
          mix_hue_shorter ~w2 h1 h2,
          mix_alpha ~w1 ~w2 ~alpha_mult alpha1 alpha2 )
      with
      | Some l, Some c, Some h, Some alpha -> Some (build l c h alpha)
      | _ -> Option.None)

(* The typed lab-family mix interpolates coordinates directly, which is only
   correct when both operands are fully opaque (premultiplying by 1 is a no-op).
   A partially transparent operand needs the premultiplied [mix_in_*_space]
   path, so a non-opaque operand falls through to [None] here and the caller
   routes it there. *)
let alpha_opaque : alpha -> bool = function
  | None -> true
  | Num f -> f >= 1.0
  | Pct f -> f >= 100.0
  | Var _ | Calc _ -> false

let mix_lab_family in_space color1 color2 ~p1 ~p2 =
  match (in_space, color1, color2) with
  | ( Some (Lab : color_space),
      Lab { l = l1; a = a1; b = b1; alpha = alpha1 },
      Lab { l = l2; a = a2; b = b2; alpha = alpha2 } )
    when alpha_opaque alpha1 && alpha_opaque alpha2 ->
      mix_lab_like_result ~l_num_max:100. ~p1 ~p2
        (fun l a b alpha -> Lab { l; a; b; alpha })
        l1 a1 b1 alpha1 l2 a2 b2 alpha2
  | ( Some (Oklab : color_space),
      Oklab { l = l1; a = a1; b = b1; alpha = alpha1 },
      Oklab { l = l2; a = a2; b = b2; alpha = alpha2 } )
    when alpha_opaque alpha1 && alpha_opaque alpha2 ->
      mix_lab_like_result ~l_num_max:1. ~p1 ~p2
        (fun l a b alpha -> Oklab { l; a; b; alpha })
        l1 a1 b1 alpha1 l2 a2 b2 alpha2
  | ( Some (Lch : color_space),
      Lch { l = l1; c = c1; h = h1; alpha = alpha1 },
      Lch { l = l2; c = c2; h = h2; alpha = alpha2 } )
    when alpha_opaque alpha1 && alpha_opaque alpha2 ->
      mix_lch_like_result ~l_num_max:100. ~p1 ~p2
        (fun l c h alpha -> Lch { l; c; h; alpha })
        l1 c1 h1 alpha1 l2 c2 h2 alpha2
  | ( Some (Oklch : color_space),
      Oklch { l = l1; c = c1; h = h1; alpha = alpha1 },
      Oklch { l = l2; c = c2; h = h2; alpha = alpha2 } )
    when alpha_opaque alpha1 && alpha_opaque alpha2 ->
      mix_lch_like_result ~l_num_max:1. ~p1 ~p2
        (fun l c h alpha -> Oklch { l; c; h; alpha })
        l1 c1 h1 alpha1 l2 c2 h2 alpha2
  | _ -> None

(* Convert any static colour to floating-point linear sRGB plus an alpha value
   in [0, 1]. Routes through [static_color_to_srgb_bytes] for the common case
   and through [Color_space] for spaces whose conversion matrices we now have
   wired up. Returns [None] for colours that contain a [Var] / [Calc] /
   [Current] component or for [none] sentinels (those need the per-channel
   carry-over path in [static_color_to_srgb_channels]). *)
let static_color_to_linear_srgb (c : color) :
    ((float * float * float) * float) option =
  let f255 b = Float.of_int b /. 255.0 in
  match c with
  | Color { space; components; alpha } when List.length components = 3 -> (
      let component_value : component -> float option = function
        | Num f -> Some f
        | Pct f -> Some (f /. 100.0)
        | Component_none -> Some 0.0
        | _ -> None
      in
      match List.map component_value components with
      | [ Some c1; Some c2; Some c3 ] -> (
          let alpha_f =
            match alpha with
            | (None : alpha) -> Some 1.0
            | Num f -> Some f
            | Pct f -> Some (f /. 100.0)
            | _ -> None
          in
          match alpha_f with
          | None -> None
          | Some alpha_f -> (
              match space with
              | Srgb ->
                  let lin = Color_space.linear_rgb_of_rgb (c1, c2, c3) in
                  Some (lin, alpha_f)
              | Srgb_linear -> Some ((c1, c2, c3), alpha_f)
              | Display_p3 ->
                  let lin = Color_space.linear_rgb_of_rgb (c1, c2, c3) in
                  let xyz = Color_space.xyz65_of_linear_p3 lin in
                  Some (Color_space.linear_srgb_of_xyz65 xyz, alpha_f)
              | A98_rgb ->
                  let lin =
                    ( Color_space.linear_of_a98_rgb c1,
                      Color_space.linear_of_a98_rgb c2,
                      Color_space.linear_of_a98_rgb c3 )
                  in
                  let xyz = Color_space.xyz65_of_linear_a98 lin in
                  Some (Color_space.linear_srgb_of_xyz65 xyz, alpha_f)
              | Prophoto_rgb ->
                  let lin =
                    ( Color_space.linear_of_prophoto_rgb c1,
                      Color_space.linear_of_prophoto_rgb c2,
                      Color_space.linear_of_prophoto_rgb c3 )
                  in
                  let xyz_d50 = Color_space.xyz50_of_linear_prophoto lin in
                  let xyz_d65 = Color_space.d65_of_xyz50 xyz_d50 in
                  Some (Color_space.linear_srgb_of_xyz65 xyz_d65, alpha_f)
              | Rec2020 ->
                  let lin =
                    ( Color_space.linear_of_rec2020 c1,
                      Color_space.linear_of_rec2020 c2,
                      Color_space.linear_of_rec2020 c3 )
                  in
                  let xyz = Color_space.xyz65_of_linear_rec2020 lin in
                  Some (Color_space.linear_srgb_of_xyz65 xyz, alpha_f)
              | Xyz | Xyz_d65 ->
                  Some (Color_space.linear_srgb_of_xyz65 (c1, c2, c3), alpha_f)
              | Xyz_d50 ->
                  let xyz_d65 = Color_space.d65_of_xyz50 (c1, c2, c3) in
                  Some (Color_space.linear_srgb_of_xyz65 xyz_d65, alpha_f)
              | _ -> None))
      | _ -> None)
  | Oklab { l = Some l; a = Some a; b = Some b; alpha } -> (
      let l =
        match l with
        | (Num f : percentage) -> Some f
        | Pct f -> Some (f /. 100.0)
        | _ -> None
      in
      let alpha_f =
        match alpha with
        | (None : alpha) -> Some 1.0
        | Num f -> Some f
        | Pct f -> Some (f /. 100.0)
        | _ -> None
      in
      match (l, alpha_f) with
      | Some l, Some alpha_f ->
          Some (Color_space.linear_srgb_of_oklab (l, a, b), alpha_f)
      | _ -> None)
  | Oklch { l = Some l; c = Some c; h; alpha } -> (
      let l =
        match l with
        | (Num f : percentage) -> Some f
        | Pct f -> Some (f /. 100.0)
        | _ -> None
      in
      let h = deg_of_hue h in
      let alpha_f =
        match alpha with
        | (None : alpha) -> Some 1.0
        | Num f -> Some f
        | Pct f -> Some (f /. 100.0)
        | _ -> None
      in
      match (l, h, alpha_f) with
      | Some l, Some h, Some alpha_f ->
          let lab = Color_space.oklab_of_oklch (l, c, h) in
          Some (Color_space.linear_srgb_of_oklab lab, alpha_f)
      | _ -> None)
  | Lab { l = Some l; a = Some a; b = Some b; alpha } -> (
      let l =
        match l with
        | (Pct f : percentage) -> Some f
        | Num f -> Some f
        | _ -> None
      in
      let alpha_f =
        match alpha with
        | (None : alpha) -> Some 1.0
        | Num f -> Some f
        | Pct f -> Some (f /. 100.0)
        | _ -> None
      in
      match (l, alpha_f) with
      | Some l, Some alpha_f ->
          let xyz_d50 = Color_space.xyz50_of_lab (l, a, b) in
          let xyz_d65 = Color_space.d65_of_xyz50 xyz_d50 in
          Some (Color_space.linear_srgb_of_xyz65 xyz_d65, alpha_f)
      | _ -> None)
  | Lch { l = Some l; c = Some c; h; alpha } -> (
      let l =
        match l with
        | (Pct f : percentage) -> Some f
        | Num f -> Some f
        | _ -> None
      in
      let h = deg_of_hue h in
      let alpha_f =
        match alpha with
        | (None : alpha) -> Some 1.0
        | Num f -> Some f
        | Pct f -> Some (f /. 100.0)
        | _ -> None
      in
      match (l, h, alpha_f) with
      | Some l, Some h, Some alpha_f ->
          let lab = Color_space.lab_of_lch (l, c, h) in
          let xyz_d50 = Color_space.xyz50_of_lab lab in
          let xyz_d65 = Color_space.d65_of_xyz50 xyz_d50 in
          Some (Color_space.linear_srgb_of_xyz65 xyz_d65, alpha_f)
      | _ -> None)
  | _ -> (
      match static_color_to_srgb_bytes c with
      | Some (r, g, b, a) ->
          let lin = Color_space.linear_rgb_of_rgb (f255 r, f255 g, f255 b) in
          Some (lin, f255 a)
      | None -> None)

(* CSS Color 4 sec. 14.2: the sRGB colour a display renders for a static colour,
   gamut mapping the ones sRGB cannot hold. This always answers, so it serves a
   caller that has to write an sRGB colour; the optimizer must not use it, as
   the mapped colour is not the authored one. A colour that is not static is
   returned unchanged. *)
let gamut_map_color (c : color) : color =
  match static_color_to_linear_srgb c with
  | None -> c
  | Some (linear, alpha_f) ->
      let lch =
        Color_space.oklch_of_oklab (Color_space.oklab_of_linear_srgb linear)
      in
      let r, g, b = Color_space.gamut_mapped_srgb_of_oklch lch in
      let byte v = Float.to_int (Float.round (v *. 255.)) in
      let clamp01 v = Float.max 0. (Float.min 1. v) in
      Hex { r = byte r; g = byte g; b = byte b; a = byte (clamp01 alpha_f) }

(* CSS Color 4 sec. 13.4 [hue interpolation method] mapping into the simpler
   [Color_space.hue_interpolation] enum. [Default] and [Specified] both collapse
   to [Shorter] for the static fold; sec. 13.4 makes [shorter hue] the default
   and [specified hue] is a no-op for [color-mix] inputs that aren't already in
   a polar space. *)
let color_space_hue (h : hue_interpolation) : Color_space.hue_interpolation =
  match h with
  | Shorter | Default | Specified -> Color_space.Shorter
  | Longer -> Color_space.Longer
  | Increasing -> Color_space.Increasing
  | Decreasing -> Color_space.Decreasing

let alpha_of_unit_float a : alpha =
  if a >= 1.0 -. 1e-6 then None else if a <= 1e-6 then Num 0.0 else Num a

(* Premultiplied interpolation (CSS Color 4 sec. 13.3): each coordinate is
   multiplied by its operand alpha before mixing and the sum divided by the
   interpolated alpha, so a [transparent] operand contributes only alpha and not
   its zero coordinates. The [alpha_mult] (<100%-sum) scaling lands on the final
   alpha, never the coordinates - matching [mix_srgb_bytes]. *)
let premult_mix3 ~w1 ~w2 ~a1 ~a2 (x1, y1, z1) (x2, y2, z2) =
  let interp_alpha = (a1 *. w1) +. (a2 *. w2) in
  let axis c1 c2 =
    let pre = (c1 *. a1 *. w1) +. (c2 *. a2 *. w2) in
    if interp_alpha <= 0. then 0. else pre /. interp_alpha
  in
  ((axis x1 x2, axis y1 y2, axis z1 z2), interp_alpha)

(* Polar variant: lightness and chroma premultiply like rectangular axes, but
   the hue angle never does. A zero-chroma operand ([transparent], grey, white)
   has a powerless hue (CSS Color 4 sec. 4.4.1): it carries the other operand's
   hue over instead of dragging the mix toward an undefined angle. *)
let premult_mix_polar ~w1 ~w2 ~a1 ~a2 ~hue (l1, c1, h1) (l2, c2, h2) =
  let interp_alpha = (a1 *. w1) +. (a2 *. w2) in
  let axis v1 v2 =
    let pre = (v1 *. a1 *. w1) +. (v2 *. a2 *. w2) in
    if interp_alpha <= 0. then 0. else pre /. interp_alpha
  in
  let powerless c = c <= 1e-6 in
  let h =
    match (powerless c1, powerless c2) with
    | true, true -> h1
    | true, false -> h2
    | false, true -> h1
    | false, false -> Color_space.interpolate_hue (color_space_hue hue) h1 h2 w2
  in
  ((axis l1 l2, axis c1 c2, h), interp_alpha)

let mix_in_oklab_space c1 c2 ~p1 ~p2 : color option =
  match (static_color_to_linear_srgb c1, static_color_to_linear_srgb c2) with
  | Some (lrgb1, alpha1), Some (lrgb2, alpha2) -> (
      match mix_weights p1 p2 with
      | None -> None
      | Some (w1, w2, alpha_mult) ->
          let (l, a, b), interp_alpha =
            premult_mix3 ~w1 ~w2 ~a1:alpha1 ~a2:alpha2
              (Color_space.oklab_of_linear_srgb lrgb1)
              (Color_space.oklab_of_linear_srgb lrgb2)
          in
          Some
            (Oklab
               {
                 l = Some (Num l : percentage);
                 a = Some a;
                 b = Some b;
                 alpha = alpha_of_unit_float (interp_alpha *. alpha_mult);
               }))
  | _ -> None

let mix_in_oklch_space c1 c2 ~p1 ~p2 hue : color option =
  match (static_color_to_linear_srgb c1, static_color_to_linear_srgb c2) with
  | Some (lrgb1, alpha1), Some (lrgb2, alpha2) -> (
      match mix_weights p1 p2 with
      | None -> None
      | Some (w1, w2, alpha_mult) ->
          let (l, c, h), interp_alpha =
            premult_mix_polar ~w1 ~w2 ~a1:alpha1 ~a2:alpha2 ~hue
              (Color_space.oklch_of_oklab
                 (Color_space.oklab_of_linear_srgb lrgb1))
              (Color_space.oklch_of_oklab
                 (Color_space.oklab_of_linear_srgb lrgb2))
          in
          Some
            (Oklch
               {
                 l = Some (Num l : percentage);
                 c = Some c;
                 h = Unitless h;
                 alpha = alpha_of_unit_float (interp_alpha *. alpha_mult);
               }))
  | _ -> None

let mix_in_lab_space c1 c2 ~p1 ~p2 : color option =
  match (static_color_to_linear_srgb c1, static_color_to_linear_srgb c2) with
  | Some (lrgb1, alpha1), Some (lrgb2, alpha2) -> (
      match mix_weights p1 p2 with
      | None -> None
      | Some (w1, w2, alpha_mult) ->
          let to_lab lrgb =
            lrgb |> Color_space.xyz65_of_linear_srgb |> Color_space.d50_of_xyz65
            |> Color_space.lab_of_xyz50
          in
          let (l, a, b), interp_alpha =
            premult_mix3 ~w1 ~w2 ~a1:alpha1 ~a2:alpha2 (to_lab lrgb1)
              (to_lab lrgb2)
          in
          Some
            (Lab
               {
                 l = Some (Num l : percentage);
                 a = Some a;
                 b = Some b;
                 alpha = alpha_of_unit_float (interp_alpha *. alpha_mult);
               }))
  | _ -> None

let mix_in_lch_space c1 c2 ~p1 ~p2 hue : color option =
  match (static_color_to_linear_srgb c1, static_color_to_linear_srgb c2) with
  | Some (lrgb1, alpha1), Some (lrgb2, alpha2) -> (
      match mix_weights p1 p2 with
      | None -> None
      | Some (w1, w2, alpha_mult) ->
          let to_lch lrgb =
            lrgb |> Color_space.xyz65_of_linear_srgb |> Color_space.d50_of_xyz65
            |> Color_space.lab_of_xyz50 |> Color_space.lch_of_lab
          in
          let (l, c, h), interp_alpha =
            premult_mix_polar ~w1 ~w2 ~a1:alpha1 ~a2:alpha2 ~hue (to_lch lrgb1)
              (to_lch lrgb2)
          in
          Some
            (Lch
               {
                 l = Some (Num l : percentage);
                 c = Some c;
                 h = Unitless h;
                 alpha = alpha_of_unit_float (interp_alpha *. alpha_mult);
               }))
  | _ -> None

let map3 f (x, y, z) = (f x, f y, f z)

(* Inverse of the [Color] arm of [static_color_to_linear_srgb]: linear sRGB back
   into the coordinates of a predefined [color()] space, by the CSS Color 4 sec.
   19 reference conversions. [None] for the spaces [color()] cannot spell (the
   lab and polar families), which have their own mixers. *)
let coords_of_linear_srgb (space : color_space) lrgb :
    (float * float * float) option =
  match space with
  | Srgb -> Some (Color_space.rgb_of_linear_rgb lrgb)
  | Srgb_linear -> Some lrgb
  | Display_p3 ->
      Some
        (map3 Color_space.display_p3_of_linear
           (Color_space.linear_p3_of_xyz65
              (Color_space.xyz65_of_linear_srgb lrgb)))
  | A98_rgb ->
      Some
        (map3 Color_space.a98_rgb_of_linear
           (Color_space.linear_a98_of_xyz65
              (Color_space.xyz65_of_linear_srgb lrgb)))
  | Prophoto_rgb ->
      Some
        (map3 Color_space.prophoto_rgb_of_linear
           (Color_space.linear_prophoto_of_xyz50
              (Color_space.d50_of_xyz65 (Color_space.xyz65_of_linear_srgb lrgb))))
  | Rec2020 ->
      Some
        (map3 Color_space.rec2020_of_linear
           (Color_space.linear_rec2020_of_xyz65
              (Color_space.xyz65_of_linear_srgb lrgb)))
  | Xyz | Xyz_d65 -> Some (Color_space.xyz65_of_linear_srgb lrgb)
  | Xyz_d50 ->
      Some (Color_space.d50_of_xyz65 (Color_space.xyz65_of_linear_srgb lrgb))
  | Lab | Oklab | Lch | Oklch | Hsl | Hwb -> None

(* CSS Color 5 sec. 3: mixing in one of the rectangular [color()] spaces. The
   operands are carried into that space's own coordinates, mixed premultiplied
   there (CSS Color 4 sec. 13.3), and the result named in the same space. CSS
   Color 4 sec. 10.1 leaves [color()] coordinates unclamped, so a mix that ends
   outside the space's gamut is still a value: gamut mapping it (sec. 14.2) is
   what a display does, not what the colour is.

   The coordinates come back through a conversion round trip, which leaves float
   noise many orders of magnitude below a channel step. Rounding to six decimals
   clears it; [color_channel_decimals] never prints more than six for a colour
   channel, so no digit that would have been shown is lost. *)
let mix_in_rectangular_space (space : color_space) c1 c2 ~p1 ~p2 : color option
    =
  let round6 v = Float.round (v *. 1e6) /. 1e6 in
  match (static_color_to_linear_srgb c1, static_color_to_linear_srgb c2) with
  | Some (lrgb1, alpha1), Some (lrgb2, alpha2) -> (
      match
        ( coords_of_linear_srgb space lrgb1,
          coords_of_linear_srgb space lrgb2,
          mix_weights p1 p2 )
      with
      | Some x1, Some x2, Some (w1, w2, alpha_mult) ->
          let (c1, c2, c3), interp_alpha =
            premult_mix3 ~w1 ~w2 ~a1:alpha1 ~a2:alpha2 x1 x2
          in
          Some
            (Color
               {
                 space;
                 components =
                   [ Num (round6 c1); Num (round6 c2); Num (round6 c3) ];
                 alpha = alpha_of_unit_float (interp_alpha *. alpha_mult);
               })
      | _ -> None)
  | _ -> None

(* CSS Color 5 sec. 3 percentage normalisation: an omitted [percentN] takes the
   complement of the other side ([100% - percentM], clamped to 0); both omitted
   defaults to [50% / 50%]. *)
let color_mix_percentages (percent1 : percentage option)
    (percent2 : percentage option) =
  let f1 = Option.bind percent1 float_of_percentage in
  let f2 = Option.bind percent2 float_of_percentage in
  match (percent1, percent2) with
  | Option.None, Option.None -> Some (50., 50.)
  | Some _, Option.None ->
      Option.map (fun p1 -> (p1, Float.max 0. (100. -. p1))) f1
  | Option.None, Some _ ->
      Option.map (fun p2 -> (Float.max 0. (100. -. p2), p2)) f2
  | Some _, Some _ -> (
      match (f1, f2) with Some p1, Some p2 -> Some (p1, p2) | _ -> None)

(** Minify a color value by converting named colors to hex when shorter,
    matching Lightning CSS behavior. *)
let shorten_hex value =
  let len = String.length value in
  (* #RRGGBB -> #RGB when R=R, G=G, B=B *)
  if
    len = 6
    && value.[0] = value.[1]
    && value.[2] = value.[3]
    && value.[4] = value.[5]
  then (
    let s = Bytes.create 3 in
    Bytes.set s 0 value.[0];
    Bytes.set s 1 value.[2];
    Bytes.set s 2 value.[4];
    Bytes.to_string s (* #RRGGBBAA -> #RGBA when R=R, G=G, B=B, A=A *))
  else if
    len = 8
    && value.[0] = value.[1]
    && value.[2] = value.[3]
    && value.[4] = value.[5]
    && value.[6] = value.[7]
  then (
    if
      (* Further shorten #RGBA -> #RGB when A=f (fully opaque) *)
      value.[6] = 'f' || value.[6] = 'F'
    then (
      let s = Bytes.create 3 in
      Bytes.set s 0 value.[0];
      Bytes.set s 1 value.[2];
      Bytes.set s 2 value.[4];
      Bytes.to_string s)
    else
      let s = Bytes.create 4 in
      Bytes.set s 0 value.[0];
      Bytes.set s 1 value.[2];
      Bytes.set s 2 value.[4];
      Bytes.set s 3 value.[6];
      Bytes.to_string s)
  else if
    (* #RRGGBBFF -> #RRGGBB when fully opaque *)
    len = 8
    && (value.[6] = 'f' || value.[6] = 'F')
    && (value.[7] = 'f' || value.[7] = 'F')
  then String.sub value 0 6
  else if
    (* #RGBA -> #RGB when A=f (fully opaque) *)
    len = 4 && (value.[3] = 'f' || value.[3] = 'F')
  then String.sub value 0 3
  else value

(* Shortest hex spelling (no [#]) of decoded sRGB byte components: the opaque
   alpha is dropped and [#rrggbb] / [#rrggbbaa] shorten to [#rgb] / [#rgba] when
   each byte is a doubled nibble. *)
let hex_string_of_bytes r g b a =
  let rgb = String.concat "" [ hex_of_byte r; hex_of_byte g; hex_of_byte b ] in
  if a = 255 then shorten_hex rgb
  else shorten_hex (String.concat "" [ rgb; hex_of_byte a ])

(* Only the functional notations carry CSS Color 4 sec. 4.1's [<alpha-value>]
   slot, so a hex, a named colour and [transparent] are re-spelled as the
   [rgb()] over the sRGB bytes sec. 6.1 and sec. 6.3 give them. A colour whose
   channels are known only at used-value time has nothing to write into. *)
let rec with_alpha (c : color) (a : alpha) : color =
  let srgb r g b =
    Rgba { rgb = Channels { r = Int r; g = Int g; b = Int b }; a }
  in
  match c with
  | Hex { r; g; b; _ } | Authored_hex { r; g; b; _ } -> srgb r g b
  | Transparent -> srgb 0 0 0
  | Named n -> (
      match rgba_of_hex (snd (color_name_hex n)) with
      | Some (r, g, b, _) -> srgb r g b
      | Option.None -> c)
  | Rgb rgb | Rgba { rgb; _ } -> Rgba { rgb; a }
  | Hsl { h; s; l; _ } -> Hsl { h; s; l; a }
  | Hwb { h; w; b; _ } -> Hwb { h; w; b; a }
  | Color { space; components; _ } -> Color { space; components; alpha = a }
  | Lab { l; a = axis_a; b; _ } -> Lab { l; a = axis_a; b; alpha = a }
  | Oklab { l; a = axis_a; b; _ } -> Oklab { l; a = axis_a; b; alpha = a }
  | Lch { l; c = chroma; h; _ } -> Lch { l; c = chroma; h; alpha = a }
  | Oklch { l; c = chroma; h; _ } -> Oklch { l; c = chroma; h; alpha = a }
  (* [light-dark()] resolves to one of its two arguments, so both take it. *)
  | Light_dark (light, dark) ->
      Light_dark (with_alpha light a, with_alpha dark a)
  | Relative_rgb _ | Relative_color _ | Contrast_color _ | Attribute _
  | System _ | Var _ | Current | Mix _ | Auto | Inherit | Initial | Unset
  | Revert | Revert_layer ->
      c

let minify_color : color -> color = function
  | Named n -> (
      let name, hex = color_name_hex n in
      if hex = "" || color_name_is_shortest ~name ~hex then Named n
      else
        match rgba_of_hex hex with
        | Some (r, g, b, a) -> Hex { r; g; b; a }
        | None -> Named n)
  | c -> c

(* CSS Color 4 sec. 15.5 normalises system colour keywords to lowercase
   ASCII. *)
let pp_system_color : system_color Pp.t =
 fun ctx -> function
  | Accent_color -> Pp.string ctx "accentcolor"
  | Accent_color_text -> Pp.string ctx "accentcolortext"
  | Active_text -> Pp.string ctx "activetext"
  | Button_border -> Pp.string ctx "buttonborder"
  | Button_face -> Pp.string ctx "buttonface"
  | Button_text -> Pp.string ctx "buttontext"
  | Canvas -> Pp.string ctx "canvas"
  | Canvas_text -> Pp.string ctx "canvastext"
  | Field -> Pp.string ctx "field"
  | Field_text -> Pp.string ctx "fieldtext"
  | Gray_text -> Pp.string ctx "graytext"
  | Highlight -> Pp.string ctx "highlight"
  | Highlight_text -> Pp.string ctx "highlighttext"
  | Link_text -> Pp.string ctx "linktext"
  | Mark -> Pp.string ctx "mark"
  | Mark_text -> Pp.string ctx "marktext"
  | Selected_item -> Pp.string ctx "selecteditem"
  | Selected_item_text -> Pp.string ctx "selecteditemtext"
  | Visited_text -> Pp.string ctx "visitedtext"
  | Webkit_focus_ring_color -> Pp.string ctx "-webkit-focus-ring-color"

let rec pp_channel : channel Pp.t =
 fun ctx -> function
  | Int i -> Pp.int ctx i
  | Num f -> Pp.float ctx f
  | Pct f ->
      Pp.float ctx f;
      Pp.char ctx '%'
  | Var v -> pp_var pp_channel ctx v
  | None -> Pp.string ctx "none"

let angle_degrees_opt = function
  | Deg value -> Some value
  | Rad value -> Some (value *. 180. /. Float.pi)
  | Turn value -> Some (value *. 360.)
  | Grad value -> Some (value *. 0.9)
  | _ -> None

let rec pp_angle : angle Pp.t =
 fun ctx -> function
  (* Pure serialiser: the authored unit is kept. Converting between
     losslessly-interchangeable units (deg / turn / grad) and folding the static
     math functions are AST rewrites done by [normalize_angle]. *)
  | Deg f -> pp_unit ctx f "deg"
  | Rad f -> pp_unit ctx f "rad"
  | Turn f -> pp_unit ctx f "turn"
  | Grad f -> pp_unit ctx f "grad"
  | Round (strategy, value, step) ->
      Pp.call "round"
        (fun ctx (strategy, value, step) ->
          if strategy <> "nearest" then (
            Pp.string ctx strategy;
            Pp.comma ctx ());
          pp_angle ctx value;
          Pp.comma ctx ();
          pp_angle ctx step)
        ctx (strategy, value, step)
  | Rem (a, b) ->
      Pp.call "rem"
        (fun ctx (a, b) ->
          pp_angle ctx a;
          Pp.comma ctx ();
          pp_angle ctx b)
        ctx (a, b)
  | Mod (a, b) ->
      Pp.call "mod"
        (fun ctx (a, b) ->
          pp_angle ctx a;
          Pp.comma ctx ();
          pp_angle ctx b)
        ctx (a, b)
  (* A math function is itself an [<angle>] production, so the [calc()] around
     one is redundant (CSS Values 4 sec. 10.8). *)
  | Calc (Math_fn fn) -> pp_math_fn ctx fn
  | Calc c -> pp_calc_with ~unwrap_num:false pp_angle ctx c
  | Var v -> pp_var pp_angle ctx v
  | Invalid tokens ->
      Pp.string ctx
        (if Pp.minified ctx then Parser.to_string_minified tokens
         else Parser.string_of_components tokens)

(* A bare or [deg] hue prints in full precision. Minified output can omit an
   explicit [deg] exactly; converting another angle unit or reducing precision
   remains a node-changing fold owned by the AST normalize pass
   ([round_hue]). *)
let pp_hue_float ctx f =
  Pp.string ctx (Pp.string_of_float ~drop_leading_zero:true f)

let rec pp_hue : hue Pp.t =
 fun ctx -> function
  | Unitless f -> pp_hue_float ctx f
  | Angle (Deg f) when Pp.minified ctx -> pp_hue_float ctx f
  | Angle a -> pp_angle ctx a
  | Var v -> pp_var pp_hue ctx v
  | Hue_none -> Pp.string ctx "none"

let rec pp_alpha : alpha Pp.t =
 fun ctx -> function
  | None -> ()
  | Num f ->
      (* CSSOM serialisation (CSSOM 1 sec. 6.7.2) drops a leading zero on
         fractional numbers: emit [.25] not [0.25] in both modes. Under minify,
         round to 3 decimals (alpha precision is 1/255 ~ 0.004 in sRGB); alpha
         is a colour channel, so [lossless] opts out of that fold like the other
         channels. *)
      let max_decimals =
        if Pp.minified ctx && not ctx.Pp.lossless then 3 else 8
      in
      Pp.float_n max_decimals ctx f
  | Pct f -> Pp.pct ctx f
  | Var v -> pp_var pp_alpha ctx v
  | Calc c -> pp_calc pp_alpha ctx c

(* Helper to print optional alpha with the correct leading separator *)
let pp_opt_alpha ctx = function
  | None -> ()
  | (Num _ | Pct _ | Var _ | Calc _) as a ->
      Pp.op_char ctx '/';
      pp_alpha ctx a

(** Pretty printer for percentage types *)
let rec pp_percentage ?(always = false) : percentage Pp.t =
 fun ctx -> function
  | Pct f -> Pp.pct ctx f
  | Num f -> Pp.float ctx f
  | Var v -> pp_var (pp_percentage ~always) ctx v
  | Calc c -> pp_calc (pp_percentage ~always) ctx c

(* Evaluate the static CSS math functions on a [<length>] (the folds the printer
   used to do under minify), so the printer stays a pure serialiser. min / max /
   clamp reduce to one dimension when the operands share a unit; round / mod /
   rem / hypot / abs fold on [px] operands; calc folds through the generic
   simplifier. A non-static operand keeps the call. *)
(* CSS Values 4 sec. 6: a zero [<length>] drops its unit ([0px] -> [0]). That
   leaves [<length>] for [<number>], so it is a type-changing rewrite, not a
   shorter spelling - the printer keeps [0px] and the strip happens here. Only a
   top-level [<length>] strips: a calc / function operand keeps its unit, and a
   zero [<percentage>] never strips (unsound: [0%] is not [0] in every context). *)
let strip_zero_length (l : length) : length =
  match l with Pct _ | Zero -> l | _ -> if length_is_zero l then Zero else l

(* [strip] is true for a top-level [<length>] (a direct property/shorthand
   value) and false for a calc / math-function operand, which keeps its unit. *)
let rec normalize_length ?(strip = true) ?(ctx = default_calc_ctx) (l : length)
    : length =
  let nf = normalize_length ~strip:false ~ctx in
  let result =
    match l with
    | Calc cv -> (
        match
          cv |> eval_length_calc ~ctx |> linear_length_calc
          |> eval_length_calc ~ctx
        with
        | Val v -> v
        | folded -> Calc folded)
    | Clamp (mn, v, mx) -> (
        let mn = nf mn and v = nf v and mx = nf mx in
        if mn = v && v = mx then v
        else
          (* clamp(lo, v, hi) = max(lo, min(v, hi)); folds to one value when all
             three share a unit, returning lo when lo > hi. *)
          match try_reduce_typed_min_max [ v; mx ] Float.min with
          | Some inner -> (
              match try_reduce_typed_min_max [ mn; inner ] Float.max with
              | Some r -> r
              | None -> Clamp (mn, v, mx))
          | None -> Clamp (mn, v, mx))
    | Min xs -> (
        let xs = List.map nf xs in
        match try_reduce_typed_min_max xs Float.min with
        | Some r -> r
        | None -> Min xs)
    | Max xs -> (
        let xs = List.map nf xs in
        match try_reduce_typed_min_max xs Float.max with
        | Some r -> r
        | None -> Max xs)
    | Minmax (mn, mx) -> Minmax (nf mn, nf mx)
    | Round (strategy, value, step) -> (
        match (nf value, nf step) with
        | Px v, Px s when s <> 0. -> Px (round_length_step strategy v s)
        | value, step -> Round (strategy, value, step))
    | Mod (a, b) -> (
        match (nf a, nf b) with
        | Px a, Px b when b <> 0. -> Px (a -. (Float.floor (a /. b) *. b))
        | a, b -> Mod (a, b))
    | Rem_fn (a, b) -> (
        match (nf a, nf b) with
        | Px a, Px b when b <> 0. -> Px (Float.rem a b)
        | a, b -> Rem_fn (a, b))
    | Hypot xs -> (
        let xs = List.map nf xs in
        match xs with
        | [ (Px _ as v) ] -> v
        | _ -> (
            match px_values xs with
            | Some (_ :: _ as vs) ->
                Px
                  (round_computed
                     (Float.sqrt
                        (List.fold_left (fun acc f -> acc +. (f *. f)) 0. vs)))
            | _ -> Hypot xs))
    | Abs v -> ( match nf v with Px x -> Px (Float.abs x) | v -> Abs v)
    | Sign v -> Sign (nf v)
    | Fit_content_arg arg -> Fit_content_arg (nf arg)
    | Calc_size (basis, calc) -> (
        let basis = nf basis in
        match
          calc |> eval_length_calc ~ctx |> linear_length_calc
          |> eval_length_calc ~ctx
        with
        | folded -> Calc_size (basis, folded))
    | _ -> l
  in
  if strip then strip_zero_length result else result

(* Fold the numeric parts of a length-percentage [calc()], keeping any [var()]:
   [calc(var(--x) + 1px + 2px)] -> [calc(var(--x) + 3px)], [calc(1px + 2px)] ->
   [3px]. A wrapped [<length>] folds its own math functions. *)
let normalize_length_percentage ?(strip = true) ?(ctx = default_calc_ctx)
    (lp : length_percentage) : length_percentage =
  match lp with
  | Calc c -> (
      match c |> eval_lp_calc ~ctx |> linear_lp_calc |> eval_lp_calc ~ctx with
      | Val v -> v
      | folded -> Calc folded)
  | Length l ->
      let l' = normalize_length ~strip ~ctx l in
      if l' == l then lp else Length l'
  | _ -> lp

(* Evaluate the static CSS math functions on [<number>] (the folds the printer
   used to do under minify), so the printer stays a pure serialiser. Recurses so
   nested calls fold ([abs(hypot(3, 4))] -> [5]); a non-static operand keeps the
   call. *)
let rec normalize_number ?(ctx = default_calc_ctx) (n : number) : number =
  let nf = normalize_number ~ctx in
  match n with
  | Calc c -> (
      match eval_calc ~ctx c with
      | Num f -> Num f
      | Val v -> nf v
      | folded -> Calc folded)
  | Round (strategy, a, b) -> (
      match (nf a, nf b) with
      | Num value, Num step when step <> 0. ->
          Num (round_to_step strategy value step)
      | a, b -> Round (strategy, a, b))
  | Mod (a, b) -> (
      match (nf a, nf b) with
      | Num a, Num b when b <> 0. -> Num (mod_value a b)
      | a, b -> Mod (a, b))
  | Rem (a, b) -> (
      match (nf a, nf b) with
      | Num a, Num b when b <> 0. -> Num (Float.rem a b)
      | a, b -> Rem (a, b))
  | Hypot (a, b) -> (
      match (nf a, nf b) with
      | Num a, Num b -> Num (Float.sqrt ((a *. a) +. (b *. b)))
      | a, b -> Hypot (a, b))
  | Pow (a, b) -> (
      match (nf a, nf b) with
      | Num a, Num b -> Num (Float.pow a b)
      | a, b -> Pow (a, b))
  | Sqrt v -> (
      match nf v with Num a when a >= 0. -> Num (Float.sqrt a) | v -> Sqrt v)
  | Abs v -> ( match nf v with Num a -> Num (Float.abs a) | v -> Abs v)
  | Sign v -> Sign (nf v)
  | Sin _ | Num _ | Var _ -> n

(* Fold the value-independent parts of a [<percentage>] [calc()] ([calc(1 / 2 *
   100%)] -> [calc(.5*100%)]), keeping any [var()]. Replaces the numeric /
   identity reduction the printer did under minify, now that pp is a pure
   serialiser. *)
(* [<percentage>] scaling/combining: [calc(50% * 2)] -> [100%], [calc(10% +
   20%)] -> [30%]. A bare [<number>] operand stays a [Num] leaf and folds
   through the generic numeric arms. *)
let pct_scale ?(exact = true) op (p : percentage) n : percentage option =
  if not (Float.is_finite n) then Option.none
  else
    let scaled = scale_leaf ~exact op n in
    match p with
    | Pct v -> scaled (fun x -> (Pct x : percentage)) v
    | Num v -> scaled (fun x -> (Num x : percentage)) v
    | _ -> Option.none

let pct_combine op (a : percentage) (b : percentage) : percentage option =
  let f = combine_leaf op in
  match (a, b) with
  | Pct x, Pct y -> Option.map (fun r -> (Pct r : percentage)) (f x y)
  | Num x, Num y -> Option.map (fun r -> (Num r : percentage)) (f x y)
  | _ -> Option.none

let pct_is_zero : percentage -> bool = function
  | Pct f | Num f -> f = 0.
  | _ -> false

let pct_calc_math_fn (fn : math_fn) : percentage calc =
  typed_math_fn
    (fun unit v ->
      if String.equal unit "%" then Option.some (Pct v : percentage)
      else Option.none)
    fn

let eval_pct_calc ?(ctx = default_calc_ctx) (c : percentage calc) :
    percentage calc =
  eval_typed_calc ~math_fn:pct_calc_math_fn
    ~scale:(fun ~exact op v n -> pct_scale ~exact op v n)
      (* [Pp.pct] / [Pp.float] print a [<percentage>] or [<number>] coefficient
         in full in both modes, so there is no budget to apply here. *)
    ~combine:pct_combine ~round:Fun.id ~is_zero:pct_is_zero
    ~zero:(Val (Pct 0.) : percentage calc)
    ~ctx c

let normalize_percentage ?(ctx = default_calc_ctx) (p : percentage) : percentage
    =
  match p with
  | Calc c -> (
      match eval_pct_calc ~ctx c with
      | Num f -> Num f
      | Val v -> v
      | folded -> Calc folded)
  | Pct _ | Num _ | Var _ -> p

(* [<time>] scaling/combining ([calc(1s * 2)] -> [2s], [calc(.5s + .5s)] ->
   [1s]); same-unit only, so [s] and [ms] stay distinct. A bare-number reduction
   stays wrapped (a [<number>] is not a [<time>]). *)
let time_scale ?(exact = true) op (d : duration) n : duration option =
  if not (Float.is_finite n) then Option.none
  else
    let scaled = scale_leaf ~exact op n in
    match d with
    | S v -> scaled (fun x -> S x) v
    | Ms v -> scaled (fun x -> Ms x) v
    | _ -> Option.none

let time_combine op (a : duration) (b : duration) : duration option =
  let f = combine_leaf op in
  match (a, b) with
  | S x, S y -> Option.map (fun r -> S r) (f x y)
  | Ms x, Ms y -> Option.map (fun r -> Ms r) (f x y)
  | _ -> Option.none

let time_is_zero : duration -> bool = function
  | S f | Ms f -> f = 0.
  | _ -> false

let time_round : duration -> duration = function
  | S f -> S (round_computed f)
  | Ms f -> Ms (round_computed f)
  | d -> d

let time_calc_math_fn (fn : math_fn) : duration calc =
  typed_math_fn
    (fun unit v ->
      match unit with
      | "s" -> Option.some (S v : duration)
      | "ms" -> Option.some (Ms v : duration)
      | _ -> Option.none)
    fn

let eval_time_calc ?(ctx = default_calc_ctx) (c : duration calc) : duration calc
    =
  eval_typed_calc ~math_fn:time_calc_math_fn
    ~scale:(fun ~exact op v n -> time_scale ~exact op v n)
    ~combine:time_combine ~round:time_round ~is_zero:time_is_zero
    ~zero:(Val (S 0.) : duration calc)
    ~ctx c

(* Choose equivalent time units and evaluate static stepped functions in the
   AST, before declaration hashes are compared. *)
let rec normalize_duration ?(ctx = default_calc_ctx) ?(canonicalize_ms = true)
    (d : duration) : duration =
  let normalize = normalize_duration ~ctx ~canonicalize_ms in
  let preserve rebuilt = if rebuilt = d then d else rebuilt in
  match d with
  | Ms f when canonicalize_ms ->
      let seconds = f /. 1000. in
      let ms = Pp.string_of_float ~drop_leading_zero:true f in
      let s = Pp.string_of_float ~drop_leading_zero:true seconds in
      if String.length s + 1 <= String.length ms + 2 then S seconds else d
  | Durations durations ->
      preserve (Durations (Common.List.map_preserve normalize durations))
  | Round (strategy, value, step) -> (
      let value = normalize value and step = normalize step in
      match (value, step) with
      | Ms value, Ms step when step <> 0. ->
          normalize (Ms (round_to_step strategy value step))
      | S value, S step when step <> 0. ->
          normalize (S (round_to_step strategy value step))
      | _ -> preserve (Round (strategy, value, step)))
  | Rem (a, b) -> (
      let a = normalize a and b = normalize b in
      match (a, b) with
      | Ms a, Ms b when b <> 0. -> normalize (Ms (Float.rem a b))
      | S a, S b when b <> 0. -> normalize (S (Float.rem a b))
      | _ -> preserve (Rem (a, b)))
  | Mod (a, b) -> (
      let a = normalize a and b = normalize b in
      match (a, b) with
      | Ms a, Ms b when b <> 0. -> normalize (Ms (mod_value a b))
      | S a, S b when b <> 0. -> normalize (S (mod_value a b))
      | _ -> preserve (Mod (a, b)))
  | Calc c -> (
      match eval_time_calc ~ctx c with
      | Val v -> normalize v
      | folded -> preserve (Calc folded))
  | Var v ->
      let fallback =
        match v.fallback with
        | Fallback value ->
            let value' = normalize value in
            if value' == value then v.fallback else Fallback value'
        | (Empty | Empty2 | None | Syntax_fallback _ | Var_fallback _) as other
          ->
            other
      in
      let default =
        match v.default with
        | Some value ->
            let value' = normalize value in
            if value' == value then v.default else Some value'
        | None -> v.default
      in
      if fallback == v.fallback && default == v.default then d
      else Var { v with fallback; default }
  | _ -> d

(* CSS Values 4 sec. 10.10.1: a typed [<angle>] multiplied or divided by a
   unitless number scales the angle's coefficient and keeps its unit, so
   [calc(1deg * -45)] reduces to [-45deg]. Division folds only when the quotient
   is [exact] (see [exact_div]); dividing by a math constant ([pi] / ...) is
   irrational, so it rounds like the matching multiplication. *)
let angle_scale ?(exact = true) op (a : angle) n : angle option =
  if not (Float.is_finite n) then Option.none
  else
    let scaled = scale_leaf ~exact op n in
    match a with
    | Deg v -> scaled (fun x -> Deg x) v
    | Rad v -> scaled (fun x -> Rad x) v
    | Turn v -> scaled (fun x -> Turn x) v
    | Grad v -> scaled (fun x -> Grad x) v
    | _ -> Option.none

let angle_is_zero : angle -> bool = function
  | Deg f | Rad f | Turn f | Grad f -> f = 0.
  | _ -> false

(* CSS Values 4 sec. 10.10.1: same-unit add/sub of two typed angles reduces to
   one angle ([calc(45deg + 45deg)] -> [90deg]). Cross-unit operands (e.g.
   [1turn + 90deg]) stay unfolded; [normalize_angle] picks the shortest spelling
   of a single folded operand, not across a mixed sum. *)
let angle_combine op (a : angle) (b : angle) : angle option =
  let f = combine_leaf op in
  match (a, b) with
  | Deg x, Deg y -> Option.map (fun r -> Deg r) (f x y)
  | Rad x, Rad y -> Option.map (fun r -> Rad r) (f x y)
  | Turn x, Turn y -> Option.map (fun r -> Turn r) (f x y)
  | Grad x, Grad y -> Option.map (fun r -> Grad r) (f x y)
  | _ -> Option.none

let angle_round : angle -> angle = function
  | Deg f -> Deg (round_computed f)
  | Rad f -> Rad (round_computed f)
  | Turn f -> Turn (round_computed f)
  | Grad f -> Grad (round_computed f)
  | a -> a

let angle_calc_math_fn (fn : math_fn) : angle calc =
  match fn with
  (* CSS Values 4 sec. 10.9: the inverse trig functions return an [<angle>],
     which [eval_math_fn] reports in degrees. *)
  | Asin _ | Acos _ | Atan _ | Atan2 _ -> (
      match eval_math_fn fn with
      | Some v when not (Float.is_nan v) -> Val (Deg v : angle)
      | Some _ | None -> Math_fn fn)
  | fn ->
      typed_math_fn
        (fun unit v ->
          match unit with
          | "deg" -> Option.some (Deg v : angle)
          | "rad" -> Option.some (Rad v : angle)
          | "turn" -> Option.some (Turn v : angle)
          | "grad" -> Option.some (Grad v : angle)
          | _ -> Option.none)
        fn

let eval_angle_calc ?(ctx = default_calc_ctx) (c : angle calc) : angle calc =
  eval_typed_calc ~math_fn:angle_calc_math_fn
    ~scale:(fun ~exact op v n -> angle_scale ~exact op v n)
    ~combine:angle_combine ~round:angle_round ~is_zero:angle_is_zero
    ~zero:(Val (Deg 0.)) ~ctx c

(* Canonicalise an [<angle>]: fold the static math functions ([round] / [mod] /
   [rem] on [deg] operands), then pick the shortest of the
   losslessly-interconvertible spellings (deg / turn / grad). [rad] goes through
   pi, so it cannot share one magnitude and is left as-is. *)
(* Shortest spelling of a concrete angle, via unit conversions whose printed form
   still denotes the same value. [deg -> turn] can floor a small angle to [0turn]
   once the printer caps the mantissa; comparing printed forms back in degrees
   rejects such value changes. The original spelling seeds the fold; ties prefer
   deg. *)
let angle_shortest (a : angle) : angle =
  let printed f = Pp.string_of_float ~drop_leading_zero:true f in
  let render unit f = String.length (printed f) + String.length unit in
  let printed_deg unit f =
    match float_of_string_opt (printed f) with
    | Some v -> (
        match unit with "turn" -> v *. 360. | "grad" -> v *. 0.9 | _ -> v)
    | None -> Float.nan
  in
  let cands =
    match a with
    | Deg f -> [ (f, "deg", a); (f /. 360., "turn", Turn (f /. 360.)) ]
    | Turn f -> [ (f, "turn", a); (f *. 360., "deg", Deg (f *. 360.)) ]
    | Grad f -> [ (f, "grad", a); (f *. 0.9, "deg", Deg (f *. 0.9)) ]
    | _ -> []
  in
  match cands with
  | [] -> a
  | (f0, u0, n0) :: rest ->
      let ref_deg = printed_deg u0 f0 in
      let round_trips (f, u, _) =
        (* Exact conversion only: the converted spelling must denote the same
           angle, modulo the float noise of the unit arithmetic, not merely a
           close one. A [grad -> deg] re-spelling that drops a significant
           figure (or a small angle collapsing to [0turn]) is a value change, so
           the authored unit is kept. *)
        Float.abs (printed_deg u f -. ref_deg)
        <= (1e-9 *. Float.abs ref_deg) +. 1e-15
      in
      let rest = List.filter round_trips rest in
      snd
        (List.fold_left
           (fun (best_len, best) (f, u, n) ->
             let l = render u f in
             if l < best_len then (l, n) else (best_len, best))
           (render u0 f0, n0)
           rest)

let normalize_angle ?(ctx = default_calc_ctx) =
  let rec go (a : angle) : angle =
    match a with
    | Round (strategy, v, s) -> (
        match (go v, go s) with
        | Deg v, Deg s when s <> 0. ->
            angle_shortest (Deg (round_to_step strategy v s))
        | v, s -> Round (strategy, v, s))
    | Mod (x, y) -> (
        match (go x, go y) with
        | Deg x, Deg y when y <> 0. -> angle_shortest (Deg (mod_value x y))
        | x, y -> Mod (x, y))
    | Rem (x, y) -> (
        match (go x, go y) with
        | Deg x, Deg y when y <> 0. -> angle_shortest (Deg (Float.rem x y))
        | x, y -> Rem (x, y))
    | Calc c -> (
        match eval_angle_calc ~ctx c with
        | Val v -> go v
        | folded -> Calc folded)
    | Deg _ | Turn _ | Grad _ -> angle_shortest a
    (* [angle_shortest] leaves radians alone because deg/rad conversion goes
       through pi and so is never exactly value-preserving. Zero is the one
       radian value that converts exactly, and it is the one that matters: a
       zero angle is what the grammars let you drop. *)
    | Rad f when f = 0. -> Deg 0.
    | Rad _ | Var _ | Invalid _ -> a
  in
  go

let rec pp_length_percentage ?(always = false) : length_percentage Pp.t =
 fun ctx -> function
  | Length l -> pp_length ~always ctx l
  | Pct f -> Pp.pct ctx f
  | Env env -> pp_env (pp_length_percentage ~always) ctx env
  | Var v -> pp_var (pp_length_percentage ~always) ctx v
  | Calc c ->
      (* Inline mode substitutes a [var()]'s default value into the calc. *)
      let c = if Pp.minified ctx then resolve_lp_calc_vars ctx c else c in
      let always = always || calc_contains_var c in
      pp_calc_with ~unwrap_num:false (pp_length_percentage ~always) ctx c
  | Invalid tokens ->
      Pp.string ctx
        (if Pp.minified ctx then Parser.to_string_minified tokens
         else Parser.string_of_components tokens)

(* Pure serialiser: the [Pct] <-> [Num] shortest-spelling choice ([Pct 0. -> 0]
   included) is a node-changing fold in [normalize_number_percentage], not here.
   [~always] is kept for caller signature parity but has no effect on top-level
   emission. *)
let rec pp_number_percentage ?(always = false) : number_percentage Pp.t =
 fun ctx -> function
  | Num f -> Pp.float ctx f
  | Pct f -> Pp.pct ctx f
  | Var v -> pp_var (pp_number_percentage ~always) ctx v
  | Calc c -> pp_calc (pp_number_percentage ~always) ctx c

(* AST-level [<number-percentage>] canonicalisation: CSS Transforms 2 secs. 5 and
   12.1-12.2 and Filter Effects 1 sec. 6.1 define typed positions where [%] and
   number are equivalent (100% = 1). Pick the shorter leaf so
   [pp_number_percentage] serialises a canonical node. [Var] / [Calc] are left
   alone (inside calc() [%] and number aren't interchangeable). *)
(* [<number-percentage>] shares [<percentage>]'s scaling/combining; the type is
   distinct, so the helpers are too. *)
let np_scale ?(exact = true) op (np : number_percentage) n :
    number_percentage option =
  if not (Float.is_finite n) then Option.none
  else
    let scaled = scale_leaf ~exact op n in
    match np with
    | Pct v -> scaled (fun x -> (Pct x : number_percentage)) v
    | Num v -> scaled (fun x -> (Num x : number_percentage)) v
    | _ -> Option.none

let np_combine op (a : number_percentage) (b : number_percentage) :
    number_percentage option =
  let f = combine_leaf op in
  match (a, b) with
  | Pct x, Pct y -> Option.map (fun r -> (Pct r : number_percentage)) (f x y)
  | Num x, Num y -> Option.map (fun r -> (Num r : number_percentage)) (f x y)
  | _ -> Option.none

let np_is_zero : number_percentage -> bool = function
  | Pct f | Num f -> f = 0.
  | _ -> false

let np_calc_math_fn (fn : math_fn) : number_percentage calc =
  typed_math_fn
    (fun unit v ->
      if String.equal unit "%" then Option.some (Pct v : number_percentage)
      else Option.none)
    fn

let eval_np_calc ?(ctx = default_calc_ctx) (c : number_percentage calc) :
    number_percentage calc =
  eval_typed_calc ~math_fn:np_calc_math_fn
    ~scale:(fun ~exact op v n -> np_scale ~exact op v n)
      (* A [<number>] / [<percentage>] coefficient prints in full. *)
    ~combine:np_combine ~round:Fun.id ~is_zero:np_is_zero
    ~zero:(Val (Num 0.) : number_percentage calc)
    ~ctx c

let rec normalize_number_percentage ?(ctx = default_calc_ctx)
    (np : number_percentage) : number_percentage =
  let len f = String.length (Pp.string_of_float ~drop_leading_zero:true f) in
  match np with
  | Calc c -> (
      match eval_np_calc ~ctx c with
      | Num f -> normalize_number_percentage ~ctx (Num f)
      | Val v -> normalize_number_percentage ~ctx v
      | folded -> Calc folded)
  | Pct f ->
      let num_f = f /. 100. in
      (* num spelling vs pct spelling (one extra '%' character); Num wins on tie
         to match the original pp logic and keep the fold idempotent. *)
      if len num_f <= len f + 1 then (Num num_f : number_percentage) else np
  | Num f ->
      let pct_f = f *. 100. in
      if len pct_f + 1 < len f then (Pct pct_f : number_percentage) else np
  | other -> other

(* Convert the three channels of a [color(<space> ...)] value to Oklab so a
   candidate rounding can be measured against the color-difference budget.
   Returns [None] for spaces that are not [color()] predefined-RGB / XYZ
   spaces. *)
let color_func_to_oklab (space : color_space) (c1, c2, c3) :
    Color_space.lab option =
  let to_oklab = Color_space.oklab_of_linear_srgb in
  let from_xyz_d65 xyz = to_oklab (Color_space.linear_srgb_of_xyz65 xyz) in
  match space with
  | Srgb ->
      Some
        (to_oklab
           ( Color_space.linear_of_srgb c1,
             Color_space.linear_of_srgb c2,
             Color_space.linear_of_srgb c3 ))
  | Srgb_linear -> Some (to_oklab (c1, c2, c3))
  | Display_p3 ->
      Some
        (from_xyz_d65
           (Color_space.xyz65_of_linear_p3
              ( Color_space.linear_of_display_p3 c1,
                Color_space.linear_of_display_p3 c2,
                Color_space.linear_of_display_p3 c3 )))
  | A98_rgb ->
      Some
        (from_xyz_d65
           (Color_space.xyz65_of_linear_a98
              ( Color_space.linear_of_a98_rgb c1,
                Color_space.linear_of_a98_rgb c2,
                Color_space.linear_of_a98_rgb c3 )))
  | Prophoto_rgb ->
      Some
        (from_xyz_d65
           (Color_space.d65_of_xyz50
              (Color_space.xyz50_of_linear_prophoto
                 ( Color_space.linear_of_prophoto_rgb c1,
                   Color_space.linear_of_prophoto_rgb c2,
                   Color_space.linear_of_prophoto_rgb c3 ))))
  | Rec2020 ->
      Some
        (from_xyz_d65
           (Color_space.xyz65_of_linear_rec2020
              ( Color_space.linear_of_rec2020 c1,
                Color_space.linear_of_rec2020 c2,
                Color_space.linear_of_rec2020 c3 )))
  | Xyz | Xyz_d65 -> Some (from_xyz_d65 (c1, c2, c3))
  | Xyz_d50 -> Some (from_xyz_d65 (Color_space.d65_of_xyz50 (c1, c2, c3)))
  | Lab | Oklab | Lch | Oklch | Hsl | Hwb -> None

(* The three numeric channels of a [color()] value, with percentages normalised
   to [0, 1]; [None] when any channel is non-numeric (var/calc/none). *)
let color_numeric_channels (components : component list) =
  match components with
  | [ a; b; c ] -> (
      let value (component : component) =
        match component with
        | Num f -> Some f
        | Pct f -> Some (f /. 100.)
        | _ -> None
      in
      match (value a, value b, value c) with
      | Some a, Some b, Some c -> Some (a, b, c)
      | _ -> None)
  | _ -> None

(* Fewest decimals whose round-trip stays within the documented 0.002 Oklab
   budget (README). Channels in gamma-encoded RGB spaces reach it at 3 decimals;
   linear-light / XYZ spaces, where a fixed channel step carries more Oklab
   distance, need 4. Falls back to 4 when the budget cannot be checked. *)
let color_channel_decimals (space : color_space)
    (channels : (float * float * float) option) =
  match channels with
  | None -> 4
  | Some (c1, c2, c3) -> (
      match color_func_to_oklab space (c1, c2, c3) with
      | None -> 4
      | Some orig ->
          let round_dec n x =
            let m = 10. ** float_of_int n in
            Float.round (x *. m) /. m
          in
          let within n =
            match
              color_func_to_oklab space
                (round_dec n c1, round_dec n c2, round_dec n c3)
            with
            | Some rounded -> Color_space.oklab_distance orig rounded <= 0.002
            | None -> false
          in
          let rec find n =
            if n >= 6 then 6 else if within n then n else find (n + 1)
          in
          find 3)

let pp_component_float ~decimals ctx f =
  let decimals = if ctx.Pp.lossless then 8 else decimals in
  if Pp.minified ctx then
    Pp.string ctx
      (Pp.string_of_float ~drop_leading_zero:true ~max_decimals:decimals f)
  else Pp.float ctx f

let rec pp_component_prec ~decimals : component Pp.t =
 fun ctx -> function
  | Num f -> pp_component_float ~decimals ctx f
  | Pct f when Pp.minified ctx -> pp_component_float ~decimals ctx (f /. 100.)
  | Pct f ->
      Pp.float ctx f;
      Pp.char ctx '%'
  | Angle h -> pp_hue ctx h
  | Var v -> pp_var (pp_component_prec ~decimals) ctx v
  | Calc c -> pp_calc (pp_component_prec ~decimals) ctx c
  | Component_none -> Pp.string ctx "none"

(* Context-free single component: without the sibling channels the Oklab budget
   cannot be checked, so use the budget-safe 4-decimal fallback. *)
let pp_component : component Pp.t = pp_component_prec ~decimals:4

let pp_hue_interpolation : hue_interpolation Pp.t =
 fun ctx -> function
  | Shorter -> Pp.string ctx "shorter"
  | Longer -> Pp.string ctx "longer"
  | Increasing -> Pp.string ctx "increasing"
  | Decreasing -> Pp.string ctx "decreasing"
  | Specified -> Pp.string ctx "specified"
  | Default -> ()

let static_component_can_touch_negative (component : component) =
  match component with
  | Num _ | Pct _ -> true
  | Angle _ | Var _ | Calc _ | Component_none -> false

let static_component_starts_negative (component : component) =
  match component with
  | Num f | Pct f -> f < 0.
  | Angle _ | Var _ | Calc _ | Component_none -> false

let color_component_needs_space ctx prev component =
  not
    (Pp.minified ctx
    && static_component_can_touch_negative prev
    && static_component_starts_negative component)

let rec pp_color_component_tail ~decimals ctx prev = function
  | [] -> ()
  | component :: rest ->
      if color_component_needs_space ctx prev component then Pp.space ctx ();
      pp_component_prec ~decimals ctx component;
      pp_color_component_tail ~decimals ctx component rest

let pp_color_components ~decimals : component list Pp.t =
 fun ctx -> function
  | [] -> ()
  | first :: rest ->
      pp_component_prec ~decimals ctx first;
      pp_color_component_tail ~decimals ctx first rest

(* Helpers to pretty print CSS color functions using Pp.call *)
let pp_rgb_args : (channel * channel * channel * alpha) Pp.t =
 fun ctx (r, g, b, alpha) ->
  Pp.list ~sep:Pp.pct_sp pp_channel ctx [ r; g; b ];
  pp_opt_alpha ctx alpha

let pp_rgb_func = Pp.call "rgb" pp_rgb_args

let rec pp_rgb : rgb Pp.t =
 fun ctx -> function
  | Channels { r; g; b } -> Pp.list ~sep:Pp.pct_sp pp_channel ctx [ r; g; b ]
  | Var v -> pp_var pp_rgb ctx v

(** Lab-like float string. CSSOM serialisation (CSSOM 1 sec. 6.7.2) drops a
    leading zero on fractional numbers; the coefficient prints in full so the
    value round-trips, with precision reduction left to [normalize_color]. *)
let string_of_lab_float f =
  (* The printer serialises a lab/oklab coefficient faithfully in both pretty
     and minified output: reducing its precision changes the colour (and can
     cross an sRGB 8-bit boundary), so it is a lossy, node-changing fold that
     belongs in [optimize] (the oklab -> hex collapse via [round_color_axis]),
     not in [pp]. This mirrors [pp_component_float] for the [color()]
     function. *)
  Pp.string_of_float ~drop_leading_zero:true f

let pp_lab_float ctx f = Pp.string ctx (string_of_lab_float f)

let string_of_scaled_color_axis ~pct_scale ctx f =
  let n = string_of_lab_float f in
  (* The number -> percentage axis swap (e.g. oklch chroma [.304] -> [76%]) is a
     shortest-spelling minify win, but [%] on these axes is an evergreen-target
     fact; [--enforce-spec] keeps the spec-canonical number serialisation. *)
  if ctx.Pp.minify && (not ctx.Pp.lossless) && not ctx.Pp.enforce_spec then
    (* Derive the percentage form from what the number string [n] re-parses to,
       not the raw float [f]: an [f] rounding to [n] ([-0.00798] -> "-.008")
       could keep the number for a long raw percentage ("-1.995%") while the
       re-parsed [-0.008] has a short one ("-2%"), flipping on the next pass
       (non-idempotent). *)
    let n_value = try float_of_string n with Failure _ -> f in
    let pct = string_of_lab_float (n_value /. pct_scale) ^ "%" in
    if String.length pct < String.length n then pct else n
  else n

let space_after_color_percentage ctx (l : percentage option) ~next =
  (* The space between the L channel and the next colour component elides when
     the L spelling closes its token cleanly: a [%] ends its percentage token
     whatever follows, and a bare-number end ([4] in [.654]) needs a sign-token
     [+] / [-] to start the next number. Functions and [None] / unknown
     left-hand values stay conservative. *)
  let starts_signed s = String.length s > 0 && (s.[0] = '+' || s.[0] = '-') in
  let elidable =
    Pp.minified ctx
    && (Pp.last_char ctx = Some '%'
       ||
       match (l, next) with
       | Some (Num _), Some s -> starts_signed s
       | _ -> false)
  in
  if not elidable then Pp.space ctx ()

let starts_unsigned_number s =
  String.length s > 0
  && match s.[0] with '0' .. '9' | '.' -> true | _ -> false

(* Pure serialiser: the percentage<->number choice for L is made in the AST
   normalize pass ([canonical_color_lightness]), so this faithfully prints
   whichever node it is given ([Pct] -> [%], [Num] -> bare number). The
   precision is fixed in the AST normalize pass, so this prints in full. *)
let pp_color_lightness ctx (l : percentage option) : percentage option =
  (match l with
  | Some (Pct f) ->
      pp_lab_float ctx f;
      Pp.char ctx '%'
  | Some (Num f) -> pp_lab_float ctx f
  | Some l -> pp_percentage ctx l
  | None -> Pp.string ctx "none");
  l

let pp_pct_chroma_hue_alpha ~chroma_pct_scale :
    (percentage option * float option * hue * alpha) Pp.t =
 fun ctx (l, c, h, alpha) ->
  let printed_l = pp_color_lightness ctx l in
  (match c with
  | Some c ->
      let c = string_of_scaled_color_axis ~pct_scale:chroma_pct_scale ctx c in
      space_after_color_percentage ctx printed_l ~next:(Some c);
      Pp.string ctx c
  | None ->
      space_after_color_percentage ctx printed_l ~next:(Some "none");
      Pp.string ctx "none");
  Pp.pct_sp ctx ();
  pp_hue ctx h;
  pp_opt_alpha ctx alpha

let pp_oklch = Pp.call "oklch" (pp_pct_chroma_hue_alpha ~chroma_pct_scale:0.004)

let pp_hue_pct_pct_alpha : (hue * percentage * percentage * alpha) Pp.t =
 fun ctx (h, s, l, a) ->
  pp_hue ctx h;
  Pp.space ctx ();
  pp_percentage ctx s;
  Pp.pct_sp ctx ();
  pp_percentage ctx l;
  pp_opt_alpha ctx a

let pp_hsl = Pp.call "hsl" pp_hue_pct_pct_alpha
let pp_hwb = Pp.call "hwb" pp_hue_pct_pct_alpha

(** Print a float always dropping leading zeros (for lab-like color axes). Under
    minify the float rounds to 3 decimals - CSS Color 4 alpha precision is 1/255
    ~ 0.004 so 3 decimals is more than display-accurate. *)
let pp_float_drop_zero ctx f =
  let max_decimals = if Pp.minified ctx && not ctx.Pp.lossless then 3 else 8 in
  Pp.float_n max_decimals ctx f

let pp_alpha_drop_zero : alpha Pp.t =
 fun ctx -> function
  | None -> ()
  | Num f -> pp_float_drop_zero ctx f
  | Pct f -> pp_float_drop_zero ctx (f /. 100.)
  | Var v -> pp_var pp_alpha ctx v
  | Calc c -> pp_calc pp_alpha ctx c

let string_of_lab_axis ~pct_scale ctx f =
  string_of_scaled_color_axis ~pct_scale ctx f

let pp_lab_like_args ~axis_pct_scale :
    (percentage option * float option * float option * alpha) Pp.t =
 fun ctx (l, a, b, alpha) ->
  let printed_l = pp_color_lightness ctx l in
  let string_of_axis = function
    | Some f -> string_of_lab_axis ~pct_scale:axis_pct_scale ctx f
    | None -> "none"
  in
  let a = string_of_axis a in
  let b = string_of_axis b in
  space_after_color_percentage ctx printed_l ~next:(Some a);
  Pp.string ctx a;
  (* CSS Color 4 lab / oklab tokens parse the same with or without whitespace
     between an unsigned-number [a] and a sign-prefixed [b] ([.285] then [-.149]
     is two number tokens either way), so under cascade's README shortest-valid
     policy the gap elides when the boundary is unambiguous. *)
  if
    not
      (ctx.Pp.minify && starts_unsigned_number a
      && String.length b > 0
      && (b.[0] = '-' || b.[0] = '+'))
  then Pp.pct_sp ctx ();
  Pp.string ctx b;
  match alpha with
  | None -> ()
  | a when ctx.Pp.minify && alpha_is_full a -> ()
  | a ->
      Pp.op_char ctx '/';
      pp_alpha_drop_zero ctx a

let pp_lab = Pp.call "lab" (pp_lab_like_args ~axis_pct_scale:1.25)
let pp_oklab = Pp.call "oklab" (pp_lab_like_args ~axis_pct_scale:0.004)
let pp_lch = Pp.call "lch" (pp_pct_chroma_hue_alpha ~chroma_pct_scale:1.5)

let pp_color_space : color_space Pp.t =
 fun ctx -> function
  | Srgb -> Pp.string ctx "srgb"
  | Srgb_linear -> Pp.string ctx "srgb-linear"
  | Display_p3 -> Pp.string ctx "display-p3"
  | A98_rgb -> Pp.string ctx "a98-rgb"
  | Prophoto_rgb -> Pp.string ctx "prophoto-rgb"
  | Rec2020 -> Pp.string ctx "rec2020"
  | Lab -> Pp.string ctx "lab"
  | Oklab -> Pp.string ctx "oklab"
  | Xyz -> Pp.string ctx "xyz"
  | Xyz_d50 -> Pp.string ctx "xyz-d50"
  | Xyz_d65 -> Pp.string ctx (if Pp.minified ctx then "xyz" else "xyz-d65")
  | Lch -> Pp.string ctx "lch"
  | Oklch -> Pp.string ctx "oklch"
  | Hsl -> Pp.string ctx "hsl"
  | Hwb -> Pp.string ctx "hwb"

let pp_color' ctx space components alpha =
  let decimals =
    color_channel_decimals space (color_numeric_channels components)
  in
  Pp.call "color"
    (fun ctx (space, components, alpha) ->
      pp_color_space ctx space;
      (match components with
      | [] -> ()
      | _ ->
          Pp.space ctx ();
          pp_color_components ~decimals ctx components);
      pp_opt_alpha ctx alpha)
    ctx (space, components, alpha)

let pp_hex_color ctx r g b a =
  (* Pure serialiser: pick the shortest spelling of the decoded components.
     Minify collapses [#rrggbb] -> [#rgb] and drops an opaque alpha; pretty
     keeps the full byte form. The cross-node folds (hex -> named) are AST
     rewrites done by [normalize_color]. *)
  Pp.char ctx '#';
  if Pp.minified ctx then Pp.string ctx (hex_string_of_bytes r g b a)
  else (
    Pp.string ctx (hex_of_byte r);
    Pp.string ctx (hex_of_byte g);
    Pp.string ctx (hex_of_byte b);
    if a <> 255 then Pp.string ctx (hex_of_byte a))

let rec pp_rgb_as_color : rgb Pp.t =
 fun ctx -> function
  | Channels { r; g; b } -> pp_rgb_func ctx (r, g, b, None)
  | Var v -> pp_var pp_rgb_as_color ctx v

let canonical_color_name = function
  | Grey -> Gray
  | Dark_grey -> Dark_gray
  | Light_grey -> Light_gray
  | Slate_grey -> Slate_gray
  | Dark_slate_grey -> Dark_slate_gray
  | Light_slate_grey -> Light_slate_gray
  | Dim_grey -> Dim_gray
  | other -> other

let pp_transparent_color ctx = Pp.string ctx "transparent"

let color_of_default_string value =
  let value = String.trim value in
  let len = String.length value in
  let is_hex c =
    ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')
  in
  if len > 1 && value.[0] = '#' then
    let hex = String.sub value 1 (len - 1) in
    let hex_len = String.length hex in
    if
      (hex_len = 3 || hex_len = 4 || hex_len = 6 || hex_len = 8)
      && String.for_all is_hex hex
    then
      match rgba_of_hex hex with
      | Some (r, g, b, a) -> Option.Some (Hex { r; g; b; a })
      | None -> Option.None
    else Option.None
  else if
    len > 5 && String.starts_with ~prefix:"rgb(" value && value.[len - 1] = ')'
  then
    let body = String.sub value 4 (len - 5) in
    let body = String.map (function ',' -> ' ' | c -> c) body in
    let parts =
      body |> String.split_on_char ' ' |> List.filter (fun s -> s <> "")
    in
    match List.map int_of_string_opt parts with
    | [ Option.Some r; Option.Some g; Option.Some b ]
      when List.for_all (fun n -> 0 <= n && n <= 255) [ r; g; b ] ->
        Option.Some (Rgb (Channels { r = Int r; g = Int g; b = Int b }))
    | _ -> Option.None
  else Option.None

let color_of_var_resolution ctx (v : color var) =
  value_of_var_resolution ctx color_of_default_string v

let rec pp_color_in_mix : color Pp.t =
 fun ctx -> function
  | Current -> Pp.string ctx "currentcolor" (* lowercase in color-mix *)
  | c -> pp_color ctx c

and pp_color_mix ctx in_space hue color1 percent1 color2 percent2 =
  (* Pure serialiser: the interpolation method, hue strategy, and percentages
     print exactly as held. Dropping the default [in oklab] header, the default
     [shorter] hue, and the redundant percentages are AST rewrites done by
     [normalize_color]. *)
  Pp.call "color-mix"
    (fun ctx (in_space, hue, color1, percent1, color2, percent2) ->
      (match in_space with
      | Some space ->
          Pp.string ctx "in ";
          pp_color_space ctx space;
          (match hue with
          | Default -> ()
          | _ ->
              Pp.space ctx ();
              pp_hue_interpolation ctx hue;
              Pp.string ctx " hue");
          Pp.comma ctx ()
      | None -> ());
      pp_color_in_mix ctx color1;
      (match percent1 with
      | Some p ->
          Pp.space ctx ();
          pp_percentage ctx p
      | None -> ());
      Pp.comma ctx ();
      pp_color_in_mix ctx color2;
      match percent2 with
      | Some p ->
          Pp.space ctx ();
          pp_percentage ctx p
      | None -> ())
    ctx
    (in_space, hue, color1, percent1, color2, percent2)

and pp_typed_relative_color_call ctx name origin tail =
  let tail =
    if Pp.minified ctx then
      tail |> minify_relative_color_alpha |> minify_relative_color_numbers
      |> minify_relative_color_spaces
    else tail
  in
  Pp.call name
    (fun ctx (origin, tail) ->
      Pp.string ctx "from";
      Pp.space ctx ();
      pp_color ctx origin;
      Pp.space ctx ();
      Pp.string ctx tail)
    ctx (origin, tail)

and pp_rgb_color ctx = function
  | Channels { r; g; b } -> pp_rgb_func ctx (r, g, b, None)
  (* keep the [rgb()] wrapper: a bare [var(--x)] assumes [--x] is a whole
     colour, whereas [rgb(var(--x))] takes [--x] as the channel list - dropping
     the wrapper changes the value (and renders black instead of the colour).
     The var prints with [pp_rgb] so a fallback stays bare channels, not a
     nested [rgb()]. *)
  | Var v -> Pp.call "rgb" pp_rgb ctx (Var v)

and pp_rgba_color ctx rgb a =
  match rgb with
  | Channels { r; g; b } -> pp_rgb_func ctx (r, g, b, a)
  | Var v ->
      Pp.call "rgb"
        (fun ctx (v, a) ->
          pp_rgb_as_color ctx (Var v);
          pp_opt_alpha ctx a)
        ctx (v, a)

and pp_hsl_color ctx h s l a = pp_hsl ctx (h, s, l, a)
and pp_hwb_color ctx h w b a = pp_hwb ctx (h, w, b, a)

and pp_light_dark_color ctx light dark =
  Pp.call "light-dark"
    (fun ctx (light, dark) ->
      pp_color ctx light;
      Pp.comma ctx ();
      pp_color ctx dark)
    ctx (light, dark)

and pp_color_attr ctx name fallback =
  Pp.string ctx "attr(";
  Pp.string ctx name;
  Pp.space ctx ();
  Pp.string ctx "type(<color>)";
  Option.iter
    (fun fallback ->
      Pp.comma ctx ();
      pp_color ctx fallback)
    fallback;
  Pp.char ctx ')'

and pp_color_var (ctx : Pp.ctx) (v : color var) =
  match v with
  | { fallback = Syntax_fallback value; default = Option.None; _ }
    when ctx.inline ->
      let rendered =
        if Pp.minified ctx then
          Parser.to_string_custom_minified ~fold_ident:fold_custom_value_ident
            value
        else Parser.string_of_components value
      in
      Pp.string ctx (first_top_level_comma_segment rendered)
  | _ when Pp.minified ctx -> (
      match color_of_var_resolution ctx v with
      | Option.Some color -> pp_color ctx color
      | Option.None -> pp_var pp_color ctx v)
  | _ -> pp_var pp_color ctx v

and pp_color : color Pp.t = fun ctx color -> pp_color_default ctx color

and pp_color_default : color Pp.t =
 fun ctx -> function
  | Hex { r; g; b; a } -> pp_hex_color ctx r g b a
  | Authored_hex { value; r; g; b; a } ->
      if Pp.minified ctx then pp_hex_color ctx r g b a
      else (
        Pp.char ctx '#';
        Pp.string ctx value)
  | Rgb rgb -> pp_rgb_color ctx rgb
  | Rgba { rgb; a } -> pp_rgba_color ctx rgb a
  | Hsl { h; s; l; a } -> pp_hsl_color ctx h s l a
  | Hwb { h; w; b; a } -> pp_hwb_color ctx h w b a
  | Color { space; components; alpha } -> pp_color' ctx space components alpha
  | Relative_rgb (origin, tail) ->
      pp_typed_relative_color_call ctx "rgb" origin tail
  | Relative_color (name, origin, tail) ->
      pp_typed_relative_color_call ctx name origin tail
  | Contrast_color color -> Pp.call "contrast-color" pp_color ctx color
  | Light_dark (light, dark) -> pp_light_dark_color ctx light dark
  | Attribute (name, fallback) -> pp_color_attr ctx name fallback
  | Lab { l; a; b; alpha } -> pp_lab ctx (l, a, b, alpha)
  | Oklch { l; c; h; alpha } -> pp_oklch ctx (l, c, h, alpha)
  | Oklab { l; a; b; alpha } -> pp_oklab ctx (l, a, b, alpha)
  | Lch { l; c; h; alpha } -> pp_lch ctx (l, c, h, alpha)
  | Named name -> pp_color_name ctx name
  | System sc -> pp_system_color ctx sc
  | Var v -> pp_color_var ctx v
  | Current ->
      Pp.string ctx (if ctx.in_function then "currentcolor" else "currentColor")
  | Transparent -> pp_transparent_color ctx
  | Auto -> Pp.string ctx "auto"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Mix { in_space; hue; color1; percent1; color2; percent2 } ->
      pp_color_mix ctx in_space hue color1 percent1 color2 percent2

let pp_specified_color = pp_color_default

(* CSS Values 4 sec. 7.2: [ms] and [s] are interchangeable. Pure minified
   serialization picks the shorter exact leaf spelling as a fallback for callers
   that deliberately skip AST normalization. [shorten_ms:false] keeps the unit
   wherever the normalization would not have converted it either: a [calc()]
   operand, and the [interest-delay] grammar. *)
let pp_duration_unit ?(shorten_ms = true) ctx f suffix =
  if f = 0. then
    if Pp.minified ctx then Pp.string ctx "0s" else pp_unit ctx f suffix
  else if (not shorten_ms) || (not (Pp.minified ctx)) || suffix <> "ms" then
    pp_unit ctx f suffix
  else
    let seconds = f /. 1000. in
    let ms = Pp.string_of_float ~drop_leading_zero:true f in
    let s = Pp.string_of_float ~drop_leading_zero:true seconds in
    if String.length s + 1 <= String.length ms + 2 then (
      Pp.string ctx s;
      Pp.char ctx 's')
    else (
      Pp.string ctx ms;
      Pp.string ctx "ms")

let rec pp_duration_with ~shorten_ms : duration Pp.t =
 fun ctx -> function
  | Ms f -> pp_duration_unit ~shorten_ms ctx f "ms"
  | S f -> pp_duration_unit ctx f "s"
  | Auto -> Pp.string ctx "auto"
  | Durations durations ->
      Pp.list ~sep:Pp.comma (pp_duration_with ~shorten_ms) ctx durations
  | Round (strategy, value, step) ->
      Pp.call "round"
        (fun ctx (strategy, value, step) ->
          if strategy <> "nearest" then (
            Pp.string ctx strategy;
            Pp.comma ctx ());
          pp_duration_with ~shorten_ms ctx value;
          Pp.comma ctx ();
          pp_duration_with ~shorten_ms ctx step)
        ctx (strategy, value, step)
  | Rem (a, b) ->
      Pp.call "rem"
        (fun ctx (a, b) ->
          pp_duration_with ~shorten_ms ctx a;
          Pp.comma ctx ();
          pp_duration_with ~shorten_ms ctx b)
        ctx (a, b)
  | Mod (a, b) ->
      Pp.call "mod"
        (fun ctx (a, b) ->
          pp_duration_with ~shorten_ms ctx a;
          Pp.comma ctx ();
          pp_duration_with ~shorten_ms ctx b)
        ctx (a, b)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var (pp_duration_with ~shorten_ms) ctx v
  (* [normalize_duration] canonicalises a typed [<time>] but folds a [calc()]
     only when it computes one, so the operands of a [calc()] that reaches the
     output stay as authored - a [var()] fallback among them. Shortening one
     here would print two unequal declarations alike and lose them a
     factoring. *)
  | Calc c ->
      pp_calc_with ~unwrap_num:false (pp_duration_with ~shorten_ms:false) ctx c

let pp_duration : duration Pp.t = pp_duration_with ~shorten_ms:true
let pp_duration_preserve_ms : duration Pp.t = pp_duration_with ~shorten_ms:false

let rec pp_number : number Pp.t =
 fun ctx -> function
  | Num f when Float.is_nan f -> Pp.nan_value ctx ""
  | Num f ->
      Pp.string ctx (Pp.string_of_float ~drop_leading_zero:(Pp.minified ctx) f)
  | Var v -> pp_var pp_number ctx v
  | Calc c -> pp_calc_with pp_number ctx c
  | Round (strategy, value, step) ->
      Pp.call "round"
        (fun ctx (strategy, value, step) ->
          if strategy <> "nearest" then (
            Pp.string ctx strategy;
            Pp.comma ctx ());
          pp_number ctx value;
          Pp.comma ctx ();
          pp_number ctx step)
        ctx (strategy, value, step)
  | Mod (a, b) ->
      Pp.call "mod"
        (fun ctx (a, b) ->
          pp_number ctx a;
          Pp.comma ctx ();
          pp_number ctx b)
        ctx (a, b)
  | Rem (a, b) ->
      Pp.call "rem"
        (fun ctx (a, b) ->
          pp_number ctx a;
          Pp.comma ctx ();
          pp_number ctx b)
        ctx (a, b)
  | Hypot (a, b) ->
      Pp.call "hypot"
        (fun ctx (a, b) ->
          pp_number ctx a;
          Pp.comma ctx ();
          pp_number ctx b)
        ctx (a, b)
  | Pow (a, b) ->
      Pp.call "pow"
        (fun ctx (a, b) ->
          pp_number ctx a;
          Pp.comma ctx ();
          pp_number ctx b)
        ctx (a, b)
  | Sqrt v -> Pp.call "sqrt" pp_number ctx v
  | Abs v -> Pp.call "abs" pp_number ctx v
  | Sign v -> Pp.call "sign" pp_number ctx v
  | Sin a -> Pp.call "sin" pp_angle ctx a

let pp_transition_behavior : transition_behavior Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Allow_discrete -> Pp.string ctx "allow-discrete"

(* Print a raw float as a percentage value *)

(* Calc module for building calc() expressions *)
module Calc = struct
  let add left right = Expr (left, Add, right)
  let sub left right = Expr (left, Sub, right)
  let mul left right = Expr (left, Mul, right)
  let div left right = Expr (left, Div, right)

  (* Operators *)
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div

  (* Value constructors *)
  let length : length -> length calc = function
    | Var v -> Var v
    | len -> Val len

  let var : ?default:'a -> ?fallback:'a fallback -> string -> 'a calc =
   fun ?default ?fallback name -> Var (var_ref ?default ?fallback name)

  let float f : 'a calc = Num f
  let infinity : 'a calc = Num infinity
  let px n = Val (Px n : length)
  let rem f = Val (Rem f : length)
  let em f = Val (Em f : length)
  let pct f : length calc = Val (Pct f : length)

  (* Wrap an expression in an explicit nested calc() *)
  let nested inner = Nested inner

  (* Wrap an expression in parentheses only *)
  let parens inner = Parens inner
end

(** Read the body of a var(name[, fallback]) call, given a cursor over the
    function arguments. *)
let read_var_body : type a. (Cursor.t -> a) -> Cursor.t -> a var =
 fun read_value t ->
  Cursor.ws t;
  let name = Cursor.ident ~keep_case:true t in
  (* CSS Custom Properties 1: a [var()] reference must name a [<dashed-ident>]
     other than the bare reserved [--] keyword. *)
  if not (Custom_property_name.is_valid name) then
    Cursor.err_invalid t ("var() requires a custom-property name: " ^ name);
  let var_name = Custom_property_name.strip_prefix name in
  Cursor.ws t;
  let fallback : _ fallback =
    if not (Cursor.comma_opt t) then None
    else (
      Cursor.ws t;
      if Cursor.is_done t then Empty
      else
        match Cursor.try_parse_full_err read_value t with
        (* The fallback is a [<declaration-value>]: it swallows the rest of the
           argument list whether or not it parses as the target syntax, so it is
           consumed either way. *)
        | Ok fb -> Fallback fb
        | Error _ -> Syntax_fallback (Cursor.consume_remaining t))
  in
  var_ref ~fallback var_name

(** Generic [var(...)] parser. Consumes the [var(...)] [Func] and applies
    [read_value] to the fallback, if any. *)
let read_var : type a. (Cursor.t -> a) -> Cursor.t -> a var =
 fun read_value t ->
  Cursor.call "var" t (fun inner -> read_var_body read_value inner)

let read_body : type a. (Cursor.t -> a) -> Cursor.t -> a env =
 fun read_value t ->
  Cursor.ws t;
  let name = Cursor.ident ~keep_case:true t in
  let rec indices acc =
    Cursor.ws t;
    match Cursor.integer_opt t with
    | Some i -> indices (i :: acc)
    | Option.None -> List.rev acc
  in
  let indices = indices [] in
  Cursor.ws t;
  let fallback =
    if Cursor.comma_opt t then (
      Cursor.ws t;
      if Cursor.is_done t then Option.None else Some (read_value t))
    else Option.None
  in
  Cursor.ws t;
  Cursor.expect_eof t;
  ({ name; indices; fallback } : a env)

let read_env : type a. (Cursor.t -> a) -> Cursor.t -> a env =
 fun read_value t ->
  Cursor.call "env" t (fun inner -> read_body read_value inner)

(* CSS Values 4 sec. 5.3: a signed zero is the same value as unsigned zero.
   Collapse any zero to the canonical [0]: normalise [-0.] to [0.] (so the AST
   never carries a negative-zero float, which breaks structural equality) and
   regenerate the repr from that value rather than echo the authored sign, so
   [-0px] / [+0px] serialise as [0px]. *)
let normalize_signed_zero n repr =
  if n = 0.0 then (0.0, Pp.string_of_float 0.0) else (n, repr)

let read_length_unit ?(allow_negative = true) ?(length_only = false) t =
  let n, repr, unit_raw = Cursor.number_repr_with_unit t in
  let n, repr = normalize_signed_zero n repr in
  if (not allow_negative) && n < 0.0 then Cursor.err_invalid t "negative";
  let unit = String.lowercase_ascii (Option.value unit_raw ~default:"") in
  let authored unit =
    match unit_raw with
    | Some raw_unit when n = 0.0 ->
        Dimension { value = n; unit = raw_unit; repr }
    | _ when Pp.string_of_float n = repr -> length_of_unit unit n
    | _ ->
        Dimension { value = n; unit = Option.value unit_raw ~default:""; repr }
  in
  match unit with
  (* CSS Values 4 sec. 5.5: a [<percentage>] is its own type, so [0%] is [Pct
     0.] and keeps [%]; folding it to the length zero would change its type (and
     is unsound for definiteness-sensitive properties like [height]). *)
  | "%" when not length_only -> (Pct n : length)
  | "%" -> Cursor.err_invalid t "percentage where a length is required"
  | "" when n = 0.0 -> Zero
  | "" -> Cursor.err t "length values must have units (except for zero)"
  | unit -> (
      match unit_of_string unit with
      | Some unit -> authored unit
      | None ->
          (* Unknown / future unit (CSS Values 4 section 5.4 reserves dimension
             tokens for future units). Readers stay binary: raise here, and the
             declaration-level recovery in [Declaration.read_declaration]
             catches the [Parse_error] and emits a warning that strict mode
             escalates to [Error]. Inside [calc()] / [sign()] / [abs()] etc. the
             math-arg path constructs a [Dim (n, unit)] leaf via
             [Cursor.number_with_unit] without going through this reader, so
             [calc(1x + 2x)] still parses. *)
          Cursor.err_invalid t ("unknown dimension unit: " ^ unit))

let read_length_keyword t : length =
  Cursor.enum "length"
    [
      ("auto", (Auto : length));
      ("none", None);
      ("normal", Normal);
      ("size", Size);
      ("max-content", Max_content);
      ("min-content", Min_content);
      ("fit-content", Fit_content);
      (* Legacy vendor-prefixed intrinsic sizing keywords (kept as a fallback
         for old Safari / Firefox; the unprefixed forms above win in modern
         ones). *)
      ("-webkit-max-content", Webkit_max_content);
      ("-webkit-min-content", Webkit_min_content);
      ("-webkit-fit-content", Webkit_fit_content);
      ("-moz-max-content", Moz_max_content);
      ("-moz-min-content", Moz_min_content);
      ("-moz-fit-content", Moz_fit_content);
      ("contain", Contain);
      ("stretch", Stretch);
      ("from-font", From_font);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    t

let calc_factor_is_dimension : type a. a calc -> bool = function
  | Val _ -> true
  | _ -> false

let read_calc_zero : type a. Cursor.t -> a calc =
 fun t ->
  (* A zero in calc is a plain Number_tok with value 0, not a dimension. *)
  let snap = Cursor.save t in
  match Cursor.number_opt t with
  | Some 0. -> Num 0.
  | _ ->
      Cursor.restore t snap;
      Cursor.err t "expected zero"

let math_arg_of_numeric_calc : type a. Cursor.t -> a calc -> math_arg =
 fun t -> function
  | Math_fn fn -> Math_call fn
  | Num f -> Lit f
  | _ -> Cursor.err t "expected numeric math arg"

let read_math_constant_name t name =
  match String.lowercase_ascii name with
  | "pi" -> Const Pi
  | "e" -> Const E
  | "infinity" -> Const Infinity
  | "-infinity" -> Const Neg_infinity
  | "nan" -> Const Nan
  | _ -> Cursor.err t "expected math constant"

let read_math_number_with_unit t =
  (* CSS Values 4 sec. 10.6 [sign()] / [abs()] accept a [<calc-sum>] over any
     numeric type, including dimensions and percentages. Capture the leading
     number plus its unit so [sign(-1vw)] and [sign(1%)] preserve their source
     shape. *)
  match Cursor.number_with_unit t with
  | n, Some unit -> Dim (n, unit)
  | n, None -> Lit n

let read_math_constant_or_number t =
  match Cursor.ident_opt t with
  | Some name -> read_math_constant_name t name
  | None -> read_math_number_with_unit t

(* CSS Values 4 (ED) sec. 10.7: [round()] takes an optional strategy ([nearest],
   [up], [down], [to-zero]); when omitted the default is [nearest]. *)
let read_round_strategy inner =
  let snap = Cursor.save inner in
  match Cursor.peek_ident inner with
  | Some (("nearest" | "up" | "down" | "to-zero") as kw) ->
      Cursor.skip inner;
      Cursor.ws inner;
      Cursor.comma inner;
      kw
  | _ ->
      Cursor.restore inner snap;
      "nearest"

(* [round(<strategy>?, value, step)] over any argument reader. *)
let read_round_call make read_x t =
  Cursor.call "round" t (fun inner ->
      let strategy = read_round_strategy inner in
      let value = read_x inner in
      Cursor.ws inner;
      Cursor.comma inner;
      let step = read_x inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      make strategy value step)

(* [rem(a, b)] / [mod(a, b)] and the other two-argument math calls, over any
   argument reader. *)
let read_binary_call name make read_x t =
  Cursor.call name t (fun inner ->
      let a = read_x inner in
      Cursor.ws inner;
      Cursor.comma inner;
      let b = read_x inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      make a b)

let read_numeric_arg inner = Cursor.number inner

let read_numeric_round : type a. Cursor.t -> a calc =
 fun t -> Num (read_round_call round_to_step read_numeric_arg t)

let read_numeric_rem : type a. Cursor.t -> a calc =
 fun t -> Num (read_binary_call "rem" Float.rem read_numeric_arg t)

let read_var_calc_factor : type a. (Cursor.t -> a) -> Cursor.t -> a calc =
 fun read_a t ->
  if Cursor.looking_at_func "var" t then Var (read_var read_a t)
  else Cursor.err t "expected var"

let read_sibling_index_factor : type a. Cursor.t -> a calc =
 fun t ->
  if Cursor.looking_at_func "sibling-index" t then
    Cursor.call "sibling-index" t (fun inner ->
        Cursor.expect_eof inner;
        Sibling_index)
  else Cursor.err t "expected sibling-index"

let read_sibling_count_factor : type a. Cursor.t -> a calc =
 fun t ->
  if Cursor.looking_at_func "sibling-count" t then
    Cursor.call "sibling-count" t (fun inner ->
        Cursor.expect_eof inner;
        Sibling_count)
  else Cursor.err t "expected sibling-count"

let math_constant_factor_of_name : type a. Cursor.t -> _ -> string -> a calc =
 fun t snap name ->
  match String.lowercase_ascii name with
  | "pi" -> Math_const Pi
  | "e" -> Math_const E
  | "infinity" -> Math_const Infinity
  | "-infinity" -> Math_const Neg_infinity
  | "nan" -> Math_const Nan
  | _ ->
      Cursor.restore t snap;
      Cursor.err t "expected math constant"

let read_math_constant_factor : type a. Cursor.t -> a calc =
 fun t ->
  (* CSS Values 4 sec. 10.7 math constants ([pi], [e], [infinity], [-infinity],
     [NaN]) appear as bare identifiers inside [calc()]. *)
  let snap = Cursor.save t in
  match Cursor.ident_opt t with
  | Some name -> math_constant_factor_of_name t snap name
  | None ->
      Cursor.restore t snap;
      Cursor.err t "expected math constant"

(* CSS Values 4 (ED) sec. 10.8: "whitespace is required on both sides of the +
   and - operators. (The * and / operators can be used without white space
   around them.)" Browsers enforce it, so a one-sided sum is not a math function
   and the whole value is invalid. [ws_before] answers for the left side, which
   the caller must have measured with [skip_ws]: [peek_delim] drops the
   whitespace before it reports the delimiter. *)
let skip_sum_operator t ~ws_before =
  if not ws_before then Cursor.err t "expected whitespace before '+' or '-'";
  Cursor.skip t;
  if not (Cursor.skip_ws t) then
    Cursor.err t "expected whitespace after '+' or '-'"

let rec read_calc_expr : type a. (Cursor.t -> a) -> Cursor.t -> a calc =
 fun read_a t ->
  Cursor.ws t;
  (* CSS Values 4 sec. 10.1 evaluates same-precedence operators left-to-right,
     so [a - b - c] groups as [(a - b) - c]. Loop on subsequent operators rather
     than recursing on the right, which would group it as [a - (b - c)]. *)
  let rec loop left =
    let ws_before = Cursor.skip_ws t in
    match Cursor.peek_delim t with
    | Some '+' ->
        let right =
          Cursor.atomic t (fun () ->
              skip_sum_operator t ~ws_before;
              read_calc_term read_a t)
        in
        loop (Expr (left, Add, right))
    | Some '-' ->
        let right =
          Cursor.atomic t (fun () ->
              skip_sum_operator t ~ws_before;
              read_calc_term read_a t)
        in
        loop (Expr (left, Sub, right))
    | _ -> left
  in
  loop (read_calc_term read_a t)

and read_calc_term : type a. (Cursor.t -> a) -> Cursor.t -> a calc =
 fun read_a t ->
  Cursor.ws t;
  read_calc_term_tail read_a t (read_calc_factor read_a t)

and read_calc_term_tail : type a.
    (Cursor.t -> a) -> Cursor.t -> a calc -> a calc =
 fun read_a t left ->
  (* Put back the whitespace when the next operator is not [*] or [/]: it is the
     left-hand side [read_calc_expr] measures for a [+] or [-]. *)
  let snap = Cursor.save t in
  Cursor.ws t;
  match Cursor.peek_delim t with
  | Some '*' -> read_calc_product read_a t left
  | Some '/' -> read_calc_quotient read_a t left
  | _ ->
      Cursor.restore t snap;
      left

and read_calc_product : type a. (Cursor.t -> a) -> Cursor.t -> a calc -> a calc
    =
 fun read_a t left ->
  (* Use atomic to ensure we either parse the full multiplication or nothing. *)
  Cursor.atomic t (fun () ->
      Cursor.skip t;
      Cursor.ws t;
      let right = read_calc_factor read_a t in
      if calc_factor_is_dimension left && calc_factor_is_dimension right then
        Cursor.err t "invalid calc: cannot multiply two dimensions";
      read_calc_term_tail read_a t (Expr (left, Mul, right)))

and read_calc_quotient : type a. (Cursor.t -> a) -> Cursor.t -> a calc -> a calc
    =
 fun read_a t left ->
  (* Use atomic to ensure we either parse the full division or nothing. *)
  Cursor.atomic t (fun () ->
      Cursor.skip t;
      Cursor.ws t;
      let right = read_calc_factor read_a t in
      read_calc_term_tail read_a t (Expr (left, Div, right)))

and read_calc_parenthesized : type a. (Cursor.t -> a) -> Cursor.t -> a calc =
 fun read_a t ->
  Parens (Cursor.parens (fun inner -> read_calc_expr read_a inner) t)

and read_calc_numeric_function : type a. Cursor.t -> a calc =
 fun t ->
  match Cursor.peek t with
  | Some (Component.Func { node = { name; _ }; _ }) -> (
      match String.lowercase_ascii name with
      | "round" -> read_numeric_round t
      | "rem" -> read_numeric_rem t
      | "mod" -> read_numeric_binary_call "mod" mod_value t
      | "min" -> read_numeric_list_call "min" Float.min Float.infinity t
      | "max" -> read_numeric_list_call "max" Float.max Float.neg_infinity t
      | "clamp" -> read_numeric_clamp t
      (* CSS Values 4 (ED) sec. 9.1 numeric math functions: parsed into the
         typed [Math_fn] AST so pretty pp re-emits [name(args)]; the optimizer
         (or minify pp) folds via [eval_math_fn]. *)
      | "sqrt" -> Math_fn (Sqrt (read_math_call_arg "sqrt" t))
      | "abs" -> Math_fn (Abs_n (read_math_call_arg "abs" t))
      | "sign" -> Math_fn (Sign_n (read_math_call_arg "sign" t))
      | "exp" -> Math_fn (Exp (read_math_call_arg "exp" t))
      | "log" -> read_math_log t
      | "pow" -> read_math_binary "pow" (fun a b -> Pow (a, b)) t
      | "hypot" -> read_math_hypot t
      | "sin" -> Math_fn (Sin (read_angle_call_arg "sin" t))
      | "cos" -> Math_fn (Cos (read_angle_call_arg "cos" t))
      | "tan" -> Math_fn (Tan (read_angle_call_arg "tan" t))
      | "asin" -> Math_fn (Asin (read_math_call_arg "asin" t))
      | "acos" -> Math_fn (Acos (read_math_call_arg "acos" t))
      | "atan" -> Math_fn (Atan (read_math_call_arg "atan" t))
      | "atan2" -> read_math_binary "atan2" (fun a b -> Atan2 (a, b)) t
      | _ -> Cursor.err t "expected numeric calc function")
  | _ -> Cursor.err t "expected numeric calc function"

and read_math_arg t : math_arg = read_math_arg_term t

and read_math_arg_term t =
  let left = read_math_arg_factor t in
  let ws_before = Cursor.skip_ws t in
  match Cursor.peek_delim t with
  | Some ('+' as c) | Some ('-' as c) ->
      skip_sum_operator t ~ws_before;
      let op : calc_op = match c with '+' -> Add | _ -> Sub in
      Op (left, op, read_math_arg_term t)
  | _ -> left

and read_math_arg_factor t =
  let left = read_math_arg_unary t in
  let snap = Cursor.save t in
  Cursor.ws t;
  match Cursor.peek_delim t with
  | Some ('*' as c) | Some ('/' as c) ->
      Cursor.skip t;
      Cursor.ws t;
      let op : calc_op = match c with '*' -> Mul | _ -> Div in
      Op (left, op, read_math_arg_factor t)
  | _ ->
      Cursor.restore t snap;
      left

and read_math_arg_unary t =
  Cursor.ws t;
  match Cursor.peek_block t with
  | Some Token.Paren ->
      Parens_arg (Cursor.parens (fun inner -> read_math_arg inner) t)
  | _ -> read_math_arg_atom t

and read_math_arg_atom t =
  match Cursor.peek t with
  | Some (Component.Func { node = { name; _ }; _ }) ->
      read_math_arg_function t name
  | _ -> read_math_constant_or_number t

and read_math_arg_function t name =
  match String.lowercase_ascii name with
  | "var" ->
      Var_arg (read_var (fun inner -> read_math_arg inner) t : math_arg var)
  | _ -> math_arg_of_numeric_calc t (read_calc_numeric_function t)

and read_math_call_arg name t : math_arg =
  Cursor.call name t (fun inner ->
      Cursor.ws inner;
      let v = read_math_arg inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      v)

and read_math_binary : type a.
    string -> (math_arg -> math_arg -> math_fn) -> Cursor.t -> a calc =
 fun name mk t ->
  Math_fn
    (Cursor.call name t (fun inner ->
         Cursor.ws inner;
         let a = read_math_arg inner in
         Cursor.ws inner;
         Cursor.comma inner;
         Cursor.ws inner;
         let b = read_math_arg inner in
         Cursor.ws inner;
         Cursor.expect_eof inner;
         mk a b))

and read_math_log : type a. Cursor.t -> a calc =
 fun t ->
  Math_fn
    (Cursor.call "log" t (fun inner ->
         Cursor.ws inner;
         let a = read_math_arg inner in
         Cursor.ws inner;
         let b =
           if Cursor.comma_opt inner then (
             Cursor.ws inner;
             let b = read_math_arg inner in
             Some b)
           else None
         in
         Cursor.ws inner;
         Cursor.expect_eof inner;
         Log (a, b)))

and read_math_hypot : type a. Cursor.t -> a calc =
 fun t ->
  Math_fn
    (Cursor.call "hypot" t (fun inner ->
         Cursor.ws inner;
         let args =
           Cursor.list ~sep:Cursor.comma ~at_least:1 read_math_arg inner
         in
         Cursor.ws inner;
         Cursor.expect_eof inner;
         (Hypot args : math_fn)))

and read_angle_arg t : angle_arg = read_angle_arg_term t

and read_angle_arg_term t =
  let left = read_angle_arg_factor t in
  Cursor.ws t;
  match Cursor.peek_delim t with
  | Some ('+' as c) | Some ('-' as c) ->
      Cursor.skip t;
      Cursor.ws t;
      let op : calc_op = match c with '+' -> Add | _ -> Sub in
      Operation (left, op, read_angle_arg_term t)
  | _ -> left

and read_angle_arg_factor t =
  let left = read_angle_arg_unary t in
  Cursor.ws t;
  match Cursor.peek_delim t with
  | Some ('*' as c) | Some ('/' as c) ->
      Cursor.skip t;
      Cursor.ws t;
      let op : calc_op = match c with '*' -> Mul | _ -> Div in
      Operation (left, op, read_angle_arg_factor t)
  | _ -> left

and read_angle_arg_unary t =
  Cursor.ws t;
  match Cursor.peek_block t with
  | Some Token.Paren ->
      Grouped (Cursor.parens (fun inner -> read_angle_arg inner) t)
  | _ -> read_angle_unit_or_math t

and read_angle_unit_or_math t =
  let snap = Cursor.save t in
  match Cursor.number_with_unit t with
  | n, Some unit -> angle_arg_of_unit t snap n unit
  | _, None -> restore_and_read_math_angle t snap
  | exception Cursor.Parse_error _ -> restore_and_read_math_angle t snap

and angle_arg_of_unit t snap n unit =
  match String.lowercase_ascii unit with
  | "deg" -> Deg n
  | "rad" -> Rad n
  | "turn" -> Turn n
  | "grad" -> Grad n
  | _ -> restore_and_read_math_angle t snap

and restore_and_read_math_angle t snap =
  Cursor.restore t snap;
  Numeric_arg (read_math_arg t)

and read_angle_call_arg name t : angle_arg =
  Cursor.call name t (fun inner ->
      Cursor.ws inner;
      let arg = read_angle_arg inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      arg)

and read_numeric_binary_call : type a.
    string -> (float -> float -> float) -> Cursor.t -> a calc =
 fun name fn t ->
  Num
    (Cursor.call name t (fun inner ->
         let a = read_num_expr inner in
         Cursor.ws inner;
         Cursor.comma inner;
         let b = read_num_expr inner in
         Cursor.ws inner;
         Cursor.expect_eof inner;
         fn a b))

and read_numeric_list_call : type a.
    string -> (float -> float -> float) -> float -> Cursor.t -> a calc =
 fun name fn initial t ->
  Num
    (Cursor.call name t (fun inner ->
         let nums =
           Cursor.list ~sep:Cursor.comma ~at_least:1 read_num_expr inner
         in
         Cursor.ws inner;
         Cursor.expect_eof inner;
         List.fold_left fn initial nums))

and read_numeric_clamp : type a. Cursor.t -> a calc =
 fun t ->
  Num
    (Cursor.call "clamp" t (fun inner ->
         let min_value = read_num_expr inner in
         Cursor.ws inner;
         Cursor.comma inner;
         let value = read_num_expr inner in
         Cursor.ws inner;
         Cursor.comma inner;
         let max_value = read_num_expr inner in
         Cursor.ws inner;
         Cursor.expect_eof inner;
         Float.max min_value (Float.min value max_value)))

(* Read a number-typed calc expression and fold it to a float. Used as the
   argument reader for [round] / [mod] / [rem] / [min] / [max] / [clamp] which
   still flatten numeric args to [Num] under the existing typed-fold path. *)
and read_num_expr t : float =
  let no_val t = Cursor.err t "expected number" in
  let calc = read_calc_expr no_val t in
  match eval_numeric_calc calc with
  | Some n -> n
  | None -> Cursor.err_invalid t "non-numeric calc expression"

and read_calc_factor : type a. (Cursor.t -> a) -> Cursor.t -> a calc =
 fun read_a t ->
  Cursor.ws t;
  match Cursor.peek_block t with
  | Some Token.Paren -> read_calc_parenthesized read_a t
  | _ -> read_calc_non_paren_factor read_a t

and read_calc_non_paren_factor : type a. (Cursor.t -> a) -> Cursor.t -> a calc =
 fun read_a t ->
  Cursor.one_of
    [
      read_nested_calc_factor read_a;
      read_var_calc_factor read_a;
      read_sibling_index_factor;
      read_sibling_count_factor;
      read_calc_zero;
      read_calc_numeric_function;
      (* A plain unitless [<number>] in a calc is the [<number>] type, not the
         surrounding property's value, so read it as a [Num] leaf before the
         typed reader. Otherwise a property that accepts a bare number (e.g.
         line-height, opacity) reads it as a [Val], which the multiplication
         dimension check then mistakes for a dimension and rejects [2 * 3]. A
         dimension ([2px]) or percentage token is not a [Number_tok], so it
         still falls through to the typed [Val] reader. *)
      (fun t -> (Num (Cursor.number t) : a calc));
      (fun t -> Val (read_a t));
      read_math_constant_factor;
    ]
    t

and read_nested_calc_factor : type a. (Cursor.t -> a) -> Cursor.t -> a calc =
 fun read_a t ->
  if Cursor.looking_at_func "calc" t then
    Nested (Cursor.call "calc" t (fun inner -> read_calc_expr read_a inner))
  else if Cursor.looking_at_func "-webkit-calc" t then
    Nested
      (Cursor.call "-webkit-calc" t (fun inner -> read_calc_expr read_a inner))
  else Cursor.err t "expected nested calc"

type inferred_calc_type =
  | Dimension of int
    (* Exponent 0 is a number, 1 is the contextual numeric value, and
       multiplication/division add/subtract exponents. This retains valid
       cancellation such as [1 / (1 / 50px)]. *)
  | Deferred
  | Invalid

let rec infer_calc_type : type a. a calc -> inferred_calc_type = function
  | Num _ | Math_const _ | Sibling_index | Sibling_count -> Dimension 0
  | Val _ -> Dimension 1
  (* A var() is substituted before a math function is type-checked. The generic
     math-function AST does not retain enough result-type information to prove
     its type here either, so both stay deferred rather than being guessed. *)
  | Var _ | Math_fn _ -> Deferred
  | Nested inner | Parens inner -> infer_calc_type inner
  | Expr (left, (Add | Sub), right) -> (
      match (infer_calc_type left, infer_calc_type right) with
      | Invalid, _ | _, Invalid -> Invalid
      | Deferred, _ | _, Deferred -> Deferred
      | Dimension l, Dimension r -> if l = r then Dimension l else Invalid)
  | Expr (left, Mul, right) -> (
      match (infer_calc_type left, infer_calc_type right) with
      | Invalid, _ | _, Invalid -> Invalid
      | Deferred, _ | _, Deferred -> Deferred
      | Dimension l, Dimension r -> Dimension (l + r))
  | Expr (left, Div, right) -> (
      match (infer_calc_type left, infer_calc_type right) with
      | Invalid, _ | _, Invalid -> Invalid
      | Deferred, _ | _, Deferred -> Deferred
      | Dimension l, Dimension r -> Dimension (l - r))

let validate_calc_type t result_type calc =
  let inferred = infer_calc_type calc in
  let accepted =
    match (result_type, inferred) with
    | _, Deferred -> true
    | `Number, Dimension 0 | `Value, Dimension 1 -> true
    | `Number_or_value, (Dimension 0 | Dimension 1) -> true
    | (`Number | `Value | `Number_or_value), (Invalid | Dimension _) -> false
  in
  if not accepted then Cursor.err_invalid t "incompatible calc types"

let read_calc : type a.
    ?result_type:[ `Number | `Number_or_value | `Value ] ->
    (Cursor.t -> a) ->
    Cursor.t ->
    a calc =
 fun ?result_type read_a t ->
  Cursor.ws t;
  let read name =
    Cursor.call name t (fun inner ->
        let result = read_calc_expr read_a inner in
        (* CSS Values 4 10: a [calc()] body must be a single expression --
           [calc(1px 2px)] (missing operator) leaves [2px] unconsumed and must
           be rejected. *)
        Cursor.ws inner;
        Cursor.expect_eof inner;
        Option.iter
          (fun result_type -> validate_calc_type inner result_type result)
          result_type;
        result)
  in
  if Cursor.looking_at_func "calc" t then read "calc"
  else if Cursor.looking_at_func "-webkit-calc" t then read "-webkit-calc"
  else if Cursor.looking_at_func "var" t then Var (read_var read_a t)
  else Cursor.err t "calc() or var()"

(* CSS Values 4 (ED) sec. 10.9: a math function resolving to [<number>] stands
   wherever an [<integer>] is accepted. A constant expression folds to its
   integer; one holding a [var()] does not resolve here and stays a calc
   node. *)
let read_integer_calc : type a.
    string -> Cursor.t -> [ `Int of int | `Calc of a calc ] =
 fun name t ->
  let expr =
    read_calc ~result_type:`Number
      (fun _ ->
        Cursor.err t
          (String.concat "" [ "unexpected value in "; name; " calc" ]))
      t
  in
  match eval_numeric_calc expr with
  | Some f when Float.is_integer f -> `Int (int_of_float f)
  | Some _ ->
      Cursor.err_invalid t
        (String.concat "" [ name; " calc must evaluate to integer" ])
  | None -> `Calc expr

let read_integer name t =
  if Cursor.looking_at_calc t then
    match
      (read_integer_calc name t : [ `Int of int | `Calc of number calc ])
    with
    | `Int n -> n
    | `Calc _ ->
        Cursor.err_invalid t
          (String.concat "" [ name; " calc must evaluate to integer" ])
  else Cursor.int t

let read_numeric_expression t = read_num_expr t

(* The typed [clamp / minmax / min / max] length readers are part of the
   [read_length] mutual-recursion group (see below); the function-call
   dispatcher in [length_function_readers] references them. *)

let read_anchor_size_length inner =
  let size = Cursor.consume_remaining_as_string ~trim:true inner in
  if size = "" then Cursor.err_expected inner "anchor-size argument";
  Anchor_size size

let read_anchor_name_side inner first =
  if Custom_property_name.is_valid first then
    let side = Cursor.ident inner in
    (Some first, side)
  else (None, first)

let read_attr_length_type inner =
  Cursor.ws inner;
  let body = Cursor.consume_remaining_as_string ~trim:true inner in
  match attr_syntax_of_string body with
  | Some syntax -> Type syntax
  | None -> Cursor.err_invalid inner ("attr() type: " ^ body)

let read_plain_attr_type_hint inner : attr_type option =
  match Cursor.ident_opt inner with
  | Some "raw-string" -> Option.Some Raw_string
  | Some "number" -> Option.Some Number_type
  | Some unit_ -> Option.Some (Unit unit_)
  | None when Cursor.peek_delim inner = Some '%' ->
      Cursor.expect '%' inner;
      Option.Some (Unit "%")
  | None -> Cursor.err_expected inner "attr() type"

let read_attr_type_hint inner : attr_type option =
  Cursor.ws inner;
  if Cursor.is_done inner || Cursor.peek_comma inner then Option.None
  else if Cursor.looking_at_func "type" inner then
    Option.Some (Cursor.call "type" inner read_attr_length_type)
  else read_plain_attr_type_hint inner

let rec read_length ?(allow_negative = true) ?(with_keywords = true)
    ?(length_only = false) t : length =
  Cursor.ws t;
  let parsers =
    [
      read_var_length ~allow_negative ~length_only ~with_keywords;
      read_calc_length ~length_only ~with_keywords;
      read_env_length ~allow_negative ~length_only ~with_keywords;
      read_function_length ~allow_negative ~length_only ~with_keywords;
      read_length_unit ~allow_negative ~length_only;
    ]
  in
  let parsers =
    if with_keywords then read_length_keyword :: parsers else parsers
  in
  Cursor.one_of parsers t

and read_var_length ~allow_negative ~length_only ~with_keywords t : length =
  if Cursor.looking_at t "var(" then
    Var (read_var (read_length ~allow_negative ~length_only ~with_keywords) t)
  else Cursor.err t "expected var"

and read_calc_length ~length_only ~with_keywords t : length =
  if Cursor.looking_at_calc t then
    (* Same exception as [read_length_percentage]: inside [calc()] the
       non-negative constraint applies to the resolved value. *)
    Calc
      (read_calc ~result_type:`Value
         (read_length ~length_only ~with_keywords)
         t)
  else Cursor.err t "expected calc"

and read_env_length ~allow_negative ~length_only ~with_keywords t : length =
  if Cursor.looking_at t "env(" then
    Env (read_env (read_length ~allow_negative ~length_only ~with_keywords) t)
  else Cursor.err t "expected env"

and read_function_length ~allow_negative ~length_only ~with_keywords t : length
    =
  match
    Cursor.any_function_call
      (read_length_function_body ~allow_negative ~length_only ~with_keywords t)
      t
  with
  | Some length -> length
  | None -> Cursor.err_expected t "function call"

and read_length_function_body ~allow_negative ~length_only ~with_keywords t name
    inner =
  let name = String.lowercase_ascii name in
  if
    length_only
    && List.mem name
         [
           "minmax"; "fit-content"; "calc-size"; "anchor"; "anchor-size"; "sign";
         ]
  then Cursor.err_invalid t "expected a length-valued math function";
  let allow_negative = allow_negative || (length_only && name <> "attr") in
  match
    List.assoc_opt name
      (length_function_readers ~allow_negative ~length_only ~with_keywords)
  with
  | Some read -> read inner
  | None -> Cursor.err t ("unknown function " ^ name)

and length_function_readers ~allow_negative ~length_only ~with_keywords =
  [
    ("clamp", read_clamp_length ~length_only);
    ("minmax", read_minmax_length ~length_only);
    ("min", read_min_length ~length_only);
    ("max", read_max_length ~length_only);
    ( "fit-content",
      read_fit_content_length ~allow_negative ~length_only ~with_keywords );
    ("round", read_round_length ~allow_negative ~length_only ~with_keywords);
    ("mod", read_mod_length ~allow_negative ~length_only ~with_keywords);
    ("rem", read_rem_length ~allow_negative ~length_only ~with_keywords);
    ("hypot", read_hypot_length ~allow_negative ~length_only ~with_keywords);
    ("abs", read_abs_length ~allow_negative ~length_only ~with_keywords);
    ("sign", read_sign_length ~allow_negative ~length_only ~with_keywords);
    ( "calc-size",
      read_calc_size_length ~allow_negative ~length_only ~with_keywords );
    ("anchor-size", read_anchor_size_length);
    ("anchor", read_anchor_length ~allow_negative ~length_only ~with_keywords);
    ("attr", read_attr_length ~allow_negative ~length_only ~with_keywords);
  ]

(* CSS Values 4 sec. 10.2: arguments to [clamp()], [min()], [max()], [minmax()]
   are implicit math expressions, so [clamp(.5rem, 2vw + .5rem, 2rem)] is valid
   without a surrounding [calc()]. Parse each argument with [read_calc_expr] and
   collapse a singleton [Val] back to the plain length so the AST stays compact
   for the common case. *)
and read_implicit_calc_length ~length_only inner =
  let expr =
    read_calc_expr
      (read_length ~length_only ~with_keywords:(not length_only))
      inner
  in
  if length_only then validate_calc_type inner `Value expr;
  match expr with Val l -> l | expr -> Calc expr

and read_clamp_length ?(length_only = false) inner =
  Cursor.ws inner;
  let min = read_implicit_calc_length ~length_only inner in
  Cursor.ws inner;
  Cursor.comma inner;
  Cursor.ws inner;
  let value = read_implicit_calc_length ~length_only inner in
  Cursor.ws inner;
  Cursor.comma inner;
  Cursor.ws inner;
  let max = read_implicit_calc_length ~length_only inner in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  Clamp (min, value, max)

and read_minmax_length ?(length_only = false) inner =
  Cursor.ws inner;
  let min = read_implicit_calc_length ~length_only inner in
  Cursor.ws inner;
  Cursor.comma inner;
  Cursor.ws inner;
  let max = read_implicit_calc_length ~length_only inner in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  Minmax (min, max)

and read_min_length ?(length_only = false) inner =
  let xs =
    Cursor.list ~at_least:1 ~sep:Cursor.comma
      (fun t ->
        Cursor.ws t;
        read_implicit_calc_length ~length_only t)
      inner
  in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  Min xs

and read_max_length ?(length_only = false) inner =
  let xs =
    Cursor.list ~at_least:1 ~sep:Cursor.comma
      (fun t ->
        Cursor.ws t;
        read_implicit_calc_length ~length_only t)
      inner
  in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  Max xs

and read_fit_content_length ~allow_negative ~length_only ~with_keywords inner =
  Cursor.ws inner;
  let arg = read_length ~allow_negative ~length_only ~with_keywords inner in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  Fit_content_arg arg

and read_round_length ~allow_negative ~length_only ~with_keywords inner =
  let strategy = read_round_strategy inner in
  let value = read_length ~allow_negative ~length_only ~with_keywords inner in
  Cursor.ws inner;
  Cursor.comma inner;
  let step = read_length ~allow_negative ~length_only ~with_keywords inner in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  Round (strategy, value, step)

and read_binary_length ~allow_negative ~length_only ~with_keywords make inner =
  let a = read_length ~allow_negative ~length_only ~with_keywords inner in
  Cursor.ws inner;
  Cursor.comma inner;
  let b = read_length ~allow_negative ~length_only ~with_keywords inner in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  make a b

and read_mod_length ~allow_negative ~length_only ~with_keywords inner =
  read_binary_length ~allow_negative ~length_only ~with_keywords
    (fun (a : length) (b : length) -> (Mod (a, b) : length))
    inner

and read_rem_length ~allow_negative ~length_only ~with_keywords inner =
  read_binary_length ~allow_negative ~length_only ~with_keywords
    (fun (a : length) (b : length) -> (Rem_fn (a, b) : length))
    inner

and read_hypot_length ~allow_negative ~length_only ~with_keywords inner =
  let values =
    Cursor.list ~sep:Cursor.comma
      (read_length ~allow_negative ~length_only ~with_keywords)
      inner
  in
  Cursor.expect_eof inner;
  Hypot values

and read_unary_length ~allow_negative ~length_only ~with_keywords make inner =
  let value = read_length ~allow_negative ~length_only ~with_keywords inner in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  make value

and read_abs_length ~allow_negative ~length_only ~with_keywords inner =
  read_unary_length ~allow_negative ~length_only ~with_keywords
    (fun (value : length) -> (Abs value : length))
    inner

and read_sign_length ~allow_negative ~length_only ~with_keywords inner =
  read_unary_length ~allow_negative ~length_only ~with_keywords
    (fun (value : length) -> (Sign value : length))
    inner

and read_calc_size_length ~allow_negative ~length_only ~with_keywords inner =
  let basis = read_length ~allow_negative ~length_only ~with_keywords inner in
  Cursor.ws inner;
  Cursor.comma inner;
  let calc =
    read_calc_expr
      (read_length ~allow_negative ~length_only ~with_keywords)
      inner
  in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  Calc_size (basis, calc)

and read_anchor_length ~allow_negative ~length_only ~with_keywords inner =
  let first = Cursor.ident inner in
  Cursor.ws inner;
  let name, side = read_anchor_name_side inner first in
  Cursor.ws inner;
  let fallback =
    if Cursor.comma_opt inner then (
      Cursor.ws inner;
      Some (read_length ~allow_negative ~length_only ~with_keywords inner))
    else None
  in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  Anchor (name, side, fallback)

and read_attr_length ~allow_negative ~length_only ~with_keywords inner =
  Cursor.ws inner;
  let name = Cursor.ident ~keep_case:true inner in
  let type_ = read_attr_type_hint inner in
  Cursor.ws inner;
  let fallback =
    read_attr_length_fallback ~allow_negative ~length_only ~with_keywords inner
  in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  Attr { name; type_; fallback }

and read_attr_length_fallback ~allow_negative ~length_only ~with_keywords inner
    =
  if Cursor.comma_opt inner then (
    Cursor.ws inner;
    if Cursor.is_done inner then Empty_fallback
    else
      Attr_fallback
        (read_length ~allow_negative ~length_only ~with_keywords inner))
  else No_fallback

(** Read a non-negative length value (for padding properties) *)
let read_non_negative_length ?(with_keywords = true) t : length =
  read_length ~allow_negative:false ~with_keywords t

(** Read a percentage value as float (number followed by %) Used for color
    components where 0-100% clamping is required *)
let read_percentage_float t : float =
  if Cursor.looking_at t "none" then (
    Cursor.expect_string "none" t;
    0.)
  else
    let n, unit = Cursor.number_with_unit t in
    match unit with
    | Some "%" | None -> max 0. (min 100. n)
    | Some unit -> Cursor.err_invalid t ("percentage unit: " ^ unit)

(** Read an alpha value *)
let rec read_alpha t : alpha =
  Cursor.ws t;
  let read_var_alpha t : alpha = Var (read_var read_alpha t) in
  let read_calc_alpha t : alpha =
    Calc (read_calc ~result_type:`Number_or_value read_alpha t)
  in
  let read_none t : alpha =
    Cursor.expect_string "none" t;
    None
  in
  let read_pct t : alpha =
    (* Alpha percentages are clamped to 0-100 per CSS spec *)
    Pct (Cursor.pct ~clamp:true t)
  in
  let read_num t : alpha =
    let n, _repr, unit = Cursor.number_repr_with_unit t in
    (match unit with
    | None -> ()
    | Some unit -> Cursor.err_invalid t ("alpha unit: " ^ unit));
    (* Clamp numeric alpha to 0-1 range per CSS spec *)
    Num (max 0. (min 1. n))
  in
  Cursor.one_of
    [ read_var_alpha; read_calc_alpha; read_none; read_pct; read_num ]
    t

(** Read optional alpha component *)
let read_optional_alpha t : alpha =
  Cursor.ws t;
  if Cursor.peek_delim t = Some '/' then (
    Cursor.slash t;
    read_alpha t)
  else None

(** Read a channel value (RGB) *)
let rec read_channel t : channel =
  Cursor.ws t;
  (* Check for var() *)
  if Cursor.looking_at t "var(" then Var (read_var read_channel t)
  else if Cursor.looking_at t "none" then (
    Cursor.expect_string "none" t;
    (None : channel))
  else if
    match Cursor.peek t with
    | Some (Component.Func { node = { name; _ }; _ })
      when String.lowercase_ascii name <> "var" ->
        true
    | _ -> false
  then Int (int_of_float (max 0. (min 255. (read_num_expr t))))
  else
    let n, unit = Cursor.number_with_unit t in
    match unit with
    | Some "%" ->
        (* Clamp percentage to 0-100 per CSS spec *)
        Pct (max 0. (min 100. n))
    | None ->
        (* CSS Color 4 sec. 5.1 rounds a channel that cannot be kept at full
           precision "towards +infinity". Sec. 16.5's worked example reads that
           as nearest-with-ties-up, not as a ceiling: 0.964 serialises as 0.96
           and 0.787 as 0.79. [Float.round] is exactly that for a non-negative
           channel, so [rgb(127.6 ...)] is [128] and [rgb(127.2 ...)] is [127],
           matching Chrome. Decimals in (0, 1) stay as [Num] so alpha helpers
           can still see them as fractional. *)
        if n <= 1.0 && n <> floor n then Num n
        else Int (int_of_float (max 0. (min 255. (Float.round n))))
    | Some _ -> Cursor.err_invalid t "channel value"

let rec read_rgb_var t : rgb var =
  Cursor.ws t;
  read_var read_rgb t

and read_rgb t : rgb =
  Cursor.ws t;
  (* Try to parse as three channels first (any could be a variable) *)
  Cursor.one_of
    [
      (fun t ->
        let r, g, b = Cursor.triple read_channel read_channel read_channel t in
        Channels { r; g; b });
      (* Fall back to a single var representing all channels *)
      (fun t -> Var (read_rgb_var t));
    ]
    t

let read_rgb_space_separated t : color =
  (* The cursor wraps the [rgb(...)] [Func] arguments, so there is no closing
     [)] to consume -- it's the block boundary. *)
  Cursor.ws t;
  if Cursor.looking_at t "var(" then (
    let snap = Cursor.save t in
    let rgb_var = read_rgb_var t in
    Cursor.ws t;
    if Cursor.is_done t then Rgb (Var rgb_var)
    else if Cursor.peek_delim t = Some '/' then (
      let alpha = read_optional_alpha t in
      Cursor.ws t;
      match alpha with
      | None -> Cursor.err t "expected alpha value after '/'"
      | _ -> Rgba { rgb = Var rgb_var; a = alpha })
    else (
      Cursor.restore t snap;
      let r = read_channel t in
      Cursor.ws t;
      let g = read_channel t in
      Cursor.ws t;
      let b = read_channel t in
      let alpha = read_optional_alpha t in
      Cursor.ws t;
      if not (Cursor.is_done t) then
        Cursor.err t "unexpected tokens after rgb()";
      match alpha with
      | None -> Rgb (Channels { r; g; b })
      | Num _ | Pct _ | Var _ | Calc _ ->
          Rgba { rgb = Channels { r; g; b }; a = alpha }))
  else
    let r = read_channel t in
    Cursor.ws t;
    let g = read_channel t in
    Cursor.ws t;
    let b = read_channel t in
    let alpha = read_optional_alpha t in
    Cursor.ws t;
    if not (Cursor.is_done t) then Cursor.err t "unexpected tokens after rgb()";
    match alpha with
    | None -> Rgb (Channels { r; g; b })
    | Num _ | Pct _ | Var _ | Calc _ ->
        Rgba { rgb = Channels { r; g; b }; a = alpha }

let read_rgb_comma_separated t : color =
  let r, g, b =
    Cursor.triple ~sep:Cursor.comma read_channel read_channel read_channel t
  in
  (* CSS4 allows mixing percentages and numbers in RGB functions. This is a
     change from CSS3 which required all values to be the same type. Since we
     target CSS4 (supported by all major browsers), we allow mixing. *)
  let alpha = if Cursor.comma_opt t then read_alpha t else None in
  Cursor.ws t;
  Cursor.expect_eof t;
  match alpha with
  | None -> Rgb (Channels { r; g; b })
  | a -> Rgba { rgb = Channels { r; g; b }; a }

(** Read color space identifier *)
let read_color_space t : color_space =
  let space_ident = Cursor.ident t in
  match space_ident with
  | "srgb" -> Srgb
  | "srgb-linear" -> Srgb_linear
  | "display-p3" -> Display_p3
  | "a98-rgb" -> A98_rgb
  | "prophoto-rgb" -> Prophoto_rgb
  | "rec2020" -> Rec2020
  | "lab" -> Lab
  | "oklab" -> Oklab
  | "xyz" -> Xyz
  | "xyz-d50" -> Xyz_d50
  | "xyz-d65" -> Xyz_d65
  | "lch" -> Lch
  | "oklch" -> Oklch
  | "hsl" -> Hsl
  | "hwb" -> Hwb
  | _ -> Cursor.err_invalid t ("color space: " ^ space_ident)

let rec read_color_component t : component =
  Cursor.ws t;
  if Cursor.looking_at t "none" then (
    Cursor.expect_string "none" t;
    Component_none)
  else if Cursor.looking_at t "var(" then Var (read_var read_color_component t)
  else if Cursor.looking_at_calc t then
    Calc (read_calc ~result_type:`Number_or_value read_color_component t)
  else
    let n, unit = Cursor.number_with_unit t in
    match unit with
    | Some "%" -> Pct n
    | Some u -> Cursor.err_invalid t ("unit: " ^ u)
    | None -> Num n

(** Read color components until the alpha separator ['/'] or end of input. *)
let rec read_color_components space t acc =
  Cursor.ws t;
  if Cursor.is_done t || Cursor.peek_delim t = Some '/' then List.rev acc
  else
    let component_count = List.length acc in
    let component = read_color_component t in
    (match ((space : color_space), component_count, component) with
    | (Lab | Oklab | Lch | Oklch), 0, (Pct _ | Var _ | Calc _ | Component_none)
      ->
        ()
    | (Lab | Oklab | Lch | Oklch), 0, _ ->
        Cursor.err_invalid t "L component must be percentage"
    | _ -> ());
    read_color_components space t (component :: acc)

(** Read hex color digits. The tokenizer represents [#deadbeef] as a single
    [Hash] token with the digits as value. Kept for parity with callers outside
    this module. *)
let _read_hex_color t =
  let hex =
    match Cursor.hash_opt t with
    | Some s -> s
    | None -> Cursor.err_invalid t "expected hex color"
  in
  let len = String.length hex in
  if len = 0 then Cursor.err_invalid t "empty hex color"
  else if len = 3 || len = 4 || len = 6 || len = 8 then hex
  else Cursor.err_invalid t ("invalid hex color length: " ^ string_of_int len)

(** Argument category for [atan2(y, x)]: both arguments must share a category
    for the result to be unit-free. The float is the value already reduced into
    the category's canonical base ([px] for length, [ms] for time, [Hz] for
    frequency, [deg] for angle, raw fraction for percent). *)
type atan2_category = Number | Percentage | Length | Angle | Time | Frequency

let read_atan2_scalar t : atan2_category * float =
  let n, unit_raw = Cursor.number_with_unit t in
  let unit = String.lowercase_ascii (Option.value ~default:"" unit_raw) in
  match unit with
  | "" -> (Number, n)
  | "%" -> (Percentage, n)
  (* CSS Values 4 sec. 6.2 absolute lengths converted to [px]. *)
  | "px" -> (Length, n)
  | "cm" -> (Length, n *. (96. /. 2.54))
  | "mm" -> (Length, n *. (96. /. 25.4))
  | "q" -> (Length, n *. (96. /. 101.6))
  | "in" -> (Length, n *. 96.)
  | "pt" -> (Length, n *. (96. /. 72.))
  | "pc" -> (Length, n *. 16.)
  (* Relative lengths share the [<length>] type but cannot reduce to a constant
     scalar at parse time; group them so [atan2(1vw, -1vw)] succeeds via
     raw-value ratio while [atan2(1vw, 1px)] is rejected. *)
  | "em" | "rem" | "ex" | "cap" | "ch" | "ic" | "lh" | "rlh" | "vw" | "vh"
  | "vmin" | "vmax" | "vi" | "vb" | "dvh" | "dvw" | "dvmin" | "dvmax" | "lvh"
  | "lvw" | "lvmin" | "lvmax" | "svh" | "svw" | "svmin" | "svmax" | "cqw"
  | "cqh" | "cqi" | "cqb" | "cqmin" | "cqmax" ->
      (Length, n)
  (* Angles, times, frequencies in their canonical units. *)
  | "deg" -> (Angle, n)
  | "rad" -> (Angle, n *. 180. /. Float.pi)
  | "turn" -> (Angle, n *. 360.)
  | "grad" -> (Angle, n *. 0.9)
  | "ms" -> (Time, n)
  | "s" -> (Time, n *. 1000.)
  | "hz" -> (Frequency, n)
  | "khz" -> (Frequency, n *. 1000.)
  | _ -> Cursor.err_invalid t ("invalid atan2 argument unit: " ^ unit)

(** Read an angle value *)
let read_angle_round read_angle t =
  read_round_call (fun s v step -> (Round (s, v, step) : angle)) read_angle t

let angle_trig_function t =
  if Cursor.looking_at_func "asin" t then Some (`Asin, "asin")
  else if Cursor.looking_at_func "acos" t then Some (`Acos, "acos")
  else if Cursor.looking_at_func "atan" t then Some (`Atan, "atan")
  else None

let read_angle_trig kind name t =
  let typed t =
    let snapshot = Cursor.save t in
    let degrees =
      Cursor.call name t (fun inner ->
          let v = read_num_expr inner in
          Cursor.ws inner;
          Cursor.expect_eof inner;
          let radians =
            match kind with
            | `Asin -> Float.asin v
            | `Acos -> Float.acos v
            | `Atan -> Float.atan v
          in
          radians *. 180. /. Float.pi)
    in
    if Float.is_nan degrees then (
      (* CSS Values 4 (ED) sec. 10.9.2: NaN lives inside a calculation tree and
         nowhere else, so it has no leaf spelling. Re-read the call and keep the
         function the author wrote. *)
      Cursor.restore t snapshot;
      let arg = read_math_call_arg name t in
      let fn : math_fn =
        match kind with
        | `Asin -> Asin arg
        | `Acos -> Acos arg
        | `Atan -> Atan arg
      in
      (Calc (Math_fn fn) : angle))
    else (Deg (round_computed degrees) : angle)
  in
  match Cursor.try_typed_call typed t with
  | Ok value -> value
  | Error comp -> Invalid [ comp ]

let read_angle_atan2 t =
  let typed t =
    Cursor.call "atan2" t (fun inner ->
        (* CSS Values 4 sec. 10.4: atan2(y, x) accepts <number>|<dimension>|
           <percentage> for both arguments (must match types). When both
           arguments reduce to a scalar in a shared category, the ratio is
           unit-free and the result folds to a [Deg] constant. *)
        let y = read_atan2_scalar inner in
        Cursor.ws inner;
        Cursor.comma inner;
        Cursor.ws inner;
        let x = read_atan2_scalar inner in
        Cursor.ws inner;
        Cursor.expect_eof inner;
        match (y, x) with
        | (yk, yv), (xk, xv) when yk = xk ->
            (Deg (round_computed (Float.atan2 yv xv *. 180. /. Float.pi))
              : angle)
        | _ -> Cursor.err_invalid inner "atan2 arguments have mismatched types")
  in
  match Cursor.try_typed_call typed t with
  | Ok value -> value
  | Error comp -> Invalid [ comp ]

let read_angle_unit ~unitless_zero t =
  let n, unit_raw = Cursor.number_with_unit t in
  let unit_raw = Option.value unit_raw ~default:"" in
  match String.lowercase_ascii unit_raw with
  | "deg" -> Deg n
  | "rad" -> Rad n
  | "turn" -> Turn n
  | "grad" -> Grad n
  | "" when unitless_zero && n = 0. -> Deg 0.
  | "" ->
      Cursor.err_invalid t
        "angle values must have units (deg, rad, turn, or grad)"
  | unit -> Cursor.err_invalid t ("invalid angle unit: " ^ unit)

let rec read_angle_with ~unitless_zero t : angle =
  Cursor.ws t;
  if Cursor.looking_at t "var(" then
    Var (read_var (read_angle_with ~unitless_zero) t)
  else if Cursor.looking_at_calc t then
    Calc (read_calc ~result_type:`Value (read_angle_with ~unitless_zero) t)
  else if Cursor.looking_at_func "round" t then
    read_angle_round (read_angle_with ~unitless_zero) t
  else if Cursor.looking_at_func "rem" t then
    read_binary_call "rem"
      (fun a b -> (Rem (a, b) : angle))
      (read_angle_with ~unitless_zero)
      t
  else if Cursor.looking_at_func "mod" t then
    read_binary_call "mod"
      (fun a b -> (Mod (a, b) : angle))
      (read_angle_with ~unitless_zero)
      t
  else
    match angle_trig_function t with
    | Some (kind, name) -> read_angle_trig kind name t
    | None ->
        if Cursor.looking_at_func "atan2" t then read_angle_atan2 t
        else read_angle_unit ~unitless_zero t

let read_angle t = read_angle_with ~unitless_zero:true t
let read_angle_unit_required t = read_angle_with ~unitless_zero:false t

(** Normalize hue value to 0-360 range *)
let normalize_hue (degrees : float) : float =
  let normalized = mod_float degrees 360.0 in
  if normalized < 0.0 then normalized +. 360.0 else normalized

(** Read a hue value (preserves unitless vs explicit angle) *)
let rec read_hue t : hue =
  Cursor.ws t;
  (* Check for var() *)
  if Cursor.looking_at t "none" then (
    Cursor.expect_string "none" t;
    Hue_none)
  else if Cursor.looking_at t "var(" then Var (read_var read_hue t)
  else
    let n, unit_raw = Cursor.number_with_unit t in
    let unit = String.lowercase_ascii (Option.value unit_raw ~default:"") in
    match unit with
    | "" ->
        Unitless (normalize_hue n) (* Unitless number, defaults to degrees *)
    | "deg" -> Angle (Deg (normalize_hue n))
    | "rad" -> Angle (Rad n)
    | "turn" -> Angle (Turn n)
    | "grad" -> Angle (Grad n)
    | _ -> Cursor.err_invalid t ("hue unit: " ^ unit)

let read_separated_values t p1 p2 =
  let v1 = p1 t in
  Cursor.ws t;
  ignore (Cursor.comma_opt t : bool);
  Cursor.ws t;
  (* Need whitespace after comma *)
  let v2 = p2 t in
  (v1, v2)

let read_hsl t : color =
  Cursor.ws t;
  let h = match read_hue t with Hue_none -> Unitless 0. | h -> h in
  Cursor.ws t;
  (* Handle comma or space separator after hue *)
  let comma_separated = Cursor.comma_opt t in
  Cursor.ws t;
  let s = read_percentage_float t in
  Cursor.ws t;
  if comma_separated then Cursor.comma t
  else if Cursor.comma_opt t then
    Cursor.err_invalid t "mixed comma and space separated hsl() syntax";
  Cursor.ws t;
  let l = read_percentage_float t in
  let a =
    Cursor.ws t;
    if comma_separated && Cursor.comma_opt t then read_alpha t
    else if (not comma_separated) && Cursor.comma_opt t then
      Cursor.err_invalid t "mixed comma and space separated hsl() syntax"
    else if Cursor.peek_delim t = Some '/' then read_optional_alpha t
    else None
  in
  Cursor.ws t;
  Cursor.expect_eof t;
  Hsl { h; s = Pct s; l = Pct l; a }

let read_hwb t : color =
  Cursor.ws t;
  let h = match read_hue t with Hue_none -> Unitless 0. | h -> h in
  let w, b =
    read_separated_values t read_percentage_float read_percentage_float
  in
  let a =
    Cursor.ws t;
    if Cursor.comma_opt t then read_alpha t
    else if Cursor.peek_delim t = Some '/' then read_optional_alpha t
    else None
  in
  Cursor.ws t;
  Cursor.expect_eof t;
  Hwb { h; w = Pct w; b = Pct b; a }

let read_ok_lightness t : percentage option =
  Cursor.ws t;
  if Cursor.looking_at t "none" then (
    Cursor.expect_string "none" t;
    Option.None)
  else
    let n, unit = Cursor.number_with_unit t in
    (* CSS Color 4 sec. 9.3/9.4: an out-of-range lightness clamps rather than
       invalidating the colour, matching lab/lch. 0% to 100% names the same 0 to
       1 a bare number does, so both spellings clamp alike. *)
    match unit with
    | Some "%" -> Some (Pct (max 0. (min 100. n)) : percentage)
    | None -> Some (Num (max 0. (min 1. n)) : percentage)
    | Some u -> Cursor.err_invalid t ("oklch() L unit: " ^ u)

let read_number_or_none ?pct_scale t : float option =
  Cursor.ws t;
  if Cursor.looking_at t "none" then (
    Cursor.expect_string "none" t;
    None)
  else
    let n, unit = Cursor.number_with_unit t in
    match unit with
    | Some "%" ->
        Some
          (n
          *.
          match pct_scale with
          | Some scale -> scale
          | None -> Cursor.err_invalid t "unexpected percentage")
    | None -> Some n
    | Some unit -> Cursor.err_invalid t ("invalid unit: " ^ unit)

let read_oklch t : color =
  Cursor.ws t;
  (* L can be 0-1 or 0%-100% per CSS spec. *)
  let l = read_ok_lightness t in
  Cursor.ws t;
  let c = read_number_or_none ~pct_scale:0.004 t in
  Cursor.ws t;
  let h = read_hue t in
  let alpha = read_optional_alpha t in
  Cursor.ws t;
  Cursor.expect_eof t;
  Oklch { l; c; h; alpha }

let read_oklab t : color =
  Cursor.ws t;
  (* L can be 0-1 or 0%-100% per CSS spec *)
  let l = read_ok_lightness t in
  let a = read_number_or_none ~pct_scale:0.004 t in
  let b = read_number_or_none ~pct_scale:0.004 t in
  let alpha = read_optional_alpha t in
  Cursor.ws t;
  Cursor.expect_eof t;
  Oklab { l; a; b; alpha }

let read_lab t : color =
  Cursor.ws t;
  (* CSS Color 4 sec. 9.3: lab() L accepts [<percentage>] or [<number>]; keep
     them distinct ([Pct] vs [Num]) so the printer can drive different precision
     (1 decimal for [<number>] over [0, 100], 4 decimals for [<percentage>]
     which scales by 100). *)
  let l : percentage option =
    if Cursor.looking_at t "none" then (
      Cursor.expect_string "none" t;
      Option.None)
    else
      let n, unit = Cursor.number_with_unit t in
      Some
        (match unit with
         | Some "%" -> Pct (max 0. (min 100. n))
         | None -> Num (max 0. (min 100. n))
         | Some u -> Cursor.err_invalid t ("lab L unit: " ^ u)
          : percentage)
  in
  let a = read_number_or_none ~pct_scale:1.25 t in
  let b = read_number_or_none ~pct_scale:1.25 t in
  let alpha = read_optional_alpha t in
  Cursor.ws t;
  Cursor.expect_eof t;
  Lab { l; a; b; alpha }

let read_lch t : color =
  Cursor.ws t;
  let read_l_axis t : percentage option =
    if Cursor.looking_at t "none" then (
      Cursor.expect_string "none" t;
      Option.None)
    else
      (* CSS Color 4 sec. 9.3 lch(): the L channel is [<percentage> | <number>],
         distinguish so [lch(54.321 ...)] doesn't round-trip as [lch(54.321%
         ...)] - the [Pct] / [Num] variants drive different printers (the
         [%]-typed one scales the precision by 100 and emits [%]). *)
      let n, unit = Cursor.number_with_unit t in
      Some
        (match unit with
         | Some "%" -> Pct (max 0. (min 100. n))
         | None -> Num (max 0. (min 100. n))
         | Some u -> Cursor.err_invalid t ("lch L unit: " ^ u)
          : percentage)
  in
  let read_c = read_number_or_none ~pct_scale:1.5 in
  let l, c, h = Cursor.triple ~sep:Cursor.ws read_l_axis read_c read_hue t in
  let alpha = read_optional_alpha t in
  Cursor.ws t;
  Cursor.expect_eof t;
  Lch { l; c; h; alpha }

let read_color_function t : color =
  Cursor.ws t;
  let space = read_color_space t in
  Cursor.ws t;
  let components = read_color_components space t [] in
  if List.length components <> 3 then
    Cursor.err_invalid t "color() requires three components";
  let alpha = read_optional_alpha t in
  Color { space; components; alpha }

(* A bare [<percentage>] leaf inside a color-mix weight [calc()]. Unlike the
   top-level weight it is not range-checked: CSS Values 4 sec. 10 clamps the
   math function's result, not its individual operands. [read_calc] handles the
   [var()] and nested-[calc()] factors itself, so the leaf only sees a token. *)
let read_color_mix_calc_pct t : percentage = Pct (Cursor.pct t)

(** Forward declaration for percentage reader used in color-mix *)
let rec read_percentage_in_color_mix t : percentage =
  Cursor.ws t;
  if Cursor.looking_at t "var(" then
    Var (read_var read_percentage_in_color_mix t)
  else if Cursor.looking_at_calc t then
    (* CSS Color 5 sec. 3: the weight may be a math function. We can't
       bound-check a [calc()] statically (it may carry a [var()]), so we keep it
       verbatim and let substitution-time clamping apply. *)
    Calc (read_calc ~result_type:`Value read_color_mix_calc_pct t)
  else
    let n = Cursor.pct t in
    if n < 0. || n > 100. then
      Cursor.err_invalid t "color-mix percentage must be between 0% and 100%";
    (Pct n : percentage)

let read_optional_percentage t : percentage option =
  (* CSS Color 5 sec. 3 (https://drafts.csswg.org/css-color-5/#color-mix): the
     [color-mix()] weight grammar is [<percentage [0,100]>?] - strictly a
     percentage token, no [<number>] alternative. We don't accept a bare decimal
     here even though some minifiers (cssnano) ship the [0% -> 0] shortcut as a
     non-spec optimization that browsers tolerate. *)
  Cursor.ws t;
  if Cursor.looking_at t "var(" then
    Some (Var (read_var read_percentage_in_color_mix t))
  else if Cursor.looking_at_calc t then
    Some (Calc (read_calc ~result_type:`Value read_color_mix_calc_pct t))
  else
    match Cursor.percentage_opt t with
    | Some n ->
        if n < 0. || n > 100. then
          Cursor.err_invalid t
            "color-mix percentage must be between 0% and 100%";
        Cursor.ws t;
        Some (Pct n : percentage)
    | None -> None

let hue_interpolation_start = function
  | "shorter" | "longer" | "increasing" | "decreasing" | "specified" -> true
  | _ -> false

let read_hue_interpolation_direction t : hue_interpolation =
  Cursor.enum "hue-interpolation"
    [
      ("shorter", Shorter);
      ("longer", Longer);
      ("increasing", Increasing);
      ("decreasing", Decreasing);
      ("specified", Specified);
    ]
    t

let read_full_hue_interpolation t : hue_interpolation =
  let hue = read_hue_interpolation_direction t in
  Cursor.ws t;
  Cursor.expect_string "hue" t;
  Cursor.ws t;
  hue

let trim_trailing_space buf =
  let blen = Buffer.length buf in
  if blen > 0 && Buffer.nth buf (blen - 1) = ' ' then
    Buffer.truncate buf (blen - 1)

let add_pending_space buf last_was_space =
  if last_was_space && Buffer.length buf > 0 then Buffer.add_char buf ' '

let normalize_relative_color_tail tail =
  let tail = String.trim tail in
  let len = String.length tail in
  let buf = Buffer.create len in
  let rec skip_spaces i =
    if i < len && tail.[i] = ' ' then skip_spaces (i + 1) else i
  in
  let rec loop i last_was_space =
    if i >= len then ()
    else
      match tail.[i] with
      | ' ' | '\n' | '\t' | '\r' | '\012' -> loop (i + 1) true
      | '/' ->
          trim_trailing_space buf;
          Buffer.add_char buf '/';
          loop (skip_spaces (i + 1)) false
      | c ->
          add_pending_space buf last_was_space;
          Buffer.add_char buf c;
          loop (i + 1) false
  in
  loop 0 false;
  Buffer.contents buf

(* CSS Color 5 sec. 4.1 gives each channel one component value, so the channels
   are the components before the alpha slash that are not whitespace. Splitting
   on whitespace instead would miscount the spellings that need no separator:
   [20%g] is a percentage and an ident, [calc(...)10] a function and a
   number. *)
let relative_color_channel_count cvs =
  let is_ws = function
    | Component.Preserved { Token.kind = Whitespace; _ } -> true
    | _ -> false
  in
  let is_alpha_sep = function
    | Component.Preserved { Token.kind = Delim "/"; _ } -> true
    | _ -> false
  in
  let rec loop count = function
    | [] -> count
    | cv :: _ when is_alpha_sep cv -> count
    | cv :: rest when is_ws cv -> loop count rest
    | _ :: rest -> loop (count + 1) rest
  in
  loop 0 cvs

let relative_color_has_empty_alpha cvs =
  let is_ws = function
    | Component.Preserved { Token.kind = Whitespace; _ } -> true
    | _ -> false
  in
  let rec only_ws = function
    | [] -> true
    | cv :: rest when is_ws cv -> only_ws rest
    | _ -> false
  in
  let rec loop = function
    | [] -> false
    | Component.Preserved { Token.kind = Delim "/"; _ } :: rest -> only_ws rest
    | _ :: rest -> loop rest
  in
  loop cvs

let try_fold_color_function_static origin t : color option =
  Cursor.ws t;
  let read_keyword kw =
    match Cursor.peek_ident t with
    | Some id when String.lowercase_ascii id = kw ->
        ignore (Cursor.ident t);
        Cursor.ws t;
        true
    | _ -> false
  in
  if not (read_keyword "srgb") then Option.None
  else if not (read_keyword "r" && read_keyword "g" && read_keyword "b") then
    Option.None
  else
    let alpha = read_optional_alpha t in
    Cursor.ws t;
    if not (Cursor.is_done t) then Option.None
    else
      match static_color_to_srgb_bytes origin with
      | Some (r, g, b, origin_a_byte) ->
          let final_alpha : alpha =
            match alpha with
            | None when origin_a_byte = 255 -> None
            | None -> Num (Float.of_int origin_a_byte /. 255.)
            | a -> a
          in
          Option.Some
            (Rgba
               {
                 rgb = Channels { r = Int r; g = Int g; b = Int b };
                 a = final_alpha;
               })
      | None -> Option.None

let try_fold_relative_color_static name origin t : color option =
  match name with
  | "color" -> try_fold_color_function_static origin t
  | _ -> Option.None

let rec read_color_mix t : color =
  Cursor.ws t;
  (* CSS Color 5 sec. 3: an omitted [<color-interpolation-method>] defaults to
     [in oklab], so the [in <space> [<hue> hue]?] prefix (and its trailing
     comma) is optional. *)
  let has_method = Cursor.looking_at_ident "in" t in
  let in_space, hue =
    if has_method then (
      Cursor.expect_string "in" t;
      Cursor.ws t;
      let space = read_color_space t in
      Cursor.ws t;
      let hue =
        match Cursor.peek_ident t with
        | Some name when hue_interpolation_start (String.lowercase_ascii name)
          ->
            read_full_hue_interpolation t
        | _ -> Default
      in
      (Some space, hue))
    else ((None : color_space option), Default)
  in

  Cursor.ws t;
  if has_method then (
    Cursor.comma t;
    Cursor.ws t);

  (* Parse first color and optional percentage *)
  let color1, percent1 = read_color_mix_component t in

  Cursor.comma t;
  Cursor.ws t;

  (* Parse second color and optional percentage *)
  let color2, percent2 = read_color_mix_component t in

  Cursor.ws t;
  Mix { in_space; hue; color1; percent1; color2; percent2 }

and read_color_mix_component t =
  (* CSS Color 5 sec. 3: a component is [<color> && <percentage>?] - the two may
     appear in either order. Prefer the [<color> <percentage>?] reading so an
     ambiguous leading [var()] is taken as the colour ([var(--c) var(--p)] keeps
     its source order rather than being re-emitted percentage-first). Fall back
     to [<percentage> <color>] only when the colour-first reading leaves the
     component unconsumed - i.e. the leading token was really the percentage and
     a concrete colour follows ([var(--p) red], [30% red]). *)
  let color_first t =
    let component = read_color_mix_suffix_percentage t in
    Cursor.ws t;
    if Cursor.peek_comma t || Cursor.is_done t then component
    else Cursor.err_expected t "color-mix component end"
  in
  match Cursor.option color_first t with
  | Some component -> component
  | None -> read_color_mix_prefix_percentage t

and read_color_mix_prefix_percentage t =
  match read_optional_percentage t with
  | None -> Cursor.err_expected t "color-mix percentage"
  | Some percent ->
      let color = read_color t in
      Cursor.ws t;
      if Option.is_some (read_optional_percentage t) then
        Cursor.err_invalid t "color-mix component cannot have two percentages";
      (color, Some percent)

and read_color_mix_suffix_percentage t =
  let color = read_color t in
  Cursor.ws t;
  match read_optional_percentage t with
  | None -> (color, None)
  | Some percent -> (color, Some percent)

and read_relative_rgb t : color =
  Cursor.ws t;
  Cursor.expect_string "from" t;
  Cursor.ws t;
  let origin = read_color t in
  Cursor.ws t;
  let tail_components = Cursor.remaining t in
  if relative_color_has_empty_alpha tail_components then
    Cursor.err_expected t "relative rgb alpha";
  let tail =
    Cursor.consume_remaining_as_string ~trim:true t
    |> normalize_relative_color_tail
  in
  if tail = "" then Cursor.err_expected t "relative rgb channels";
  if relative_color_channel_count tail_components <> 3 then
    Cursor.err_expected t "relative rgb channels";
  Relative_rgb (origin, tail)

and read_contrast_color t : color =
  Cursor.ws t;
  let color = read_color t in
  Cursor.ws t;
  Contrast_color color

and read_light_dark t : color =
  Cursor.ws t;
  let light = read_color t in
  Cursor.ws t;
  Cursor.comma t;
  Cursor.ws t;
  let dark = read_color t in
  Cursor.ws t;
  Cursor.expect_eof t;
  Light_dark (light, dark)

and read_color_attr t : color =
  Cursor.ws t;
  let name = Cursor.ident t in
  Cursor.ws t;
  Cursor.call "type" t (fun inner ->
      Cursor.ws inner;
      Cursor.expect '<' inner;
      Cursor.expect_string "color" inner;
      Cursor.expect '>' inner;
      Cursor.ws inner;
      Cursor.expect_eof inner);
  Cursor.ws t;
  let fallback =
    if Cursor.comma_opt t then (
      Cursor.ws t;
      Some (read_color t))
    else None
  in
  Cursor.ws t;
  Cursor.expect_eof t;
  Attribute (name, fallback)

and read_relative_color name t : color =
  (* CSS Color 5 sec. 4.2: any colour function may take [from <origin> <c1> <c2>
     <c3> [/ <alpha>]?]. The origin has a fixed <color> type, so retain the
     parsed node while keeping the not-yet-modelled channel expression
     opaque. *)
  Cursor.ws t;
  Cursor.expect_string "from" t;
  Cursor.ws t;
  let origin = read_color t in
  Cursor.ws t;
  let snap = Cursor.save t in
  match try_fold_relative_color_static name origin t with
  | Some folded -> folded
  | None -> (
      Cursor.restore t snap;
      let tail =
        Cursor.consume_remaining_as_string ~trim:true t
        |> normalize_relative_color_tail
      in
      if tail = "" then Cursor.err_expected t (name ^ " channels");
      match fold_relative_color_pass_through name origin tail with
      | Some color -> color
      | None -> Relative_color (name, origin, tail))

(* CSS Color 5 sec. 5.1: [color(from <origin> srgb r g b [/ <alpha>]?)] is a
   self-substitution of the origin's sRGB channels. Static folding requires the
   origin to be reducible to sRGB bytes and the three channel slots to be the
   bare [r] [g] [b] keywords. Wider Color 5 substitutions (other spaces,
   calc()-on-keyword arithmetic, swapped channels) need real per-space
   conversion machinery; absent that we fall through and keep the original
   [color(from ...)] string. *)
and with_relative_fallback name fallback t =
  Cursor.ws t;
  if Cursor.looking_at t "from" then read_relative_color name t else fallback t

and color_parsers =
  [
    ( "rgb",
      fun t ->
        Cursor.ws t;
        if Cursor.looking_at t "from" then read_relative_rgb t
        else
          Cursor.one_of [ read_rgb_space_separated; read_rgb_comma_separated ] t
    );
    ( "rgba",
      fun t ->
        Cursor.ws t;
        Cursor.one_of [ read_rgb_space_separated; read_rgb_comma_separated ] t
    );
    ("hsl", fun t -> with_relative_fallback "hsl" read_hsl t);
    ("hsla", fun t -> with_relative_fallback "hsl" read_hsl t);
    ("hwb", fun t -> with_relative_fallback "hwb" read_hwb t);
    ("oklch", fun t -> with_relative_fallback "oklch" read_oklch t);
    ("lab", fun t -> with_relative_fallback "lab" read_lab t);
    ("oklab", fun t -> with_relative_fallback "oklab" read_oklab t);
    ("lch", fun t -> with_relative_fallback "lch" read_lch t);
    ("color", fun t -> with_relative_fallback "color" read_color_function t);
    ("contrast-color", read_contrast_color);
    ("light-dark", read_light_dark);
    ("attr", read_color_attr);
    ("color-mix", read_color_mix);
  ]

and read_color t : color =
  Cursor.ws t;
  (* The two branches below consume the token before judging it, so they mark
     [loc] rather than whatever the cursor has moved on to. *)
  let loc = Cursor.position t in
  let color =
    match Cursor.peek t with
    | Some (Component.Preserved { kind = Token.Hash { value; _ }; _ }) -> (
        Cursor.skip t;
        let len = String.length value in
        let is_hex c =
          ('0' <= c && c <= '9')
          || ('a' <= c && c <= 'f')
          || ('A' <= c && c <= 'F')
        in
        if not (len = 3 || len = 4 || len = 6 || len = 8) then
          Cursor.err_invalid ~loc t ("hex color length: " ^ string_of_int len)
        else if not (String.for_all is_hex value) then
          Cursor.err_invalid ~loc t ("hex color digits: " ^ value)
        else
          match rgba_of_hex value with
          | Some (r, g, b, a) -> Authored_hex { value; r; g; b; a }
          | None -> Cursor.err_invalid ~loc t ("hex color digits: " ^ value))
    | Some (Component.Func ({ node = { name; _ }; _ } as fn)) -> (
        (* CSS Values 4 sec. 4.1: a function name is a keyword, so it names the
           same function in whatever case it was written. *)
        let fn_name = Common.String.lowercase_ascii_preserve name in
        match List.assoc_opt fn_name color_parsers with
        | Some parser ->
            Cursor.skip t;
            parser (Cursor.func_sub fn t)
        | None when fn_name = "var" -> Var (read_var read_color t)
        | None -> Cursor.err t ("unknown color function: " ^ name))
    | Some (Component.Preserved { kind = Token.Ident ident; _ }) -> (
        Cursor.skip t;
        (* CSS color keywords are case-insensitive. *)
        match read_color_keyword_of_string (String.lowercase_ascii ident) with
        | Some Auto -> Cursor.err_invalid ~loc t "auto is not a colour"
        | Some color -> color
        | None -> Cursor.err ~loc t ("unknown color: " ^ ident))
    | _ -> Cursor.err t "color"
  in
  Cursor.ws t;
  (match Cursor.peek_delim t with
  | Some '/' -> Cursor.err_invalid t "unexpected color alpha separator"
  | _ -> ());
  color

let rec color_has_specified_hue = function
  | Mix { hue = Specified; _ } -> true
  | Mix { color1; color2; _ } ->
      color_has_specified_hue color1 || color_has_specified_hue color2
  | Relative_rgb (origin, _) -> color_has_specified_hue origin
  | Relative_color (_, origin, _) -> color_has_specified_hue origin
  | Contrast_color color -> color_has_specified_hue color
  | Light_dark (a, b) -> color_has_specified_hue a || color_has_specified_hue b
  | Attribute (_, Some color) -> color_has_specified_hue color
  | _ -> false

(* [color_exists p c] is [true] when [p] holds for [c] or for any color nested
   inside it (color-mix operands, light-dark arms, contrast-color, attr()
   fallback). Those composites are the only colors that carry sub-colors. *)
let color_exists p =
  let rec go c =
    p c
    ||
    match c with
    | Mix { color1; color2; _ } -> go color1 || go color2
    | Relative_rgb (origin, _) -> go origin
    | Relative_color (_, origin, _) -> go origin
    | Light_dark (a, b) -> go a || go b
    | Contrast_color c -> go c
    | Attribute (_, Some c) -> go c
    | _ -> false
  in
  go

let color_is_color_4 =
  color_exists (function
    | Lab _ | Lch _ | Oklab _ | Oklch _ | Hwb _ | Color _ | Relative_rgb _
    | Relative_color _ ->
        true
    | _ -> false)

let read_system_color t : system_color =
  Cursor.ws t;
  let keyword = Cursor.ident t in
  match String.lowercase_ascii keyword with
  | "accentcolor" -> Accent_color
  | "accentcolortext" -> Accent_color_text
  | "activetext" -> Active_text
  | "buttonborder" -> Button_border
  | "buttonface" -> Button_face
  | "buttontext" -> Button_text
  | "canvas" -> Canvas
  | "canvastext" -> Canvas_text
  | "field" -> Field
  | "fieldtext" -> Field_text
  | "graytext" -> Gray_text
  | "highlight" -> Highlight
  | "highlighttext" -> Highlight_text
  | "linktext" -> Link_text
  | "mark" -> Mark
  | "marktext" -> Mark_text
  | "selecteditem" -> Selected_item
  | "selecteditemtext" -> Selected_item_text
  | "visitedtext" -> Visited_text
  | "-webkit-focus-ring-color" -> Webkit_focus_ring_color
  | _ -> Cursor.err_invalid t ("system color: " ^ keyword)

let duration_css_wide =
  [
    ("inherit", (Inherit : duration));
    ("initial", Initial);
    ("unset", Unset);
    ("revert", Revert);
    ("revert-layer", Revert_layer);
  ]

let read_duration_number ~canonicalize_ms:_ t : duration =
  let n, unit_raw = Cursor.number_with_unit t in
  if n < 0.0 then Cursor.err_invalid t "negative durations are not allowed"
  else
    let unit = String.lowercase_ascii (Option.value unit_raw ~default:"") in
    match unit with
    | "" when n = 0.0 -> S 0.0
    | "s" -> S n
    | "ms" -> Ms n
    | _ -> Cursor.err_invalid t ("duration unit: " ^ unit)

let read_time_number t : duration =
  let n, unit_raw = Cursor.number_with_unit t in
  let unit = String.lowercase_ascii (Option.value unit_raw ~default:"") in
  match unit with
  | "" when n = 0.0 -> S 0.0
  | "s" -> S n
  | "ms" -> Ms n
  | _ -> Cursor.err_invalid t ("time unit: " ^ unit)

let read_duration_round read_duration_self t =
  read_round_call
    (fun s v step -> (Round (s, v, step) : duration))
    read_duration_self t

let rec read_duration_with ?(css_wide = true) ~canonicalize_ms t : duration =
  let read_duration_self t = read_duration_with ~css_wide ~canonicalize_ms t in
  Cursor.enum_or_calls
    ~default:(read_duration_number ~canonicalize_ms)
    "duration"
    (if css_wide then duration_css_wide else [])
    ~calls:
      [
        ("var", fun t -> Var (read_var read_duration_self t));
        ( "calc",
          fun t -> Calc (read_calc ~result_type:`Value read_duration_in_calc t)
        );
        ("round", read_duration_round read_duration_self);
        ( "rem",
          read_binary_call "rem"
            (fun a b -> (Rem (a, b) : duration))
            read_duration_self );
        ( "mod",
          read_binary_call "mod"
            (fun a b -> (Mod (a, b) : duration))
            read_duration_self );
      ]
    t

(** Read a duration value *)
and read_duration_in_calc t : duration =
  read_duration_with ~css_wide:false ~canonicalize_ms:false t

(** Read a duration value *)
let read_duration t : duration = read_duration_with ~canonicalize_ms:true t

let read_duration_preserve_ms t : duration =
  read_duration_with ~canonicalize_ms:false t

(** Read a time value that can be negative (for animation-delay,
    transition-delay) *)
let rec read_time_with ?(css_wide = true) t : duration =
  Cursor.enum_or_calls ~default:read_time_number "time"
    (if css_wide then duration_css_wide else [])
    ~calls:
      [
        ("var", fun t -> Var (read_var read_time t));
        ( "calc",
          fun t -> Calc (read_calc ~result_type:`Value read_time_in_calc t) );
        ("round", read_duration_round read_time);
        ( "rem",
          read_binary_call "rem" (fun a b -> (Rem (a, b) : duration)) read_time
        );
        ( "mod",
          read_binary_call "mod" (fun a b -> (Mod (a, b) : duration)) read_time
        );
      ]
    t

and read_time t : duration = read_time_with t
and read_time_in_calc t : duration = read_time_with ~css_wide:false t

let number_binary_functions =
  [
    ("mod", fun a b -> Mod (a, b));
    ("rem", fun a b -> Rem (a, b));
    ("hypot", fun a b -> Hypot (a, b));
    ("pow", fun a b -> Pow (a, b));
  ]

let number_unary_functions =
  [
    ("sqrt", fun value -> Sqrt value);
    ("abs", fun value -> Abs value);
    ("sign", fun value -> Sign value);
  ]

let angle_number_functions = [ ("sin", fun value -> Sin value) ]

let read_angle_number_function name make t =
  Cursor.call name t (fun inner ->
      let value = read_angle inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      make value)

(** Read a number value *)
let rec read_number t : number =
  Cursor.ws t;
  let number =
    match read_number_function t with
    | Some value -> value
    | None -> Num (Cursor.number t)
  in
  Cursor.ws t;
  (match Cursor.peek t with
  | Some (Component.Func _)
  | Some (Component.Preserved { kind = Token.Number_tok _; _ }) ->
      Cursor.err_invalid t "unexpected tokens after number"
  | _ -> ());
  number

and read_number_function t =
  match Cursor.peek t with
  | Some (Component.Func { node = { name; _ }; _ }) -> (
      let name = String.lowercase_ascii name in
      match name with
      | "var" -> Some (Var (read_var read_number t))
      | "calc" ->
          let read_number_dim_only t = Cursor.err t "expected numeric factor" in
          Some (Calc (read_calc ~result_type:`Number read_number_dim_only t))
      | "min" ->
          Some
            (Num
               (Cursor.call "min" t (fun inner ->
                    let nums =
                      Cursor.list ~sep:Cursor.comma ~at_least:1 read_num_expr
                        inner
                    in
                    Cursor.ws inner;
                    Cursor.expect_eof inner;
                    List.fold_left Float.min Float.infinity nums)))
      | "max" ->
          Some
            (Num
               (Cursor.call "max" t (fun inner ->
                    let nums =
                      Cursor.list ~sep:Cursor.comma ~at_least:1 read_num_expr
                        inner
                    in
                    Cursor.ws inner;
                    Cursor.expect_eof inner;
                    List.fold_left Float.max Float.neg_infinity nums)))
      | "clamp" ->
          Some
            (Num
               (Cursor.call "clamp" t (fun inner ->
                    let min_value = read_num_expr inner in
                    Cursor.ws inner;
                    Cursor.comma inner;
                    let value = read_num_expr inner in
                    Cursor.ws inner;
                    Cursor.comma inner;
                    let max_value = read_num_expr inner in
                    Cursor.ws inner;
                    Cursor.expect_eof inner;
                    Float.max min_value (Float.min value max_value))))
      | "round" -> Some (read_round_number t)
      | _ -> read_math_number_function t name)
  | _ -> None

and read_math_number_function t name =
  match List.assoc_opt name number_binary_functions with
  | Some make -> Some (read_binary_number_function name make t)
  | None -> (
      match List.assoc_opt name number_unary_functions with
      | Some make -> Some (read_unary_number_function name make t)
      | None -> (
          match List.assoc_opt name angle_number_functions with
          | Some make -> Some (read_angle_number_function name make t)
          | None -> None))

and read_round_number t =
  read_round_call (fun s v step -> (Round (s, v, step) : number)) read_number t

and read_binary_number_function name make t =
  read_binary_call name make read_number t

and read_unary_number_function name make t =
  Cursor.call name t (fun inner ->
      let value = read_number inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      make value)

(** Read transition_behavior value *)
let read_transition_behavior t : transition_behavior =
  Cursor.enum "transition-behavior"
    [ ("normal", Normal); ("allow-discrete", Allow_discrete) ]
    t

(** Read a percentage type with var() and calc() support *)
let rec read_percentage t : percentage =
  Cursor.ws t;
  if Cursor.looking_at t "var(" then Var (read_var read_percentage t)
  else if Cursor.looking_at_calc t then
    Calc (read_calc ~result_type:`Value read_percentage t)
  else Pct (Cursor.pct t)

(* CSS Values 4 sec. 10.4: math functions that produce a non-length type ([asin]
   / [acos] / [atan] / [atan2] return angles, [sin] / [cos] / [tan] return
   numbers). Used in a [<length-percentage>] context they are spec-invalid;
   lightning et al. preserve verbatim, so cascade captures the original call as
   [Invalid [<call>]]. *)
let length_invalid_function_name name =
  match String.lowercase_ascii name with
  | "asin" | "acos" | "atan" | "atan2" | "sin" | "cos" | "tan" -> true
  | _ -> false

let read_invalid_length_percentage_function t : length_percentage =
  match Cursor.peek t with
  | Some (Component.Func { node = { name; _ }; _ } as comp)
    when length_invalid_function_name name ->
      Cursor.skip t;
      Invalid [ comp ]
  | _ -> Cursor.err t "expected invalid length-percentage function"

let read_length_percentage_pct ~allow_negative t : length_percentage =
  let n = Cursor.pct t in
  if (not allow_negative) && n < 0.0 then Cursor.err_invalid t "negative";
  Pct n

let read_length_percentage_length ~allow_negative ~with_keywords t :
    length_percentage =
  Length (read_length ~allow_negative ~with_keywords t)

(** Read length_percentage value *)
let rec read_length_percentage ?(allow_negative = true) ?(with_keywords = true)
    t : length_percentage =
  Cursor.ws t;
  Cursor.one_of
    [
      read_length_percentage_var ~allow_negative ~with_keywords;
      read_length_percentage_env ~allow_negative ~with_keywords;
      read_length_percentage_calc ~with_keywords;
      read_invalid_length_percentage_function;
      read_length_percentage_pct ~allow_negative;
      read_length_percentage_length ~allow_negative ~with_keywords;
    ]
    t

and read_length_percentage_var ~allow_negative ~with_keywords t :
    length_percentage =
  if Cursor.looking_at t "var(" then
    Var (read_var (read_length_percentage ~allow_negative ~with_keywords) t)
  else Cursor.err t "expected var"

and read_length_percentage_env ~allow_negative ~with_keywords t :
    length_percentage =
  if Cursor.looking_at t "env(" then
    Env (read_env (read_length_percentage ~allow_negative ~with_keywords) t)
  else Cursor.err t "expected env"

and read_length_percentage_calc ~with_keywords t : length_percentage =
  if Cursor.looking_at_calc t then
    (* CSS Values 4 10 (calc): inside [calc()] negative operands are always
       allowed even when the surrounding property is non-negative; the
       non-negative constraint applies to the resolved value, not to inner
       operands. *)
    Calc
      (read_calc ~result_type:`Value (read_length_percentage ~with_keywords) t)
  else Cursor.err t "expected calc"

(** Read number_percentage value. Inside a [<number-percentage>] [calc()], a raw
    number is modelled at the calc level as the [Num x] node rather than
    [Val (Num x)] (the [Num] sub-variant of [number_percentage] wrapped in
    [Val]); the dedicated [_dim_only] reader excludes the raw-number alternative
    so the generic [read_calc_factor] falls through to its own [Num] path. *)
let rec read_number_percentage_dim_only t : number_percentage =
  Cursor.ws t;
  Cursor.one_of
    [
      (fun t -> (Pct (Cursor.pct t) : number_percentage));
      (fun t ->
        (Var (read_var read_number_percentage_dim_only t) : number_percentage));
    ]
    t

let rec read_number_percentage t : number_percentage =
  Cursor.ws t;
  if Cursor.looking_at t "var(" then Var (read_var read_number_percentage t)
  else if Cursor.looking_at_calc t then
    Calc
      (read_calc ~result_type:`Number_or_value read_number_percentage_dim_only t)
  else if
    Cursor.looking_at_func "min" t
    || Cursor.looking_at_func "max" t
    || Cursor.looking_at_func "clamp" t
  then Num (read_numeric_expression t)
  else
    (* Try to read as percentage or number *)
    Cursor.one_of
      [
        (fun t -> (Pct (Cursor.pct t) : number_percentage));
        (fun t -> (Num (Cursor.number t) : number_percentage));
      ]
      t

(** Read color_name value *)
let read_color_name t : color_name =
  Cursor.ws t;
  let s = Cursor.ident t in
  match color_name_of_string (String.lowercase_ascii s) with
  | Some name -> name
  | None -> Cursor.err_invalid t ("color name: " ^ s)

(* Shortest spelling of a hex value as a [color]: shorten, then take the named
   form when the table has one, which it holds only for the colours whose name
   is the shorter spelling (the [pp_hex_color] choice, as an AST rewrite
   producing a [color] instead of printing). *)
let canonical_color_of_hex r g b a : color =
  match color_name_of_hex (hex_string_of_bytes r g b a) with
  | Some name -> Named name
  | None -> Hex { r; g; b; a }

(* Canonicalise a colour's alpha for a colour the static fold leaves alone (e.g.
   a [var()] channel). CSS Color 4 sec. 4.2: a fully-opaque [/ 1] / [/ 100%] is
   redundant and drops; an alpha [<percentage>] is the [<number>] divided by 100
   and serialises as the number. *)
let alpha_scale ?(exact = true) op (a : alpha) n : alpha option =
  if not (Float.is_finite n) then Option.none
  else
    let scaled = scale_leaf ~exact op n in
    match a with
    | Num v -> scaled (fun x -> (Num x : alpha)) v
    | Pct v -> scaled (fun x -> (Pct x : alpha)) v
    | _ -> Option.none

let alpha_combine op (a : alpha) (b : alpha) : alpha option =
  let f = combine_leaf op in
  match (a, b) with
  | Num x, Num y -> Option.map (fun r -> (Num r : alpha)) (f x y)
  | Pct x, Pct y -> Option.map (fun r -> (Pct r : alpha)) (f x y)
  | _ -> Option.none

let alpha_is_zero : alpha -> bool = function
  | Num f | Pct f -> f = 0.
  | _ -> false

let alpha_calc_math_fn (fn : math_fn) : alpha calc =
  typed_math_fn
    (fun unit v ->
      if String.equal unit "%" then Option.some (Pct v : alpha) else Option.none)
    fn

let eval_alpha_calc ?(ctx = default_calc_ctx) (c : alpha calc) : alpha calc =
  eval_typed_calc ~math_fn:alpha_calc_math_fn
    ~scale:(fun ~exact op v n -> alpha_scale ~exact op v n)
      (* An alpha coefficient prints in full through [Pp.float] / [Pp.pct]. *)
    ~combine:alpha_combine ~round:Fun.id ~is_zero:alpha_is_zero
    ~zero:(Val (Num 0.) : alpha calc)
    ~ctx c

let rec normalize_alpha (a : alpha) : alpha =
  match a with
  | Num 1.0 | Pct 100.0 -> None
  | Pct f -> Num (f /. 100.)
  | Calc c -> (
      match eval_alpha_calc c with
      | Num f -> normalize_alpha (Num f)
      | Val v -> normalize_alpha v
      | folded -> Calc folded)
  | other -> other

let drop_full_alpha (c : color) : color =
  match c with
  | Rgba { rgb; a } -> Rgba { rgb; a = normalize_alpha a }
  | Hsl r -> Hsl { r with a = normalize_alpha r.a }
  | Hwb r -> Hwb { r with a = normalize_alpha r.a }
  | Color r -> Color { r with alpha = normalize_alpha r.alpha }
  | Oklab r -> Oklab { r with alpha = normalize_alpha r.alpha }
  | Oklch r -> Oklch { r with alpha = normalize_alpha r.alpha }
  | Lab r -> Lab { r with alpha = normalize_alpha r.alpha }
  | Lch r -> Lch { r with alpha = normalize_alpha r.alpha }
  | _ -> c

(* CSS Color 4 secs. 16.3-16.4 makes [<percentage>] and [<number>] equivalent
   for oklch/oklab/lch/lab lightness ([num = pct *. pct_scale]: ok* L 100% = 1,
   lch/lab L 100% = 100). Canonicalise to the shorter spelling here, in the AST
   normalize pass, so [pp_color_lightness] serialises the node faithfully rather
   than swapping percentage for number at print time. *)
(* Round a lab/oklab/lch/oklch coefficient to its canonical decimal budget. The
   printer serialises faithfully, so this AST fold owns the value-changing
   precision reduction: [axis_max_decimals] picks the shortest form for the
   canonical (optimize) output, while a consumer that skips [normalize_color]
   (e.g. [to_string ~minify:true] on a typed colour) keeps the authored
   coefficient. [lossless] suppresses the reduction. *)
let round_color_axis ~lossless ~decimals f =
  if lossless then f
  else
    let f = Pp.round_sig 6 f in
    let m = 10. ** float_of_int decimals in
    Float.round (f *. m) /. m

(* Round an oklch/lch hue to 3 decimals for the canonical (optimize) form and
   collapse every concrete angle unit to canonical bare degrees. A [var()] /
   [calc()] / [none] hue is left opaque. *)
let round_hue ~lossless (h : hue) : hue =
  let r f = round_color_axis ~lossless ~decimals:3 f in
  match h with
  | Unitless f -> Unitless (r f)
  | Angle (Deg f) -> Unitless (r f)
  | Angle a -> (
      match deg_of_hue (Angle a) with
      | Some f -> Unitless (r (normalize_hue f))
      | None -> h)
  | Hue_none | Var _ -> h

let canonical_hsl_hue ~lossless (h : hue) : hue =
  match h with
  | Angle (Deg f) -> Unitless f
  | Angle a -> (
      match deg_of_hue (Angle a) with
      | Some f ->
          Unitless (round_color_axis ~lossless ~decimals:3 (normalize_hue f))
      | None -> h)
  | Unitless _ | Hue_none | Var _ -> h

let canonical_hsl_hue_in_color ~lossless = function
  | Hsl r -> Hsl { r with h = canonical_hsl_hue ~lossless r.h }
  | Hwb r -> Hwb { r with h = canonical_hsl_hue ~lossless r.h }
  | color -> color

let canonical_color_lightness ~lossless ~pct_scale ~axis_max_decimals
    (l : percentage option) : percentage option =
  let fmt ~max_decimals f =
    let max_decimals = if lossless then 8 else max_decimals in
    Pp.string_of_float ~drop_leading_zero:true ~max_decimals
      (if lossless then f else Pp.round_sig 6 f)
  in
  let num_len f = String.length (fmt ~max_decimals:axis_max_decimals f) in
  let pct_len f =
    String.length (fmt ~max_decimals:(axis_max_decimals + 2) f) + 1
  in
  let num f : percentage =
    Num (round_color_axis ~lossless ~decimals:axis_max_decimals f)
  in
  let pct f : percentage =
    Pct (round_color_axis ~lossless ~decimals:(axis_max_decimals + 2) f)
  in
  match l with
  | Some (Pct f) ->
      (* Preserve the node's representation on an equal-length spelling. A
         trailing [%] can terminate the token before an unsigned following axis,
         so an authored [oklab(25% 20 50)] stays [oklab(25%20 50)]. *)
      if num_len (f *. pct_scale) < pct_len f then Some (num (f *. pct_scale))
      else Some (pct f)
  | Some (Num f) ->
      (* A computed lightness is numeric. Keeping [Num] on a tie avoids turning
         a colour-mix result such as [.54] into [54%]. *)
      if pct_len (f /. pct_scale) < num_len f then Some (pct (f /. pct_scale))
      else Some (num f)
  | other -> other

let normalize_static_modern_color ~exact_srgb ~lossless c =
  let hex_of_bytes r g b (a : alpha) =
    match alpha_value_byte a with
    | Some ab -> canonical_color_of_hex r g b ab
    | Option.None -> c
  in
  if lossless then
    match
      if exact_srgb then exact_srgb_function_channels c else Option.None
    with
    | Some (r, g, b, alpha) -> (
        match exact_alpha_value_byte alpha with
        | Some a -> canonical_color_of_hex r g b a
        | Option.None ->
            (* The channels are exact but the alpha is not a whole byte, so
               [rgb()] is as far as the fold goes: it is the same spelling the
               lossless path leaves an authored [rgb()] in. *)
            drop_full_alpha
              (Rgba
                 {
                   rgb = Channels { r = Int r; g = Int g; b = Int b };
                   a = alpha;
                 }))
    | Option.None -> drop_full_alpha c
  else
    match static_color_to_linear_srgb c with
    | Some (linear, alpha_f) -> (
        match Color_space.srgb_bytes_of_linear linear with
        | Some (r, g, b) ->
            let clamp01 v = Float.max 0.0 (Float.min 1.0 v) in
            let alpha_byte =
              Float.to_int (Float.round (clamp01 alpha_f *. 255.0))
            in
            let a : alpha =
              if alpha_byte = 255 then None
              else if alpha_byte = 0 then Num 0.0
              else Num (Float.of_int alpha_byte /. 255.0)
            in
            hex_of_bytes r g b a
        | None -> drop_full_alpha c)
    | None -> drop_full_alpha c

(* [canonical_color_of_hex] answers a name or the same bytes back, so a [Hex]
   that reaches its own canonical form is shared rather than rebuilt. *)
let normalize_hex_color c r g b a =
  match canonical_color_of_hex r g b a with Hex _ -> c | color -> color

let normalize_named_color c orig_name =
  (* Pick the shortest spelling: a named colour collapses to hex only when the
     SHORTENED hex is shorter than the name. [canonical_color_name] first folds
     aliases (grey -> gray) so the choice is made on the canonical spelling. *)
  let name = canonical_color_name orig_name in
  let name_str, hex = color_name_hex name in
  match rgba_of_hex hex with
  | Some (r, g, b, a)
    when String.length (hex_string_of_bytes r g b a) + 1
         <= String.length name_str ->
      canonical_color_of_hex r g b a
  | _ -> if name == orig_name then c else Named name

let normalize_srgb_mix color1 color2 ~p1 ~p2 =
  let srgb_operand color =
    match static_color_to_srgb_channels color with
    | Some (Some r, Some g, Some b, Some a) ->
        let unit byte = Float.of_int byte /. 255. in
        Some ((unit r, unit g, unit b), unit a)
    | Some _ -> None
    | None ->
        Option.map
          (fun (linear, alpha) -> (Color_space.rgb_of_linear_rgb linear, alpha))
          (static_color_to_linear_srgb color)
  in
  match (srgb_operand color1, srgb_operand color2, mix_weights p1 p2) with
  | Some (rgb1, alpha1), Some (rgb2, alpha2), Some (w1, w2, alpha_mult) -> (
      let (r, g, b), interp_alpha =
        premult_mix3 ~w1 ~w2 ~a1:alpha1 ~a2:alpha2 rgb1 rgb2
      in
      let alpha = interp_alpha *. alpha_mult in
      let linear = Color_space.linear_rgb_of_rgb (r, g, b) in
      match Color_space.srgb_bytes_of_linear linear with
      | Some (r, g, b) ->
          let clamp01 v = Float.max 0. (Float.min 1. v) in
          let a = Float.to_int (Float.round (clamp01 alpha *. 255.)) in
          Some (canonical_color_of_hex r g b a)
      | None ->
          (* CSS Color 4 sec. 10.1 leaves out-of-gamut sRGB coordinates
             unclamped, so retain them in [color()] notation. *)
          let round6 v = Float.round (v *. 1e6) /. 1e6 in
          Some
            (Color
               {
                 space = Srgb;
                 components = [ Num (round6 r); Num (round6 g); Num (round6 b) ];
                 alpha = alpha_of_unit_float alpha;
               }))
  | _ -> (
      (* The floating-point converter declines sRGB [none] channels. Keep the
         channel carry-over behaviour in the byte mixer for that case. *)
      match mix_srgb_bytes color1 color2 ~p1 ~p2 with
      | Some (r, g, b, a) -> Some (canonical_color_of_hex r g b a)
      | Option.None -> None)

let normalize_lab_family_mix effective color1 color2 ~p1 ~p2 =
  match mix_lab_family effective color1 color2 ~p1 ~p2 with
  | Some color -> Some color
  | None -> (
      match effective with
      | Some (Lab : color_space) -> mix_in_lab_space color1 color2 ~p1 ~p2
      | Some Oklab -> mix_in_oklab_space color1 color2 ~p1 ~p2
      | Some Lch -> mix_in_lch_space color1 color2 ~p1 ~p2 Default
      | Some Oklch -> mix_in_oklch_space color1 color2 ~p1 ~p2 Default
      | _ -> None)

(* Round the surviving coefficients of a lab-family colour to its canonical
   decimal budget. Applied only after the sRGB fold has run, so a colour that
   already collapsed to hex / named keeps its full-precision projection and only
   one that stayed in its own space (out of gamut) gets rounded for display. *)
let round_lab_family_axes ~lossless (c : color) : color =
  let r3 = Option.map (round_color_axis ~lossless ~decimals:3) in
  let r1 = Option.map (round_color_axis ~lossless ~decimals:1) in
  let ok_l =
    canonical_color_lightness ~lossless ~pct_scale:0.01 ~axis_max_decimals:3
  in
  let lab_l =
    canonical_color_lightness ~lossless ~pct_scale:1.0 ~axis_max_decimals:1
  in
  match c with
  | Oklch r ->
      Oklch { r with l = ok_l r.l; c = r3 r.c; h = round_hue ~lossless r.h }
  | Oklab r -> Oklab { r with l = ok_l r.l; a = r3 r.a; b = r3 r.b }
  | Lch r ->
      Lch { r with l = lab_l r.l; c = r1 r.c; h = round_hue ~lossless r.h }
  | Lab r -> Lab { r with l = lab_l r.l; a = r1 r.a; b = r1 r.b }
  | other -> other

(* CSS Color 4 sec. 4.4: "a missing component behaves as a zero value, in the
   appropriate unit for that component", and the spec names rendering the colour
   and converting it to another space among the purposes that holds for. The
   sRGB family already resolves its own [none] channels that way on the path
   through [static_color_to_srgb_bytes]; the Lab family reaches no fold at all
   while a channel is missing, so a converted achromatic OKLab and the hex it
   stands for never meet. This is that half, written as its own step so the
   folds below read one shape of colour.

   Sec. 13.3 is why it is not unconditional: interpolation gives a missing
   component the other colour's analogous component instead of a zero, so the
   two spellings are two different ramps there. *)
let zero_missing_components (c : color) : color =
  let lightness (l : percentage option) : percentage option =
    match l with Option.None -> Some (Pct 0.) | l -> l
  in
  let axis (a : float option) =
    match a with Option.None -> Some 0. | a -> a
  in
  let hue (h : hue) : hue = match h with Hue_none -> Unitless 0. | h -> h in
  match c with
  | Lab r -> Lab { r with l = lightness r.l; a = axis r.a; b = axis r.b }
  | Oklab r -> Oklab { r with l = lightness r.l; a = axis r.a; b = axis r.b }
  | Lch r -> Lch { r with l = lightness r.l; c = axis r.c; h = hue r.h }
  | Oklch r -> Oklch { r with l = lightness r.l; c = axis r.c; h = hue r.h }
  | other -> other

(* AST-level color canonicalisation: the value-changing colour folds live here,
   producing a canonical [color] so [pp_color] stays a pure serialiser. The sRGB
   fold runs on the authored coefficients first; [round_lab_family_axes] then
   rounds only what survives in its own colour space. *)
let rec normalize_color ?(lossless = false) ?(exact_srgb = false)
    ?(resolve_missing = false) (c : color) : color =
  let c = if resolve_missing then zero_missing_components c else c in
  let normalize_color ?(lossless = lossless) =
    normalize_color ~lossless ~exact_srgb
  in
  let hex_of_byte_quad r g b ab = canonical_color_of_hex r g b ab in
  let static_fold () =
    round_lab_family_axes ~lossless
      (normalize_static_modern_color ~exact_srgb ~lossless c)
  in
  match c with
  | Oklab { l = Some l; a = None; b = None; alpha }
    when pct_is_zero l && alpha_is_zero alpha ->
      Transparent
  | Oklch { l = Some _; c = Some _; _ } -> static_fold ()
  | Oklab { l = Some _; a = Some _; b = Some _; _ } -> static_fold ()
  | Lch { l = Some _; c = Some _; _ } -> static_fold ()
  | Lab { l = Some _; a = Some _; b = Some _; _ } -> static_fold ()
  | Color _ -> normalize_static_modern_color ~exact_srgb ~lossless c
  | Hex { r; g; b; a } -> normalize_hex_color c r g b a
  (* The authored spelling is a pretty-printing detail the printer reads, not
     part of the colour, so the fold drops it: two spellings of one colour have
     to reach the same node or nothing downstream can see them as one value. *)
  | Authored_hex { r; g; b; a; _ } -> canonical_color_of_hex r g b a
  | Named orig_name -> normalize_named_color c orig_name
  | Rgb _ | Rgba _ | Hsl _ | Hwb _ | Transparent -> (
      let bytes =
        if lossless then exact_rgb_to_srgb_bytes c
        else static_color_to_srgb_bytes c
      in
      match bytes with
      | Some (r, g, b, a) -> hex_of_byte_quad r g b a
      | Option.None -> canonical_hsl_hue_in_color ~lossless (drop_full_alpha c))
  | Mix { in_space; hue; color1; percent1; color2; percent2 } ->
      normalize_mix_color ~lossless ~in_space ~hue ~color1 ~percent1 ~color2
        ~percent2
  | Light_dark (l, d) ->
      Light_dark (normalize_color ~lossless l, normalize_color ~lossless d)
  | Contrast_color inner -> Contrast_color (normalize_color ~lossless inner)
  | Relative_rgb (origin, tail) ->
      Relative_rgb (normalize_color ~lossless origin, tail)
  | Relative_color (name, origin, tail) ->
      Relative_color (name, normalize_color ~lossless origin, tail)
  | Var v ->
      (* A typed [var()] fallback / default is a colour, so canonicalise it the
         same way it would be if it stood alone. The opaque [Syntax_fallback] /
         [Var_fallback] forms are token streams, not typed colours, and stay
         untouched. *)
      let fallback =
        match v.fallback with
        | Fallback c -> Fallback (normalize_color ~lossless c)
        | (Empty | Empty2 | None | Syntax_fallback _ | Var_fallback _) as other
          ->
            other
      in
      Var
        {
          v with
          fallback;
          default = Option.map (normalize_color ~lossless) v.default;
        }
  | _ -> c

and normalize_mix_color ~lossless ~in_space ~hue ~color1 ~percent1 ~color2
    ~percent2 =
  let keep () =
    (* CSS Color 5 sec. 3 / CSS Color 4 sec. 13.4: [shorter] is the default hue
       and drops, and percentages that restate the [100% - other] default drop
       too. The interpolation method itself is required syntax, so a written [in
       oklab] stays - dropping it yields [color-mix(<color>, <color>)], which
       every browser rejects. *)
    let hue = match hue with Shorter -> Default | h -> h in
    let pct (p : percentage option) =
      match p with Some (Pct f) -> Some f | _ -> None
    in
    let percent1, percent2 =
      match (pct percent1, pct percent2) with
      | Some a, Some b when Float.abs (a +. b -. 100.) < 0.0001 ->
          let none : percentage option = None in
          if Float.abs (a -. 50.) < 0.0001 then (none, none)
          else (percent1, none)
      | _ -> (percent1, percent2)
    in
    Mix
      {
        in_space;
        hue;
        color1 = normalize_color ~lossless color1;
        percent1;
        color2 = normalize_color ~lossless color2;
        percent2;
      }
  in
  let folded : color option =
    if lossless then None
    else
      match (in_space, hue, color_mix_percentages percent1 percent2) with
      | Some Srgb, Default, Some (p1, p2) ->
          normalize_srgb_mix color1 color2 ~p1 ~p2
      | ( Some
            (( Srgb_linear | Display_p3 | A98_rgb | Prophoto_rgb | Rec2020 | Xyz
             | Xyz_d50 | Xyz_d65 ) as space),
          Default,
          Some (p1, p2) ) ->
          mix_in_rectangular_space space color1 color2 ~p1 ~p2
      | ((Some (Lab | Oklab | Lch | Oklch) | None) as sp), Default, Some (p1, p2)
        ->
          (* CSS Color 5 sec. 3: [in oklab] is the default interpolation space,
             so an absent space folds the same as [Some Oklab]. *)
          let effective =
            match sp with Some _ -> sp | None -> Some (Oklab : color_space)
          in
          normalize_lab_family_mix effective color1 color2 ~p1 ~p2
      | _ -> None
  in
  match folded with
  | Some color -> normalize_color ~lossless color
  | None -> keep ()

(** Read hue_interpolation *)
let read_hue_interpolation t : hue_interpolation =
  Cursor.ws t;
  let hue =
    Cursor.enum "hue-interpolation"
      [
        ("shorter", Shorter);
        ("longer", Longer);
        ("increasing", Increasing);
        ("decreasing", Decreasing);
        ("specified", Specified);
        ("default", Default);
      ]
      t
  in
  (match hue with
  | Default -> ()
  | _ ->
      Cursor.ws t;
      ignore (Cursor.option (Cursor.expect_string "hue") t));
  hue

(** Read calc_op *)
let read_calc_op t : calc_op =
  Cursor.ws t;
  match Cursor.peek_delim t with
  | Some '+' ->
      Cursor.skip t;
      Add
  | Some '-' ->
      Cursor.skip t;
      Sub
  | Some '*' ->
      Cursor.skip t;
      Mul
  | Some '/' ->
      Cursor.skip t;
      Div
  | _ -> Cursor.err_invalid t "calc operator"

(** Read component value *)
let rec read_component t : component =
  Cursor.ws t;
  if Cursor.looking_at t "var(" then Var (read_var read_component t)
  else if Cursor.looking_at_calc t then
    Calc (read_calc ~result_type:`Number_or_value read_component t)
  else
    let n, unit = Cursor.number_with_unit t in
    match unit with
    | Some "%" ->
        (* Clamp percentage to 0-100 range per CSS spec *)
        Pct (max 0. (min 100. n))
    | _ ->
        (* Clamp numeric component to 0-255 range for RGB values per CSS spec *)
        Num (max 0. (min 255. n))

(* Var helper functions *)
let var_name (v : 'a var) = v.name
let var_layer (v : 'a var) = v.layer
let var_meta (v : 'a var) = v.meta

let with_fallback (v : 'a var) fallback_value : 'a var =
  { v with fallback = Fallback fallback_value }

(** Read padding shorthand property (1-4 values) *)
let read_padding_shorthand t : length list =
  (* CSS padding accepts 1-4 space-separated non-negative values *)
  (* CSS-wide keywords must be the only value when present *)
  Cursor.enum "padding"
    [
      ("inherit", [ Inherit ]);
      ("initial", [ Initial ]);
      ("unset", [ Unset ]);
      ("revert", [ Revert ]);
      ("revert-layer", [ Revert_layer ]);
    ]
    ~default:(fun t ->
      Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:4
        (read_non_negative_length ~with_keywords:false)
        t)
    t

(** Read margin shorthand property (1-4 values). CSS Box 4 (ED) sec. 3.2 gives
    [margin] the value [<'margin-top'>{1,4}] and sec. 3.1 gives [margin-top] the
    value [<length-percentage> | auto], so [auto] is a component of the box and
    stands in any slot beside any length. Only the CSS-wide keywords of CSS
    Cascade 5 sec. 6 own the whole value. *)
let read_margin_shorthand t : length list =
  let rec read_margin_component t : length =
    if Cursor.looking_at_func "var" t then
      Var (read_var read_margin_component t)
    else
      Cursor.enum "margin component"
        [ ("auto", (Auto : length)) ]
        ~default:(read_length ~with_keywords:false)
        t
  in
  Cursor.enum "margin"
    [
      ("inherit", [ (Inherit : length) ]);
      ("initial", [ Initial ]);
      ("unset", [ Unset ]);
      ("revert", [ Revert ]);
      ("revert-layer", [ Revert_layer ]);
    ]
    ~default:(fun t ->
      Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:4 read_margin_component t)
    t
