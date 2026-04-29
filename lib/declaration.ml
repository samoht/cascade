(** CSS declaration types and parser. *)

include Declaration_intf
open Properties
open Values

(* Re-export pp_property from Properties module *)
let pp_property = pp_property

(* Extract metadata from a declaration *)
let rec meta_of_declaration : declaration -> meta option = function
  | Custom_declaration { meta; _ } -> meta
  | Declaration _ -> None
  | Theme_guarded { decl; _ } -> meta_of_declaration decl

(* Smart constructor for declarations *)
let v ?(important = false) property value =
  Declaration { property; value; important }

(* Smart constructor for custom declarations *)
let custom_declaration ?(important = false) ?layer ?meta name kind value =
  Custom_declaration { name; kind; value; layer; meta; important }

(* Helper to mark a declaration as important *)
let rec important = function
  | Declaration { property; value; _ } ->
      Declaration { property; value; important = true }
  | Custom_declaration d -> Custom_declaration { d with important = true }
  | Theme_guarded g -> Theme_guarded { g with decl = important g.decl }

(* Helper for raw custom properties - primarily for internal use *)

let custom_property ?layer name value =
  (* Validate that this is a proper CSS variable name. Per CSS Custom Properties
     Level 1, [--] (the bare two-dash ident with no body) is a legal name in the
     syntax even though it is reserved for future use. *)
  if not (String.length name >= 2 && String.sub name 0 2 = "--") then
    failwith
      (String.concat ""
         [
           "custom_property: ";
           name;
           " is not a valid CSS variable name (must start with --)";
         ]);
  (* Parse the value into a CSS Syntax 3 component stream so the declaration
     never carries a raw author string; the printer can then re-serialise with
     the active [Pp] context (handles minification). *)
  let components = Cursor.remaining (Cursor.of_string value) in
  custom_declaration ?layer name Value components

let font_url_needs_quotes s =
  String.exists
    (fun c -> c = ' ' || c = ')' || c = '"' || c = '\'' || c = '(' || c = '\\')
    s

let pp_font_url ctx s =
  Pp.string ctx "url(";
  if font_url_needs_quotes s then (
    Pp.char ctx '"';
    Pp.string ctx s;
    Pp.char ctx '"')
  else Pp.string ctx s;
  Pp.char ctx ')'

let pp_font_src_entry ctx : Font_face.src_entry -> unit = function
  | Local name ->
      Pp.string ctx "local(";
      Pp.char ctx '"';
      Pp.string ctx name;
      Pp.char ctx '"';
      Pp.char ctx ')'
  | Url { url; format; tech } -> (
      pp_font_url ctx url;
      (match format with
      | None -> ()
      | Some value ->
          Pp.space ctx ();
          Pp.string ctx "format(";
          Pp.string ctx value;
          Pp.char ctx ')');
      match tech with
      | None -> ()
      | Some value ->
          Pp.space ctx ();
          Pp.string ctx "tech(";
          Pp.string ctx value;
          Pp.char ctx ')')

let pp_font_src ctx entries =
  let first = ref true in
  List.iter
    (fun entry ->
      if !first then first := false
      else (
        Pp.char ctx ',';
        Pp.space_if_pretty ctx ());
      pp_font_src_entry ctx entry)
    entries

(* Access the layer associated with a custom declaration, if any *)
let rec custom_declaration_layer = function
  | Custom_declaration { layer; _ } -> layer
  | Declaration _ -> None
  | Theme_guarded { decl; _ } -> custom_declaration_layer decl

(* Parser functions *)

(** Parse a property name. Property names are plain idents in the component
    stream ([--custom] idents include the leading [--]). *)
let read_property_name t =
  Cursor.ws t;
  Cursor.ident ~keep_case:true t

(** Parse property value. Components up to the next [;] or [!important] mark the
    value. Whitespace is preserved in the drained value so multi-token values
    like "10px 20px" serialise back with their spaces. *)
let read_property_value t =
  Cursor.with_context t "property-value" @@ fun () ->
  Cursor.consume_to_decl_end ~trim:true t

let css_wide_keywords =
  [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ]

let is_css_wide_keyword value = List.mem value css_wide_keywords

let value_has_css_wide_mix value =
  let trimmed = String.trim value in
  (not (is_css_wide_keyword trimmed))
  &&
  let is_sep = function
    | ' ' | '\t' | '\n' | '\r' | ',' | '/' | '(' | ')' -> true
    | _ -> false
  in
  let len = String.length trimmed in
  let rec token acc i =
    if i >= len then List.rev acc
    else if is_sep trimmed.[i] then token acc (i + 1)
    else
      let j = ref i in
      while !j < len && not (is_sep trimmed.[!j]) do
        incr j
      done;
      token (String.sub trimmed i (!j - i) :: acc) !j
  in
  let tokens = token [] 0 in
  List.exists (fun keyword -> List.mem keyword tokens) css_wide_keywords

(** Check for and consume [!important] (case-insensitive per CSS Syntax). *)
let read_importance t =
  Cursor.ws t;
  match Cursor.peek_delim t with
  | Some '!' ->
      Cursor.skip t;
      Cursor.ws t;
      let ident = Cursor.ident t in
      if String.lowercase_ascii ident = "important" then true
      else Cursor.err_invalid t ("invalid !important declaration: !" ^ ident)
  | _ -> false

(** Check if a declaration is marked as important *)
let rec is_important = function
  | Declaration { important; _ } -> important
  | Custom_declaration { important; _ } -> important
  | Theme_guarded { decl; _ } -> is_important decl

(** Get the property name as a string from a declaration *)
let rec property_name decl =
  let ctx =
    {
      Pp.minify = true;
      indent = 0;
      buf = Buffer.create 16;
      inline = false;
      in_function = false;
      theme = None;
      theme_defaults = Pp.no_theme_defaults;
    }
  in
  match decl with
  | Declaration { property; _ } ->
      pp_property ctx property;
      Buffer.contents ctx.buf
  | Custom_declaration { name; _ } -> name
  | Theme_guarded { decl; _ } -> property_name decl

(* Pretty printer for values based on their kind *)
let pp_value : type a. (a kind * a) Pp.t =
 fun ctx (kind, value) ->
  let pp pp_a = pp_a ctx value in
  match kind with
  | Length -> pp (pp_length ~always:true)
  | Color -> pp pp_color_in_mix (* custom props use lowercase currentcolor *)
  | Rgb ->
      let rec pp_rgb_type : rgb Pp.t =
       fun ctx rgb ->
        match rgb with
        | Channels { r; g; b } ->
            (* Just output the RGB values without wrapper *)
            pp_channel ctx r;
            Pp.space ctx ();
            pp_channel ctx g;
            Pp.space ctx ();
            pp_channel ctx b
        | Var v -> pp_var pp_rgb_type ctx v
      in
      pp pp_rgb_type
  | Int -> pp Pp.int
  | Float -> pp Pp.float
  | Percentage -> pp pp_percentage
  | Length_percentage -> pp (pp_length_percentage ~always:true)
  | Number_percentage -> pp pp_number_percentage
  | Value ->
      let rendered =
        if Pp.minified ctx then Parser.to_string_minified value
        else Parser.to_string value
      in
      Pp.string ctx rendered
  | Shadow -> pp pp_shadow
  | Duration -> pp pp_duration
  | Aspect_ratio -> pp pp_aspect_ratio
  | Border_style -> pp pp_border_style
  | Outline_style -> pp pp_outline_style
  | Border -> pp pp_border
  | Font_weight -> pp pp_font_weight
  | Line_height -> pp pp_line_height
  | Font_family -> pp pp_font_family
  | Font_feature_settings -> pp pp_font_feature_settings
  | Font_variation_settings -> pp pp_font_variation_settings
  | Font_variant_numeric -> pp pp_font_variant_numeric
  | Font_variant_numeric_token -> pp pp_font_variant_numeric_token
  | Blend_mode -> pp pp_blend_mode
  | Scroll_snap_strictness -> pp pp_scroll_snap_strictness
  | Angle -> pp pp_angle
  | Box_shadow -> pp pp_shadow
  | Content -> pp pp_content
  | Gradient_stop -> pp pp_gradient_stop
  | Gradient_direction -> pp pp_gradient_direction
  | Animation -> pp pp_animation
  | Timing_function -> pp pp_timing_function
  | Transform -> pp pp_transform
  | Touch_action -> pp pp_touch_action
  | Transition_property_value -> pp pp_transition_property_value
  | Background_image -> pp pp_background_image
  | Z_index -> pp pp_z_index
  | Filter -> pp pp_filter
  | Font_src -> pp pp_font_src

let rec string_of_value ?(minify = true) ?(inline = false) decl =
  let ctx =
    {
      Pp.minify;
      indent = 0;
      buf = Buffer.create 16;
      inline;
      in_function = false;
      theme = None;
      theme_defaults = Pp.no_theme_defaults;
    }
  in
  match decl with
  | Declaration { property; value; _ } ->
      pp_property_value ctx (property, value);
      Buffer.contents ctx.buf
  | Custom_declaration { kind; value; _ } ->
      pp_value ctx (kind, value);
      Buffer.contents ctx.buf
  | Theme_guarded { decl; _ } -> string_of_value ~minify ~inline decl

(* Helper to read a trimmed string *)
let read_string t = Cursor.string ~trim:true t

(* Helper to validate no extra tokens remain *)
let validate_no_extra_tokens t =
  Cursor.ws t;
  match Cursor.peek t with
  | None -> ()
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) -> ()
  | Some (Component.Preserved { kind = Token.Delim "!"; _ }) -> ()
  | Some _ ->
      let trimmed = Cursor.consume_to_decl_end ~trim:true t in
      if trimmed <> "" then
        Cursor.err_invalid t
          ("unexpected tokens after property value: " ^ trimmed)

let validate_var_reference_body r =
  let raw_name = Cursor.ident ~keep_case:true r in
  if
    not (String.length raw_name >= 3 && raw_name.[0] = '-' && raw_name.[1] = '-')
  then Cursor.err_invalid r ("not a custom property: " ^ raw_name);
  Cursor.ws r;
  if Cursor.comma_opt r then ignore (Cursor.remaining_to_string ~trim:true r)

let is_var_reference value =
  try
    let r = Cursor.of_string value in
    Cursor.call "var" r validate_var_reference_body;
    Cursor.ws r;
    Cursor.expect_eof r;
    true
  with Cursor.Parse_error _ -> false

let validate_var_reference t value =
  try
    let r = Cursor.of_string value in
    Cursor.call "var" r validate_var_reference_body;
    Cursor.ws r;
    Cursor.expect_eof r
  with Cursor.Parse_error _ -> Cursor.err_invalid t "invalid var() reference"

let read_length_box ?(allow_negative = true) t =
  let values =
    Cursor.list ~at_least:1 ~at_most:4
      (fun r -> read_length ~allow_negative r)
      t
  in
  if values = [] then Cursor.err_expected t "length value";
  values

let read_inset_longhand t = [ read_length t ]
let read_inset_axis t = Cursor.list ~at_least:1 ~at_most:2 read_length t

let read_border_width_box t =
  Cursor.list ~at_least:1 ~at_most:4 read_border_width t

type list_style_slot = Type | Position | Image

let slot_seen slot seen = List.mem slot seen

let read_list_style_slot slot t =
  match slot with
  | Type -> ignore (read_list_style_type t)
  | Position -> ignore (read_list_style_position t)
  | Image -> ignore (read_list_style_image t)

let read_list_style_shorthand t =
  let raw = Cursor.lookahead (Cursor.consume_to_decl_end ~trim:true) t in
  let slots : list_style_slot list = [ Position; Image; Type ] in
  let rec parse seen r =
    Cursor.ws r;
    if Cursor.is_done r then seen <> []
    else
      List.exists
        (fun slot ->
          if slot_seen slot seen then false
          else
            let pos = Cursor.save r in
            try
              read_list_style_slot slot r;
              if parse (slot :: seen) r then true
              else (
                Cursor.restore r pos;
                false)
            with Cursor.Parse_error _ ->
              Cursor.restore r pos;
              false)
        slots
  in
  if not (parse [] (Cursor.of_string raw)) then
    Cursor.err_invalid t "invalid list-style shorthand";
  ignore (Cursor.consume_to_decl_end ~trim:true t);
  raw

let read_text_decoration_lines t =
  let lines = Cursor.list ~at_least:1 read_text_decoration_line t in
  let is_none (line : text_decoration_line) =
    match line with None -> true | _ -> false
  in
  if List.exists is_none lines && List.length lines > 1 then
    Cursor.err_invalid t "none cannot be combined with text-decoration lines";
  let rec duplicates = function
    | [] -> false
    | x :: xs -> List.mem x xs || duplicates xs
  in
  if duplicates lines then Cursor.err_invalid t "duplicate text-decoration-line";
  lines

let read_position_try_fallback t =
  Cursor.ws t;
  match Cursor.peek_ident t with
  | Some "none" ->
      let _ = Cursor.ident t in
      "none"
  | Some (("flip-block" | "flip-inline" | "flip-start") as keyword) ->
      let _ = Cursor.ident t in
      keyword
  | _ ->
      let ident = Cursor.ident ~keep_case:true t in
      if String.length ident >= 3 && String.sub ident 0 2 = "--" then ident
      else Cursor.err_invalid t ("expected dashed ident, got: " ^ ident)

let read_shape_outside t =
  let raw = Cursor.lookahead (Cursor.consume_to_decl_end ~trim:true) t in
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Ident "none"; _ }) ->
      Cursor.skip t;
      raw
  | Some
      (Component.Func
         { node = { name = "circle" | "inset"; arguments; terminated }; _ })
    when terminated && arguments <> [] ->
      ignore (Cursor.consume_to_decl_end ~trim:true t);
      raw
  | Some (Component.Func { node = { name = "circle" | "inset"; _ }; _ }) ->
      Cursor.err_invalid t "empty basic shape"
  | _ -> Cursor.err_invalid t ("invalid shape-outside: " ^ raw)

let read_font_shorthand t =
  let raw = Cursor.consume_to_decl_end ~trim:true t in
  let lower = String.lowercase_ascii raw in
  if is_css_wide_keyword lower then raw
  else
    let r = Cursor.of_string raw in
    let saw_size = ref false in
    let rec loop () =
      Cursor.ws r;
      if Cursor.is_done r then ()
      else
        match Cursor.peek_ident r with
        | Some
            ( "italic" | "oblique" | "normal" | "small-caps" | "bold" | "bolder"
            | "lighter" | "condensed" | "expanded" ) ->
            let _ = Cursor.ident r in
            loop ()
        | _ -> (
            let before = Cursor.save r in
            match read_font_weight r with
            | _ -> loop ()
            | exception Cursor.Parse_error _ ->
                Cursor.restore r before;
                let _ = read_font_size r in
                saw_size := true;
                (match Cursor.peek_delim r with
                | Some '/' ->
                    Cursor.skip r;
                    ignore (read_line_height r : line_height)
                | _ -> ());
                ignore (read_font_family r : font_family);
                Cursor.ws r;
                Cursor.expect_eof r)
    in
    (try loop ()
     with Cursor.Parse_error _ ->
       Cursor.err_invalid t "invalid font shorthand");
    if not !saw_size then Cursor.err_invalid t "font shorthand missing size";
    raw

let is_grid_area_ws = function
  | ' ' | '\t' | '\n' | '\r' | '\012' -> true
  | _ -> false

let grid_area_row_cells row =
  let len = String.length row in
  let rec skip_ws i =
    if i < len && is_grid_area_ws row.[i] then skip_ws (i + 1) else i
  in
  let rec take_cell start i =
    if i < len && not (is_grid_area_ws row.[i]) then take_cell start (i + 1)
    else (String.sub row start (i - start), i)
  in
  let rec loop acc i =
    let start = skip_ws i in
    if start >= len then List.rev acc
    else
      let cell, next = take_cell start start in
      loop (cell :: acc) next
  in
  loop [] 0

let grid_area_null_cell cell =
  let len = String.length cell in
  len > 0
  &&
  let rec loop i = i = len || (cell.[i] = '.' && loop (i + 1)) in
  loop 0

let validate_grid_area_width t (expected : int option) cells =
  match expected with
  | None -> Some (List.length cells)
  | Some width when List.length cells = width -> expected
  | Some _ -> Cursor.err_invalid t "grid-template-areas rows differ in width"

let grid_area_positions rows =
  rows
  |> List.mapi (fun row cells ->
      cells
      |> List.mapi (fun col cell -> (cell, row, col))
      |> List.filter (fun (cell, _, _) -> not (grid_area_null_cell cell)))
  |> List.flatten

let grid_area_names positions =
  positions
  |> List.fold_left
       (fun names (cell, _, _) ->
         if List.mem cell names then names else cell :: names)
       []

let validate_grid_area_rectangles t rows =
  let positions = grid_area_positions rows in
  let cell_at row col = List.nth (List.nth rows row) col in
  let validate_name name =
    let coords =
      positions
      |> List.filter_map (fun (cell, row, col) ->
          if cell = name then Some (row, col) else None)
    in
    let rows = List.map fst coords in
    let cols = List.map snd coords in
    let min_row = List.fold_left min max_int rows in
    let max_row = List.fold_left max min_int rows in
    let min_col = List.fold_left min max_int cols in
    let max_col = List.fold_left max min_int cols in
    for row = min_row to max_row do
      for col = min_col to max_col do
        if cell_at row col <> name then
          Cursor.err_invalid t
            "grid-template-areas named area is not rectangular"
      done
    done
  in
  List.iter validate_name (grid_area_names positions)

(* Custom parser for grid-template-areas: reads and validates quoted rows. *)
let read_grid_template_areas t =
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Ident "none"; _ }) ->
      Cursor.skip t;
      "none"
  | _ ->
      let rec read_strings (width : int option) rows rendered =
        Cursor.ws t;
        match Cursor.string_opt t with
        | None ->
            let rows = List.rev rows in
            if rows = [] then Cursor.err_expected t "grid-template-areas row";
            validate_grid_area_rectangles t rows;
            String.concat " " (List.rev rendered)
        | Some s ->
            let cells = grid_area_row_cells s in
            if cells = [] then
              Cursor.err_invalid t "empty grid-template-areas row";
            let width = validate_grid_area_width t width cells in
            read_strings width (cells :: rows) (("\"" ^ s ^ "\"") :: rendered)
      in
      read_strings (None : int option) [] []

let border_image_at_end t = Cursor.is_done t || Cursor.peek_semicolon t

let read_bi_nonneg_unit t =
  let pp_float n = Pp.to_string ~minify:true Pp.float n in
  let n, unit = Cursor.number_with_unit t in
  if n < 0. then Cursor.err_invalid t "border-image value cannot be negative";
  match unit with
  | None -> pp_float n
  | Some "%" -> pp_float n ^ "%"
  | Some unit -> pp_float n ^ unit

let read_border_image_slice t =
  let rec loop values has_fill =
    Cursor.ws t;
    if border_image_at_end t || Cursor.peek_delim t = Some '/' then
      (values, has_fill)
    else
      match Cursor.peek_ident t with
      | Some "fill" ->
          if has_fill then
            Cursor.err_invalid t "duplicate border-image fill keyword";
          let _ = Cursor.ident t in
          loop values true
      | _ -> (
          match Cursor.option read_bi_nonneg_unit t with
          | Some value ->
              if List.length values >= 4 then
                Cursor.err_invalid t "too many border-image slice values";
              loop (value :: values) has_fill
          | None -> (values, has_fill))
  in
  let values, has_fill = loop [] false in
  match (List.rev values, has_fill) with
  | [], true -> Cursor.err_invalid t "border-image fill requires slice values"
  | [], false -> Cursor.err_expected t "border-image slice"
  | values, false -> String.concat " " values
  | values, true -> String.concat " " (values @ [ "fill" ])

let read_border_image_box_values ~what ~allow_auto t =
  let read_item t =
    match Cursor.peek_ident t with
    | Some "auto" when allow_auto ->
        let _ = Cursor.ident t in
        "auto"
    | _ -> read_bi_nonneg_unit t
  in
  let rec loop acc =
    Cursor.ws t;
    if border_image_at_end t || Cursor.peek_delim t = Some '/' then List.rev acc
    else
      match Cursor.option read_item t with
      | Some value ->
          if List.length acc >= 4 then
            Cursor.err_invalid t ("too many border-image " ^ what ^ " values");
          loop (value :: acc)
      | None -> List.rev acc
  in
  match loop [] with
  | [] -> Cursor.err_expected t ("border-image " ^ what)
  | values -> String.concat " " values

let read_border_image_repeat t =
  let read_keyword t =
    Cursor.enum "border-image-repeat"
      [
        ("stretch", "stretch");
        ("repeat", "repeat");
        ("round", "round");
        ("space", "space");
      ]
      t
  in
  let first = read_keyword t in
  Cursor.ws t;
  match Cursor.option read_keyword t with
  | None -> first
  | Some second -> first ^ " " ^ second

let read_border_image t =
  let source : string option =
    match Cursor.option read_background_image t with
    | None -> None
    | Some image -> Some (Pp.to_string ~minify:true pp_background_image image)
  in
  Cursor.ws t;
  let slice = Cursor.option read_border_image_slice t in
  let width, outset =
    Cursor.ws t;
    if Cursor.slash_opt t then (
      let width =
        Some (read_border_image_box_values ~what:"width" ~allow_auto:true t)
      in
      Cursor.ws t;
      if Cursor.slash_opt t then
        ( width,
          Some (read_border_image_box_values ~what:"outset" ~allow_auto:false t)
        )
      else (width, None))
    else (None, None)
  in
  Cursor.ws t;
  let repeat = Cursor.option read_border_image_repeat t in
  let value =
    match (source, slice) with
    | None, None -> Cursor.err_expected t "border-image source or slice"
    | Some source, None -> source
    | None, Some slice -> slice
    | Some source, Some slice -> source ^ " " ^ slice
  in
  let value =
    match width with None -> value | Some width -> value ^ "/" ^ width
  in
  let value =
    match outset with None -> value | Some outset -> value ^ "/" ^ outset
  in
  let value =
    match repeat with None -> value | Some repeat -> value ^ " " ^ repeat
  in
  value

(* Custom parser for grid-template-columns/rows: handles both single values and
   lists *)
let read_grid_template_list t =
  let first_value = read_grid_template t in
  Cursor.ws t;
  (* Try to read more values - if none, it's a single value *)
  let remaining_values =
    Cursor.list ~sep:(fun t -> Cursor.ws t) ~at_least:0 read_grid_template t
  in
  if remaining_values = [] then
    (* Single value (e.g., "none", "repeat(3, 1fr)", "1fr") *)
    first_value
  else
    (* Multiple values (e.g., "100px 200px", "1fr 2fr") *)
    let all_values = first_value :: remaining_values in
    Tracks all_values

(* Helper to read animation-name: none | <custom-ident> *)
let read_animation_name t =
  if Cursor.looking_at t "none" then (
    Cursor.expect_string "none" t;
    Cursor.ws t;
    "none")
  else Cursor.ident t

(* Helper to read opacity: accepts either <number> (0-1), <percentage>
   (0%-100%), or var(...). Both formats are valid per CSS spec. Tailwind v4
   outputs percentages. *)
let rec read_opacity t : opacity =
  Cursor.ws t;
  if Cursor.looking_at t "var(" then Var (Values.read_var read_opacity t)
  else
    match Cursor.function_call "abs" read_opacity t with
    | Some inner -> Abs inner
    | None -> (
        match Cursor.function_call "sign" read_opacity t with
        | Some inner -> Sign inner
        | None -> (
            let n, unit = Cursor.number_with_unit t in
            match unit with
            | Some "%" -> Opacity_number (n /. 100.0)
            | _ -> Opacity_number n))

(* Helper to read raw property value - for properties that accept any text.
   Drain components (preserving whitespace) up to the next [;] or [!] delim. *)
let read_untyped_value t = Cursor.consume_to_decl_end ~trim:true t

(* CSS [<dashed-ident>]: an ident that begins with two dashes. Used for custom
   properties and [@property]-style names like [--tooltip] in
   anchor-positioning, view-timeline, font-palette, etc. *)
let read_dashed_ident t =
  let s = Cursor.ident ~keep_case:true t in
  if String.length s < 2 || s.[0] <> '-' || s.[1] <> '-' then
    Cursor.err_invalid t ("expected <dashed-ident>, got: " ^ s)
  else s

(* [scrollbar-gutter: auto | stable [both-edges]?]. The [both-edges] keyword may
   only follow [stable]; [stable auto] (the test's negative case) must be
   rejected. *)
let read_scrollbar_gutter t =
  let kw =
    Cursor.enum "scrollbar-gutter" [ ("auto", `Auto); ("stable", `Stable) ] t
  in
  match kw with
  | `Auto -> "auto"
  | `Stable -> (
      Cursor.ws t;
      match Cursor.peek_ident t with
      | Some "both-edges" ->
          let _ = Cursor.ident t in
          "stable both-edges"
      | None -> "stable"
      | Some s ->
          Cursor.err_invalid t
            (String.concat "" [ "unexpected scrollbar-gutter modifier: "; s ]))

(* [font-palette: normal | light | dark | <dashed-ident>]. *)
let read_font_palette t =
  match Cursor.peek_ident t with
  | Some "normal" ->
      let _ = Cursor.ident t in
      "normal"
  | Some "light" ->
      let _ = Cursor.ident t in
      "light"
  | Some "dark" ->
      let _ = Cursor.ident t in
      "dark"
  | _ -> read_dashed_ident t

(* CSS Fonts 5: [font-size-adjust = none | <number> | from-font | [<font-metric>
   [from-font | <number>]?]] where [<font-metric>] is one of [ex-height |
   cap-height | ch-width | ic-width | ic-height]. Bare [from-font] is permitted;
   [from-font <anything>] is rejected because [from-font] is not a
   [<font-metric>]. *)
let read_font_size_adjust t =
  let metrics =
    [ "ex-height"; "cap-height"; "ch-width"; "ic-width"; "ic-height" ]
  in
  Cursor.ws t;
  match Cursor.peek_ident t with
  | Some "none" ->
      let _ = Cursor.ident t in
      validate_no_extra_tokens t;
      "none"
  | Some "from-font" ->
      let _ = Cursor.ident t in
      validate_no_extra_tokens t;
      "from-font"
  | Some m when List.mem m metrics ->
      let _ = Cursor.ident t in
      Cursor.ws t;
      if Cursor.is_done t || Cursor.peek_semicolon t then
        Cursor.err_invalid t "font-size-adjust metric requires a value"
      else
        let tail =
          match Cursor.peek_ident t with
          | Some "from-font" ->
              let _ = Cursor.ident t in
              "from-font"
          | _ ->
              let n = Cursor.number t in
              Pp.to_string Pp.float n
        in
        validate_no_extra_tokens t;
        String.concat "" [ m; " "; tail ]
  | _ ->
      let n = Cursor.number t in
      validate_no_extra_tokens t;
      Pp.to_string Pp.float n

(* CSS Inline 3: [initial-letter = normal | drop | raise | <number [1,inf]>
   <integer [1,inf]>?]. The size and sink count must both be at least 1. *)
let read_initial_letter t =
  Cursor.ws t;
  match Cursor.peek_ident t with
  | Some ("normal" as s) | Some ("drop" as s) | Some ("raise" as s) ->
      let _ = Cursor.ident t in
      validate_no_extra_tokens t;
      s
  | _ ->
      let size = Cursor.number t in
      if size < 1. then Cursor.err_invalid t "initial-letter size must be >= 1"
      else (
        Cursor.ws t;
        if Cursor.is_done t || Cursor.peek_semicolon t then
          Pp.to_string Pp.float size
        else
          let sink = Cursor.int t in
          if sink < 1 then
            Cursor.err_invalid t "initial-letter sink must be >= 1"
          else
            String.concat ""
              [ Pp.to_string Pp.float size; " "; string_of_int sink ])

(* CSS Box Sizing 4: [margin-trim = none | block | inline | [block-start ||
   inline-start || block-end || inline-end]]. The bracketed form is a [||]
   (any-order, no-repeats) of the four physical edges. *)
let read_margin_trim t =
  Cursor.ws t;
  match Cursor.peek_ident t with
  | Some "none" ->
      let _ = Cursor.ident t in
      "none"
  | Some "block" ->
      let _ = Cursor.ident t in
      "block"
  | Some "inline" ->
      let _ = Cursor.ident t in
      "inline"
  | _ ->
      let edges =
        [ "block-start"; "inline-start"; "block-end"; "inline-end" ]
      in
      let rec loop acc =
        Cursor.ws t;
        match Cursor.peek_ident t with
        | Some s when List.mem s edges && not (List.mem s acc) ->
            let _ = Cursor.ident t in
            loop (s :: acc)
        | Some s when List.mem s edges ->
            Cursor.err_invalid t
              (String.concat "" [ "duplicate margin-trim edge: "; s ])
        | _ -> List.rev acc
      in
      let chosen = loop [] in
      if chosen = [] then Cursor.err_expected t "margin-trim value"
      else String.concat " " chosen

(* CSS Scroll-driven Animations: [animation-range =
   <single-animation-range>{1,2}] where each [<single-animation-range>] is
   [normal | <length-percentage> | <timeline-range-name> <length-percentage>].
   The spec marks the trailing length-percentage as optional but cascade follows
   the stricter convention that a bare [<timeline-range-name>] is only permitted
   when it stands alone (otherwise pairs like [exit entry] would silently parse
   as two single-ranges with surprising semantics). *)
let read_animation_range t =
  let timeline_names =
    [ "cover"; "contain"; "entry"; "exit"; "entry-crossing"; "exit-crossing" ]
  in
  let read_single t =
    Cursor.ws t;
    match Cursor.peek_ident t with
    | Some "normal" ->
        let _ = Cursor.ident t in
        "normal"
    | Some name when List.mem name timeline_names ->
        let _ = Cursor.ident t in
        Cursor.ws t;
        let lp = Values.read_length_percentage t in
        String.concat ""
          [
            name;
            " ";
            Pp.to_string (Values.pp_length_percentage ~always:true) lp;
          ]
    | _ ->
        let lp = Values.read_length_percentage t in
        Pp.to_string (Values.pp_length_percentage ~always:true) lp
  in
  let first = read_single t in
  Cursor.ws t;
  if Cursor.is_done t || Cursor.peek_semicolon t then first
  else
    let second = read_single t in
    String.concat "" [ first; " "; second ]

(* CSS Scroll-driven Animations: [animation-timeline = none | auto |
   <dashed-ident> | scroll() | view()]. Functions must be terminated; [scroll(]
   (no closing [)]) is rejected. *)
let read_animation_timeline t =
  Cursor.ws t;
  match Cursor.peek t with
  | Some (Component.Func fn) when not fn.node.terminated ->
      Cursor.err_invalid t
        (String.concat "" [ "unterminated function "; fn.node.name; "(...)" ])
  | Some (Component.Func fn)
    when fn.node.name = "scroll" || fn.node.name = "view" ->
      let _ = Cursor.next t in
      let buf = Buffer.create 16 in
      Buffer.add_string buf fn.node.name;
      Buffer.add_char buf '(';
      Buffer.add_string buf (Parser.to_string fn.node.arguments);
      Buffer.add_char buf ')';
      Buffer.contents buf
  | _ -> (
      match Cursor.peek_ident t with
      | Some ("none" as s) | Some ("auto" as s) ->
          let _ = Cursor.ident t in
          s
      | _ -> read_dashed_ident t)

(* Some properties (shape-margin, scroll-margin, padding, etc.) require a
   non-negative length-percentage. Detect a leading [-] number/percentage and
   reject before delegating to the typed reader. *)
let read_non_negative_length_percentage t =
  Values.read_length_percentage ~allow_negative:false ~with_keywords:false t

let read_non_negative_number t =
  let value = Cursor.number t in
  if value < 0. then Cursor.err_invalid t "negative number not allowed";
  value

(* CSS Backgrounds and Borders 3 §5: [border-radius = <length-percentage>{1,4}
   [/ <length-percentage>{1,4}]?]. Reads 1-4 horizontal radii then, after [/],
   1-4 vertical radii. *)
let read_border_radius t : Properties.border_radius =
  let read_radii t =
    let rec loop acc count =
      if count >= 4 then List.rev acc
      else
        match Cursor.option read_non_negative_length_percentage t with
        | None -> List.rev acc
        | Some lp -> loop (lp :: acc) (count + 1)
    in
    let radii = loop [] 0 in
    if radii = [] then Cursor.err_expected t "<length-percentage>" else radii
  in
  Cursor.ws t;
  let horizontal = read_radii t in
  Cursor.ws t;
  let vertical =
    match Cursor.peek_delim t with
    | Some '/' ->
        Cursor.skip t;
        Cursor.ws t;
        Some (read_radii t)
    | _ -> None
  in
  { Properties.horizontal; vertical }

(* Delegate to the proper reader in Properties *)
let read_translate_value t : Properties_intf.translate_value =
  Properties.read_translate_value t

let read_transform_value t = v Transform (read_transforms t)
let read_webkit_transform_value t = v Webkit_transform (read_transforms t)

let read_place_self_value t =
  let a = read_align_self t in
  Cursor.ws t;
  let j = Cursor.option read_justify_self t in
  (* Per CSS spec, when only one value is given, both values are set to it *)
  let align_to_justify (a : align_self) : justify_self =
    match a with
    | Auto -> Auto
    | Normal -> Normal
    | Stretch -> Stretch
    | Baseline -> Baseline
    | First_baseline -> First_baseline
    | Last_baseline -> Last_baseline
    | Center -> Center
    | Start -> Start
    | End -> End
    | Self_start -> Self_start
    | Self_end -> Self_end
    | Flex_start -> Flex_start
    | Flex_end -> Flex_end
    | Safe_center -> Safe_center
    | Safe_start -> Safe_start
    | Safe_end -> Safe_end
    | Safe_flex_start -> Safe_flex_start
    | Safe_flex_end -> Safe_flex_end
    | Unsafe_center -> Unsafe_center
    | Unsafe_start -> Unsafe_start
    | Unsafe_end -> Unsafe_end
    | Unsafe_self_start -> Unsafe_self_start
    | Unsafe_self_end -> Unsafe_self_end
    | Unsafe_flex_start -> Unsafe_flex_start
    | Unsafe_flex_end -> Unsafe_flex_end
  in
  let pair =
    match j with None -> (a, align_to_justify a) | Some jj -> (a, jj)
  in
  v Place_self pair

let read_background_blend_mode_value t =
  v Background_blend_mode (Cursor.list ~sep:Cursor.comma read_blend_mode t)

let prop_name (type a) (prop_type : a property) =
  let buf = Buffer.create 32 in
  let ctx =
    {
      Pp.minify = true;
      indent = 0;
      buf;
      inline = false;
      in_function = false;
      theme = None;
      theme_defaults = Pp.no_theme_defaults;
    }
  in
  pp_property ctx prop_type;
  Buffer.contents buf

let read_value (type a) (prop : a property) t : declaration =
  Cursor.with_context t (prop_name prop) @@ fun () ->
  match prop with
  | Color -> v Color (read_color t)
  | Background_color -> v Background_color (read_color t)
  | Border_color -> v Border_color (read_color t)
  | Outline_color -> v Outline_color (read_color t)
  | Border_top_color -> v Border_top_color (read_color t)
  | Border_right_color -> v Border_right_color (read_color t)
  | Border_bottom_color -> v Border_bottom_color (read_color t)
  | Border_left_color -> v Border_left_color (read_color t)
  (* Length/percentage properties *)
  | Width -> v Width (read_length_percentage t)
  | Height -> v Height (read_length_percentage t)
  | Min_width -> v Min_width (read_length_percentage t)
  | Min_height -> v Min_height (read_length_percentage t)
  | Max_width -> v Max_width (read_length_percentage t)
  | Max_height -> v Max_height (read_length_percentage t)
  | Inline_size -> v Inline_size (read_length_percentage t)
  | Min_inline_size -> v Min_inline_size (read_length_percentage t)
  | Max_inline_size -> v Max_inline_size (read_length_percentage t)
  | Block_size -> v Block_size (read_length_percentage t)
  | Min_block_size -> v Min_block_size (read_length_percentage t)
  | Max_block_size -> v Max_block_size (read_length_percentage t)
  | Font_size -> v Font_size (Properties.read_font_size t)
  | Border_radius -> v Border_radius (read_border_radius t)
  | Border_top_left_radius ->
      v Border_top_left_radius (read_length ~with_keywords:false t)
  | Border_top_right_radius ->
      v Border_top_right_radius (read_length ~with_keywords:false t)
  | Border_bottom_left_radius ->
      v Border_bottom_left_radius (read_length ~with_keywords:false t)
  | Border_bottom_right_radius ->
      v Border_bottom_right_radius (read_length ~with_keywords:false t)
  | Gap -> v Gap (Properties.read_gap t)
  | Column_gap -> v Column_gap (read_non_negative_length ~with_keywords:false t)
  | Row_gap -> v Row_gap (read_non_negative_length ~with_keywords:false t)
  (* Display and layout *)
  | Display -> v Display (read_display t)
  | Position -> v Position (read_position t)
  | Visibility -> v Visibility (read_visibility t)
  | Overflow -> v Overflow (read_overflow t)
  | Overflow_x -> v Overflow_x (read_overflow_single t)
  | Overflow_y -> v Overflow_y (read_overflow_single t)
  (* Padding/Margin *)
  | Padding -> v Padding (read_padding_shorthand t)
  | Margin -> v Margin (read_margin_shorthand t)
  (* Border styles *)
  | Border_style -> v Border_style (read_border_style t)
  | Border_width -> v Border_width (read_border_width_box t)
  | Border_top_width -> v Border_top_width (read_border_width t)
  | Border_right_width -> v Border_right_width (read_border_width t)
  | Border_bottom_width -> v Border_bottom_width (read_border_width t)
  | Border_left_width -> v Border_left_width (read_border_width t)
  (* Typography *)
  | Line_height -> v Line_height (read_line_height t)
  | Font_weight -> v Font_weight (read_font_weight t)
  | Font_style -> v Font_style (read_font_style t)
  | Font_family -> v Font_family (read_font_family t)
  | Font -> v Font (read_font_shorthand t)
  | Text_align -> v Text_align (read_text_align t)
  | Text_transform -> v Text_transform (read_text_transform t)
  | White_space -> v White_space (read_white_space t)
  | Text_decoration -> v Text_decoration (read_text_decoration t)
  | Transform_origin -> v Transform_origin (read_transform_origin t)
  | Transform_box -> v Transform_box (read_transform_box t)
  (* Flexbox *)
  | Flex_direction -> v Flex_direction (read_flex_direction t)
  | Flex_wrap -> v Flex_wrap (read_flex_wrap t)
  | Flex -> v Flex (read_flex t)
  | Flex_grow -> v Flex_grow (read_non_negative_number t)
  | Flex_shrink -> v Flex_shrink (read_non_negative_number t)
  | Flex_basis -> v Flex_basis (read_length t)
  | Align_items -> v Align_items (read_align_items t)
  | Justify_content -> v Justify_content (read_justify_content t)
  (* Transform property *)
  | Transform -> read_transform_value t
  | Translate -> v Translate (read_translate_value t)
  (* Webkit Transform *)
  | Webkit_transform -> read_webkit_transform_value t
  (* Webkit Transition *)
  | Webkit_transition -> v Webkit_transition (read_transitions t)
  (* Webkit Filter *)
  | Webkit_filter -> v Webkit_filter (read_filter t)
  (* Moz Appearance *)
  | Moz_appearance -> v Moz_appearance (read_appearance t)
  (* Ms Filter *)
  | Ms_filter -> v Ms_filter (read_filter t)
  (* O Transition *)
  | O_transition -> v O_transition (read_transitions t)
  (* Filter *)
  | Filter -> v Filter (read_filter t)
  (* Appearance *)
  | Appearance -> v Appearance (read_appearance t)
  (* Color scheme *)
  | Color_scheme -> v Color_scheme (read_color_scheme t)
  (* Background *)
  | Background_image ->
      let images = read_background_images t in
      v Background_image images
  | Background -> v Background (read_backgrounds t)
  | Border -> v Border (read_border t)
  (* Grid properties *)
  | Grid_template_columns -> v Grid_template_columns (read_grid_template_list t)
  | Grid_template_rows -> v Grid_template_rows (read_grid_template_list t)
  | Grid_row_start -> v Grid_row_start (read_grid_line t)
  | Grid_row_end -> v Grid_row_end (read_grid_line t)
  | Grid_column_start -> v Grid_column_start (read_grid_line t)
  | Grid_column_end -> v Grid_column_end (read_grid_line t)
  | Grid_auto_flow -> v Grid_auto_flow (read_grid_auto_flow t)
  | Grid_template_areas -> v Grid_template_areas (read_grid_template_areas t)
  (* Shadows *)
  | Box_shadow -> v Box_shadow (read_shadow t)
  | Text_shadow -> v Text_shadow (read_text_shadows t)
  (* Content *)
  | Content -> v Content (read_content t)
  (* Other properties *)
  | Z_index -> v Z_index (Properties.read_z_index t)
  | Opacity -> v Opacity (read_opacity t)
  | Cursor -> v Cursor (read_cursor t)
  | Box_sizing -> v Box_sizing (read_box_sizing t)
  | Field_sizing -> v Field_sizing (read_field_sizing t)
  | Caption_side -> v Caption_side (read_caption_side t)
  | User_select -> v User_select (read_user_select t)
  | Webkit_user_select -> v Webkit_user_select (read_user_select t)
  | Pointer_events -> v Pointer_events (read_pointer_events t)
  | Resize -> v Resize (read_resize t)
  | Transition -> v Transition (read_transitions t)
  | Animation -> v Animation (read_animations t)
  (* Border style properties *)
  | Border_top_style -> v Border_top_style (read_border_style t)
  | Border_right_style -> v Border_right_style (read_border_style t)
  | Border_bottom_style -> v Border_bottom_style (read_border_style t)
  | Border_left_style -> v Border_left_style (read_border_style t)
  (* Additional margin/padding properties *)
  | Padding_left ->
      v Padding_left (read_non_negative_length ~with_keywords:false t)
  | Padding_right ->
      v Padding_right (read_non_negative_length ~with_keywords:false t)
  | Padding_top ->
      v Padding_top (read_non_negative_length ~with_keywords:false t)
  | Padding_bottom ->
      v Padding_bottom (read_non_negative_length ~with_keywords:false t)
  | Padding_inline ->
      v Padding_inline (read_non_negative_length ~with_keywords:false t)
  | Padding_inline_start ->
      v Padding_inline_start (read_non_negative_length ~with_keywords:false t)
  | Padding_inline_end ->
      v Padding_inline_end (read_non_negative_length ~with_keywords:false t)
  | Padding_block ->
      v Padding_block (read_non_negative_length ~with_keywords:false t)
  | Padding_block_start ->
      v Padding_block_start (read_non_negative_length ~with_keywords:false t)
  | Padding_block_end ->
      v Padding_block_end (read_non_negative_length ~with_keywords:false t)
  | Margin_left -> v Margin_left (read_length t)
  | Margin_right -> v Margin_right (read_length t)
  | Margin_top -> v Margin_top (read_length t)
  | Margin_bottom -> v Margin_bottom (read_length t)
  | Margin_inline ->
      v Margin_inline
        (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2 read_length t)
  | Margin_inline_start -> v Margin_inline_start (read_length t)
  | Margin_inline_end -> v Margin_inline_end (read_length t)
  | Margin_block ->
      v Margin_block
        (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2 read_length t)
  | Margin_block_start -> v Margin_block_start (read_length t)
  | Margin_block_end -> v Margin_block_end (read_length t)
  (* Additional color properties *)
  | Text_decoration_color -> v Text_decoration_color (read_color t)
  (* Text decoration line and style *)
  | Text_decoration_line ->
      v Text_decoration_line (read_text_decoration_lines t)
  | Text_decoration_style ->
      v Text_decoration_style (read_text_decoration_style t)
  | Text_underline_offset -> v Text_underline_offset (read_length t)
  | Letter_spacing -> v Letter_spacing (read_length ~with_keywords:false t)
  (* List properties *)
  | List_style_type -> v List_style_type (read_list_style_type t)
  | List_style_position -> v List_style_position (read_list_style_position t)
  | List_style_image -> v List_style_image (read_list_style_image t)
  | List_style -> v List_style (read_list_style_shorthand t)
  (* Flexbox order *)
  | Order -> v Order (Properties.read_order t)
  (* Justify properties *)
  | Justify_items -> v Justify_items (read_justify_items t)
  | Justify_self -> v Justify_self (read_justify_self t)
  (* Align content *)
  | Align_content -> v Align_content (read_align_content t)
  | Align_self -> v Align_self (read_align_self t)
  (* Place properties *)
  | Place_content -> v Place_content (read_place_content t)
  | Place_items -> v Place_items (read_place_items t)
  | Place_self -> read_place_self_value t
  (* Additional grid properties *)
  | Grid_template -> v Grid_template (read_grid_template t)
  | Grid_area -> v Grid_area (read_grid_area t)
  | Grid_auto_columns ->
      let value = read_grid_template t in
      (match value with
      | Subgrid | Masonry ->
          Cursor.err_invalid t "grid-auto track cannot be subgrid or masonry"
      | _ -> ());
      v Grid_auto_columns value
  | Grid_auto_rows ->
      let value = read_grid_template t in
      (match value with
      | Subgrid | Masonry ->
          Cursor.err_invalid t "grid-auto track cannot be subgrid or masonry"
      | _ -> ());
      v Grid_auto_rows value
  | Grid_column -> v Grid_column (read_grid_line_pair t)
  | Grid_row -> v Grid_row (read_grid_line_pair t)
  (* Border inline/block properties *)
  | Border_inline_start_width ->
      v Border_inline_start_width (read_border_width t)
  | Border_inline_end_width -> v Border_inline_end_width (read_border_width t)
  | Border_block_start_width -> v Border_block_start_width (read_border_width t)
  | Border_block_end_width -> v Border_block_end_width (read_border_width t)
  | Border_inline_start_color -> v Border_inline_start_color (read_color t)
  | Border_inline_end_color -> v Border_inline_end_color (read_color t)
  | Border_inline_style -> v Border_inline_style (read_border_style t)
  | Border_block_style -> v Border_block_style (read_border_style t)
  | Border_start_start_radius ->
      v Border_start_start_radius (read_length ~with_keywords:false t)
  | Border_start_end_radius ->
      v Border_start_end_radius (read_length ~with_keywords:false t)
  | Border_end_start_radius ->
      v Border_end_start_radius (read_length ~with_keywords:false t)
  | Border_end_end_radius ->
      v Border_end_end_radius (read_length ~with_keywords:false t)
  (* Position properties *)
  | Inset -> v Inset (read_length_box t)
  | Inset_inline -> v Inset_inline (read_inset_axis t)
  | Inset_inline_start -> v Inset_inline_start (read_inset_longhand t)
  | Inset_inline_end -> v Inset_inline_end (read_inset_longhand t)
  | Inset_block -> v Inset_block (read_inset_axis t)
  | Inset_block_start -> v Inset_block_start (read_inset_longhand t)
  | Inset_block_end -> v Inset_block_end (read_inset_longhand t)
  | Top -> v Top (read_inset_longhand t)
  | Right -> v Right (read_inset_longhand t)
  | Bottom -> v Bottom (read_inset_longhand t)
  | Left -> v Left (read_inset_longhand t)
  (* Outline properties *)
  | Outline -> v Outline (read_outline t)
  | Outline_style -> v Outline_style (read_outline_style t)
  | Outline_width -> v Outline_width (read_length t)
  | Outline_offset -> v Outline_offset (read_length t)
  (* Forced color adjust *)
  | Forced_color_adjust -> v Forced_color_adjust (read_forced_color_adjust t)
  (* Scroll snap *)
  | Scroll_snap_type -> v Scroll_snap_type (read_scroll_snap_type t)
  (* Tab size *)
  | Tab_size -> v Tab_size (int_of_float (Cursor.number t))
  (* Webkit properties *)
  | Webkit_text_size_adjust ->
      v Webkit_text_size_adjust (read_text_size_adjust t)
  | Webkit_tap_highlight_color -> v Webkit_tap_highlight_color (read_color t)
  | Webkit_text_decoration -> v Webkit_text_decoration (read_text_decoration t)
  | Webkit_text_decoration_color ->
      v Webkit_text_decoration_color (read_color t)
  | Webkit_appearance -> v Webkit_appearance (read_webkit_appearance t)
  | Webkit_font_smoothing ->
      v Webkit_font_smoothing (read_webkit_font_smoothing t)
  | Webkit_line_clamp -> v Webkit_line_clamp (read_webkit_line_clamp t)
  | Webkit_box_orient -> v Webkit_box_orient (read_webkit_box_orient t)
  | Webkit_hyphens -> v Webkit_hyphens (read_hyphens t)
  (* Font properties *)
  | Font_feature_settings ->
      v Font_feature_settings (read_font_feature_settings t)
  | Font_variation_settings ->
      v Font_variation_settings (read_font_variation_settings t)
  | Font_stretch -> v Font_stretch (read_font_stretch t)
  | Font_variant_numeric -> v Font_variant_numeric (read_font_variant_numeric t)
  (* Text properties *)
  | Text_indent -> v Text_indent (read_length t)
  | Text_overflow -> v Text_overflow (read_text_overflow t)
  | Text_wrap -> v Text_wrap (read_text_wrap t)
  | Text_decoration_thickness -> v Text_decoration_thickness (read_length t)
  | Text_size_adjust -> v Text_size_adjust (read_text_size_adjust t)
  | Text_decoration_skip_ink ->
      v Text_decoration_skip_ink (read_text_decoration_skip_ink t)
  (* Word/text breaking *)
  | Word_break -> v Word_break (read_word_break t)
  | Overflow_wrap -> v Overflow_wrap (read_overflow_wrap t)
  | Hyphens -> v Hyphens (read_hyphens t)
  | Word_spacing -> v Word_spacing (read_length t)
  (* Container properties *)
  | Container_type -> v Container_type (read_container_type t)
  | Container_name -> v Container_name (read_untyped_value t)
  | Container -> v Container (read_container_shorthand t)
  (* Anchor positioning properties. [anchor-name] / [position-anchor] take a
     [<dashed-ident>] (ident that begins with [--]), and
     [position-try-fallbacks] is a comma-separated list of the same. *)
  | Anchor_name ->
      v Anchor_name
        (Cursor.list ~sep:Cursor.comma ~at_least:1
           (fun r ->
             match Cursor.peek_ident r with
             | Some "none" ->
                 let _ = Cursor.ident r in
                 "none"
             | _ -> read_dashed_ident r)
           t
        |> String.concat ",")
  | Position_anchor ->
      v Position_anchor
        (match Cursor.peek_ident t with
        | Some "auto" ->
            let _ = Cursor.ident t in
            "auto"
        | _ -> read_dashed_ident t)
  | Position_try_fallbacks ->
      v Position_try_fallbacks
        (Cursor.list ~sep:Cursor.comma ~at_least:1 read_position_try_fallback t)
  | Shape_outside -> v Shape_outside (read_shape_outside t)
  | Shape_margin -> v Shape_margin (read_non_negative_length_percentage t)
  | Overflow_clip_margin ->
      v Overflow_clip_margin
        (read_length ~allow_negative:false ~with_keywords:false t)
  | Overflow_anchor ->
      v Overflow_anchor
        (Cursor.enum "overflow-anchor" [ ("auto", "auto"); ("none", "none") ] t)
  | Scrollbar_width ->
      v Scrollbar_width
        (Cursor.enum "scrollbar-width"
           [ ("auto", "auto"); ("thin", "thin"); ("none", "none") ]
           t)
  | Scrollbar_color -> v Scrollbar_color (read_untyped_value t)
  | Scrollbar_gutter -> v Scrollbar_gutter (read_scrollbar_gutter t)
  | Line_height_step -> v Line_height_step (read_length ~allow_negative:false t)
  | Font_palette -> v Font_palette (read_font_palette t)
  | Font_synthesis -> v Font_synthesis (read_untyped_value t)
  | Text_wrap_style ->
      v Text_wrap_style
        (Cursor.enum "text-wrap-style"
           [
             ("auto", "auto");
             ("balance", "balance");
             ("pretty", "pretty");
             ("stable", "stable");
           ]
           t)
  | Text_box_trim ->
      v Text_box_trim
        (Cursor.enum "text-box-trim"
           [
             ("none", "none");
             ("trim-start", "trim-start");
             ("trim-end", "trim-end");
             ("trim-both", "trim-both");
           ]
           t)
  | Animation_timeline -> v Animation_timeline (read_animation_timeline t)
  | Animation_range -> v Animation_range (read_animation_range t)
  | Scroll_timeline -> v Scroll_timeline (read_timeline_shorthand t)
  | View_transition_name ->
      (* [none | <custom-ident>] - a single ident; reject extra tokens. *)
      let s =
        match Cursor.peek_ident t with
        | Some "none" ->
            let _ = Cursor.ident t in
            "none"
        | _ -> Cursor.ident ~keep_case:true t
      in
      validate_no_extra_tokens t;
      v View_transition_name s
  | Image_orientation ->
      v Image_orientation
        (Cursor.enum "image-orientation"
           [ ("from-image", "from-image"); ("none", "none") ]
           t)
  | Contain_intrinsic_size -> v Contain_intrinsic_size (read_untyped_value t)
  | Margin_trim -> v Margin_trim (read_margin_trim t)
  | Offset_path -> v Offset_path (read_untyped_value t)
  | Offset_distance -> v Offset_distance (read_non_negative_length_percentage t)
  | Font_size_adjust -> v Font_size_adjust (read_font_size_adjust t)
  | Font_variant_emoji ->
      v Font_variant_emoji
        (Cursor.enum "font-variant-emoji"
           [
             ("normal", "normal");
             ("text", "text");
             ("emoji", "emoji");
             ("unicode", "unicode");
           ]
           t)
  | Text_spacing_trim ->
      v Text_spacing_trim
        (Cursor.enum "text-spacing-trim"
           [
             ("normal", "normal");
             ("space-all", "space-all");
             ("trim-start", "trim-start");
             ("space-first", "space-first");
           ]
           t)
  | Hyphenate_limit_chars -> v Hyphenate_limit_chars (read_untyped_value t)
  | Initial_letter -> v Initial_letter (read_initial_letter t)
  | View_timeline_name -> v View_timeline_name (read_untyped_value t)
  | View_timeline_axis ->
      v View_timeline_axis
        (Cursor.enum "view-timeline-axis"
           [ ("block", "block"); ("inline", "inline"); ("x", "x"); ("y", "y") ]
           t)
  | View_timeline -> v View_timeline (read_timeline_shorthand t)
  | Timeline_scope -> v Timeline_scope (read_untyped_value t)
  (* Transform properties *)
  | Perspective -> v Perspective (read_length ~with_keywords:false t)
  | Perspective_origin -> v Perspective_origin (read_perspective_origin t)
  | Transform_style -> v Transform_style (read_transform_style t)
  | Backface_visibility -> v Backface_visibility (read_backface_visibility t)
  | Rotate -> v Rotate (read_rotate_value t)
  | Scale -> v Scale (read_scale t)
  (* Object properties *)
  | Object_position -> v Object_position (read_position_value t)
  | Object_fit -> v Object_fit (read_object_fit t)
  (* Transition properties *)
  | Transition_duration -> v Transition_duration (read_duration t)
  | Transition_timing_function ->
      v Transition_timing_function (read_timing_function t)
  | Transition_delay -> v Transition_delay (read_time t)
  | Transition_property -> v Transition_property (read_transition_property t)
  | Transition_behavior ->
      v Transition_behavior (Properties.read_transition_behavior t)
  (* Will change *)
  | Will_change -> v Will_change (read_will_change t)
  (* Contain and isolation *)
  | Contain -> v Contain (read_contain t)
  | Isolation -> v Isolation (read_isolation t)
  (* Break properties *)
  | Break_before -> v Break_before (read_break_value t)
  | Break_after -> v Break_after (read_break_value t)
  | Break_inside -> v Break_inside (read_break_inside_value t)
  | Page_size -> v Page_size (read_page_size t)
  | Columns -> v Columns (read_columns_value t)
  (* Background properties *)
  | Background_attachment ->
      v Background_attachment (read_background_attachment t)
  | Background_origin -> v Background_origin (read_background_box t)
  | Background_clip -> v Background_clip (read_background_box t)
  | Webkit_background_clip -> v Webkit_background_clip (read_background_box t)
  | Background_position -> v Background_position (read_background_position t)
  | Background_repeat -> v Background_repeat (read_background_repeat t)
  | Background_size -> v Background_size (read_background_size t)
  | Background_blend_mode -> read_background_blend_mode_value t
  (* Border shorthands *)
  | Border_top -> v Border_top (read_string t)
  | Border_right -> v Border_right (read_string t)
  | Border_bottom -> v Border_bottom (read_string t)
  | Border_left -> v Border_left (read_string t)
  | Border_spacing ->
      (* border-spacing accepts 1 or 2 length values *)
      let lengths =
        Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2 read_length t
      in
      v Border_spacing lengths
  | Border_image -> v Border_image (read_border_image t)
  | Border_collapse -> v Border_collapse (read_border_collapse t)
  (* Clip and mask *)
  | Clip_path -> v Clip_path (read_clip_path t)
  | Mask -> v Mask (read_string t)
  | Clip -> v Clip (read_clip t)
  (* Content visibility *)
  | Content_visibility -> v Content_visibility (read_content_visibility t)
  (* Aspect ratio *)
  | Aspect_ratio -> v Aspect_ratio (read_aspect_ratio t)
  (* Vertical align *)
  | Vertical_align -> v Vertical_align (read_vertical_align t)
  (* Moz properties *)
  | Moz_osx_font_smoothing ->
      v Moz_osx_font_smoothing (read_moz_osx_font_smoothing t)
  (* Backdrop filter *)
  | Backdrop_filter -> v Backdrop_filter (read_filter t)
  | Webkit_backdrop_filter -> v Webkit_backdrop_filter (read_filter t)
  (* Scroll properties *)
  | Scroll_snap_align -> v Scroll_snap_align (read_scroll_snap_align t)
  | Scroll_snap_stop -> v Scroll_snap_stop (read_scroll_snap_stop t)
  | Scroll_behavior -> v Scroll_behavior (read_scroll_behavior t)
  | Scroll_margin -> v Scroll_margin (read_length t)
  | Scroll_margin_top -> v Scroll_margin_top (read_length t)
  | Scroll_margin_right -> v Scroll_margin_right (read_length t)
  | Scroll_margin_bottom -> v Scroll_margin_bottom (read_length t)
  | Scroll_margin_left -> v Scroll_margin_left (read_length t)
  | Scroll_margin_inline -> v Scroll_margin_inline (read_length t)
  | Scroll_margin_inline_start -> v Scroll_margin_inline_start (read_length t)
  | Scroll_margin_inline_end -> v Scroll_margin_inline_end (read_length t)
  | Scroll_margin_block ->
      let lengths =
        Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2 read_length t
      in
      (match lengths with
      | [ Zero; Zero ] -> Cursor.err_invalid t "duplicate zero scroll margin"
      | _ -> ());
      v Scroll_margin_block lengths
  | Scroll_margin_block_start -> v Scroll_margin_block_start (read_length t)
  | Scroll_margin_block_end -> v Scroll_margin_block_end (read_length t)
  | Scroll_padding -> v Scroll_padding (read_length t)
  | Scroll_padding_top -> v Scroll_padding_top (read_length t)
  | Scroll_padding_right -> v Scroll_padding_right (read_length t)
  | Scroll_padding_bottom -> v Scroll_padding_bottom (read_length t)
  | Scroll_padding_left -> v Scroll_padding_left (read_length t)
  | Scroll_padding_inline -> v Scroll_padding_inline (read_length t)
  | Scroll_padding_inline_start -> v Scroll_padding_inline_start (read_length t)
  | Scroll_padding_inline_end -> v Scroll_padding_inline_end (read_length t)
  | Scroll_padding_block -> v Scroll_padding_block (read_length t)
  | Scroll_padding_block_start -> v Scroll_padding_block_start (read_length t)
  | Scroll_padding_block_end -> v Scroll_padding_block_end (read_length t)
  | Overscroll_behavior ->
      let xs, _ = Cursor.many (fun r -> read_overscroll_behavior r) t in
      v Overscroll_behavior xs
  | Overscroll_behavior_x ->
      v Overscroll_behavior_x (read_overscroll_behavior t)
  | Overscroll_behavior_y ->
      v Overscroll_behavior_y (read_overscroll_behavior t)
  (* Quotes *)
  | Quotes -> v Quotes (read_quotes t)
  (* Touch action *)
  | Touch_action -> v Touch_action (read_touch_action t)
  (* Clear and float *)
  | Clear -> v Clear (read_clear t)
  | Float -> v Float (read_float_side t)
  (* SVG properties *)
  | Fill -> v Fill (read_svg_paint t)
  | Stroke -> v Stroke (read_svg_paint t)
  | Stroke_width -> v Stroke_width (read_length t)
  (* Direction and writing *)
  | Direction -> v Direction (read_direction t)
  | Unicode_bidi -> v Unicode_bidi (read_unicode_bidi t)
  | Writing_mode -> v Writing_mode (read_writing_mode t)
  (* Animation properties *)
  | Animation_name -> v Animation_name (read_animation_name t)
  | Animation_duration -> v Animation_duration (read_duration t)
  | Animation_timing_function ->
      v Animation_timing_function (read_timing_function t)
  | Animation_delay -> v Animation_delay (read_time t)
  | Animation_iteration_count ->
      v Animation_iteration_count (read_animation_iteration_count t)
  | Animation_direction -> v Animation_direction (read_animation_direction t)
  | Animation_fill_mode -> v Animation_fill_mode (read_animation_fill_mode t)
  | Animation_play_state -> v Animation_play_state (read_animation_play_state t)
  (* Color properties *)
  | Accent_color -> v Accent_color (read_color t)
  | Caret_color -> v Caret_color (read_color t)
  (* Mix blend mode *)
  | Mix_blend_mode -> v Mix_blend_mode (read_blend_mode t)
  (* Table layout *)
  | Table_layout -> v Table_layout (read_table_layout t)
  (* Print color adjust *)
  | Print_color_adjust -> v Print_color_adjust (read_print_color_adjust t)
  (* Box decoration break *)
  | Box_decoration_break -> v Box_decoration_break (read_box_decoration_break t)
  | Webkit_box_decoration_break ->
      v Webkit_box_decoration_break (read_box_decoration_break t)
  (* Webkit mask properties *)
  | Webkit_mask_image -> v Webkit_mask_image (read_background_image t)
  | Webkit_mask_composite ->
      v Webkit_mask_composite (read_webkit_mask_composite t)
  | Webkit_mask_source_type ->
      v Webkit_mask_source_type (read_webkit_mask_source_type t)
  | Webkit_mask_size -> v Webkit_mask_size (read_background_size t)
  | Webkit_mask_position -> v Webkit_mask_position (read_background_position t)
  | Webkit_mask_repeat -> v Webkit_mask_repeat (read_background_repeat t)
  | Webkit_mask_clip -> v Webkit_mask_clip (read_mask_box t)
  | Webkit_mask_origin -> v Webkit_mask_origin (read_mask_box t)
  (* Unprefixed mask properties *)
  | Mask_image -> v Mask_image (read_background_image t)
  | Mask_composite -> v Mask_composite (read_mask_composite t)
  | Mask_mode -> v Mask_mode (read_mask_mode t)
  | Mask_size -> v Mask_size (read_background_size t)
  | Mask_position -> v Mask_position (read_background_position t)
  | Mask_repeat -> v Mask_repeat (read_background_repeat t)
  | Mask_clip -> v Mask_clip (read_mask_box t)
  | Mask_origin -> v Mask_origin (read_mask_box t)
  | Mask_type -> v Mask_type (read_mask_type t)

(* Check if a custom property name is a font-family variable *)

(** Parse a custom property (--name: value) *)
let is_font_family_var name =
  let bare =
    if String.length name > 2 && String.sub name 0 2 = "--" then
      String.sub name 2 (String.length name - 2)
    else name
  in
  let starts_with prefix s =
    String.length s >= String.length prefix
    && String.sub s 0 (String.length prefix) = prefix
  in
  starts_with "font-" bare
  || starts_with "default-font-family" bare
  || starts_with "default-mono-font-family" bare

(* For custom properties only, !important is recognised solely as the literal
   10-character suffix [!important]; [! important] (with whitespace between the
   bang and the ident) is part of the value, not the importance flag. Per
   test_declaration's spec_custom_tokens: this matches Tailwind/lightningcss's
   conservative handling for [--*] values, where any whitespace inside the flag
   means the user wrote arbitrary tokens, not the cascade marker. *)
let split_custom_important value =
  let trimmed = String.trim value in
  let len = String.length trimmed in
  let suffix = "!important" in
  let suffix_len = String.length suffix in
  if
    len >= suffix_len
    && String.lowercase_ascii (String.sub trimmed (len - suffix_len) suffix_len)
       = suffix
  then
    let head = String.sub trimmed 0 (len - suffix_len) in
    (String.trim head, true)
  else (trimmed, false)

let read_custom_property_declaration t : declaration =
  let name = read_property_name t in
  Cursor.ws t;
  if not (Cursor.colon t) then Cursor.err_expected t "':'";
  Cursor.ws t;
  let raw_value = Cursor.consume_to_semicolon ~trim:true t in
  let value_str, is_important = split_custom_important raw_value in
  (* custom_property may raise Failure for invalid names like "--" *)
  try
    let decl =
      if is_font_family_var name then
        let trimmed = String.trim value_str in
        if String.length trimmed >= 4 && String.sub trimmed 0 4 = "var(" then
          custom_property name value_str
        else
          match Cursor.of_string value_str |> read_font_family with
          | ff -> custom_declaration name Font_family ff
          | exception _ -> custom_property name value_str
      else custom_property name value_str
    in
    if is_important then important decl else decl
  with Failure msg -> Cursor.err_invalid t msg

let starts_with_var raw_value =
  String.length raw_value >= 4 && String.sub raw_value 0 4 = "var("

let validate_legacy_page_break t name raw_value =
  if not (is_css_wide_keyword raw_value) then
    match name with
    | ("page-break-before" | "page-break-after")
      when not
             (List.mem raw_value [ "auto"; "always"; "avoid"; "left"; "right" ])
      ->
        Cursor.err_invalid t "invalid legacy page-break value"
    | "page-break-inside" when not (List.mem raw_value [ "auto"; "avoid" ]) ->
        Cursor.err_invalid t "invalid legacy page-break-inside value"
    | _ -> ()

let validate_regular_property_raw t name raw_value =
  if value_has_css_wide_mix raw_value then
    Cursor.err_invalid t "CSS-wide keyword mixed with other values";
  if name = "all" && not (is_css_wide_keyword raw_value) then
    Cursor.err_invalid t "all accepts only CSS-wide keywords";
  validate_legacy_page_break t name raw_value

let read_keyword_or_var_decl t name raw_value =
  if starts_with_var raw_value then validate_var_reference t raw_value;
  ignore (Cursor.consume_to_decl_end ~trim:true t);
  let is_important = read_importance t in
  validate_no_extra_tokens t;
  let value = Cursor.remaining (Cursor.of_string raw_value) in
  let decl = custom_declaration name Value value in
  if is_important then important decl else decl

let read_font_src_declaration t raw_value =
  ignore (Cursor.consume_to_decl_end ~trim:true t);
  let is_important = read_importance t in
  validate_no_extra_tokens t;
  let decl =
    try custom_declaration "src" Font_src (Font_face.src_of_string raw_value)
    with Failure msg -> Cursor.err_invalid t msg
  in
  if is_important then important decl else decl

let read_typed_property_declaration t start =
  Cursor.restore t start;
  let (Prop prop_type) = read_any_property t in
  Cursor.ws t;
  if not (Cursor.colon t) then Cursor.err_expected t "':'";
  Cursor.ws t;
  let decl = read_value prop_type t in
  validate_no_extra_tokens t;
  let is_important = read_importance t in
  validate_no_extra_tokens t;
  (match Cursor.peek_delim t with
  | Some '!' -> Cursor.err_invalid t "duplicate !important"
  | _ -> ());
  if is_important then important decl else decl

(** Parse a regular property (name: value) *)
let read_regular_property_declaration t : declaration =
  let start = Cursor.save t in
  let name = String.lowercase_ascii (read_property_name t) in
  Cursor.ws t;
  if not (Cursor.colon t) then Cursor.err_expected t "':'";
  Cursor.ws t;
  let raw_value = Cursor.lookahead (Cursor.consume_to_decl_end ~trim:true) t in
  validate_regular_property_raw t name raw_value;
  if is_css_wide_keyword raw_value || is_var_reference raw_value then
    read_keyword_or_var_decl t name raw_value
  else if String.equal name "src" then read_font_src_declaration t raw_value
  else read_typed_property_declaration t start

(** Parse a single declaration directly from stream - no string roundtrips *)
let read_declaration t : declaration option =
  let read_one () =
    Cursor.with_context t "read_declaration" @@ fun () ->
    (* Custom properties are idents starting with [--]. *)
    let is_custom =
      match Cursor.peek t with
      | Some (Component.Preserved { kind = Token.Ident s; _ }) ->
          String.length s >= 2 && s.[0] = '-' && s.[1] = '-'
      | _ -> false
    in
    if is_custom then read_custom_property_declaration t
    else read_regular_property_declaration t
  in
  Cursor.ws t;
  match Cursor.peek t with
  | None -> None (* EOF is acceptable at top-level parsing *)
  | Some (Component.Preserved { kind = Token.Colon; _ })
  | Some (Component.Preserved { kind = Token.Hash _; _ })
  | Some (Component.Block { node = { opening = Token.Square; _ }; _ })
  | Some (Component.Preserved { kind = Token.Delim ("." | "*" | "&"); _ }) ->
      (* Selector-like components indicate a nested rule. *)
      None
  | Some _ -> Some (read_one ())

(* Skip from the current cursor position to just past the next top-level [;], or
   stop at EOF. Used to recover from a failed declaration inside a block: per
   CSS Syntax section 5.4.4 ("consume a list of declarations"), an invalid
   declaration is dropped, parsing resumes at the next [;], and the surrounding
   rule survives. *)
let skip_to_next_declaration t =
  let rec loop () =
    match Cursor.next_raw t with
    | None -> ()
    | Some (Component.Preserved { kind = Token.Semicolon; _ }) -> ()
    | Some _ -> loop ()
  in
  loop ()

let read_declarations t =
  Cursor.with_context t "declarations" @@ fun () ->
  let rec check_separator acc =
    Cursor.ws t;
    match Cursor.peek t with
    | None -> List.rev acc (* End of input *)
    | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
        Cursor.skip t;
        loop acc
    | Some (Component.Preserved { kind = Token.Ident _; _ }) ->
        Cursor.err t "missing semicolon between declarations"
    | _ ->
        (* Some other component - let the next iteration handle it *)
        List.rev acc
  and loop acc =
    Cursor.ws t;
    match Cursor.peek t with
    | None -> List.rev acc
    | _ -> (
        if Cursor.recover t then (
          match
            try Ok (read_declaration t) with Error.Parse_error e -> Error e
          with
          | Ok None -> List.rev acc
          | Ok (Some decl) -> (
              let acc = decl :: acc in
              try check_separator acc
              with Error.Parse_error e ->
                Cursor.push_warning t e;
                skip_to_next_declaration t;
                loop acc)
          | Error e ->
              Cursor.push_warning t e;
              skip_to_next_declaration t;
              loop acc)
        else
          match read_declaration t with
          | None -> List.rev acc
          | Some decl ->
              let acc = decl :: acc in
              check_separator acc)
  in
  loop []

let read_block t =
  Cursor.ws t;
  Cursor.braces (fun inner -> read_declarations inner) t

let of_string s =
  match read_declaration (Cursor.of_string s) with
  | Some d -> d
  | None -> failwith ("Declaration.of_string: invalid declaration: " ^ s)

(* Pretty printer for declarations *)
let rec pp_declaration : declaration Pp.t =
 fun ctx -> function
  | Declaration { property; value; important } ->
      pp_property ctx property;
      Pp.string ctx ":";
      Pp.space_if_pretty ctx ();
      pp_property_value ctx (property, value);
      if important then
        Pp.string ctx (if ctx.minify then "!important" else " !important")
  | Custom_declaration { name; kind; value; layer; important; _ } ->
      Pp.string ctx name;
      Pp.string ctx ":";
      Pp.space_if_pretty ctx ();
      (* For theme layer declarations, check if theme_defaults provides an
         override value (e.g., --font-weight-bold: 650 from theme config) *)
      let bare_name =
        if String.length name > 2 && String.sub name 0 2 = "--" then
          String.sub name 2 (String.length name - 2)
        else name
      in
      (match (layer, kind, ctx.theme_defaults bare_name) with
      | Some "theme", Font_family, _ ->
          (* Font_family values must go through pp_font_family for proper line
             wrapping; raw theme_defaults strings have wrong indent *)
          pp_value ctx (kind, value)
      | Some "theme", _, Some override_value -> Pp.string ctx override_value
      | _ -> pp_value ctx (kind, value));
      if important then
        Pp.string ctx (if ctx.minify then "!important" else " !important")
  | Theme_guarded { var_name; decl } ->
      if Pp.in_theme ctx var_name then pp_declaration ctx decl

(* Convert a declaration to its string representation *)
let string_of_declaration ?(minify = false) decl =
  let buf = Buffer.create 32 in
  let ctx =
    {
      Pp.minify;
      indent = 0;
      buf;
      inline = false;
      in_function = false;
      theme = None;
      theme_defaults = Pp.no_theme_defaults;
    }
  in
  pp_declaration ctx decl;
  Buffer.contents buf

(* Resolve theme guards: filter out Theme_guarded declarations whose var_name is
   not in the theme, and unwrap those that are *)
let resolve_theme_guards ctx decls =
  List.filter_map
    (fun decl ->
      match decl with
      | Theme_guarded { var_name; decl } ->
          if Pp.in_theme ctx var_name then Some decl else None
      | d -> Some d)
    decls

(* Single-to-list property helpers *)
let background_image value = v Background_image [ value ]
let text_shadow value = v Text_shadow [ value ]
let text_shadows values = v Text_shadow values
let transition value = v Transition [ value ]
let transitions values = v Transition values
let animation value = v Animation [ value ]
let box_shadow value = v Box_shadow value

let box_shadows = function
  | [] -> failwith "empty box_shadows"
  | values -> v Box_shadow (List values)

(* Special helpers *)
let z_index_auto = v Z_index Auto

(* Font variant helpers *)
let font_variant_numeric_tokens tokens = Tokens tokens

let font_variant_numeric_composed ?ordinal ?slashed_zero ?numeric_figure
    ?numeric_spacing ?numeric_fraction () =
  Composed
    { ordinal; slashed_zero; numeric_figure; numeric_spacing; numeric_fraction }

(* Property constructors with typed values *)
let background bg = v Background [ bg ]
let background_color c = v Background_color c
let color c = v Color c
let border_color c = v Border_color c
let border_style bs = v Border_style bs
let border_top_style bs = v Border_top_style bs
let border_right_style bs = v Border_right_style bs
let border_bottom_style bs = v Border_bottom_style bs
let border_left_style bs = v Border_left_style bs
let text_decoration td = v Text_decoration td
let font_style fs = v Font_style fs
let list_style_type lst = v List_style_type lst
let list_style_position ls = v List_style_position ls
let list_style_image is = v List_style_image is
let padding (values : length list) = v Padding values
let padding_left len = v Padding_left len
let padding_right len = v Padding_right len
let padding_bottom len = v Padding_bottom len
let padding_top len = v Padding_top len
let margin (values : length list) = v Margin values
let margin_left len = v Margin_left len
let margin_right len = v Margin_right len
let margin_top len = v Margin_top len
let margin_bottom len = v Margin_bottom len

(* Remove deprecated string-based versions *)
let gap len = v Gap len
let column_gap len = v Column_gap len
let row_gap len = v Row_gap len

(* Grid functions *)
let grid_template_areas template = v Grid_template_areas template
let grid_template template = v Grid_template template
let grid_auto_columns size = v Grid_auto_columns size
let grid_auto_rows size = v Grid_auto_rows size
let grid_row_start value = v Grid_row_start value
let grid_row_end value = v Grid_row_end value
let grid_column_start value = v Grid_column_start value
let grid_column_end value = v Grid_column_end value
let grid_row (pair : grid_line * grid_line) = v Grid_row pair
let grid_column (pair : grid_line * grid_line) = v Grid_column pair
let grid_area value = v Grid_area value
let width len = v Width (Length len)
let height len = v Height (Length len)

(* Remove deprecated string-based versions *)
let min_width len = v Min_width (Length len)
let min_height len = v Min_height (Length len)
let max_width len = v Max_width (Length len)
let max_height len = v Max_height (Length len)
let inline_size len = v Inline_size (Length len)
let min_inline_size len = v Min_inline_size (Length len)
let max_inline_size len = v Max_inline_size (Length len)
let block_size len = v Block_size (Length len)
let min_block_size len = v Min_block_size (Length len)
let max_block_size len = v Max_block_size (Length len)
let font_size len = v Font_size (Length len)
let font_size_kw fs = v Font_size fs
let line_height len = v Line_height len
let font_weight w = v Font_weight w
let text_align a = v Text_align a
let text_decoration_style value = v Text_decoration_style value
let text_decoration_line value = v Text_decoration_line [ value ]
let text_underline_offset value = v Text_underline_offset value
let text_transform value = v Text_transform value
let letter_spacing len = v Letter_spacing len
let white_space value = v White_space value
let display d = v Display d
let position p = v Position p
let visibility p = v Visibility p
let inset len = v Inset len
let inset_inline len = v Inset_inline len
let inset_inline_start len = v Inset_inline_start [ len ]
let inset_inline_end len = v Inset_inline_end [ len ]
let inset_block len = v Inset_block len
let inset_block_start len = v Inset_block_start [ len ]
let inset_block_end len = v Inset_block_end [ len ]
let top len = v Top [ len ]
let right len = v Right [ len ]
let bottom len = v Bottom [ len ]
let left len = v Left [ len ]
let opacity value = v Opacity value

(* Remove deprecated string-based versions *)
let flex_direction d = v Flex_direction d
let flex value = v Flex value
let flex_grow value = v Flex_grow value
let flex_shrink value = v Flex_shrink value
let flex_basis value = v Flex_basis value
let flex_wrap value = v Flex_wrap value
let order value = v Order value
let align_items a = v Align_items a
let align_content a = v Align_content a
let align_self a = v Align_self a
let justify_content a = v Justify_content a
let justify_items a = v Justify_items a
let justify_self a = v Justify_self a
let place_content value = v Place_content value
let place_items value = v Place_items value
let place_self value = v Place_self value
let border_width len = v Border_width [ len ]
let border_radius len = v Border_radius len
let border_top_left_radius len = v Border_top_left_radius len
let border_top_right_radius len = v Border_top_right_radius len
let border_bottom_left_radius len = v Border_bottom_left_radius len
let border_bottom_right_radius len = v Border_bottom_right_radius len
let fill value = v Fill value
let stroke value = v Stroke value
let stroke_width value = v Stroke_width value
let outline_style o = v Outline_style o
let outline_width len = v Outline_width len
let outline_color c = v Outline_color c
let forced_color_adjust c = v Forced_color_adjust c
let table_layout value = v Table_layout value
let border_spacing lens = v Border_spacing lens
let overflow o = v Overflow o
let object_fit value = v Object_fit value
let clip value = v Clip value
let clear value = v Clear value
let float value = v Float value
let touch_action value = v Touch_action value
let direction value = v Direction value
let unicode_bidi value = v Unicode_bidi value
let writing_mode value = v Writing_mode value
let text_decoration_skip_ink value = v Text_decoration_skip_ink value
let animation_name value = v Animation_name value
let animation_duration value = v Animation_duration value
let animation_timing_function value = v Animation_timing_function value
let animation_delay value = v Animation_delay value
let animation_iteration_count value = v Animation_iteration_count value
let animation_direction value = v Animation_direction value
let animation_fill_mode value = v Animation_fill_mode value
let animation_play_state value = v Animation_play_state value
let background_blend_mode value = v Background_blend_mode [ value ]
let scroll_margin value = v Scroll_margin value
let scroll_margin_top value = v Scroll_margin_top value
let scroll_margin_right value = v Scroll_margin_right value
let scroll_margin_bottom value = v Scroll_margin_bottom value
let scroll_margin_left value = v Scroll_margin_left value
let scroll_margin_inline value = v Scroll_margin_inline value
let scroll_margin_inline_start value = v Scroll_margin_inline_start value
let scroll_margin_inline_end value = v Scroll_margin_inline_end value
let scroll_margin_block value = v Scroll_margin_block value
let scroll_margin_block_start value = v Scroll_margin_block_start value
let scroll_margin_block_end value = v Scroll_margin_block_end value
let scroll_padding value = v Scroll_padding value
let scroll_padding_top value = v Scroll_padding_top value
let scroll_padding_right value = v Scroll_padding_right value
let scroll_padding_bottom value = v Scroll_padding_bottom value
let scroll_padding_left value = v Scroll_padding_left value
let scroll_padding_inline value = v Scroll_padding_inline value
let scroll_padding_inline_start value = v Scroll_padding_inline_start value
let scroll_padding_inline_end value = v Scroll_padding_inline_end value
let scroll_padding_block value = v Scroll_padding_block value
let scroll_padding_block_start value = v Scroll_padding_block_start value
let scroll_padding_block_end value = v Scroll_padding_block_end value
let overscroll_behavior value = v Overscroll_behavior value
let overscroll_behavior_x value = v Overscroll_behavior_x value
let overscroll_behavior_y value = v Overscroll_behavior_y value
let accent_color value = v Accent_color value
let caret_color value = v Caret_color value
let text_decoration_color value = v Text_decoration_color value
let text_decoration_thickness value = v Text_decoration_thickness value
let text_size_adjust value = v Text_size_adjust value
let aspect_ratio a = v Aspect_ratio a
let filter value = v Filter value

let filter_var_empty name : filter =
  Var { name; fallback = Empty; default = None; layer = None; meta = None }

let background_image_var_none name : background_image =
  Var { name; fallback = None; default = None; layer = None; meta = None }

let word_spacing value = v Word_spacing value
let quotes value = v Quotes value

let border ?width ?style ?color () =
  let border_value : border =
    match (width, style, color) with
    | None, None, None -> None
    | _ -> Shorthand { width; style; color }
  in
  v Border border_value

let tab_size value = v Tab_size value
let webkit_text_size_adjust value = v Webkit_text_size_adjust value
let font_feature_settings value = v Font_feature_settings value
let font_variation_settings value = v Font_variation_settings value
let webkit_tap_highlight_color value = v Webkit_tap_highlight_color value
let webkit_text_decoration value = v Webkit_text_decoration value
let webkit_text_decoration_color value = v Webkit_text_decoration_color value
let text_indent len = v Text_indent len
let border_collapse value = v Border_collapse value
let list_style value = v List_style value
let font value = v Font value
let webkit_appearance value = v Webkit_appearance value
let transform_style value = v Transform_style value
let backface_visibility value = v Backface_visibility value
let object_position value = v Object_position value
let transition_duration value = v Transition_duration value
let transition_timing_function value = v Transition_timing_function value
let transition_delay value = v Transition_delay value
let transition_behavior value = v Transition_behavior value
let transition_property value = v Transition_property value

(* Additional v constructors to match the interface *)
let mix_blend_mode value = v Mix_blend_mode value
let grid_template_columns value = v Grid_template_columns value
let grid_template_rows value = v Grid_template_rows value
let grid_auto_flow value = v Grid_auto_flow value
let pointer_events value = v Pointer_events value
let z_index value = v Z_index value
let appearance value = v Appearance value
let overflow_x value = v Overflow_x value
let overflow_y value = v Overflow_y value
let resize value = v Resize value
let vertical_align value = v Vertical_align value
let box_sizing value = v Box_sizing value
let field_sizing value = v Field_sizing value
let caption_side value = v Caption_side value
let font_family value = v Font_family value
let print_color_adjust value = v Print_color_adjust value
let box_decoration_break value = v Box_decoration_break value
let webkit_box_decoration_break value = v Webkit_box_decoration_break value
let background_origin value = v Background_origin value
let background_clip value = v Background_clip value
let webkit_background_clip value = v Webkit_background_clip value

let font_families = function
  | [] -> failwith "empty font_families"
  | fonts -> v Font_family (List fonts)

let background_attachment value = v Background_attachment value
let border_top value = v Border_top value
let border_right value = v Border_right value
let border_bottom value = v Border_bottom value
let border_left value = v Border_left value
let transform_origin value = v Transform_origin value
let transform_box value = v Transform_box value
let clip_path value = v Clip_path value
let mask value = v Mask value
let webkit_mask_image value = v Webkit_mask_image value
let mask_image value = v Mask_image value
let webkit_mask_composite value = v Webkit_mask_composite value
let mask_composite value = v Mask_composite value
let webkit_mask_source_type value = v Webkit_mask_source_type value
let mask_mode value = v Mask_mode value
let mask_type value = v Mask_type value
let webkit_mask_size value = v Webkit_mask_size value
let mask_size value = v Mask_size value
let webkit_mask_position value = v Webkit_mask_position value
let mask_position value = v Mask_position value
let webkit_mask_repeat value = v Webkit_mask_repeat value
let mask_repeat value = v Mask_repeat value
let webkit_mask_clip value = v Webkit_mask_clip value
let mask_clip value = v Mask_clip value
let webkit_mask_origin value = v Webkit_mask_origin value
let mask_origin value = v Mask_origin value
let content_visibility value = v Content_visibility value
let moz_osx_font_smoothing value = v Moz_osx_font_smoothing value
let webkit_line_clamp value = v Webkit_line_clamp value
let webkit_box_orient value = v Webkit_box_orient value
let text_overflow value = v Text_overflow value
let text_wrap value = v Text_wrap value
let word_break value = v Word_break value
let overflow_wrap value = v Overflow_wrap value
let hyphens value = v Hyphens value
let webkit_hyphens value = v Webkit_hyphens value
let font_stretch value = v Font_stretch value
let font_variant_numeric value = v Font_variant_numeric value
let backdrop_filter value = v Backdrop_filter value
let webkit_backdrop_filter value = v Webkit_backdrop_filter value
let background_position value = v Background_position value
let background_repeat value = v Background_repeat value
let background_size value = v Background_size value
let content value = v Content value
let border_left_width value = v Border_left_width value
let border_inline_start_width value = v Border_inline_start_width value
let border_inline_end_width value = v Border_inline_end_width value
let border_block_start_width value = v Border_block_start_width value
let border_block_end_width value = v Border_block_end_width value
let border_bottom_width value = v Border_bottom_width value
let border_top_width value = v Border_top_width value
let border_right_width value = v Border_right_width value
let border_top_color value = v Border_top_color value
let border_right_color value = v Border_right_color value
let border_bottom_color value = v Border_bottom_color value
let border_left_color value = v Border_left_color value
let border_inline_start_color value = v Border_inline_start_color value
let border_inline_end_color value = v Border_inline_end_color value
let border_inline_style value = v Border_inline_style value
let border_block_style value = v Border_block_style value
let border_start_start_radius value = v Border_start_start_radius value
let border_start_end_radius value = v Border_start_end_radius value
let border_end_start_radius value = v Border_end_start_radius value
let border_end_end_radius value = v Border_end_end_radius value
let webkit_font_smoothing value = v Webkit_font_smoothing value
let cursor value = v Cursor value
let user_select value = v User_select value
let webkit_user_select value = v Webkit_user_select value
let container_type value = v Container_type value
let container_name value = v Container_name value
let transform value = v Transform [ value ]
let transforms value = v Transform value
let rotate (value : Properties_intf.rotate_value) = v Rotate value
let scale (value : Properties_intf.scale) = v Scale value
let translate (value : Properties_intf.translate_value) = v Translate value
let perspective value = v Perspective value
let perspective_origin value = v Perspective_origin value
let padding_inline value = v Padding_inline value
let padding_inline_start value = v Padding_inline_start value
let padding_inline_end value = v Padding_inline_end value
let padding_block value = v Padding_block value
let padding_block_start value = v Padding_block_start value
let padding_block_end value = v Padding_block_end value
let margin_inline value = v Margin_inline [ value ]
let margin_inline_start value = v Margin_inline_start value
let margin_inline_end value = v Margin_inline_end value
let margin_block value = v Margin_block [ value ]
let margin_block_start value = v Margin_block_start value
let margin_block_end value = v Margin_block_end value
let will_change value = v Will_change value
let contain value = v Contain value
let isolation value = v Isolation value
let break_before value = v Break_before value
let break_after value = v Break_after value
let break_inside value = v Break_inside value

let break_of_page_break (value : page_break_value) : break_value =
  match value with
  | Auto -> Auto
  | Always -> Page
  | Avoid -> Avoid
  | Left -> Left
  | Right -> Right
  | Inherit -> Inherit

let break_inside_of_page_break (value : page_break_inside_value) :
    break_inside_value =
  match value with Auto -> Auto | Avoid -> Avoid | Inherit -> Inherit

let page_break_before value = v Break_before (break_of_page_break value)
let page_break_after value = v Break_after (break_of_page_break value)
let page_break_inside value = v Break_inside (break_inside_of_page_break value)
let columns value = v Columns value
let outline value = v Outline value
let outline_offset len = v Outline_offset len
let scroll_snap_type value = v Scroll_snap_type value
let scroll_snap_align value = v Scroll_snap_align value
let scroll_snap_stop value = v Scroll_snap_stop value
let scroll_behavior value = v Scroll_behavior value
let color_scheme value = v Color_scheme value

(* Alignment constructor helpers (declarations) *)
