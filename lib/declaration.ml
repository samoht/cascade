(** CSS declaration types and parser. *)

include Declaration_intf
open Properties
open Values

(* Re-export pp_property from Properties module *)
let pp_property = pp_property

(* Extract metadata from a declaration *)
let rec meta_of_declaration : declaration -> meta option = function
  | Declaration
      { property = Custom_property _; value = Custom_value { meta; _ }; _ } ->
      meta
  | Declaration _ -> None
  | Theme_guarded { decl; _ } -> meta_of_declaration decl

(* Smart constructor for declarations *)
let v ?(important = false) property value =
  Declaration { property; value; important }

let unknown_property ?(important = false) name value =
  let components = Cursor.remaining (Cursor.of_string value) in
  v ~important (Unknown_property name) components

(* Helper to mark a declaration as important *)
let rec important = function
  | Declaration { property; value; _ } ->
      Declaration { property; value; important = true }
  | Theme_guarded g -> Theme_guarded { g with decl = important g.decl }

(* Helper for raw custom properties - primarily for internal use *)

let custom_property ?layer name value =
  (* Validate that this is a proper CSS variable name. Custom-property names are
     dashed idents starting with [--] and a non-empty name body. *)
  if not (String.length name > 2 && String.sub name 0 2 = "--") then
    failwith
      (String.concat ""
         [
           "custom_property: ";
           name;
           " is not a valid CSS variable name (must start with -- and include \
            a name)";
         ]);
  (* Parse the value into a CSS Syntax 3 component stream so the declaration
     never carries a raw author string; the printer can then re-serialise with
     the active [Pp] context (handles minification). *)
  let components = Cursor.remaining (Cursor.of_string value) in
  v (Custom_property name)
    (Custom_value { value = Tokens components; layer; meta = None })

(* Access the layer associated with a custom declaration, if any *)
let rec custom_declaration_layer = function
  | Declaration
      { property = Custom_property _; value = Custom_value { layer; _ }; _ } ->
      layer
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

let is_decl_value_stop = function
  | Component.Preserved { kind = Token.Semicolon | Token.Delim "!"; _ } -> true
  | _ -> false

let value_components t =
  let rec take acc = function
    | [] -> List.rev acc
    | cv :: _ when is_decl_value_stop cv -> List.rev acc
    | cv :: rest -> take (cv :: acc) rest
  in
  take [] (Cursor.remaining t)

let rec component_is_complete = function
  | Component.Preserved _ -> true
  | Component.Block { node = { closed; value; _ }; _ } ->
      closed && List.for_all component_is_complete value
  | Component.Func { node = { terminated; arguments; _ }; _ } ->
      terminated && List.for_all component_is_complete arguments

let validate_complete_declaration_value t =
  if not (List.for_all component_is_complete (value_components t)) then
    Cursor.err_invalid t "unterminated component value"

let rec component_has_curly_block = function
  | Component.Block { node = { opening = Token.Curly; _ }; _ } -> true
  | Component.Block { node = { value; _ }; _ } ->
      List.exists component_has_curly_block value
  | Component.Func { node = { arguments; _ }; _ } ->
      List.exists component_has_curly_block arguments
  | Component.Preserved _ -> false

let reject_curly_block_value t =
  if List.exists component_has_curly_block (value_components t) then
    Cursor.err_invalid t "curly block in declaration value"

let rec component_has_unterminated_string = function
  | Component.Preserved { kind = Token.String { terminated = false; _ }; _ }
  | Component.Preserved { kind = Token.Bad_string; _ } ->
      true
  | Component.Block { node = { value; _ }; _ } ->
      List.exists component_has_unterminated_string value
  | Component.Func { node = { arguments; _ }; _ } ->
      List.exists component_has_unterminated_string arguments
  | Component.Preserved _ -> false

let reject_unterminated_string_value t =
  if List.exists component_has_unterminated_string (value_components t) then
    Cursor.err_invalid t "unterminated string in declaration value"

let css_wide_keywords =
  [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ]

let is_css_wide_keyword value =
  List.mem (String.lowercase_ascii value) css_wide_keywords

let is_css_wide_value value =
  let reader = Cursor.of_string value in
  match Properties.read_css_wide reader with
  | _ ->
      Cursor.ws reader;
      Cursor.is_done reader
  | exception Cursor.Parse_error _ -> false

let is_ws_component = function
  | Component.Preserved { kind = Token.Whitespace; _ } -> true
  | _ -> false

let invalid_var_arguments arguments =
  match
    List.filter (fun component -> not (is_ws_component component)) arguments
  with
  | Component.Preserved { kind = Token.Ident name; _ } :: _
    when String.length name >= 2 && name.[0] = '-' && name.[1] = '-' ->
      false
  | _ -> true

let rec components_have_invalid_var components =
  List.exists component_has_invalid_var components

and component_has_invalid_var = function
  | Component.Func { node = { name; arguments; terminated; _ }; _ }
    when String.lowercase_ascii name = "var" ->
      (not terminated)
      || invalid_var_arguments arguments
      || components_have_invalid_var arguments
  | Component.Func { node = { arguments; _ }; _ } ->
      components_have_invalid_var arguments
  | Component.Block { node = { value; _ }; _ } ->
      components_have_invalid_var value
  | Component.Preserved _ -> false

let raw_value_has_invalid_var raw_value =
  Cursor.of_string raw_value |> Cursor.remaining |> components_have_invalid_var

let raw_value_contains_var raw_value =
  (* CSS Custom Properties Level 1 section 3: a top-level [var()] in a
     declaration value makes the typed reader unable to validate the substituted
     result at parse time, so cascade preserves the value verbatim. A nested
     [var()] inside another function (e.g. [attr(name type(<color>), var(--fb,
     red))]) doesn't extend that leniency to the surrounding tokens - it's only
     relevant if it's a top-level component of the value. *)
  let is_top_level_var = function
    | Component.Func { node = { name; _ }; _ }
      when String.lowercase_ascii name = "var" ->
        true
    | _ -> false
  in
  Cursor.of_string raw_value |> Cursor.remaining |> List.exists is_top_level_var

let value_has_css_wide_mix value =
  let trimmed = String.trim value in
  (not (is_css_wide_keyword trimmed))
  &&
  let components = Cursor.remaining (Cursor.of_string trimmed) in
  List.exists
    (function
      | Component.Preserved { kind = Token.Ident ident; _ } ->
          is_css_wide_keyword ident
      | _ -> false)
    components

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

let is_plain_property_name name =
  let is_start = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | '-' -> true
    | _ -> false
  in
  let is_continue = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
    | _ -> false
  in
  let len = String.length name in
  len > 0
  && is_start name.[0]
  && (not (len = 1 && name.[0] = '-'))
  && (not (len >= 2 && name.[0] = '-' && name.[1] >= '0' && name.[1] <= '9'))
  && String.for_all is_continue name

let validate_printable_property_name t name =
  if not (is_plain_property_name name) then
    Cursor.err_invalid t ("unprintable property name: " ^ name)

let is_vendor_extension_property_name name =
  let len = String.length name in
  let rec has_vendor_separator i =
    i < len
    &&
    if name.[i] = '-' then i > 1 && i < len - 1 else has_vendor_separator (i + 1)
  in
  len > 0 && name.[0] = '-' && has_vendor_separator 1

let validate_unknown_property_name t name =
  validate_printable_property_name t name;
  if
    String.length name > 0
    && name.[0] = '-'
    && not (is_vendor_extension_property_name name)
  then Cursor.err_invalid t ("invalid vendor extension property name: " ^ name)

(** Check if a declaration is marked as important *)
let rec is_important = function
  | Declaration { important; _ } -> important
  | Theme_guarded { decl; _ } -> is_important decl

let is_decl_unknown_property_name name =
  let r = Cursor.of_string name in
  match read_any_property r with
  | Prop (Unknown_property _) ->
      Cursor.ws r;
      Cursor.is_done r
  | Prop _ -> false
  | exception Cursor.Parse_error _ -> (
      (* [read_any_property] now rejects unrecognized names; treat them as
         unknown if the input was otherwise a well-formed ident. *)
      let r = Cursor.of_string name in
      match Cursor.ident r with
      | _ ->
          Cursor.ws r;
          Cursor.is_done r
      | exception Cursor.Parse_error _ -> false)

(** [is_invalid decl] is [true] when [decl]'s typed value is a known
    spec-violation cascade detected at parse time. The minify-time
    [Optimize.drop_invalid] pass removes such declarations. *)
let rec is_invalid = function
  | Declaration { property = Unknown_property name; _ } ->
      is_decl_unknown_property_name name
      && not (is_vendor_extension_property_name name)
  | Declaration { property; value; _ } ->
      Properties.is_invalid_value property value
  | Theme_guarded { decl; _ } -> is_invalid decl

let color_opt_is_color_4 = function
  | Some c -> Values.color_is_color_4 c
  | None -> false

let rec shadow_uses_color_4 : Properties.shadow -> bool = function
  | Shadow { color; _ } -> color_opt_is_color_4 color
  | List shs -> List.exists shadow_uses_color_4 shs
  | _ -> false

let text_shadow_uses_color_4 : Properties.text_shadow -> bool = function
  | Text_shadow { color; _ } -> color_opt_is_color_4 color
  | _ -> false

let border_uses_color_4 : Properties.border -> bool = function
  | Shorthand { color; _ } -> color_opt_is_color_4 color
  | _ -> false

let outline_uses_color_4 : Properties.outline -> bool = function
  | Shorthand { color; _ } -> color_opt_is_color_4 color
  | _ -> false

let logical_color_uses_color_4 : Properties.logical_border_color -> bool =
  function
  | Single c -> Values.color_is_color_4 c
  | Pair (a, b) -> Values.color_is_color_4 a || Values.color_is_color_4 b
  | _ -> false

let rec gradient_stop_uses_color_4 : Properties.gradient_stop -> bool = function
  | Color_percentage (c, _, _) | Color_length (c, _, _) ->
      Values.color_is_color_4 c
  | List stops -> List.exists gradient_stop_uses_color_4 stops
  | _ -> false

let rec background_image_uses_color_4 : Properties.background_image -> bool =
  function
  | Linear_gradient (_, stops)
  | Repeating_linear_gradient (_, stops)
  | Webkit_linear_gradient (_, stops)
  | Webkit_repeating_linear_gradient (_, stops)
  | Moz_linear_gradient (_, stops)
  | Moz_repeating_linear_gradient (_, stops)
  | O_linear_gradient (_, stops)
  | O_repeating_linear_gradient (_, stops)
  | Radial_gradient (_, stops)
  | Repeating_radial_gradient (_, stops)
  | Webkit_radial_gradient (_, stops)
  | Webkit_repeating_radial_gradient (_, stops)
  | Moz_radial_gradient (_, stops)
  | Moz_repeating_radial_gradient (_, stops)
  | O_radial_gradient (_, stops)
  | O_repeating_radial_gradient (_, stops)
  | Conic_gradient (_, stops)
  | Repeating_conic_gradient (_, stops) ->
      List.exists gradient_stop_uses_color_4 stops
  | List imgs -> List.exists background_image_uses_color_4 imgs
  | _ -> false

let rec filter_uses_color_4 : Properties.filter -> bool = function
  | Drop_shadow sh -> shadow_uses_color_4 sh
  | List fs -> List.exists filter_uses_color_4 fs
  | _ -> false

let property_value_uses_color_4 (type a) (property : a Properties.property)
    (value : a) : bool =
  match property with
  | Color -> Values.color_is_color_4 value
  | Background_color -> Values.color_is_color_4 value
  | Border_color -> Values.color_is_color_4 value
  | Border_top_color -> Values.color_is_color_4 value
  | Border_right_color -> Values.color_is_color_4 value
  | Border_bottom_color -> Values.color_is_color_4 value
  | Border_left_color -> Values.color_is_color_4 value
  | Border_inline_start_color -> Values.color_is_color_4 value
  | Border_inline_end_color -> Values.color_is_color_4 value
  | Outline_color -> Values.color_is_color_4 value
  | Text_decoration_color -> Values.color_is_color_4 value
  | Text_emphasis_color -> Values.color_is_color_4 value
  | Accent_color -> Values.color_is_color_4 value
  | Caret_color -> Values.color_is_color_4 value
  | Webkit_tap_highlight_color -> Values.color_is_color_4 value
  | Webkit_text_decoration_color -> Values.color_is_color_4 value
  | Border_inline_color -> logical_color_uses_color_4 value
  | Box_shadow -> shadow_uses_color_4 value
  | Text_shadow -> List.exists text_shadow_uses_color_4 value
  | Border -> border_uses_color_4 value
  | Border_top -> border_uses_color_4 value
  | Border_right -> border_uses_color_4 value
  | Border_bottom -> border_uses_color_4 value
  | Border_left -> border_uses_color_4 value
  | Border_block -> border_uses_color_4 value
  | Column_rule -> border_uses_color_4 value
  | Outline -> outline_uses_color_4 value
  | Background_image -> List.exists background_image_uses_color_4 value
  | Webkit_mask_image -> background_image_uses_color_4 value
  | Mask_image -> background_image_uses_color_4 value
  | Filter -> filter_uses_color_4 value
  | Webkit_filter -> filter_uses_color_4 value
  | Ms_filter -> filter_uses_color_4 value
  | Backdrop_filter -> filter_uses_color_4 value
  | Webkit_backdrop_filter -> filter_uses_color_4 value
  | _ -> false

let rec value_uses_color_4 = function
  | Theme_guarded { decl; _ } -> value_uses_color_4 decl
  | Declaration { property; value; _ } ->
      property_value_uses_color_4 property value

let length_list_has_runtime_subst lengths =
  List.exists Values.length_has_runtime_subst lengths

let property_value_uses_runtime_subst (type a)
    (property : a Properties.property) (value : a) : bool =
  match property with
  | Margin_top -> Values.length_has_runtime_subst value
  | Margin_right -> Values.length_has_runtime_subst value
  | Margin_bottom -> Values.length_has_runtime_subst value
  | Margin_left -> Values.length_has_runtime_subst value
  | Padding_top -> Values.length_has_runtime_subst value
  | Padding_right -> Values.length_has_runtime_subst value
  | Padding_bottom -> Values.length_has_runtime_subst value
  | Padding_left -> Values.length_has_runtime_subst value
  | Border_top_left_radius -> Values.length_has_runtime_subst value
  | Border_top_right_radius -> Values.length_has_runtime_subst value
  | Border_bottom_left_radius -> Values.length_has_runtime_subst value
  | Border_bottom_right_radius -> Values.length_has_runtime_subst value
  | Outline_width -> Values.length_has_runtime_subst value
  | Margin -> length_list_has_runtime_subst value
  | Padding -> length_list_has_runtime_subst value
  | Inset -> length_list_has_runtime_subst value
  | _ -> false

let rec value_uses_runtime_subst = function
  | Theme_guarded { decl; _ } -> value_uses_runtime_subst decl
  | Declaration { property; value; _ } ->
      property_value_uses_runtime_subst property value

(** Get the property name as a string from a declaration *)
let rec property_name decl =
  let ctx =
    {
      Pp.minify = true;
      level = 0;
      indent = None;
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
  | Theme_guarded { decl; _ } -> property_name decl

let pp_value = Properties.pp_value

let rec string_of_value ?(minify = true) ?(inline = false) decl =
  let ctx =
    {
      Pp.minify;
      level = 0;
      indent = None;
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
  | Theme_guarded { decl; _ } -> string_of_value ~minify ~inline decl

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

let list_style_try_slot parse slot seen r =
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
      false

let parse_list_style_slots slots =
  let rec parse seen r =
    Cursor.ws r;
    if Cursor.is_done r then seen <> []
    else List.exists (fun slot -> list_style_try_slot parse slot seen r) slots
  in
  parse

let read_list_style_shorthand t =
  let raw = Cursor.lookahead (Cursor.consume_to_decl_end ~trim:true) t in
  let lower = String.lowercase_ascii raw in
  let is_valid_var () =
    let r = Cursor.of_string raw in
    match
      Values.read_var (fun r -> Cursor.consume_to_decl_end ~trim:true r) r
    with
    | (_ : string var) ->
        Cursor.ws r;
        Cursor.is_done r
    | exception Cursor.Parse_error _ -> false
  in
  if is_css_wide_keyword lower || is_valid_var () then (
    ignore (Cursor.consume_to_decl_end ~trim:true t);
    raw)
  else
    let slots : list_style_slot list = [ Position; Image; Type ] in
    let parse = parse_list_style_slots slots in
    if not (parse [] (Cursor.of_string raw)) then
      Cursor.err_invalid t "invalid list-style shorthand";
    ignore (Cursor.consume_to_decl_end ~trim:true t);
    raw

let read_text_decoration_lines t =
  let lines = Cursor.list ~at_least:1 read_text_decoration_line t in
  let is_none (line : text_decoration_line) =
    match line with None -> true | _ -> false
  in
  let is_error_kind (line : text_decoration_line) =
    match line with Spelling_error | Grammar_error -> true | _ -> false
  in
  if List.exists is_none lines && List.length lines > 1 then
    Cursor.err_invalid t "none cannot be combined with text-decoration lines";
  (* CSS Text Decoration 4 section 2.1: [<text-decoration-line>] is [none |
     [underline || overline || line-through || blink] | spelling-error |
     grammar-error]. [spelling-error] and [grammar-error] are alternatives to
     the [||] group, not members of it, so they cannot be combined with any
     other line. *)
  if List.exists is_error_kind lines && List.length lines > 1 then
    Cursor.err_invalid t
      "spelling-error and grammar-error cannot be combined with other \
       text-decoration lines";
  let rec duplicates = function
    | [] -> false
    | x :: xs -> List.mem x xs || duplicates xs
  in
  if duplicates lines then Cursor.err_invalid t "duplicate text-decoration-line";
  lines

let read_shape_outside t =
  let raw = Cursor.lookahead (Cursor.consume_to_decl_end ~trim:true) t in
  let accept_single () =
    Cursor.skip t;
    Cursor.expect_eof t;
    raw
  in
  let accept_var () =
    let _ : string var =
      Values.read_var
        (fun inner -> Cursor.consume_remaining_as_string ~trim:true inner)
        t
    in
    Cursor.expect_eof t;
    raw
  in
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Ident keyword; _ })
    when is_css_wide_keyword keyword ->
      accept_single ()
  | Some (Component.Preserved { kind = Token.Ident "none"; _ }) ->
      accept_single ()
  | Some
      (Component.Func
         { node = { name = "var"; terminated = true; arguments = []; _ }; _ })
    ->
      Cursor.err_invalid t "empty var()"
  | Some (Component.Func { node = { name = "var"; terminated = true; _ }; _ })
    ->
      accept_var ()
  | Some (Component.Func { node = { name = "circle"; terminated; _ }; _ })
    when terminated ->
      (* CSS Shapes 1 section 3.1: [circle()] is valid (both [<shape-radius>]
         and [at <position>] are optional). *)
      accept_single ()
  | Some
      (Component.Func { node = { name = "inset"; arguments; terminated }; _ })
    when terminated && arguments <> [] ->
      accept_single ()
  | Some (Component.Func { node = { name = "inset"; _ }; _ }) ->
      Cursor.err_invalid t "empty basic shape"
  | _ -> Cursor.err_invalid t ("invalid shape-outside: " ^ raw)

let read_shorthand_line_height r =
  Cursor.one_of
    [
      (fun r -> ignore (read_length r : length));
      (fun r -> ignore (read_percentage r : percentage));
      (fun r -> ignore (Cursor.number r : float));
      (fun r -> ignore (Cursor.enum "font line-height" [ ("normal", ()) ] r));
    ]
    r

let generic_font_family_keywords =
  [
    "sans-serif";
    "serif";
    "monospace";
    "cursive";
    "fantasy";
    "system-ui";
    "ui-sans-serif";
    "ui-serif";
    "ui-monospace";
    "ui-rounded";
    "emoji";
    "math";
    "fangsong";
  ]

let long_generic_family_start r =
  let is_ws = function
    | Component.Preserved { kind = Token.Whitespace; _ } -> true
    | _ -> false
  in
  let is_comma = function
    | Component.Preserved { kind = Token.Comma; _ } -> true
    | _ -> false
  in
  let rec drop_while p = function
    | x :: rest when p x -> drop_while p rest
    | l -> l
  in
  let item =
    Cursor.remaining r |> drop_while is_ws |> List.to_seq
    |> Seq.take_while (fun cv -> not (is_comma cv))
    |> List.of_seq
    |> List.filter (fun cv -> not (is_ws cv))
  in
  match item with
  | Component.Preserved { kind = Token.Ident first; _ } :: _ :: _ ->
      List.mem (String.lowercase_ascii first) generic_font_family_keywords
  | _ -> false

let read_optional_line_height r =
  match Cursor.peek_delim r with
  | Some '/' ->
      Cursor.skip r;
      read_shorthand_line_height r
  | _ -> ()

let read_font_shorthand_size_tail r =
  let _ = read_font_size r in
  read_optional_line_height r;
  if long_generic_family_start r then
    Cursor.err_invalid r "generic font family must be a standalone family item";
  ignore (read_font_family r : font_family);
  Cursor.ws r;
  Cursor.expect_eof r

let font_shorthand_prefix_ident = function
  | Some
      ( "italic" | "oblique" | "normal" | "small-caps" | "bold" | "bolder"
      | "lighter" | "condensed" | "expanded" ) ->
      true
  | _ -> false

let read_font_shorthand_step r =
  Cursor.ws r;
  if Cursor.is_done r then `Done false
  else if font_shorthand_prefix_ident (Cursor.peek_ident r) then
    let _ = Cursor.ident r in
    `Continue
  else
    let before = Cursor.save r in
    match read_font_weight r with
    | _ -> `Continue
    | exception Cursor.Parse_error _ ->
        Cursor.restore r before;
        read_font_shorthand_size_tail r;
        `Done true

let read_font_shorthand_body r =
  let rec loop () =
    match read_font_shorthand_step r with
    | `Continue -> loop ()
    | `Done saw_size -> saw_size
  in
  loop ()

let is_system_font_keyword = function
  | "caption" | "icon" | "menu" | "message-box" | "small-caption" | "status-bar"
    ->
      true
  | _ -> false

let rec read_font_shorthand t =
  let raw = Cursor.consume_to_decl_end ~trim:true t in
  let lower = String.lowercase_ascii raw in
  if is_css_wide_keyword lower then raw
    (* CSS Fonts 4 2.4: a [font:] declaration may be a single system font
       keyword like [caption] / [icon] / [menu]. They are bare idents and skip
       the regular [font-size]-required shorthand structure. *)
  else if is_system_font_keyword (String.trim lower) then raw
  else
    let is_valid_var () =
      let r = Cursor.of_string raw in
      match Values.read_var read_font_shorthand r with
      | (_ : string var) ->
          Cursor.ws r;
          Cursor.is_done r
      | exception Cursor.Parse_error _ -> false
    in
    if is_valid_var () then raw
    else if
      (* CSS Cascade 5 section 7.3: a CSS-wide keyword stands alone, so it
         cannot appear inside a multi-value shorthand like [font: initial 16px
         serif]. *)
      value_has_css_wide_mix raw
    then Cursor.err_invalid t "CSS-wide keyword mixed with other values"
    else
      let r = Cursor.of_string raw in
      let saw_size =
        try read_font_shorthand_body r
        with Cursor.Parse_error _ ->
          Cursor.err_invalid t "invalid font shorthand"
      in
      if not saw_size then Cursor.err_invalid t "font shorthand missing size";
      raw

let read_grid_template_list t = read_grid_template t

(* Some properties (shape-margin, scroll-margin, padding, etc.) require a
   non-negative length-percentage. Detect a leading [-] number/percentage and
   reject before delegating to the typed reader. *)
let read_non_negative_length_percentage t =
  Values.read_length_percentage ~allow_negative:false ~with_keywords:false t

let read_nn_lp_or_global t =
  Cursor.enum "non-negative length-percentage"
    [
      ("inherit", (Length Inherit : length_percentage));
      ("initial", Length Initial);
      ("unset", Length Unset);
      ("revert", Length Revert);
      ("revert-layer", Length Revert_layer);
    ]
    ~default:read_non_negative_length_percentage t

let read_nn_length_or_global t =
  Cursor.enum "non-negative length"
    [
      ("inherit", (Inherit : length));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~default:(read_non_negative_length ~with_keywords:false)
    t

(* CSS Text 3 section 10: [letter-spacing] is [normal | <length>],
   [word-spacing] is [normal | <length-percentage>]; both accept negative
   values. *)
let read_normal_or_length name t =
  Cursor.enum name
    [
      ("normal", (Normal : length));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~default:(Values.read_length ~with_keywords:false)
    t

let read_letter_spacing t = read_normal_or_length "letter-spacing" t
let read_word_spacing t = read_normal_or_length "word-spacing" t

(* CSS Logical Properties 1 3.5: [padding-block] / [padding-inline] are 2-value
   shorthands. A bare CSS-wide keyword counts as a single value (not a list
   element). *)
let read_padding_logical_shorthand t =
  let read_global_singleton t =
    let kw : length =
      Cursor.enum "padding logical"
        [
          ("inherit", (Inherit : length));
          ("initial", Initial);
          ("unset", Unset);
          ("revert", Revert);
          ("revert-layer", Revert_layer);
        ]
        t
    in
    [ kw ]
  in
  let read_lengths t =
    Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
      (Values.read_length ~allow_negative:false ~with_keywords:false)
      t
  in
  Cursor.one_of [ read_global_singleton; read_lengths ] t

let read_scroll_margin_length t =
  Cursor.enum "scroll-margin length"
    [
      ("inherit", (Inherit : length));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~default:(fun t ->
      match read_non_negative_length ~with_keywords:false t with
      | Pct _ -> Cursor.err_invalid t "scroll-margin percentage"
      | length -> length)
    t

let read_scroll_padding_length t =
  Cursor.enum "scroll-padding length"
    [
      ("auto", (Auto : length));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~default:(read_non_negative_length ~with_keywords:false)
    t

let rec read_length_or_css_wide t =
  Cursor.enum_or_calls "length"
    [
      ("inherit", (Inherit : length));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (read_var read_length_or_css_wide t)) ]
    ~default:(read_length ~with_keywords:false)
    t

let rec read_text_decoration_thickness t =
  Cursor.enum_or_calls "text-decoration-thickness"
    [
      ("auto", (Auto : length));
      ("from-font", From_font);
      ("hairline", Hairline);
      ("thin", Thin);
      ("medium", Medium);
      ("thick", Thick);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:
      [
        ("var", fun t -> Var (read_var read_text_decoration_thickness t));
        ( "calc",
          fun t ->
            Calc (read_calc (read_non_negative_length ~with_keywords:false) t)
        );
      ]
    ~default:(read_non_negative_length ~with_keywords:false)
    t

let read_border_radius_radii t =
  let rec loop acc count =
    if count >= 4 then List.rev acc
    else
      match Cursor.option read_non_negative_length_percentage t with
      | None -> List.rev acc
      | Some lp -> loop (lp :: acc) (count + 1)
  in
  match loop [] 0 with
  | [] -> Cursor.err_expected t "<length-percentage>"
  | radii -> radii

let read_border_radius_vertical t =
  match Cursor.peek_delim t with
  | Some '/' ->
      Cursor.skip t;
      Cursor.ws t;
      Some (read_border_radius_radii t)
  | _ -> None

(* CSS Backgrounds and Borders 3 §5: [border-radius = <length-percentage>{1,4}
   [/ <length-percentage>{1,4}]?]. Reads 1-4 horizontal radii then, after [/],
   1-4 vertical radii. *)
let rec read_border_radius (t : Cursor.t) : Properties.border_radius =
  Cursor.ws t;
  Cursor.enum_or_var "border-radius"
    [
      ("inherit", (Properties.Inherit : Properties.border_radius));
      ("initial", Properties.Initial);
      ("unset", Properties.Unset);
      ("revert", Properties.Revert);
      ("revert-layer", Properties.Revert_layer);
    ]
    ~var:(fun t ->
      (Properties.Var (Values.read_var read_border_radius t)
        : Properties.border_radius))
    ~default:(fun t ->
      let horizontal = read_border_radius_radii t in
      Cursor.ws t;
      let vertical = read_border_radius_vertical t in
      Properties.Radius { horizontal; vertical })
    t

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
  let rec align_to_justify (a : align_self) : justify_self =
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
    | Inherit -> Inherit
    | Initial -> Initial
    | Unset -> Unset
    | Revert -> Revert
    | Revert_layer -> Revert_layer
    | Var v -> Var (align_var_to_justify v)
  and align_var_to_justify (v : align_self var) : justify_self var =
    let fallback : justify_self fallback =
      match v.fallback with
      | None -> None
      | Empty -> Empty
      | Empty2 -> Empty2
      | Var_fallback name -> Var_fallback name
      | Syntax_fallback value -> Syntax_fallback value
      | Fallback value -> Fallback (align_to_justify value)
    in
    {
      name = v.name;
      fallback;
      default = Option.map align_to_justify v.default;
      layer = v.layer;
      meta = v.meta;
    }
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
      level = 0;
      indent = None;
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
  | Custom_property name ->
      Cursor.err_invalid t
        ("custom property read through regular property: " ^ name)
  | Unknown_property name ->
      Cursor.err_invalid t
        ("unknown property read through typed parser: " ^ name)
  | Color -> v Color (read_color t)
  | Background_color -> v Background_color (read_color t)
  | Border_color -> v Border_color (read_color t)
  | Outline_color -> v Outline_color (read_color t)
  | Border_top_color -> v Border_top_color (read_color t)
  | Border_right_color -> v Border_right_color (read_color t)
  | Border_bottom_color -> v Border_bottom_color (read_color t)
  | Border_left_color -> v Border_left_color (read_color t)
  (* Length/percentage properties *)
  | Width -> v Width (read_length_percentage ~allow_negative:false t)
  | Height -> v Height (read_length_percentage ~allow_negative:false t)
  | Min_width -> v Min_width (read_length_percentage ~allow_negative:false t)
  | Min_height -> v Min_height (read_length_percentage ~allow_negative:false t)
  | Max_width -> v Max_width (read_length_percentage ~allow_negative:false t)
  | Max_height -> v Max_height (read_length_percentage ~allow_negative:false t)
  | Inline_size ->
      v Inline_size (read_length_percentage ~allow_negative:false t)
  | Min_inline_size ->
      v Min_inline_size (read_length_percentage ~allow_negative:false t)
  | Max_inline_size ->
      v Max_inline_size (read_length_percentage ~allow_negative:false t)
  | Block_size -> v Block_size (read_length_percentage ~allow_negative:false t)
  | Min_block_size ->
      v Min_block_size (read_length_percentage ~allow_negative:false t)
  | Max_block_size ->
      v Max_block_size (read_length_percentage ~allow_negative:false t)
  | Font_size -> v Font_size (Properties.read_font_size t)
  | Border_radius -> v Border_radius (read_border_radius t)
  | Border_top_left_radius ->
      v Border_top_left_radius (read_nn_length_or_global t)
  | Border_top_right_radius ->
      v Border_top_right_radius (read_nn_length_or_global t)
  | Border_bottom_left_radius ->
      v Border_bottom_left_radius (read_nn_length_or_global t)
  | Border_bottom_right_radius ->
      v Border_bottom_right_radius (read_nn_length_or_global t)
  | Gap -> v Gap (Properties.read_gap t)
  | Column_gap -> v Column_gap (read_nn_length_or_global t)
  | Row_gap -> v Row_gap (read_nn_length_or_global t)
  (* Display and layout *)
  | Display -> v Display (read_display t)
  | Position -> v Position (read_position t)
  | Visibility -> v Visibility (read_visibility t)
  | Baseline_source -> v Baseline_source (read_baseline_source t)
  | Alignment_baseline -> v Alignment_baseline (read_alignment_baseline t)
  | Baseline_shift -> v Baseline_shift (read_baseline_shift t)
  | Overflow -> v Overflow (read_overflow t)
  | Overflow_x -> v Overflow_x (read_overflow_single t)
  | Overflow_y -> v Overflow_y (read_overflow_single t)
  | Overflow_block -> v Overflow_block (read_overflow_single t)
  | Overflow_inline -> v Overflow_inline (read_overflow_single t)
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
  | Source -> v Source (Properties.read_font_src t)
  | Text_align -> v Text_align (read_text_align t)
  | Text_transform -> v Text_transform (read_text_transform t)
  | White_space -> v White_space (read_white_space t)
  | Text_decoration -> v Text_decoration (read_text_decoration t)
  | Transform_origin -> v Transform_origin (read_transform_origin t)
  | Transform_box -> v Transform_box (read_transform_box t)
  (* Flexbox *)
  | Flex_direction -> v Flex_direction (read_flex_direction t)
  | Flex_wrap -> v Flex_wrap (read_flex_wrap t)
  | Flex_flow -> v Flex_flow (read_flex_flow t)
  | Flex -> v Flex (read_flex t)
  | Flex_grow -> v Flex_grow (Properties.read_flex_factor t)
  | Flex_shrink -> v Flex_shrink (Properties.read_flex_factor t)
  | Flex_basis -> v Flex_basis (read_flex_basis t)
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
  | Counter_reset -> v Counter_reset (read_counter_set t)
  | Counter_increment -> v Counter_increment (read_counter_set t)
  (* Other properties *)
  | Z_index -> v Z_index (Properties.read_z_index t)
  | Opacity -> v Opacity (Properties.read_opacity t)
  | Cursor -> v Cursor (read_cursor t)
  | Interactivity -> v Interactivity (read_interactivity t)
  | Caret_animation -> v Caret_animation (read_caret_animation t)
  | Caret_shape -> v Caret_shape (read_caret_shape t)
  | Caret -> v Caret (read_caret t)
  | Interest_delay -> v Interest_delay (read_interest_delay t)
  | Interest_delay_start ->
      v Interest_delay_start (read_interest_delay ~longhand:true t)
  | Interest_delay_end ->
      v Interest_delay_end (read_interest_delay ~longhand:true t)
  | Nav_up -> v Nav_up (read_nav t)
  | Nav_right -> v Nav_right (read_nav t)
  | Nav_down -> v Nav_down (read_nav t)
  | Nav_left -> v Nav_left (read_nav t)
  | Box_sizing -> v Box_sizing (read_box_sizing t)
  | Field_sizing -> v Field_sizing (read_field_sizing t)
  | Caption_side -> v Caption_side (read_caption_side t)
  | User_select -> v User_select (read_user_select t)
  | Webkit_user_select -> v Webkit_user_select (read_user_select t)
  | Moz_user_select -> v Moz_user_select (read_user_select t)
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
  | Padding_left -> v Padding_left (read_nn_length_or_global t)
  | Padding_right -> v Padding_right (read_nn_length_or_global t)
  | Padding_top -> v Padding_top (read_nn_length_or_global t)
  | Padding_bottom -> v Padding_bottom (read_nn_length_or_global t)
  | Padding_inline -> v Padding_inline (read_padding_logical_shorthand t)
  | Padding_inline_start -> v Padding_inline_start (read_nn_length_or_global t)
  | Padding_inline_end -> v Padding_inline_end (read_nn_length_or_global t)
  | Padding_block -> v Padding_block (read_padding_logical_shorthand t)
  | Padding_block_start -> v Padding_block_start (read_nn_length_or_global t)
  | Padding_block_end -> v Padding_block_end (read_nn_length_or_global t)
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
  | Text_underline_offset ->
      v Text_underline_offset (read_nn_length_or_global t)
  | Text_emphasis -> v Text_emphasis (read_text_emphasis t)
  | Text_emphasis_style -> v Text_emphasis_style (read_text_emphasis_style t)
  | Text_emphasis_color -> v Text_emphasis_color (read_color t)
  | Text_emphasis_position ->
      v Text_emphasis_position (read_text_emphasis_position t)
  | Text_emphasis_skip -> v Text_emphasis_skip (read_text_emphasis_skip t)
  | Text_orientation -> v Text_orientation (read_text_orientation t)
  | Letter_spacing -> v Letter_spacing (read_letter_spacing t)
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
  | Grid -> v Grid (read_grid t)
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
  | Border_inline_color -> v Border_inline_color (read_logical_border_color t)
  | Border_inline_style -> v Border_inline_style (read_border_style t)
  | Border_block_style -> v Border_block_style (read_border_style t)
  | Border_start_start_radius ->
      v Border_start_start_radius (read_nn_length_or_global t)
  | Border_start_end_radius ->
      v Border_start_end_radius (read_nn_length_or_global t)
  | Border_end_start_radius ->
      v Border_end_start_radius (read_nn_length_or_global t)
  | Border_end_end_radius ->
      v Border_end_end_radius (read_nn_length_or_global t)
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
  | Outline_width -> v Outline_width (read_nn_length_or_global t)
  | Outline_offset -> v Outline_offset (read_length_or_css_wide t)
  (* Forced color adjust *)
  | Forced_color_adjust -> v Forced_color_adjust (read_forced_color_adjust t)
  (* Scroll snap *)
  | Scroll_snap_type -> v Scroll_snap_type (read_scroll_snap_type t)
  (* Tab size *)
  | Tab_size -> v Tab_size (read_tab_size t)
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
  | Moz_orient -> v Moz_orient (read_moz_orient t)
  | Webkit_hyphens -> v Webkit_hyphens (read_hyphens t)
  (* Font properties *)
  | Font_feature_settings ->
      v Font_feature_settings (read_font_feature_settings t)
  | Font_variation_settings ->
      v Font_variation_settings (read_font_variation_settings t)
  | Font_stretch -> v Font_stretch (read_font_stretch t)
  | Font_optical_sizing -> v Font_optical_sizing (read_font_optical_sizing t)
  | Font_kerning -> v Font_kerning (read_font_kerning t)
  | Font_language_override ->
      v Font_language_override (read_font_language_override t)
  | Font_synthesis_style -> v Font_synthesis_style (read_font_synthesis_style t)
  | Font_synthesis_weight ->
      v Font_synthesis_weight (read_font_synthesis_weight t)
  | Font_synthesis_small_caps ->
      v Font_synthesis_small_caps (read_font_synthesis_small_caps t)
  | Font_synthesis_position ->
      v Font_synthesis_position (read_font_synthesis_position t)
  | Font_variant_ligatures ->
      v Font_variant_ligatures (read_font_variant_ligatures t)
  | Font_variant_caps -> v Font_variant_caps (read_font_variant_caps t)
  | Font_variant_numeric -> v Font_variant_numeric (read_font_variant_numeric t)
  | Font_variant_position ->
      v Font_variant_position (read_font_variant_position t)
  | Font_variant_east_asian ->
      v Font_variant_east_asian (read_font_variant_east_asian t)
  (* Text properties *)
  | Text_indent -> v Text_indent (read_text_indent_value t)
  | Text_overflow -> v Text_overflow (read_text_overflow t)
  | Text_wrap -> v Text_wrap (read_text_wrap t)
  | Text_decoration_thickness ->
      v Text_decoration_thickness (read_text_decoration_thickness t)
  | Text_size_adjust -> v Text_size_adjust (read_text_size_adjust t)
  | Text_decoration_skip_ink ->
      v Text_decoration_skip_ink (read_text_decoration_skip_ink t)
  | Text_decoration_skip -> v Text_decoration_skip (read_text_decoration_skip t)
  | Text_decoration_skip_self ->
      v Text_decoration_skip_self (read_text_decoration_skip_self t)
  | Text_decoration_skip_box ->
      v Text_decoration_skip_box (read_text_decoration_skip_box t)
  | Text_decoration_skip_inset ->
      v Text_decoration_skip_inset (read_text_decoration_skip_inset t)
  | Text_decoration_skip_spaces ->
      v Text_decoration_skip_spaces (read_text_decoration_skip_spaces t)
  (* Word/text breaking *)
  | Word_break -> v Word_break (read_word_break t)
  | Overflow_wrap -> v Overflow_wrap (read_overflow_wrap t)
  | Line_break -> v Line_break (read_line_break t)
  | Hyphens -> v Hyphens (read_hyphens t)
  | Word_spacing -> v Word_spacing (read_word_spacing t)
  (* Container properties *)
  | Container_type -> v Container_type (read_container_type t)
  | Container_name -> v Container_name (read_container_name t)
  | Container -> v Container (read_container_shorthand t)
  (* Anchor positioning properties. [anchor-name] / [position-anchor] take a
     [<dashed-ident>] (ident that begins with [--]), and
     [position-try-fallbacks] is a comma-separated list of the same. *)
  | Anchor_name -> v Anchor_name (read_anchor_name t)
  | Position_anchor -> v Position_anchor (read_position_anchor t)
  | Position_try_fallbacks ->
      v Position_try_fallbacks (Properties.read_position_try_fallbacks t)
  | Position_try_order -> v Position_try_order (read_position_try_order t)
  | Position_visibility -> v Position_visibility (read_position_visibility t)
  | Position_area -> v Position_area (read_position_area t)
  | Shape_outside -> v Shape_outside (read_shape_outside t)
  | Shape_margin -> v Shape_margin (read_nn_lp_or_global t)
  | Shape_image_threshold ->
      v Shape_image_threshold (read_shape_image_threshold t)
  | Overflow_clip_margin -> v Overflow_clip_margin (read_overflow_clip_margin t)
  | Overflow_anchor -> v Overflow_anchor (read_overflow_anchor t)
  | Scrollbar_width -> v Scrollbar_width (read_scrollbar_width t)
  | Scrollbar_color -> v Scrollbar_color (read_scrollbar_color t)
  | Scrollbar_gutter -> v Scrollbar_gutter (read_scrollbar_gutter t)
  | Line_height_step -> v Line_height_step (read_nn_length_or_global t)
  | Font_palette -> v Font_palette (read_font_palette t)
  | Font_synthesis -> v Font_synthesis (read_font_synthesis t)
  | Text_wrap_mode -> v Text_wrap_mode (read_text_wrap_mode t)
  | Text_wrap_style -> v Text_wrap_style (read_text_wrap_style t)
  | Text_box_trim -> v Text_box_trim (read_text_box_trim t)
  | Text_underline_position ->
      v Text_underline_position (read_text_underline_position t)
  | Text_box_edge -> v Text_box_edge (read_text_box_edge t)
  | Text_box -> v Text_box (read_text_box t)
  | Inline_sizing -> v Inline_sizing (read_inline_sizing t)
  | Line_fit_edge -> v Line_fit_edge (read_line_fit_edge t)
  | Interpolate_size -> v Interpolate_size (read_interpolate_size t)
  | Min_intrinsic_sizing -> v Min_intrinsic_sizing (read_min_intrinsic_sizing t)
  | Ruby_align -> v Ruby_align (read_ruby_align t)
  | Ruby_merge -> v Ruby_merge (read_ruby_merge t)
  | Ruby_overhang -> v Ruby_overhang (read_ruby_overhang t)
  | Ruby_position -> v Ruby_position (read_ruby_position t)
  | Glyph_orientation_vertical ->
      v Glyph_orientation_vertical (read_glyph_orientation_vertical t)
  | Animation_timeline -> v Animation_timeline (read_animation_timeline t)
  | Animation_range -> v Animation_range (read_animation_range t)
  | Animation_range_start ->
      v Animation_range_start (read_animation_range_item t)
  | Animation_range_end -> v Animation_range_end (read_animation_range_item t)
  | Scroll_timeline -> v Scroll_timeline (read_timeline_shorthand t)
  | Scroll_timeline_name -> v Scroll_timeline_name (read_timeline_name t)
  | Scroll_timeline_axis -> v Scroll_timeline_axis (read_timeline_axis t)
  | View_transition_name -> v View_transition_name (read_view_transition_name t)
  | View_transition_class ->
      v View_transition_class (read_view_transition_class t)
  | Image_orientation -> v Image_orientation (read_image_orientation t)
  | Image_rendering -> v Image_rendering (read_image_rendering t)
  | Image_resolution -> v Image_resolution (read_image_resolution t)
  | Contain_intrinsic_size ->
      v Contain_intrinsic_size (read_contain_intrinsic_size t)
  | Contain_intrinsic_width ->
      v Contain_intrinsic_width (read_contain_intrinsic_longhand t)
  | Contain_intrinsic_height ->
      v Contain_intrinsic_height (read_contain_intrinsic_longhand t)
  | Contain_intrinsic_block_size ->
      v Contain_intrinsic_block_size (read_contain_intrinsic_longhand t)
  | Contain_intrinsic_inline_size ->
      v Contain_intrinsic_inline_size (read_contain_intrinsic_longhand t)
  | Margin_trim -> v Margin_trim (read_margin_trim t)
  | Offset_path -> v Offset_path (read_offset_path t)
  | Offset_distance -> v Offset_distance (read_nn_lp_or_global t)
  | Offset_rotate -> v Offset_rotate (read_offset_rotate t)
  | Font_size_adjust -> v Font_size_adjust (read_font_size_adjust t)
  | Font_variant_emoji -> v Font_variant_emoji (read_font_variant_emoji t)
  | Text_spacing_trim -> v Text_spacing_trim (read_text_spacing_trim t)
  | Hyphenate_limit_chars ->
      v Hyphenate_limit_chars (read_hyphenate_limit_chars t)
  | Initial_letter -> v Initial_letter (read_initial_letter t)
  | Initial_letter_align -> v Initial_letter_align (read_initial_letter_align t)
  | Initial_letter_wrap -> v Initial_letter_wrap (read_initial_letter_wrap t)
  | Dominant_baseline -> v Dominant_baseline (read_dominant_baseline t)
  | View_timeline_name -> v View_timeline_name (read_timeline_name t)
  | View_timeline_axis -> v View_timeline_axis (read_timeline_axis t)
  | View_timeline_inset -> v View_timeline_inset (read_timeline_inset t)
  | View_timeline -> v View_timeline (read_timeline_shorthand t)
  | Timeline_scope -> v Timeline_scope (read_timeline_name t)
  (* Transform properties *)
  | Perspective -> v Perspective (read_nn_length_or_global t)
  | Perspective_origin -> v Perspective_origin (read_perspective_origin t)
  | Transform_style -> v Transform_style (read_transform_style t)
  | Backface_visibility -> v Backface_visibility (read_backface_visibility t)
  | Rotate -> v Rotate (read_rotate_value t)
  | Scale -> v Scale (read_scale t)
  (* Object properties *)
  | Object_position -> v Object_position (read_position_value t)
  | Object_fit -> v Object_fit (read_object_fit t)
  | Object_view_box -> v Object_view_box (read_object_view_box t)
  (* Transition properties *)
  | Transition_duration ->
      v Transition_duration (read_duration_list read_duration t)
  | Transition_timing_function ->
      v Transition_timing_function (read_timing_function_list t)
  | Transition_delay -> v Transition_delay (read_duration_list read_time t)
  | Transition_property -> v Transition_property (read_transition_property t)
  | Transition_behavior ->
      v Transition_behavior (Properties.read_transition_behavior t)
  | Overlay -> v Overlay (read_overlay t)
  (* Will change *)
  | Will_change -> v Will_change (read_will_change t)
  (* Contain and isolation *)
  | Contain -> v Contain (read_contain t)
  | Isolation -> v Isolation (read_isolation t)
  (* Break properties *)
  | Break_before -> v Break_before (read_break_value t)
  | Break_after -> v Break_after (read_break_value t)
  | Break_inside -> v Break_inside (read_break_inside_value t)
  | Page_break_before -> v Page_break_before (read_page_break_value t)
  | Page_break_after -> v Page_break_after (read_page_break_value t)
  | Page_break_inside -> v Page_break_inside (read_page_break_inside_value t)
  | Page_size -> v Page_size (read_page_size t)
  | Columns -> v Columns (read_columns_value t)
  | Column_rule -> v Column_rule (read_border t)
  | Column_span -> v Column_span (read_column_span t)
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
  | Border_top -> v Border_top (read_border t)
  | Border_right -> v Border_right (read_border t)
  | Border_bottom -> v Border_bottom (read_border t)
  | Border_left -> v Border_left (read_border t)
  | Border_block -> v Border_block (read_border t)
  | Border_spacing -> v Border_spacing (read_border_spacing t)
  | Border_image -> v Border_image (read_border_image t)
  | Border_collapse -> v Border_collapse (read_border_collapse t)
  (* Clip and mask *)
  | Clip_path -> v Clip_path (read_clip_path t)
  | Mask -> v Mask (read_mask t)
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
  | Scroll_margin ->
      v Scroll_margin
        (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:4
           read_scroll_margin_length t)
  | Scroll_margin_top -> v Scroll_margin_top (read_scroll_margin_length t)
  | Scroll_margin_right -> v Scroll_margin_right (read_scroll_margin_length t)
  | Scroll_margin_bottom -> v Scroll_margin_bottom (read_scroll_margin_length t)
  | Scroll_margin_left -> v Scroll_margin_left (read_scroll_margin_length t)
  | Scroll_margin_inline ->
      v Scroll_margin_inline
        (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
           read_scroll_margin_length t)
  | Scroll_margin_inline_start ->
      v Scroll_margin_inline_start (read_scroll_margin_length t)
  | Scroll_margin_inline_end ->
      v Scroll_margin_inline_end (read_scroll_margin_length t)
  | Scroll_margin_block ->
      let lengths =
        Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
          read_scroll_margin_length t
      in
      (match lengths with
      | [ Zero; Zero ] -> Cursor.err_invalid t "duplicate zero scroll margin"
      | _ -> ());
      v Scroll_margin_block lengths
  | Scroll_margin_block_start ->
      v Scroll_margin_block_start (read_scroll_margin_length t)
  | Scroll_margin_block_end ->
      v Scroll_margin_block_end (read_scroll_margin_length t)
  | Scroll_padding ->
      v Scroll_padding
        (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:4
           read_scroll_padding_length t)
  | Scroll_padding_top -> v Scroll_padding_top (read_scroll_padding_length t)
  | Scroll_padding_right ->
      v Scroll_padding_right (read_scroll_padding_length t)
  | Scroll_padding_bottom ->
      v Scroll_padding_bottom (read_scroll_padding_length t)
  | Scroll_padding_left -> v Scroll_padding_left (read_scroll_padding_length t)
  | Scroll_padding_inline ->
      v Scroll_padding_inline
        (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
           read_scroll_padding_length t)
  | Scroll_padding_inline_start ->
      v Scroll_padding_inline_start (read_scroll_padding_length t)
  | Scroll_padding_inline_end ->
      v Scroll_padding_inline_end (read_scroll_padding_length t)
  | Scroll_padding_block ->
      v Scroll_padding_block
        (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
           read_scroll_padding_length t)
  | Scroll_padding_block_start ->
      v Scroll_padding_block_start (read_scroll_padding_length t)
  | Scroll_padding_block_end ->
      v Scroll_padding_block_end (read_scroll_padding_length t)
  | Overscroll_behavior ->
      let xs =
        Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
          read_overscroll_behavior t
      in
      v Overscroll_behavior xs
  | Overscroll_behavior_x ->
      v Overscroll_behavior_x (read_overscroll_behavior t)
  | Overscroll_behavior_y ->
      v Overscroll_behavior_y (read_overscroll_behavior t)
  | Overscroll_behavior_block ->
      v Overscroll_behavior_block (read_overscroll_behavior t)
  | Overscroll_behavior_inline ->
      v Overscroll_behavior_inline (read_overscroll_behavior t)
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
  | Stroke_width -> v Stroke_width (read_nn_length_or_global t)
  (* Direction and writing *)
  | Direction -> v Direction (read_direction t)
  | Unicode_bidi -> v Unicode_bidi (read_unicode_bidi t)
  | Writing_mode -> v Writing_mode (read_writing_mode t)
  | Text_combine_upright -> v Text_combine_upright (read_text_combine_upright t)
  (* Animation properties *)
  | Animation_name -> v Animation_name (read_animation_name t)
  | Animation_duration ->
      v Animation_duration (read_duration_list read_duration t)
  | Animation_timing_function ->
      v Animation_timing_function (read_timing_function_list t)
  | Animation_delay -> v Animation_delay (read_duration_list read_time t)
  | Animation_iteration_count ->
      v Animation_iteration_count (read_animation_iteration_count t)
  | Animation_direction -> v Animation_direction (read_animation_direction t)
  | Animation_fill_mode -> v Animation_fill_mode (read_animation_fill_mode t)
  | Animation_play_state -> v Animation_play_state (read_animation_play_state t)
  | Animation_composition ->
      v Animation_composition (read_animation_composition t)
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
  | Mask_border -> v Mask_border (read_border_image t)
  | Mask_size -> v Mask_size (read_background_size t)
  | Mask_position -> v Mask_position (read_background_position t)
  | Mask_repeat -> v Mask_repeat (read_background_repeat t)
  | Mask_clip -> v Mask_clip (read_mask_box t)
  | Mask_origin -> v Mask_origin (read_mask_box t)
  | Mask_type -> v Mask_type (read_mask_type t)
  | All -> v All (Properties.read_css_wide t)

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
  (* CSS Syntax 3 §4.3.7 lets [\X] escapes carry any code point into an ident,
     so the name may contain characters ([/], whitespace, etc.) that don't
     tokenize as a bare ident on a string round-trip. We trust the original
     lexer's tokenization: the only validation we still run is the
     [<dashed-ident>] prefix check. *)
  if String.length name <= 2 || name.[0] <> '-' || name.[1] <> '-' then
    Cursor.err_invalid t ("expected <dashed-ident>, got: " ^ name);
  Cursor.ws t;
  if not (Cursor.colon t) then Cursor.err_expected t "':'";
  Cursor.ws t;
  let raw_value = Cursor.consume_until_semicolon ~trim:true t in
  let value_str, is_important = split_custom_important raw_value in
  (* custom_property may raise Failure for invalid names like "--" *)
  try
    let decl =
      let custom_value =
        if is_font_family_var name then
          let trimmed = String.trim value_str in
          if String.length trimmed >= 4 && String.sub trimmed 0 4 = "var(" then
            read_custom_property_value (Cursor.of_string value_str)
          else
            read_custom_property_value ~font_family:true
              (Cursor.of_string value_str)
        else read_custom_property_value (Cursor.of_string value_str)
      in
      v (Custom_property name)
        (Custom_value { value = custom_value; layer = None; meta = None })
    in
    if is_important then important decl else decl
  with Failure msg -> Cursor.err_invalid t msg

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

(* Properties whose grammar allows multi-token values where a CSS-wide keyword
   can legitimately appear as a non-special ident. [animation-name] /
   [grid-area] / [will-change] / etc. accept arbitrary ident lists.
   [font-family] also takes a [<custom-ident>#] list, so a CSS-wide keyword
   inside the list is invalid CSS (CSS Cascade 5 §7.3) but upstream tools
   (lightningcss, csso) preserve the source verbatim. *)
let property_allows_keyword_as_ident = function
  | "animation-name" | "grid-area" | "grid-row" | "grid-column"
  | "grid-row-start" | "grid-row-end" | "grid-column-start" | "grid-column-end"
  | "will-change" | "view-transition-name" | "font-family" | "font" ->
      true
  | _ -> false

let validate_regular_property_raw t name raw_value =
  if
    (not (property_allows_keyword_as_ident name))
    && value_has_css_wide_mix raw_value
  then Cursor.err_invalid t "CSS-wide keyword mixed with other values";
  if name = "all" && not (is_css_wide_value raw_value) then
    Cursor.err_invalid t "all accepts only CSS-wide keywords";
  validate_legacy_page_break t name raw_value

(* Color functions cascade types directly; anything else (e.g. a vendor color
   function or a typed value cascade hasn't grown yet) is treated as a [color]
   declaration cascade should preserve verbatim. *)
let color_fallback_function raw_value =
  let components =
    Cursor.of_string raw_value |> Cursor.remaining
    |> List.filter (fun component -> not (is_ws_component component))
  in
  match components with
  | [ Component.Func { node = { name; _ }; _ } ] ->
      let fn = String.lowercase_ascii name in
      not
        (List.mem fn
           [
             "rgb";
             "rgba";
             "hsl";
             "hsla";
             "hwb";
             "lab";
             "lch";
             "oklab";
             "oklch";
             "color";
             "color-mix";
             "light-dark";
             "var";
           ])
  | _ -> false

let is_unsupported_color_fallback name raw_value =
  color_fallback_function raw_value
  &&
  match name with
  | "color" | "background-color" | "border-color" | "border-top-color"
  | "border-right-color" | "border-bottom-color" | "border-left-color"
  | "border-inline-start-color" | "border-inline-end-color"
  | "text-decoration-color" | "-webkit-text-decoration-color"
  | "-webkit-tap-highlight-color" | "outline-color" | "accent-color"
  | "caret-color" | "fill" | "stroke" ->
      true
  | _ -> false

let is_unknown_property_name = is_decl_unknown_property_name

(* The typed readers carry their own [Invalid] arms for spec-violations they
   detect ([Values.angle.Invalid] / [Properties.clip_path.Invalid]), so the
   declaration-level unknown fallback only needs to handle truly unknown
   properties or property-specific colour fallback edges. *)
let allows_unknown_fallback name raw_value =
  (not (raw_value_has_invalid_var raw_value))
  && (is_unknown_property_name name
     || is_unsupported_color_fallback name raw_value
     || raw_value_contains_var raw_value)

let read_font_src_declaration t raw_value =
  ignore raw_value;
  let decl = v Source (Properties.read_font_src t) in
  validate_no_extra_tokens t;
  let is_important = read_importance t in
  validate_no_extra_tokens t;
  if is_important then important decl else decl

let read_unknown_property_declaration t name =
  validate_unknown_property_name t name;
  validate_complete_declaration_value t;
  reject_curly_block_value t;
  reject_unterminated_string_value t;
  let raw_value = Cursor.consume_to_decl_end ~trim:true t in
  let is_important = read_importance t in
  validate_no_extra_tokens t;
  (match Cursor.peek_delim t with
  | Some '!' -> Cursor.err_invalid t "duplicate !important"
  | _ -> ());
  unknown_property ~important:is_important name raw_value

let read_typed_property_declaration t start =
  Cursor.restore t start;
  let (Prop prop_type) = read_any_property t in
  Cursor.ws t;
  if not (Cursor.colon t) then Cursor.err_expected t "':'";
  Cursor.ws t;
  (* CSS Syntax 3 §5.3.7 / §4.3.5 auto-close unterminated functions, brackets
     and strings at EOF. Typed readers consume the spec-recovered tokens; the
     declaration survives with the auto-closed shape. *)
  match prop_type with
  | Unknown_property name -> read_unknown_property_declaration t name
  | _ ->
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
  if String.equal name "src" then read_font_src_declaration t raw_value
  else
    try read_typed_property_declaration t start
    with Cursor.Parse_error _ as exn ->
      if not (allows_unknown_fallback name raw_value) then raise exn;
      Cursor.restore t start;
      let name = String.lowercase_ascii (read_property_name t) in
      Cursor.ws t;
      if not (Cursor.colon t) then Cursor.err_expected t "':'";
      Cursor.ws t;
      read_unknown_property_declaration t name

(* CSS Nesting 1: distinguish a declaration ([<ident> : <value>]) from a nested
   rule whose selector starts with an ident ([html &:hover { ... }]). The
   starts-with-ident shape isn't conclusive on its own, so look at the next
   non-ident component: if it's [:] this is a declaration, otherwise walk the
   lookahead window for a [{ ... }] block before the next [;]. *)
let rec scan_for_curly_block t =
  match Cursor.peek t with
  | None | Some (Component.Preserved { kind = Token.Semicolon; _ }) -> false
  | Some (Component.Block { node = { opening = Token.Curly; _ }; _ }) -> true
  | Some _ ->
      Cursor.skip t;
      scan_for_curly_block t

let is_nested_rule_inner t =
  (match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Ident _; _ }) -> Cursor.skip t
  | _ -> ());
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Colon; _ }) -> false
  | _ -> scan_for_curly_block t

let is_nested_rule t = Cursor.lookahead is_nested_rule_inner t

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
  | Some _ ->
      (* CSS Nesting 1: a nested rule may start with an ident-shaped selector
         ([html &:hover], [li:nth-child(2)]). A declaration always starts with
         [<ident> :]; anything else followed by a [{ ... }] block (before the
         next [;]) is a nested rule. *)
      if is_nested_rule t then None else Some (read_one ())

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

type parse_step =
  | Step_done of declaration list
  | Step_continue of declaration list
  | Step_recover of declaration list * Error.t

let check_declaration_separator t acc =
  Cursor.ws t;
  match Cursor.peek t with
  | None -> Step_done (List.rev acc)
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
      Cursor.skip t;
      Step_continue acc
  | Some (Component.Preserved { kind = Token.Ident _; _ }) ->
      Cursor.err t "missing semicolon between declarations"
  | _ -> Step_done (List.rev acc)

let read_declaration_no_recovery t acc =
  match read_declaration t with
  | None -> Step_done (List.rev acc)
  | Some decl -> check_declaration_separator t (decl :: acc)

let read_declaration_with_recovery t acc =
  match read_declaration t with
  | None -> Step_done (List.rev acc)
  | Some decl -> (
      let acc = decl :: acc in
      match check_declaration_separator t acc with
      | step -> step
      | exception Error.Parse_error e -> Step_recover (acc, e))
  | exception Error.Parse_error e -> Step_recover (acc, e)

let read_declaration_step t acc =
  if Cursor.recover t then read_declaration_with_recovery t acc
  else read_declaration_no_recovery t acc

let recover_declaration_step t acc e =
  Cursor.push_warning t e;
  skip_to_next_declaration t;
  acc

let rec read_declarations_loop t acc =
  Cursor.ws t;
  match Cursor.peek t with
  | None -> List.rev acc
  | _ -> (
      match read_declaration_step t acc with
      | Step_done decls -> decls
      | Step_continue acc -> read_declarations_loop t acc
      | Step_recover (acc, e) ->
          read_declarations_loop t (recover_declaration_step t acc e))

let read_declarations t =
  Cursor.with_context t "declarations" @@ fun () -> read_declarations_loop t []

let read_block t =
  Cursor.ws t;
  Cursor.braces (fun inner -> read_declarations inner) t

let of_string s =
  match read_declaration (Cursor.of_string s) with
  | Some d -> d
  | None -> failwith ("Declaration.of_string: invalid declaration: " ^ s)

let read t =
  match read_declaration t with
  | Some d -> d
  | None -> Cursor.err_expected t "declaration"

(* Pretty printer for declarations *)
let rec pp_declaration : declaration Pp.t =
 fun ctx -> function
  | Declaration
      {
        property = Custom_property name;
        value = Custom_value { value; layer; _ };
        important;
      } ->
      Pp.string ctx name;
      Pp.string ctx ":";
      Pp.space_if_pretty ctx ();
      let bare_name =
        if String.length name > 2 && String.sub name 0 2 = "--" then
          String.sub name 2 (String.length name - 2)
        else name
      in
      (match (layer, value, ctx.theme_defaults bare_name) with
      | Some "theme", Typed { kind = Font_family; value }, _ ->
          pp_value ctx (Font_family, value)
      | Some "theme", _, Some override_value -> Pp.string ctx override_value
      | _ ->
          pp_property_value ctx
            (Custom_property name, Custom_value { value; layer; meta = None }));
      if important then
        Pp.string ctx (if ctx.minify then "!important" else " !important")
  | Declaration { property; value; important } ->
      pp_property ctx property;
      Pp.string ctx ":";
      Pp.space_if_pretty ctx ();
      pp_property_value ctx (property, value);
      if important then
        Pp.string ctx (if ctx.minify then "!important" else " !important")
  | Theme_guarded { var_name; decl } ->
      if Pp.in_theme ctx var_name then pp_declaration ctx decl

let pp = pp_declaration

(* Convert a declaration to its string representation *)
let string_of_declaration ?(minify = false) decl =
  let buf = Buffer.create 32 in
  let ctx =
    {
      Pp.minify;
      level = 0;
      indent = None;
      buf;
      inline = false;
      in_function = false;
      theme = None;
      theme_defaults = Pp.no_theme_defaults;
    }
  in
  pp_declaration ctx decl;
  Buffer.contents buf

let to_string = string_of_declaration

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
let font_variant_numeric_tokens tokens = (Tokens tokens : font_variant_numeric)

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
let grid_template_areas template = v Grid_template_areas (Areas template)
let grid_template template = v Grid_template template
let grid_auto_columns size = v Grid_auto_columns size
let grid_auto_rows size = v Grid_auto_rows size
let grid_row_start value = v Grid_row_start value
let grid_row_end value = v Grid_row_end value
let grid_column_start value = v Grid_column_start value
let grid_column_end value = v Grid_column_end value
let grid_row (start, end_) = v Grid_row (Lines (start, end_))
let grid_column (start, end_) = v Grid_column (Lines (start, end_))
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
let text_decoration_skip value = v Text_decoration_skip value
let text_decoration_skip_self value = v Text_decoration_skip_self value
let text_decoration_skip_box value = v Text_decoration_skip_box value
let text_decoration_skip_inset value = v Text_decoration_skip_inset value
let text_decoration_skip_spaces value = v Text_decoration_skip_spaces value
let text_emphasis value = v Text_emphasis value
let text_emphasis_style value = v Text_emphasis_style value
let text_emphasis_color value = v Text_emphasis_color value
let text_emphasis_position value = v Text_emphasis_position value
let text_emphasis_skip value = v Text_emphasis_skip value
let text_orientation value = v Text_orientation value
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
let flex_grow value = v Flex_grow (Number value : flex_factor)
let flex_shrink value = v Flex_shrink (Number value : flex_factor)
let flex_basis value = v Flex_basis value
let flex_wrap value = v Flex_wrap value
let flex_flow value = v Flex_flow value
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
let object_view_box value = v Object_view_box value
let clip value = v Clip value
let clear value = v Clear value
let float value = v Float value
let interactivity value = v Interactivity value
let caret_animation value = v Caret_animation value
let caret_shape value = v Caret_shape value
let caret value = v Caret value
let interest_delay value = v Interest_delay value
let interest_delay_start value = v Interest_delay_start value
let interest_delay_end value = v Interest_delay_end value
let nav_up value = v Nav_up value
let nav_right value = v Nav_right value
let nav_down value = v Nav_down value
let nav_left value = v Nav_left value
let touch_action value = v Touch_action value
let direction value = v Direction value
let unicode_bidi value = v Unicode_bidi value
let writing_mode value = v Writing_mode value
let text_combine_upright value = v Text_combine_upright value
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

let border_block value = v Border_block value
let tab_size value = v Tab_size (Int value : tab_size)
let webkit_text_size_adjust value = v Webkit_text_size_adjust value
let font_feature_settings value = v Font_feature_settings value
let font_variation_settings value = v Font_variation_settings value
let webkit_tap_highlight_color value = v Webkit_tap_highlight_color value
let webkit_text_decoration value = v Webkit_text_decoration value
let webkit_text_decoration_color value = v Webkit_text_decoration_color value
let text_indent value = v Text_indent value
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
let text_wrap_mode value = v Text_wrap_mode value
let text_underline_position value = v Text_underline_position value
let text_box_edge value = v Text_box_edge value
let inline_sizing value = v Inline_sizing value
let line_fit_edge value = v Line_fit_edge value
let interpolate_size value = v Interpolate_size value
let min_intrinsic_sizing value = v Min_intrinsic_sizing value
let ruby_align value = v Ruby_align value
let ruby_merge value = v Ruby_merge value
let ruby_overhang value = v Ruby_overhang value
let ruby_position value = v Ruby_position value
let glyph_orientation_vertical value = v Glyph_orientation_vertical value
let word_break value = v Word_break value
let overflow_wrap value = v Overflow_wrap value
let line_break value = v Line_break value
let hyphens value = v Hyphens value
let webkit_hyphens value = v Webkit_hyphens value
let font_stretch value = v Font_stretch value
let font_optical_sizing value = v Font_optical_sizing value
let font_kerning value = v Font_kerning value
let font_language_override value = v Font_language_override value
let font_synthesis_style value = v Font_synthesis_style value
let font_synthesis_weight value = v Font_synthesis_weight value
let font_synthesis_small_caps value = v Font_synthesis_small_caps value
let font_synthesis_position value = v Font_synthesis_position value
let font_variant_ligatures value = v Font_variant_ligatures value
let font_variant_caps value = v Font_variant_caps value
let font_variant_numeric value = v Font_variant_numeric value
let font_variant_position value = v Font_variant_position value
let font_variant_east_asian value = v Font_variant_east_asian value
let backdrop_filter value = v Backdrop_filter value
let webkit_backdrop_filter value = v Webkit_backdrop_filter value
let background_position value = v Background_position value
let background_repeat value = v Background_repeat value
let background_size value = v Background_size value
let content value = v Content value
let counter_reset value = v Counter_reset value
let counter_increment value = v Counter_increment value
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
let border_inline_color value = v Border_inline_color value
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

let container_name value =
  let names = String.split_on_char ' ' value |> List.filter (( <> ) "") in
  match names with
  | [ "none" ] -> v Container_name (None : container_name)
  | _ -> v Container_name (Names names)

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
let page_break_before value = v Page_break_before value
let page_break_after value = v Page_break_after value
let page_break_inside value = v Page_break_inside value
let columns value = v Columns value
let column_rule value = v Column_rule value
let column_span value = v Column_span value
let outline value = v Outline value
let outline_offset len = v Outline_offset len
let scroll_snap_type value = v Scroll_snap_type value
let scroll_snap_align value = v Scroll_snap_align value
let scroll_snap_stop value = v Scroll_snap_stop value
let scroll_behavior value = v Scroll_behavior value
let color_scheme value = v Color_scheme value
let image_rendering value = v Image_rendering value
let image_resolution value = v Image_resolution value

(* Alignment constructor helpers (declarations) *)
