open Common
open Values
open Syntax
include Properties_intf
include Prop_common
include Prop_svg
include Prop_multicol
include Prop_writing
include Prop_filter
include Prop_align
include Prop_flex
include Prop_grid
include Prop_image
include Prop_background
include Prop_mask
include Prop_transform
include Prop_ui
include Prop_layout

(* Generic length parsing helpers *)
let read_line_height_length t : line_height =
  let n, repr, unit = Cursor.number_repr_with_unit t in
  let n, repr = normalize_signed_zero n repr in
  if n < 0. then Cursor.err_invalid t "line-height cannot be negative"
  else
    let authored () : line_height = Number { value = n; unit; repr } in
    match unit with
    | Some "px" -> if Pp.string_of_float n = repr then Px n else authored ()
    | Some "rem" -> if Pp.string_of_float n = repr then Rem n else authored ()
    | Some "em" -> if Pp.string_of_float n = repr then Em n else authored ()
    | Some "%" -> if Pp.string_of_float n = repr then Pct n else authored ()
    | None -> if Pp.string_of_float n = repr then Num n else authored ()
    | Some _ -> authored ()

let rec numeric_line_height_calc_leaves : line_height calc -> line_height calc =
  function
  | Val (Num n) | Val (Number { value = n; unit = None; _ }) -> Num n
  | Nested inner -> Nested (numeric_line_height_calc_leaves inner)
  | Parens inner -> Parens (numeric_line_height_calc_leaves inner)
  | Expr (left, op, right) ->
      Expr
        ( numeric_line_height_calc_leaves left,
          op,
          numeric_line_height_calc_leaves right )
  | leaf -> leaf

let read_vertical_align_length t : vertical_align =
  let n, unit = Cursor.number_with_unit t in
  match unit with
  | Some "px" -> Px n
  | Some "rem" -> Rem n
  | Some "em" -> Em n
  | Some "%" -> Pct n
  (* A unitless [0] is the valid zero <length> (CSS Values 4 sec. 6.1); any
     other unitless number is not a length and is rejected. *)
  | None when n = 0. -> Zero
  | None -> Cursor.err_invalid t "vertical-align requires a unit"
  | Some u -> Cursor.err_invalid t ("invalid vertical-align unit: " ^ u)

(* CSS Display 3 sec. 2.1 [display-outside]: pre-existing aliases inside the
   single-value vocabulary that compose with a [display-inside] in the two-value
   form. The composite [<outside> <inside>] is a [Multi]. *)
let display_outside_idents : (string * display) list =
  [
    ("block", Block);
    ("inline", Inline);
    ("run-in", Run_in);
    ("list-item", List_item);
  ]

let display_inside_idents : (string * display) list =
  [
    ("flow", Block);
    ("flow-root", Flow_root);
    ("table", Table);
    ("flex", Flex);
    ("grid", Grid);
    ("ruby", Ruby);
  ]

let list_item_inside_idents : (string * display) list =
  [ ("flow", Block); ("flow-root", Flow_root) ]

let read_display_legacy t : display =
  Cursor.enum "display"
    [
      ("none", (None : display));
      ("block", Block);
      ("inline", Inline);
      ("inline-block", Inline_block);
      ("flex", Flex);
      ("inline-flex", Inline_flex);
      ("grid", Grid);
      ("inline-grid", Inline_grid);
      ("flow-root", Flow_root);
      ("table", Table);
      ("table-row", Table_row);
      ("table-cell", Table_cell);
      ("table-caption", Table_caption);
      ("table-column", Table_column);
      ("table-column-group", Table_column_group);
      ("table-footer-group", Table_footer_group);
      ("table-header-group", Table_header_group);
      ("table-row-group", Table_row_group);
      ("inline-table", Inline_table);
      ("list-item", List_item);
      ("contents", Contents);
      ("run-in", Run_in);
      ("ruby", Ruby);
      ("ruby-base", Ruby_base);
      ("ruby-text", Ruby_text);
      ("ruby-base-container", Ruby_base_container);
      ("ruby-text-container", Ruby_text_container);
      ("math", Math);
      ("-webkit-flex", Webkit_flex);
      ("-webkit-inline-flex", Webkit_inline_flex);
      ("-ms-flexbox", Ms_flexbox);
      ("-webkit-box", Webkit_box);
      ("-moz-box", Moz_box);
      ("-moz-inline-box", Moz_inline_box);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    t

let read_display_two_value t : display =
  (* CSS Display 3 sec. 2.1 two-value form [<display-outside> <display-inside>].
     Both keywords must come from their respective vocabularies; otherwise
     reject so the caller can fall back to the legacy single-value form. *)
  let outside = Cursor.enum "display-outside" display_outside_idents t in
  Cursor.ws t;
  match Cursor.peek_ident t with
  | Some s when List.mem_assoc s display_inside_idents ->
      let inside = Cursor.enum "display-inside" display_inside_idents t in
      Multi (outside, inside)
  | _ -> Cursor.err_expected t "<display-inside>"

let read_display_list_item t : display =
  let outside = ref Option.None in
  let inside = ref Option.None in
  let list_item = ref false in
  let consume_slot () =
    Cursor.ws t;
    match Cursor.peek_ident t with
    | Some "list-item" when not !list_item ->
        ignore (Cursor.ident t : string);
        list_item := true;
        true
    | Some s when Option.is_none !outside -> (
        match List.assoc_opt s display_outside_idents with
        | Some value ->
            ignore (Cursor.ident t : string);
            outside := Option.Some value;
            true
        | Option.None -> false)
    | Some s when Option.is_none !inside -> (
        match List.assoc_opt s list_item_inside_idents with
        | Some value ->
            ignore (Cursor.ident t : string);
            inside := Option.Some value;
            true
        | Option.None -> false)
    | _ -> false
  in
  while consume_slot () do
    ()
  done;
  if not !list_item then Cursor.err_expected t "list-item";
  if Option.is_none !outside && Option.is_none !inside then
    Cursor.err_expected t "list-item with outside or inside display";
  let outside = Option.value !outside ~default:Block in
  let inside = Option.value !inside ~default:Block in
  Multi (Multi (outside, inside), List_item)

let rec read_display t : display =
  let read_var t : display = Var (read_var read_display t) in
  Cursor.ws t;
  match Cursor.peek t with
  | Some (Component.Func { node = { name = "var"; _ }; _ }) -> read_var t
  | _ -> (
      match Cursor.option read_display_list_item t with
      | Some d -> d
      | None -> (
          match Cursor.option read_display_two_value t with
          | Some d -> d
          | None -> read_display_legacy t))

let rec read_position t : position =
  Cursor.enum_or_var "position"
    [
      ("static", (Static : position));
      ("relative", Relative);
      ("absolute", Absolute);
      ("fixed", Fixed);
      ("sticky", Sticky);
      ("-webkit-sticky", Webkit_sticky);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_position t))
    t

let rec read_font_weight t : font_weight =
  let read_var t : font_weight = Var (read_var read_font_weight t) in
  Cursor.ws t;
  Cursor.enum_or_calls "font-weight"
    [
      ("normal", Normal);
      ("bold", Bold);
      ("bolder", Bolder);
      ("lighter", Lighter);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:(fun t ->
      let n = Cursor.number t in
      let weight = int_of_float n in
      if weight >= 1 && weight <= 1000 then (Weight weight : font_weight)
      else err_invalid_value t "font-weight" (string_of_int weight))
    t

let rec read_font_style t : font_style =
  Cursor.enum_or_var "font-style"
    [
      ("normal", (Normal : font_style));
      ("italic", (Italic : font_style));
      ("inherit", (Inherit : font_style));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_style t))
    ~default:(fun t ->
      Cursor.expect_string "oblique" t;
      Cursor.ws t;
      if Cursor.is_done t || Cursor.peek_semicolon t then Oblique
      else
        let first = read_angle t in
        Cursor.ws t;
        if Cursor.is_done t || Cursor.peek_semicolon t then Oblique_angle first
        else
          (* CSS Fonts 4 sec. 11.2 wants the first oblique angle <= the second.
             Browsers keep a descending range ([oblique 20deg 10deg]), so accept
             it but warn, leaving [Css.of_string ~strict] free to reject it. *)
          let second = read_angle t in
          (match (angle_degrees_opt first, angle_degrees_opt second) with
          | Some a, Some b when a > b ->
              Cursor.push_warning t
                (Error.bad_value (Cursor.position t) ~property:"font-style"
                   ~reason:
                     "oblique angle range must run from the smaller angle to \
                      the larger (CSS Fonts 4 \u{00a7}11.2)")
          | _ -> ());
          Oblique_range (first, second))
    t

let rec read_font_size t : font_size =
  let read_var t : font_size = Var (read_var read_font_size t) in
  let read_calc t : font_size = Calc (read_calc read_font_size t) in
  let read_length t : font_size =
    let len = read_non_negative_length ~with_keywords:false t in
    Length len
  in
  let read_pct t : font_size =
    let n = Cursor.number t in
    if n < 0. then Cursor.err_invalid t "negative font-size percentage";
    Cursor.expect '%' t;
    Pct n
  in
  Cursor.enum_or_calls "font-size"
    [
      ("xx-small", (Xx_small : font_size));
      ("x-small", X_small);
      ("small", Small);
      ("medium", Medium);
      ("large", Large);
      ("x-large", X_large);
      ("xx-large", Xx_large);
      ("xxx-large", Xxx_large);
      ("larger", Larger);
      ("smaller", Smaller);
      ("math", Math);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var); ("calc", read_calc) ]
    ~default:(fun t ->
      (* Try percentage first, then length *)
      Cursor.one_of [ read_pct; read_length ] t)
    t

let rec read_text_align t : text_align =
  Cursor.enum_or_var "text-align"
    [
      ("left", (Left : text_align));
      ("right", Right);
      ("center", Center);
      ("justify", Justify);
      ("start", Start);
      ("end", End);
      ("match-parent", Match_parent);
      ("-webkit-match-parent", Webkit_match_parent);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_align t))
    t

let rec read_text_decoration_line t : text_decoration_line =
  Cursor.enum_or_var "text-decoration-line"
    [
      ("none", (None : text_decoration_line));
      ("underline", Underline);
      ("overline", Overline);
      ("line-through", Line_through);
      ("blink", Blink);
      ("spelling-error", Spelling_error);
      ("grammar-error", Grammar_error);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_decoration_line t))
    t

let rec read_text_decoration_style t : text_decoration_style =
  Cursor.enum_or_var "text-decoration-style"
    [
      ("solid", (Solid : text_decoration_style));
      ("double", Double);
      ("dotted", Dotted);
      ("dashed", Dashed);
      ("wavy", Wavy);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_decoration_style t))
    t

module Text_decoration = struct
  type component =
    | Line of text_decoration_line
    | Style of text_decoration_style
    | Color of color
    | Thickness of length

  type components = {
    lines : text_decoration_line list;
    style : text_decoration_style option;
    color : color option;
    thickness : length option;
  }

  let empty = { lines = []; style = None; color = None; thickness = None }

  let read_component t =
    Cursor.one_of
      [
        (fun t -> Line (read_text_decoration_line t));
        (fun t -> Style (read_text_decoration_style t));
        (fun t -> Color (read_color t));
        (fun t -> Thickness (read_length t));
      ]
      t

  let merge t acc = function
    | Line l ->
        (* Check for duplicate lines - per CSS spec, || combinator means each
           component at most once *)
        if l = (None : text_decoration_line) && acc.lines <> [] then
          Cursor.err t "text-decoration-line none cannot be mixed"
        else if
          l <> (None : text_decoration_line)
          && List.mem (None : text_decoration_line) acc.lines
        then Cursor.err t "text-decoration-line none cannot be mixed"
        else if List.mem l acc.lines then
          Cursor.err t
            ("duplicate text-decoration-line: "
            ^
            match l with
            | None -> "none"
            | Underline -> "underline"
            | Overline -> "overline"
            | Line_through -> "line-through"
            | Blink -> "blink"
            | Spelling_error -> "spelling-error"
            | Grammar_error -> "grammar-error"
            | Inherit -> "inherit"
            | Initial -> "initial"
            | Unset -> "unset"
            | Revert -> "revert"
            | Revert_layer -> "revert-layer"
            | Var _ -> "var(...)")
        else { acc with lines = acc.lines @ [ l ] }
    | Style s when acc.style = None -> { acc with style = Some s }
    | Color c when acc.color = None -> { acc with color = Some c }
    | Thickness th when acc.thickness = None -> { acc with thickness = Some th }
    | Style _ -> Cursor.err t "duplicate text-decoration-style"
    | Color _ -> Cursor.err t "duplicate text-decoration-color"
    | Thickness _ -> Cursor.err t "duplicate text-decoration-thickness"

  let to_shorthand (components : components) : text_decoration_shorthand =
    {
      lines = components.lines;
      style = components.style;
      color = components.color;
      thickness = components.thickness;
    }
end

let read_text_decoration_shorthand t : text_decoration_shorthand =
  let acc, _ =
    Cursor.fold_many Text_decoration.read_component ~init:Text_decoration.empty
      ~f:(Text_decoration.merge t) t
  in
  Text_decoration.to_shorthand acc

let rec read_text_decoration t : text_decoration =
  let read_var t : text_decoration = Var (read_var read_text_decoration t) in
  Cursor.enum_or_calls "text-decoration"
    [
      ("inherit", (Inherit : text_decoration));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
      ("none", None);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:(fun t ->
      let shorthand = read_text_decoration_shorthand t in
      (* For the main text-decoration property, require at least one line
         decoration *)
      if shorthand.lines = [] then
        Cursor.err t
          "text-decoration requires at least one line decoration (underline, \
           overline, or line-through)"
      else (Shorthand shorthand : text_decoration))
    t

let rec read_text_decoration_skip t : text_decoration_skip =
  Cursor.enum_or_var "text-decoration-skip"
    [
      ("none", (None : text_decoration_skip));
      ("auto", Auto);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_decoration_skip t))
    t

let rec read_text_decoration_skip_self t : text_decoration_skip_self =
  Cursor.enum_or_var "text-decoration-skip-self"
    [
      ("none", (None : text_decoration_skip_self));
      ("objects", Objects);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_decoration_skip_self t))
    t

let rec read_text_decoration_skip_box t : text_decoration_skip_box =
  Cursor.enum_or_var "text-decoration-skip-box"
    [
      ("all", (All : text_decoration_skip_box));
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_decoration_skip_box t))
    t

let rec read_text_decoration_skip_inset t : text_decoration_skip_inset =
  Cursor.enum_or_var "text-decoration-skip-inset"
    [
      ("none", (None : text_decoration_skip_inset));
      ("auto", Auto);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_decoration_skip_inset t))
    t

let read_text_decoration_skip_space t : text_decoration_skip_space =
  Cursor.enum "text-decoration-skip-spaces"
    [
      ("all", (All : text_decoration_skip_space)); ("start", Start); ("end", End);
    ]
    t

let rec read_text_decoration_skip_spaces t : text_decoration_skip_spaces =
  Cursor.enum_or_var "text-decoration-skip-spaces"
    [
      ("inherit", (Inherit : text_decoration_skip_spaces));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_decoration_skip_spaces t))
    ~default:(fun t ->
      let spaces =
        Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
          read_text_decoration_skip_space t
      in
      match spaces with
      | [ All ] | [ Start ] | [ End ] | [ Start; End ] | [ End; Start ] ->
          (Spaces spaces : text_decoration_skip_spaces)
      | _ -> Cursor.err_invalid t "text-decoration-skip-spaces")
    t

let rec read_text_indent_value t : text_indent_value =
  Cursor.enum_or_var "text-indent"
    [
      ("inherit", (Inherit : text_indent_value));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_indent_value t))
    ~default:(fun t ->
      let length = ref Option.None in
      let hanging = ref false in
      let each_line = ref false in
      let at_end t =
        Cursor.is_done t || Cursor.peek_semicolon t
        || Cursor.peek_delim t = Some '!'
      in
      while not (at_end t) do
        Cursor.ws t;
        match Cursor.peek_ident t with
        | Some "hanging" when not !hanging ->
            Cursor.skip t;
            hanging := true
        | Some "each-line" when not !each_line ->
            Cursor.skip t;
            each_line := true
        | _ when Option.is_none !length ->
            length := Some (read_length_percentage ~with_keywords:false t)
        | _ -> Cursor.err_invalid t "text-indent"
      done;
      match !length with
      | Some length ->
          Indent { length; hanging = !hanging; each_line = !each_line }
      | None -> Cursor.err_invalid t "text-indent")
    t

let pp_text_emphasis_fill ctx = function
  | Filled -> Pp.string ctx "filled"
  | Open -> Pp.string ctx "open"

let pp_text_emphasis_shape ctx = function
  | Dot -> Pp.string ctx "dot"
  | Circle -> Pp.string ctx "circle"
  | Double_circle -> Pp.string ctx "double-circle"
  | Triangle -> Pp.string ctx "triangle"
  | Sesame -> Pp.string ctx "sesame"

let rec pp_text_emphasis_style : text_emphasis_style Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Mark (fill, shape) ->
      let first = ref true in
      let space_if_needed () =
        if !first then first := false else Pp.space ctx ()
      in
      Option.iter
        (fun fill ->
          space_if_needed ();
          pp_text_emphasis_fill ctx fill)
        fill;
      Option.iter
        (fun shape ->
          space_if_needed ();
          pp_text_emphasis_shape ctx shape)
        shape
  | String s -> Pp.quoted_string ctx s
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_emphasis_style ctx v

let rec pp_text_emphasis : text_emphasis Pp.t =
 fun ctx -> function
  | Emphasis (style, color) ->
      let first = ref true in
      let space_if_needed () =
        if !first then first := false else Pp.space ctx ()
      in
      Option.iter
        (fun style ->
          space_if_needed ();
          pp_text_emphasis_style ctx style)
        style;
      Option.iter
        (fun color ->
          space_if_needed ();
          pp_color ctx color)
        color
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_emphasis ctx v

let read_text_emphasis_fill t =
  Cursor.enum "text-emphasis fill"
    [ ("filled", (Filled : text_emphasis_fill)); ("open", Open) ]
    t

let read_text_emphasis_shape t =
  Cursor.enum "text-emphasis shape"
    [
      ("dot", (Dot : text_emphasis_shape));
      ("circle", Circle);
      ("double-circle", Double_circle);
      ("triangle", Triangle);
      ("sesame", Sesame);
    ]
    t

module Text_emphasis_style = struct
  type part = Fill of text_emphasis_fill | Shape of text_emphasis_shape

  let read_part t =
    Cursor.one_of
      [
        (fun t -> Fill (read_text_emphasis_fill t));
        (fun t -> Shape (read_text_emphasis_shape t));
      ]
      t

  let merge t (fill_opt, shape_opt) = function
    | Fill fill when Option.is_none fill_opt -> (Some fill, shape_opt)
    | Shape shape when Option.is_none shape_opt -> (fill_opt, Some shape)
    | Fill _ -> Cursor.err t "duplicate text-emphasis fill"
    | Shape _ -> Cursor.err t "duplicate text-emphasis shape"

  let read_mark t =
    let first = read_part t in
    let second = Cursor.option read_part t in
    let fill, shape =
      List.fold_left (merge t) (None, None) (first :: Option.to_list second)
    in
    (Mark (fill, shape) : text_emphasis_style)
end

let rec read_text_emphasis_style t : text_emphasis_style =
  Cursor.enum_or_calls "text-emphasis-style"
    [
      ("none", (None : text_emphasis_style));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (read_var read_text_emphasis_style t)) ]
    ~default:(fun t ->
      Cursor.one_of
        [
          (fun t -> (String (Cursor.string t) : text_emphasis_style));
          Text_emphasis_style.read_mark;
        ]
        t)
    t

module Text_emphasis = struct
  type component = Style of text_emphasis_style | Color of color

  let read_component t =
    Cursor.one_of
      [
        (fun t -> Style (read_text_emphasis_style t));
        (fun t -> Color (read_color t));
      ]
      t

  let merge t (style_opt, color_opt) = function
    | Style style when Option.is_none style_opt -> (Some style, color_opt)
    | Color color when Option.is_none color_opt -> (style_opt, Some color)
    | Style _ -> Cursor.err t "duplicate text-emphasis-style"
    | Color _ -> Cursor.err t "duplicate text-emphasis-color"
end

let rec read_text_emphasis t : text_emphasis =
  Cursor.enum_or_calls "text-emphasis"
    [
      ("inherit", (Inherit : text_emphasis));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (read_var read_text_emphasis t)) ]
    ~default:(fun t ->
      let style, color =
        Cursor.fold_many Text_emphasis.read_component
          ~init:((None : text_emphasis_style option), (None : color option))
          ~f:(Text_emphasis.merge t) t
        |> fst
      in
      match (style, color) with
      | None, None -> Cursor.err_expected t "text-emphasis value"
      | _ -> Emphasis (style, color))
    t

let read_text_emphasis_skip_keyword t : text_emphasis_skip_keyword =
  Cursor.enum "text-emphasis-skip"
    [
      ("spaces", (Spaces : text_emphasis_skip_keyword));
      ("punctuation", Punctuation);
      ("symbols", Symbols);
      ("narrow", Narrow);
    ]
    t

let rec read_text_emphasis_skip t : text_emphasis_skip =
  Cursor.enum_or_var "text-emphasis-skip"
    [
      ("inherit", (Inherit : text_emphasis_skip));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_emphasis_skip t))
    ~default:(fun t ->
      let keywords =
        Cursor.list ~sep:Cursor.ws ~at_least:1 read_text_emphasis_skip_keyword t
      in
      let duplicate =
        List.exists
          (fun keyword ->
            List.length (List.filter (( = ) keyword) keywords) > 1)
          keywords
      in
      if duplicate then Cursor.err_invalid t "text-emphasis-skip"
      else (Skip keywords : text_emphasis_skip))
    t

let pp_text_emphasis_line : text_emphasis_line Pp.t =
 fun ctx -> function
  | Over -> Pp.string ctx "over"
  | Under -> Pp.string ctx "under"

let pp_text_emphasis_side : text_emphasis_side Pp.t =
 fun ctx -> function
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"

let pp_text_emphasis_skip_keyword : text_emphasis_skip_keyword Pp.t =
 fun ctx -> function
  | Spaces -> Pp.string ctx "spaces"
  | Punctuation -> Pp.string ctx "punctuation"
  | Symbols -> Pp.string ctx "symbols"
  | Narrow -> Pp.string ctx "narrow"

let rec pp_text_emphasis_skip : text_emphasis_skip Pp.t =
 fun ctx -> function
  | Skip keywords ->
      Pp.list ~sep:Pp.space pp_text_emphasis_skip_keyword ctx keywords
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_emphasis_skip ctx v

let rec pp_text_emphasis_position : text_emphasis_position Pp.t =
 fun ctx -> function
  | Position (line, side) ->
      pp_text_emphasis_line ctx line;
      Option.iter
        (fun side ->
          Pp.space ctx ();
          pp_text_emphasis_side ctx side)
        side
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_emphasis_position ctx v

let read_text_emphasis_line t =
  Cursor.enum "text-emphasis-position line"
    [ ("over", (Over : text_emphasis_line)); ("under", Under) ]
    t

let read_text_emphasis_side t =
  Cursor.enum "text-emphasis-position side"
    [ ("left", (Left : text_emphasis_side)); ("right", Right) ]
    t

module Text_emphasis_position = struct
  type component = Line of text_emphasis_line | Side of text_emphasis_side

  let read_component t =
    Cursor.one_of
      [
        (fun t -> Line (read_text_emphasis_line t));
        (fun t -> Side (read_text_emphasis_side t));
      ]
      t

  let merge t (line_opt, side_opt) = function
    | Line line when Option.is_none line_opt -> (Some line, side_opt)
    | Side side when Option.is_none side_opt -> (line_opt, Some side)
    | Line _ -> Cursor.err t "duplicate text-emphasis-position line"
    | Side _ -> Cursor.err t "duplicate text-emphasis-position side"
end

let rec read_text_emphasis_position t : text_emphasis_position =
  Cursor.enum_or_calls "text-emphasis-position"
    [
      ("inherit", (Inherit : text_emphasis_position));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (read_var read_text_emphasis_position t)) ]
    ~default:(fun t ->
      let line, side =
        Cursor.fold_many Text_emphasis_position.read_component
          ~init:
            ( (None : text_emphasis_line option),
              (None : text_emphasis_side option) )
          ~f:(Text_emphasis_position.merge t)
          t
        |> fst
      in
      match (line, side) with
      | Some line, side -> (Position (line, side) : text_emphasis_position)
      | _ -> Cursor.err_expected t "text-emphasis-position value")
    t

let pp_text_underline_position_keyword : text_underline_position_keyword Pp.t =
 fun ctx -> function
  | Under -> Pp.string ctx "under"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"

let rec pp_text_underline_position : text_underline_position Pp.t =
 fun ctx (value : text_underline_position) ->
  match value with
  | Auto -> Pp.string ctx "auto"
  | From_font -> Pp.string ctx "from-font"
  | Position keywords ->
      Pp.list ~sep:Pp.space pp_text_underline_position_keyword ctx keywords
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_underline_position ctx v

let read_text_underline_position_keyword t : text_underline_position_keyword =
  Cursor.enum "text-underline-position"
    [
      ("under", (Under : text_underline_position_keyword));
      ("left", Left);
      ("right", Right);
    ]
    t

let rec read_text_underline_position t : text_underline_position =
  Cursor.enum_or_var "text-underline-position"
    [
      ("auto", (Auto : text_underline_position));
      ("from-font", From_font);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_text_underline_position t)
        : text_underline_position))
    ~default:(fun t ->
      let keywords =
        Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
          read_text_underline_position_keyword t
      in
      let valid =
        match keywords with
        | [ Under ] -> true
        | [ Under; (Left | Right) ] | [ (Left | Right); Under ] -> true
        | _ -> false
      in
      if not valid then Cursor.err_invalid t "text-underline-position";
      (Position keywords : text_underline_position))
    t

let rec read_text_transform t : text_transform =
  Cursor.enum_or_var "text-transform"
    [
      ("none", (None : text_transform));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_transform t))
    ~default:(fun t ->
      let case : text_transform_case option ref = ref Option.None in
      let full_width = ref false in
      let full_size_kana = ref false in
      let consumed = ref true in
      while !consumed do
        consumed := false;
        Cursor.ws t;
        match Cursor.peek_ident t with
        | Some "uppercase" when Option.is_none !case ->
            Cursor.skip t;
            case := Option.Some Uppercase;
            consumed := true
        | Some "lowercase" when Option.is_none !case ->
            Cursor.skip t;
            case := Option.Some Lowercase;
            consumed := true
        | Some "capitalize" when Option.is_none !case ->
            Cursor.skip t;
            case := Option.Some Capitalize;
            consumed := true
        | Some "full-width" when not !full_width ->
            Cursor.skip t;
            full_width := true;
            consumed := true
        | Some "full-size-kana" when not !full_size_kana ->
            Cursor.skip t;
            full_size_kana := true;
            consumed := true
        | _ -> ()
      done;
      match (!case, !full_width, !full_size_kana) with
      | None, false, false -> Cursor.err_expected t "text-transform"
      | Some c, false, false -> Case c
      | case, full_width, full_size_kana ->
          Combo { case; full_width; full_size_kana })
    t

let read_text_transform_case t =
  Cursor.enum "text-transform case"
    [
      ("capitalize", (Capitalize : text_transform_case));
      ("uppercase", Uppercase);
      ("lowercase", Lowercase);
    ]
    t

let rec pp_line_break : line_break Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Loose -> Pp.string ctx "loose"
  | Normal -> Pp.string ctx "normal"
  | Strict -> Pp.string ctx "strict"
  | Anywhere -> Pp.string ctx "anywhere"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_line_break ctx v

let rec read_line_break t : line_break =
  Cursor.enum_or_var "line-break"
    [
      ("auto", (Auto : line_break));
      ("loose", Loose);
      ("normal", Normal);
      ("strict", Strict);
      ("anywhere", Anywhere);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_line_break t))
    t

let rec pp_font_optical_sizing : font_optical_sizing Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_optical_sizing ctx v

let rec read_font_optical_sizing t : font_optical_sizing =
  Cursor.enum_or_var "font-optical-sizing"
    [
      ("auto", (Auto : font_optical_sizing));
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_optical_sizing t))
    t

let rec pp_font_kerning : font_kerning Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Normal -> Pp.string ctx "normal"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_kerning ctx v

let rec read_font_kerning t : font_kerning =
  Cursor.enum_or_var "font-kerning"
    [
      ("auto", (Auto : font_kerning));
      ("normal", Normal);
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_kerning t))
    t

let rec pp_font_language_override : font_language_override Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | String s -> Pp.quoted_string ctx s
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_language_override ctx v

let rec read_font_language_override t : font_language_override =
  Cursor.enum_or_calls "font-language-override"
    [
      ("normal", (Normal : font_language_override));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (read_var read_font_language_override t)) ]
    ~default:(fun t -> (String (Cursor.string t) : font_language_override))
    t

let rec pp_font_synthesis_style : font_synthesis_style Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Oblique_only -> Pp.string ctx "oblique-only"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_synthesis_style ctx v

let rec read_font_synthesis_style t : font_synthesis_style =
  Cursor.enum_or_var "font-synthesis-style"
    [
      ("auto", (Auto : font_synthesis_style));
      ("none", None);
      ("oblique-only", Oblique_only);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_synthesis_style t))
    t

let rec pp_font_synthesis_weight : font_synthesis_weight Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_synthesis_weight ctx v

let rec read_font_synthesis_weight t : font_synthesis_weight =
  Cursor.enum_or_var "font-synthesis-weight"
    [
      ("auto", (Auto : font_synthesis_weight));
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_synthesis_weight t))
    t

let rec pp_font_synthesis_small_caps : font_synthesis_small_caps Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_synthesis_small_caps ctx v

let rec read_font_synthesis_small_caps t : font_synthesis_small_caps =
  Cursor.enum_or_var "font-synthesis-small-caps"
    [
      ("auto", (Auto : font_synthesis_small_caps));
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_synthesis_small_caps t))
    t

let rec pp_font_synthesis_position : font_synthesis_position Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_synthesis_position ctx v

let rec read_font_synthesis_position t : font_synthesis_position =
  Cursor.enum_or_var "font-synthesis-position"
    [
      ("auto", (Auto : font_synthesis_position));
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_synthesis_position t))
    t

let pp_font_variant_ligature ctx = function
  | Common_ligatures -> Pp.string ctx "common-ligatures"
  | No_common_ligatures -> Pp.string ctx "no-common-ligatures"
  | Discretionary_ligatures -> Pp.string ctx "discretionary-ligatures"
  | No_discretionary_ligatures -> Pp.string ctx "no-discretionary-ligatures"
  | Historical_ligatures -> Pp.string ctx "historical-ligatures"
  | No_historical_ligatures -> Pp.string ctx "no-historical-ligatures"
  | Contextual -> Pp.string ctx "contextual"
  | No_contextual -> Pp.string ctx "no-contextual"

let rec pp_font_variant_ligatures : font_variant_ligatures Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | None -> Pp.string ctx "none"
  | Ligatures ligatures ->
      Pp.list ~sep:Pp.space pp_font_variant_ligature ctx ligatures
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_variant_ligatures ctx v

let read_font_variant_ligature t =
  Cursor.enum "font-variant-ligature"
    [
      ("common-ligatures", Common_ligatures);
      ("no-common-ligatures", No_common_ligatures);
      ("discretionary-ligatures", Discretionary_ligatures);
      ("no-discretionary-ligatures", No_discretionary_ligatures);
      ("historical-ligatures", Historical_ligatures);
      ("no-historical-ligatures", No_historical_ligatures);
      ("contextual", Contextual);
      ("no-contextual", No_contextual);
    ]
    t

let font_variant_ligature_slot = function
  | Common_ligatures | No_common_ligatures -> `Common
  | Discretionary_ligatures | No_discretionary_ligatures -> `Discretionary
  | Historical_ligatures | No_historical_ligatures -> `Historical
  | Contextual | No_contextual -> `Contextual

let has_duplicate_ligature_slot ligatures =
  List.exists
    (fun ligature ->
      let slot = font_variant_ligature_slot ligature in
      List.length
        (List.filter
           (fun other -> font_variant_ligature_slot other = slot)
           ligatures)
      > 1)
    ligatures

let rec read_font_variant_ligatures t : font_variant_ligatures =
  Cursor.enum_or_var "font-variant-ligatures"
    [
      ("normal", (Normal : font_variant_ligatures));
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_variant_ligatures t))
    ~default:(fun t ->
      match Cursor.many read_font_variant_ligature t with
      | [], _ -> Cursor.err_invalid t "font-variant-ligatures"
      | ligatures, _ ->
          if has_duplicate_ligature_slot ligatures then
            Cursor.err_invalid t "font-variant-ligatures";
          Ligatures ligatures)
    t

let rec pp_font_variant_caps : font_variant_caps Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Small_caps -> Pp.string ctx "small-caps"
  | All_small_caps -> Pp.string ctx "all-small-caps"
  | Petite_caps -> Pp.string ctx "petite-caps"
  | All_petite_caps -> Pp.string ctx "all-petite-caps"
  | Unicase -> Pp.string ctx "unicase"
  | Titling_caps -> Pp.string ctx "titling-caps"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_variant_caps ctx v

let rec read_font_variant_caps t : font_variant_caps =
  Cursor.enum_or_var "font-variant-caps"
    [
      ("normal", (Normal : font_variant_caps));
      ("small-caps", Small_caps);
      ("all-small-caps", All_small_caps);
      ("petite-caps", Petite_caps);
      ("all-petite-caps", All_petite_caps);
      ("unicase", Unicase);
      ("titling-caps", Titling_caps);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_variant_caps t))
    t

let rec pp_font_variant_position : font_variant_position Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Sub -> Pp.string ctx "sub"
  | Super -> Pp.string ctx "super"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_variant_position ctx v

let rec read_font_variant_position t : font_variant_position =
  Cursor.enum_or_var "font-variant-position"
    [
      ("normal", (Normal : font_variant_position));
      ("sub", Sub);
      ("super", Super);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_variant_position t))
    t

let pp_east_asian_feature ctx = function
  | Jis78 -> Pp.string ctx "jis78"
  | Jis83 -> Pp.string ctx "jis83"
  | Jis90 -> Pp.string ctx "jis90"
  | Jis04 -> Pp.string ctx "jis04"
  | Simplified -> Pp.string ctx "simplified"
  | Traditional -> Pp.string ctx "traditional"
  | Full_width -> Pp.string ctx "full-width"
  | Proportional_width -> Pp.string ctx "proportional-width"
  | Ruby -> Pp.string ctx "ruby"

let rec pp_font_variant_east_asian : font_variant_east_asian Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Features features ->
      Pp.list ~sep:Pp.space pp_east_asian_feature ctx features
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_variant_east_asian ctx v

let read_east_asian_feature t =
  Cursor.enum "font-variant-east-asian-feature"
    [
      ("jis78", Jis78);
      ("jis83", Jis83);
      ("jis90", Jis90);
      ("jis04", Jis04);
      ("simplified", Simplified);
      ("traditional", Traditional);
      ("full-width", Full_width);
      ("proportional-width", Proportional_width);
      ("ruby", Ruby);
    ]
    t

let rec read_font_variant_east_asian t : font_variant_east_asian =
  let invalid_feature_set features =
    let variant_count = ref 0 in
    let width_count = ref 0 in
    let seen = ref [] in
    List.exists
      (fun feature ->
        let duplicate = List.mem feature !seen in
        seen := feature :: !seen;
        (match feature with
        | Jis78 | Jis83 | Jis90 | Jis04 | Simplified | Traditional ->
            incr variant_count
        | Full_width | Proportional_width -> incr width_count
        | Ruby -> ());
        duplicate || !variant_count > 1 || !width_count > 1)
      features
  in
  Cursor.enum_or_var "font-variant-east-asian"
    [
      ("normal", (Normal : font_variant_east_asian));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_variant_east_asian t))
    ~default:(fun t ->
      match Cursor.many read_east_asian_feature t with
      | [], _ -> Cursor.err_invalid t "font-variant-east-asian"
      | features, _ when invalid_feature_set features ->
          Cursor.err_invalid t "font-variant-east-asian"
      | features, _ -> (Features features : font_variant_east_asian))
    t

let rec read_overflow_single (t : Cursor.t) : overflow =
  Cursor.enum_or_var "overflow"
    [
      ("visible", (Visible : overflow));
      ("hidden", Hidden);
      ("scroll", Scroll);
      ("auto", Auto);
      ("clip", Clip);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_overflow_single t))
    t

let read_overflow t : overflow =
  let first = read_overflow_single t in
  Cursor.ws t;
  if
    Cursor.is_done t || Cursor.peek_semicolon t
    || Cursor.peek_delim t = Some '!'
  then first
  else
    let second = read_overflow_single t in
    Cursor.ws t;
    if
      (not (Cursor.is_done t))
      && not (Cursor.peek_semicolon t || Cursor.peek_delim t = Some '!')
    then Cursor.expect_eof t;
    Overflow_pair (first, second)

(* pp_box_shadow removed - use pp_shadow with List constructor *)

let rec pp_filter : filter Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Blur l -> Pp.call "blur" pp_length ctx l
  | Brightness n ->
      Pp.call "brightness" (pp_number_percentage ~always:true) ctx n
  | Contrast n -> Pp.call "contrast" (pp_number_percentage ~always:true) ctx n
  | Drop_shadow s -> Pp.call "drop-shadow" pp_shadow ctx s
  | Grayscale n -> Pp.call "grayscale" (pp_number_percentage ~always:true) ctx n
  | Hue_rotate (Deg 0.) when Pp.minified ctx -> Pp.string ctx "hue-rotate()"
  | Hue_rotate a -> Pp.call "hue-rotate" pp_angle ctx a
  | Invert n -> Pp.call "invert" (pp_number_percentage ~always:true) ctx n
  | Opacity n -> Pp.call "opacity" (pp_number_percentage ~always:true) ctx n
  | Saturate n -> Pp.call "saturate" (pp_number_percentage ~always:true) ctx n
  | Sepia n -> Pp.call "sepia" (pp_number_percentage ~always:true) ctx n
  | Url url -> Pp.url ctx url
  | List filters ->
      let sep = if Pp.minified ctx then Pp.nop else Pp.space in
      Pp.list ~sep pp_filter ctx filters
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_filter ctx v

let is_font_family_ident_word s =
  let len = String.length s in
  let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false in
  let is_digit = function '0' .. '9' -> true | _ -> false in
  let is_ident_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true
    | _ -> false
  in
  len > 0
  && (is_alpha s.[0] || s.[0] = '_' || s.[0] = '-')
  && (not (len >= 2 && s.[0] = '-' && is_digit s.[1]))
  && String.for_all is_ident_char s

let pp_font_family_name ctx s =
  (* A multi-word named family unquotes under minify (shorter, valid), but the
     CSSOM-canonical serialization quotes it, so enforce_spec keeps the
     quotes. *)
  if Pp.minified ctx && not ctx.Pp.enforce_spec then Pp.string ctx s
  else (
    Pp.char ctx '"';
    Pp.string ctx s;
    Pp.char ctx '"')

let can_unquote_font_family_name s =
  match String.split_on_char ' ' s with
  | _ :: _ :: _ as words ->
      (* CSS Fonts 4 sec. 4.1: a [<family-name>] formed of two or more
         [<custom-ident>]s is unambiguous - none of its words can be picked up
         as a property-level CSS-wide keyword once the parser has committed to a
         multi-token value. So [inherit test] / [revert serif] etc. round-trip
         unquoted, just like [Times New Roman]. *)
      List.for_all is_font_family_ident_word words
  | _ -> false

(* Walk a component stream and rewrite each [<string>] token whose content is a
   multi-word identifier sequence (the [can_unquote_font_family_name] guard) as
   an explicit [<ident>] sequence. Used by the [@property]-registered custom
   property promotion path when the registered syntax accepts [<custom-ident>+]
   - the two forms ([custom-ident>+] vs [<string>]) are spec-equivalent there
   (CSS Fonts 4 sec. 15.3), so the rewrite produces a single canonical AST. The
   guard's "two or more words" rule avoids the CSS-wide-keyword trap (a quoted
   ["inherit"] never collapses to the bare keyword). *)
let unquote_font_family_strings components =
  let changed = ref false in
  let words_of s =
    String.split_on_char ' ' s |> List.filter (fun w -> w <> "")
  in
  let rec interleave loc = function
    | [] -> []
    | [ w ] -> [ Component.Preserved (Token.v ~kind:(Token.Ident w) ~loc) ]
    | w :: rest ->
        Component.Preserved (Token.v ~kind:(Token.Ident w) ~loc)
        :: Component.Preserved (Token.v ~kind:Token.Whitespace ~loc)
        :: interleave loc rest
  in
  let result =
    List.concat_map
      (fun c ->
        match c with
        | Component.Preserved { kind = Token.String { value; _ }; loc }
          when can_unquote_font_family_name value ->
            changed := true;
            interleave loc (words_of value)
        | _ -> [ c ])
      components
  in
  if !changed then result else components

let is_generic_family : font_family -> bool = function
  | Sans_serif | Serif | Monospace | Cursive | Fantasy | System_ui
  | Ui_sans_serif | Ui_serif | Ui_monospace | Ui_rounded | Emoji | Math
  | Fangsong ->
      true
  | _ -> false

let rec pp_font_family : font_family Pp.t =
 fun ctx -> function
  (* Generic CSS font families *)
  | Sans_serif -> Pp.string ctx "sans-serif"
  | Serif -> Pp.string ctx "serif"
  | Monospace -> Pp.string ctx "monospace"
  | Cursive -> Pp.string ctx "cursive"
  | Fantasy -> Pp.string ctx "fantasy"
  | System_ui -> Pp.string ctx "system-ui"
  | Ui_sans_serif -> Pp.string ctx "ui-sans-serif"
  | Ui_serif -> Pp.string ctx "ui-serif"
  | Ui_monospace -> Pp.string ctx "ui-monospace"
  | Ui_rounded -> Pp.string ctx "ui-rounded"
  | Emoji -> Pp.string ctx "emoji"
  | Math -> Pp.string ctx "math"
  | Fangsong -> Pp.string ctx "fangsong"
  (* Popular web fonts *)
  | Inter -> Pp.string ctx "Inter"
  | Roboto -> Pp.string ctx "Roboto"
  | Open_sans -> Pp.string ctx "\"Open Sans\""
  | Lato -> Pp.string ctx "Lato"
  | Montserrat -> Pp.string ctx "Montserrat"
  | Poppins -> Pp.string ctx "Poppins"
  | Source_sans_pro -> Pp.string ctx "\"Source Sans Pro\""
  | Raleway -> Pp.string ctx "Raleway"
  | Oswald -> Pp.string ctx "Oswald"
  | Noto_sans -> Pp.string ctx "\"Noto Sans\""
  | Ubuntu -> Pp.string ctx "Ubuntu"
  | Playfair_display -> Pp.string ctx "\"Playfair Display\""
  | Merriweather -> Pp.string ctx "Merriweather"
  | Lora -> Pp.string ctx "Lora"
  | PT_sans -> Pp.string ctx "\"PT Sans\""
  | PT_serif -> Pp.string ctx "\"PT Serif\""
  | Nunito -> Pp.string ctx "Nunito"
  | Nunito_sans -> Pp.string ctx "\"Nunito Sans\""
  | Work_sans -> Pp.string ctx "\"Work Sans\""
  | Rubik -> Pp.string ctx "Rubik"
  | Fira_sans -> Pp.string ctx "\"Fira Sans\""
  | Fira_code -> Pp.string ctx "\"Fira Code\""
  | JetBrains_mono -> Pp.string ctx "\"JetBrains Mono\""
  | IBM_plex_sans -> Pp.string ctx "\"IBM Plex Sans\""
  | IBM_plex_serif -> Pp.string ctx "\"IBM Plex Serif\""
  | IBM_plex_mono -> Pp.string ctx "\"IBM Plex Mono\""
  | Source_code_pro -> Pp.string ctx "\"Source Code Pro\""
  | Space_mono -> Pp.string ctx "\"Space Mono\""
  | DM_sans -> Pp.string ctx "\"DM Sans\""
  | DM_serif_display -> Pp.string ctx "\"DM Serif Display\""
  | Bebas_neue -> Pp.string ctx "\"Bebas Neue\""
  | Barlow -> Pp.string ctx "Barlow"
  | Mulish -> Pp.string ctx "Mulish"
  | Josefin_sans -> Pp.string ctx "\"Josefin Sans\""
  (* Platform-specific fonts. Multi-word names emit unquoted under minify (CSS
     Fonts 4 sec. 4.1: a [<family-name>] of two or more [<custom-ident>] words
     parses without quotes and is the shorter spelling). Pretty mode keeps the
     quoted form for readability. *)
  | Helvetica -> Pp.string ctx "Helvetica"
  | Helvetica_neue -> pp_font_family_name ctx "Helvetica Neue"
  | Arial -> Pp.string ctx "Arial"
  | Verdana -> Pp.string ctx "Verdana"
  | Tahoma -> Pp.string ctx "Tahoma"
  | Trebuchet_ms -> pp_font_family_name ctx "Trebuchet MS"
  | Times_new_roman -> pp_font_family_name ctx "Times New Roman"
  | Times -> Pp.string ctx "Times"
  | Georgia -> Pp.string ctx "Georgia"
  | Cambria -> Pp.string ctx "Cambria"
  | Garamond -> Pp.string ctx "Garamond"
  | Courier_new -> pp_font_family_name ctx "Courier New"
  | Courier -> Pp.string ctx "Courier"
  | Lucida_console -> pp_font_family_name ctx "Lucida Console"
  | SF_pro -> pp_font_family_name ctx "SF Pro"
  | SF_pro_display -> pp_font_family_name ctx "SF Pro Display"
  | SF_pro_text -> pp_font_family_name ctx "SF Pro Text"
  | SF_mono -> pp_font_family_name ctx "SF Mono"
  | NY -> pp_font_family_name ctx "New York"
  | Segoe_ui -> pp_font_family_name ctx "Segoe UI"
  | Segoe_ui_emoji -> pp_font_family_name ctx "Segoe UI Emoji"
  | Segoe_ui_symbol -> pp_font_family_name ctx "Segoe UI Symbol"
  | Apple_color_emoji -> pp_font_family_name ctx "Apple Color Emoji"
  | Noto_color_emoji -> pp_font_family_name ctx "Noto Color Emoji"
  | Android_emoji -> pp_font_family_name ctx "Android Emoji"
  | Twemoji_mozilla -> pp_font_family_name ctx "Twemoji Mozilla"
  (* Developer fonts *)
  | Menlo -> Pp.string ctx "Menlo"
  | Monaco -> Pp.string ctx "Monaco"
  | Consolas -> Pp.string ctx "Consolas"
  | Liberation_mono -> pp_font_family_name ctx "Liberation Mono"
  | SFMono_regular -> Pp.string ctx "SFMono-Regular"
  | Cascadia_code -> pp_font_family_name ctx "Cascadia Code"
  | Cascadia_mono -> pp_font_family_name ctx "Cascadia Mono"
  | Victor_mono -> pp_font_family_name ctx "Victor Mono"
  | Inconsolata -> Pp.string ctx "Inconsolata"
  | Hack -> Pp.string ctx "Hack"
  (* CSS keywords *)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Name s ->
      let safe_ident_char = function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true
        | _ -> false
      in
      (* A single-word [Name] that matches a generic family / CSS-wide keyword
         must stay quoted - dropping the quotes turns the [<family-name>] into
         the generic keyword (different semantics in [@font-face] and in
         [font-family] cascade). *)
      let collides_with_keyword =
        List.mem (String.lowercase_ascii s)
          [
            "serif";
            "sans-serif";
            "monospace";
            "cursive";
            "fantasy";
            "system-ui";
            "ui-serif";
            "ui-sans-serif";
            "ui-monospace";
            "ui-rounded";
            "emoji";
            "math";
            "fangsong";
            "inherit";
            "initial";
            "unset";
            "revert";
            "revert-layer";
            "none";
            "default";
          ]
      in
      if
        Pp.minified ctx && (not ctx.Pp.enforce_spec)
        && can_unquote_font_family_name s
      then Pp.string ctx s
      else if
        s = ""
        || (not (String.for_all safe_ident_char s))
        || collides_with_keyword
      then Pp.quoted_string ctx s
      else Pp.string ctx s
  | Var v -> pp_var pp_font_family ctx v
  | List fonts ->
      let level_chars =
        match ctx.Pp.indent with Some w -> w * ctx.Pp.level | None -> 0
      in
      (* CSS Fonts 4 sec. 4.1: [font-family] is a fallback list, so a duplicate
         entry never wins under cascade resolution - drop it under minify (the
         first occurrence keeps the source position). A bare generic keyword
         (notably [monospace]) takes the UA generic-font size, so the
         [monospace, monospace] idiom opts back into the normal size; a dedup
         must not collapse a list to a single generic, which would shrink the
         text. *)
      let fonts =
        if Pp.minified ctx then
          let seen = Hashtbl.create 8 in
          let deduped =
            List.filter
              (fun f ->
                let key = Pp.to_string ~minify:true pp_font_family f in
                if Hashtbl.mem seen key then false
                else (
                  Hashtbl.add seen key ();
                  true))
              fonts
          in
          match deduped with
          | [ single ] when is_generic_family single && List.length fonts > 1 ->
              fonts
          | _ -> deduped
        else fonts
      in
      Pp.list_wrap ~threshold:90 ~sep:Pp.comma ~wrap_indent:(level_chars + 2)
        pp_font_family ctx fonts
  | Invalid tokens ->
      let rendered =
        if Pp.minified ctx then Parser.to_string_minified tokens
        else Parser.string_of_components tokens
      in
      Pp.string ctx rendered

(* pp_font_families is no longer needed since Fonts is now part of
   font_family *)

(* [object-view-box] has its own [Inset] / [Xywh] / [Rect] variants distinct
   from [clip-path]'s. Same principle as the clip-path normalisers: the slot
   values are plain [<length>] / [<length-percentage>] positions, not [calc()]
   operands, so the zero-unit drop ([0px] -> [0]) applies (CSS Values L4 sec.
   6.1.1). *)
let normalize_object_view_box (value : object_view_box) : object_view_box =
  match value with
  | Inset (top, right, bottom, left) ->
      let nl = Values.normalize_length ~strip:true in
      let top' = nl top in
      let right' = option_map_preserve nl right in
      let bottom' = option_map_preserve nl bottom in
      let left' = option_map_preserve nl left in
      if top' == top && right' == right && bottom' == bottom && left' == left
      then value
      else Inset (top', right', bottom', left')
  | Xywh r ->
      let lp = Values.normalize_length_percentage ~strip:true in
      let x = lp r.x in
      let y = lp r.y in
      let width = lp r.width in
      let height = lp r.height in
      let rounded =
        option_map_preserve (normalize_border_radius ~strip:true) r.rounded
      in
      if
        x == r.x && y == r.y && width == r.width && height == r.height
        && rounded == r.rounded
      then value
      else Xywh { x; y; width; height; rounded }
  | Rect r ->
      let lp = Values.normalize_length_percentage ~strip:true in
      let top = lp r.top in
      let right = lp r.right in
      let bottom = lp r.bottom in
      let left = lp r.left in
      let rounded =
        option_map_preserve (normalize_border_radius ~strip:true) r.rounded
      in
      if
        top == r.top && right == r.right && bottom == r.bottom && left == r.left
        && rounded == r.rounded
      then value
      else Rect { top; right; bottom; left; rounded }
  | other -> other

let normalize_text_indent : text_indent_value -> text_indent_value =
 fun value ->
  match value with
  | Indent r ->
      let length = Values.normalize_length_percentage r.length in
      if length == r.length then value else Indent { r with length }
  | other -> other

let normalize_animation_range_item :
    animation_range_item -> animation_range_item =
 fun value ->
  match value with
  | Offset lp ->
      preserve_if_equal value (Offset (Values.normalize_length_percentage lp))
  | Named (n, lp) ->
      preserve_if_equal value
        (Named (n, option_map_preserve Values.normalize_length_percentage lp))
  | other -> other

let normalize_animation_range : animation_range -> animation_range =
 fun value ->
  match value with
  | Range (a, b) ->
      preserve_if_equal value
        (Range
           ( normalize_animation_range_item a,
             option_map_preserve normalize_animation_range_item b ))
  | other -> other

let normalize_timeline_inset_item : timeline_inset_item -> timeline_inset_item =
 fun value ->
  match value with
  | Length lp ->
      preserve_if_equal value (Length (Values.normalize_length_percentage lp))
  | other -> other

let normalize_timeline_inset : timeline_inset -> timeline_inset =
 fun value ->
  match value with
  | Inset (a, b) ->
      preserve_if_equal value
        (Inset
           ( normalize_timeline_inset_item a,
             option_map_preserve normalize_timeline_inset_item b ))
  | other -> other

let normalize_aspect_ratio : aspect_ratio -> aspect_ratio =
 fun value ->
  match value with
  | Auto_ratio_calc (a, b) ->
      preserve_if_equal value
        (Auto_ratio_calc (Values.normalize_number a, Values.normalize_number b))
  | Ratio_calc (a, b) ->
      preserve_if_equal value
        (Ratio_calc (Values.normalize_number a, Values.normalize_number b))
  | other -> other

let normalize_text_decoration ?(lossless = false) :
    text_decoration -> text_decoration =
 fun value ->
  match value with
  | Shorthand s ->
      let color = option_map_preserve (normalize_color ~lossless) s.color in
      if color == s.color then value else Shorthand { s with color }
  | other -> other

let normalize_text_emphasis ?(lossless = false) : text_emphasis -> text_emphasis
    =
 fun value ->
  match value with
  | Emphasis (style, color) ->
      preserve_if_equal value
        (Emphasis (style, option_map_preserve (normalize_color ~lossless) color))
  | other -> other

let normalize_text_shadow ?(lossless = false) : text_shadow -> text_shadow =
 fun value ->
  match value with
  | Text_shadow s ->
      preserve_if_equal value
        (Text_shadow
           {
             h_offset = Values.normalize_length s.h_offset;
             v_offset = Values.normalize_length s.v_offset;
             blur = option_map_preserve Values.normalize_length s.blur;
             color = option_map_preserve (normalize_color ~lossless) s.color;
           })
  | other -> other

let rec normalize_filter ?(lossless = false) : filter -> filter =
 fun value ->
  let np = Values.normalize_number_percentage in
  match value with
  | Drop_shadow s ->
      preserve_if_equal value (Drop_shadow (normalize_shadow ~lossless s))
  | Hue_rotate a ->
      preserve_if_equal value (Hue_rotate (Values.normalize_angle a))
  | Brightness x -> preserve_if_equal value (Brightness (np x))
  | Contrast x -> preserve_if_equal value (Contrast (np x))
  | Grayscale x -> preserve_if_equal value (Grayscale (np x))
  | Invert x -> preserve_if_equal value (Invert (np x))
  | Opacity x -> preserve_if_equal value (Opacity (np x))
  | Saturate x -> preserve_if_equal value (Saturate (np x))
  | Sepia x -> preserve_if_equal value (Sepia (np x))
  | List filters ->
      preserve_if_equal value
        (List (map_preserve (normalize_filter ~lossless) filters))
  | other -> other

let normalize_font_style : font_style -> font_style =
  let na = Values.normalize_angle in
  fun value ->
    match value with
    | Oblique_angle a -> preserve_if_equal value (Oblique_angle (na a))
    | Oblique_range (a, b) ->
        preserve_if_equal value (Oblique_range (na a, na b))
    | other -> other

let rec pp_display : display Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Block -> Pp.string ctx "block"
  | Inline -> Pp.string ctx "inline"
  | Inline_block -> Pp.string ctx "inline-block"
  | Flex -> Pp.string ctx "flex"
  | Inline_flex -> Pp.string ctx "inline-flex"
  | Grid -> Pp.string ctx "grid"
  | Inline_grid -> Pp.string ctx "inline-grid"
  | Flow_root -> Pp.string ctx "flow-root"
  | Table -> Pp.string ctx "table"
  | Table_row -> Pp.string ctx "table-row"
  | Table_cell -> Pp.string ctx "table-cell"
  | Table_caption -> Pp.string ctx "table-caption"
  | Table_column -> Pp.string ctx "table-column"
  | Table_column_group -> Pp.string ctx "table-column-group"
  | Table_footer_group -> Pp.string ctx "table-footer-group"
  | Table_header_group -> Pp.string ctx "table-header-group"
  | Table_row_group -> Pp.string ctx "table-row-group"
  | Inline_table -> Pp.string ctx "inline-table"
  | List_item -> Pp.string ctx "list-item"
  | Contents -> Pp.string ctx "contents"
  | Run_in -> Pp.string ctx "run-in"
  | Ruby -> Pp.string ctx "ruby"
  | Ruby_base -> Pp.string ctx "ruby-base"
  | Ruby_text -> Pp.string ctx "ruby-text"
  | Ruby_base_container -> Pp.string ctx "ruby-base-container"
  | Ruby_text_container -> Pp.string ctx "ruby-text-container"
  | Math -> Pp.string ctx "math"
  | Webkit_flex -> Pp.string ctx "-webkit-flex"
  | Webkit_inline_flex -> Pp.string ctx "-webkit-inline-flex"
  | Ms_flexbox -> Pp.string ctx "-ms-flexbox"
  | Webkit_box -> Pp.string ctx "-webkit-box"
  | Moz_box -> Pp.string ctx "-moz-box"
  | Moz_inline_box -> Pp.string ctx "-moz-inline-box"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_display ctx v
  | Multi (Multi (Block, Block), List_item) when Pp.minified ctx ->
      Pp.string ctx "list-item"
  | Multi (Multi (outside, Block), List_item) when Pp.minified ctx ->
      pp_display ctx outside;
      Pp.space ctx ();
      Pp.string ctx "list-item"
  | Multi (Multi (outside, inside), List_item) ->
      pp_display ctx outside;
      Pp.space ctx ();
      pp_display_inside ctx inside;
      Pp.space ctx ();
      Pp.string ctx "list-item"
  (* CSS Display 3 sec. 2: [<display-outside> <display-inside>] with
     [<display-inside>] = [flow] (encoded as the inner [Block] arm here)
     collapses to just the outside keyword - [block flow] -> [block], [inline
     flow] -> [inline]. *)
  | Multi (Block, Block) when Pp.minified ctx -> Pp.string ctx "block"
  | Multi (Inline, Block) when Pp.minified ctx -> Pp.string ctx "inline"
  | Multi (Run_in, Block) when Pp.minified ctx -> Pp.string ctx "run-in"
  | Multi (Block, Flow_root) when Pp.minified ctx -> Pp.string ctx "flow-root"
  | Multi (Inline, Flow_root) when Pp.minified ctx ->
      (* CSS Display 3 sec. 2.6: [inline flow-root] is the two-value equivalent
         of the legacy [inline-block] keyword. *)
      Pp.string ctx "inline-block"
  | Multi (Block, Flex) when Pp.minified ctx -> Pp.string ctx "flex"
  | Multi (Inline, Flex) when Pp.minified ctx -> Pp.string ctx "inline-flex"
  | Multi (Block, Grid) when Pp.minified ctx -> Pp.string ctx "grid"
  | Multi (Inline, Grid) when Pp.minified ctx -> Pp.string ctx "inline-grid"
  | Multi (Block, Table) when Pp.minified ctx -> Pp.string ctx "table"
  | Multi (Inline, Table) when Pp.minified ctx -> Pp.string ctx "inline-table"
  | Multi (Block, Ruby) when Pp.minified ctx -> Pp.string ctx "ruby"
  | Multi (outside, inside) ->
      pp_display ctx outside;
      Pp.space ctx ();
      pp_display_inside ctx inside

and pp_display_inside ctx = function
  | Block -> Pp.string ctx "flow"
  | Flow_root -> Pp.string ctx "flow-root"
  | display -> pp_display ctx display

let rec pp_position : position Pp.t =
 fun ctx -> function
  | Static -> Pp.string ctx "static"
  | Relative -> Pp.string ctx "relative"
  | Absolute -> Pp.string ctx "absolute"
  | Fixed -> Pp.string ctx "fixed"
  | Sticky -> Pp.string ctx "sticky"
  | Webkit_sticky -> Pp.string ctx "-webkit-sticky"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> Values.pp_var pp_position ctx v

let rec pp_visibility : visibility Pp.t =
 fun ctx -> function
  | Visible -> Pp.string ctx "visible"
  | Hidden -> Pp.string ctx "hidden"
  | Collapse -> Pp.string ctx "collapse"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> Values.pp_var pp_visibility ctx v

let rec pp_z_index : z_index Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Index i -> Pp.int ctx i
  | Calc c -> pp_calc pp_z_index ctx c
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_z_index ctx v

let rec pp_tab_size : tab_size Pp.t =
 fun ctx -> function
  | Int i -> Pp.int ctx i
  | Length len -> pp_length ~always:true ctx len
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_tab_size ctx v

let rec pp_zoom : zoom Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Reset -> Pp.string ctx "reset"
  | Num n -> Pp.float ctx n
  | Pct p -> Pp.pct ctx p
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_zoom ctx v

(* Opacity as a float (0.0-1.0). Tailwind's minifier writes percentages as
   decimals (50% -> .5), so minified output emits decimals to match. *)
let rec pp_opacity : opacity Pp.t =
 fun ctx -> function
  | Opacity_number f -> Pp.float ctx f
  | Calc c -> pp_calc pp_opacity ctx c
  | Abs v -> Pp.call "abs" pp_opacity ctx v
  | Sign v -> Pp.call "sign" pp_opacity ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_opacity ctx v

(* Inside an [opacity] [calc()], a raw [<number>] / [<percentage>] is modelled
   at the calc level as the [Num x] node rather than [Val (Opacity_number x)].
   The [_dim_only] reader excludes the unitless alternative so
   [read_calc_factor] falls through to its own [Num] path, matching the
   [<number-percentage>] convention. *)
let rec read_opacity_dim_only t : opacity =
  Cursor.ws t;
  Cursor.one_of
    [
      (* A [<percentage>] operand is the number it denotes (50% = 0.5), matching
         how a bare opacity percentage parses. A raw [<number>] is excluded so
         [read_calc] falls through to its own [Num] path. *)
      (fun t -> (Opacity_number (Cursor.pct t /. 100.) : opacity));
      (fun t ->
        Cursor.enum_or_calls "opacity"
          [
            ("inherit", (Inherit : opacity));
            ("initial", Initial);
            ("unset", Unset);
            ("revert", Revert);
            ("revert-layer", Revert_layer);
          ]
          ~calls:[ ("var", fun t -> Var (read_var read_opacity_dim_only t)) ]
          ~default:(fun t ->
            Cursor.err_expected t "opacity (var/calc inside calc)")
          t);
    ]
    t

let rec read_opacity t : opacity =
  let read_var t : opacity = Var (read_var read_opacity t) in
  let read_numeric_math t : opacity =
    Opacity_number (Values.read_numeric_expression t)
  in
  let read_number_or_percentage t =
    let n, unit = Cursor.number_with_unit t in
    let value =
      match unit with
      | Some "%" -> n /. 100.
      | Some unit -> Cursor.err_invalid t ("opacity unit: " ^ unit)
      | None -> n
    in
    Opacity_number value
  in
  Cursor.enum_or_calls "opacity"
    [
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:
      [
        ("var", read_var);
        ("calc", fun t -> Calc (Values.read_calc read_opacity_dim_only t));
        ("min", read_numeric_math);
        ("max", read_numeric_math);
        ("clamp", read_numeric_math);
        ( "abs",
          fun t ->
            Cursor.call "abs" t (fun inner ->
                (Abs (read_opacity inner) : opacity)) );
        ( "sign",
          fun t ->
            Cursor.call "sign" t (fun inner ->
                (Sign (read_opacity inner) : opacity)) );
      ]
    ~default:read_number_or_percentage t

let rec pp_shape_image_threshold : shape_image_threshold Pp.t =
 fun ctx -> function
  | Number n -> Pp.float ctx n
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_shape_image_threshold ctx v

let rec read_shape_image_threshold t : shape_image_threshold =
  let read_var t : shape_image_threshold =
    Var (read_var read_shape_image_threshold t)
  in
  let read_number t : shape_image_threshold =
    let value = Cursor.number t in
    if value < 0. || value > 1. then
      Cursor.err_invalid t "shape-image-threshold must be between 0 and 1";
    Number value
  in
  Cursor.enum_or_calls "shape-image-threshold"
    [
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:read_number t

let rec pp_overflow : overflow Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_overflow ctx v
  | Visible -> Pp.string ctx "visible"
  | Hidden -> Pp.string ctx "hidden"
  | Scroll -> Pp.string ctx "scroll"
  | Auto -> Pp.string ctx "auto"
  | Clip -> Pp.string ctx "clip"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Overflow_pair (x, y) when Pp.minified ctx && x = y -> pp_overflow ctx x
  | Overflow_pair (x, y) ->
      pp_overflow ctx x;
      Pp.space ctx ();
      pp_overflow ctx y

let pp_overflow_clip_box : overflow_clip_box Pp.t =
 fun ctx -> function
  | Content_box -> Pp.string ctx "content-box"
  | Padding_box -> Pp.string ctx "padding-box"
  | Border_box -> Pp.string ctx "border-box"

let rec pp_overflow_clip_margin : overflow_clip_margin Pp.t =
 fun ctx -> function
  | Clip_margin (Some box, Some length) ->
      pp_overflow_clip_box ctx box;
      Pp.space ctx ();
      pp_length ~always:true ctx length
  | Clip_margin (Some box, None) -> pp_overflow_clip_box ctx box
  | Clip_margin (None, Some length) -> pp_length ~always:true ctx length
  | Clip_margin (None, None) -> ()
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_overflow_clip_margin ctx v

let read_overflow_clip_box t : overflow_clip_box =
  Cursor.enum "overflow-clip-margin box"
    [
      ("content-box", (Content_box : overflow_clip_box));
      ("padding-box", Padding_box);
      ("border-box", Border_box);
    ]
    t

let read_overflow_clip_box_item (box : overflow_clip_box option ref) t =
  Cursor.ws t;
  let snap = Cursor.save t in
  match !box with
  | None -> (
      match read_overflow_clip_box t with
      | value ->
          box := Some value;
          true
      | exception Error.Parse_error _ ->
          Cursor.restore t snap;
          false)
  | Some _ -> false

let read_overflow_clip_length_item (length : length option ref) t =
  Cursor.ws t;
  match !length with
  | None ->
      length := Some (read_length ~allow_negative:false ~with_keywords:false t);
      true
  | Some _ -> false

let rec read_overflow_clip_margin_items box length consumed t =
  Cursor.ws t;
  if Cursor.is_done t || Cursor.peek_semicolon t then consumed
  else
    let snap = Cursor.save t in
    if
      read_overflow_clip_box_item box t
      || read_overflow_clip_length_item length t
    then read_overflow_clip_margin_items box length true t
    else (
      Cursor.restore t snap;
      consumed)

let rec read_overflow_clip_margin t : overflow_clip_margin =
  let read_var t : overflow_clip_margin =
    Var (read_var read_overflow_clip_margin t)
  in
  let read_value t =
    let box : overflow_clip_box option ref =
      ref (None : overflow_clip_box option)
    in
    let length : length option ref = ref (None : length option) in
    if not (read_overflow_clip_margin_items box length false t) then
      Cursor.err_expected t "overflow-clip-margin";
    Clip_margin (!box, !length)
  in
  Cursor.enum_or_calls "overflow-clip-margin"
    [
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:read_value t

let rec pp_font_style : font_style Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_font_style ctx v
  | Normal -> Pp.string ctx "normal"
  | Italic -> Pp.string ctx "italic"
  | Oblique -> Pp.string ctx "oblique"
  | Oblique_angle angle ->
      Pp.string ctx "oblique";
      Pp.space ctx ();
      pp_angle ctx angle
  | Oblique_range (first, second) ->
      Pp.string ctx "oblique";
      Pp.space ctx ();
      pp_angle ctx first;
      Pp.space ctx ();
      pp_angle ctx second
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_text_align : text_align Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_align ctx v
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Center -> Pp.string ctx "center"
  | Justify -> Pp.string ctx "justify"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Match_parent -> Pp.string ctx "match-parent"
  | Webkit_match_parent -> Pp.string ctx "-webkit-match-parent"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_text_indent_value : text_indent_value Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_indent_value ctx v
  | Indent { length; hanging; each_line } ->
      pp_length_percentage ctx length;
      if hanging then (
        Pp.space ctx ();
        Pp.string ctx "hanging");
      if each_line then (
        Pp.space ctx ();
        Pp.string ctx "each-line")
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_text_decoration_line : text_decoration_line Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_decoration_line ctx v
  | None -> Pp.string ctx "none"
  | Underline -> Pp.string ctx "underline"
  | Overline -> Pp.string ctx "overline"
  | Line_through -> Pp.string ctx "line-through"
  | Blink -> Pp.string ctx "blink"
  | Spelling_error -> Pp.string ctx "spelling-error"
  | Grammar_error -> Pp.string ctx "grammar-error"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_text_decoration_style : text_decoration_style Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_decoration_style ctx v
  | Solid -> Pp.string ctx "solid"
  | Double -> Pp.string ctx "double"
  | Dotted -> Pp.string ctx "dotted"
  | Dashed -> Pp.string ctx "dashed"
  | Wavy -> Pp.string ctx "wavy"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_text_decoration_shorthand : text_decoration_shorthand Pp.t =
 fun ctx { lines; style; color; thickness } ->
  let first = ref true in
  let space_if_needed () = if !first then first := false else Pp.space ctx () in
  (* CSS Text Decoration 4 sec. 2: under minify, drop components that equal the
     longhand default ([style: solid], [color: currentcolor]); the shorthand is
     interpreted with the dropped fields restored to default. *)
  let drop_default = Pp.minified ctx in
  let style : text_decoration_style option =
    if drop_default then match style with Some Solid -> None | s -> s
    else style
  in
  let color : Values.color option =
    if drop_default then match color with Some Values.Current -> None | c -> c
    else color
  in
  (match lines with
  | [] -> ()
  | ls ->
      space_if_needed ();
      Pp.list ~sep:Pp.space pp_text_decoration_line ctx ls);
  (match style with
  | None -> ()
  | Some s ->
      space_if_needed ();
      pp_text_decoration_style ctx s);
  (match color with
  | None -> ()
  | Some c ->
      space_if_needed ();
      pp_color ctx c);
  match thickness with
  | None -> ()
  | Some l ->
      space_if_needed ();
      pp_length ctx l

let rec pp_text_decoration : text_decoration Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Shorthand shorthand -> pp_text_decoration_shorthand ctx shorthand
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_decoration ctx v

let rec pp_text_decoration_skip : text_decoration_skip Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Auto -> Pp.string ctx "auto"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_decoration_skip ctx v

let rec pp_text_decoration_skip_self : text_decoration_skip_self Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Objects -> Pp.string ctx "objects"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_decoration_skip_self ctx v

let rec pp_text_decoration_skip_box : text_decoration_skip_box Pp.t =
 fun ctx -> function
  | All -> Pp.string ctx "all"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_decoration_skip_box ctx v

let rec pp_text_decoration_skip_inset : text_decoration_skip_inset Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Auto -> Pp.string ctx "auto"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_decoration_skip_inset ctx v

let pp_text_decoration_skip_space : text_decoration_skip_space Pp.t =
 fun ctx -> function
  | All -> Pp.string ctx "all"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"

let rec pp_text_decoration_skip_spaces : text_decoration_skip_spaces Pp.t =
 fun ctx -> function
  | Spaces spaces ->
      Pp.list ~sep:Pp.space pp_text_decoration_skip_space ctx spaces
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_decoration_skip_spaces ctx v

let pp_text_transform_case ctx = function
  | (Capitalize : text_transform_case) -> Pp.string ctx "capitalize"
  | Uppercase -> Pp.string ctx "uppercase"
  | Lowercase -> Pp.string ctx "lowercase"

let rec pp_text_transform : text_transform Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Case c -> pp_text_transform_case ctx c
  | Combo { case; full_width; full_size_kana } ->
      let first = ref true in
      let space () = if !first then first := false else Pp.space ctx () in
      Option.iter
        (fun c ->
          space ();
          pp_text_transform_case ctx c)
        case;
      if full_width then (
        space ();
        Pp.string ctx "full-width");
      if full_size_kana then (
        space ();
        Pp.string ctx "full-size-kana")
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_transform ctx v

let rec pp_text_overflow : text_overflow Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_overflow ctx v
  | Clip -> Pp.string ctx "clip"
  | Ellipsis -> Pp.string ctx "ellipsis"
  | String s -> Pp.quoted_string ctx s
  | Pair (first, second) ->
      pp_text_overflow ctx first;
      Pp.space ctx ();
      pp_text_overflow ctx second
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_text_wrap : text_wrap Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_wrap ctx v
  | Wrap -> Pp.string ctx "wrap"
  | No_wrap -> Pp.string ctx "nowrap"
  | Balance -> Pp.string ctx "balance"
  | Pretty -> Pp.string ctx "pretty"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_text_wrap_mode : text_wrap_mode Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_wrap_mode ctx v
  | Wrap -> Pp.string ctx "wrap"
  | No_wrap -> Pp.string ctx "nowrap"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_text_wrap_style : text_wrap_style Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_wrap_style ctx v
  | Auto -> Pp.string ctx "auto"
  | Balance -> Pp.string ctx "balance"
  | Pretty -> Pp.string ctx "pretty"
  | Stable -> Pp.string ctx "stable"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_text_box_trim : text_box_trim Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_box_trim ctx v
  | None -> Pp.string ctx "none"
  | Trim_start -> Pp.string ctx "trim-start"
  | Trim_end -> Pp.string ctx "trim-end"
  | Trim_both -> Pp.string ctx "trim-both"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_text_box_edge_keyword : text_box_edge_keyword Pp.t =
 fun ctx -> function
  | Text -> Pp.string ctx "text"
  | Cap -> Pp.string ctx "cap"
  | Ex -> Pp.string ctx "ex"
  | Alphabetic -> Pp.string ctx "alphabetic"
  | Ideographic -> Pp.string ctx "ideographic"
  | Ideographic_ink -> Pp.string ctx "ideographic-ink"

let rec pp_text_box_edge : text_box_edge Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_box_edge ctx v
  | Auto -> Pp.string ctx "auto"
  | Edge (first, second) ->
      pp_text_box_edge_keyword ctx first;
      Option.iter
        (fun keyword ->
          Pp.space ctx ();
          pp_text_box_edge_keyword ctx keyword)
        second
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_text_box : text_box Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_box ctx v
  | Box (trim, edge) ->
      pp_text_box_trim ctx trim;
      Option.iter
        (fun edge ->
          Pp.space ctx ();
          pp_text_box_edge ctx edge)
        edge
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_inline_sizing : inline_sizing Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_inline_sizing ctx v
  | Normal -> Pp.string ctx "normal"
  | Stretch -> Pp.string ctx "stretch"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_line_fit_edge_keyword : line_fit_edge_keyword Pp.t =
 fun ctx -> function
  | Leading -> Pp.string ctx "leading"
  | Text -> Pp.string ctx "text"
  | Cap -> Pp.string ctx "cap"
  | Ex -> Pp.string ctx "ex"
  | Alphabetic -> Pp.string ctx "alphabetic"
  | Ideographic -> Pp.string ctx "ideographic"
  | Ideographic_ink -> Pp.string ctx "ideographic-ink"

let rec pp_line_fit_edge : line_fit_edge Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_line_fit_edge ctx v
  | Edge (first, second) ->
      pp_line_fit_edge_keyword ctx first;
      Option.iter
        (fun keyword ->
          Pp.space ctx ();
          pp_line_fit_edge_keyword ctx keyword)
        second
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_interpolate_size : interpolate_size Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_interpolate_size ctx v
  | Numeric_only -> Pp.string ctx "numeric-only"
  | Allow_keywords -> Pp.string ctx "allow-keywords"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_min_intrinsic_sizing_keyword : min_intrinsic_sizing_keyword Pp.t =
 fun ctx -> function
  | Legacy -> Pp.string ctx "legacy"
  | Zero_if_scroll -> Pp.string ctx "zero-if-scroll"
  | Zero_if_extrinsic -> Pp.string ctx "zero-if-extrinsic"

let rec pp_min_intrinsic_sizing : min_intrinsic_sizing Pp.t =
 fun ctx -> function
  | Sizing keywords ->
      Pp.list ~sep:Pp.space pp_min_intrinsic_sizing_keyword ctx keywords
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_min_intrinsic_sizing ctx v

let rec pp_text_spacing_trim : text_spacing_trim Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_spacing_trim ctx v
  | Normal -> Pp.string ctx "normal"
  | Space_all -> Pp.string ctx "space-all"
  | Trim_start -> Pp.string ctx "trim-start"
  | Space_first -> Pp.string ctx "space-first"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_hyphenate_limit_chars : hyphenate_limit_chars Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_hyphenate_limit_chars ctx v
  | Auto -> Pp.string ctx "auto"
  | One a -> Pp.int ctx a
  | Two (a, b) ->
      Pp.int ctx a;
      Pp.space ctx ();
      Pp.int ctx b
  | Three (a, b, c) ->
      Pp.int ctx a;
      Pp.space ctx ();
      Pp.int ctx b;
      Pp.space ctx ();
      Pp.int ctx c
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_initial_letter : initial_letter Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_initial_letter ctx v
  | Normal -> Pp.string ctx "normal"
  | Drop -> Pp.string ctx "drop"
  | Raise -> Pp.string ctx "raise"
  | Size size -> Pp.float ctx size
  | Size_sink (size, sink) ->
      Pp.float ctx size;
      Pp.space ctx ();
      Pp.int ctx sink
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_initial_letter_align_keyword : initial_letter_align_keyword Pp.t =
 fun ctx -> function
  | Alphabetic -> Pp.string ctx "alphabetic"
  | Ideographic -> Pp.string ctx "ideographic"
  | Hanging -> Pp.string ctx "hanging"
  | Leading -> Pp.string ctx "leading"
  | Border_box -> Pp.string ctx "border-box"

let rec pp_initial_letter_align : initial_letter_align Pp.t =
 fun ctx -> function
  | Align keywords ->
      Pp.list ~sep:Pp.space pp_initial_letter_align_keyword ctx keywords
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_initial_letter_align ctx v

let rec pp_initial_letter_wrap : initial_letter_wrap Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | First -> Pp.string ctx "first"
  | All -> Pp.string ctx "all"
  | Grid -> Pp.string ctx "grid"
  | Length value -> pp_length_percentage ~always:true ctx value
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_initial_letter_wrap ctx v

let rec pp_ruby_merge : ruby_merge Pp.t =
 fun ctx -> function
  | Separate -> Pp.string ctx "separate"
  | Merge -> Pp.string ctx "merge"
  | Auto -> Pp.string ctx "auto"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_ruby_merge ctx v

let rec pp_ruby_align : ruby_align Pp.t =
 fun ctx -> function
  | Start -> Pp.string ctx "start"
  | Center -> Pp.string ctx "center"
  | Space_between -> Pp.string ctx "space-between"
  | Space_around -> Pp.string ctx "space-around"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_ruby_align ctx v

let rec pp_ruby_overhang : ruby_overhang Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_ruby_overhang ctx v

let pp_ruby_position_keyword : ruby_position_keyword Pp.t =
 fun ctx -> function
  | Alternate -> Pp.string ctx "alternate"
  | Over -> Pp.string ctx "over"
  | Under -> Pp.string ctx "under"
  | Inter_character -> Pp.string ctx "inter-character"

let rec pp_ruby_position : ruby_position Pp.t =
 fun ctx (value : ruby_position) ->
  match value with
  | Position keywords ->
      Pp.list ~sep:Pp.space pp_ruby_position_keyword ctx keywords
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_ruby_position ctx v

let rec pp_white_space : white_space Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_white_space ctx v
  | Normal -> Pp.string ctx "normal"
  | Nowrap -> Pp.string ctx "nowrap"
  | Pre -> Pp.string ctx "pre"
  | Pre_wrap -> Pp.string ctx "pre-wrap"
  | Pre_line -> Pp.string ctx "pre-line"
  | Break_spaces -> Pp.string ctx "break-spaces"
  | Preserve_nowrap -> Pp.string ctx "preserve nowrap"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_word_break : word_break Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_word_break ctx v
  | Normal -> Pp.string ctx "normal"
  | Break_all -> Pp.string ctx "break-all"
  | Keep_all -> Pp.string ctx "keep-all"
  | Break_word -> Pp.string ctx "break-word"
  | Auto_phrase -> Pp.string ctx "auto-phrase"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_overflow_wrap : overflow_wrap Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_overflow_wrap ctx v
  | Normal -> Pp.string ctx "normal"
  | Break_word -> Pp.string ctx "break-word"
  | Anywhere -> Pp.string ctx "anywhere"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_hyphens : hyphens Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_hyphens ctx v
  | None -> Pp.string ctx "none"
  | Manual -> Pp.string ctx "manual"
  | Auto -> Pp.string ctx "auto"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_list_style_symbol_sep ctx first (symbol : list_style_symbol) =
  if !first then first := false
  else
    match symbol with
    | String _ when Pp.minified ctx -> ()
    | _ -> Pp.space ctx ()

let pp_symbols_type ctx (kind : symbols_type) =
  match kind with
  | Cyclic -> Pp.string ctx "cyclic"
  | Numeric -> Pp.string ctx "numeric"
  | Alphabetic -> Pp.string ctx "alphabetic"
  | Symbolic -> Pp.string ctx "symbolic"
  | Fixed -> Pp.string ctx "fixed"

let pp_list_style_symbol ctx (symbol : list_style_symbol) =
  match symbol with
  | String symbol -> Pp.quoted_string ctx symbol
  | Url url -> Pp.url ctx url

let pp_list_style_symbols ctx (kind, symbols) =
  let first = ref true in
  let sep symbol = pp_list_style_symbol_sep ctx first symbol in
  let kind =
    match kind with
    | Option.Some Symbolic when Pp.minified ctx -> Option.None
    | kind -> (kind : symbols_type option)
  in
  Option.iter
    (fun kind ->
      sep (String "");
      pp_symbols_type ctx kind)
    kind;
  List.iter
    (fun symbol ->
      sep symbol;
      pp_list_style_symbol ctx symbol)
    symbols

let rec pp_list_style_type : list_style_type Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Disc -> Pp.string ctx "disc"
  | Circle -> Pp.string ctx "circle"
  | Square -> Pp.string ctx "square"
  | Decimal -> Pp.string ctx "decimal"
  | Lower_alpha -> Pp.string ctx "lower-alpha"
  | Upper_alpha -> Pp.string ctx "upper-alpha"
  | Lower_roman -> Pp.string ctx "lower-roman"
  | Upper_roman -> Pp.string ctx "upper-roman"
  | Decimal_leading_zero -> Pp.string ctx "decimal-leading-zero"
  | Arabic_indic -> Pp.string ctx "arabic-indic"
  | Armenian -> Pp.string ctx "armenian"
  | Upper_armenian -> Pp.string ctx "upper-armenian"
  | Lower_armenian -> Pp.string ctx "lower-armenian"
  | Bengali -> Pp.string ctx "bengali"
  | Cambodian -> Pp.string ctx "cambodian"
  | Khmer -> Pp.string ctx "khmer"
  | Cjk_decimal -> Pp.string ctx "cjk-decimal"
  | Devanagari -> Pp.string ctx "devanagari"
  | Georgian -> Pp.string ctx "georgian"
  | Gujarati -> Pp.string ctx "gujarati"
  | Gurmukhi -> Pp.string ctx "gurmukhi"
  | Hebrew -> Pp.string ctx "hebrew"
  | Kannada -> Pp.string ctx "kannada"
  | Lao -> Pp.string ctx "lao"
  | Malayalam -> Pp.string ctx "malayalam"
  | Mongolian -> Pp.string ctx "mongolian"
  | Myanmar -> Pp.string ctx "myanmar"
  | Oriya -> Pp.string ctx "oriya"
  | Persian -> Pp.string ctx "persian"
  | Tamil -> Pp.string ctx "tamil"
  | Telugu -> Pp.string ctx "telugu"
  | Thai -> Pp.string ctx "thai"
  | Tibetan -> Pp.string ctx "tibetan"
  | Lower_latin -> Pp.string ctx "lower-latin"
  | Upper_latin -> Pp.string ctx "upper-latin"
  | Cjk_earthly_branch -> Pp.string ctx "cjk-earthly-branch"
  | Cjk_heavenly_stem -> Pp.string ctx "cjk-heavenly-stem"
  | Lower_greek -> Pp.string ctx "lower-greek"
  | Hiragana -> Pp.string ctx "hiragana"
  | Hiragana_iroha -> Pp.string ctx "hiragana-iroha"
  | Katakana -> Pp.string ctx "katakana"
  | Katakana_iroha -> Pp.string ctx "katakana-iroha"
  | Disclosure_open -> Pp.string ctx "disclosure-open"
  | Disclosure_closed -> Pp.string ctx "disclosure-closed"
  | Cjk_ideographic -> Pp.string ctx "cjk-ideographic"
  | Japanese_informal -> Pp.string ctx "japanese-informal"
  | Japanese_formal -> Pp.string ctx "japanese-formal"
  | Korean_hangul_formal -> Pp.string ctx "korean-hangul-formal"
  | Korean_hanja_informal -> Pp.string ctx "korean-hanja-informal"
  | Korean_hanja_formal -> Pp.string ctx "korean-hanja-formal"
  | Simp_chinese_informal -> Pp.string ctx "simp-chinese-informal"
  | Simp_chinese_formal -> Pp.string ctx "simp-chinese-formal"
  | Trad_chinese_informal -> Pp.string ctx "trad-chinese-informal"
  | Trad_chinese_formal -> Pp.string ctx "trad-chinese-formal"
  | Ethiopic_numeric -> Pp.string ctx "ethiopic-numeric"
  | String s -> Pp.quoted_string ctx s
  | Symbols (kind, symbols) ->
      Pp.call "symbols" pp_list_style_symbols ctx (kind, symbols)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_list_style_type ctx v

let rec pp_list_style_position : list_style_position Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_list_style_position ctx v
  | Inside -> Pp.string ctx "inside"
  | Outside -> Pp.string ctx "outside"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_list_style_image : list_style_image Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Url u -> Pp.url ctx u
  | Var v -> pp_var pp_list_style_image ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_list_style_shorthand : list_style_shorthand Pp.t =
 fun ctx { type_; position; image } ->
  let drop = Pp.minified ctx in
  let type_ =
    drop_default_if ~drop
      ~is_default:(fun (t : list_style_type) -> t = Disc)
      type_
  in
  let position =
    drop_default_if ~drop
      ~is_default:(fun (p : list_style_position) -> p = Outside)
      position
  in
  let image =
    drop_default_if ~drop
      ~is_default:(fun (i : list_style_image) -> i = None)
      image
  in
  let is_none_type = type_ = Some (None : list_style_type) in
  let is_none_image = image = Some (None : list_style_image) in
  if is_none_type && is_none_image && position = Option.None && drop then
    Pp.string ctx "none"
  else
    let first = ref true in
    let emit pp = function
      | Option.None -> ()
      | Some v ->
          if !first then first := false else Pp.space ctx ();
          pp ctx v
    in
    emit pp_list_style_type type_;
    emit pp_list_style_position position;
    emit pp_list_style_image image;
    (* Everything was an initial value and got dropped: the shorthand still
       needs one token. Emit the type initial [disc] (the shortest spelling of
       the all-initial value), not the position initial [outside] - a lone
       [outside] would set the position, changing nothing, but [disc] is shorter
       and is the canonical single-value form. *)
    if !first then Pp.string ctx "disc"

let rec pp_list_style : list_style Pp.t =
 fun ctx -> function
  | Shorthand sh -> pp_list_style_shorthand ctx sh
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_list_style ctx v

let rec pp_table_layout : table_layout Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_table_layout ctx v
  | Auto -> Pp.string ctx "auto"
  | Fixed -> Pp.string ctx "fixed"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_vertical_align : vertical_align Pp.t =
 fun ctx -> function
  | Baseline -> Pp.string ctx "baseline"
  | Top -> Pp.string ctx "top"
  | Middle -> Pp.string ctx "middle"
  | Bottom -> Pp.string ctx "bottom"
  | Text_top -> Pp.string ctx "text-top"
  | Text_bottom -> Pp.string ctx "text-bottom"
  | Sub -> Pp.string ctx "sub"
  | Super -> Pp.string ctx "super"
  | Zero -> Pp.string ctx "0"
  | Px f -> Pp.unit ctx f "px"
  | Rem f -> Pp.unit ctx f "rem"
  | Em f -> Pp.unit ctx f "em"
  | Pct p -> Pp.pct ctx p
  | Calc c -> pp_calc pp_vertical_align ctx c
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_vertical_align ctx v

let pp_aspect_ratio_number ctx value =
  match (Pp.minified ctx, eval_number_value value) with
  | true, Some value -> Pp.float ctx value
  | _ -> pp_number ctx value

let pp_aspect_ratio_pair ctx a b =
  let b_value = eval_number_value b in
  if Pp.minified ctx && b_value = Some 1. then pp_aspect_ratio_number ctx a
  else (
    pp_aspect_ratio_number ctx a;
    if b_value <> Some 1. then (
      Pp.op_char ctx '/';
      pp_aspect_ratio_number ctx b))

let rec pp_aspect_ratio : aspect_ratio Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Auto_ratio (a, b) ->
      Pp.string ctx "auto";
      Pp.space ctx ();
      pp_aspect_ratio ctx (Ratio (a, b))
  | Auto_ratio_calc (a, b) ->
      Pp.string ctx "auto";
      Pp.space ctx ();
      pp_aspect_ratio_pair ctx a b
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_aspect_ratio ctx v
  | Ratio_calc (a, b) -> pp_aspect_ratio_pair ctx a b
  | Ratio (a, b) ->
      if b = 1.0 then
        (* Single number case - don't show "/1" *)
        Pp.float ctx a
      else (
        Pp.float ctx a;
        Pp.op_char ctx '/';
        Pp.float ctx b)

let pp_property : type a. a property Pp.t =
 fun ctx -> function
  | Custom_property name -> Pp.string ctx name
  | Unknown_property name -> Pp.string ctx name
  | All -> Pp.string ctx "all"
  | Background_color -> Pp.string ctx "background-color"
  | Color -> Pp.string ctx "color"
  | Border_color -> Pp.string ctx "border-color"
  | Border_style -> Pp.string ctx "border-style"
  | Border_top_style -> Pp.string ctx "border-top-style"
  | Border_right_style -> Pp.string ctx "border-right-style"
  | Border_bottom_style -> Pp.string ctx "border-bottom-style"
  | Border_left_style -> Pp.string ctx "border-left-style"
  | Border_inline_start_style -> Pp.string ctx "border-inline-start-style"
  | Border_inline_end_style -> Pp.string ctx "border-inline-end-style"
  | Border_block_start_style -> Pp.string ctx "border-block-start-style"
  | Border_block_end_style -> Pp.string ctx "border-block-end-style"
  | Padding -> Pp.string ctx "padding"
  | Padding_left -> Pp.string ctx "padding-left"
  | Padding_right -> Pp.string ctx "padding-right"
  | Padding_bottom -> Pp.string ctx "padding-bottom"
  | Padding_top -> Pp.string ctx "padding-top"
  | Padding_inline -> Pp.string ctx "padding-inline"
  | Padding_inline_start -> Pp.string ctx "padding-inline-start"
  | Padding_inline_end -> Pp.string ctx "padding-inline-end"
  | Padding_block -> Pp.string ctx "padding-block"
  | Padding_block_start -> Pp.string ctx "padding-block-start"
  | Padding_block_end -> Pp.string ctx "padding-block-end"
  | Margin -> Pp.string ctx "margin"
  | Margin_inline_end -> Pp.string ctx "margin-inline-end"
  | Margin_inline_start -> Pp.string ctx "margin-inline-start"
  | Margin_left -> Pp.string ctx "margin-left"
  | Margin_right -> Pp.string ctx "margin-right"
  | Margin_top -> Pp.string ctx "margin-top"
  | Margin_bottom -> Pp.string ctx "margin-bottom"
  | Margin_inline -> Pp.string ctx "margin-inline"
  | Margin_block -> Pp.string ctx "margin-block"
  | Margin_block_start -> Pp.string ctx "margin-block-start"
  | Margin_block_end -> Pp.string ctx "margin-block-end"
  | Gap -> Pp.string ctx "gap"
  | Column_gap -> Pp.string ctx "column-gap"
  | Row_gap -> Pp.string ctx "row-gap"
  | Width -> Pp.string ctx "width"
  | Height -> Pp.string ctx "height"
  | Min_width -> Pp.string ctx "min-width"
  | Min_height -> Pp.string ctx "min-height"
  | Max_width -> Pp.string ctx "max-width"
  | Max_height -> Pp.string ctx "max-height"
  | Inline_size -> Pp.string ctx "inline-size"
  | Min_inline_size -> Pp.string ctx "min-inline-size"
  | Max_inline_size -> Pp.string ctx "max-inline-size"
  | Block_size -> Pp.string ctx "block-size"
  | Min_block_size -> Pp.string ctx "min-block-size"
  | Max_block_size -> Pp.string ctx "max-block-size"
  | Font_size -> Pp.string ctx "font-size"
  | Line_height -> Pp.string ctx "line-height"
  | Font_weight -> Pp.string ctx "font-weight"
  | Font_style -> Pp.string ctx "font-style"
  | Text_align -> Pp.string ctx "text-align"
  | Text_decoration -> Pp.string ctx "text-decoration"
  | Text_decoration_line -> Pp.string ctx "text-decoration-line"
  | Text_decoration_style -> Pp.string ctx "text-decoration-style"
  | Text_decoration_color -> Pp.string ctx "text-decoration-color"
  | Text_decoration_thickness -> Pp.string ctx "text-decoration-thickness"
  | Text_underline_offset -> Pp.string ctx "text-underline-offset"
  | Text_decoration_skip -> Pp.string ctx "text-decoration-skip"
  | Text_decoration_skip_self -> Pp.string ctx "text-decoration-skip-self"
  | Text_decoration_skip_box -> Pp.string ctx "text-decoration-skip-box"
  | Text_decoration_skip_inset -> Pp.string ctx "text-decoration-skip-inset"
  | Text_decoration_skip_spaces -> Pp.string ctx "text-decoration-skip-spaces"
  | Text_emphasis -> Pp.string ctx "text-emphasis"
  | Text_emphasis_style -> Pp.string ctx "text-emphasis-style"
  | Text_emphasis_color -> Pp.string ctx "text-emphasis-color"
  | Text_emphasis_position -> Pp.string ctx "text-emphasis-position"
  | Text_emphasis_skip -> Pp.string ctx "text-emphasis-skip"
  | Text_orientation -> Pp.string ctx "text-orientation"
  | Text_transform -> Pp.string ctx "text-transform"
  | Letter_spacing -> Pp.string ctx "letter-spacing"
  | List_style_type -> Pp.string ctx "list-style-type"
  | List_style_position -> Pp.string ctx "list-style-position"
  | List_style_image -> Pp.string ctx "list-style-image"
  | Display -> Pp.string ctx "display"
  | Position -> Pp.string ctx "position"
  | Visibility -> Pp.string ctx "visibility"
  | Baseline_source -> Pp.string ctx "baseline-source"
  | Alignment_baseline -> Pp.string ctx "alignment-baseline"
  | Baseline_shift -> Pp.string ctx "baseline-shift"
  | Flex_direction -> Pp.string ctx "flex-direction"
  | Flex_wrap -> Pp.string ctx "flex-wrap"
  | Flex_flow -> Pp.string ctx "flex-flow"
  | Flex -> Pp.string ctx "flex"
  | Flex_grow -> Pp.string ctx "flex-grow"
  | Flex_shrink -> Pp.string ctx "flex-shrink"
  | Flex_basis -> Pp.string ctx "flex-basis"
  | Order -> Pp.string ctx "order"
  | Align_items -> Pp.string ctx "align-items"
  | Justify_content -> Pp.string ctx "justify-content"
  | Justify_items -> Pp.string ctx "justify-items"
  | Align_content -> Pp.string ctx "align-content"
  | Align_self -> Pp.string ctx "align-self"
  | Justify_self -> Pp.string ctx "justify-self"
  | Place_content -> Pp.string ctx "place-content"
  | Place_items -> Pp.string ctx "place-items"
  | Place_self -> Pp.string ctx "place-self"
  | Grid_template_columns -> Pp.string ctx "grid-template-columns"
  | Grid_template_rows -> Pp.string ctx "grid-template-rows"
  | Grid_template_areas -> Pp.string ctx "grid-template-areas"
  | Grid_template -> Pp.string ctx "grid-template"
  | Grid -> Pp.string ctx "grid"
  | Grid_area -> Pp.string ctx "grid-area"
  | Grid_auto_flow -> Pp.string ctx "grid-auto-flow"
  | Grid_auto_columns -> Pp.string ctx "grid-auto-columns"
  | Grid_auto_rows -> Pp.string ctx "grid-auto-rows"
  | Grid_column -> Pp.string ctx "grid-column"
  | Grid_row -> Pp.string ctx "grid-row"
  | Grid_column_start -> Pp.string ctx "grid-column-start"
  | Grid_column_end -> Pp.string ctx "grid-column-end"
  | Grid_row_start -> Pp.string ctx "grid-row-start"
  | Grid_row_end -> Pp.string ctx "grid-row-end"
  | Border_width -> Pp.string ctx "border-width"
  | Border_top_width -> Pp.string ctx "border-top-width"
  | Border_right_width -> Pp.string ctx "border-right-width"
  | Border_bottom_width -> Pp.string ctx "border-bottom-width"
  | Border_left_width -> Pp.string ctx "border-left-width"
  | Border_inline_start_width -> Pp.string ctx "border-inline-start-width"
  | Border_inline_end_width -> Pp.string ctx "border-inline-end-width"
  | Border_block_start_width -> Pp.string ctx "border-block-start-width"
  | Border_block_end_width -> Pp.string ctx "border-block-end-width"
  | Border_image -> Pp.string ctx "border-image"
  | Border_radius -> Pp.string ctx "border-radius"
  | Border_top_left_radius -> Pp.string ctx "border-top-left-radius"
  | Border_top_right_radius -> Pp.string ctx "border-top-right-radius"
  | Border_bottom_left_radius -> Pp.string ctx "border-bottom-left-radius"
  | Border_bottom_right_radius -> Pp.string ctx "border-bottom-right-radius"
  | Border_top_color -> Pp.string ctx "border-top-color"
  | Border_right_color -> Pp.string ctx "border-right-color"
  | Border_bottom_color -> Pp.string ctx "border-bottom-color"
  | Border_left_color -> Pp.string ctx "border-left-color"
  | Border_inline_start_color -> Pp.string ctx "border-inline-start-color"
  | Border_inline_end_color -> Pp.string ctx "border-inline-end-color"
  | Border_block_start_color -> Pp.string ctx "border-block-start-color"
  | Border_block_end_color -> Pp.string ctx "border-block-end-color"
  | Border_inline_color -> Pp.string ctx "border-inline-color"
  | Border_block_color -> Pp.string ctx "border-block-color"
  | Border_inline_width -> Pp.string ctx "border-inline-width"
  | Border_block_width -> Pp.string ctx "border-block-width"
  | Border_inline_style -> Pp.string ctx "border-inline-style"
  | Border_block_style -> Pp.string ctx "border-block-style"
  | Border_start_start_radius -> Pp.string ctx "border-start-start-radius"
  | Border_start_end_radius -> Pp.string ctx "border-start-end-radius"
  | Border_end_start_radius -> Pp.string ctx "border-end-start-radius"
  | Border_end_end_radius -> Pp.string ctx "border-end-end-radius"
  | Box_shadow -> Pp.string ctx "box-shadow"
  | Fill -> Pp.string ctx "fill"
  | Stroke -> Pp.string ctx "stroke"
  | Stroke_width -> Pp.string ctx "stroke-width"
  | Fill_rule -> Pp.string ctx "fill-rule"
  | Clip_rule -> Pp.string ctx "clip-rule"
  | Stroke_linecap -> Pp.string ctx "stroke-linecap"
  | Stroke_linejoin -> Pp.string ctx "stroke-linejoin"
  | Stroke_miterlimit -> Pp.string ctx "stroke-miterlimit"
  | Stroke_dashoffset -> Pp.string ctx "stroke-dashoffset"
  | Stroke_dasharray -> Pp.string ctx "stroke-dasharray"
  | Paint_order -> Pp.string ctx "paint-order"
  | Vector_effect -> Pp.string ctx "vector-effect"
  | Stop_color -> Pp.string ctx "stop-color"
  | Flood_color -> Pp.string ctx "flood-color"
  | Lighting_color -> Pp.string ctx "lighting-color"
  | Opacity -> Pp.string ctx "opacity"
  | Fill_opacity -> Pp.string ctx "fill-opacity"
  | Stroke_opacity -> Pp.string ctx "stroke-opacity"
  | Stop_opacity -> Pp.string ctx "stop-opacity"
  | Flood_opacity -> Pp.string ctx "flood-opacity"
  | Mix_blend_mode -> Pp.string ctx "mix-blend-mode"
  | Transition -> Pp.string ctx "transition"
  | Transform -> Pp.string ctx "transform"
  | Translate -> Pp.string ctx "translate"
  | Cursor -> Pp.string ctx "cursor"
  | Interactivity -> Pp.string ctx "interactivity"
  | Caret_animation -> Pp.string ctx "caret-animation"
  | Caret_shape -> Pp.string ctx "caret-shape"
  | Caret -> Pp.string ctx "caret"
  | Interest_delay -> Pp.string ctx "interest-delay"
  | Interest_delay_start -> Pp.string ctx "interest-delay-start"
  | Interest_delay_end -> Pp.string ctx "interest-delay-end"
  | Nav_up -> Pp.string ctx "nav-up"
  | Nav_right -> Pp.string ctx "nav-right"
  | Nav_down -> Pp.string ctx "nav-down"
  | Nav_left -> Pp.string ctx "nav-left"
  | Table_layout -> Pp.string ctx "table-layout"
  | Border_collapse -> Pp.string ctx "border-collapse"
  | Border_spacing -> Pp.string ctx "border-spacing"
  | User_select -> Pp.string ctx "user-select"
  | Pointer_events -> Pp.string ctx "pointer-events"
  | Overflow -> Pp.string ctx "overflow"
  | Inset -> Pp.string ctx "inset"
  | Inset_inline -> Pp.string ctx "inset-inline"
  | Inset_inline_start -> Pp.string ctx "inset-inline-start"
  | Inset_inline_end -> Pp.string ctx "inset-inline-end"
  | Inset_block -> Pp.string ctx "inset-block"
  | Inset_block_start -> Pp.string ctx "inset-block-start"
  | Inset_block_end -> Pp.string ctx "inset-block-end"
  | Top -> Pp.string ctx "top"
  | Right -> Pp.string ctx "right"
  | Bottom -> Pp.string ctx "bottom"
  | Left -> Pp.string ctx "left"
  | Z_index -> Pp.string ctx "z-index"
  | Outline -> Pp.string ctx "outline"
  | Outline_style -> Pp.string ctx "outline-style"
  | Outline_width -> Pp.string ctx "outline-width"
  | Outline_color -> Pp.string ctx "outline-color"
  | Outline_offset -> Pp.string ctx "outline-offset"
  | Forced_color_adjust -> Pp.string ctx "forced-color-adjust"
  | Scroll_snap_type -> Pp.string ctx "scroll-snap-type"
  | Clip -> Pp.string ctx "clip"
  | Clear -> Pp.string ctx "clear"
  | Float -> Pp.string ctx "float"
  | White_space -> Pp.string ctx "white-space"
  | Border -> Pp.string ctx "border"
  | Background -> Pp.string ctx "background"
  | Tab_size -> Pp.string ctx "tab-size"
  | Zoom -> Pp.string ctx "zoom"
  | Webkit_text_size_adjust -> Pp.string ctx "-webkit-text-size-adjust"
  | Font_feature_settings -> Pp.string ctx "font-feature-settings"
  | Font_variation_settings -> Pp.string ctx "font-variation-settings"
  | Webkit_tap_highlight_color -> Pp.string ctx "-webkit-tap-highlight-color"
  | Webkit_text_fill_color -> Pp.string ctx "-webkit-text-fill-color"
  | Webkit_user_select -> Pp.string ctx "-webkit-user-select"
  | Ms_user_select -> Pp.string ctx "-ms-user-select"
  | Moz_user_select -> Pp.string ctx "-moz-user-select"
  | Webkit_text_decoration -> Pp.string ctx "-webkit-text-decoration"
  | Webkit_text_decoration_color ->
      Pp.string ctx "-webkit-text-decoration-color"
  | Text_indent -> Pp.string ctx "text-indent"
  | List_style -> Pp.string ctx "list-style"
  | Font -> Pp.string ctx "font"
  | Source -> Pp.string ctx "src"
  | Webkit_appearance -> Pp.string ctx "-webkit-appearance"
  | Container_type -> Pp.string ctx "container-type"
  | Container_name -> Pp.string ctx "container-name"
  | Container -> Pp.string ctx "container"
  | Anchor_name -> Pp.string ctx "anchor-name"
  | Position_anchor -> Pp.string ctx "position-anchor"
  | Position_try_fallbacks -> Pp.string ctx "position-try-fallbacks"
  | Position_try_order -> Pp.string ctx "position-try-order"
  | Position_try -> Pp.string ctx "position-try"
  | Position_visibility -> Pp.string ctx "position-visibility"
  | Position_area -> Pp.string ctx "position-area"
  | Shape_outside -> Pp.string ctx "shape-outside"
  | Shape_margin -> Pp.string ctx "shape-margin"
  | Shape_image_threshold -> Pp.string ctx "shape-image-threshold"
  | Overflow_clip_margin -> Pp.string ctx "overflow-clip-margin"
  | Overflow_anchor -> Pp.string ctx "overflow-anchor"
  | Scrollbar_width -> Pp.string ctx "scrollbar-width"
  | Scrollbar_color -> Pp.string ctx "scrollbar-color"
  | Scrollbar_gutter -> Pp.string ctx "scrollbar-gutter"
  | Line_height_step -> Pp.string ctx "line-height-step"
  | Font_palette -> Pp.string ctx "font-palette"
  | Font_synthesis -> Pp.string ctx "font-synthesis"
  | Text_wrap_mode -> Pp.string ctx "text-wrap-mode"
  | Text_wrap_style -> Pp.string ctx "text-wrap-style"
  | Text_box_trim -> Pp.string ctx "text-box-trim"
  | Text_underline_position -> Pp.string ctx "text-underline-position"
  | Text_box_edge -> Pp.string ctx "text-box-edge"
  | Text_box -> Pp.string ctx "text-box"
  | Inline_sizing -> Pp.string ctx "inline-sizing"
  | Line_fit_edge -> Pp.string ctx "line-fit-edge"
  | Interpolate_size -> Pp.string ctx "interpolate-size"
  | Min_intrinsic_sizing -> Pp.string ctx "min-intrinsic-sizing"
  | Ruby_align -> Pp.string ctx "ruby-align"
  | Ruby_merge -> Pp.string ctx "ruby-merge"
  | Ruby_overhang -> Pp.string ctx "ruby-overhang"
  | Ruby_position -> Pp.string ctx "ruby-position"
  | Glyph_orientation_vertical -> Pp.string ctx "glyph-orientation-vertical"
  | Animation_timeline -> Pp.string ctx "animation-timeline"
  | Animation_range -> Pp.string ctx "animation-range"
  | Animation_range_start -> Pp.string ctx "animation-range-start"
  | Animation_range_end -> Pp.string ctx "animation-range-end"
  | Scroll_timeline -> Pp.string ctx "scroll-timeline"
  | Scroll_timeline_name -> Pp.string ctx "scroll-timeline-name"
  | Scroll_timeline_axis -> Pp.string ctx "scroll-timeline-axis"
  | View_transition_name -> Pp.string ctx "view-transition-name"
  | View_transition_class -> Pp.string ctx "view-transition-class"
  | Image_orientation -> Pp.string ctx "image-orientation"
  | Image_rendering -> Pp.string ctx "image-rendering"
  | Image_resolution -> Pp.string ctx "image-resolution"
  | Contain_intrinsic_size -> Pp.string ctx "contain-intrinsic-size"
  | Contain_intrinsic_width -> Pp.string ctx "contain-intrinsic-width"
  | Contain_intrinsic_height -> Pp.string ctx "contain-intrinsic-height"
  | Contain_intrinsic_block_size -> Pp.string ctx "contain-intrinsic-block-size"
  | Contain_intrinsic_inline_size ->
      Pp.string ctx "contain-intrinsic-inline-size"
  | Margin_trim -> Pp.string ctx "margin-trim"
  | Offset_path -> Pp.string ctx "offset-path"
  | Offset_distance -> Pp.string ctx "offset-distance"
  | Offset_rotate -> Pp.string ctx "offset-rotate"
  | Font_size_adjust -> Pp.string ctx "font-size-adjust"
  | Font_variant_emoji -> Pp.string ctx "font-variant-emoji"
  | Text_spacing_trim -> Pp.string ctx "text-spacing-trim"
  | Hyphenate_limit_chars -> Pp.string ctx "hyphenate-limit-chars"
  | Initial_letter -> Pp.string ctx "initial-letter"
  | Initial_letter_align -> Pp.string ctx "initial-letter-align"
  | Initial_letter_wrap -> Pp.string ctx "initial-letter-wrap"
  | Dominant_baseline -> Pp.string ctx "dominant-baseline"
  | View_timeline_name -> Pp.string ctx "view-timeline-name"
  | View_timeline_axis -> Pp.string ctx "view-timeline-axis"
  | View_timeline_inset -> Pp.string ctx "view-timeline-inset"
  | View_timeline -> Pp.string ctx "view-timeline"
  | Timeline_scope -> Pp.string ctx "timeline-scope"
  | Perspective -> Pp.string ctx "perspective"
  | Perspective_origin -> Pp.string ctx "perspective-origin"
  | Transform_style -> Pp.string ctx "transform-style"
  | Backface_visibility -> Pp.string ctx "backface-visibility"
  | Object_position -> Pp.string ctx "object-position"
  | Rotate -> Pp.string ctx "rotate"
  | Scale -> Pp.string ctx "scale"
  | Transition_duration -> Pp.string ctx "transition-duration"
  | Transition_timing_function -> Pp.string ctx "transition-timing-function"
  | Transition_delay -> Pp.string ctx "transition-delay"
  | Transition_property -> Pp.string ctx "transition-property"
  | Transition_behavior -> Pp.string ctx "transition-behavior"
  | Overlay -> Pp.string ctx "overlay"
  | Will_change -> Pp.string ctx "will-change"
  | Contain -> Pp.string ctx "contain"
  | Isolation -> Pp.string ctx "isolation"
  | Break_before -> Pp.string ctx "break-before"
  | Break_after -> Pp.string ctx "break-after"
  | Break_inside -> Pp.string ctx "break-inside"
  | Page_break_before ->
      Pp.string ctx
        (if Pp.minified ctx then "break-before" else "page-break-before")
  | Page_break_after ->
      Pp.string ctx
        (if Pp.minified ctx then "break-after" else "page-break-after")
  | Page_break_inside ->
      Pp.string ctx
        (if Pp.minified ctx then "break-inside" else "page-break-inside")
  | Page_size -> Pp.string ctx "size"
  | Columns -> Pp.string ctx "columns"
  | Column_width -> Pp.string ctx "column-width"
  | Column_count -> Pp.string ctx "column-count"
  | Column_rule -> Pp.string ctx "column-rule"
  | Column_span -> Pp.string ctx "column-span"
  | Word_spacing -> Pp.string ctx "word-spacing"
  | Background_attachment -> Pp.string ctx "background-attachment"
  | Border_top -> Pp.string ctx "border-top"
  | Border_right -> Pp.string ctx "border-right"
  | Border_bottom -> Pp.string ctx "border-bottom"
  | Border_left -> Pp.string ctx "border-left"
  | Border_block -> Pp.string ctx "border-block"
  | Border_block_start -> Pp.string ctx "border-block-start"
  | Border_block_end -> Pp.string ctx "border-block-end"
  | Border_inline -> Pp.string ctx "border-inline"
  | Border_inline_start -> Pp.string ctx "border-inline-start"
  | Border_inline_end -> Pp.string ctx "border-inline-end"
  | Transform_origin -> Pp.string ctx "transform-origin"
  | Transform_box -> Pp.string ctx "transform-box"
  | Text_shadow -> Pp.string ctx "text-shadow"
  | Clip_path -> Pp.string ctx "clip-path"
  | Mask -> Pp.string ctx "mask"
  | Mask_border -> Pp.string ctx "mask-border"
  | Content_visibility -> Pp.string ctx "content-visibility"
  | Filter -> Pp.string ctx "filter"
  | Background_image -> Pp.string ctx "background-image"
  | Background_origin -> Pp.string ctx "background-origin"
  | Background_clip -> Pp.string ctx "background-clip"
  | Webkit_background_clip -> Pp.string ctx "-webkit-background-clip"
  | Animation -> Pp.string ctx "animation"
  | Aspect_ratio -> Pp.string ctx "aspect-ratio"
  | Overflow_x -> Pp.string ctx "overflow-x"
  | Overflow_y -> Pp.string ctx "overflow-y"
  | Overflow_block -> Pp.string ctx "overflow-block"
  | Overflow_inline -> Pp.string ctx "overflow-inline"
  | Vertical_align -> Pp.string ctx "vertical-align"
  | Font_family -> Pp.string ctx "font-family"
  | Background_position -> Pp.string ctx "background-position"
  | Background_repeat -> Pp.string ctx "background-repeat"
  | Background_size -> Pp.string ctx "background-size"
  | Webkit_font_smoothing -> Pp.string ctx "-webkit-font-smoothing"
  | Moz_osx_font_smoothing -> Pp.string ctx "-moz-osx-font-smoothing"
  | Webkit_line_clamp -> Pp.string ctx "-webkit-line-clamp"
  | Webkit_box_orient -> Pp.string ctx "-webkit-box-orient"
  | Moz_orient -> Pp.string ctx "-moz-orient"
  | Text_overflow -> Pp.string ctx "text-overflow"
  | Text_wrap -> Pp.string ctx "text-wrap"
  | Word_break -> Pp.string ctx "word-break"
  | Overflow_wrap -> Pp.string ctx "overflow-wrap"
  | Line_break -> Pp.string ctx "line-break"
  | Hyphens -> Pp.string ctx "hyphens"
  | Webkit_hyphens -> Pp.string ctx "-webkit-hyphens"
  | Font_stretch -> Pp.string ctx "font-stretch"
  | Font_optical_sizing -> Pp.string ctx "font-optical-sizing"
  | Font_kerning -> Pp.string ctx "font-kerning"
  | Font_language_override -> Pp.string ctx "font-language-override"
  | Font_synthesis_style -> Pp.string ctx "font-synthesis-style"
  | Font_synthesis_weight -> Pp.string ctx "font-synthesis-weight"
  | Font_synthesis_small_caps -> Pp.string ctx "font-synthesis-small-caps"
  | Font_synthesis_position -> Pp.string ctx "font-synthesis-position"
  | Font_variant_ligatures -> Pp.string ctx "font-variant-ligatures"
  | Caps -> Pp.string ctx "font-variant-caps"
  | Numeric -> Pp.string ctx "font-variant-numeric"
  | Font_variant_position -> Pp.string ctx "font-variant-position"
  | East_asian -> Pp.string ctx "font-variant-east-asian"
  | Backdrop_filter -> Pp.string ctx "backdrop-filter"
  | Webkit_backdrop_filter -> Pp.string ctx "-webkit-backdrop-filter"
  | Webkit_mask_image -> Pp.string ctx "-webkit-mask-image"
  | Webkit_mask_composite -> Pp.string ctx "-webkit-mask-composite"
  | Webkit_mask_source_type -> Pp.string ctx "-webkit-mask-source-type"
  | Webkit_mask_size -> Pp.string ctx "-webkit-mask-size"
  | Webkit_mask_position -> Pp.string ctx "-webkit-mask-position"
  | Webkit_mask_repeat -> Pp.string ctx "-webkit-mask-repeat"
  | Webkit_mask_clip -> Pp.string ctx "-webkit-mask-clip"
  | Webkit_mask_origin -> Pp.string ctx "-webkit-mask-origin"
  | Border_image_source -> Pp.string ctx "border-image-source"
  | Border_image_slice -> Pp.string ctx "border-image-slice"
  | Border_image_repeat -> Pp.string ctx "border-image-repeat"
  | Border_image_width -> Pp.string ctx "border-image-width"
  | Border_image_outset -> Pp.string ctx "border-image-outset"
  | Mask_image -> Pp.string ctx "mask-image"
  | Mask_composite -> Pp.string ctx "mask-composite"
  | Mask_mode -> Pp.string ctx "mask-mode"
  | Mask_size -> Pp.string ctx "mask-size"
  | Mask_position -> Pp.string ctx "mask-position"
  | Mask_repeat -> Pp.string ctx "mask-repeat"
  | Mask_clip -> Pp.string ctx "mask-clip"
  | Mask_origin -> Pp.string ctx "mask-origin"
  | Mask_type -> Pp.string ctx "mask-type"
  | Scroll_snap_align -> Pp.string ctx "scroll-snap-align"
  | Scroll_snap_stop -> Pp.string ctx "scroll-snap-stop"
  | Scroll_behavior -> Pp.string ctx "scroll-behavior"
  | Box_sizing -> Pp.string ctx "box-sizing"
  | Webkit_box_sizing -> Pp.string ctx "-webkit-box-sizing"
  | Moz_box_sizing -> Pp.string ctx "-moz-box-sizing"
  | Field_sizing -> Pp.string ctx "field-sizing"
  | Caption_side -> Pp.string ctx "caption-side"
  | Resize -> Pp.string ctx "resize"
  | Object_fit -> Pp.string ctx "object-fit"
  | Object_view_box -> Pp.string ctx "object-view-box"
  | Appearance -> Pp.string ctx "appearance"
  | Color_scheme -> Pp.string ctx "color-scheme"
  | Print_color_adjust -> Pp.string ctx "print-color-adjust"
  | Webkit_print_color_adjust -> Pp.string ctx "-webkit-print-color-adjust"
  | Box_decoration_break -> Pp.string ctx "box-decoration-break"
  | Webkit_box_decoration_break -> Pp.string ctx "-webkit-box-decoration-break"
  | Content -> Pp.string ctx "content"
  | Counter_reset -> Pp.string ctx "counter-reset"
  | Counter_increment -> Pp.string ctx "counter-increment"
  | Quotes -> Pp.string ctx "quotes"
  | Text_size_adjust -> Pp.string ctx "text-size-adjust"
  | Touch_action -> Pp.string ctx "touch-action"
  | Direction -> Pp.string ctx "direction"
  | Unicode_bidi -> Pp.string ctx "unicode-bidi"
  | Writing_mode -> Pp.string ctx "writing-mode"
  | Text_combine_upright -> Pp.string ctx "text-combine-upright"
  | Text_decoration_skip_ink -> Pp.string ctx "text-decoration-skip-ink"
  | Animation_name -> Pp.string ctx "animation-name"
  | Animation_duration -> Pp.string ctx "animation-duration"
  | Animation_timing_function -> Pp.string ctx "animation-timing-function"
  | Animation_delay -> Pp.string ctx "animation-delay"
  | Animation_iteration_count -> Pp.string ctx "animation-iteration-count"
  | Animation_direction -> Pp.string ctx "animation-direction"
  | Animation_fill_mode -> Pp.string ctx "animation-fill-mode"
  | Animation_play_state -> Pp.string ctx "animation-play-state"
  | Animation_composition -> Pp.string ctx "animation-composition"
  | Background_blend_mode -> Pp.string ctx "background-blend-mode"
  | Scroll_margin -> Pp.string ctx "scroll-margin"
  | Scroll_margin_top -> Pp.string ctx "scroll-margin-top"
  | Scroll_margin_right -> Pp.string ctx "scroll-margin-right"
  | Scroll_margin_bottom -> Pp.string ctx "scroll-margin-bottom"
  | Scroll_margin_left -> Pp.string ctx "scroll-margin-left"
  | Scroll_margin_inline -> Pp.string ctx "scroll-margin-inline"
  | Scroll_margin_inline_start -> Pp.string ctx "scroll-margin-inline-start"
  | Scroll_margin_inline_end -> Pp.string ctx "scroll-margin-inline-end"
  | Scroll_margin_block -> Pp.string ctx "scroll-margin-block"
  | Scroll_margin_block_start -> Pp.string ctx "scroll-margin-block-start"
  | Scroll_margin_block_end -> Pp.string ctx "scroll-margin-block-end"
  | Scroll_padding -> Pp.string ctx "scroll-padding"
  | Scroll_padding_top -> Pp.string ctx "scroll-padding-top"
  | Scroll_padding_right -> Pp.string ctx "scroll-padding-right"
  | Scroll_padding_bottom -> Pp.string ctx "scroll-padding-bottom"
  | Scroll_padding_left -> Pp.string ctx "scroll-padding-left"
  | Scroll_padding_inline -> Pp.string ctx "scroll-padding-inline"
  | Scroll_padding_inline_start -> Pp.string ctx "scroll-padding-inline-start"
  | Scroll_padding_inline_end -> Pp.string ctx "scroll-padding-inline-end"
  | Scroll_padding_block -> Pp.string ctx "scroll-padding-block"
  | Scroll_padding_block_start -> Pp.string ctx "scroll-padding-block-start"
  | Scroll_padding_block_end -> Pp.string ctx "scroll-padding-block-end"
  | Overscroll_behavior -> Pp.string ctx "overscroll-behavior"
  | Overscroll_behavior_x -> Pp.string ctx "overscroll-behavior-x"
  | Overscroll_behavior_y -> Pp.string ctx "overscroll-behavior-y"
  | Overscroll_behavior_block -> Pp.string ctx "overscroll-behavior-block"
  | Overscroll_behavior_inline -> Pp.string ctx "overscroll-behavior-inline"
  | Accent_color -> Pp.string ctx "accent-color"
  | Caret_color -> Pp.string ctx "caret-color"
  | Webkit_transform -> Pp.string ctx "-webkit-transform"
  | Moz_transform -> Pp.string ctx "-moz-transform"
  | Ms_transform -> Pp.string ctx "-ms-transform"
  | O_transform -> Pp.string ctx "-o-transform"
  | Webkit_transition -> Pp.string ctx "-webkit-transition"
  | Webkit_transition_delay -> Pp.string ctx "-webkit-transition-delay"
  | Webkit_transition_duration -> Pp.string ctx "-webkit-transition-duration"
  | Webkit_transition_property -> Pp.string ctx "-webkit-transition-property"
  | Webkit_transition_timing_function ->
      Pp.string ctx "-webkit-transition-timing-function"
  | Webkit_animation -> Pp.string ctx "-webkit-animation"
  | Webkit_animation_delay -> Pp.string ctx "-webkit-animation-delay"
  | Webkit_animation_duration -> Pp.string ctx "-webkit-animation-duration"
  | Webkit_animation_direction -> Pp.string ctx "-webkit-animation-direction"
  | Webkit_animation_iteration_count ->
      Pp.string ctx "-webkit-animation-iteration-count"
  | Webkit_animation_name -> Pp.string ctx "-webkit-animation-name"
  | Webkit_animation_timing_function ->
      Pp.string ctx "-webkit-animation-timing-function"
  | Webkit_animation_fill_mode -> Pp.string ctx "-webkit-animation-fill-mode"
  | Webkit_animation_play_state -> Pp.string ctx "-webkit-animation-play-state"
  | Webkit_flex_direction -> Pp.string ctx "-webkit-flex-direction"
  | Webkit_flex_wrap -> Pp.string ctx "-webkit-flex-wrap"
  | Webkit_flex_flow -> Pp.string ctx "-webkit-flex-flow"
  | Webkit_justify_content -> Pp.string ctx "-webkit-justify-content"
  | Webkit_align_items -> Pp.string ctx "-webkit-align-items"
  | Webkit_align_content -> Pp.string ctx "-webkit-align-content"
  | Webkit_align_self -> Pp.string ctx "-webkit-align-self"
  | Webkit_border_radius -> Pp.string ctx "-webkit-border-radius"
  | Webkit_box_shadow -> Pp.string ctx "-webkit-box-shadow"
  | Webkit_background_size -> Pp.string ctx "-webkit-background-size"
  | Webkit_filter -> Pp.string ctx "-webkit-filter"
  | Moz_appearance -> Pp.string ctx "-moz-appearance"
  | Moz_animation -> Pp.string ctx "-moz-animation"
  | Moz_animation_delay -> Pp.string ctx "-moz-animation-delay"
  | Moz_animation_duration -> Pp.string ctx "-moz-animation-duration"
  | Moz_animation_direction -> Pp.string ctx "-moz-animation-direction"
  | Moz_animation_iteration_count ->
      Pp.string ctx "-moz-animation-iteration-count"
  | Moz_animation_name -> Pp.string ctx "-moz-animation-name"
  | Moz_animation_timing_function ->
      Pp.string ctx "-moz-animation-timing-function"
  | Moz_animation_fill_mode -> Pp.string ctx "-moz-animation-fill-mode"
  | Moz_animation_play_state -> Pp.string ctx "-moz-animation-play-state"
  | Moz_transition -> Pp.string ctx "-moz-transition"
  | Moz_transition_delay -> Pp.string ctx "-moz-transition-delay"
  | Moz_transition_duration -> Pp.string ctx "-moz-transition-duration"
  | Moz_transition_property -> Pp.string ctx "-moz-transition-property"
  | Moz_transition_timing_function ->
      Pp.string ctx "-moz-transition-timing-function"
  | Moz_border_radius -> Pp.string ctx "-moz-border-radius"
  | Moz_box_shadow -> Pp.string ctx "-moz-box-shadow"
  | Ms_filter -> Pp.string ctx "-ms-filter"
  | O_transition -> Pp.string ctx "-o-transition"

let rec pp_font_feature_settings : font_feature_settings Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Feature_list s ->
      (* Feature list contains quoted tags already in the stored string *)
      Pp.string ctx s
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | String s -> Pp.quoted_string ctx s
  | Var v -> pp_var pp_font_feature_settings ctx v

(* Collapse the optional whitespace after each axis-separator comma in a
   [font-variation-settings] list. Commas inside a quoted axis tag (a 4-char tag
   may contain U+2C) are left untouched. *)
let minify_axis_list s =
  let buf = Buffer.create (String.length s) in
  let n = String.length s in
  let in_quote = ref false in
  let i = ref 0 in
  while !i < n do
    let c = s.[!i] in
    if !in_quote then (
      Buffer.add_char buf c;
      if c = '"' then in_quote := false;
      incr i)
    else if c = '"' then (
      Buffer.add_char buf c;
      in_quote := true;
      incr i)
    else if c = ',' then (
      Buffer.add_char buf ',';
      incr i;
      while !i < n && s.[!i] = ' ' do
        incr i
      done)
    else (
      Buffer.add_char buf c;
      incr i)
  done;
  Buffer.contents buf

let rec pp_font_variation_settings : font_variation_settings Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Axis_list s ->
      Pp.string ctx (if Pp.minified ctx then minify_axis_list s else s)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | String s -> Pp.quoted_string ctx s
  | Var v -> pp_var pp_font_variation_settings ctx v

let rec pp_text_shadow : text_shadow Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_shadow ctx v
  | Text_shadow { h_offset; v_offset; blur; color } -> (
      pp_length ctx h_offset;
      Pp.space ctx ();
      pp_length ctx v_offset;
      (match blur with
      | Some b when not (Pp.minified ctx && is_zero_length b) ->
          Pp.space ctx ();
          pp_length ctx b
      | Some _ | None -> ());
      match color with Some c -> pp_color_after_length ctx c | None -> ())

let rec pp_animation_direction : animation_direction Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_animation_direction ctx v
  | Directions directions ->
      Pp.list ~sep:Pp.comma pp_animation_direction ctx directions
  | Normal -> Pp.string ctx "normal"
  | Reverse -> Pp.string ctx "reverse"
  | Alternate -> Pp.string ctx "alternate"
  | Alternate_reverse -> Pp.string ctx "alternate-reverse"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_animation_fill_mode : animation_fill_mode Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_animation_fill_mode ctx v
  | Fill_modes modes -> Pp.list ~sep:Pp.comma pp_animation_fill_mode ctx modes
  | None -> Pp.string ctx "none"
  | Forwards -> Pp.string ctx "forwards"
  | Backwards -> Pp.string ctx "backwards"
  | Both -> Pp.string ctx "both"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_animation_iteration_count : animation_iteration_count Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_animation_iteration_count ctx v
  | Counts counts ->
      Pp.list ~sep:Pp.comma pp_animation_iteration_count ctx counts
  | Infinite -> Pp.string ctx "infinite"
  | Num n -> Pp.float ctx n
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_animation_name : animation_name Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Name name -> Pp.string ctx name
  | Ambiguous name -> Pp.string ctx name
  | Quoted name ->
      (* CSS Animations 1 sec. 3.3: [<keyframes-name>] excludes [none], the
         CSS-wide keywords, and [default]. A source [animation-name: "none"]
         therefore can't refer to a real [@keyframes none] - it's invalid input
         that browsers tolerate. Minified output drops the quotes so the value
         collapses to the equivalent (and shorter) keyword form. *)
      if Pp.minified ctx then Pp.string ctx name else Pp.quoted_string ctx name
  | Names names -> Pp.list ~sep:Pp.comma pp_animation_name ctx names
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_animation_name ctx v

let rec pp_animation_play_state : animation_play_state Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_animation_play_state ctx v
  | States states -> Pp.list ~sep:Pp.comma pp_animation_play_state ctx states
  | Running -> Pp.string ctx "running"
  | Paused -> Pp.string ctx "paused"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_animation_composition_item ctx = function
  | Replace -> Pp.string ctx "replace"
  | Add -> Pp.string ctx "add"
  | Accumulate -> Pp.string ctx "accumulate"

let rec pp_animation_composition : animation_composition Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_animation_composition ctx v
  | Compositions items ->
      Pp.list ~sep:Pp.comma pp_animation_composition_item ctx items
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_steps_direction : steps_direction Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_steps_direction ctx v
  | Jump_start -> Pp.string ctx "jump-start"
  | Jump_end -> Pp.string ctx "jump-end"
  | Jump_none -> Pp.string ctx "jump-none"
  | Jump_both -> Pp.string ctx "jump-both"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"

let rec pp_box_sizing : box_sizing Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_box_sizing ctx v
  | Border_box -> Pp.string ctx "border-box"
  | Content_box -> Pp.string ctx "content-box"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_caption_side : caption_side Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_caption_side ctx v
  | Top -> Pp.string ctx "top"
  | Bottom -> Pp.string ctx "bottom"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_clear : clear Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_clear ctx v
  | None -> Pp.string ctx "none"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Both -> Pp.string ctx "both"
  | Inline_start -> Pp.string ctx "inline-start"
  | Inline_end -> Pp.string ctx "inline-end"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_overflow_anchor : overflow_anchor Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_overflow_anchor ctx v
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_font_palette : font_palette Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_font_palette ctx v
  | Normal -> Pp.string ctx "normal"
  | Light -> Pp.string ctx "light"
  | Dark -> Pp.string ctx "dark"
  | Palette name -> Pp.string ctx name
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_font_synthesis_feature : font_synthesis_feature Pp.t =
 fun ctx -> function
  | Weight -> Pp.string ctx "weight"
  | Style -> Pp.string ctx "style"
  | Small_caps -> Pp.string ctx "small-caps"
  | Position -> Pp.string ctx "position"

let rec pp_font_synthesis : font_synthesis Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_font_synthesis ctx v
  | None -> Pp.string ctx "none"
  | Features features ->
      Pp.list ~sep:Pp.space pp_font_synthesis_feature ctx features
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let split_ws_words s =
  s |> String.split_on_char ' '
  |> List.filter (fun word -> String.length word > 0)

let canonical_scroll_timeline_args args =
  let words = split_ws_words args in
  let axes = [ "block"; "inline" ] in
  let scrollers = [ "nearest"; "root"; "self" ] in
  if
    List.for_all
      (fun word -> List.mem word axes || List.mem word scrollers)
      words
    && List.length words <= 2
  then
    let axis =
      Option.value
        (List.find_opt (fun word -> List.mem word axes) words)
        ~default:"block"
    in
    let scroller =
      Option.value
        (List.find_opt (fun word -> List.mem word scrollers) words)
        ~default:"nearest"
    in
    match (scroller, axis) with
    | "nearest", "block" -> ""
    | "nearest", axis -> axis
    | scroller, "block" -> scroller
    | scroller, axis -> scroller ^ " " ^ axis
  else args

let canonical_view_timeline_args args =
  match split_ws_words args with
  | [] -> ""
  | words ->
      let axis, insets =
        match words with
        | (("block" | "inline") as axis) :: rest -> (axis, rest)
        | words -> ("block", words)
      in
      let insets =
        match insets with
        | [] | [ "auto" ] | [ "auto"; "auto" ] -> []
        | [ first; second ] when first = second -> [ first ]
        | insets -> insets
      in
      String.concat " " ((if axis = "block" then [] else [ axis ]) @ insets)

let rec pp_animation_timeline : animation_timeline Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_animation_timeline ctx v
  | None -> Pp.string ctx "none"
  | Auto -> Pp.string ctx "auto"
  | Name name -> Pp.string ctx name
  | Scroll args ->
      Pp.string ctx "scroll(";
      Pp.string ctx (canonical_scroll_timeline_args args);
      Pp.char ctx ')'
  | View args ->
      Pp.string ctx "view(";
      Pp.string ctx (canonical_view_timeline_args args);
      Pp.char ctx ')'
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_animation_range_name : animation_range_name Pp.t =
 fun ctx -> function
  | Cover -> Pp.string ctx "cover"
  | Contain -> Pp.string ctx "contain"
  | Entry -> Pp.string ctx "entry"
  | Exit -> Pp.string ctx "exit"
  | Entry_crossing -> Pp.string ctx "entry-crossing"
  | Exit_crossing -> Pp.string ctx "exit-crossing"

let rec pp_animation_range_item : animation_range_item Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Offset lp -> pp_length_percentage ~always:true ctx lp
  | Named (name, None) -> pp_animation_range_name ctx name
  | Named (name, Some lp) ->
      pp_animation_range_name ctx name;
      Pp.space ctx ();
      pp_length_percentage ~always:true ctx lp
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_animation_range_item ctx v

let animation_range_boundary_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true
  | _ -> false

let animation_range_needs_space ctx =
  if not (Pp.minified ctx) then true
  else
    match Pp.last_char ctx with
    | None -> true
    | Some c -> animation_range_boundary_char c

let rec pp_animation_range : animation_range Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_animation_range ctx v
  | Range (first, None) -> pp_animation_range_item ctx first
  | Range (first, Some Normal) -> pp_animation_range_item ctx first
  | Range
      ( Named (start_name, Some start_offset),
        Some (Named (end_name, Some end_offset)) )
    when start_name = end_name
         && start_offset = (Pct 0. : length_percentage)
         && end_offset = (Pct 100. : length_percentage) ->
      pp_animation_range_item ctx (Named (start_name, Some start_offset))
  | Range (first, Some second) ->
      pp_animation_range_item ctx first;
      if animation_range_needs_space ctx then Pp.space ctx ();
      pp_animation_range_item ctx second
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_view_transition_name : view_transition_name Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_view_transition_name ctx v
  | None -> Pp.string ctx "none"
  | Match_element -> Pp.string ctx "match-element"
  | Name name -> Pp.string ctx name
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_view_transition_class : view_transition_class Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_view_transition_class ctx v
  | None -> Pp.string ctx "none"
  | Classes classes -> Pp.list ~sep:Pp.space Pp.string ctx classes
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_content : content Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Normal -> Pp.string ctx "normal"
  | String s -> Pp.quoted_string ctx s
  | Quoted { value; quote; repr } -> (
      if Pp.minified ctx then Pp.quoted_string ctx value
      else
        match repr with
        | Some repr -> Pp.string ctx repr
        | None ->
            Pp.char ctx quote;
            Pp.string ctx value;
            Pp.char ctx quote)
  | Open_quote -> Pp.string ctx "open-quote"
  | Close_quote -> Pp.string ctx "close-quote"
  | Attr attr -> Pp.call "attr" (Values.pp_attr_call pp_content) ctx attr
  | Counter name -> Pp.call "counter" Pp.string ctx name
  | String_ref name -> Pp.call "string" Pp.string ctx name
  | Counters (name, separator) ->
      Pp.string ctx "counters(";
      Pp.string ctx name;
      Pp.char ctx ',';
      Pp.space ctx ();
      Pp.quoted_string ctx separator;
      Pp.char ctx ')'
  | Content_list items -> Pp.list ~sep:Pp.space pp_content ctx items
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_content ctx v

let pp_counter_item ctx { name; value } =
  Pp.string ctx name;
  match value with
  | None -> ()
  | Some n ->
      Pp.space ctx ();
      Pp.int ctx n

let rec pp_counter_set : counter_set Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Counters items -> Pp.list ~sep:Pp.space pp_counter_item ctx items
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_counter_set ctx v

let rec pp_quotes : quotes Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Pairs pairs ->
      List.iter
        (fun (open_q, close_q) ->
          Pp.char ctx '"';
          Pp.string ctx open_q;
          Pp.char ctx '"';
          Pp.char ctx '"';
          Pp.string ctx close_q;
          Pp.char ctx '"')
        pairs
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_quotes ctx v

let rec pp_timeline_axis : timeline_axis Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_timeline_axis ctx v
  | Block -> Pp.string ctx "block"
  | Inline -> Pp.string ctx "inline"
  | X -> Pp.string ctx "x"
  | Y -> Pp.string ctx "y"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_timeline_name : timeline_name Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_timeline_name ctx v
  | None -> Pp.string ctx "none"
  | Names names -> Pp.list ~sep:Pp.comma Pp.string ctx names
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_timeline_shorthand_item : timeline_shorthand_item Pp.t =
 fun ctx { name; axis } ->
  Pp.string ctx name;
  Pp.space ctx ();
  pp_timeline_axis ctx axis

let rec pp_timeline_shorthand : timeline_shorthand Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Timelines items ->
      Pp.list ~sep:Pp.comma pp_timeline_shorthand_item ctx items
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_timeline_shorthand ctx v

let pp_timeline_inset_item : timeline_inset_item Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Length lp -> pp_length_percentage ~always:true ctx lp

let rec pp_timeline_inset : timeline_inset Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_timeline_inset ctx v
  | Inset (first, second) ->
      pp_timeline_inset_item ctx first;
      Option.iter
        (fun item ->
          Pp.space ctx ();
          pp_timeline_inset_item ctx item)
        second
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_object_fit : object_fit Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_object_fit ctx v
  | Fill -> Pp.string ctx "fill"
  | Contain -> Pp.string ctx "contain"
  | Cover -> Pp.string ctx "cover"
  | None -> Pp.string ctx "none"
  | Scale_down -> Pp.string ctx "scale-down"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_object_view_box : object_view_box Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_object_view_box ctx v
  | None -> Pp.string ctx "none"
  | Inset (top, right, bottom, left) ->
      Pp.string ctx "inset(";
      pp_length ctx top;
      pp_optional_inset_sides pp_length ctx right bottom left;
      Pp.char ctx ')'
  | Xywh { x; y; width; height; rounded } ->
      Pp.string ctx "xywh(";
      pp_clip_path_inset_quad ctx x y width height;
      pp_clip_path_round ctx rounded;
      Pp.char ctx ')'
  | Rect { top; right; bottom; left; rounded } ->
      Pp.string ctx "rect(";
      pp_clip_path_inset_quad ctx top right bottom left;
      pp_clip_path_round ctx rounded;
      Pp.char ctx ')'
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

(* CSS Fonts 4 sec. 5.3 defines each keyword as a percentage, and the percentage
   is never longer, so minified output uses it. *)
let font_stretch_pct = function
  | Ultra_condensed -> Some 50.
  | Extra_condensed -> Some 62.5
  | Condensed -> Some 75.
  | Semi_condensed -> Some 87.5
  | Normal -> Some 100.
  | Semi_expanded -> Some 112.5
  | Expanded -> Some 125.
  | Extra_expanded -> Some 150.
  | Ultra_expanded -> Some 200.
  | _ -> None

let rec pp_font_stretch : font_stretch Pp.t =
 fun ctx v ->
  match if Pp.minified ctx then font_stretch_pct v else None with
  | Some pct -> Pp.pct ctx pct
  | None -> pp_font_stretch_keyword ctx v

and pp_font_stretch_keyword : font_stretch Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_font_stretch ctx v
  | Pct f -> Pp.pct ctx f
  | Ultra_condensed -> Pp.string ctx "ultra-condensed"
  | Extra_condensed -> Pp.string ctx "extra-condensed"
  | Condensed -> Pp.string ctx "condensed"
  | Semi_condensed -> Pp.string ctx "semi-condensed"
  | Normal -> Pp.string ctx "normal"
  | Semi_expanded -> Pp.string ctx "semi-expanded"
  | Expanded -> Pp.string ctx "expanded"
  | Extra_expanded -> Pp.string ctx "extra-expanded"
  | Ultra_expanded -> Pp.string ctx "ultra-expanded"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_font_size_adjust_metric : font_size_adjust_metric Pp.t =
 fun ctx -> function
  | Ex_height -> Pp.string ctx "ex-height"
  | Cap_height -> Pp.string ctx "cap-height"
  | Ch_width -> Pp.string ctx "ch-width"
  | Ic_width -> Pp.string ctx "ic-width"
  | Ic_height -> Pp.string ctx "ic-height"

let rec pp_font_size_adjust : font_size_adjust Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Number f -> Pp.float ctx f
  | From_font -> Pp.string ctx "from-font"
  | Metric_number (metric, f) ->
      pp_font_size_adjust_metric ctx metric;
      Pp.space ctx ();
      Pp.float ctx f
  | Metric_from_font metric ->
      pp_font_size_adjust_metric ctx metric;
      Pp.space ctx ();
      Pp.string ctx "from-font"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_size_adjust ctx v

let rec pp_font_variant_emoji : font_variant_emoji Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Text -> Pp.string ctx "text"
  | Emoji -> Pp.string ctx "emoji"
  | Unicode -> Pp.string ctx "unicode"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_variant_emoji ctx v

let rec pp_font_display : font_display Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_font_display ctx v
  | Auto -> Pp.string ctx "auto"
  | Block -> Pp.string ctx "block"
  | Swap -> Pp.string ctx "swap"
  | Fallback -> Pp.string ctx "fallback"
  | Optional -> Pp.string ctx "optional"

let pp_unicode_range_range ctx start end_ =
  Pp.string ctx "U+";
  Pp.hex ctx start;
  Pp.char ctx '-';
  Pp.hex ctx end_

let unicode_range_wildcard start end_ : string option =
  let wildcard_for q : string option =
    let size = 1 lsl (4 * q) in
    if start mod size <> 0 || end_ <> start + size - 1 then None
    else
      let prefix = start / size in
      let prefix = if prefix = 0 then "" else hex_string prefix in
      let wildcard = "U+" ^ prefix ^ String.make q '?' in
      let range = "U+" ^ hex_string start ^ "-" ^ hex_string end_ in
      if String.length wildcard < String.length range then
        (Some wildcard : string option)
      else (None : string option)
  in
  let rec loop q =
    if q > 6 then (None : string option)
    else
      match wildcard_for q with
      | (Some _ : string option) as wildcard -> wildcard
      | None -> loop (q + 1)
  in
  loop 1

let rec pp_unicode_range : unicode_range Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_unicode_range ctx v
  | Single hex ->
      Pp.string ctx "U+";
      Pp.hex ctx hex
  | Range (start, end_) -> (
      let pp_range () = pp_unicode_range_range ctx start end_ in
      if not (Pp.minified ctx) then pp_range ()
      else
        match unicode_range_wildcard start end_ with
        | Some wildcard -> Pp.string ctx wildcard
        | None -> pp_range ())
  | Padded_single (value, width) ->
      if Pp.minified ctx then pp_unicode_range ctx (Single value)
      else (
        Pp.string ctx "U+";
        Pp.string ctx (padded_hex width value))
  | Padded_range { start; end_; start_width; end_width } ->
      if Pp.minified ctx then pp_unicode_range ctx (Range (start, end_))
      else (
        Pp.string ctx "U+";
        Pp.string ctx (padded_hex start_width start);
        Pp.char ctx '-';
        Pp.string ctx (padded_hex end_width end_))
  | Wildcard { prefix; prefix_width; wildcards } ->
      let start = prefix lsl (4 * wildcards) in
      let end_ = start + (1 lsl (4 * wildcards)) - 1 in
      if Pp.minified ctx then
        match unicode_range_wildcard start end_ with
        | Some wildcard -> Pp.string ctx wildcard
        | None -> pp_unicode_range_range ctx start end_
      else (
        Pp.string ctx "U+";
        if prefix_width > 0 then Pp.string ctx (padded_hex prefix_width prefix);
        Pp.string ctx (String.make wildcards '?'))

let rec pp_font_variant_numeric_token : font_variant_numeric_token Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Lining_nums -> Pp.string ctx "lining-nums"
  | Oldstyle_nums -> Pp.string ctx "oldstyle-nums"
  | Proportional_nums -> Pp.string ctx "proportional-nums"
  | Tabular_nums -> Pp.string ctx "tabular-nums"
  | Diagonal_fractions -> Pp.string ctx "diagonal-fractions"
  | Stacked_fractions -> Pp.string ctx "stacked-fractions"
  | Ordinal -> Pp.string ctx "ordinal"
  | Slashed_zero -> Pp.string ctx "slashed-zero"
  | Var v -> pp_var pp_font_variant_numeric_token ctx v

let rec pp_font_variant_numeric : font_variant_numeric Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Tokens tokens ->
      Pp.list ~sep:Pp.space pp_font_variant_numeric_token ctx tokens
  | Var v -> pp_var pp_font_variant_numeric ctx v
  | Composed
      {
        ordinal;
        slashed_zero;
        numeric_figure;
        numeric_spacing;
        numeric_fraction;
      } ->
      (* Print all 5 variables, including None values The Empty fallback in vars
         will produce var(--name,) *)
      let tokens =
        List.filter_map Fun.id
          [
            ordinal;
            slashed_zero;
            numeric_figure;
            numeric_spacing;
            numeric_fraction;
          ]
      in
      Pp.list ~sep:Pp.space pp_font_variant_numeric_token ctx tokens

let rec pp_text_size_adjust : text_size_adjust Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_size_adjust ctx v
  | None -> Pp.string ctx "none"
  | Auto -> Pp.string ctx "auto"
  | Pct n -> Pp.pct ctx n
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_webkit_font_smoothing : webkit_font_smoothing Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_webkit_font_smoothing ctx v
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Antialiased -> Pp.string ctx "antialiased"
  | Subpixel_antialiased -> Pp.string ctx "subpixel-antialiased"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_font_size : font_size Pp.t =
 fun ctx -> function
  | Length l -> pp_length ctx l
  | Pct f -> Pp.pct ctx f
  | Var v -> pp_var pp_font_size ctx v
  | Calc c -> (
      let rec to_length_calc : font_size calc -> length calc option = function
        | Val (Length l) -> Some (Val l)
        | Val (Pct n) -> Some (Val (Pct n : length))
        | Num n -> Some (Num n)
        | Math_const c -> Some (Math_const c)
        | Math_fn fn -> Some (Math_fn fn)
        | Var _ | Sibling_index | Sibling_count | Val _ -> None
        | Nested inner -> Option.map (fun c -> Nested c) (to_length_calc inner)
        | Parens inner -> Option.map (fun c -> Parens c) (to_length_calc inner)
        | Expr (left, op, right) -> (
            match (to_length_calc left, to_length_calc right) with
            | Some left, Some right -> Some (Expr (left, op, right))
            | _ -> None)
      in
      match (Pp.minified ctx, to_length_calc c) with
      | true, Some c -> pp_length ctx (Calc c)
      | _ -> pp_calc pp_font_size ctx c)
  | Xx_small -> Pp.string ctx "xx-small"
  | X_small -> Pp.string ctx "x-small"
  | Small -> Pp.string ctx "small"
  | Medium -> Pp.string ctx "medium"
  | Large -> Pp.string ctx "large"
  | X_large -> Pp.string ctx "x-large"
  | Xx_large -> Pp.string ctx "xx-large"
  | Xxx_large -> Pp.string ctx "xxx-large"
  | Larger -> Pp.string ctx "larger"
  | Smaller -> Pp.string ctx "smaller"
  | Math -> Pp.string ctx "math"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_moz_osx_font_smoothing : moz_osx_font_smoothing Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_moz_osx_font_smoothing ctx v
  | Auto -> Pp.string ctx "auto"
  | Grayscale -> Pp.string ctx "grayscale"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

(* Helpers for timing-function pretty printing *)

let pp_timing_float ctx f =
  Pp.string ctx (Pp.string_of_float ~drop_leading_zero:(Pp.minified ctx) f)

let pp_cubic_bezier_args : (float * float * float * float) Pp.t =
 fun ctx (a, b, c, d) ->
  Pp.list ~sep:Pp.comma pp_timing_float ctx [ a; b; c; d ]

let pp_cubic_bezier = Pp.call "cubic-bezier" pp_cubic_bezier_args

let rec pp_timing_function : timing_function Pp.t =
 fun ctx -> function
  | Ease -> Pp.string ctx "ease"
  | Linear -> Pp.string ctx "linear"
  | Ease_in -> Pp.string ctx "ease-in"
  | Ease_out -> Pp.string ctx "ease-out"
  | Ease_in_out -> Pp.string ctx "ease-in-out"
  | Step_start -> Pp.string ctx "step-start"
  | Step_end -> Pp.string ctx "step-end"
  | Steps (1, Some (Jump_start | Start)) when Pp.minified ctx ->
      (* CSS Easing 1 sec. 2: [steps(1, jump-start)] = [steps(1, start)] = the
         [step-start] alias; the [end] / [jump-end] equivalents fold to
         [step-end]. *)
      Pp.string ctx "step-start"
  | Steps (1, Some (Jump_end | End)) when Pp.minified ctx ->
      Pp.string ctx "step-end"
  | Steps (n, jump_term_opt) ->
      Pp.string ctx "steps(";
      Pp.int ctx n;
      (match jump_term_opt with
      | Some d ->
          Pp.char ctx ',';
          Pp.sp ctx ();
          pp_steps_direction ctx d
      | None -> ());
      Pp.char ctx ')'
  | Cubic_bezier (0.25, 0.1, 0.25, 1.0) when Pp.minified ctx ->
      (* CSS Easing 1 2: the named cubic-bezier aliases canonicalise to the
         keyword form, which is shorter than the four-argument call. *)
      Pp.string ctx "ease"
  | Cubic_bezier (0.42, 0.0, 1.0, 1.0) when Pp.minified ctx ->
      Pp.string ctx "ease-in"
  | Cubic_bezier (0.0, 0.0, 0.58, 1.0) when Pp.minified ctx ->
      Pp.string ctx "ease-out"
  | Cubic_bezier (0.42, 0.0, 0.58, 1.0) when Pp.minified ctx ->
      Pp.string ctx "ease-in-out"
  | Cubic_bezier (0.0, 0.0, 1.0, 1.0) when Pp.minified ctx ->
      (* CSS Easing 1 sec. 3: [cubic-bezier(0, 0, 1, 1)] is the identity curve,
         equivalent to the [linear] keyword. *)
      Pp.string ctx "linear"
  | Cubic_bezier (x1, y1, x2, y2) -> pp_cubic_bezier ctx (x1, y1, x2, y2)
  | Timing_functions timings ->
      Pp.list ~sep:Pp.comma pp_timing_function ctx timings
  | Linear_function body ->
      Pp.string ctx "linear(";
      Pp.string ctx body;
      Pp.char ctx ')'
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_timing_function ctx v

let rec pp_transition_property_value : transition_property_value Pp.t =
 fun ctx -> function
  | All -> Pp.string ctx "all"
  | None -> Pp.string ctx "none"
  | Property s -> Pp.string ctx s
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_transition_property_value ctx v

let pp_transition_property : transition_property Pp.t =
 fun ctx -> Pp.list ~sep:Pp.comma pp_transition_property_value ctx

let rec pp_transition_behavior : transition_behavior Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_transition_behavior ctx v
  | Normal -> Pp.string ctx "normal"
  | Allow_discrete -> Pp.string ctx "allow-discrete"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let transition_timing_is_default ctx = function
  | Ease when Pp.minified ctx -> true
  | Cubic_bezier (0.25, 0.1, 0.25, 1.0) when Pp.minified ctx -> true
  | _ -> false

let rec pp_overlay : overlay Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_overlay ctx v
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_transition_shorthand : transition_shorthand Pp.t =
 fun ctx { property; duration; timing_function; delay; behavior } ->
  pp_transition_property_value ctx property;
  (* Only output non-default values: defaults are 0s, ease, 0s *)
  (match duration with
  | Some (S 0.) | Some (Ms 0.) | None -> ()
  | Some d ->
      Pp.space ctx ();
      pp_duration ctx d);
  (match timing_function with
  | None -> ()
  | Some tf when transition_timing_is_default ctx tf -> ()
  | Some tf ->
      Pp.space ctx ();
      pp_timing_function ctx tf);
  (match delay with
  | Some (S 0.) | Some (Ms 0.) | None -> ()
  | Some d ->
      Pp.space ctx ();
      pp_duration ctx d);
  match behavior with
  | None | Some Normal -> ()
  | Some b ->
      Pp.space ctx ();
      pp_transition_behavior ctx b

let rec pp_transition : transition Pp.t =
 fun ctx -> function
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | None -> Pp.string ctx "none"
  | Var v -> pp_var pp_transition ctx v
  | Shorthand s -> pp_transition_shorthand ctx s

let rec pp_float_side : float_side Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_float_side ctx v
  | None -> Pp.string ctx "none"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Inline_start -> Pp.string ctx "inline-start"
  | Inline_end -> Pp.string ctx "inline-end"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_text_decoration_skip_ink : text_decoration_skip_ink Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_decoration_skip_ink ctx v
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | All -> Pp.string ctx "all"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_line_height : line_height Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Px f -> Pp.unit ctx f "px"
  | Rem f -> Pp.unit ctx f "rem"
  | Em f -> Pp.unit ctx f "em"
  | Pct p -> Pp.pct ctx p
  | Num n -> Pp.float ctx n
  | Number { value; unit; repr } -> (
      match (ctx.minify, unit) with
      | false, None -> Pp.string ctx repr
      | false, Some unit ->
          Pp.string ctx repr;
          Pp.string ctx unit
      | true, None -> Pp.float ctx value
      | true, Some "%" -> Pp.pct ctx value
      | true, Some unit -> Pp.unit ctx value unit)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_line_height ctx v
  | Calc c -> pp_calc pp_line_height ctx c

let rec pp_font_weight : font_weight Pp.t =
 fun ctx -> function
  | Weight n -> Pp.int ctx n
  | Normal when Pp.minified ctx ->
      (* CSS Fonts 4 5.1.2: [normal] is spec-equivalent to [400]. *)
      Pp.string ctx "400"
  | Normal -> Pp.string ctx "normal"
  | Bold when Pp.minified ctx ->
      (* CSS Fonts 4 5.1.2: [bold] is spec-equivalent to [700]. *)
      Pp.string ctx "700"
  | Bold -> Pp.string ctx "bold"
  | Bolder -> Pp.string ctx "bolder"
  | Lighter -> Pp.string ctx "lighter"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_weight ctx v

(* CSS Fonts 4 sec. 2.7: under minify, drop [<style>? <weight>? <stretch>?]
   components that equal their longhand initial value, and drop [line-height]
   when it's [normal]. The shorthand body itself is always [<size>
   [/<line-height>]? <family>+]; size and family are required. *)
let pp_font_variant_css21 ctx = function
  | (Normal : font_variant_css21) -> Pp.string ctx "normal"
  | Small_caps -> Pp.string ctx "small-caps"

let read_font_variant_css21 t : font_variant_css21 =
  Cursor.enum "font-variant-css21"
    [ ("normal", (Normal : font_variant_css21)); ("small-caps", Small_caps) ]
    t

let drop_font_default ctx (type a) ~(is_default : a -> bool) (opt : a option) :
    a option =
  if Pp.minified ctx then
    match opt with Some v when is_default v -> None | _ -> opt
  else opt

let drop_font_shorthand_defaults ctx style variant weight stretch line_height =
  let style =
    drop_font_default ctx style ~is_default:(function
      | (Normal : font_style) -> true
      | _ -> false)
  in
  let variant =
    drop_font_default ctx variant ~is_default:(function
      | (Normal : font_variant_css21) -> true
      | _ -> false)
  in
  let weight =
    drop_font_default ctx weight ~is_default:(function
      | (Normal : font_weight) | Weight 400 -> true
      | _ -> false)
  in
  let stretch =
    drop_font_default ctx stretch ~is_default:(function
      | (Normal : font_stretch) -> true
      | _ -> false)
  in
  let line_height =
    drop_font_default ctx line_height ~is_default:(function
      | (Normal : line_height) -> true
      | _ -> false)
  in
  (style, variant, weight, stretch, line_height)

let pp_font_prefix ctx style variant weight stretch =
  let first = ref true in
  let emit pp opt =
    Option.iter
      (fun v ->
        if not !first then Pp.space ctx ();
        first := false;
        pp ctx v)
      opt
  in
  emit pp_font_style style;
  emit pp_font_variant_css21 variant;
  emit pp_font_weight weight;
  (* The [font] shorthand's stretch component takes only the keywords, so the
     percentage the standalone property minifies to would be invalid here. *)
  emit pp_font_stretch_keyword stretch;
  !first

let pp_font_shorthand : font_shorthand Pp.t =
 fun ctx { style; variant; weight; stretch; size; line_height; family } ->
  let style, variant, weight, stretch, line_height =
    drop_font_shorthand_defaults ctx style variant weight stretch line_height
  in
  if not (pp_font_prefix ctx style variant weight stretch) then Pp.space ctx ();
  pp_font_size ctx size;
  Option.iter
    (fun lh ->
      Pp.char ctx '/';
      pp_line_height ctx lh)
    line_height;
  Pp.space ctx ();
  pp_font_family ctx family

let rec pp_font : font Pp.t =
 fun ctx -> function
  | Shorthand sh -> pp_font_shorthand ctx sh
  | Caption -> Pp.string ctx "caption"
  | Icon -> Pp.string ctx "icon"
  | Menu -> Pp.string ctx "menu"
  | Message_box -> Pp.string ctx "message-box"
  | Small_caption -> Pp.string ctx "small-caption"
  | Status_bar -> Pp.string ctx "status-bar"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font ctx v

let rec read_visibility t : visibility =
  Cursor.enum_or_var "visibility"
    [
      ("visible", (Visible : visibility));
      ("hidden", Hidden);
      ("collapse", Collapse);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_visibility t))
    t

let rec read_z_index t : z_index =
  let read_calc_z t =
    (* read_calc handles the calc(...) wrapper itself *)
    let expr =
      read_calc (fun _ -> Cursor.err t "unexpected value in z-index calc") t
    in
    match eval_numeric_calc expr with
    | Some f when Float.is_integer f -> Index (int_of_float f)
    | Some _ -> Cursor.err_invalid t "z-index calc must evaluate to integer"
    | None -> Calc expr
  in
  let read_var_z t : z_index = Var (read_var read_z_index t) in
  Cursor.enum_or_calls "z-index"
    [
      ("auto", (Auto : z_index));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("calc", read_calc_z); ("var", read_var_z) ]
    ~default:(fun t ->
      let n = Cursor.number t in
      if Float.is_integer n then Index (int_of_float n)
      else Cursor.err_invalid t "z-index must be integer")
    t

(* CSS Grid template - flattened type with direct constructors *)

let read_aspect_ratio_number t =
  (* CSS Sizing 4 5: an [<aspect-ratio>] component may be a [calc()] that
     resolves to a number. Keep the number AST for normal-output fidelity; the
     printer folds constant expressions for minified output. *)
  read_number t

let rec read_aspect_ratio (t : Cursor.t) : aspect_ratio =
  let read_var_ar t : aspect_ratio =
    (Var (read_var read_aspect_ratio t) : aspect_ratio)
  in
  let read_ratio t =
    let w = read_aspect_ratio_number t in
    Cursor.ws t;
    if Cursor.peek_delim t = Some '/' then (
      Cursor.expect '/' t;
      Cursor.ws t;
      let h = read_aspect_ratio_number t in
      (w, h))
    else (w, Num 1.0)
  in
  let read_number_or_ratio t : aspect_ratio =
    let w, h = read_ratio t in
    Cursor.ws t;
    match Cursor.peek_ident t with
    | Some "auto" ->
        Cursor.skip t;
        Auto_ratio_calc (w, h)
    | _ -> Ratio_calc (w, h)
  in
  let read_auto t : aspect_ratio =
    match Cursor.peek_ident t with
    | Some "auto" -> (
        Cursor.skip t;
        (* [auto] may stand alone or be followed by a [<ratio>]. Only treat a
           following number as a ratio so a trailing separator / whitespace
           (e.g. [aspect-ratio: auto;]) resolves to plain [Auto]. *)
        match Cursor.option read_ratio t with
        | Some (w, h) -> Auto_ratio_calc (w, h)
        | None -> Auto)
    | _ -> Cursor.err_expected t "auto"
  in
  Cursor.enum_or_var "aspect-ratio"
    [
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:read_var_ar
    ~default:(Cursor.one_of [ read_auto; read_number_or_ratio ])
    t

let rec read_text_overflow t : text_overflow =
  let read_var t : text_overflow = Var (read_var read_text_overflow t) in
  let read_css_wide_or_var t =
    Cursor.enum_or_calls "text-overflow"
      [
        ("inherit", (Inherit : text_overflow));
        ("initial", Initial);
        ("unset", Unset);
        ("revert", Revert);
        ("revert-layer", Revert_layer);
      ]
      ~calls:[ ("var", read_var) ]
      t
  in
  let read_single t =
    let read_string_overflow t : text_overflow = String (Cursor.string t) in
    Cursor.one_of
      [
        read_string_overflow;
        (fun t ->
          Cursor.enum "text-overflow"
            [ ("clip", (Clip : text_overflow)); ("ellipsis", Ellipsis) ]
            t);
      ]
      t
  in
  Cursor.one_of
    [
      read_css_wide_or_var;
      (fun t ->
        let first = read_single t in
        Cursor.ws t;
        (* CSS Text 4 sec. 9.1 two-value form. The declaration cursor extends
           past the value (the [;] / [!important] / [}] terminator is the
           caller's concern), so [Cursor.is_done] is the wrong gate here: try to
           read a second value, restore on failure. *)
        match
          try Some (Cursor.lookahead (fun t -> Some (read_single t)) t)
          with Cursor.Parse_error _ -> None
        with
        | Some (Some _) ->
            let second = read_single t in
            Cursor.ws t;
            Pair (first, second)
        | _ -> first);
    ]
    t

let rec read_text_wrap t : text_wrap =
  Cursor.enum_or_var "text-wrap"
    [
      ("wrap", (Wrap : text_wrap));
      ("nowrap", No_wrap);
      ("balance", Balance);
      ("pretty", Pretty);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_wrap t))
    t

let rec read_text_wrap_mode t : text_wrap_mode =
  Cursor.enum_or_var "text-wrap-mode"
    [
      ("wrap", (Wrap : text_wrap_mode));
      ("nowrap", No_wrap);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_wrap_mode t))
    t

let rec read_text_wrap_style t : text_wrap_style =
  Cursor.enum_or_var "text-wrap-style"
    [
      ("auto", (Auto : text_wrap_style));
      ("balance", Balance);
      ("pretty", Pretty);
      ("stable", Stable);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_wrap_style t))
    t

let rec read_text_box_trim t : text_box_trim =
  Cursor.enum_or_var "text-box-trim"
    [
      ("none", (None : text_box_trim));
      ("trim-start", Trim_start);
      ("trim-end", Trim_end);
      ("trim-both", Trim_both);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_box_trim t))
    t

let read_text_box_edge_keyword t : text_box_edge_keyword =
  Cursor.enum "text-box-edge"
    [
      ("text", (Text : text_box_edge_keyword));
      ("cap", Cap);
      ("ex", Ex);
      ("alphabetic", Alphabetic);
      ("ideographic", Ideographic);
      ("ideographic-ink", Ideographic_ink);
    ]
    t

let read_text_box_edge_value t : text_box_edge =
  let keywords =
    Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2 read_text_box_edge_keyword
      t
  in
  match keywords with
  | [ Text ] | [ Ideographic_ink ] -> Edge (List.hd keywords, None)
  | [ ((Cap | Ex) as first); ((Alphabetic | Text) as second) ]
  | [ (Text as first); ((Alphabetic | Ideographic) as second) ] ->
      Edge (first, Some second)
  | _ -> Cursor.err_invalid t "text-box-edge"

let rec read_text_box_edge ?(global = true) t : text_box_edge =
  if not global then read_text_box_edge_value t
  else
    Cursor.enum_or_var "text-box-edge"
      [
        ("auto", (Auto : text_box_edge));
        ("inherit", Inherit);
        ("initial", Initial);
        ("unset", Unset);
        ("revert", Revert);
        ("revert-layer", Revert_layer);
      ]
      ~var:(fun t ->
        (Var (Values.read_var read_text_box_edge t) : text_box_edge))
      ~default:read_text_box_edge_value t

let rec read_text_box t : text_box =
  Cursor.enum_or_var "text-box"
    [
      ("initial", (Initial : text_box));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_text_box t) : text_box))
    ~default:(fun t ->
      let trim = read_text_box_trim t in
      Cursor.ws t;
      let edge : text_box_edge option =
        if Cursor.is_done t then None
        else Some (read_text_box_edge ~global:false t)
      in
      (Box (trim, edge) : text_box))
    t

let rec read_inline_sizing t : inline_sizing =
  Cursor.enum_or_var "inline-sizing"
    [
      ("normal", (Normal : inline_sizing));
      ("stretch", Stretch);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_inline_sizing t) : inline_sizing))
    t

let read_line_fit_edge_keyword t : line_fit_edge_keyword =
  Cursor.enum "line-fit-edge"
    [
      ("leading", (Leading : line_fit_edge_keyword));
      ("text", Text);
      ("cap", Cap);
      ("ex", Ex);
      ("alphabetic", Alphabetic);
      ("ideographic", Ideographic);
      ("ideographic-ink", Ideographic_ink);
    ]
    t

let rec read_line_fit_edge t : line_fit_edge =
  Cursor.enum_or_var "line-fit-edge"
    [
      ("inherit", (Inherit : line_fit_edge));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_line_fit_edge t) : line_fit_edge))
    ~default:(fun t ->
      let keywords =
        Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
          read_line_fit_edge_keyword t
      in
      match keywords with
      | [ Leading ] | [ Text ] | [ Ideographic_ink ] ->
          (Edge (List.hd keywords, None) : line_fit_edge)
      | [ ((Cap | Ex) as first); ((Alphabetic | Text) as second) ]
      | [ (Text as first); (Alphabetic as second) ] ->
          Edge (first, Some second)
      | _ -> Cursor.err_invalid t "line-fit-edge")
    t

let rec read_interpolate_size t : interpolate_size =
  Cursor.enum_or_var "interpolate-size"
    [
      ("numeric-only", (Numeric_only : interpolate_size));
      ("allow-keywords", Allow_keywords);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_interpolate_size t) : interpolate_size))
    t

let read_min_intrinsic_sizing_keyword t : min_intrinsic_sizing_keyword =
  Cursor.enum "min-intrinsic-sizing"
    [
      ("legacy", (Legacy : min_intrinsic_sizing_keyword));
      ("zero-if-scroll", Zero_if_scroll);
      ("zero-if-extrinsic", Zero_if_extrinsic);
    ]
    t

let rec read_min_intrinsic_sizing t : min_intrinsic_sizing =
  Cursor.enum_or_var "min-intrinsic-sizing"
    [
      ("inherit", (Inherit : min_intrinsic_sizing));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_min_intrinsic_sizing t) : min_intrinsic_sizing))
    ~default:(fun t ->
      let keywords =
        Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
          read_min_intrinsic_sizing_keyword t
      in
      let duplicate keyword =
        List.length (List.filter (( = ) keyword) keywords) > 1
      in
      if List.exists duplicate keywords then
        Cursor.err_invalid t "min-intrinsic-sizing";
      Sizing keywords)
    t

let rec read_ruby_merge t : ruby_merge =
  Cursor.enum_or_var "ruby-merge"
    [
      ("separate", (Separate : ruby_merge));
      ("merge", Merge);
      ("auto", Auto);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_ruby_merge t) : ruby_merge))
    t

let rec read_ruby_align t : ruby_align =
  Cursor.enum_or_var "ruby-align"
    [
      ("start", (Start : ruby_align));
      ("center", Center);
      ("space-between", Space_between);
      ("space-around", Space_around);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_ruby_align t) : ruby_align))
    t

let rec read_ruby_overhang t : ruby_overhang =
  Cursor.enum_or_var "ruby-overhang"
    [
      ("auto", (Auto : ruby_overhang));
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_ruby_overhang t) : ruby_overhang))
    t

let read_ruby_position_keyword t : ruby_position_keyword =
  Cursor.enum "ruby-position"
    [
      ("alternate", (Alternate : ruby_position_keyword));
      ("over", Over);
      ("under", Under);
      ("inter-character", Inter_character);
    ]
    t

let rec read_ruby_position t : ruby_position =
  Cursor.enum_or_var "ruby-position"
    [
      ("inherit", (Inherit : ruby_position));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_ruby_position t) : ruby_position))
    ~default:(fun t ->
      let keywords =
        Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
          read_ruby_position_keyword t
      in
      let duplicate keyword =
        List.length (List.filter (( = ) keyword) keywords) > 1
      in
      let valid =
        match keywords with
        | [ _ ] -> true
        | [ Alternate; (Over | Under) ] | [ (Over | Under); Alternate ] -> true
        | _ -> false
      in
      if (not valid) || List.exists duplicate keywords then
        Cursor.err_invalid t "ruby-position";
      (Position keywords : ruby_position))
    t

let rec read_text_spacing_trim t : text_spacing_trim =
  Cursor.enum_or_var "text-spacing-trim"
    [
      ("normal", (Normal : text_spacing_trim));
      ("space-all", Space_all);
      ("trim-start", Trim_start);
      ("space-first", Space_first);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_spacing_trim t))
    t

let rec read_hyphenate_limit_chars t : hyphenate_limit_chars =
  let read_counts t =
    let counts =
      Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:3 Cursor.int t
    in
    let check_count n =
      if n < 1 then Cursor.err_invalid t "hyphenate-limit-chars must be >= 1"
    in
    List.iter check_count counts;
    Cursor.ws t;
    Cursor.expect_eof t;
    match counts with
    | [ a ] -> (One a : hyphenate_limit_chars)
    | [ a; b ] -> Two (a, b)
    | [ a; b; c ] -> Three (a, b, c)
    | _ -> Cursor.err_invalid t "expected one to three integers"
  in
  Cursor.enum_or_calls "hyphenate-limit-chars"
    [
      ("auto", (Auto : hyphenate_limit_chars));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (read_var read_hyphenate_limit_chars t)) ]
    ~default:read_counts t

let rec read_white_space t : white_space =
  Cursor.ws t;
  match Cursor.peek_ident t with
  | Some "preserve" ->
      ignore (Cursor.ident t : string);
      Cursor.ws t;
      Cursor.expect_string "nowrap" t;
      Preserve_nowrap
  | _ ->
      Cursor.enum_or_var "white-space"
        [
          ("normal", (Normal : white_space));
          ("nowrap", Nowrap);
          ("pre", Pre);
          ("pre-wrap", Pre_wrap);
          ("pre-line", Pre_line);
          ("break-spaces", Break_spaces);
          ("inherit", Inherit);
          ("initial", Initial);
          ("unset", Unset);
          ("revert", Revert);
          ("revert-layer", Revert_layer);
        ]
        ~var:(fun t -> Var (read_var read_white_space t))
        t

let rec read_word_break t : word_break =
  Cursor.enum_or_var "word-break"
    [
      ("normal", (Normal : word_break));
      ("break-all", Break_all);
      ("keep-all", Keep_all);
      ("break-word", Break_word);
      ("auto-phrase", Auto_phrase);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_word_break t))
    t

let rec read_overflow_wrap t : overflow_wrap =
  Cursor.enum_or_var "overflow-wrap"
    [
      ("normal", (Normal : overflow_wrap));
      ("break-word", Break_word);
      ("anywhere", Anywhere);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_overflow_wrap t))
    t

let rec read_hyphens t : hyphens =
  Cursor.enum_or_var "hyphens"
    [
      ("none", (None : hyphens));
      ("manual", Manual);
      ("auto", Auto);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_hyphens t))
    t

let rec read_line_height t : line_height =
  let read_var t : line_height = Var (read_var read_line_height t) in
  let read_calc t : line_height =
    Calc (read_calc read_line_height t |> numeric_line_height_calc_leaves)
  in
  Cursor.enum_or_calls "line-height"
    [
      ("normal", Normal);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var); ("calc", read_calc) ]
    ~default:read_line_height_length t

let read_symbols_type t : symbols_type =
  Cursor.enum "symbols type"
    [
      ("cyclic", (Cyclic : symbols_type));
      ("numeric", Numeric);
      ("alphabetic", Alphabetic);
      ("symbolic", Symbolic);
      ("fixed", Fixed);
    ]
    t

let read_list_style_symbol t : list_style_symbol =
  Cursor.one_of
    [
      (fun t -> (Url (Cursor.url t) : list_style_symbol));
      (fun t -> String (Cursor.string t));
    ]
    t

let list_style_type_keywords : (string * list_style_type) list =
  [
    ("none", (None : list_style_type));
    ("disc", Disc);
    ("circle", Circle);
    ("square", Square);
    ("decimal", Decimal);
    ("lower-alpha", Lower_alpha);
    ("upper-alpha", Upper_alpha);
    ("lower-roman", Lower_roman);
    ("upper-roman", Upper_roman);
    ("decimal-leading-zero", (Decimal_leading_zero : list_style_type));
    ("arabic-indic", (Arabic_indic : list_style_type));
    ("armenian", (Armenian : list_style_type));
    ("upper-armenian", (Upper_armenian : list_style_type));
    ("lower-armenian", (Lower_armenian : list_style_type));
    ("bengali", (Bengali : list_style_type));
    ("cambodian", (Cambodian : list_style_type));
    ("khmer", (Khmer : list_style_type));
    ("cjk-decimal", (Cjk_decimal : list_style_type));
    ("devanagari", (Devanagari : list_style_type));
    ("georgian", (Georgian : list_style_type));
    ("gujarati", (Gujarati : list_style_type));
    ("gurmukhi", (Gurmukhi : list_style_type));
    ("hebrew", (Hebrew : list_style_type));
    ("kannada", (Kannada : list_style_type));
    ("lao", (Lao : list_style_type));
    ("malayalam", (Malayalam : list_style_type));
    ("mongolian", (Mongolian : list_style_type));
    ("myanmar", (Myanmar : list_style_type));
    ("oriya", (Oriya : list_style_type));
    ("persian", (Persian : list_style_type));
    ("tamil", (Tamil : list_style_type));
    ("telugu", (Telugu : list_style_type));
    ("thai", (Thai : list_style_type));
    ("tibetan", (Tibetan : list_style_type));
    ("lower-latin", (Lower_latin : list_style_type));
    ("upper-latin", (Upper_latin : list_style_type));
    ("cjk-earthly-branch", (Cjk_earthly_branch : list_style_type));
    ("cjk-heavenly-stem", (Cjk_heavenly_stem : list_style_type));
    ("lower-greek", (Lower_greek : list_style_type));
    ("hiragana", (Hiragana : list_style_type));
    ("hiragana-iroha", (Hiragana_iroha : list_style_type));
    ("katakana", (Katakana : list_style_type));
    ("katakana-iroha", (Katakana_iroha : list_style_type));
    ("disclosure-open", (Disclosure_open : list_style_type));
    ("disclosure-closed", (Disclosure_closed : list_style_type));
    ("cjk-ideographic", (Cjk_ideographic : list_style_type));
    ("japanese-informal", (Japanese_informal : list_style_type));
    ("japanese-formal", (Japanese_formal : list_style_type));
    ("korean-hangul-formal", (Korean_hangul_formal : list_style_type));
    ("korean-hanja-informal", (Korean_hanja_informal : list_style_type));
    ("korean-hanja-formal", (Korean_hanja_formal : list_style_type));
    ("simp-chinese-informal", (Simp_chinese_informal : list_style_type));
    ("simp-chinese-formal", (Simp_chinese_formal : list_style_type));
    ("trad-chinese-informal", (Trad_chinese_informal : list_style_type));
    ("trad-chinese-formal", (Trad_chinese_formal : list_style_type));
    ("ethiopic-numeric", (Ethiopic_numeric : list_style_type));
    ("inherit", Inherit);
    ("initial", Initial);
    ("unset", Unset);
    ("revert", Revert);
    ("revert-layer", Revert_layer);
  ]

let rec read_list_style_type t : list_style_type =
  let read_var t : list_style_type = Var (read_var read_list_style_type t) in
  let read_symbols_body t : list_style_type =
    let kind = Cursor.option read_symbols_type t in
    Cursor.ws t;
    let symbols =
      Cursor.list ~sep:Cursor.ws ~at_least:1 read_list_style_symbol t
    in
    Symbols (kind, symbols)
  in
  Cursor.enum_or_var "list-style-type" list_style_type_keywords ~var:read_var
    ~default:
      (Cursor.one_of
         [
           (fun t -> Cursor.call "symbols" t read_symbols_body);
           (fun t -> (String (Cursor.string t) : list_style_type));
         ])
    t

let rec read_list_style_position t : list_style_position =
  Cursor.enum_or_var "list-style-position"
    [
      ("inside", (Inside : list_style_position));
      ("outside", Outside);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_list_style_position t))
    t

let rec read_list_style_image t : list_style_image =
  let read_url t = (Url (Cursor.url t) : list_style_image) in
  let read_var t : list_style_image = Var (read_var read_list_style_image t) in
  Cursor.one_of
    [
      read_url;
      (fun t ->
        Cursor.enum_or_calls "list-style-image"
          [
            ("none", (None : list_style_image));
            ("inherit", Inherit);
            ("initial", Initial);
            ("unset", Unset);
            ("revert", Revert);
            ("revert-layer", Revert_layer);
          ]
          ~calls:[ ("var", read_var) ]
          t);
    ]
    t

(* Parse the [list-style] shorthand into a typed [list_style_shorthand] record.
   Each slot is recognised by the longhand reader; a single bare [none]
   populates both [type_] and [image] per CSS Lists 3 sec. 4.1. *)
let try_list_style_slot r read_fn (slot : 'a option ref) =
  if !slot <> Option.None then false
  else
    let pos = Cursor.save r in
    match read_fn r with
    | v ->
        slot := Some v;
        true
    | exception Cursor.Parse_error _ ->
        Cursor.restore r pos;
        false

let read_list_style_shorthand r : list_style_shorthand =
  let type_ : list_style_type option ref = ref Option.None in
  let position : list_style_position option ref = ref Option.None in
  let image : list_style_image option ref = ref Option.None in
  let saw_none = ref false in
  let try_one () =
    try_list_style_slot r read_list_style_position position
    || try_list_style_slot r read_list_style_image image
    || try_list_style_slot r read_list_style_type type_
  in
  let rec consume () =
    Cursor.ws r;
    if Cursor.is_done r then ()
    else
      let saved = Cursor.save r in
      let kw = Cursor.peek_ident r in
      if kw = Some "none" then begin
        let _ = Cursor.ident r in
        saw_none := true;
        consume ()
      end
      else if try_one () then consume ()
      else Cursor.restore r saved
  in
  consume ();
  Cursor.ws r;
  if not (Cursor.is_done r) then
    Cursor.err_invalid r "invalid list-style shorthand";
  if !saw_none then begin
    if !type_ = Option.None then type_ := Some (None : list_style_type);
    if !image = Option.None then image := Some (None : list_style_image)
  end;
  if
    !type_ = Option.None && !position = Option.None && !image = Option.None
    && not !saw_none
  then Cursor.err_invalid r "invalid list-style shorthand";
  { type_ = !type_; position = !position; image = !image }

let rec read_list_style t : list_style =
  let raw = Cursor.lookahead (Cursor.consume_to_decl_end ~trim:true) t in
  let lower = String.lowercase_ascii (String.trim raw) in
  match lower with
  | "inherit" ->
      ignore (Cursor.consume_to_decl_end ~trim:true t);
      Inherit
  | "initial" ->
      ignore (Cursor.consume_to_decl_end ~trim:true t);
      Initial
  | "unset" ->
      ignore (Cursor.consume_to_decl_end ~trim:true t);
      Unset
  | "revert" ->
      ignore (Cursor.consume_to_decl_end ~trim:true t);
      Revert
  | "revert-layer" ->
      ignore (Cursor.consume_to_decl_end ~trim:true t);
      Revert_layer
  | _ ->
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
      if is_valid_var () then (
        let r = Cursor.of_string raw in
        let var = Values.read_var (fun r -> read_list_style r) r in
        ignore (Cursor.consume_to_decl_end ~trim:true t);
        Var var)
      else
        let body =
          try read_list_style_shorthand (Cursor.of_string raw)
          with Cursor.Parse_error _ ->
            Cursor.err_invalid t "invalid list-style shorthand"
        in
        ignore (Cursor.consume_to_decl_end ~trim:true t);
        Shorthand body

let rec read_table_layout t : table_layout =
  Cursor.enum_or_var "table-layout"
    [
      ("auto", (Auto : table_layout));
      ("fixed", Fixed);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_table_layout t) : table_layout))
    t

let rec read_box_sizing (t : Cursor.t) : box_sizing =
  Cursor.enum_or_var "box-sizing"
    [
      ("border-box", (Border_box : box_sizing));
      ("content-box", Content_box);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_box_sizing t) : box_sizing))
    t

let rec read_caption_side t : caption_side =
  Cursor.enum_or_var "caption-side"
    [
      ("top", (Top : caption_side));
      ("bottom", Bottom);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_caption_side t))
    t

let rec read_object_fit t : object_fit =
  Cursor.enum_or_var "object-fit"
    [
      ("fill", (Fill : object_fit));
      ("contain", Contain);
      ("cover", Cover);
      ("none", None);
      ("scale-down", Scale_down);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_object_fit t))
    t

let read_content_string t =
  match Cursor.string_repr_with_quote_opt t with
  | Some (value, quote, repr) -> Quoted { value; quote; repr }
  | None -> Cursor.err_expected t "string"

let read_content_counter t =
  Cursor.call "counter" t (fun inner ->
      Cursor.ws inner;
      let name = Cursor.ident inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      Counter name)

let read_content_string_ref t =
  Cursor.call "string" t (fun inner ->
      Cursor.ws inner;
      let name = Cursor.ident inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      String_ref name)

let read_content_counters t =
  Cursor.call "counters" t (fun inner ->
      Cursor.ws inner;
      let name = Cursor.ident inner in
      Cursor.ws inner;
      Cursor.comma inner;
      Cursor.ws inner;
      let separator = Cursor.string inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      (Counters (name, separator) : content))

let rec read_content_attr t =
  Cursor.call "attr" t (fun inner ->
      Cursor.ws inner;
      let name = Cursor.ident ~keep_case:true inner in
      let type_ : Values.attr_type option =
        Cursor.ws inner;
        if Cursor.is_done inner || Cursor.peek_comma inner then Option.None
        else Option.Some (Values.read_attr_type inner)
      in
      Cursor.ws inner;
      let fallback : content Values.attr_fallback =
        if Cursor.comma_opt inner then (
          Cursor.ws inner;
          if Cursor.is_done inner then Empty_fallback
          else Attr_fallback (read_content inner))
        else No_fallback
      in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      Attr { name; type_; fallback })

and read_content_single t =
  let read_var t : content = Var (read_var read_content t) in
  Cursor.enum_or_calls "content"
    [
      ("none", (None : content));
      ("normal", Normal);
      ("open-quote", Open_quote);
      ("close-quote", Close_quote);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:
      [
        ("var", read_var);
        ("attr", read_content_attr);
        ("counter", read_content_counter);
        ("string", read_content_string_ref);
        ("counters", read_content_counters);
      ]
    ~default:read_content_string t

and read_content t : content =
  let items = Cursor.list ~sep:Cursor.ws ~at_least:1 read_content_single t in
  match items with
  | [ item ] -> item
  | _ ->
      if
        List.exists
          (fun (item : content) ->
            match item with None | Normal -> true | _ -> false)
          items
      then Cursor.err_invalid t "none/normal cannot be combined in content";
      Content_list items

let counter_name_reserved =
  [ "none"; "inherit"; "initial"; "unset"; "revert"; "revert-layer" ]

let read_counter_name t =
  let name = Cursor.ident t in
  if List.mem name counter_name_reserved then
    Cursor.err_invalid t ("reserved counter name: " ^ name);
  name

let read_counter_item t =
  let name = read_counter_name t in
  Cursor.ws t;
  let value = Cursor.integer_opt t in
  { name; value }

let rec read_counter_set t : counter_set =
  Cursor.enum_or_var "counter"
    [
      ("none", (None : counter_set));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_counter_set t))
    ~default:(fun t ->
      let items = Cursor.list ~sep:Cursor.ws ~at_least:1 read_counter_item t in
      Counters items)
    t

let rec read_quotes t : quotes =
  let read_var' t : quotes = Var (read_var read_quotes t) in
  (* Read pairs of strings for quotes property *)
  let read_pairs t =
    let rec read_quotes_pairs acc =
      Cursor.ws t;
      match Cursor.string_opt t with
      | Some open_q ->
          Cursor.ws t;
          let close_q = Cursor.string t in
          read_quotes_pairs ((open_q, close_q) :: acc)
      | None -> List.rev acc
    in
    Pairs (read_quotes_pairs [])
  in
  Cursor.enum_or_calls "quotes"
    [
      ("auto", (Auto : quotes));
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert-layer", Revert_layer);
      ("revert", Revert);
    ]
    ~calls:[ ("var", read_var') ]
    ~default:read_pairs t

let rec read_overflow_anchor (t : Cursor.t) : overflow_anchor =
  let keywords : (string * overflow_anchor) list =
    [
      ("auto", Auto);
      ("none", None);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "overflow-anchor" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_overflow_anchor t) : overflow_anchor))
     t
    : overflow_anchor)

let rec read_font_palette (t : Cursor.t) : font_palette =
  let keywords : (string * font_palette) list =
    [
      ("normal", Normal);
      ("light", Light);
      ("dark", Dark);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "font-palette" keywords
     ~var:(fun t -> (Var (Values.read_var read_font_palette t) : font_palette))
     ~default:(fun t -> (Palette (read_dashed_ident t) : font_palette))
     t
    : font_palette)

let read_font_synthesis_feature t : font_synthesis_feature =
  Cursor.enum "font-synthesis feature"
    [
      ("weight", Weight);
      ("style", Style);
      ("small-caps", Small_caps);
      ("position", Position);
    ]
    t

let rec read_font_synthesis (t : Cursor.t) : font_synthesis =
  let keywords : (string * font_synthesis) list =
    [
      ("none", None);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  let read_features t =
    let features =
      Cursor.list ~sep:Cursor.ws ~at_least:1 read_font_synthesis_feature t
    in
    let rec duplicates = function
      | [] -> false
      | x :: xs -> List.mem x xs || duplicates xs
    in
    if duplicates features then
      Cursor.err_invalid t "duplicate font-synthesis feature";
    (Features features : font_synthesis)
  in
  (Cursor.enum_or_var "font-synthesis" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_font_synthesis t) : font_synthesis))
     ~default:read_features t
    : font_synthesis)

let rec read_animation_timeline (t : Cursor.t) : animation_timeline =
  Cursor.ws t;
  match Cursor.peek t with
  | Some (Component.Func { node = { name = "var"; _ }; _ }) ->
      (Var (Values.read_var read_animation_timeline t) : animation_timeline)
  | Some (Component.Func fn) when not fn.node.terminated ->
      Cursor.err_invalid t
        (String.concat "" [ "unterminated function "; fn.node.name; "(...)" ])
  | Some (Component.Func fn)
    when fn.node.name = "scroll" || fn.node.name = "view" ->
      let _ = Cursor.next t in
      let args = Parser.string_of_components fn.node.arguments in
      if fn.node.name = "scroll" then Scroll args else View args
  | _ ->
      let keywords : (string * animation_timeline) list =
        [
          ("none", None);
          ("auto", Auto);
          ("initial", Initial);
          ("inherit", Inherit);
          ("unset", Unset);
          ("revert", Revert);
          ("revert-layer", Revert_layer);
        ]
      in
      (Cursor.enum "animation-timeline" keywords
         ~default:(fun t -> (Name (read_dashed_ident t) : animation_timeline))
         t
        : animation_timeline)

let rec read_view_transition_name (t : Cursor.t) : view_transition_name =
  let keywords : (string * view_transition_name) list =
    [
      ("none", None);
      ("match-element", Match_element);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  let read_name t =
    let name = Cursor.ident ~keep_case:true t in
    if String.lowercase_ascii name = "auto" then
      Cursor.err_invalid t "invalid view-transition-name: auto";
    (Name name : view_transition_name)
  in
  (Cursor.enum_or_var "view-transition-name" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_view_transition_name t)
         : view_transition_name))
     ~default:read_name t
    : view_transition_name)

let view_transition_class_reserved =
  [ "none"; "initial"; "inherit"; "unset"; "revert"; "revert-layer" ]

let read_view_transition_class_ident t =
  let ident = Cursor.ident ~keep_case:true t in
  if List.mem (String.lowercase_ascii ident) view_transition_class_reserved then
    Cursor.err_invalid t ("reserved view-transition-class ident: " ^ ident)
  else ident

let rec read_view_transition_class t : view_transition_class =
  let keywords : (string * view_transition_class) list =
    [
      ("none", None);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "view-transition-class" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_view_transition_class t)
         : view_transition_class))
     ~default:(fun t ->
       (Classes
          (Cursor.list ~sep:Cursor.ws ~at_least:1
             read_view_transition_class_ident t)
         : view_transition_class))
     t
    : view_transition_class)

let rec read_timeline_axis t : timeline_axis =
  Cursor.enum_or_var "timeline-axis"
    [
      ("block", (Block : timeline_axis));
      ("inline", (Inline : timeline_axis));
      ("x", (X : timeline_axis));
      ("y", (Y : timeline_axis));
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_timeline_axis t))
    t

let rec read_timeline_name t : timeline_name =
  Cursor.enum_or_var "timeline-name"
    [
      ("none", (None : timeline_name));
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_timeline_name t) : timeline_name))
    ~default:(fun t ->
      (Names (Cursor.list ~sep:Cursor.comma ~at_least:1 read_dashed_ident t)
        : timeline_name))
    t

let read_timeline_shorthand_item t : timeline_shorthand_item =
  Cursor.ws t;
  let name = Cursor.ident ~keep_case:true t in
  if not (String.starts_with ~prefix:"--" name) then
    Cursor.err_invalid t "timeline name";
  Cursor.ws t;
  let axis = read_timeline_axis t in
  { name; axis }

let rec read_timeline_shorthand t : timeline_shorthand =
  Cursor.enum_or_var "timeline"
    [
      ("none", (None : timeline_shorthand));
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_timeline_shorthand t))
    ~default:(fun t ->
      Timelines
        (Cursor.list ~sep:Cursor.comma ~at_least:1 read_timeline_shorthand_item
           t))
    t

let read_timeline_inset_item t : timeline_inset_item =
  Cursor.enum "timeline-inset item"
    [ ("auto", (Auto : timeline_inset_item)) ]
    ~default:(fun t ->
      (Length
         (read_length_percentage ~allow_negative:false ~with_keywords:false t)
        : timeline_inset_item))
    t

let rec read_timeline_inset t : timeline_inset =
  Cursor.enum_or_var "timeline-inset"
    [
      ("initial", (Initial : timeline_inset));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_timeline_inset t) : timeline_inset))
    ~default:(fun t ->
      match Cursor.list ~at_least:1 ~at_most:2 read_timeline_inset_item t with
      | [ first ] -> (Inset (first, None) : timeline_inset)
      | [ first; second ] -> (Inset (first, Some second) : timeline_inset)
      | _ -> Cursor.err_expected t "timeline-inset")
    t

let rec read_text_size_adjust t : text_size_adjust =
  Cursor.ws t;
  match Cursor.percentage_opt t with
  | Some n ->
      if n < 0.0 then
        Cursor.err t "text-size-adjust percentages cannot be negative"
      else Pct n
  | _ ->
      (* Keyword *)
      Cursor.enum_or_var "text-size-adjust"
        [
          ("none", (None : text_size_adjust));
          ("auto", Auto);
          ("inherit", Inherit);
          ("initial", Initial);
          ("unset", Unset);
          ("revert", Revert);
          ("revert-layer", Revert_layer);
        ]
        ~var:(fun t -> Var (Values.read_var read_text_size_adjust t))
        t

let rec read_webkit_font_smoothing t : webkit_font_smoothing =
  Cursor.enum_or_var "webkit-font-smoothing"
    [
      ("auto", (Auto : webkit_font_smoothing));
      ("none", None);
      ("antialiased", Antialiased);
      ("subpixel-antialiased", Subpixel_antialiased);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_webkit_font_smoothing t))
    t

let rec read_moz_osx_font_smoothing t : moz_osx_font_smoothing =
  Cursor.enum_or_var "moz-osx-font-smoothing"
    [
      ("auto", (Auto : moz_osx_font_smoothing));
      ("grayscale", Grayscale);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_moz_osx_font_smoothing t))
    t

let rec read_clear (t : Cursor.t) : clear =
  Cursor.enum_or_var "clear"
    [
      ("none", (None : clear));
      ("left", Left);
      ("right", Right);
      ("both", Both);
      ("inline-start", Inline_start);
      ("inline-end", Inline_end);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_clear t))
    t

let rec read_float_side (t : Cursor.t) : float_side =
  Cursor.enum_or_var "float-side"
    [
      ("none", (None : float_side));
      ("left", Left);
      ("right", Right);
      ("inline-start", Inline_start);
      ("inline-end", Inline_end);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_float_side t))
    t

let rec read_tab_size (t : Cursor.t) : tab_size =
  let read_value t =
    match Cursor.integer_opt t with
    | Some i ->
        if i < 0 then Cursor.err_invalid t "negative tab-size integer";
        (Int i : tab_size)
    | None ->
        Length (Values.read_length ~allow_negative:false ~with_keywords:false t)
  in
  Cursor.enum_or_var "tab-size"
    [
      ("initial", (Initial : tab_size));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_tab_size t))
    ~default:read_value t

let rec read_zoom (t : Cursor.t) : zoom =
  let read_value t =
    match Cursor.percentage_opt t with
    | Some p -> (Pct p : zoom)
    | None -> (
        match Cursor.number_opt t with
        | Some n -> Num n
        | None ->
            Cursor.err_invalid t "expected a number or percentage for zoom")
  in
  Cursor.enum_or_var "zoom"
    [
      ("normal", (Normal : zoom));
      ("reset", Reset);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_zoom t))
    ~default:read_value t

let rec read_text_decoration_skip_ink t : text_decoration_skip_ink =
  Cursor.enum_or_var "text-decoration-skip-ink"
    [
      ("auto", (Auto : text_decoration_skip_ink));
      ("none", None);
      ("all", All);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_text_decoration_skip_ink t))
    t

let rec read_vertical_align t : vertical_align =
  let read_var t : vertical_align = Var (read_var read_vertical_align t) in
  let read_calc t : vertical_align = Calc (read_calc read_vertical_align t) in
  Cursor.enum_or_calls "vertical-align"
    [
      ("baseline", (Baseline : vertical_align));
      ("top", Top);
      ("middle", Middle);
      ("bottom", Bottom);
      ("text-top", Text_top);
      ("text-bottom", Text_bottom);
      ("sub", Sub);
      ("super", Super);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var); ("calc", read_calc) ]
    ~default:read_vertical_align_length t

let font_family_generic_css =
  [
    ("sans-serif", Sans_serif);
    ("serif", Serif);
    ("monospace", Monospace);
    ("cursive", Cursive);
    ("fantasy", Fantasy);
    ("system-ui", System_ui);
    ("ui-sans-serif", Ui_sans_serif);
    ("ui-serif", Ui_serif);
    ("ui-monospace", Ui_monospace);
    ("ui-rounded", Ui_rounded);
    ("emoji", Emoji);
    ("math", Math);
    ("fangsong", Fangsong);
  ]

let font_family_css_keywords : (string * font_family) list =
  [
    ("inherit", Inherit);
    ("initial", Initial);
    ("unset", Unset);
    ("revert", Revert);
    ("revert-layer", Revert_layer);
  ]

let font_family_popular_web =
  [
    ("inter", Inter);
    ("roboto", Roboto);
    ("open-sans", Open_sans);
    ("lato", Lato);
    ("montserrat", Montserrat);
    ("poppins", Poppins);
    ("source-sans-pro", Source_sans_pro);
    ("raleway", Raleway);
    ("oswald", Oswald);
    ("noto-sans", Noto_sans);
    ("ubuntu", Ubuntu);
    ("playfair-display", Playfair_display);
    ("merriweather", Merriweather);
    ("lora", Lora);
    ("pt-sans", PT_sans);
    ("pt-serif", PT_serif);
    ("nunito", Nunito);
    ("nunito-sans", Nunito_sans);
    ("work-sans", Work_sans);
    ("rubik", Rubik);
    ("fira-sans", Fira_sans);
    ("fira-code", Fira_code);
    ("jetbrains-mono", JetBrains_mono);
    ("ibm-plex-sans", IBM_plex_sans);
    ("ibm-plex-serif", IBM_plex_serif);
    ("ibm-plex-mono", IBM_plex_mono);
    ("source-code-pro", Source_code_pro);
    ("space-mono", Space_mono);
    ("dm-sans", DM_sans);
    ("dm-serif-display", DM_serif_display);
    ("bebas-neue", Bebas_neue);
    ("barlow", Barlow);
    ("mulish", Mulish);
    ("josefin-sans", Josefin_sans);
  ]

let font_family_platform =
  [
    ("helvetica", Helvetica);
    ("helvetica-neue", Helvetica_neue);
    ("arial", Arial);
    ("verdana", Verdana);
    ("tahoma", Tahoma);
    ("trebuchet-ms", Trebuchet_ms);
    ("times-new-roman", Times_new_roman);
    ("times", Times);
    ("georgia", Georgia);
    ("cambria", Cambria);
    ("garamond", Garamond);
    ("courier-new", Courier_new);
    ("courier", Courier);
    ("lucida-console", Lucida_console);
    ("sf-pro", SF_pro);
    ("sf-pro-display", SF_pro_display);
    ("sf-pro-text", SF_pro_text);
    ("sf-mono", SF_mono);
    ("ny", NY);
    ("segoe-ui", Segoe_ui);
    ("segoe-ui-emoji", Segoe_ui_emoji);
    ("segoe-ui-symbol", Segoe_ui_symbol);
    ("apple-color-emoji", Apple_color_emoji);
    ("noto-color-emoji", Noto_color_emoji);
    ("android-emoji", Android_emoji);
    ("twemoji-mozilla", Twemoji_mozilla);
  ]

let font_family_developer =
  [
    ("menlo", Menlo);
    ("monaco", Monaco);
    ("consolas", Consolas);
    ("liberation-mono", Liberation_mono);
    ("sfmono-regular", SFMono_regular);
    ("cascadia-code", Cascadia_code);
    ("cascadia-mono", Cascadia_mono);
    ("victor-mono", Victor_mono);
    ("inconsolata", Inconsolata);
    ("hack", Hack);
  ]

let font_family_all_enums : (string * font_family) list =
  font_family_generic_css @ font_family_css_keywords @ font_family_popular_web
  @ font_family_platform @ font_family_developer

let font_family_lookup_key name =
  name |> String.lowercase_ascii |> String.map (function ' ' -> '-' | c -> c)

(* The unquoted single-word lookup matches generic family keywords; a quoted
   name is always a [<custom-ident>] by spec, so it preserves the user's intent
   even when its text matches a generic keyword ([font-family: "serif"] is a
   custom family named "serif", not the [serif] generic). Multi-word quoted
   names still match the platform-name table since those entries are not
   keywords. *)
let font_family_of_quoted_name name =
  match List.assoc_opt (font_family_lookup_key name) font_family_all_enums with
  | Some family when String.contains name ' ' -> family
  | _ -> Name name

let rec read_font_family_single t : font_family =
  let read_var t : font_family = Var (read_var read_font_family t) in
  (* CSS Fonts 4 sec. 2.1 / CSS Cascade 5 sec. 7.3: the CSS-wide keywords and
     the reserved [default] are excluded from [<custom-ident>], so none may
     appear as any word of an unquoted family name. *)
  let is_reserved_word word =
    List.mem
      (String.lowercase_ascii word)
      [ "inherit"; "initial"; "unset"; "revert"; "revert-layer"; "default" ]
  in
  (* Read unquoted multi-word font names, e.g., "arial rounded" *)
  let rec read_unquoted_name_words acc =
    let word = Cursor.ident ~keep_case:true t in
    if is_reserved_word word then
      Cursor.err_invalid t
        "font-family: reserved word cannot appear in an unquoted family name";
    let acc = word :: acc in
    Cursor.ws t;
    if Option.is_some (Cursor.peek_ident t) then read_unquoted_name_words acc
    else String.concat " " (List.rev acc)
  in
  let read_single_word t : font_family =
    (* For single-word names, try enum match first *)
    (Cursor.enum_or_calls "font-family" font_family_all_enums
       ~calls:[ ("var", read_var) ]
       ~default:(fun t ->
         let name = Cursor.ident ~keep_case:true t in
         (* CSS Fonts 4 sec. 2.1: [default] is reserved and is not a valid
            unquoted [<custom-ident>] family name; it must be quoted. *)
         if String.lowercase_ascii name = "default" then
           Cursor.err_invalid t
             "font-family: 'default' is reserved and must be quoted"
         else (Name name : font_family))
       t
      : font_family)
  in
  Cursor.ws t;
  match Cursor.string_opt t with
  | Some name -> font_family_of_quoted_name name
  | None when Cursor.looking_at_func "var" t -> read_var t
  | None when Option.is_some (Cursor.peek_ident t) ->
      (* Peek ahead to see if this is multi-word or single-word *)
      let is_multi_word =
        Cursor.lookahead
          (fun t ->
            let _ = Cursor.ident t in
            Cursor.ws t;
            Option.is_some (Cursor.peek_ident t))
          t
      in
      if is_multi_word then
        (* Multi-word unquoted name; [read_unquoted_name_words] rejects any
           reserved word in the sequence. *)
        Name (read_unquoted_name_words [])
      else
        (* Single word - try enum match *)
        read_single_word t
  | None -> Cursor.err t "expected font-family value"

and read_font_family t : font_family =
  (* CSS Cascade 5 sec. 7.3: a CSS-wide keyword ([inherit] / [initial] / [unset]
     / [revert] / [revert-layer]) must stand alone; mixed inside a
     [<custom-ident>#] list it makes the whole declaration invalid. *)
  let rec loop acc =
    Cursor.ws t;
    if Cursor.comma_opt t then (
      Cursor.ws t;
      loop (read_font_family_single t :: acc))
    else List.rev acc
  in
  let first = read_font_family_single t in
  let items = loop [ first ] in
  let is_css_wide = function
    | (Inherit : font_family) | Initial | Unset | Revert | Revert_layer -> true
    | _ -> false
  in
  match items with
  | [ x ] -> x
  | _ when List.exists is_css_wide items ->
      (* CSS Cascade 5 sec. 7.3: a CSS-wide keyword must be the sole value; in a
         [<family-name>#] list it makes the whole declaration invalid. *)
      Cursor.err_invalid t
        "font-family: a CSS-wide keyword cannot appear in a family list"
  | l -> List l

let read_shorthand_line_height_typed r : line_height =
  let before = Cursor.save r in
  match read_line_height r with
  | lh -> lh
  | exception Cursor.Parse_error _ ->
      Cursor.restore r before;
      Cursor.err_invalid r "invalid line-height in font shorthand"

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

(* A bare ident matching a generic family ([sans-serif], [ui-monospace], ...) is
   only valid inside a font-family list, so its presence proves the whole token
   stream is a font-family value (the same "the type is obviously correct"
   reasoning that folds a colour function in an opaque stream). *)
let components_have_generic_family components =
  List.exists
    (function
      | Component.Preserved { kind = Token.Ident name; _ } ->
          List.mem (String.lowercase_ascii name) generic_font_family_keywords
      | _ -> false)
    components

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

let font_shorthand_prefix_ident = function
  | Some
      ( "italic" | "oblique" | "normal" | "small-caps" | "bold" | "bolder"
      | "lighter" | "condensed" | "expanded" ) ->
      true
  | _ -> false

(* Keyword -> which prefix slot it fills in the [font] shorthand. [Normal] is
   the absence keyword (style / variant / weight / stretch each have their own
   [normal]); we accept it and move on. *)
type font_prefix_slot =
  | Style of font_style
  | Variant of font_variant_css21
  | Weight of font_weight
  | Stretch of font_stretch
  | No_op

let font_prefix_slot_of = function
  | "italic" -> Style Italic
  | "oblique" -> Style Oblique
  | "small-caps" -> Variant Small_caps
  | "bold" -> Weight Bold
  | "bolder" -> Weight Bolder
  | "lighter" -> Weight Lighter
  | "condensed" -> Stretch Condensed
  | "expanded" -> Stretch Expanded
  | "normal" | _ -> No_op

let assign_font_prefix_slot ~(style : font_style option ref)
    ~(variant : font_variant_css21 option ref)
    ~(weight : font_weight option ref) ~(stretch : font_stretch option ref) =
  function
  | Style s -> if !style = None then style := Some s
  | Variant v -> if !variant = None then variant := Some v
  | Weight w -> if !weight = None then weight := Some w
  | Stretch st -> if !stretch = None then stretch := Some st
  | No_op -> ()

let try_numeric_font_weight r (weight : font_weight option ref) =
  let before = Cursor.save r in
  match read_font_weight r with
  | w when !weight = None ->
      weight := Some w;
      true
  | _ ->
      Cursor.restore r before;
      false
  | exception Cursor.Parse_error _ ->
      Cursor.restore r before;
      false

let read_optional_line_height r =
  Cursor.ws r;
  match Cursor.peek_delim r with
  | Some '/' ->
      Cursor.skip r;
      Cursor.ws r;
      Some (read_shorthand_line_height_typed r)
  | _ -> None

(* Parse the [font] shorthand body: a keyword prefix loop fills style / variant
   / weight / stretch (a [normal] binds no slot), then the required [size
   [/<line-height>]? <family>+] tail. *)
let read_font_shorthand r : font_shorthand =
  let style : font_style option ref = ref Option.None in
  let variant : font_variant_css21 option ref = ref Option.None in
  let weight : font_weight option ref = ref Option.None in
  let stretch : font_stretch option ref = ref Option.None in
  let assign = assign_font_prefix_slot ~style ~variant ~weight ~stretch in
  let rec consume_prefix () =
    Cursor.ws r;
    if Cursor.is_done r then ()
    else if font_shorthand_prefix_ident (Cursor.peek_ident r) then (
      assign (font_prefix_slot_of (Cursor.ident r));
      consume_prefix ())
    else if try_numeric_font_weight r weight then consume_prefix ()
  in
  consume_prefix ();
  Cursor.ws r;
  let size = read_font_size r in
  let line_height = read_optional_line_height r in
  if long_generic_family_start r then
    Cursor.err_invalid r "generic font family must be a standalone family item";
  let family = read_font_family r in
  Cursor.ws r;
  Cursor.expect_eof r;
  {
    style = !style;
    variant = !variant;
    weight = !weight;
    stretch = !stretch;
    size;
    line_height;
    family;
  }

let rec read_font t : font =
  let raw = Cursor.consume_to_decl_end ~trim:true t in
  let lower = String.lowercase_ascii (String.trim raw) in
  match lower with
  | "inherit" -> Inherit
  | "initial" -> Initial
  | "unset" -> Unset
  | "revert" -> Revert
  | "revert-layer" -> Revert_layer
  | "caption" -> Caption
  | "icon" -> Icon
  | "menu" -> Menu
  | "message-box" -> Message_box
  | "small-caption" -> Small_caption
  | "status-bar" -> Status_bar
  | _ ->
      let is_valid_var () =
        let r = Cursor.of_string raw in
        match Values.read_var (fun r -> read_font r) r with
        | (_ : font var) ->
            Cursor.ws r;
            Cursor.is_done r
        | exception Cursor.Parse_error _ -> false
      in
      if is_valid_var () then
        let r = Cursor.of_string raw in
        Var (Values.read_var (fun r -> read_font r) r)
      else if value_has_css_wide_mix raw then
        Cursor.err_invalid t "CSS-wide keyword mixed with other values"
      else
        let body =
          try read_font_shorthand (Cursor.of_string raw)
          with Cursor.Parse_error _ ->
            Cursor.err_invalid t "invalid font shorthand"
        in
        Shorthand body

let rec read_font_stretch t : font_stretch =
  let read_percentage t : font_stretch =
    let n = Cursor.pct t in
    (* CSS Fonts 4 sec. 6.1.2: font-stretch percentage is non-negative. *)
    if n < 0. then err_invalid_value t "font-stretch" (string_of_float n);
    Pct n
  in
  Cursor.enum_or_var "font-stretch"
    [
      ("ultra-condensed", Ultra_condensed);
      ("extra-condensed", Extra_condensed);
      ("condensed", Condensed);
      ("semi-condensed", Semi_condensed);
      ("normal", Normal);
      ("semi-expanded", Semi_expanded);
      ("expanded", Expanded);
      ("extra-expanded", Extra_expanded);
      ("ultra-expanded", Ultra_expanded);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_stretch t))
    ~default:read_percentage t

let read_font_display t : font_display =
  Cursor.enum "font-display"
    [
      ("auto", (Auto : font_display));
      ("block", Block);
      ("swap", Swap);
      ("fallback", Fallback);
      ("optional", Optional);
    ]
    t

let read_unicode_single start_value width =
  if width > String.length (hex_string start_value) then
    (Padded_single (start_value, width) : unicode_range)
  else Single start_value

let read_unicode_range_pair start_value end_value start_width end_width =
  if
    start_width > String.length (hex_string start_value)
    || end_width > String.length (hex_string end_value)
  then
    Padded_range
      { start = start_value; end_ = end_value; start_width; end_width }
  else Range (start_value, end_value)

let read_unicode_wildcard start_value prefix_width wildcards =
  let prefix = start_value lsr (4 * wildcards) in
  Wildcard { prefix; prefix_width; wildcards }

let read_unicode_token_form start_value end_value = function
  | Token.Single { width } -> read_unicode_single start_value width
  | Token.Range { start_width; end_width } ->
      read_unicode_range_pair start_value end_value start_width end_width
  | Token.Wildcard { prefix_width; wildcards } ->
      read_unicode_wildcard start_value prefix_width wildcards

let read_unicode_token t start_value end_value form =
  Cursor.skip t;
  if start_value > end_value then
    Cursor.err_invalid t "unicode range: start > end";
  if end_value > 0x10FFFF then
    Cursor.err_invalid t "unicode range: code point out of range";
  read_unicode_token_form start_value end_value form

let rec read_unicode_range t : unicode_range =
  (* The lexer emits a single [Unicode_range] token for [U+...] forms (CSS
     Syntax section 4.3.14); we just translate it to the [unicode_range] ADT. *)
  Cursor.with_context t "unicode-range" @@ fun () ->
  match Cursor.peek t with
  | Some (Component.Func { node = { name = "var"; _ }; _ }) ->
      (Var (Values.read_var read_unicode_range t) : unicode_range)
  | Some
      (Component.Preserved
         { kind = Token.Unicode_range { start_value; end_value; form }; _ }) ->
      read_unicode_token t start_value end_value form
  | _ -> Cursor.err_expected t "unicode-range"

let rec read_font_variant_numeric_token t : font_variant_numeric_token =
  let read_var t : font_variant_numeric_token =
    Var (read_var read_font_variant_numeric_token t)
  in
  Cursor.enum_or_var "font-variant-numeric-token"
    [
      ("normal", (Normal : font_variant_numeric_token));
      ("lining-nums", Lining_nums);
      ("oldstyle-nums", Oldstyle_nums);
      ("proportional-nums", Proportional_nums);
      ("tabular-nums", Tabular_nums);
      ("diagonal-fractions", Diagonal_fractions);
      ("stacked-fractions", Stacked_fractions);
      ("ordinal", Ordinal);
      ("slashed-zero", Slashed_zero);
    ]
    ~var:read_var t

let font_variant_numeric_token_family : font_variant_numeric_token -> _ =
  function
  | Lining_nums | Oldstyle_nums -> `Numeric_figure
  | Proportional_nums | Tabular_nums -> `Numeric_spacing
  | Diagonal_fractions | Stacked_fractions -> `Numeric_fraction
  | Ordinal -> `Ordinal
  | Slashed_zero -> `Slashed_zero
  | Normal | Var _ -> `Other

let numeric_token_is_normal : font_variant_numeric_token -> bool = function
  | Normal -> true
  | _ -> false

let reject_duplicate_numeric_families t
    (tokens : font_variant_numeric_token list) =
  let seen = Hashtbl.create 5 in
  List.iter
    (fun token ->
      match font_variant_numeric_token_family token with
      | `Other -> ()
      | family ->
          if Hashtbl.mem seen family then
            err_invalid_value t "font-variant-numeric" "duplicate token";
          Hashtbl.add seen family ())
    tokens

let read_font_variant_numeric_tokens t : font_variant_numeric =
  let tokens, _ = Cursor.many read_font_variant_numeric_token t in
  match tokens with
  | [] -> err_invalid_value t "font-variant-numeric" "<empty>"
  | tokens ->
      (* CSS Fonts 4 section 6.6: [normal] resets all sub-properties and must
         stand alone; it can't be mixed with other numeric tokens. *)
      if List.exists numeric_token_is_normal tokens && List.length tokens > 1
      then
        err_invalid_value t "font-variant-numeric"
          "[normal] cannot be mixed with other tokens";
      reject_duplicate_numeric_families t tokens;
      Tokens tokens

let rec read_font_variant_numeric t : font_variant_numeric =
  Cursor.enum_or_var "font-variant-numeric"
    [
      ("normal", (Normal : font_variant_numeric));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_variant_numeric t))
    ~default:read_font_variant_numeric_tokens t

let rec read_font_feature_settings t : font_feature_settings =
  let read_var t : font_feature_settings =
    Var (read_var read_font_feature_settings t)
  in
  let read_feature t =
    let tag_content = Cursor.string t in
    if String.length tag_content <> 4 then
      Cursor.err t "font-feature-settings tag must be exactly 4 characters";
    let tag = "\"" ^ tag_content ^ "\"" in
    Cursor.ws t;
    match Cursor.option Cursor.number t with
    | Some n ->
        if n <> 0.0 && n <> 1.0 then
          Cursor.err t "font-feature-settings value must be 0 or 1";
        tag ^ " " ^ string_of_int (int_of_float n)
    | None -> (
        match Cursor.option Cursor.ident t with
        | Some "on" -> tag ^ " on"
        | Some "off" -> tag ^ " off"
        | Some _ -> Cursor.err t "font-feature-settings value must be on/off"
        | None -> tag)
  in
  let read_feature_list t =
    let items = Cursor.list ~sep:Cursor.comma ~at_least:1 read_feature t in
    Feature_list (String.concat ", " items)
  in
  Cursor.enum_or_calls "font-feature-settings"
    [
      ("normal", (Normal : font_feature_settings));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:read_feature_list t

let rec read_font_variation_settings t : font_variation_settings =
  let read_var t : font_variation_settings =
    Var (read_var read_font_variation_settings t)
  in
  let read_axis t =
    let tag_content = Cursor.string t in
    if String.length tag_content <> 4 then
      Cursor.err t
        "font-variation-settings axis tag must be exactly 4 characters";
    String.iter
      (fun c ->
        let code = Char.code c in
        if code < 0x20 || code > 0x7E then
          Cursor.err t
            "font-variation-settings axis tag must contain only ASCII \
             characters (U+20 - U+7E)")
      tag_content;
    let tag = "\"" ^ tag_content ^ "\"" in
    Cursor.ws t;
    let value = Cursor.number t in
    tag ^ " " ^ string_of_int (int_of_float value)
  in
  let read_axis_list t =
    let items = Cursor.list ~sep:Cursor.comma ~at_least:1 read_axis t in
    Axis_list (String.concat ", " items)
  in
  Cursor.enum_or_calls "font-variation-settings"
    [
      ("normal", (Normal : font_variation_settings));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:read_axis_list t

let read_steps_direction t : steps_direction =
  Cursor.enum "steps direction"
    [
      ("jump-start", Jump_start);
      ("jump-end", Jump_end);
      ("jump-none", Jump_none);
      ("jump-both", Jump_both);
      ("start", Start);
      ("end", End);
    ]
    t

module Timing_function = struct
  let css_wide =
    [
      ("inherit", (Inherit : timing_function));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]

  let read_linear_function t : timing_function =
    Cursor.call "linear" t (fun t ->
        let body = Cursor.consume_remaining_as_string ~trim:true t in
        if body = "" then Cursor.err t "linear() requires at least one stop";
        Linear_function body)

  let read_steps t : timing_function =
    Cursor.call "steps" t (fun t ->
        let n = int_of_float (Cursor.number t) in
        if n <= 0 then Cursor.err t "steps() requires a positive step count";
        let kind =
          Cursor.option
            (fun t ->
              Cursor.comma t;
              read_steps_direction t)
            t
        in
        Steps (n, kind))

  let read_cubic_bezier t : timing_function =
    Cursor.call "cubic-bezier" t (fun t ->
        let a = Cursor.number t in
        Cursor.comma t;
        Cursor.ws t;
        let b = Cursor.number t in
        Cursor.comma t;
        Cursor.ws t;
        let c = Cursor.number t in
        Cursor.comma t;
        Cursor.ws t;
        let d = Cursor.number t in
        Cubic_bezier (a, b, c, d))

  let rec read t : timing_function =
    let read_var_timing t : timing_function = Var (Values.read_var read t) in
    Cursor.enum_or_calls "timing-function"
      ([
         ("ease", (Ease : timing_function));
         ("linear", Linear);
         ("ease-in", Ease_in);
         ("ease-out", Ease_out);
         ("ease-in-out", Ease_in_out);
         ("step-start", Step_start);
         ("step-end", Step_end);
       ]
      @ css_wide)
      ~calls:
        [
          ("linear", read_linear_function);
          ("steps", read_steps);
          ("cubic-bezier", read_cubic_bezier);
          ("var", read_var_timing);
        ]
      t
end

let read_timing_function t : timing_function = Timing_function.read t

let read_timing_function_list t =
  match Cursor.list ~sep:Cursor.comma ~at_least:1 read_timing_function t with
  | [ value ] -> value
  | values -> Timing_functions values

let read_duration_list (read_one : Cursor.t -> Values.duration) t :
    Values.duration =
  match Cursor.list ~sep:Cursor.comma ~at_least:1 read_one t with
  | [ value ] -> value
  | values -> Durations values

let rec read_transition_property_value t : transition_property_value =
  let read_var t : transition_property_value =
    Var (read_var read_transition_property_value t)
  in
  let read_property_ident t =
    let name = Cursor.ident ~keep_case:true t in
    (* CSS Transitions 1 section 2.1: [transition-property] is a
       [<custom-ident>], so it excludes keywords reserved for other slots of the
       [transition] shorthand. [normal] and [allow-discrete] are the
       [transition-behavior] enum and would silently absorb a duplicate (e.g.
       [transition: normal normal]) into the property slot. *)
    if List.mem (String.lowercase_ascii name) [ "normal"; "allow-discrete" ]
    then
      Cursor.err_invalid t
        ("transition-property cannot be reserved keyword: " ^ name)
    else Property name
  in
  Cursor.enum_or_calls "transition-property-value"
    [
      ("all", (All : transition_property_value));
      ("none", (None : transition_property_value));
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:read_property_ident t

let read_transition_property t : transition_property =
  (* Parse comma-separated list of transition properties *)
  let rec loop acc =
    let v = read_transition_property_value t in
    Cursor.ws t;
    if Cursor.comma_opt t then loop (v :: acc) else List.rev (v :: acc)
  in
  let values = loop [] in
  let singleton_only : transition_property_value -> bool = function
    | All | None | Initial | Inherit | Unset | Revert | Revert_layer -> true
    | Property _ | Var _ -> false
  in
  if List.length values > 1 && List.exists singleton_only values then
    Cursor.err_invalid t "transition-property singleton value in list";
  values

let rec read_transition_behavior t : transition_behavior =
  Cursor.enum_or_var "transition-behavior"
    [
      ("normal", (Normal : transition_behavior));
      ("allow-discrete", (Allow_discrete : transition_behavior));
      ("inherit", (Inherit : transition_behavior));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_transition_behavior t))
    t

let rec read_overlay t : overlay =
  Cursor.enum_or_var "overlay"
    [
      ("auto", (Auto : overlay));
      ("none", None);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_overlay t) : overlay))
    t

type transition_parts = {
  mutable property : transition_property_value option;
  mutable times : duration list;
  mutable timing : timing_function option;
  mutable behavior : transition_behavior option;
}

let read_transition_part t read set =
  let snap = Cursor.save t in
  try
    set (read t);
    true
  with Cursor.Parse_error _ ->
    Cursor.restore t snap;
    false

let transition_property_start t =
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Ident _; _ })
  | Some (Component.Func { node = { name = "var"; _ }; _ }) ->
      true
  | _ -> false

let read_transition_property_part parts t =
  if Option.is_some parts.property || not (transition_property_start t) then
    false
  else
    read_transition_part t read_transition_property_value (fun v ->
        parts.property <- Option.Some v)

let read_transition_var_property_part parts t =
  if Option.is_some parts.property || not (Cursor.looking_at_func "var" t) then
    false
  else read_transition_property_part parts t

let read_transition_timing_part parts t =
  if Option.is_some parts.timing then false
  else
    read_transition_part t read_timing_function (fun v ->
        parts.timing <- Option.Some v)

let read_transition_behavior_part parts t =
  if Option.is_some parts.behavior then false
  else
    read_transition_part t read_transition_behavior (fun v ->
        parts.behavior <- Option.Some v)

let read_transition_time_part parts t =
  if List.length parts.times >= 2 then false
  else
    read_transition_part t read_duration (fun v ->
        parts.times <- v :: parts.times)

let transition_duration_delay parts =
  match List.rev parts.times with
  | [] -> (Option.None, Option.None)
  | [ d ] -> (Option.Some d, Option.None)
  | d :: l :: _ -> (Option.Some d, Option.Some l)

let read_transition_next_part parts t =
  read_transition_var_property_part parts t
  || read_transition_time_part parts t
  || read_transition_timing_part parts t
  || read_transition_behavior_part parts t
  || read_transition_property_part parts t

let transition_property_or_all = function
  | Option.Some property -> property
  | Option.None -> (All : transition_property_value)

let read_transition_shorthand t : transition_shorthand =
  let parts =
    {
      property = Option.None;
      times = [];
      timing = Option.None;
      behavior = Option.None;
    }
  in
  let consumed = ref true in
  while !consumed do
    Cursor.ws t;
    consumed := read_transition_next_part parts t
  done;
  let property = transition_property_or_all parts.property in
  let duration, delay = transition_duration_delay parts in
  {
    property;
    duration;
    timing_function = parts.timing;
    delay;
    behavior = parts.behavior;
  }

let rec read_transition t : transition =
  Cursor.enum "transition"
    [
      ("inherit", (Inherit : transition));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
      ("none", None);
    ]
    ~default:(fun t : transition ->
      if Cursor.looking_at_func "var" t then (
        let snap = Cursor.save t in
        let value = (Var (read_var read_transition t) : transition) in
        Cursor.ws t;
        if Cursor.is_done t then value
        else (
          Cursor.restore t snap;
          Shorthand (read_transition_shorthand t)))
      else Shorthand (read_transition_shorthand t))
    t

let read_transitions t : transition list =
  Cursor.list ~at_least:1 ~sep:Cursor.comma read_transition t

let rec read_animation_direction t : animation_direction =
  let read_one t =
    Cursor.enum "animation-direction-item"
      [
        ("normal", (Normal : animation_direction));
        ("reverse", Reverse);
        ("alternate", Alternate);
        ("alternate-reverse", Alternate_reverse);
      ]
      t
  in
  Cursor.enum_or_var "animation-direction"
    [
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_animation_direction t))
    ~default:(fun t ->
      match Cursor.list ~sep:Cursor.comma ~at_least:1 read_one t with
      | [ value ] -> value
      | values -> Directions values)
    t

let rec read_animation_fill_mode t : animation_fill_mode =
  let read_one t =
    Cursor.enum "animation-fill-mode-item"
      [
        ("none", (None : animation_fill_mode));
        ("forwards", Forwards);
        ("backwards", Backwards);
        ("both", Both);
      ]
      t
  in
  Cursor.enum_or_var "animation-fill-mode"
    [
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_animation_fill_mode t))
    ~default:(fun t ->
      match Cursor.list ~sep:Cursor.comma ~at_least:1 read_one t with
      | [ value ] -> value
      | values -> Fill_modes values)
    t

let read_animation_count_number t =
  let n, unit = Cursor.number_with_unit t in
  match unit with
  | Some u ->
      Cursor.err_invalid t
        ("animation-iteration-count must be unitless, got: " ^ u)
  | None ->
      if n < 0. then
        Cursor.err_invalid t "animation-iteration-count cannot be negative";
      Num n

let read_animation_count_item t =
  Cursor.enum "animation-iteration-count-item"
    [ ("infinite", (Infinite : animation_iteration_count)) ]
    ~default:read_animation_count_number t

let read_animation_counts t =
  match
    Cursor.list ~sep:Cursor.comma ~at_least:1 read_animation_count_item t
  with
  | [ count ] -> count
  | counts -> Counts counts

let rec read_animation_iteration_count t : animation_iteration_count =
  Cursor.enum_or_var "animation-iteration-count"
    [
      ("initial", (Initial : animation_iteration_count));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_animation_iteration_count t))
    ~default:read_animation_counts t

type animation_reserved_string_name =
  | None
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Default

let animation_reserved_string_name = function
  | "none" -> Some (None : animation_reserved_string_name)
  | "initial" -> Some Initial
  | "inherit" -> Some Inherit
  | "unset" -> Some Unset
  | "revert" -> Some Revert
  | "revert-layer" -> Some Revert_layer
  | "default" -> Some Default
  | _ -> None

type animation_shorthand_kind =
  | Timing
  | Iteration
  | Direction
  | Fill
  | Play
  | Timeline

let animation_shorthand_kind = function
  | "ease" | "linear" | "ease-in" | "ease-out" | "ease-in-out" | "step-start"
  | "step-end" ->
      Some Timing
  | "infinite" -> Some Iteration
  | "normal" | "reverse" | "alternate" | "alternate-reverse" -> Some Direction
  | "none" | "forwards" | "backwards" | "both" -> Some Fill
  | "running" | "paused" -> Some Play
  | "auto" -> Some Timeline
  | _ -> None

let animation_quoted_or_name s =
  match animation_reserved_string_name (String.lowercase_ascii s) with
  | Some _ -> (Quoted s : animation_name)
  | None -> (
      match animation_shorthand_kind (String.lowercase_ascii s) with
      | Some _ -> Ambiguous s
      | None -> Name s)

let rec read_animation_name t : animation_name =
  let read_item t =
    Cursor.enum "animation-name-item"
      [ ("none", (None : animation_name)) ]
      ~default:(fun t ->
        match Cursor.string_opt t with
        | Some s -> (animation_quoted_or_name s : animation_name)
        | None -> Name (Cursor.ident t))
      t
  in
  Cursor.enum_or_var "animation-name"
    [
      ("initial", (Initial : animation_name));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_animation_name t))
    ~default:(fun t ->
      let names = Cursor.list ~sep:Cursor.comma ~at_least:1 read_item t in
      match names with [ name ] -> name | names -> Names names)
    t

let rec read_animation_play_state t : animation_play_state =
  let read_state t =
    Cursor.enum "animation-play-state-item"
      [ ("running", (Running : animation_play_state)); ("paused", Paused) ]
      t
  in
  Cursor.enum_or_var "animation-play-state"
    [
      ("initial", (Initial : animation_play_state));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_animation_play_state t))
    ~default:(fun t ->
      let states = Cursor.list ~sep:Cursor.comma ~at_least:1 read_state t in
      match states with [ state ] -> state | states -> States states)
    t

let read_animation_composition_item t =
  Cursor.enum "animation-composition-item"
    [ ("replace", Replace); ("add", Add); ("accumulate", Accumulate) ]
    t

let rec read_animation_composition t : animation_composition =
  Cursor.enum_or_var "animation-composition"
    [
      ("initial", (Initial : animation_composition));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_animation_composition t))
    ~default:(fun t ->
      Compositions
        (Cursor.list ~sep:Cursor.comma ~at_least:1
           read_animation_composition_item t))
    t

module Animation = struct
  type component =
    | Name of animation_name option
    | Duration of duration
    | Timing_function of timing_function
    | Iteration_count of animation_iteration_count
    | Direction of animation_direction
    | Fill_mode of animation_fill_mode
    | Play_state of animation_play_state
    | Timeline of animation_timeline

  let timing_name : timing_function -> string option = function
    | Ease -> Some "ease"
    | Linear -> Some "linear"
    | Ease_in -> Some "ease-in"
    | Ease_out -> Some "ease-out"
    | Ease_in_out -> Some "ease-in-out"
    | Step_start -> Some "step-start"
    | Step_end -> Some "step-end"
    | _ -> None

  let direction_name : animation_direction -> string option = function
    | Normal -> Some "normal"
    | Reverse -> Some "reverse"
    | Alternate -> Some "alternate"
    | Alternate_reverse -> Some "alternate-reverse"
    | _ -> None

  let fill_name : animation_fill_mode -> animation_name option = function
    | None -> Some (None : animation_name)
    | Forwards -> Some (Ambiguous "forwards")
    | Backwards -> Some (Ambiguous "backwards")
    | Both -> Some (Ambiguous "both")
    | _ -> None

  let play_name : animation_play_state -> string option = function
    | Running -> Some "running"
    | Paused -> Some "paused"
    | _ -> None

  let name : animation_timeline -> string option = function
    | Auto -> Some "auto"
    | _ -> None

  let set_name name_seen (acc : animation_shorthand) (name : animation_name) =
    name_seen := true;
    { acc with name = Some name }

  let set_string_name t label name_seen acc = function
    | Some name when not !name_seen -> set_name name_seen acc (Ambiguous name)
    | _ -> Cursor.err t ("duplicate " ^ label)

  let set_animation_name t label name_seen acc = function
    | Some name when not !name_seen -> set_name name_seen acc name
    | _ -> Cursor.err t ("duplicate " ^ label)

  type read_state = {
    duration_count : int ref;
    name_seen : bool ref;
    timing_seen : bool ref;
    iteration_seen : bool ref;
    direction_seen : bool ref;
    fill_seen : bool ref;
    play_seen : bool ref;
    timeline_seen : bool ref;
    component_seen : bool ref;
  }

  let read_state () =
    {
      duration_count = ref 0;
      name_seen = ref false;
      timing_seen = ref false;
      iteration_seen = ref false;
      direction_seen = ref false;
      fill_seen = ref false;
      play_seen = ref false;
      timeline_seen = ref false;
      component_seen = ref false;
    }

  let default_shorthand =
    {
      name = None;
      (* CSS default: none *)
      duration = Some (S 0.0);
      (* CSS default: 0s *)
      timing_function = Some Ease;
      (* CSS default: ease *)
      delay = Some (S 0.0);
      (* CSS default: 0s *)
      iteration_count = Some (Num 1.0);
      (* CSS default: 1 *)
      direction = Some Normal;
      (* CSS default: normal *)
      fill_mode = Some None;
      (* CSS default: none *)
      play_state = Some Running;
      (* CSS default: running *)
      timeline = Some Auto;
      (* CSS default: auto *)
    }

  let apply_name t state (acc : animation_shorthand)
      (name : animation_name option) =
    if !(state.name_seen) then Cursor.err t "duplicate animation-name";
    state.name_seen := true;
    { acc with name }

  let apply_duration t state acc d =
    (* CSS spec: First time value is duration, second is delay *)
    incr state.duration_count;
    if !(state.duration_count) > 2 then
      Cursor.err t "animation shorthand cannot have more than two time values";
    if !(state.duration_count) = 1 then { acc with duration = Some d }
    else { acc with delay = Some d }

  let apply_timing t state acc tf =
    if !(state.timing_seen) then
      set_string_name t "animation-timing-function" state.name_seen acc
        (timing_name tf)
    else (
      state.timing_seen := true;
      { acc with timing_function = Some tf })

  let apply_iteration t state acc ic =
    if !(state.iteration_seen) then
      (* CSS Animations 1 section 8.5: [<single-animation-iteration-count>] is
         [infinite | <number>]; [infinite] is a reserved keyword that cannot be
         a [<custom-ident>] animation name. Reject the duplicate rather than
         coercing it into the name slot. *)
      Cursor.err t "duplicate animation-iteration-count";
    state.iteration_seen := true;
    { acc with iteration_count = Some ic }

  let apply_direction t state acc dir =
    if !(state.direction_seen) then
      set_string_name t "animation-direction" state.name_seen acc
        (direction_name dir)
    else (
      state.direction_seen := true;
      { acc with direction = Some dir })

  let apply_fill t state acc fm =
    if !(state.fill_seen) then
      set_animation_name t "animation-fill-mode" state.name_seen acc
        (fill_name fm)
    else (
      state.fill_seen := true;
      { acc with fill_mode = Some fm })

  let apply_play t state acc ps =
    if !(state.play_seen) then
      set_string_name t "animation-play-state" state.name_seen acc
        (play_name ps)
    else (
      state.play_seen := true;
      { acc with play_state = Some ps })

  let apply_timeline t state acc tl =
    if !(state.timeline_seen) then
      set_string_name t "animation-timeline" state.name_seen acc (name tl)
    else (
      state.timeline_seen := true;
      { acc with timeline = Some tl })

  let apply_component t state (acc : animation_shorthand) component =
    state.component_seen := true;
    match component with
    | Name name -> apply_name t state acc name
    | Duration d -> apply_duration t state acc d
    | Timing_function tf -> apply_timing t state acc tf
    | Iteration_count ic -> apply_iteration t state acc ic
    | Direction dir -> apply_direction t state acc dir
    | Fill_mode fm -> apply_fill t state acc fm
    | Play_state ps -> apply_play t state acc ps
    | Timeline tl -> apply_timeline t state acc tl

  let read_component t =
    let read_duration t = Duration (read_duration t) in
    let read_timing t = Timing_function (read_timing_function t) in
    let read_iteration t = Iteration_count (read_animation_count_item t) in
    let read_direction t =
      match Option.map String.lowercase_ascii (Cursor.ident_opt t) with
      | Some "normal" -> Direction (Normal : animation_direction)
      | Some "reverse" -> Direction Reverse
      | Some "alternate" -> Direction Alternate
      | Some "alternate-reverse" -> Direction Alternate_reverse
      | Some s -> Cursor.err t ("unknown animation-direction-item: " ^ s)
      | None -> Cursor.err_expected t "animation-direction-item"
    in
    let read_fill t =
      match Option.map String.lowercase_ascii (Cursor.ident_opt t) with
      | Some "none" -> Fill_mode (None : animation_fill_mode)
      | Some "forwards" -> Fill_mode Forwards
      | Some "backwards" -> Fill_mode Backwards
      | Some "both" -> Fill_mode Both
      | Some s -> Cursor.err t ("unknown animation-fill-mode-item: " ^ s)
      | None -> Cursor.err_expected t "animation-fill-mode-item"
    in
    let read_play t =
      match Option.map String.lowercase_ascii (Cursor.ident_opt t) with
      | Some "running" -> Play_state (Running : animation_play_state)
      | Some "paused" -> Play_state Paused
      | Some s -> Cursor.err t ("unknown animation-play-state-item: " ^ s)
      | None -> Cursor.err_expected t "animation-play-state-item"
    in
    let read_timeline t = Timeline (read_animation_timeline t) in
    let read_var_name t =
      Name (Some (Var (Values.read_var read_animation_name t)))
    in
    let read_string_name t =
      match Cursor.string_opt t with
      | Some s -> Name (Some (animation_quoted_or_name s))
      | None -> Cursor.err t "expected animation-name string"
    in
    let read_name t =
      let v = Cursor.ident t in
      if Option.is_some (animation_shorthand_kind (String.lowercase_ascii v))
      then
        (* This identifier is for another property, not animation-name *)
        Cursor.err t
          ("'" ^ v ^ "' is a reserved keyword for animation properties")
      else Name (Some (Name v))
    in
    Cursor.one_of
      [
        read_duration;
        read_timing;
        read_iteration;
        read_direction;
        read_fill;
        read_play;
        read_timeline;
        read_var_name;
        read_string_name;
        (* Animation name - parse this LAST since it accepts any non-reserved
           identifier *)
        read_name;
      ]
      t

  let read_shorthand t =
    let state = read_state () in
    let acc, _ =
      Cursor.fold_many read_component ~init:default_shorthand
        ~f:(apply_component t state) t
    in
    (* CSS spec: All components are optional *)
    if not !(state.component_seen) then
      Cursor.err t "animation shorthand requires at least one component"
    else acc

  let is_zero_duration = function S 0. | Ms 0. -> true | _ -> false
  let pp_iter_count = pp_animation_iteration_count

  (* Check if a timing function prints with a trailing ')'. Some function values
     canonicalise to keyword aliases in minified output. *)
  let rec ends_with_paren ctx = function
    | Cubic_bezier (0.25, 0.1, 0.25, 1.0)
    | Cubic_bezier (0.42, 0.0, 1.0, 1.0)
    | Cubic_bezier (0.0, 0.0, 0.58, 1.0)
    | Cubic_bezier (0.42, 0.0, 0.58, 1.0)
      when Pp.minified ctx ->
        false
    | (Steps (1, Some (Jump_start | Start)) | Steps (1, Some (Jump_end | End)))
      when Pp.minified ctx ->
        (* These fold to the [step-start] / [step-end] keywords in
           [pp_timing_function], so the rendered output is keyword-shaped, no
           closing paren. *)
        false
    | Cubic_bezier _ | Steps _ | Linear_function _ | Var _ -> true
    | Timing_functions [] -> false
    | Timing_functions values -> ends_with_paren ctx (List.hd (List.rev values))
    | Inherit | Initial | Unset | Revert | Revert_layer -> false
    | Linear | Ease | Ease_in | Ease_out | Ease_in_out | Step_start | Step_end
      ->
        false

  let pp_timing = pp_timing_function

  let is_duration : duration option -> bool = function
    | Some d when not (is_zero_duration d) -> true
    | _ -> false

  let is_default_timing ctx = function
    | Ease -> true
    | Cubic_bezier (0.25, 0.1, 0.25, 1.0) when Pp.minified ctx -> true
    | _ -> false

  let is_timing ctx : timing_function option -> bool = function
    | Some Ease | None -> false
    | Some tf -> not (is_default_timing ctx tf)

  let is_iteration : animation_iteration_count option -> bool = function
    | Some (Num 1.) | None -> false
    | Some _ -> true

  let is_direction : animation_direction option -> bool = function
    | Some Normal | None -> false
    | Some _ -> true

  let is_fill_mode : animation_fill_mode option -> bool = function
    | Some None | None -> false
    | Some _ -> true

  let is_play_state : animation_play_state option -> bool = function
    | Some Running | None -> false
    | Some _ -> true

  let is_timeline : animation_timeline option -> bool = function
    | Some Auto | None -> false
    | Some _ -> true

  let has_non_defaults ctx (anim : animation_shorthand) =
    is_duration anim.duration
    || is_timing ctx anim.timing_function
    || is_duration anim.delay
    || is_iteration anim.iteration_count
    || is_direction anim.direction
    || is_fill_mode anim.fill_mode
    || is_play_state anim.play_state
    || is_timeline anim.timeline

  let ambiguous_name_kind (anim : animation_shorthand) =
    match anim.name with
    | Some (Ambiguous name) ->
        animation_shorthand_kind (String.lowercase_ascii name)
    | _ -> None

  (* If the caller will quote an ambiguous animation name, the placeholder slot
     used for unquoted disambiguation is no longer needed. *)
  let effective_ambiguous_kind ~quote_name anim =
    if quote_name then Option.None else ambiguous_name_kind anim

  (* When the colliding shorthand slot already carries an explicit non-default
     value, the bare-ident form of the ambiguous name is unambiguous on
     re-parse: the slot fills first, and the second occurrence falls through to
     the keyframes-name. In that case the printer can skip the quoting trick
     entirely. *)
  let bare_ambiguous_safe ctx (anim : animation_shorthand) =
    match ambiguous_name_kind anim with
    | None -> false
    | Some Timing -> (
        match anim.timing_function with
        | Some tf -> not (is_default_timing ctx tf)
        | None -> false)
    | Some Iteration -> is_iteration anim.iteration_count
    | Some Direction -> is_direction anim.direction
    | Some Fill -> is_fill_mode anim.fill_mode
    | Some Play -> is_play_state anim.play_state
    | Some Timeline -> is_timeline anim.timeline

  let duration (anim : animation_shorthand) : duration option =
    match (anim.duration, anim.delay) with
    | Some d, Some delay when is_zero_duration d && is_duration (Some delay) ->
        Some d
    | _ -> (
        match anim.duration with
        | Some d when not (is_zero_duration d) -> Some d
        | _ -> None)

  let timing ?(quote_name = false) ctx (anim : animation_shorthand) :
      timing_function option =
    match (anim.timing_function, effective_ambiguous_kind ~quote_name anim) with
    | (Some Ease | None), Some Timing -> Some Ease
    | Some tf, _ when is_default_timing ctx tf -> None
    | None, _ -> None
    | Some t, _ -> Some t

  let delay (anim : animation_shorthand) : duration option =
    match anim.delay with
    | Some d when not (is_zero_duration d) -> Some d
    | _ -> None

  let iteration ?(quote_name = false) (anim : animation_shorthand) :
      animation_iteration_count option =
    match (anim.iteration_count, effective_ambiguous_kind ~quote_name anim) with
    | (Some (Num 1.) | None), Some Iteration -> Some (Num 1.)
    | Some (Num 1.), _ | None, _ -> None
    | Some c, _ -> Some c

  let direction ?(quote_name = false) (anim : animation_shorthand) :
      animation_direction option =
    match (anim.direction, effective_ambiguous_kind ~quote_name anim) with
    | (Some Normal | None), Some Direction -> Some Normal
    | Some Normal, _ | None, _ -> None
    | Some d, _ -> Some d

  let fill_mode ?(quote_name = false) (anim : animation_shorthand) :
      animation_fill_mode option =
    match (anim.fill_mode, effective_ambiguous_kind ~quote_name anim) with
    | (Some None | None), Some Fill -> Some None
    | Some None, _ | None, _ -> None
    | Some m, _ -> Some m

  let play_state ?(quote_name = false) (anim : animation_shorthand) :
      animation_play_state option =
    match (anim.play_state, effective_ambiguous_kind ~quote_name anim) with
    | (Some Running | None), Some Play -> Some Running
    | Some Running, _ | None, _ -> None
    | Some s, _ -> Some s

  let timeline ?(quote_name = false) (anim : animation_shorthand) :
      animation_timeline option =
    match (anim.timeline, effective_ambiguous_kind ~quote_name anim) with
    | (Some Auto | None), Some Timeline -> Some Auto
    | Some Auto, _ | None, _ -> None
    | Some tl, _ -> Some tl
end

let read_animation_shorthand t : animation_shorthand =
  Animation.read_shorthand t

type animation_pp_state = {
  first : bool ref;
  prev_ends_with_paren : bool ref;
  prev_ends_with_quote : bool ref;
}

let animation_pp_state () =
  {
    first = ref true;
    prev_ends_with_paren = ref false;
    prev_ends_with_quote = ref false;
  }

let pp_animation_space_before state ?(ends_with_paren = false)
    ?(ends_with_quote = false) ?(starts_with_quote = false) pp ctx x =
  if !(state.first) then state.first := false
  else if
    Pp.minified ctx
    && (!(state.prev_ends_with_paren)
       || !(state.prev_ends_with_quote)
       || starts_with_quote)
  then
    (* Minified output drops the inter-token space when one side already carries
       a self-delimiting boundary - a closing function paren, a closing string
       quote, or an upcoming opening string quote. *)
    ()
  else Pp.char ctx ' ';
  state.prev_ends_with_paren := ends_with_paren;
  state.prev_ends_with_quote := ends_with_quote;
  pp ctx x

let animation_name_is_default_none ctx = function
  | Some (None : animation_name) -> true
  | Some (Quoted s) when Pp.minified ctx && String.lowercase_ascii s = "none" ->
      true
  | _ -> false

let animation_quote_ambiguous_name ctx anim =
  Pp.minified ctx
  && Animation.ambiguous_name_kind anim <> None
  && not (Animation.bare_ambiguous_safe ctx anim)

let pp_animation_initial_none ctx (anim : animation_shorthand)
    ~name_is_default_none ~has_any_non_default =
  match (anim.name, name_is_default_none, has_any_non_default) with
  | _, true, true -> ()
  | Option.None, _, false -> Pp.string ctx "none"
  | Option.None, _, true -> ()
  | Option.Some _, _, _ -> ()

let pp_animation_name_slot ctx state ~quote_ambiguous_name
    (anim : animation_shorthand) =
  if not (animation_name_is_default_none ctx anim.name) then
    Option.iter
      (fun (name : animation_name) ->
        match name with
        | Ambiguous s when quote_ambiguous_name ->
            pp_animation_space_before state ~starts_with_quote:true
              ~ends_with_quote:true Pp.quoted_string ctx s
        | Quoted s ->
            pp_animation_space_before state ~starts_with_quote:true
              ~ends_with_quote:true Pp.quoted_string ctx s
        | _ -> pp_animation_space_before state pp_animation_name ctx name)
      anim.name

let pp_animation_timing_slot ctx state ~quote_ambiguous_name anim =
  match Animation.timing ~quote_name:quote_ambiguous_name ctx anim with
  | Some tf ->
      let ends = Animation.ends_with_paren ctx tf in
      pp_animation_space_before state ~ends_with_paren:ends Animation.pp_timing
        ctx tf
  | None -> ()

let pp_animation_shorthand : animation_shorthand Pp.t =
 fun ctx anim ->
  let state = animation_pp_state () in
  let has_any_non_default = Animation.has_non_defaults ctx anim in
  let name_is_default_none = animation_name_is_default_none ctx anim.name in
  let quote_ambiguous_name = animation_quote_ambiguous_name ctx anim in
  pp_animation_initial_none ctx anim ~name_is_default_none ~has_any_non_default;
  (* Cascade canonical order puts the animation [name] first: it is the only
     ident-shaped component that survives the rest defaulting away, so leading
     with it makes "single-token" outputs ([animation:slide]) read naturally and
     matches the common minifier convention. *)
  pp_animation_name_slot ctx state ~quote_ambiguous_name anim;
  Pp.option
    (pp_animation_space_before state pp_duration)
    ctx (Animation.duration anim);
  pp_animation_timing_slot ctx state ~quote_ambiguous_name anim;
  Pp.option
    (pp_animation_space_before state pp_duration)
    ctx (Animation.delay anim);
  Pp.option
    (pp_animation_space_before state Animation.pp_iter_count)
    ctx
    (Animation.iteration ~quote_name:quote_ambiguous_name anim);
  Pp.option
    (pp_animation_space_before state pp_animation_direction)
    ctx
    (Animation.direction ~quote_name:quote_ambiguous_name anim);
  Pp.option
    (pp_animation_space_before state pp_animation_fill_mode)
    ctx
    (Animation.fill_mode ~quote_name:quote_ambiguous_name anim);
  Pp.option
    (pp_animation_space_before state pp_animation_play_state)
    ctx
    (Animation.play_state ~quote_name:quote_ambiguous_name anim);
  Pp.option
    (pp_animation_space_before state pp_animation_timeline)
    ctx
    (Animation.timeline ~quote_name:quote_ambiguous_name anim)

let rec pp_animation : animation Pp.t =
 fun ctx -> function
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | None -> Pp.string ctx "none"
  | Var v -> pp_var pp_animation ctx v
  | Shorthand s -> pp_animation_shorthand ctx s

let animation_global_value ident : animation option =
  match String.lowercase_ascii ident with
  | "inherit" -> Some Inherit
  | "initial" -> Some Initial
  | "none" -> Some None
  | _ -> None

let animation_value_boundary t =
  Cursor.ws t;
  Cursor.is_done t || Cursor.peek_comma t

let read_animation_shorthand_from t snap : animation =
  Cursor.restore t snap;
  Shorthand (read_animation_shorthand t)

let read_animation_global_or_shorthand t =
  let snap = Cursor.save t in
  match Cursor.ident_opt t with
  | Some ident -> (
      match animation_global_value ident with
      | Some value when animation_value_boundary t -> value
      | _ -> read_animation_shorthand_from t snap)
  | None -> read_animation_shorthand_from t snap

let read_animation_var_or_shorthand read_self t =
  let snap = Cursor.save t in
  let value = (Var (read_var read_self t) : animation) in
  if animation_value_boundary t then value
  else read_animation_shorthand_from t snap

let rec read_animation t : animation =
  if Cursor.looking_at_func "var" t then
    read_animation_var_or_shorthand read_animation t
  else read_animation_global_or_shorthand t

let read_animations t : animation list =
  Cursor.list ~at_least:1 ~sep:Cursor.comma read_animation t

module Text_shadow = struct
  type component = Color of color | Length of length

  let read_component t : component =
    Cursor.one_of
      [ (fun t -> Color (read_color t)); (fun t -> Length (read_length t)) ]
      t

  let fold_components components =
    let lengths =
      List.filter_map (function Length l -> Some l | _ -> None) components
    in
    let color =
      List.find_map (function Color c -> Some c | _ -> None) components
    in
    (lengths, color)
end

let rec read_text_shadow t : text_shadow =
  let read_var t : text_shadow = Var (read_var read_text_shadow t) in
  Cursor.enum_or_calls "text-shadow"
    [
      ("none", None);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:(fun t ->
      let components, _ = Cursor.many Text_shadow.read_component t in
      let lengths, color = Text_shadow.fold_components components in
      match lengths with
      | h :: v :: rest ->
          let blur = match rest with b :: _ -> Some b | _ -> None in
          (Text_shadow { h_offset = h; v_offset = v; blur; color }
            : text_shadow)
      | _ -> err_invalid_value t "text-shadow" "expected at least two lengths")
    t

let read_text_shadows t : text_shadow list =
  Cursor.list ~sep:Cursor.comma ~at_least:1 read_text_shadow t

module Filter = struct
  let read_brightness t : filter =
    Cursor.call "brightness" t (fun t ->
        Brightness (Values.read_number_percentage t))

  let read_contrast t : filter =
    Cursor.call "contrast" t (fun t ->
        Contrast (Values.read_number_percentage t))

  let read_grayscale t : filter =
    Cursor.call "grayscale" t (fun t : filter ->
        Grayscale (Values.read_number_percentage t))

  let read_hue_rotate t : filter =
    Cursor.call "hue-rotate" t (fun t ->
        if Cursor.is_done t then Hue_rotate (Deg 0.)
        else Hue_rotate (read_angle t))

  let read_invert t : filter =
    Cursor.call "invert" t (fun t -> Invert (Values.read_number_percentage t))

  let read_opacity t : filter =
    Cursor.call "opacity" t (fun t : filter ->
        Opacity (Values.read_number_percentage t))

  let read_saturate t : filter =
    Cursor.call "saturate" t (fun t ->
        Saturate (Values.read_number_percentage t))

  let read_sepia t : filter =
    Cursor.call "sepia" t (fun t -> Sepia (Values.read_number_percentage t))

  let read_drop_shadow t : filter =
    Cursor.call "drop-shadow" t (fun t ->
        let read_var t : filter = Drop_shadow (Var (read_var Shadow.read t)) in
        let read_shadow t : filter = Drop_shadow (Shadow.read t) in
        Cursor.one_of [ read_var; read_shadow ] t)
end

let rec read_filter_item t : filter =
  let read_var t : filter = Var (read_var read_filter t) in
  (* [<filter-value-list>] mixes filter functions with a bare [<url>] reference
     to an SVG filter. [url(#id)] tokenises as a url-token, not a [url(]
     function, so it is read via [Cursor.url] (which handles both that and the
     quoted [url("#id")] form) and backtracks to the function dispatch. *)
  let read_url t = (Url (Cursor.url t) : filter) in
  Cursor.one_of
    [
      read_url;
      (fun t ->
        Cursor.enum_or_calls "filter"
          [ ("none", (None : filter)) ]
          ~calls:
            [
              ("blur", read_blur);
              ("brightness", Filter.read_brightness);
              ("contrast", Filter.read_contrast);
              ("grayscale", Filter.read_grayscale);
              ("hue-rotate", Filter.read_hue_rotate);
              ("invert", Filter.read_invert);
              ("opacity", Filter.read_opacity);
              ("saturate", Filter.read_saturate);
              ("sepia", Filter.read_sepia);
              ("drop-shadow", Filter.read_drop_shadow);
              ("var", read_var);
            ]
          t);
    ]
    t

and read_filter t : filter =
  let read_filter_list t =
    let filters, _ = Cursor.many read_filter_item t in
    match filters with
    | [] -> err_invalid_value t "filter" "expected filter function(s)"
    | [ f ] -> f
    | fs
      when List.exists
             (fun (value : filter) ->
               match value with None -> true | _ -> false)
             fs ->
        err_invalid_value t "filter" "none cannot be combined"
    | fs -> List fs
  in
  Cursor.enum "filter"
    [
      ("none", (None : filter));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~default:read_filter_list t

let read_any_property t =
  (* CSS property names are case-insensitive per Syntax sec. 3.3. *)
  let prop_name = String.lowercase_ascii_preserve (Cursor.ident t) in
  (* PROPERTY_MATCHING_START - Used by scripts/check_properties.ml *)
  match prop_name with
  | "all" -> Prop All
  | "width" -> Prop Width
  | "height" -> Prop Height
  | "min-width" -> Prop Min_width
  | "min-height" -> Prop Min_height
  | "max-width" -> Prop Max_width
  | "max-height" -> Prop Max_height
  | "inline-size" -> Prop Inline_size
  | "min-inline-size" -> Prop Min_inline_size
  | "max-inline-size" -> Prop Max_inline_size
  | "block-size" -> Prop Block_size
  | "min-block-size" -> Prop Min_block_size
  | "max-block-size" -> Prop Max_block_size
  | "color" -> Prop Color
  | "background-color" -> Prop Background_color
  | "background" -> Prop Background (* Shorthand property *)
  | "background-image" -> Prop Background_image
  | "border-color" -> Prop Border_color
  | "border-top-color" -> Prop Border_top_color
  | "border-right-color" -> Prop Border_right_color
  | "border-bottom-color" -> Prop Border_bottom_color
  | "border-left-color" -> Prop Border_left_color
  | "border-inline-color" -> Prop Border_inline_color
  | "border-block-color" -> Prop Border_block_color
  | "border-inline-width" -> Prop Border_inline_width
  | "border-block-width" -> Prop Border_block_width
  | "border-style" -> Prop Border_style
  | "border-top-style" -> Prop Border_top_style
  | "border-right-style" -> Prop Border_right_style
  | "border-bottom-style" -> Prop Border_bottom_style
  | "border-left-style" -> Prop Border_left_style
  | "border-inline-start-style" -> Prop Border_inline_start_style
  | "border-inline-end-style" -> Prop Border_inline_end_style
  | "border-block-start-style" -> Prop Border_block_start_style
  | "border-block-end-style" -> Prop Border_block_end_style
  | "border-width" -> Prop Border_width
  | "border-top-width" -> Prop Border_top_width
  | "border-right-width" -> Prop Border_right_width
  | "border-bottom-width" -> Prop Border_bottom_width
  | "border-left-width" -> Prop Border_left_width
  | "border-image" -> Prop Border_image
  | "border-radius" -> Prop Border_radius
  | "border-top-left-radius" -> Prop Border_top_left_radius
  | "border-top-right-radius" -> Prop Border_top_right_radius
  | "border-bottom-left-radius" -> Prop Border_bottom_left_radius
  | "border-bottom-right-radius" -> Prop Border_bottom_right_radius
  | "outline-color" -> Prop Outline_color
  | "text-decoration-color" -> Prop Text_decoration_color
  | "display" -> Prop Display
  | "position" -> Prop Position
  | "visibility" -> Prop Visibility
  | "baseline-source" -> Prop Baseline_source
  | "alignment-baseline" -> Prop Alignment_baseline
  | "baseline-shift" -> Prop Baseline_shift
  | "overflow" -> Prop Overflow
  | "overflow-x" -> Prop Overflow_x
  | "overflow-y" -> Prop Overflow_y
  | "overflow-block" -> Prop Overflow_block
  | "overflow-inline" -> Prop Overflow_inline
  | "margin" -> Prop Margin
  | "margin-left" -> Prop Margin_left
  | "margin-right" -> Prop Margin_right
  | "margin-top" -> Prop Margin_top
  | "margin-bottom" -> Prop Margin_bottom
  | "margin-inline" -> Prop Margin_inline
  | "margin-inline-start" -> Prop Margin_inline_start
  | "margin-inline-end" -> Prop Margin_inline_end
  | "margin-block" -> Prop Margin_block
  | "margin-block-start" -> Prop Margin_block_start
  | "margin-block-end" -> Prop Margin_block_end
  | "padding" -> Prop Padding
  | "padding-left" -> Prop Padding_left
  | "padding-right" -> Prop Padding_right
  | "padding-top" -> Prop Padding_top
  | "padding-bottom" -> Prop Padding_bottom
  | "padding-inline" -> Prop Padding_inline
  | "padding-inline-start" -> Prop Padding_inline_start
  | "padding-inline-end" -> Prop Padding_inline_end
  | "padding-block" -> Prop Padding_block
  | "padding-block-start" -> Prop Padding_block_start
  | "padding-block-end" -> Prop Padding_block_end
  | "font-size" -> Prop Font_size
  | "font-weight" -> Prop Font_weight
  | "font-style" -> Prop Font_style
  | "font-family" -> Prop Font_family
  | "font-feature-settings" -> Prop Font_feature_settings
  | "font-variation-settings" -> Prop Font_variation_settings
  | "src" -> Prop Source
  | "text-align" -> Prop Text_align
  | "text-decoration" -> Prop Text_decoration
  | "text-decoration-line" -> Prop Text_decoration_line
  | "text-decoration-skip" -> Prop Text_decoration_skip
  | "text-decoration-skip-self" -> Prop Text_decoration_skip_self
  | "text-decoration-skip-box" -> Prop Text_decoration_skip_box
  | "text-decoration-skip-inset" -> Prop Text_decoration_skip_inset
  | "text-decoration-skip-spaces" -> Prop Text_decoration_skip_spaces
  | "text-emphasis" -> Prop Text_emphasis
  | "text-emphasis-style" -> Prop Text_emphasis_style
  | "text-emphasis-color" -> Prop Text_emphasis_color
  | "text-emphasis-position" -> Prop Text_emphasis_position
  | "text-emphasis-skip" -> Prop Text_emphasis_skip
  | "text-orientation" -> Prop Text_orientation
  | "text-transform" -> Prop Text_transform
  | "text-indent" -> Prop Text_indent
  | "letter-spacing" -> Prop Letter_spacing
  | "flex" -> Prop Flex
  | "flex-direction" -> Prop Flex_direction
  | "flex-wrap" -> Prop Flex_wrap
  | "flex-flow" -> Prop Flex_flow
  | "align-items" -> Prop Align_items
  | "justify-content" -> Prop Justify_content
  | "opacity" -> Prop Opacity
  (* SVG 1.1 sec. 11.4 / Filter Effects 1: each is an <alpha-value>, the same
     number-or-percentage as [opacity]. *)
  | "fill-opacity" -> Prop Fill_opacity
  | "stroke-opacity" -> Prop Stroke_opacity
  | "stop-opacity" -> Prop Stop_opacity
  | "flood-opacity" -> Prop Flood_opacity
  | "animation-name" -> Prop Animation_name
  | "transform" -> Prop Transform
  | "transform-origin" -> Prop Transform_origin
  | "transform-box" -> Prop Transform_box
  | "translate" -> Prop Translate
  | "box-sizing" -> Prop Box_sizing
  | "field-sizing" -> Prop Field_sizing
  | "caption-side" -> Prop Caption_side
  | "grid-template-columns" -> Prop Grid_template_columns
  | "grid-template-rows" -> Prop Grid_template_rows
  | "box-shadow" -> Prop Box_shadow
  | "content" -> Prop Content
  | "counter-reset" -> Prop Counter_reset
  | "counter-increment" -> Prop Counter_increment
  | "accent-color" -> Prop Accent_color
  | "caret-color" -> Prop Caret_color
  (* Common properties that were missing *)
  | "border" -> Prop Border
  | "resize" -> Prop Resize
  | "user-select" -> Prop User_select
  | "pointer-events" -> Prop Pointer_events
  | "cursor" -> Prop Cursor
  | "interactivity" -> Prop Interactivity
  | "caret-animation" -> Prop Caret_animation
  | "caret-shape" -> Prop Caret_shape
  | "caret" -> Prop Caret
  | "interest-delay" -> Prop Interest_delay
  | "interest-delay-start" -> Prop Interest_delay_start
  | "interest-delay-end" -> Prop Interest_delay_end
  | "nav-up" -> Prop Nav_up
  | "nav-right" -> Prop Nav_right
  | "nav-down" -> Prop Nav_down
  | "nav-left" -> Prop Nav_left
  | "appearance" -> Prop Appearance
  | "color-scheme" -> Prop Color_scheme
  | "print-color-adjust" -> Prop Print_color_adjust
  | "-webkit-print-color-adjust" -> Prop Webkit_print_color_adjust
  | "box-decoration-break" -> Prop Box_decoration_break
  | "-webkit-box-decoration-break" -> Prop Webkit_box_decoration_break
  | "filter" -> Prop Filter
  | "transition" -> Prop Transition
  | "animation" -> Prop Animation
  | "transition-behavior" -> Prop Transition_behavior
  | "overlay" -> Prop Overlay
  | "text-shadow" -> Prop Text_shadow
  | "font" -> Prop Font
  | "outline" -> Prop Outline
  | "z-index" -> Prop Z_index
  | "zoom" -> Prop Zoom
  | "inset" -> Prop Inset
  | "inset-inline" -> Prop Inset_inline
  | "inset-inline-start" -> Prop Inset_inline_start
  | "inset-inline-end" -> Prop Inset_inline_end
  | "inset-block" -> Prop Inset_block
  | "inset-block-start" -> Prop Inset_block_start
  | "inset-block-end" -> Prop Inset_block_end
  | "top" -> Prop Top
  | "right" -> Prop Right
  | "bottom" -> Prop Bottom
  | "left" -> Prop Left
  | "border-top" -> Prop Border_top
  | "border-right" -> Prop Border_right
  | "border-bottom" -> Prop Border_bottom
  | "border-left" -> Prop Border_left
  | "border-collapse" -> Prop Border_collapse
  | "tab-size" -> Prop Tab_size
  | "line-height" -> Prop Line_height
  | "list-style" -> Prop List_style
  | "vertical-align" -> Prop Vertical_align
  (* Missing properties to add *)
  | "align-content" -> Prop Align_content
  | "align-self" -> Prop Align_self
  | "animation-delay" -> Prop Animation_delay
  | "animation-direction" -> Prop Animation_direction
  | "animation-duration" -> Prop Animation_duration
  | "animation-fill-mode" -> Prop Animation_fill_mode
  | "animation-iteration-count" -> Prop Animation_iteration_count
  | "animation-play-state" -> Prop Animation_play_state
  | "animation-composition" -> Prop Animation_composition
  | "animation-timing-function" -> Prop Animation_timing_function
  | "aspect-ratio" -> Prop Aspect_ratio
  | "backdrop-filter" -> Prop Backdrop_filter
  | "-webkit-backdrop-filter" -> Prop Webkit_backdrop_filter
  | "-webkit-mask-image" -> Prop Webkit_mask_image
  | "-webkit-mask-composite" -> Prop Webkit_mask_composite
  | "-webkit-mask-source-type" -> Prop Webkit_mask_source_type
  | "-webkit-mask-size" -> Prop Webkit_mask_size
  | "-webkit-mask-position" -> Prop Webkit_mask_position
  | "-webkit-mask-repeat" -> Prop Webkit_mask_repeat
  | "-webkit-mask-clip" -> Prop Webkit_mask_clip
  | "-webkit-mask-origin" -> Prop Webkit_mask_origin
  | "border-image-source" -> Prop Border_image_source
  | "border-image-slice" -> Prop Border_image_slice
  | "border-image-repeat" -> Prop Border_image_repeat
  | "border-image-width" -> Prop Border_image_width
  | "border-image-outset" -> Prop Border_image_outset
  | "mask-image" -> Prop Mask_image
  | "mask-composite" -> Prop Mask_composite
  | "mask-mode" -> Prop Mask_mode
  | "mask-size" -> Prop Mask_size
  | "mask-position" -> Prop Mask_position
  | "mask-repeat" -> Prop Mask_repeat
  | "mask-border" -> Prop Mask_border
  | "mask-clip" -> Prop Mask_clip
  | "mask-origin" -> Prop Mask_origin
  | "mask-type" -> Prop Mask_type
  | "backface-visibility" -> Prop Backface_visibility
  | "background-attachment" -> Prop Background_attachment
  | "background-blend-mode" -> Prop Background_blend_mode
  | "background-origin" -> Prop Background_origin
  | "background-clip" -> Prop Background_clip
  | "-webkit-background-clip" -> Prop Webkit_background_clip
  | "background-position" -> Prop Background_position
  | "background-repeat" -> Prop Background_repeat
  | "background-size" -> Prop Background_size
  | "border-block" -> Prop Border_block
  | "border-block-start" -> Prop Border_block_start
  | "border-block-end" -> Prop Border_block_end
  | "border-inline" -> Prop Border_inline
  | "border-inline-start" -> Prop Border_inline_start
  | "border-inline-end" -> Prop Border_inline_end
  | "border-end-end-radius" -> Prop Border_end_end_radius
  | "border-end-start-radius" -> Prop Border_end_start_radius
  | "border-inline-end-color" -> Prop Border_inline_end_color
  | "border-block-start-color" -> Prop Border_block_start_color
  | "border-block-end-color" -> Prop Border_block_end_color
  | "border-block-end-width" -> Prop Border_block_end_width
  | "border-block-start-width" -> Prop Border_block_start_width
  | "border-block-style" -> Prop Border_block_style
  | "border-inline-end-width" -> Prop Border_inline_end_width
  | "border-inline-start-color" -> Prop Border_inline_start_color
  | "border-inline-start-width" -> Prop Border_inline_start_width
  | "border-inline-style" -> Prop Border_inline_style
  | "border-spacing" -> Prop Border_spacing
  | "border-start-end-radius" -> Prop Border_start_end_radius
  | "border-start-start-radius" -> Prop Border_start_start_radius
  | "break-before" -> Prop Break_before
  | "break-after" -> Prop Break_after
  | "break-inside" -> Prop Break_inside
  | "size" -> Prop Page_size
  (* CSS Fragmentation 3 sec. 6 page-break-* aliases. Keep them as typed legacy
     properties so pretty output preserves the authored property name; minified
     output still serializes through the shorter modern break-* spelling. *)
  | "page-break-before" -> Prop Page_break_before
  | "page-break-after" -> Prop Page_break_after
  | "page-break-inside" -> Prop Page_break_inside
  | "columns" -> Prop Columns
  | "column-width" -> Prop Column_width
  | "column-count" -> Prop Column_count
  | "column-rule" -> Prop Column_rule
  | "column-span" -> Prop Column_span
  | "clear" -> Prop Clear
  | "clip" -> Prop Clip
  | "clip-path" -> Prop Clip_path
  | "column-gap" -> Prop Column_gap
  | "contain" -> Prop Contain
  | "container-name" -> Prop Container_name
  | "container-type" -> Prop Container_type
  | "container" -> Prop Container
  | "anchor-name" -> Prop Anchor_name
  | "position-anchor" -> Prop Position_anchor
  | "position-try-fallbacks" -> Prop Position_try_fallbacks
  | "position-try-order" -> Prop Position_try_order
  | "position-try" -> Prop Position_try
  | "position-visibility" -> Prop Position_visibility
  | "position-area" -> Prop Position_area
  | "shape-outside" -> Prop Shape_outside
  | "shape-margin" -> Prop Shape_margin
  | "shape-image-threshold" -> Prop Shape_image_threshold
  | "overflow-clip-margin" -> Prop Overflow_clip_margin
  | "overflow-anchor" -> Prop Overflow_anchor
  | "scrollbar-width" -> Prop Scrollbar_width
  | "scrollbar-color" -> Prop Scrollbar_color
  | "scrollbar-gutter" -> Prop Scrollbar_gutter
  | "line-height-step" -> Prop Line_height_step
  | "font-palette" -> Prop Font_palette
  | "font-synthesis" -> Prop Font_synthesis
  | "text-wrap-mode" -> Prop Text_wrap_mode
  | "text-wrap-style" -> Prop Text_wrap_style
  | "text-box-trim" -> Prop Text_box_trim
  | "text-underline-position" -> Prop Text_underline_position
  | "text-box-edge" -> Prop Text_box_edge
  | "text-box" -> Prop Text_box
  | "inline-sizing" -> Prop Inline_sizing
  | "line-fit-edge" -> Prop Line_fit_edge
  | "interpolate-size" -> Prop Interpolate_size
  | "min-intrinsic-sizing" -> Prop Min_intrinsic_sizing
  | "ruby-align" -> Prop Ruby_align
  | "ruby-merge" -> Prop Ruby_merge
  | "ruby-overhang" -> Prop Ruby_overhang
  | "ruby-position" -> Prop Ruby_position
  | "glyph-orientation-vertical" -> Prop Glyph_orientation_vertical
  | "animation-timeline" -> Prop Animation_timeline
  | "animation-range" -> Prop Animation_range
  | "animation-range-start" -> Prop Animation_range_start
  | "animation-range-end" -> Prop Animation_range_end
  | "scroll-timeline" -> Prop Scroll_timeline
  | "scroll-timeline-name" -> Prop Scroll_timeline_name
  | "scroll-timeline-axis" -> Prop Scroll_timeline_axis
  | "view-transition-name" -> Prop View_transition_name
  | "view-transition-class" -> Prop View_transition_class
  | "image-orientation" -> Prop Image_orientation
  | "image-rendering" -> Prop Image_rendering
  | "image-resolution" -> Prop Image_resolution
  | "contain-intrinsic-size" -> Prop Contain_intrinsic_size
  | "contain-intrinsic-width" -> Prop Contain_intrinsic_width
  | "contain-intrinsic-height" -> Prop Contain_intrinsic_height
  | "contain-intrinsic-block-size" -> Prop Contain_intrinsic_block_size
  | "contain-intrinsic-inline-size" -> Prop Contain_intrinsic_inline_size
  | "margin-trim" -> Prop Margin_trim
  | "offset-path" -> Prop Offset_path
  | "offset-distance" -> Prop Offset_distance
  | "offset-rotate" -> Prop Offset_rotate
  | "font-size-adjust" -> Prop Font_size_adjust
  | "font-variant-emoji" -> Prop Font_variant_emoji
  | "text-spacing-trim" -> Prop Text_spacing_trim
  | "hyphenate-limit-chars" -> Prop Hyphenate_limit_chars
  | "initial-letter" -> Prop Initial_letter
  | "initial-letter-align" -> Prop Initial_letter_align
  | "initial-letter-wrap" -> Prop Initial_letter_wrap
  | "dominant-baseline" -> Prop Dominant_baseline
  | "view-timeline-name" -> Prop View_timeline_name
  | "view-timeline-axis" -> Prop View_timeline_axis
  | "view-timeline-inset" -> Prop View_timeline_inset
  | "view-timeline" -> Prop View_timeline
  | "timeline-scope" -> Prop Timeline_scope
  | "content-visibility" -> Prop Content_visibility
  | "direction" -> Prop Direction
  | "fill" -> Prop Fill
  (* SVG 2 sec. 13.5 and 14.4: both take the same <fill-rule>. *)
  | "fill-rule" -> Prop Fill_rule
  | "clip-rule" -> Prop Clip_rule
  | "stroke-linecap" -> Prop Stroke_linecap
  | "stroke-linejoin" -> Prop Stroke_linejoin
  | "stroke-miterlimit" -> Prop Stroke_miterlimit
  | "stroke-dashoffset" -> Prop Stroke_dashoffset
  | "stroke-dasharray" -> Prop Stroke_dasharray
  | "paint-order" -> Prop Paint_order
  | "vector-effect" -> Prop Vector_effect
  (* SVG 2 sec. 13.4 / Filter Effects 1 sec. 9.3 and 12.2: each is a plain
     <color>, so they minify like any other colour-valued property. *)
  | "stop-color" -> Prop Stop_color
  | "flood-color" -> Prop Flood_color
  | "lighting-color" -> Prop Lighting_color
  | "flex-basis" -> Prop Flex_basis
  | "flex-grow" -> Prop Flex_grow
  | "flex-shrink" -> Prop Flex_shrink
  | "float" -> Prop Float
  | "font-stretch" -> Prop Font_stretch
  | "font-optical-sizing" -> Prop Font_optical_sizing
  | "font-kerning" -> Prop Font_kerning
  | "font-language-override" -> Prop Font_language_override
  | "font-synthesis-style" -> Prop Font_synthesis_style
  | "font-synthesis-weight" -> Prop Font_synthesis_weight
  | "font-synthesis-small-caps" -> Prop Font_synthesis_small_caps
  | "font-synthesis-position" -> Prop Font_synthesis_position
  | "font-variant-ligatures" -> Prop Font_variant_ligatures
  | "font-variant-caps" -> Prop Caps
  | "font-variant-numeric" -> Prop Numeric
  | "font-variant-position" -> Prop Font_variant_position
  | "font-variant-east-asian" -> Prop East_asian
  | "forced-color-adjust" -> Prop Forced_color_adjust
  | "gap" -> Prop Gap
  | "grid-area" -> Prop Grid_area
  | "grid-auto-columns" -> Prop Grid_auto_columns
  | "grid-auto-flow" -> Prop Grid_auto_flow
  | "grid-auto-rows" -> Prop Grid_auto_rows
  | "grid-column" -> Prop Grid_column
  | "grid-column-end" -> Prop Grid_column_end
  | "grid-column-start" -> Prop Grid_column_start
  | "grid-row" -> Prop Grid_row
  | "grid-row-end" -> Prop Grid_row_end
  | "grid-row-start" -> Prop Grid_row_start
  | "grid" -> Prop Grid
  | "grid-template" -> Prop Grid_template
  | "grid-template-areas" -> Prop Grid_template_areas
  | "hyphens" -> Prop Hyphens
  | "isolation" -> Prop Isolation
  | "justify-items" -> Prop Justify_items
  | "justify-self" -> Prop Justify_self
  | "list-style-image" -> Prop List_style_image
  | "list-style-position" -> Prop List_style_position
  | "list-style-type" -> Prop List_style_type
  | "mask" -> Prop Mask
  | "mix-blend-mode" -> Prop Mix_blend_mode
  | "object-fit" -> Prop Object_fit
  | "object-view-box" -> Prop Object_view_box
  | "object-position" -> Prop Object_position
  | "order" -> Prop Order
  | "outline-offset" -> Prop Outline_offset
  | "outline-style" -> Prop Outline_style
  | "outline-width" -> Prop Outline_width
  | "overflow-wrap" -> Prop Overflow_wrap
  | "overscroll-behavior" -> Prop Overscroll_behavior
  | "overscroll-behavior-x" -> Prop Overscroll_behavior_x
  | "overscroll-behavior-y" -> Prop Overscroll_behavior_y
  | "overscroll-behavior-block" -> Prop Overscroll_behavior_block
  | "overscroll-behavior-inline" -> Prop Overscroll_behavior_inline
  | "perspective" -> Prop Perspective
  | "perspective-origin" -> Prop Perspective_origin
  | "place-content" -> Prop Place_content
  | "place-items" -> Prop Place_items
  | "place-self" -> Prop Place_self
  | "quotes" -> Prop Quotes
  | "rotate" -> Prop Rotate
  | "row-gap" -> Prop Row_gap
  | "scale" -> Prop Scale
  | "scroll-behavior" -> Prop Scroll_behavior
  | "scroll-margin" -> Prop Scroll_margin
  | "scroll-margin-bottom" -> Prop Scroll_margin_bottom
  | "scroll-margin-left" -> Prop Scroll_margin_left
  | "scroll-margin-right" -> Prop Scroll_margin_right
  | "scroll-margin-top" -> Prop Scroll_margin_top
  | "scroll-margin-inline" -> Prop Scroll_margin_inline
  | "scroll-margin-inline-start" -> Prop Scroll_margin_inline_start
  | "scroll-margin-inline-end" -> Prop Scroll_margin_inline_end
  | "scroll-margin-block" -> Prop Scroll_margin_block
  | "scroll-margin-block-start" -> Prop Scroll_margin_block_start
  | "scroll-margin-block-end" -> Prop Scroll_margin_block_end
  | "scroll-padding" -> Prop Scroll_padding
  | "scroll-padding-bottom" -> Prop Scroll_padding_bottom
  | "scroll-padding-left" -> Prop Scroll_padding_left
  | "scroll-padding-right" -> Prop Scroll_padding_right
  | "scroll-padding-top" -> Prop Scroll_padding_top
  | "scroll-padding-inline" -> Prop Scroll_padding_inline
  | "scroll-padding-inline-start" -> Prop Scroll_padding_inline_start
  | "scroll-padding-inline-end" -> Prop Scroll_padding_inline_end
  | "scroll-padding-block" -> Prop Scroll_padding_block
  | "scroll-padding-block-start" -> Prop Scroll_padding_block_start
  | "scroll-padding-block-end" -> Prop Scroll_padding_block_end
  | "scroll-snap-align" -> Prop Scroll_snap_align
  | "scroll-snap-stop" -> Prop Scroll_snap_stop
  | "scroll-snap-type" -> Prop Scroll_snap_type
  | "stroke" -> Prop Stroke
  | "stroke-width" -> Prop Stroke_width
  | "table-layout" -> Prop Table_layout
  | "text-decoration-skip-ink" -> Prop Text_decoration_skip_ink
  | "text-decoration-style" -> Prop Text_decoration_style
  | "text-decoration-thickness" -> Prop Text_decoration_thickness
  | "text-overflow" -> Prop Text_overflow
  | "text-size-adjust" -> Prop Text_size_adjust
  | "text-underline-offset" -> Prop Text_underline_offset
  | "text-wrap" -> Prop Text_wrap
  | "line-break" -> Prop Line_break
  | "touch-action" -> Prop Touch_action
  | "transform-style" -> Prop Transform_style
  | "transition-delay" -> Prop Transition_delay
  | "transition-duration" -> Prop Transition_duration
  | "transition-property" -> Prop Transition_property
  | "transition-timing-function" -> Prop Transition_timing_function
  | "unicode-bidi" -> Prop Unicode_bidi
  | "white-space" -> Prop White_space
  | "will-change" -> Prop Will_change
  | "word-break" -> Prop Word_break
  | "word-spacing" -> Prop Word_spacing
  | "writing-mode" -> Prop Writing_mode
  | "text-combine-upright" -> Prop Text_combine_upright
  (* Vendor prefixed properties *)
  | "-webkit-transform" -> Prop Webkit_transform
  | "-moz-transform" -> Prop Moz_transform
  | "-ms-transform" -> Prop Ms_transform
  | "-o-transform" -> Prop O_transform
  | "-webkit-transition" -> Prop Webkit_transition
  | "-webkit-transition-delay" -> Prop Webkit_transition_delay
  | "-webkit-transition-duration" -> Prop Webkit_transition_duration
  | "-webkit-transition-property" -> Prop Webkit_transition_property
  | "-webkit-transition-timing-function" ->
      Prop Webkit_transition_timing_function
  | "-webkit-animation" -> Prop Webkit_animation
  | "-webkit-animation-delay" -> Prop Webkit_animation_delay
  | "-webkit-animation-duration" -> Prop Webkit_animation_duration
  | "-webkit-animation-direction" -> Prop Webkit_animation_direction
  | "-webkit-animation-iteration-count" -> Prop Webkit_animation_iteration_count
  | "-webkit-animation-name" -> Prop Webkit_animation_name
  | "-webkit-animation-timing-function" -> Prop Webkit_animation_timing_function
  | "-webkit-animation-fill-mode" -> Prop Webkit_animation_fill_mode
  | "-webkit-animation-play-state" -> Prop Webkit_animation_play_state
  | "-webkit-flex-direction" -> Prop Webkit_flex_direction
  | "-webkit-flex-wrap" -> Prop Webkit_flex_wrap
  | "-webkit-flex-flow" -> Prop Webkit_flex_flow
  | "-webkit-justify-content" -> Prop Webkit_justify_content
  | "-webkit-align-items" -> Prop Webkit_align_items
  | "-webkit-align-content" -> Prop Webkit_align_content
  | "-webkit-align-self" -> Prop Webkit_align_self
  | "-webkit-border-radius" -> Prop Webkit_border_radius
  | "-webkit-box-sizing" -> Prop Webkit_box_sizing
  | "-moz-box-sizing" -> Prop Moz_box_sizing
  | "-webkit-box-shadow" -> Prop Webkit_box_shadow
  | "-webkit-background-size" -> Prop Webkit_background_size
  | "-webkit-filter" -> Prop Webkit_filter
  | "-moz-animation" -> Prop Moz_animation
  | "-moz-animation-delay" -> Prop Moz_animation_delay
  | "-moz-animation-duration" -> Prop Moz_animation_duration
  | "-moz-animation-direction" -> Prop Moz_animation_direction
  | "-moz-animation-iteration-count" -> Prop Moz_animation_iteration_count
  | "-moz-animation-name" -> Prop Moz_animation_name
  | "-moz-animation-timing-function" -> Prop Moz_animation_timing_function
  | "-moz-animation-fill-mode" -> Prop Moz_animation_fill_mode
  | "-moz-animation-play-state" -> Prop Moz_animation_play_state
  | "-moz-transition" -> Prop Moz_transition
  | "-moz-transition-delay" -> Prop Moz_transition_delay
  | "-moz-transition-duration" -> Prop Moz_transition_duration
  | "-moz-transition-property" -> Prop Moz_transition_property
  | "-moz-transition-timing-function" -> Prop Moz_transition_timing_function
  | "-moz-border-radius" -> Prop Moz_border_radius
  | "-moz-box-shadow" -> Prop Moz_box_shadow
  | "-webkit-text-size-adjust" -> Prop Webkit_text_size_adjust
  | "-webkit-tap-highlight-color" -> Prop Webkit_tap_highlight_color
  | "-webkit-text-fill-color" -> Prop Webkit_text_fill_color
  | "-webkit-user-select" -> Prop Webkit_user_select
  | "-ms-user-select" -> Prop Ms_user_select
  | "-moz-user-select" -> Prop Moz_user_select
  | "-webkit-text-decoration" -> Prop Webkit_text_decoration
  | "-webkit-text-decoration-color" -> Prop Webkit_text_decoration_color
  | "-webkit-appearance" -> Prop Webkit_appearance
  | "-webkit-font-smoothing" -> Prop Webkit_font_smoothing
  | "-webkit-line-clamp" -> Prop Webkit_line_clamp
  | "-webkit-box-orient" -> Prop Webkit_box_orient
  | "-webkit-hyphens" -> Prop Webkit_hyphens
  | "-moz-appearance" -> Prop Moz_appearance
  | "-moz-orient" -> Prop Moz_orient
  | "-moz-osx-font-smoothing" -> Prop Moz_osx_font_smoothing
  | "-ms-filter" -> Prop Ms_filter
  | "-o-transition" -> Prop O_transition
  (* PROPERTY_MATCHING_END - Used by scripts/check_properties.ml *)
  (* Custom properties [--*] always pass through as [Unknown_property] (their
     value is opaque); other unrecognized names fail here. The lenient
     declaration recovery in [Declaration.read_regular_property_declaration]
     catches and falls back to [read_unknown_property_declaration]. *)
  | _ when String.length prop_name >= 2 && String.sub prop_name 0 2 = "--" ->
      Prop (Unknown_property prop_name)
  | _ -> Cursor.err_invalid t ("unknown property: " ^ prop_name)

(* Helper functions for property types *)

let animation_shorthand ?name ?duration ?timing_function ?delay ?iteration_count
    ?direction ?fill_mode ?play_state ?timeline () : animation =
  Shorthand
    {
      name = Option.map (fun name -> (Name name : animation_name)) name;
      duration;
      timing_function;
      delay;
      iteration_count;
      direction;
      fill_mode;
      play_state;
      timeline;
    }

let transition_shorthand ?(property = (All : transition_property_value))
    ?duration ?timing_function ?delay ?behavior () : transition =
  Shorthand { property; duration; timing_function; delay; behavior }

let text_decoration_shorthand ?lines ?style ?color ?thickness () :
    text_decoration =
  Shorthand { lines = Option.value ~default:[] lines; style; color; thickness }

let read_object_view_box_inset t =
  Cursor.call "inset" t (fun t ->
      Cursor.ws t;
      let top = read_length t in
      Cursor.ws t;
      let read_opt () : length option =
        if Cursor.is_done t then None else Some (read_length t)
      in
      let right = read_opt () in
      Cursor.ws t;
      let bottom = if Option.is_some right then read_opt () else None in
      Cursor.ws t;
      let left = if Option.is_some bottom then read_opt () else None in
      (Inset (top, right, bottom, left) : object_view_box))

let read_object_view_box_xywh t =
  Cursor.call "xywh" t (fun inner ->
      Cursor.ws inner;
      let x, y, width, height = read_clip_path_inset_quad inner in
      let rounded = read_clip_path_round inner in
      (Xywh { x; y; width; height; rounded } : object_view_box))

let read_object_view_box_rect t =
  Cursor.call "rect" t (fun inner ->
      Cursor.ws inner;
      let top, right, bottom, left = read_clip_path_inset_quad inner in
      let rounded = read_clip_path_round inner in
      (Rect { top; right; bottom; left; rounded } : object_view_box))

let rec read_object_view_box t : object_view_box =
  Cursor.enum_or_calls "object-view-box"
    [
      ("none", (None : object_view_box));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:
      [
        ("inset", read_object_view_box_inset);
        ("xywh", read_object_view_box_xywh);
        ("rect", read_object_view_box_rect);
        ("var", fun t -> Var (Values.read_var read_object_view_box t));
      ]
    t

let pp_any_property ctx (Prop p) = pp_property ctx p

let pp_font_url ctx s =
  Pp.string ctx "url(";
  if url_needs_quotes s then (
    Pp.char ctx '"';
    Pp.string ctx s;
    Pp.char ctx '"')
  else Pp.string ctx s;
  Pp.char ctx ')'

let pp_quoted_font_url ctx quote s =
  Pp.string ctx "url(";
  Pp.char ctx quote;
  Pp.string ctx s;
  Pp.char ctx quote;
  Pp.char ctx ')'

let pp_font_src_modifiers ctx (format : string option) (tech : string option) =
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
      Pp.char ctx ')'

let rec pp_font_src_entry ctx : Font_face.src_entry -> unit = function
  | Local name ->
      Pp.string ctx "local(";
      Pp.char ctx '"';
      Pp.string ctx name;
      Pp.char ctx '"';
      Pp.char ctx ')'
  | Url { url; format; tech } ->
      pp_font_url ctx url;
      pp_font_src_modifiers ctx format tech
  | Quoted_url { url; quote; format; tech } ->
      pp_quoted_font_url ctx quote url;
      pp_font_src_modifiers ctx format tech
  | Var var -> pp_var pp_font_src ctx var

and pp_font_src ctx entries =
  let first = ref true in
  List.iter
    (fun entry ->
      if !first then first := false
      else (
        Pp.char ctx ',';
        Pp.space_if_pretty ctx ());
      pp_font_src_entry ctx entry)
    entries

let read_font_src = Font_face.read_src

let read_custom_value_as kind read components =
  match
    let cursor = Cursor.of_components components in
    let parsed = read cursor in
    Cursor.ws cursor;
    Cursor.expect_eof cursor;
    Some (Typed { kind; value = parsed })
  with
  | result -> result
  | exception Cursor.Parse_error _ -> None

(* CSS Custom Properties for Cascading Variables 1 sec. 2: an unregistered
   custom property is an opaque token stream that [var()] later substitutes
   wholesale into whichever consumer site invokes it. Canonical typed rewrites
   like [rgb(0 0 0) -> #000] assume a consumer type that only [@property --foo {
   syntax: "<color>"; ... }] can promise, so the parser leaves the tokens alone
   here. *)
let read_custom_property_value ?font_family:_ cursor =
  Tokens (Cursor.remaining cursor)

(* A registered [<color>] custom property carries a typed colour once promoted,
   so canonicalise it the same way a real colour property would. *)
let is_color_function name =
  match String.lowercase_ascii name with
  | "rgb" | "rgba" | "hsl" | "hsla" | "hwb" | "lab" | "lch" | "oklab" | "oklch"
  | "color" | "color-mix" | "light-dark" ->
      true
  | _ -> false

(* A construct whose type is fixed by its own syntax - a complete colour
   function ([oklab(...)], [color-mix(...)], ...) or a hex colour ([#abc]) - is
   unconditionally a colour in every [var()] substitution site, so folding it to
   its canonical spelling inside an opaque custom-property token stream
   preserves every rendered result while collapsing two spellings of the same
   colour. The only observable change is the exact token string a script reads
   back via [getPropertyValue]; that readback is the optimizer's domain (it
   already folds insignificant math whitespace here), so the canonical diff
   inherits this fold rather than shimming it. Bare keywords are left untouched
   - they may be a [<custom-ident>] in a non-colour context, whereas a hex token
   never can. *)
(* [c] is one component whose syntax fixes it as a colour; fold it to the
   shortest non-keyword spelling, falling back to [fallback ()] when it does not
   actually parse as a complete colour. *)
let fold_custom_color ~lossless (c : Component.t) ~fallback =
  let text = Parser.to_string_custom [ c ] in
  let cur = Cursor.of_string text in
  match
    try Some (Values.read_color cur) with Cursor.Parse_error _ -> None
  with
  | Some col when Cursor.is_done cur -> (
      let canon =
        Pp.to_string ~minify:true Values.pp_color
          (Values.nonkeyword_color
             (Values.normalize_color ~lossless ~in_feature_query:false col))
      in
      match read_custom_property_value (Cursor.of_string canon) with
      | Tokens cs -> cs
      | Typed _ -> [ c ])
  | _ -> fallback ()

(* A math function ([calc()], [min()], [clamp()], ...) is unconditionally a math
   expression whose type is fixed by its operands' units, so when it reduces to
   a single constant it has that value in every [var()] substitution site - fold
   it inside an opaque custom-property stream like a complete colour. [<number>]
   and the unit-unambiguous dimensions ([<angle>] / [<time>]) qualify;
   [<percentage>] is ambiguous (length vs number percentage) so it stays
   verbatim, as does a function that still references a [var()] (it does not
   reduce to a leaf). *)
let is_math_function name =
  match String.lowercase_ascii name with
  | "calc" | "min" | "max" | "clamp" | "round" | "mod" | "rem" | "abs" | "sign"
  | "hypot" | "pow" | "sqrt" | "exp" | "log" ->
      true
  | _ -> false

let fold_custom_calc (c : Component.t) ~fallback =
  let text = Parser.to_string_custom [ c ] in
  (* Parse the whole token as one typed value and fold only when it reduces to a
     single concrete leaf (not a [calc()] that still carries a [var()]). *)
  let try_typed : type a.
      (Cursor.t -> a) ->
      (a -> a) ->
      a Pp.t ->
      (a -> bool) ->
      Component.t list option =
   fun reader normalize pp reduced ->
    let cur = Cursor.of_string text in
    match try Some (reader cur) with Cursor.Parse_error _ -> None with
    | Some v when Cursor.is_done cur ->
        let folded = normalize v in
        if reduced folded then
          match
            read_custom_property_value
              (Cursor.of_string (Pp.to_string ~minify:true pp folded))
          with
          | Tokens cs -> Some cs
          | Typed _ -> None
        else None
    | _ -> None
  in
  let number_reduced = function (Num _ : number) -> true | _ -> false in
  let angle_reduced = function
    | (Deg _ | Rad _ | Turn _ | Grad _ : angle) -> true
    | _ -> false
  in
  let time_reduced = function (S _ | Ms _ : duration) -> true | _ -> false in
  match
    List.find_map Fun.id
      [
        try_typed read_number
          (fun n -> normalize_number n)
          pp_number number_reduced;
        try_typed read_angle_unit_required
          (fun a -> normalize_angle a)
          pp_angle angle_reduced;
        try_typed read_duration
          (fun d -> normalize_duration d)
          pp_duration time_reduced;
      ]
  with
  | Some cs -> cs
  | None -> fallback ()

(* Filter Effects 1 sec. 8.5 gives [hue-rotate()] the argument [[ <angle> |
   <zero> ]?] and 0 when omitted, so a zero argument is redundant. [hue-rotate]
   names a filter function and nothing else, so this holds wherever the stream
   is substituted, which is the same argument that lets a colour function fold
   here. *)
let hue_rotate_zero_argument (func : Component.func) =
  String.lowercase_ascii func.name = "hue-rotate"
  && func.arguments <> []
  &&
  let cur = Cursor.of_string (Parser.to_string_custom func.arguments) in
  match read_angle_unit_required cur with
  | exception Cursor.Parse_error _ -> false
  | angle ->
      Cursor.is_done cur
      && Values.angle_degrees_opt (normalize_angle angle) = Some 0.

let drop_function_arguments wrapped =
  Component.Func
    { wrapped with node = { wrapped.Component.node with arguments = [] } }

let rec canonicalize_custom_colors_components ~lossless comps =
  let fold_color c ~fallback = fold_custom_color ~lossless c ~fallback in
  List.concat_map
    (fun (c : Component.t) ->
      match c with
      | Component.Func wrapped
        when is_color_function wrapped.Component.node.name ->
          fold_color c ~fallback:(fun () ->
              let func = wrapped.Component.node in
              let args =
                canonicalize_custom_colors_components ~lossless func.arguments
              in
              [
                Component.Func
                  { wrapped with node = { func with arguments = args } };
              ])
      | Component.Preserved { kind = Token.Hash _; _ } ->
          fold_color c ~fallback:(fun () -> [ c ])
      | Component.Func wrapped when is_math_function wrapped.Component.node.name
        ->
          fold_custom_calc c ~fallback:(fun () ->
              let func = wrapped.Component.node in
              let args =
                canonicalize_custom_colors_components ~lossless func.arguments
              in
              [
                Component.Func
                  { wrapped with node = { func with arguments = args } };
              ])
      | Component.Func wrapped
        when hue_rotate_zero_argument wrapped.Component.node ->
          [ drop_function_arguments wrapped ]
      | Component.Func wrapped ->
          let func = wrapped.Component.node in
          let args =
            canonicalize_custom_colors_components ~lossless func.arguments
          in
          [
            Component.Func
              { wrapped with node = { func with arguments = args } };
          ]
      | Component.Block wrapped ->
          let block = wrapped.Component.node in
          let value =
            canonicalize_custom_colors_components ~lossless block.value
          in
          [ Component.Block { wrapped with node = { block with value } } ]
      | Component.Preserved _ -> [ c ])
    comps

(* Typed re-readers exposed for the registry pass that consumes [@property]
   declarations. Each reader takes a token stream and tries to parse it as the
   matching typed kind, returning [None] when the stream doesn't match. The
   unregistered path stays opaque; the registry pass is what flips a value to
   [Typed]. *)
let try_read_custom_color components =
  read_custom_value_as Color read_color components

let try_read_custom_length components =
  read_custom_value_as Length (read_length ~with_keywords:false) components

let try_read_custom_length_percentage components =
  read_custom_value_as Length_percentage
    (read_length_percentage ~with_keywords:false)
    components

let try_read_custom_number components =
  read_custom_value_as Number read_number components

let try_read_custom_percentage components =
  read_custom_value_as Percentage read_percentage components

let try_read_custom_angle components =
  read_custom_value_as Angle read_angle_unit_required components

let try_read_custom_time components =
  read_custom_value_as Duration read_duration components

let pp_number_value ctx (value : number) =
  let pp_rounded f = Pp.float ctx (Pp.round_sig 6 f) in
  match value with
  | Num f when Pp.minified ctx -> pp_rounded f
  | Calc c when Pp.minified ctx -> (
      match eval_numeric_calc c with
      | Some f -> pp_rounded f
      | None -> pp_number ctx (Calc (eval_calc c)))
  | _ -> pp_number ctx value

let pp_value : type a. (a kind * a) Pp.t =
 fun ctx (kind, value) ->
  let pp pp_a = pp_a ctx value in
  match kind with
  | Length -> pp (pp_length ~always:true)
  | Color -> pp pp_specified_color
  | Rgb ->
      let rec pp_rgb_type : rgb Pp.t =
       fun ctx rgb ->
        match rgb with
        | Channels { r; g; b } ->
            pp_channel ctx r;
            Pp.space ctx ();
            pp_channel ctx g;
            Pp.space ctx ();
            pp_channel ctx b
        | Var v -> pp_var pp_rgb_type ctx v
      in
      pp pp_rgb_type
  | Number -> pp pp_number_value
  | Int -> pp Pp.int
  | Float -> pp Pp.float
  | Percentage -> pp pp_percentage
  | Length_percentage -> pp (pp_length_percentage ~always:true)
  | Number_percentage -> pp pp_number_percentage
  | Opacity -> pp pp_opacity
  | Value ->
      let rendered =
        if Pp.minified ctx then
          Parser.to_string_custom_minified
            ~fold_ident:Values.fold_custom_value_ident value
        else Parser.to_string_custom value
      in
      Pp.string ctx rendered
  | Shadow -> pp pp_shadow
  | Duration -> pp pp_duration
  | Aspect_ratio -> pp pp_aspect_ratio
  | Border_style -> pp pp_border_style
  | Outline_style -> pp pp_outline_style
  | Border -> pp pp_border
  | Font_weight -> pp pp_font_weight
  | Font_size -> pp pp_font_size
  | Line_height -> pp pp_line_height
  | Font_family -> pp pp_font_family
  | Font_feature_settings -> pp pp_font_feature_settings
  | Font_variation_settings -> pp pp_font_variation_settings
  | Numeric -> pp pp_font_variant_numeric
  | Font_variant_numeric_token -> pp pp_font_variant_numeric_token
  | Blend_mode -> pp pp_blend_mode
  | Scroll_snap_strictness -> pp pp_scroll_snap_strictness
  | Angle -> pp pp_angle
  | Rotate -> pp pp_rotate_value
  | Scale -> pp pp_scale
  | Box_shadow -> pp pp_shadow
  | Content -> pp pp_content
  | Gradient_stop -> pp pp_gradient_stop
  | Gradient_direction -> pp pp_gradient_direction
  | Gradient_position -> pp pp_gradient_position
  | Animation -> pp pp_animation
  | Timing_function -> pp pp_timing_function
  | Transform -> pp pp_transform
  | Touch_action -> pp pp_touch_action
  | Transition_property_value -> pp pp_transition_property_value
  | Background_image -> pp pp_background_image
  | Z_index -> pp pp_z_index
  | Filter -> pp pp_filter
  | Font_src -> pp pp_font_src

let string_of_channel : channel -> string = function
  | Int i -> string_of_int i
  | Num f -> Pp.string_of_float f
  | Pct p -> Pp.string_of_float p ^ "%"
  | Var _ -> "0"
  | None -> "none"

let string_of_kind_value : type a. a kind -> a -> string =
 fun kind value ->
  match kind with
  | Length -> Pp.to_string (pp_length ~always:false) value
  | Color -> Pp.to_string pp_color value
  | Angle -> Pp.to_string pp_angle value
  | Duration -> Pp.to_string pp_duration value
  | Float -> Pp.string_of_float value
  | Percentage -> (
      match value with Pct f -> Pp.string_of_float f | _ -> "initial")
  | Number_percentage -> Values.string_of_number_percentage value
  | Number -> Pp.to_string pp_number value
  | Int -> string_of_int value
  | Value -> Parser.to_string_custom value
  | Content -> (
      match value with
      | String "" -> "\"\""
      | String s -> "\"" ^ s ^ "\""
      | Quoted { value; quote; repr = _ } ->
          String.make 1 quote ^ value ^ String.make 1 quote
      | None -> "none"
      | Normal -> "normal"
      | Open_quote -> "open-quote"
      | Close_quote -> "close-quote"
      | Attr _ | Counter _ | Counters _ | String_ref _ | Content_list _
      | Inherit | Initial | Unset | Revert | Revert_layer | Var _ ->
          "initial")
  | Font_weight -> Pp.to_string pp_font_weight value
  | Shadow -> "0 0 #0000"
  | Border_style -> Pp.to_string pp_border_style value
  | Outline_style -> Pp.to_string pp_outline_style value
  | Scroll_snap_strictness -> Pp.to_string pp_scroll_snap_strictness value
  | Rgb -> (
      match value with
      | Channels { r; g; b } ->
          string_of_channel r ^ " " ^ string_of_channel g ^ " "
          ^ string_of_channel b
      | Var _ -> "initial")
  | Animation -> Pp.to_string pp_animation value
  | Gradient_direction -> Pp.to_string pp_gradient_direction value
  | Gradient_position -> Pp.to_string pp_gradient_position value
  | _ -> "initial"

let pp_custom_property_value ctx = function
  | Typed { kind; value } -> pp_value ctx (kind, value)
  | Tokens value -> pp_value ctx (Value, value)

let components_of_custom_property_value = function
  | Tokens components -> components
  | Typed { kind; value } ->
      Cursor.remaining
        (Cursor.of_string (Pp.to_string ~minify:true pp_value (kind, value)))

let pp_custom_property ctx (Custom_value { value; _ }) =
  pp_custom_property_value ctx value

(* CSS Values 4 sec. 6.5: the [initial] keyword resolves to the property's
   spec-defined initial value at computed time. Under [--minify] swap the
   keyword for that value when its serialization is shorter (or the same length
   but a more canonical spelling that cleancss / csso emit). *)
(* Detect the [<css-wide-keyword>] keyword sequences that the box-shorthand
   expander leaves behind: [margin: initial] is read as the singleton
   [[Initial]], then [try_merge_box_shorthand] fans it out to
   [[Initial; Initial; Initial; Initial]] before we reach the printer. *)
let box_is_all_initial : length list -> bool = function
  | [ Initial ] | [ Initial; Initial; Initial; Initial ] -> true
  | _ -> false

let canonical_initial_for_minify : type a. a property -> a -> a =
 fun prop value ->
  match (prop, value) with
  | Z_index, Initial -> Auto
  | Opacity, Initial -> Opacity_number 1.
  | Margin, vs when box_is_all_initial vs -> [ Px 0. ]
  | Padding, vs when box_is_all_initial vs -> [ Px 0. ]
  | Margin_top, Initial -> Px 0.
  | Margin_right, Initial -> Px 0.
  | Margin_bottom, Initial -> Px 0.
  | Margin_left, Initial -> Px 0.
  | Padding_top, Initial -> Px 0.
  | Padding_right, Initial -> Px 0.
  | Padding_bottom, Initial -> Px 0.
  | Padding_left, Initial -> Px 0.
  | Width, Length Initial -> Length Auto
  | Height, Length Initial -> Length Auto
  | Min_width, Length Initial -> Length Auto
  | Min_height, Length Initial -> Length Auto
  | _ -> value

(* CSS Values L4 sec. 10.10 ("Mathematical Expressions"): inside a math function
   ([calc], [min], [max], [clamp], [round], [mod], [rem], the trig family,
   [pow]/[sqrt]/[hypot]/[log]/[exp], [abs]/[sign]) whitespace is *required*
   around binary [+] and [-] (sign-token disambiguation - stripping it changes
   [100% - var(--a)] to [100%-var(--a)], where [-var] is one ident-like function
   token) but *optional* around [*], [/], [(], [)], [,]. Strip the optional
   whitespace from math-function arguments so two custom-property token streams
   that differ only there have the same canonical AST. Typed math is already
   minified by [pp_calc]; this matters for opaque [Tokens _] custom-property
   values where cascade preserves the author's whitespace verbatim by design.
   Nested non-math functions ([var()] etc.) get a recursive component walk but
   no whitespace stripping; nested math functions get their own. *)
let math_function_names =
  [
    "calc";
    "min";
    "max";
    "clamp";
    "round";
    "mod";
    "rem";
    "sin";
    "cos";
    "tan";
    "asin";
    "acos";
    "atan";
    "atan2";
    "pow";
    "sqrt";
    "hypot";
    "log";
    "exp";
    "abs";
    "sign";
  ]

let is_math_function_name name =
  List.mem (String.lowercase_ascii name) math_function_names

let is_plus_or_minus_delim = function
  | Component.Preserved { kind = Token.Delim "+"; _ }
  | Component.Preserved { kind = Token.Delim "-"; _ } ->
      true
  | _ -> false

let strip_math_whitespace comps =
  let rec aux acc = function
    | [] -> List.rev acc
    | (Component.Preserved { kind = Token.Whitespace; _ } as ws) :: rest ->
        let prev_pm =
          match acc with [] -> false | p :: _ -> is_plus_or_minus_delim p
        in
        let next_pm =
          match rest with [] -> false | n :: _ -> is_plus_or_minus_delim n
        in
        if prev_pm || next_pm then aux (ws :: acc) rest else aux acc rest
    | other :: rest -> aux (other :: acc) rest
  in
  aux [] comps

let is_mul_or_div_delim = function
  | Component.Preserved { kind = Token.Delim ("*" | "/"); _ } -> true
  | _ -> false

(* Outside a math function only the whitespace adjacent to a [*] or [/] delim is
   insignificant (CSS Values 4 sec. 10.1): [16 / 9] and [16/9] re-tokenise
   identically wherever the stream is substituted. Every other separator stays
   (a whitespace token between two values is part of the stream). *)
let strip_mul_div_whitespace comps =
  let rec aux acc = function
    | [] -> List.rev acc
    | (Component.Preserved { kind = Token.Whitespace; _ } as ws) :: rest ->
        let prev_md =
          match acc with [] -> false | p :: _ -> is_mul_or_div_delim p
        in
        let next_md =
          match rest with [] -> false | n :: _ -> is_mul_or_div_delim n
        in
        if prev_md || next_md then aux acc rest else aux (ws :: acc) rest
    | other :: rest -> aux (other :: acc) rest
  in
  aux [] comps

(* CSS Values 4 sec. 2.5: [var()] / [env()] / [attr()] substitute a token stream
   textually, so the whitespace next to one is significant - dropping it lets
   the substituted values merge ([var(--a) var(--b)] could become [1px2px]).
   Every other function and every block closes with a hard token boundary ([)],
   []], [}]) that no neighbour can merge across. *)
let is_substitution_func_name name =
  match String.lowercase_ascii name with
  | "var" | "env" | "attr" -> true
  | _ -> false

(* Whitespace immediately after a function or block that closes with a hard
   token boundary is insignificant: [drop-shadow(a) drop-shadow(b)] and
   [calc(45deg*-1) in oklab] re-tokenise identically without it, wherever the
   stream is substituted. Whitespace after a substitution function stays. *)
let strip_after_close_paren comps =
  let closes_hard = function
    | Component.Func wrapped ->
        not (is_substitution_func_name wrapped.Component.node.name)
    | Component.Block _ -> true
    | _ -> false
  in
  let rec aux acc = function
    | [] -> List.rev acc
    | (Component.Preserved { kind = Token.Whitespace; _ } as ws) :: rest ->
        let prev_hard =
          match acc with [] -> false | p :: _ -> closes_hard p
        in
        if prev_hard then aux acc rest else aux (ws :: acc) rest
    | other :: rest -> aux (other :: acc) rest
  in
  aux [] comps

(* [in_math] tracks whether the current component list is inside a math
   function's grammar. It enters at the args of a [calc()] / [min()] / ... call,
   propagates through grouping parens ([Block]s) since those are math operands,
   and turns off when entering a nested non-math function like [var()] which has
   its own grammar. *)
let rec canonicalize_math_whitespace_components ?(in_math = false) comps =
  let comps' =
    List.map
      (fun c ->
        match c with
        | Component.Func wrapped ->
            let func = wrapped.Component.node in
            let nested_in_math = is_math_function_name func.name in
            let args =
              canonicalize_math_whitespace_components ~in_math:nested_in_math
                func.arguments
            in
            Component.Func
              { wrapped with node = { func with arguments = args } }
        | Component.Block wrapped ->
            let block = wrapped.Component.node in
            let value =
              canonicalize_math_whitespace_components ~in_math block.value
            in
            Component.Block { wrapped with node = { block with value } }
        | Component.Preserved _ -> c)
      comps
  in
  let comps' =
    if in_math then strip_math_whitespace comps'
    else strip_mul_div_whitespace comps'
  in
  strip_after_close_paren comps'

(* AST-level value normaliser: applies semantic (equivalence) canonicalisation
   so the optimizer holds a canonical AST and [pp] stays a pure serialiser. Add
   property cases here as their folds migrate out of [pp]; everything else is
   identity. *)
let normalize_font_size (fs : font_size) : font_size =
  match fs with
  | Length l -> preserve_if_equal fs (Length (Values.normalize_length l))
  | Calc c -> (
      match Values.eval_calc c with Values.Val v -> v | folded -> Calc folded)
  | _ -> fs

(* Fold the value-independent parts of these own-typed [calc()]s ([calc(var(--x)
   * 1)] -> [calc(var(--x))], [calc(10 / 2)] -> [5]), keeping any [var()]. The
   printer is a pure serialiser, so these AST-level folds replace the numeric /
   identity reduction the printer did under minify. *)
(* A [<percentage>] operand parsed inside an opacity [calc()] is the number it
   denotes ([Val (Opacity_number f)] = [f]); drop the [Val] wrapper so the
   generic numeric fold combines it like any other number. *)
let rec flatten_opacity_pct (c : opacity Values.calc) : opacity Values.calc =
  match c with
  | Values.Val (Opacity_number f) -> Values.Num f
  | Values.Nested inner -> Values.Nested (flatten_opacity_pct inner)
  | Values.Parens inner -> Values.Parens (flatten_opacity_pct inner)
  | Values.Expr (l, op, r) ->
      Values.Expr (flatten_opacity_pct l, op, flatten_opacity_pct r)
  | _ -> c

let normalize_opacity (o : opacity) : opacity =
  match o with
  | Calc c -> (
      match Values.eval_calc (flatten_opacity_pct c) with
      | Values.Num f -> Opacity_number f
      | Values.Val v -> v
      | folded -> Calc folded)
  | _ -> o

let normalize_line_height (lh : line_height) : line_height =
  match lh with
  | Calc c -> (
      match Values.eval_calc c with
      | Values.Num f -> Num f
      | Values.Val v -> v
      | folded -> Calc folded)
  | _ -> lh

let normalize_vertical_align (va : vertical_align) : vertical_align =
  match va with
  | Calc c -> (
      match Values.eval_calc c with Values.Val v -> v | folded -> Calc folded)
  | _ -> va

let normalize_property_value : type a.
    ?lossless:bool -> ?ctx:Values.calc_ctx -> a property -> a -> a =
 fun ?(lossless = false) ?(ctx = Values.default_calc_ctx) property value ->
  let normalize_color =
    Values.normalize_color ~lossless ~in_feature_query:false
  in
  (* [initial] -> shortest spec-equivalent (e.g. min-width:initial -> auto) is a
     semantic rewrite, so it belongs here, not in pp. *)
  let value = canonical_initial_for_minify property value in
  match property with
  | Transform -> map_preserve normalize_transform value
  | Webkit_transform -> map_preserve normalize_transform value
  | Webkit_border_radius -> normalize_border_radius value
  | Moz_border_radius -> normalize_border_radius value
  | Webkit_box_shadow -> normalize_shadow ~lossless value
  | Moz_box_shadow -> normalize_shadow ~lossless value
  | Rotate -> normalize_rotate value
  | Scale -> normalize_scale value
  | Translate -> normalize_translate_value value
  | Transform_origin -> normalize_transform_origin value
  | Offset_path -> normalize_offset_path value
  | Offset_rotate -> normalize_offset_rotate value
  | Font_style -> normalize_font_style value
  | Width -> Values.normalize_length_percentage ~ctx value
  | Height -> Values.normalize_length_percentage ~ctx value
  | Min_width -> Values.normalize_length_percentage ~ctx value
  | Min_height -> Values.normalize_length_percentage ~ctx value
  | Min_inline_size -> Values.normalize_length_percentage ~ctx value
  | Min_block_size -> Values.normalize_length_percentage ~ctx value
  | Max_width -> Values.normalize_length_percentage ~ctx value
  | Max_height -> Values.normalize_length_percentage ~ctx value
  | Inline_size -> Values.normalize_length_percentage ~ctx value
  | Max_inline_size -> Values.normalize_length_percentage ~ctx value
  | Block_size -> Values.normalize_length_percentage ~ctx value
  | Max_block_size -> Values.normalize_length_percentage ~ctx value
  | Shape_margin -> Values.normalize_length_percentage ~ctx value
  | Offset_distance -> Values.normalize_length_percentage ~ctx value
  | Border_radius -> normalize_border_radius value
  | Background_image ->
      map_preserve (normalize_background_image ~lossless) value
  | Mask_image -> normalize_background_image ~lossless value
  | Webkit_mask_image -> normalize_background_image ~lossless value
  | Border_image_source -> normalize_background_image ~lossless value
  | Background -> map_preserve (normalize_background ~lossless) value
  | Mask -> normalize_mask ~lossless value
  | Clip_path -> normalize_clip_path value
  | Object_view_box -> normalize_object_view_box value
  | Object_position -> normalize_position_value value
  | Perspective_origin -> normalize_position_value value
  | Background_position -> map_preserve normalize_position_value value
  | Mask_position -> map_preserve normalize_position_value value
  | Webkit_mask_position -> map_preserve normalize_position_value value
  | Text_indent -> normalize_text_indent value
  | Animation_range -> normalize_animation_range value
  | View_timeline_inset -> normalize_timeline_inset value
  | Baseline_shift -> normalize_baseline_shift value
  | Background_color -> normalize_color value
  | Color -> normalize_color value
  | Border_color -> map_preserve normalize_color value
  | Border_top_color -> normalize_color value
  | Border_right_color -> normalize_color value
  | Border_bottom_color -> normalize_color value
  | Border_left_color -> normalize_color value
  | Border_inline_start_color -> normalize_color value
  | Border_inline_end_color -> normalize_color value
  | Border_block_start_color -> normalize_color value
  | Border_block_end_color -> normalize_color value
  | Border_inline_color -> normalize_logical_border_color ~lossless value
  | Border_block_color -> normalize_logical_border_color ~lossless value
  | Text_decoration_color -> normalize_color value
  | Webkit_text_decoration_color -> normalize_color value
  | Webkit_tap_highlight_color -> normalize_color value
  | Text_emphasis_color -> normalize_color value
  | Outline_color -> normalize_color value
  | Accent_color -> normalize_color value
  | Caret_color -> normalize_color value
  | Stop_color -> normalize_color value
  | Flood_color -> normalize_color value
  | Lighting_color -> normalize_color value
  | Border -> normalize_border ~lossless value
  | Border_block -> normalize_border ~lossless value
  | Border_block_start -> normalize_border ~lossless value
  | Border_block_end -> normalize_border ~lossless value
  | Border_inline -> normalize_border ~lossless value
  | Border_inline_start -> normalize_border ~lossless value
  | Border_inline_end -> normalize_border ~lossless value
  | Border_top -> normalize_border ~lossless value
  | Border_right -> normalize_border ~lossless value
  | Border_bottom -> normalize_border ~lossless value
  | Border_left -> normalize_border ~lossless value
  | Column_rule -> normalize_border ~lossless value
  | Outline -> normalize_outline ~lossless value
  | Box_shadow -> normalize_shadow ~lossless value
  | Text_shadow -> map_preserve (normalize_text_shadow ~lossless) value
  | Text_decoration -> normalize_text_decoration ~lossless value
  | Webkit_text_decoration -> normalize_text_decoration ~lossless value
  | Text_emphasis -> normalize_text_emphasis ~lossless value
  | Caret -> normalize_caret ~lossless value
  | Fill -> normalize_svg_paint ~lossless value
  | Stroke -> normalize_svg_paint ~lossless value
  | Scrollbar_color -> normalize_scrollbar_color ~lossless value
  | Filter -> normalize_filter ~lossless value
  | Webkit_filter -> normalize_filter ~lossless value
  | Ms_filter -> normalize_filter ~lossless value
  | Backdrop_filter -> normalize_filter ~lossless value
  | Webkit_backdrop_filter -> normalize_filter ~lossless value
  | Flex_grow -> normalize_flex_factor value
  | Stroke_miterlimit -> normalize_stroke_miterlimit value
  | Stroke_dashoffset -> normalize_stroke_dashoffset ~ctx value
  | Stroke_dasharray -> normalize_stroke_dasharray ~ctx value
  | Paint_order -> normalize_paint_order value
  | Vector_effect -> normalize_vector_effect value
  | Flex_shrink -> normalize_flex_factor value
  | Flex_basis -> normalize_flex_basis value
  | Flex -> normalize_flex value
  | Grid_template_columns -> normalize_grid_template value
  | Grid_template_rows -> normalize_grid_template value
  | Grid_template -> normalize_grid_template value
  | Grid -> normalize_grid_template value
  | Grid_auto_columns -> normalize_grid_template value
  | Grid_auto_rows -> normalize_grid_template value
  | Grid_auto_flow -> normalize_grid_auto_flow value
  | Aspect_ratio -> normalize_aspect_ratio value
  | Gap -> normalize_gap value
  | Font_size -> normalize_font_size value
  | Padding_left -> Values.normalize_length ~ctx value
  | Padding_right -> Values.normalize_length ~ctx value
  | Padding_bottom -> Values.normalize_length ~ctx value
  | Padding_top -> Values.normalize_length ~ctx value
  | Padding_inline_start -> Values.normalize_length ~ctx value
  | Padding_inline_end -> Values.normalize_length ~ctx value
  | Padding_block_start -> Values.normalize_length ~ctx value
  | Padding_block_end -> Values.normalize_length ~ctx value
  | Margin_inline_end -> Values.normalize_length ~ctx value
  | Margin_inline_start -> Values.normalize_length ~ctx value
  | Margin_left -> Values.normalize_length ~ctx value
  | Margin_right -> Values.normalize_length ~ctx value
  | Margin_top -> Values.normalize_length ~ctx value
  | Margin_bottom -> Values.normalize_length ~ctx value
  | Margin_block_start -> Values.normalize_length ~ctx value
  | Margin_block_end -> Values.normalize_length ~ctx value
  | Column_gap -> Values.normalize_length ~ctx value
  | Row_gap -> Values.normalize_length ~ctx value
  | Text_underline_offset -> Values.normalize_length ~ctx value
  | Letter_spacing -> Values.normalize_length ~ctx value
  | Border_top_left_radius -> Values.normalize_length ~ctx value
  | Border_top_right_radius -> Values.normalize_length ~ctx value
  | Border_bottom_left_radius -> Values.normalize_length ~ctx value
  | Border_bottom_right_radius -> Values.normalize_length ~ctx value
  | Border_start_start_radius -> Values.normalize_length ~ctx value
  | Border_start_end_radius -> Values.normalize_length ~ctx value
  | Border_end_start_radius -> Values.normalize_length ~ctx value
  | Border_end_end_radius -> Values.normalize_length ~ctx value
  | Outline_width -> Values.normalize_length ~ctx value
  | Outline_offset -> Values.normalize_length ~ctx value
  | Line_height_step -> Values.normalize_length ~ctx value
  | Perspective -> Values.normalize_length ~ctx value
  | Word_spacing -> Values.normalize_length ~ctx value
  | Text_decoration_thickness -> Values.normalize_length ~ctx value
  | Stroke_width -> Values.normalize_length ~ctx value
  | Scroll_margin_top -> Values.normalize_length ~ctx value
  | Scroll_margin_right -> Values.normalize_length ~ctx value
  | Scroll_margin_bottom -> Values.normalize_length ~ctx value
  | Scroll_margin_left -> Values.normalize_length ~ctx value
  | Scroll_margin_inline_start -> Values.normalize_length ~ctx value
  | Scroll_margin_inline_end -> Values.normalize_length ~ctx value
  | Scroll_margin_block_start -> Values.normalize_length ~ctx value
  | Scroll_margin_block_end -> Values.normalize_length ~ctx value
  | Scroll_padding_top -> Values.normalize_length ~ctx value
  | Scroll_padding_right -> Values.normalize_length ~ctx value
  | Scroll_padding_bottom -> Values.normalize_length ~ctx value
  | Scroll_padding_left -> Values.normalize_length ~ctx value
  | Scroll_padding_inline_start -> Values.normalize_length ~ctx value
  | Scroll_padding_inline_end -> Values.normalize_length ~ctx value
  | Scroll_padding_block_start -> Values.normalize_length ~ctx value
  | Scroll_padding_block_end -> Values.normalize_length ~ctx value
  | Padding -> map_preserve (Values.normalize_length ~ctx) value
  | Padding_inline -> map_preserve (Values.normalize_length ~ctx) value
  | Padding_block -> map_preserve (Values.normalize_length ~ctx) value
  | Margin -> map_preserve (Values.normalize_length ~ctx) value
  | Margin_inline -> map_preserve (Values.normalize_length ~ctx) value
  | Margin_block -> map_preserve (Values.normalize_length ~ctx) value
  | Inset -> map_preserve (Values.normalize_length ~ctx) value
  | Inset_inline -> map_preserve (Values.normalize_length ~ctx) value
  | Inset_inline_start -> map_preserve (Values.normalize_length ~ctx) value
  | Inset_inline_end -> map_preserve (Values.normalize_length ~ctx) value
  | Inset_block -> map_preserve (Values.normalize_length ~ctx) value
  | Inset_block_start -> map_preserve (Values.normalize_length ~ctx) value
  | Inset_block_end -> map_preserve (Values.normalize_length ~ctx) value
  | Top -> map_preserve (Values.normalize_length ~ctx) value
  | Right -> map_preserve (Values.normalize_length ~ctx) value
  | Bottom -> map_preserve (Values.normalize_length ~ctx) value
  | Left -> map_preserve (Values.normalize_length ~ctx) value
  | Scroll_margin -> map_preserve (Values.normalize_length ~ctx) value
  | Scroll_margin_inline -> map_preserve (Values.normalize_length ~ctx) value
  | Scroll_margin_block -> map_preserve (Values.normalize_length ~ctx) value
  | Scroll_padding -> map_preserve (Values.normalize_length ~ctx) value
  | Scroll_padding_inline -> map_preserve (Values.normalize_length ~ctx) value
  | Scroll_padding_block -> map_preserve (Values.normalize_length ~ctx) value
  | Custom_property _ -> (
      match value with
      | Custom_value ({ value = Tokens components; _ } as r) ->
          let components' =
            components
            |> canonicalize_custom_colors_components ~lossless
            |> canonicalize_math_whitespace_components
          in
          if components' == components then value
          else Custom_value { r with value = Tokens components' }
      | Custom_value _ -> value)
  | Opacity -> normalize_opacity value
  | Fill_opacity -> normalize_opacity value
  | Stroke_opacity -> normalize_opacity value
  | Stop_opacity -> normalize_opacity value
  | Flood_opacity -> normalize_opacity value
  | Line_height -> normalize_line_height value
  | Vertical_align -> normalize_vertical_align value
  | Border_width -> map_preserve normalize_border_width value
  | Border_top_width -> normalize_border_width value
  | Border_right_width -> normalize_border_width value
  | Border_bottom_width -> normalize_border_width value
  | Border_left_width -> normalize_border_width value
  | Border_inline_start_width -> normalize_border_width value
  | Border_inline_end_width -> normalize_border_width value
  | Border_block_start_width -> normalize_border_width value
  | Border_block_end_width -> normalize_border_width value
  | Border_inline_width -> normalize_logical_border_width value
  | Border_block_width -> normalize_logical_border_width value
  | Transition_duration -> Values.normalize_duration ~ctx value
  | Transition_delay -> Values.normalize_duration ~ctx value
  | Animation_duration -> Values.normalize_duration ~ctx value
  | Animation_delay -> Values.normalize_duration ~ctx value
  | Webkit_transition_duration -> Values.normalize_duration ~ctx value
  | Webkit_transition_delay -> Values.normalize_duration ~ctx value
  | Webkit_animation_duration -> Values.normalize_duration ~ctx value
  | Webkit_animation_delay -> Values.normalize_duration ~ctx value
  | Moz_transition_duration -> Values.normalize_duration ~ctx value
  | Moz_transition_delay -> Values.normalize_duration ~ctx value
  | Moz_animation_duration -> Values.normalize_duration ~ctx value
  | Moz_animation_delay -> Values.normalize_duration ~ctx value
  | _ -> value

let normalize_custom_property_value ?(lossless = false)
    ?(ctx = Values.default_calc_ctx) :
    custom_property_value -> custom_property_value = function
  | Typed { kind = Length; value } ->
      Typed { kind = Length; value = Values.normalize_length ~ctx value }
  | Typed { kind = Color; value } ->
      Typed
        {
          kind = Color;
          value = Values.normalize_color ~lossless ~in_feature_query:false value;
        }
  | Typed { kind = Number; value } ->
      Typed { kind = Number; value = Values.normalize_number ~ctx value }
  | Typed { kind = Percentage; value } ->
      Typed
        { kind = Percentage; value = Values.normalize_percentage ~ctx value }
  | Typed { kind = Length_percentage; value } ->
      Typed
        {
          kind = Length_percentage;
          value = Values.normalize_length_percentage ~ctx value;
        }
  | Typed { kind = Angle; value } ->
      Typed { kind = Angle; value = Values.normalize_angle ~ctx value }
  | Typed { kind = Duration; value } ->
      Typed { kind = Duration; value = Values.normalize_duration ~ctx value }
  | Typed { kind = Gradient_direction; value } ->
      Typed
        {
          kind = Gradient_direction;
          value = normalize_gradient_direction value;
        }
  | Tokens components ->
      Tokens
        (canonicalize_math_whitespace_components
           (canonicalize_custom_colors_components ~lossless components))
  | Typed _ as other -> other

let pp_property_value : type a. (a property * a) Pp.t =
 fun ctx (prop, value) ->
  let pp pp_a = pp_a ctx value in
  match prop with
  | Custom_property _ -> pp pp_custom_property
  | Unknown_property _ ->
      let rendered =
        if Pp.minified ctx then Parser.to_string_minified value
        else Parser.string_of_components value
      in
      Pp.string ctx rendered
  | All -> pp pp_css_wide
  | Background_color -> pp pp_color
  | Color -> pp pp_color
  | Border_color -> pp (pp_box_shorthand pp_color)
  | Border_style -> pp pp_border_style
  | Border_top_style -> pp pp_border_style
  | Border_right_style -> pp pp_border_style
  | Border_bottom_style -> pp pp_border_style
  | Border_left_style -> pp pp_border_style
  | Border_inline_start_style -> pp pp_border_style
  | Border_inline_end_style -> pp pp_border_style
  | Border_block_start_style -> pp pp_border_style
  | Border_block_end_style -> pp pp_border_style
  | Padding -> pp (pp_box_shorthand pp_length)
  | Padding_left -> pp pp_length
  | Padding_right -> pp pp_length
  | Padding_bottom -> pp pp_length
  | Padding_top -> pp pp_length
  | Padding_inline -> pp (pp_box_shorthand pp_length)
  | Padding_inline_start -> pp pp_length
  | Padding_inline_end -> pp pp_length
  | Padding_block -> pp (pp_box_shorthand pp_length)
  | Padding_block_start -> pp pp_length
  | Padding_block_end -> pp pp_length
  | Margin -> pp (pp_box_shorthand pp_length)
  | Margin_inline_end -> pp pp_length
  | Margin_inline_start -> pp pp_length
  | Margin_left -> pp pp_length
  | Margin_right -> pp pp_length
  | Margin_top -> pp pp_length
  | Margin_bottom -> pp pp_length
  | Margin_inline -> pp (Pp.list ~sep:Pp.space pp_length)
  | Margin_block -> pp (Pp.list ~sep:Pp.space pp_length)
  | Margin_block_start -> pp pp_length
  | Margin_block_end -> pp pp_length
  | Gap -> pp pp_gap
  | Column_gap -> pp pp_length
  | Row_gap -> pp pp_length
  | Width -> pp pp_length_percentage
  | Height -> pp pp_length_percentage
  | Min_width -> pp pp_length_min_max
  | Min_height -> pp pp_length_min_max
  | Max_width -> pp pp_length_percentage
  | Max_height -> pp pp_length_percentage
  | Inline_size -> pp pp_length_percentage
  | Min_inline_size -> pp pp_length_min_max
  | Max_inline_size -> pp pp_length_percentage
  | Block_size -> pp pp_length_percentage
  | Min_block_size -> pp pp_length_min_max
  | Max_block_size -> pp pp_length_percentage
  | Font_size -> pp pp_font_size
  | Line_height -> pp pp_line_height
  | Font_weight -> pp pp_font_weight
  | Display -> pp pp_display
  | Position -> pp pp_position
  | Visibility -> pp pp_visibility
  | Baseline_source -> pp pp_baseline_source
  | Alignment_baseline -> pp pp_alignment_baseline
  | Baseline_shift -> pp pp_baseline_shift
  | Align_items -> pp pp_align_items
  | Justify_content -> pp pp_justify_content
  | Justify_items -> pp pp_justify_items
  | Align_self -> pp pp_align_self
  | Border_collapse -> pp pp_border_collapse
  | Table_layout -> pp pp_table_layout
  | Grid_auto_flow -> pp pp_grid_auto_flow
  | Opacity -> pp pp_opacity
  | Fill_opacity -> pp pp_opacity
  | Stroke_opacity -> pp pp_opacity
  | Stop_opacity -> pp pp_opacity
  | Flood_opacity -> pp pp_opacity
  | Mix_blend_mode -> pp pp_blend_mode
  | Z_index -> pp pp_z_index
  | Tab_size -> pp pp_tab_size
  | Zoom -> pp pp_zoom
  | Webkit_line_clamp -> pp pp_webkit_line_clamp
  | Webkit_box_orient -> pp pp_webkit_box_orient
  | Inset -> pp (pp_box_shorthand pp_length)
  | Inset_inline -> pp (pp_box_shorthand pp_length)
  | Inset_inline_start -> pp (Pp.list ~sep:Pp.space pp_length)
  | Inset_inline_end -> pp (Pp.list ~sep:Pp.space pp_length)
  | Inset_block -> pp (pp_box_shorthand pp_length)
  | Inset_block_start -> pp (Pp.list ~sep:Pp.space pp_length)
  | Inset_block_end -> pp (Pp.list ~sep:Pp.space pp_length)
  | Top -> pp (Pp.list ~sep:Pp.space pp_length)
  | Right -> pp (Pp.list ~sep:Pp.space pp_length)
  | Bottom -> pp (Pp.list ~sep:Pp.space pp_length)
  | Left -> pp (Pp.list ~sep:Pp.space pp_length)
  | Border_width -> pp (Pp.list ~sep:Pp.space pp_border_width)
  | Border_top_width -> pp pp_border_width
  | Border_right_width -> pp pp_border_width
  | Border_bottom_width -> pp pp_border_width
  | Border_left_width -> pp pp_border_width
  | Border_inline_start_width -> pp pp_border_width
  | Border_inline_end_width -> pp pp_border_width
  | Border_block_start_width -> pp pp_border_width
  | Border_block_end_width -> pp pp_border_width
  | Border_image -> pp pp_border_image
  | Border_radius -> pp pp_border_radius
  | Border_top_left_radius -> pp pp_length
  | Border_top_right_radius -> pp pp_length
  | Border_bottom_left_radius -> pp pp_length
  | Border_bottom_right_radius -> pp pp_length
  | Border_top_color -> pp pp_color
  | Border_right_color -> pp pp_color
  | Border_bottom_color -> pp pp_color
  | Border_left_color -> pp pp_color
  | Border_inline_start_color -> pp pp_color
  | Border_inline_end_color -> pp pp_color
  | Border_block_start_color -> pp pp_color
  | Border_block_end_color -> pp pp_color
  | Border_inline_color -> pp pp_logical_border_color
  | Border_block_color -> pp pp_logical_border_color
  | Border_inline_width -> pp pp_logical_border_width
  | Border_block_width -> pp pp_logical_border_width
  | Border_inline_style -> pp pp_border_style
  | Border_block_style -> pp pp_border_style
  | Border_start_start_radius -> pp pp_length
  | Border_start_end_radius -> pp pp_length
  | Border_end_start_radius -> pp pp_length
  | Border_end_end_radius -> pp pp_length
  | Text_decoration_color -> pp pp_color
  | Webkit_text_decoration_color -> pp pp_color
  | Webkit_tap_highlight_color -> pp pp_color
  | Webkit_text_fill_color -> pp pp_color
  | Text_indent -> pp pp_text_indent_value
  | Border_spacing -> pp pp_border_spacing
  | Outline_offset -> pp pp_length
  | Perspective -> pp pp_length
  | Transform -> pp pp_transforms
  | Translate -> pp pp_translate_value
  | Isolation -> pp pp_isolation
  | Break_before -> pp pp_break_value
  | Break_after -> pp pp_break_value
  | Break_inside -> pp pp_break_inside_value
  | Page_break_before ->
      if Pp.minified ctx then pp_break_value ctx (break_of_page_break value)
      else pp_page_break_value ctx value
  | Page_break_after ->
      if Pp.minified ctx then pp_break_value ctx (break_of_page_break value)
      else pp_page_break_value ctx value
  | Page_break_inside ->
      if Pp.minified ctx then
        pp_break_inside_value ctx (break_inside_of_page_break value)
      else pp_page_break_inside_value ctx value
  | Page_size -> pp pp_page_size
  | Columns -> pp pp_columns_value
  | Column_width -> pp pp_column_width
  | Column_count -> pp pp_column_count
  | Column_rule -> pp pp_border
  | Column_span -> pp pp_column_span
  | Transform_style -> pp pp_transform_style
  | Backface_visibility -> pp pp_backface_visibility
  | Scroll_snap_align -> pp pp_scroll_snap_align
  | Scroll_snap_stop -> pp pp_scroll_snap_stop
  | Scroll_behavior -> pp pp_scroll_behavior
  | Box_sizing -> pp pp_box_sizing
  | Webkit_box_sizing -> pp pp_box_sizing
  | Moz_box_sizing -> pp pp_box_sizing
  | Field_sizing -> pp pp_field_sizing
  | Caption_side -> pp pp_caption_side
  | Resize -> pp pp_resize
  | Object_fit -> pp pp_object_fit
  | Object_view_box -> pp pp_object_view_box
  | Appearance -> pp pp_appearance
  | Color_scheme -> pp pp_color_scheme
  | Print_color_adjust -> pp pp_print_color_adjust
  | Webkit_print_color_adjust -> pp pp_print_color_adjust
  | Box_decoration_break -> pp pp_box_decoration_break
  | Webkit_box_decoration_break -> pp pp_box_decoration_break
  | Flex_grow -> pp pp_flex_factor
  | Flex_shrink -> pp pp_flex_factor
  | Order -> pp pp_order
  | Flex_direction -> pp pp_flex_direction
  | Flex_wrap -> pp pp_flex_wrap
  | Flex_flow -> pp pp_flex_flow
  | Font_style -> pp pp_font_style
  | Text_align -> pp pp_text_align
  | Text_decoration -> pp pp_text_decoration
  | Text_decoration_line -> pp (Pp.list ~sep:Pp.space pp_text_decoration_line)
  | Text_decoration_style -> pp pp_text_decoration_style
  | Text_decoration_skip -> pp pp_text_decoration_skip
  | Text_decoration_skip_self -> pp pp_text_decoration_skip_self
  | Text_decoration_skip_box -> pp pp_text_decoration_skip_box
  | Text_decoration_skip_inset -> pp pp_text_decoration_skip_inset
  | Text_decoration_skip_spaces -> pp pp_text_decoration_skip_spaces
  | Text_emphasis -> pp pp_text_emphasis
  | Text_emphasis_style -> pp pp_text_emphasis_style
  | Text_emphasis_color -> pp pp_color
  | Text_emphasis_position -> pp pp_text_emphasis_position
  | Text_emphasis_skip -> pp pp_text_emphasis_skip
  | Text_orientation -> pp pp_text_orientation
  | Text_transform -> pp pp_text_transform
  | List_style_type -> pp pp_list_style_type
  | List_style_position -> pp pp_list_style_position
  | List_style_image -> pp pp_list_style_image
  | Overflow -> pp pp_overflow
  | Overflow_x -> pp pp_overflow
  | Overflow_y -> pp pp_overflow
  | Overflow_block -> pp pp_overflow
  | Overflow_inline -> pp pp_overflow
  | Vertical_align -> pp pp_vertical_align
  | Text_overflow -> pp pp_text_overflow
  | Text_wrap -> pp pp_text_wrap
  | Word_break -> pp pp_word_break
  | Overflow_wrap -> pp pp_overflow_wrap
  | Line_break -> pp pp_line_break
  | Hyphens -> pp pp_hyphens
  | Webkit_hyphens -> pp pp_hyphens
  | Font_stretch -> pp pp_font_stretch
  | Font_optical_sizing -> pp pp_font_optical_sizing
  | Font_kerning -> pp pp_font_kerning
  | Font_language_override -> pp pp_font_language_override
  | Font_synthesis_style -> pp pp_font_synthesis_style
  | Font_synthesis_weight -> pp pp_font_synthesis_weight
  | Font_synthesis_small_caps -> pp pp_font_synthesis_small_caps
  | Font_synthesis_position -> pp pp_font_synthesis_position
  | Font_variant_ligatures -> pp pp_font_variant_ligatures
  | Caps -> pp pp_font_variant_caps
  | Numeric -> pp pp_font_variant_numeric
  | Font_variant_position -> pp pp_font_variant_position
  | East_asian -> pp pp_font_variant_east_asian
  | Webkit_font_smoothing -> pp pp_webkit_font_smoothing
  | Scroll_snap_type -> pp pp_scroll_snap_type
  | Container_type -> pp pp_container_type
  | Container -> pp pp_container_shorthand
  | White_space -> pp pp_white_space
  | Grid_template_columns -> pp pp_grid_template
  | Grid_template_rows -> pp pp_grid_template
  | Grid_template_areas -> pp pp_grid_template_areas
  | Grid_template -> pp pp_grid_template
  | Grid -> pp pp_grid_template
  | Grid_area -> pp pp_grid_area
  | Grid_auto_columns -> pp pp_grid_template
  | Grid_auto_rows -> pp pp_grid_template
  | Flex -> pp pp_flex
  | Flex_basis -> pp pp_flex_basis
  | Align_content -> pp pp_align_content
  | Justify_self -> pp pp_justify_self
  | Place_content -> pp pp_place_content
  | Place_items -> pp pp_place_items
  | Place_self ->
      pp (fun ctx (a, j) ->
          pp_align_self ctx a;
          (* Tailwind's minifier quirk: outputs single value for most cases, but
             expands stretch to two values *)
          let needs_second_value =
            match (a, j) with
            | Stretch, Stretch -> false
            | Auto, Auto -> false
            | Normal, Normal -> false
            | Baseline, Baseline -> false
            | First_baseline, First_baseline -> false
            | Last_baseline, Last_baseline -> false
            | Center, Center -> false
            | Start, Start -> false
            | End, End -> false
            | Self_start, Self_start -> false
            | Self_end, Self_end -> false
            | Flex_start, Flex_start -> false
            | Flex_end, Flex_end -> false
            | Safe_center, Safe_center -> false
            | Safe_start, Safe_start -> false
            | Safe_end, Safe_end -> false
            | Safe_flex_start, Safe_flex_start -> false
            | Safe_flex_end, Safe_flex_end -> false
            | Unsafe_center, Unsafe_center -> false
            | Unsafe_start, Unsafe_start -> false
            | Unsafe_end, Unsafe_end -> false
            | Inherit, Inherit -> false
            | Initial, Initial -> false
            | Unset, Unset -> false
            | Revert, Revert -> false
            | Revert_layer, Revert_layer -> false
            | _ -> true (* Different values always need both *)
          in
          if needs_second_value then (
            Pp.space ctx ();
            pp_justify_self ctx j))
  | Grid_column -> pp pp_grid_line_pair
  | Grid_row -> pp pp_grid_line_pair
  | Grid_column_start -> pp pp_grid_line
  | Grid_column_end -> pp pp_grid_line
  | Grid_row_start -> pp pp_grid_line
  | Grid_row_end -> pp pp_grid_line
  | Text_underline_offset -> pp pp_length
  | Background_position -> pp pp_background_position
  | Background_repeat -> pp pp_background_repeat
  | Background_size -> pp pp_background_size
  | Moz_osx_font_smoothing -> pp pp_moz_osx_font_smoothing
  | Backdrop_filter -> pp pp_filter
  | Webkit_backdrop_filter -> pp pp_filter
  | Webkit_mask_image -> pp pp_background_image
  | Webkit_mask_composite -> pp pp_webkit_mask_composite
  | Webkit_mask_source_type -> pp pp_webkit_mask_source_type
  | Webkit_mask_size -> pp pp_background_size
  | Webkit_mask_position -> pp pp_background_position
  | Webkit_mask_repeat -> pp pp_background_repeat
  | Webkit_mask_clip -> pp pp_mask_box
  | Webkit_mask_origin -> pp pp_mask_box
  | Border_image_source -> pp pp_background_image
  | Border_image_slice -> pp pp_border_image_slice
  | Border_image_repeat -> pp pp_border_image_repeat
  | Border_image_width -> pp pp_border_image_width
  | Border_image_outset -> pp pp_border_image_outset
  | Mask_image -> pp pp_background_image
  | Mask_composite -> pp pp_mask_composite
  | Mask_mode -> pp pp_mask_mode
  | Mask_size -> pp pp_background_size
  | Mask_position -> pp pp_background_position
  | Mask_repeat -> pp pp_background_repeat
  | Mask_clip -> pp pp_mask_box
  | Mask_origin -> pp pp_mask_box
  | Mask_type -> pp pp_mask_type
  | Mask -> pp pp_mask
  | Container_name -> pp pp_container_name
  | Anchor_name -> pp pp_anchor_name
  | Position_anchor -> pp pp_position_anchor
  | Position_try_fallbacks -> pp pp_position_try_fallbacks
  | Position_try_order -> pp pp_position_try_order
  | Position_try -> pp pp_position_try
  | Position_visibility -> pp pp_position_visibility
  | Position_area -> pp pp_position_area
  | Shape_outside -> pp Pp.string
  | Shape_margin -> pp (pp_length_percentage ~always:true)
  | Shape_image_threshold -> pp pp_shape_image_threshold
  | Overflow_clip_margin -> pp pp_overflow_clip_margin
  | Overflow_anchor -> pp pp_overflow_anchor
  | Scrollbar_width -> pp pp_scrollbar_width
  | Scrollbar_color -> pp pp_scrollbar_color
  | Scrollbar_gutter -> pp pp_scrollbar_gutter
  | Line_height_step -> pp (pp_length ~always:true)
  | Font_palette -> pp pp_font_palette
  | Font_synthesis -> pp pp_font_synthesis
  | Text_wrap_mode -> pp pp_text_wrap_mode
  | Text_wrap_style -> pp pp_text_wrap_style
  | Text_box_trim -> pp pp_text_box_trim
  | Text_underline_position -> pp pp_text_underline_position
  | Text_box_edge -> pp pp_text_box_edge
  | Text_box -> pp pp_text_box
  | Inline_sizing -> pp pp_inline_sizing
  | Line_fit_edge -> pp pp_line_fit_edge
  | Interpolate_size -> pp pp_interpolate_size
  | Min_intrinsic_sizing -> pp pp_min_intrinsic_sizing
  | Ruby_align -> pp pp_ruby_align
  | Ruby_merge -> pp pp_ruby_merge
  | Ruby_overhang -> pp pp_ruby_overhang
  | Ruby_position -> pp pp_ruby_position
  | Glyph_orientation_vertical -> pp pp_glyph_orientation_vertical
  | Animation_timeline -> pp pp_animation_timeline
  | Animation_range -> pp pp_animation_range
  | Animation_range_start -> pp pp_animation_range_item
  | Animation_range_end -> pp pp_animation_range_item
  | Scroll_timeline -> pp pp_timeline_shorthand
  | Scroll_timeline_name -> pp pp_timeline_name
  | Scroll_timeline_axis -> pp pp_timeline_axis
  | View_transition_name -> pp pp_view_transition_name
  | View_transition_class -> pp pp_view_transition_class
  | Image_orientation -> pp pp_image_orientation
  | Image_rendering -> pp pp_image_rendering
  | Image_resolution -> pp pp_image_resolution
  | Contain_intrinsic_size -> pp pp_contain_intrinsic_size
  | Contain_intrinsic_width -> pp pp_contain_intrinsic_longhand
  | Contain_intrinsic_height -> pp pp_contain_intrinsic_longhand
  | Contain_intrinsic_block_size -> pp pp_contain_intrinsic_longhand
  | Contain_intrinsic_inline_size -> pp pp_contain_intrinsic_longhand
  | Margin_trim -> pp pp_margin_trim
  | Offset_path -> pp pp_offset_path
  | Offset_distance -> pp (pp_length_percentage ~always:true)
  | Offset_rotate -> pp pp_offset_rotate
  | Font_size_adjust -> pp pp_font_size_adjust
  | Font_variant_emoji -> pp pp_font_variant_emoji
  | Text_spacing_trim -> pp pp_text_spacing_trim
  | Hyphenate_limit_chars -> pp pp_hyphenate_limit_chars
  | Initial_letter -> pp pp_initial_letter
  | Initial_letter_align -> pp pp_initial_letter_align
  | Initial_letter_wrap -> pp pp_initial_letter_wrap
  | Dominant_baseline -> pp pp_dominant_baseline
  | View_timeline_name -> pp pp_timeline_name
  | View_timeline_axis -> pp pp_timeline_axis
  | View_timeline_inset -> pp pp_timeline_inset
  | View_timeline -> pp pp_timeline_shorthand
  | Timeline_scope -> pp pp_timeline_name
  | Perspective_origin -> pp pp_perspective_origin
  | Object_position -> pp pp_position_value
  | Rotate -> pp pp_rotate_value
  | Transition_duration -> pp pp_duration
  | Transition_timing_function -> pp pp_timing_function
  | Transition_delay -> pp pp_duration
  | Transition_property -> pp pp_transition_property
  | Transition_behavior -> pp pp_transition_behavior
  | Overlay -> pp pp_overlay
  | Will_change -> pp pp_will_change
  | Contain -> pp pp_contain
  | Word_spacing -> pp pp_length
  | Background_attachment -> pp pp_background_attachment
  | Border_top -> pp pp_border
  | Border_right -> pp pp_border
  | Border_bottom -> pp pp_border
  | Border_left -> pp pp_border
  | Transform_origin -> pp pp_transform_origin
  | Transform_box -> pp pp_transform_box
  | Text_shadow -> pp (Pp.list ~sep:Pp.comma pp_text_shadow)
  | Clip_path -> pp pp_clip_path
  | Mask_border -> pp pp_border_image
  | Content_visibility -> pp pp_content_visibility
  | Filter -> pp pp_filter
  | Background_image -> pp (Pp.list ~sep:Pp.comma pp_background_image)
  | Background_origin -> pp pp_background_box
  | Background_clip -> pp pp_background_box
  | Webkit_background_clip -> pp pp_background_box
  | Animation -> pp (Pp.list ~sep:Pp.comma pp_animation)
  | Aspect_ratio -> pp pp_aspect_ratio
  | Content -> pp pp_content
  | Counter_reset -> pp pp_counter_set
  | Counter_increment -> pp pp_counter_set
  | Quotes -> pp pp_quotes
  | Box_shadow -> pp pp_shadow
  | Fill -> pp pp_svg_paint
  | Stroke -> pp pp_svg_paint
  | Stroke_width -> pp pp_length
  | Transition -> pp (Pp.list ~sep:Pp.comma pp_transition)
  | Scale -> pp pp_scale
  | Outline -> pp pp_outline
  | Outline_style -> pp pp_outline_style
  | Outline_width -> pp pp_length
  | Outline_color -> pp pp_color
  | Forced_color_adjust -> pp pp_forced_color_adjust
  | Clip -> pp pp_clip
  | Clear -> pp pp_clear
  | Float -> pp pp_float_side
  | Border -> pp pp_border
  | Border_block -> pp pp_border
  | Border_block_start -> pp pp_border
  | Border_block_end -> pp pp_border
  | Border_inline -> pp pp_border
  | Border_inline_start -> pp pp_border
  | Border_inline_end -> pp pp_border
  | Background -> pp (Pp.list ~sep:Pp.comma pp_background)
  | Text_decoration_thickness -> pp pp_length
  | Text_size_adjust -> pp pp_text_size_adjust
  | Touch_action -> pp pp_touch_action
  | Direction -> pp pp_direction
  | Fill_rule -> pp pp_fill_rule
  | Clip_rule -> pp pp_fill_rule
  | Stroke_linecap -> pp pp_stroke_linecap
  | Stroke_linejoin -> pp pp_stroke_linejoin
  | Stroke_miterlimit -> pp pp_stroke_miterlimit
  | Stroke_dashoffset -> pp pp_stroke_dashoffset
  | Stroke_dasharray -> pp pp_stroke_dasharray
  | Paint_order -> pp pp_paint_order
  | Vector_effect -> pp pp_vector_effect
  | Unicode_bidi -> pp pp_unicode_bidi
  | Writing_mode -> pp pp_writing_mode
  | Text_combine_upright -> pp pp_text_combine_upright
  | Text_decoration_skip_ink -> pp pp_text_decoration_skip_ink
  | Animation_name -> pp pp_animation_name
  | Animation_duration -> pp pp_duration
  | Animation_timing_function -> pp pp_timing_function
  | Animation_delay -> pp pp_duration
  | Animation_iteration_count -> pp pp_animation_iteration_count
  | Animation_direction -> pp pp_animation_direction
  | Animation_fill_mode -> pp pp_animation_fill_mode
  | Animation_play_state -> pp pp_animation_play_state
  | Animation_composition -> pp pp_animation_composition
  | Background_blend_mode -> pp (Pp.list ~sep:Pp.comma pp_blend_mode)
  | Scroll_margin -> pp (Pp.list ~sep:Pp.space (pp_length ~always:true))
  | Scroll_margin_top -> pp pp_length
  | Scroll_margin_right -> pp pp_length
  | Scroll_margin_bottom -> pp pp_length
  | Scroll_margin_left -> pp pp_length
  | Scroll_margin_inline -> pp (Pp.list ~sep:Pp.space (pp_length ~always:true))
  | Scroll_margin_inline_start -> pp pp_length
  | Scroll_margin_inline_end -> pp pp_length
  | Scroll_margin_block -> pp (Pp.list ~sep:Pp.space (pp_length ~always:true))
  | Scroll_margin_block_start -> pp pp_length
  | Scroll_margin_block_end -> pp pp_length
  | Scroll_padding -> pp (Pp.list ~sep:Pp.space (pp_length ~always:true))
  | Scroll_padding_top -> pp pp_length
  | Scroll_padding_right -> pp pp_length
  | Scroll_padding_bottom -> pp pp_length
  | Scroll_padding_left -> pp pp_length
  | Scroll_padding_inline -> pp (Pp.list ~sep:Pp.space (pp_length ~always:true))
  | Scroll_padding_inline_start -> pp pp_length
  | Scroll_padding_inline_end -> pp pp_length
  | Scroll_padding_block -> pp (Pp.list ~sep:Pp.space (pp_length ~always:true))
  | Scroll_padding_block_start -> pp pp_length
  | Scroll_padding_block_end -> pp pp_length
  | Overscroll_behavior -> pp (Pp.list ~sep:Pp.space pp_overscroll_behavior)
  | Overscroll_behavior_x -> pp pp_overscroll_behavior
  | Overscroll_behavior_y -> pp pp_overscroll_behavior
  | Overscroll_behavior_block -> pp pp_overscroll_behavior
  | Overscroll_behavior_inline -> pp pp_overscroll_behavior
  | Accent_color -> pp pp_color
  | Caret_color -> pp pp_color
  | Stop_color -> pp pp_color
  | Flood_color -> pp pp_color
  | Lighting_color -> pp pp_color
  | List_style -> pp pp_list_style
  | Font -> pp pp_font
  | Source -> pp pp_font_src
  | Webkit_appearance -> pp pp_webkit_appearance
  | Letter_spacing -> pp pp_length
  | Cursor -> pp pp_cursor
  | Interactivity -> pp pp_interactivity
  | Caret_animation -> pp pp_caret_animation
  | Caret_shape -> pp pp_caret_shape
  | Caret -> pp pp_caret
  | Interest_delay -> pp pp_interest_delay
  | Interest_delay_start -> pp pp_interest_delay
  | Interest_delay_end -> pp pp_interest_delay
  | Nav_up -> pp pp_nav
  | Nav_right -> pp pp_nav
  | Nav_down -> pp pp_nav
  | Nav_left -> pp pp_nav
  | Pointer_events -> pp pp_pointer_events
  | User_select -> pp pp_user_select
  | Webkit_user_select -> pp pp_user_select
  | Ms_user_select -> pp pp_user_select
  | Moz_user_select -> pp pp_user_select
  | Font_feature_settings -> pp pp_font_feature_settings
  | Font_variation_settings -> pp pp_font_variation_settings
  | Webkit_text_decoration -> pp pp_text_decoration
  | Webkit_text_size_adjust -> pp pp_text_size_adjust
  | Webkit_transform -> pp (Pp.list ~sep:Pp.space pp_transform)
  | Moz_transform -> pp (Pp.list ~sep:Pp.space pp_transform)
  | Ms_transform -> pp (Pp.list ~sep:Pp.space pp_transform)
  | O_transform -> pp (Pp.list ~sep:Pp.space pp_transform)
  | Webkit_transition -> pp (Pp.list ~sep:Pp.comma pp_transition)
  | Webkit_transition_delay -> pp pp_duration
  | Webkit_transition_duration -> pp pp_duration
  | Webkit_transition_property -> pp pp_transition_property
  | Webkit_transition_timing_function -> pp pp_timing_function
  | Webkit_animation -> pp (Pp.list ~sep:Pp.comma pp_animation)
  | Webkit_animation_delay -> pp pp_duration
  | Webkit_animation_duration -> pp pp_duration
  | Webkit_animation_direction -> pp pp_animation_direction
  | Webkit_animation_iteration_count -> pp pp_animation_iteration_count
  | Webkit_animation_name -> pp pp_animation_name
  | Webkit_animation_timing_function -> pp pp_timing_function
  | Webkit_animation_fill_mode -> pp pp_animation_fill_mode
  | Webkit_animation_play_state -> pp pp_animation_play_state
  | Webkit_flex_direction -> pp pp_flex_direction
  | Webkit_flex_wrap -> pp pp_flex_wrap
  | Webkit_flex_flow -> pp pp_flex_flow
  | Webkit_justify_content -> pp pp_justify_content
  | Webkit_align_items -> pp pp_align_items
  | Webkit_align_content -> pp pp_align_content
  | Webkit_align_self -> pp pp_align_self
  | Webkit_border_radius -> pp pp_border_radius
  | Webkit_box_shadow -> pp pp_shadow
  | Webkit_background_size -> pp pp_background_size
  | Webkit_filter -> pp pp_filter
  | Moz_appearance -> pp pp_appearance
  | Moz_animation -> pp (Pp.list ~sep:Pp.comma pp_animation)
  | Moz_animation_delay -> pp pp_duration
  | Moz_animation_duration -> pp pp_duration
  | Moz_animation_direction -> pp pp_animation_direction
  | Moz_animation_iteration_count -> pp pp_animation_iteration_count
  | Moz_animation_name -> pp pp_animation_name
  | Moz_animation_timing_function -> pp pp_timing_function
  | Moz_animation_fill_mode -> pp pp_animation_fill_mode
  | Moz_animation_play_state -> pp pp_animation_play_state
  | Moz_transition -> pp (Pp.list ~sep:Pp.comma pp_transition)
  | Moz_transition_delay -> pp pp_duration
  | Moz_transition_duration -> pp pp_duration
  | Moz_transition_property -> pp pp_transition_property
  | Moz_transition_timing_function -> pp pp_timing_function
  | Moz_border_radius -> pp pp_border_radius
  | Moz_box_shadow -> pp pp_shadow
  | Moz_orient -> pp pp_moz_orient
  | Ms_filter -> pp pp_filter
  | O_transition -> pp (Pp.list ~sep:Pp.comma pp_transition)
  | Font_family -> pp pp_font_family

(* Cascade detected the value is spec-invalid (an [Invalid] arm in one of the
   typed value types). The minify-time [Optimize.drop_invalid] pass uses this to
   discard the declaration. *)
let invalid_angle : angle -> bool = function Invalid _ -> true | _ -> false

let invalid_length_percentage : length_percentage -> bool = function
  | Invalid _ -> true
  | _ -> false

let invalid_rotate_value : rotate_value -> bool = function
  | Angle a | X a | Y a | Z a | Axis (_, _, _, a) -> invalid_angle a
  | _ -> false

let invalid_clip_path : clip_path -> bool = function
  | Invalid _ -> true
  | _ -> false

let invalid_text_indent_value : text_indent_value -> bool = function
  | Indent { length; _ } -> invalid_length_percentage length
  | _ -> false

let is_invalid_value : type a. a property -> a -> bool =
 fun prop value ->
  match prop with
  | Rotate -> invalid_rotate_value value
  | Width -> invalid_length_percentage value
  | Height -> invalid_length_percentage value
  | Min_width -> invalid_length_percentage value
  | Min_height -> invalid_length_percentage value
  | Max_width -> invalid_length_percentage value
  | Max_height -> invalid_length_percentage value
  | Block_size -> invalid_length_percentage value
  | Inline_size -> invalid_length_percentage value
  | Min_block_size -> invalid_length_percentage value
  | Min_inline_size -> invalid_length_percentage value
  | Max_block_size -> invalid_length_percentage value
  | Max_inline_size -> invalid_length_percentage value
  | Clip_path -> invalid_clip_path value
  | Text_indent -> invalid_text_indent_value value
  | Font_family -> ( match value with Invalid _ -> true | _ -> false)
  | _ -> false

let property_value_kind : type a. a property -> a property_value_kind option =
  function
  | Padding_left -> Some Length
  | Padding_right -> Some Length
  | Padding_bottom -> Some Length
  | Padding_top -> Some Length
  | Padding_inline -> Some Lengths
  | Padding_inline_start -> Some Length
  | Padding_inline_end -> Some Length
  | Padding_block -> Some Lengths
  | Padding_block_start -> Some Length
  | Padding_block_end -> Some Length
  | Margin_inline_end -> Some Length
  | Margin_inline_start -> Some Length
  | Margin_left -> Some Length
  | Margin_right -> Some Length
  | Margin_top -> Some Length
  | Margin_bottom -> Some Length
  | Margin_block_start -> Some Length
  | Margin_block_end -> Some Length
  | Column_gap -> Some Length
  | Row_gap -> Some Length
  | Text_underline_offset -> Some Length
  | Letter_spacing -> Some Length
  | Border_top_left_radius -> Some Length
  | Border_top_right_radius -> Some Length
  | Border_bottom_left_radius -> Some Length
  | Border_bottom_right_radius -> Some Length
  | Border_start_start_radius -> Some Length
  | Border_start_end_radius -> Some Length
  | Border_end_start_radius -> Some Length
  | Border_end_end_radius -> Some Length
  | Outline_width -> Some Length
  | Border_top_width -> Some Border_width
  | Border_right_width -> Some Border_width
  | Border_bottom_width -> Some Border_width
  | Border_left_width -> Some Border_width
  | Border_inline_start_width -> Some Border_width
  | Border_inline_end_width -> Some Border_width
  | Border_block_start_width -> Some Border_width
  | Border_block_end_width -> Some Border_width
  | Outline_offset -> Some Length
  | Text_indent -> None
  | Line_height_step -> Some Length
  | Perspective -> Some Length
  | Text_decoration_thickness -> Some Length
  | Stroke_width -> Some Length
  | Scroll_margin_top -> Some Length
  | Scroll_margin_right -> Some Length
  | Scroll_margin_bottom -> Some Length
  | Scroll_margin_left -> Some Length
  | Scroll_margin_inline_start -> Some Length
  | Scroll_margin_inline_end -> Some Length
  | Scroll_margin_block_start -> Some Length
  | Scroll_margin_block_end -> Some Length
  | Scroll_padding_top -> Some Length
  | Scroll_padding_right -> Some Length
  | Scroll_padding_bottom -> Some Length
  | Scroll_padding_left -> Some Length
  | Scroll_padding_inline_start -> Some Length
  | Scroll_padding_inline_end -> Some Length
  | Scroll_padding_block_start -> Some Length
  | Scroll_padding_block_end -> Some Length
  | Padding -> Some Lengths
  | Margin -> Some Lengths
  | Margin_inline -> Some Lengths
  | Margin_block -> Some Lengths
  | Inset -> Some Lengths
  | Inset_inline -> Some Lengths
  | Inset_inline_start -> Some Lengths
  | Inset_inline_end -> Some Lengths
  | Inset_block -> Some Lengths
  | Inset_block_start -> Some Lengths
  | Inset_block_end -> Some Lengths
  | Top -> Some Lengths
  | Right -> Some Lengths
  | Bottom -> Some Lengths
  | Left -> Some Lengths
  | Border_width -> Some Border_widths
  | Scroll_margin -> Some Lengths
  | Scroll_margin_inline -> Some Lengths
  | Scroll_margin_block -> Some Lengths
  | Scroll_padding -> Some Lengths
  | Scroll_padding_inline -> Some Lengths
  | Scroll_padding_block -> Some Lengths
  | Width -> Some Length_percentage
  | Height -> Some Length_percentage
  | Min_width -> Some Length_percentage
  | Min_height -> Some Length_percentage
  | Max_width -> Some Length_percentage
  | Max_height -> Some Length_percentage
  | Inline_size -> Some Length_percentage
  | Min_inline_size -> Some Length_percentage
  | Max_inline_size -> Some Length_percentage
  | Block_size -> Some Length_percentage
  | Min_block_size -> Some Length_percentage
  | Max_block_size -> Some Length_percentage
  | Shape_margin -> Some Length_percentage
  | Font_size -> Some Font_size
  | Opacity -> Some Opacity
  | Fill_opacity -> Some Opacity
  | Stroke_opacity -> Some Opacity
  | Stop_opacity -> Some Opacity
  | Flood_opacity -> Some Opacity
  | Rotate -> Some Rotate
  | Animation_duration -> Some Duration
  | Animation_delay -> Some Duration
  | Webkit_animation_duration -> Some Duration
  | Webkit_animation_delay -> Some Duration
  | Moz_animation_duration -> Some Duration
  | Moz_animation_delay -> Some Duration
  | Transition_duration -> Some Duration
  | Transition_delay -> Some Duration
  | Webkit_transition_duration -> Some Duration
  | Webkit_transition_delay -> Some Duration
  | Moz_transition_duration -> Some Duration
  | Moz_transition_delay -> Some Duration
  | Display -> Some Display
  | Position -> Some Position
  | Visibility -> Some Visibility
  | Clear -> Some Clear
  | Float -> Some Float
  | Scale -> Some Scale
  | Translate -> Some Translate
  | Transform -> Some Transform
  | Webkit_transform -> Some Transform
  | Animation -> Some (Animation : animation list property_value_kind)
  | Webkit_animation -> Some (Animation : animation list property_value_kind)
  | Moz_animation -> Some (Animation : animation list property_value_kind)
  | Transition -> Some (Transition : transition list property_value_kind)
  | Webkit_transition -> Some (Transition : transition list property_value_kind)
  | Moz_transition -> Some (Transition : transition list property_value_kind)
  | O_transition -> Some (Transition : transition list property_value_kind)
  | Filter -> Some Filter
  | Backdrop_filter -> Some Filter
  | Webkit_backdrop_filter -> Some Filter
  | Webkit_filter -> Some Filter
  | Ms_filter -> Some Filter
  | Box_shadow -> Some Shadow
  | Webkit_box_shadow -> Some Shadow
  | Moz_box_shadow -> Some Shadow
  | Border_radius -> Some Border_radius
  | Webkit_border_radius -> Some Border_radius
  | Moz_border_radius -> Some Border_radius
  | Offset_distance -> Some Length_percentage
  | Background_color -> Some Color
  | Animation_name -> Some Animation_name
  | Webkit_animation_name -> Some Animation_name
  | Moz_animation_name -> Some Animation_name
  | Color -> Some Color
  | Border_color -> Some Colors
  | Text_decoration_color -> Some Color
  | Border_top_color -> Some Color
  | Border_right_color -> Some Color
  | Border_bottom_color -> Some Color
  | Border_left_color -> Some Color
  | Outline_color -> Some Color
  | Webkit_tap_highlight_color -> Some Color
  | Webkit_text_decoration_color -> Some Color
  | Accent_color -> Some Color
  | Caret_color -> Some Color
  | Stop_color -> Some Color
  | Flood_color -> Some Color
  | Lighting_color -> Some Color
  | Background_image -> Some Background_images
  | Background -> Some Background
  | Webkit_mask_image -> Some Background_image
  | Border_image_source -> Some Background_image
  | Mask_image -> Some Background_image
  | Source -> Some Font_src
  | Font_family -> Some Font_family
  | _ -> None

(* ===== Readers moved here from Declaration so the API consistency script can
   surface them in [properties.mli]. ===== *)

let rec read_font_variant_emoji t : font_variant_emoji =
  Cursor.enum_or_var "font-variant-emoji"
    [
      ("normal", (Normal : font_variant_emoji));
      ("text", Text);
      ("emoji", Emoji);
      ("unicode", Unicode);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_font_variant_emoji t))
    t

let read_initial_letter_align_keyword t : initial_letter_align_keyword =
  Cursor.enum "initial-letter-align"
    [
      ("alphabetic", (Alphabetic : initial_letter_align_keyword));
      ("ideographic", Ideographic);
      ("hanging", Hanging);
      ("leading", Leading);
      ("border-box", Border_box);
    ]
    t

let read_font_size_adjust_metric t : font_size_adjust_metric =
  Cursor.enum "font-size-adjust metric"
    [
      ("ex-height", (Ex_height : font_size_adjust_metric));
      ("cap-height", Cap_height);
      ("ch-width", Ch_width);
      ("ic-width", Ic_width);
      ("ic-height", Ic_height);
    ]
    t

let read_animation_range_name t : animation_range_name =
  Cursor.enum "animation-range name"
    [
      ("cover", Cover);
      ("contain", Contain);
      ("entry", Entry);
      ("exit", Exit);
      ("entry-crossing", Entry_crossing);
      ("exit-crossing", Exit_crossing);
    ]
    t

let rec read_font_size_adjust t : font_size_adjust =
  let read_non_negative_number t =
    let n = Cursor.number t in
    if n < 0. then Cursor.err_invalid t "font-size-adjust must be non-negative";
    n
  in
  let read_metric_value t =
    let metric = read_font_size_adjust_metric t in
    Cursor.ws t;
    match Cursor.peek_ident t with
    | Some "from-font" ->
        let _ = Cursor.ident t in
        Metric_from_font metric
    | _ -> Metric_number (metric, read_non_negative_number t)
  in
  Cursor.enum_or_var "font-size-adjust"
    [
      ("none", (None : font_size_adjust));
      ("from-font", From_font);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_font_size_adjust t))
    ~default:(fun t ->
      match Cursor.peek_ident t with
      | Some _ -> read_metric_value t
      | None -> Number (read_non_negative_number t))
    t

let rec read_initial_letter t : initial_letter =
  let read_number t =
    let size = Cursor.number t in
    if size < 1. then Cursor.err_invalid t "initial-letter size must be >= 1";
    Cursor.ws t;
    if Cursor.is_done t || Cursor.peek_semicolon t then
      (Size size : initial_letter)
    else
      let sink = Cursor.int t in
      if sink < 1 then Cursor.err_invalid t "initial-letter sink must be >= 1";
      Cursor.ws t;
      Cursor.expect_eof t;
      Size_sink (size, sink)
  in
  Cursor.enum_or_calls "initial-letter"
    [
      ("normal", (Normal : initial_letter));
      ("drop", Drop);
      ("raise", Raise);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (Values.read_var read_initial_letter t)) ]
    ~default:read_number t

let rec read_initial_letter_align t : initial_letter_align =
  Cursor.enum_or_var "initial-letter-align"
    [
      ("inherit", (Inherit : initial_letter_align));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_initial_letter_align t))
    ~default:(fun t ->
      let keywords =
        Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
          read_initial_letter_align_keyword t
      in
      let duplicate =
        List.exists
          (fun keyword ->
            List.length (List.filter (( = ) keyword) keywords) > 1)
          keywords
      in
      let valid_pair =
        match keywords with
        | [ _ ] -> true
        | [ first; second ] ->
            (first = Border_box && second <> Border_box)
            || (first <> Border_box && second = Border_box)
        | _ -> false
      in
      if duplicate || not valid_pair then
        Cursor.err_invalid t "initial-letter-align"
      else (Align keywords : initial_letter_align))
    t

let rec read_initial_letter_wrap t : initial_letter_wrap =
  Cursor.enum_or_var "initial-letter-wrap"
    [
      ("none", (None : initial_letter_wrap));
      ("first", First);
      ("all", All);
      ("grid", Grid);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_initial_letter_wrap t))
    ~default:(fun t ->
      (Length (Values.read_length_percentage ~with_keywords:false t)
        : initial_letter_wrap))
    t

let animation_range_names =
  [ "cover"; "contain"; "entry"; "exit"; "entry-crossing"; "exit-crossing" ]

let read_animation_range_offset t : length_percentage option =
  Cursor.ws t;
  if Cursor.is_done t || Cursor.peek_semicolon t then
    (None : length_percentage option)
  else
    match Cursor.peek_ident t with
    | Some "normal" -> (None : length_percentage option)
    | Some next when List.mem next animation_range_names ->
        (None : length_percentage option)
    | _ -> (Some (Values.read_length_percentage t) : length_percentage option)

let rec read_animation_range_item t : animation_range_item =
  let keywords : (string * animation_range_item) list =
    [
      ("normal", (Normal : animation_range_item));
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  let read_item t =
    Cursor.ws t;
    match Cursor.peek_ident t with
    | Some name when List.mem name animation_range_names ->
        let name : animation_range_name = read_animation_range_name t in
        let lp = read_animation_range_offset t in
        (Named (name, lp) : animation_range_item)
    | _ ->
        let lp = Values.read_length_percentage t in
        Offset lp
  in
  Cursor.enum_or_var "animation-range-item" keywords
    ~var:(fun t ->
      (Var (Values.read_var read_animation_range_item t) : animation_range_item))
    ~default:read_item t

let rec read_animation_range t : animation_range =
  let keywords : (string * animation_range) list =
    [
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  let read_range t =
    let read_single t =
      Cursor.ws t;
      match Cursor.peek_ident t with
      | Some "normal" ->
          let _ = Cursor.ident t in
          (Normal : animation_range_item)
      | Some name when List.mem name animation_range_names ->
          let name : animation_range_name = read_animation_range_name t in
          let lp = read_animation_range_offset t in
          (Named (name, lp) : animation_range_item)
      | _ ->
          let lp = Values.read_length_percentage t in
          Offset lp
    in
    let first = read_single t in
    Cursor.ws t;
    if Cursor.is_done t || Cursor.peek_semicolon t then Range (first, None)
    else
      let second = read_single t in
      Range (first, Some second)
  in
  (Cursor.enum_or_var "animation-range" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_animation_range t) : animation_range))
     ~default:read_range t
    : animation_range)

let pp_property_value_kind : type a. a property_value_kind Pp.t =
 fun ctx -> function
  | Length -> Pp.string ctx "length"
  | Lengths -> Pp.string ctx "lengths"
  | Length_percentage -> Pp.string ctx "length-percentage"
  | Border_width -> Pp.string ctx "border-width"
  | Border_widths -> Pp.string ctx "border-widths"
  | Opacity -> Pp.string ctx "opacity"
  | Rotate -> Pp.string ctx "rotate"
  | Duration -> Pp.string ctx "duration"
  | Number_percentage -> Pp.string ctx "number-percentage"
  | Font_size -> Pp.string ctx "font-size"
  | Display -> Pp.string ctx "display"
  | Position -> Pp.string ctx "position"
  | Visibility -> Pp.string ctx "visibility"
  | Clear -> Pp.string ctx "clear"
  | Float -> Pp.string ctx "float"
  | Scale -> Pp.string ctx "scale"
  | Translate -> Pp.string ctx "translate"
  | Transform -> Pp.string ctx "transform"
  | Animation -> Pp.string ctx "animation"
  | Transition -> Pp.string ctx "transition"
  | Filter -> Pp.string ctx "filter"
  | Shadow -> Pp.string ctx "shadow"
  | Border_radius -> Pp.string ctx "border-radius"
  | Color -> Pp.string ctx "color"
  | Colors -> Pp.string ctx "colors"
  | Animation_name -> Pp.string ctx "animation-name"
  | Background -> Pp.string ctx "background"
  | Background_image -> Pp.string ctx "background-image"
  | Background_images -> Pp.string ctx "background-images"
  | Font_src -> Pp.string ctx "font-src"
  | Font_family -> Pp.string ctx "font-family"

let read_property_value_kind (type a) (_ : Cursor.t) : a property_value_kind =
  invalid_arg
    "Properties.read_property_value_kind: property_value_kind is a phantom \
     GADT and cannot be parsed standalone"
