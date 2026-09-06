(* css-text-4, css-text-decor-4 and css-inline-3: text alignment, indent,
   transform, overflow and wrapping, the text-decoration and text-emphasis
   families, text-shadow, white-space, word-break, overflow-wrap, hyphens, ruby,
   initial-letter, text-box, tab-size and vertical-align.

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name here stays visible under [Css.Properties] exactly as when
   it lived in properties.ml. *)

open Common
open Values
open Properties_intf
open Prop_common
open Prop_background

let read_vertical_align_length t : vertical_align =
  let n, unit = Cursor.number_with_unit t in
  match unit with
  | Some "px" -> Px n
  | Some "rem" -> Rem n
  | Some "em" -> Em n
  | Some "%" -> Pct n
  (* A unitless [0] is the valid zero <length> (CSS Values 4 sec. 6); any other
     unitless number is not a length and is rejected. *)
  | None when n = 0. -> Zero
  | None -> Cursor.err_invalid t "vertical-align requires a unit"
  | Some u -> Cursor.err_invalid t ("invalid vertical-align unit: " ^ u)

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

  let is_empty { lines; style; color; thickness } =
    lines = [] && Option.is_none style && Option.is_none color
    && Option.is_none thickness

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
  if Text_decoration.is_empty acc then
    Cursor.err_expected t "text-decoration value";
  Text_decoration.to_shorthand acc

let rec read_text_decoration t : text_decoration =
  let read_var t : text_decoration = Var (read_var read_text_decoration t) in
  let snap = Cursor.save t in
  let value =
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
        (Shorthand shorthand : text_decoration))
      t
  in
  (* CSS Text Decoration 4 sec. 2.5: the shorthand is a [||] of its longhands
     and [none] is a [<text-decoration-line>], so it is the whole value only
     when nothing follows it. *)
  match value with
  | None ->
      Cursor.ws t;
      if Cursor.is_done t then value
      else (
        Cursor.restore t snap;
        (Shorthand (read_text_decoration_shorthand t) : text_decoration))
  | _ -> value

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
      while not (Cursor.is_done t) do
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
            List.length
              (List.filter (equal_text_emphasis_skip_keyword keyword) keywords)
            > 1)
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

let normalize_text_indent : text_indent_value -> text_indent_value =
 fun value ->
  match value with
  | Indent r ->
      let length = Values.normalize_length_percentage r.length in
      if length == r.length then value else Indent { r with length }
  | other -> other

(* CSS Text Decoration 4 (ED) sec. 2.6: the [text-decoration] shorthand sets the
   line, thickness, style and colour longhands, and "Omitted values are set to
   their initial values" - [solid] (sec. 2.2) and [currentcolor] (sec. 2.3).
   Writing an initial out names what leaving it out names, and leaving it out is
   the shorter spelling. Written on its own the initial is the whole value, and
   dropping it drains the shorthand: what is left declares the four initials and
   nothing else, which is what [none] declares. *)
let normalize_hyphenate_limit_chars_item ~ctx :
    hyphenate_limit_chars_item -> hyphenate_limit_chars_item =
 fun item ->
  match item with
  | Auto -> item
  | Chars n ->
      let n' = Values.normalize_number ~ctx n in
      if n' == n then item else Chars n'

let normalize_hyphenate_limit_chars ~ctx :
    hyphenate_limit_chars -> hyphenate_limit_chars =
 fun value ->
  let item = normalize_hyphenate_limit_chars_item ~ctx in
  match value with
  | One a ->
      let a' = item a in
      if a' == a then value else One a'
  | Two (a, b) ->
      let a' = item a and b' = item b in
      if a' == a && b' == b then value else Two (a', b')
  | Three (a, b, c) ->
      let a' = item a and b' = item b and c' = item c in
      if a' == a && b' == b && c' == c then value else Three (a', b', c')
  | other -> other

let normalize_text_decoration ?(lossless = false) :
    text_decoration -> text_decoration =
 fun value ->
  match value with
  | Shorthand s -> (
      let style =
        drop_default
          ~is_default:(fun (d : text_decoration_style) -> d = Solid)
          s.style
      in
      let color =
        drop_default
          ~is_default:(fun (c : Values.color) -> c = Values.Current)
          (option_map_preserve (normalize_color ~lossless) s.color)
      in
      match (s.lines, style, color, s.thickness) with
      | [], Option.None, Option.None, Option.None -> (None : text_decoration)
      | _ ->
          if
            option_is_phys_same style s.style
            && option_is_phys_same color s.color
          then value
          else Shorthand { s with style; color })
  | other -> other

let normalize_text_emphasis ?(lossless = false) : text_emphasis -> text_emphasis
    =
 fun value ->
  match value with
  | Emphasis (style, color) ->
      (* CSS Text Decoration 4 (ED) sec. 3.4: a component left out takes its
         longhand's initial - [none] for the style (sec. 3.1) and [currentColor]
         for the colour (sec. 3.3) - so writing one out names what leaving it
         out names. Both cannot go: the value would say nothing. *)
      let color = option_map_preserve (normalize_color ~lossless) color in
      let dropped_style =
        match style with
        | Some (None : text_emphasis_style) -> Option.None
        | style -> style
      in
      let dropped_color =
        match color with
        | Some (Current : color) -> Option.None
        | color -> color
      in
      if Option.is_none dropped_style && Option.is_none dropped_color then
        preserve_if_equal value
          (Emphasis (Some (None : text_emphasis_style), Option.None))
      else preserve_if_equal value (Emphasis (dropped_style, dropped_color))
  | other -> other

(* CSS Text Decoration 4 (ED) sec. 4 reads a text shadow as a [<shadow>] "as for
   box-shadow", and CSS Backgrounds 3 (ED) sec. 6.1 says "Omitted lengths are
   0". The blur is the last length a text shadow carries, so a zero blur is the
   spelled-out form of leaving it off. *)
let normalize_text_shadow ?(lossless = false) : text_shadow -> text_shadow =
 fun value ->
  match value with
  | Text_shadow s ->
      preserve_if_equal value
        (Text_shadow
           {
             h_offset = Values.normalize_length s.h_offset;
             v_offset = Values.normalize_length s.v_offset;
             blur =
               drop_default ~is_default:is_zero_length
                 (option_map_preserve Values.normalize_length s.blur);
             color = option_map_preserve (normalize_color ~lossless) s.color;
           })
  | other -> other

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
  (match thickness with
  | None -> ()
  | Some l ->
      space_if_needed ();
      pp_length ctx l);
  (* The record is public, so a caller can hand over a value with no slot
     filled. It declares nothing but the initial longhands, which is what
     [text-decoration: none] declares (CSS Text Decoration 4 (ED) sec. 2.6); the
     empty string is not a value any parser reads back. *)
  if !first then Pp.string ctx "none"

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
  | Auto -> Pp.string ctx "auto"
  | Balance -> Pp.string ctx "balance"
  | Stable -> Pp.string ctx "stable"
  | Pretty -> Pp.string ctx "pretty"
  | Mode_style (mode, style) ->
      Pp.string ctx (match mode with `Wrap -> "wrap" | `No_wrap -> "nowrap");
      Pp.space ctx ();
      Pp.string ctx
        (match style with
        | `Auto -> "auto"
        | `Balance -> "balance"
        | `Stable -> "stable"
        | `Pretty -> "pretty")
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
  | Normal -> Pp.string ctx "normal"
  | Box (None, None) -> pp_text_box_trim ctx Trim_both
  | Box (trim, edge) ->
      Option.iter (pp_text_box_trim ctx) trim;
      Option.iter
        (fun edge ->
          if Option.is_some trim then Pp.space ctx ();
          pp_text_box_edge ctx edge)
        edge
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
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

let pp_hyphenate_limit_chars_item : hyphenate_limit_chars_item Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Chars n -> Values.pp_number ctx n

let rec pp_hyphenate_limit_chars : hyphenate_limit_chars Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_hyphenate_limit_chars ctx v
  | One a -> pp_hyphenate_limit_chars_item ctx a
  | Two (a, b) ->
      pp_hyphenate_limit_chars_item ctx a;
      Pp.space ctx ();
      pp_hyphenate_limit_chars_item ctx b
  | Three (a, b, c) ->
      pp_hyphenate_limit_chars_item ctx a;
      Pp.space ctx ();
      pp_hyphenate_limit_chars_item ctx b;
      Pp.space ctx ();
      pp_hyphenate_limit_chars_item ctx c
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
  | Collapse -> Pp.string ctx "collapse"
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
      | Some b ->
          Pp.space ctx ();
          pp_length ctx b
      | None -> ());
      match color with Some c -> pp_color_after_length ctx c | None -> ())

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

(* CSS Text 4 sec. 5.5: [<'text-wrap-mode'> || <'text-wrap-style'>], so each
   side appears at most once and either may be omitted. *)
let rec read_text_wrap t : text_wrap =
  let single =
    [
      ("wrap", (Wrap : text_wrap));
      ("nowrap", No_wrap);
      ("auto", Auto);
      ("balance", Balance);
      ("stable", Stable);
      ("pretty", Pretty);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  let first =
    Cursor.enum_or_var "text-wrap" single
      ~var:(fun t -> Var (read_var read_text_wrap t))
      t
  in
  let mode_of : text_wrap -> _ = function
    | Wrap -> Some `Wrap
    | No_wrap -> Some `No_wrap
    | _ -> None
  in
  let style_of : text_wrap -> _ = function
    | Auto -> Some `Auto
    | Balance -> Some `Balance
    | Stable -> Some `Stable
    | Pretty -> Some `Pretty
    | _ -> None
  in
  let second () =
    Cursor.ws t;
    match Cursor.peek_ident t with
    | Some _ -> Cursor.option (Cursor.enum "text-wrap" single) t
    | None -> None
  in
  match (mode_of first, style_of first) with
  | Some mode, _ -> (
      match second () with
      | Some other -> (
          match style_of other with
          | Some style -> Mode_style (mode, style)
          | None -> Cursor.err_invalid t "text-wrap repeats its wrap mode")
      | None -> first)
  | _, Some style -> (
      match second () with
      | Some other -> (
          match mode_of other with
          | Some mode -> Mode_style (mode, style)
          | None -> Cursor.err_invalid t "text-wrap repeats its wrap style")
      | None -> first)
  | None, None -> first

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

let read_text_box_trim_value t : text_box_trim =
  Cursor.enum "text-box-trim"
    [
      ("none", (None : text_box_trim));
      ("trim-start", Trim_start);
      ("trim-end", Trim_end);
      ("trim-both", Trim_both);
    ]
    t

let rec read_text_box_trim t : text_box_trim =
  Cursor.enum_or_var "text-box-trim"
    [
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_box_trim t))
    ~default:read_text_box_trim_value t

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

(* CSS Inline 3 sec. 4.4 gives [text-box-edge] [auto | <text-edge>], so [auto]
   is a value of the property rather than a CSS-wide keyword and reaches the
   [text-box] shorthand along with the rest. *)
let read_text_box_edge_value t : text_box_edge =
  match Cursor.peek_ident t with
  | Some "auto" ->
      let _ = Cursor.ident t in
      (Auto : text_box_edge)
  | _ -> (
      let keywords =
        Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
          read_text_box_edge_keyword t
      in
      match keywords with
      | [ Text ] | [ Ideographic_ink ] -> Edge (List.hd keywords, None)
      | [ ((Cap | Ex) as first); ((Alphabetic | Text) as second) ]
      | [ (Text as first); ((Alphabetic | Ideographic) as second) ] ->
          Edge (first, Some second)
      | _ -> Cursor.err_invalid t "text-box-edge")

let rec read_text_box_edge ?(global = true) t : text_box_edge =
  if not global then read_text_box_edge_value t
  else
    Cursor.enum_or_var "text-box-edge"
      [
        ("inherit", (Inherit : text_box_edge));
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
      ("normal", (Normal : text_box));
      ("initial", (Initial : text_box));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_text_box t) : text_box))
    ~default:(fun t ->
      (* CSS Inline 3 sec. 6.1 joins the trim and edge slots with [||], so
         either may be omitted and they may appear in either order. *)
      let trim = ref Option.None in
      let edge = ref Option.None in
      let try_trim () =
        if !trim <> Option.None then false
        else
          match Cursor.option read_text_box_trim_value t with
          | Some value ->
              trim := Some value;
              true
          | None -> false
      in
      let try_edge () =
        if !edge <> Option.None then false
        else
          match Cursor.option (read_text_box_edge ~global:false) t with
          | Some value ->
              edge := Some value;
              true
          | None -> false
      in
      let rec loop () =
        Cursor.ws t;
        if try_trim () || try_edge () then loop ()
      in
      loop ();
      if !trim = Option.None && !edge = Option.None then
        Cursor.err_invalid t "text-box";
      (Box (!trim, !edge) : text_box))
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
        List.length (List.filter (equal_ruby_position_keyword keyword) keywords)
        > 1
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

(* Sec. 6.3.4 writes each slot [auto | <integer [0,inf]>]; Chrome 152 refuses
   the zero the range grants, so the literal floor stays at one. A math function
   holds no value to compare, and the whole-value number reader refuses anything
   after the number it read, so the component is handed to it on its own. *)
let read_hyphenate_limit_chars_item t : hyphenate_limit_chars_item =
  Cursor.enum "hyphenate-limit-chars"
    [ ("auto", (Auto : hyphenate_limit_chars_item)) ]
    ~default:(fun t ->
      match Cursor.peek t with
      | Some (Component.Func _ as component) ->
          let _ = Cursor.next t in
          Chars (Values.read_number (Cursor.of_components [ component ]))
      | _ ->
          let n = Cursor.int t in
          if n < 1 then
            Cursor.err_invalid t "hyphenate-limit-chars must be >= 1";
          Chars (Num (float_of_int n)))
    t

let rec read_hyphenate_limit_chars t : hyphenate_limit_chars =
  let read_slots t =
    let slots =
      Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:3
        read_hyphenate_limit_chars_item t
    in
    Cursor.ws t;
    Cursor.expect_eof t;
    match slots with
    | [ a ] -> (One a : hyphenate_limit_chars)
    | [ a; b ] -> Two (a, b)
    | [ a; b; c ] -> Three (a, b, c)
    | _ -> Cursor.err_invalid t "expected one to three slots"
  in
  Cursor.enum_or_calls "hyphenate-limit-chars"
    [
      ("inherit", (Inherit : hyphenate_limit_chars));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (read_var read_hyphenate_limit_chars t)) ]
    ~default:read_slots t

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
          ("collapse", Collapse);
          ("inherit", Inherit);
          ("initial", Initial);
          ("unset", Unset);
          ("revert", Revert);
          ("revert-layer", Revert_layer);
        ]
        ~var:(fun t -> Var (read_var read_white_space t))
        t

(* CSS Text 4 sec. 3.1. *)
let rec pp_white_space_collapse : white_space_collapse Pp.t =
 fun ctx -> function
  | Collapse -> Pp.string ctx "collapse"
  | Discard -> Pp.string ctx "discard"
  | Preserve -> Pp.string ctx "preserve"
  | Preserve_breaks -> Pp.string ctx "preserve-breaks"
  | Preserve_spaces -> Pp.string ctx "preserve-spaces"
  | Break_spaces -> Pp.string ctx "break-spaces"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_white_space_collapse ctx v

let rec read_white_space_collapse t : white_space_collapse =
  Cursor.enum_or_var "white-space-collapse"
    [
      ("collapse", (Collapse : white_space_collapse));
      ("discard", Discard);
      ("preserve", Preserve);
      ("preserve-breaks", Preserve_breaks);
      ("preserve-spaces", Preserve_spaces);
      ("break-spaces", Break_spaces);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_white_space_collapse t))
    t

(* [-webkit-text-stroke] is [<line-width> || <color>]: either component may be
   left out and either order is accepted, so the two are read by type. A
   component left out takes its longhand's initial - [0] for the width and
   [currentColor] for the colour - which is what the printer then omits. *)
let pp_webkit_text_stroke : webkit_text_stroke Pp.t =
 fun ctx s ->
  let wrote = ref false in
  let sep () = if !wrote then Pp.space ctx () else wrote := true in
  Option.iter
    (fun w ->
      sep ();
      pp_border_width ctx w)
    s.width;
  Option.iter
    (fun c ->
      sep ();
      pp_color ctx c)
    s.color;
  if not !wrote then Pp.string ctx "currentColor"

let read_webkit_text_stroke t : webkit_text_stroke =
  let width = ref Option.None and color = ref Option.None in
  let read_one t =
    match Cursor.peek_ident t with
    | Some ("thin" | "medium" | "thick") -> width := Some (read_border_width t)
    | _ -> (
        let snap = Cursor.save t in
        match read_border_width t with
        | w -> width := Some w
        | exception Cursor.Parse_error _ ->
            Cursor.restore t snap;
            color := Some (Values.read_color t))
  in
  read_one t;
  Cursor.ws t;
  if not (Cursor.is_done t || Cursor.peek_comma t) then read_one t;
  { width = !width; color = !color }

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

let rec read_tab_size (t : Cursor.t) : tab_size =
  let number_value t i =
    if i < 0 then Cursor.err_invalid t "negative tab-size integer";
    (Int i : tab_size)
  in
  let read_value t =
    match Cursor.integer_opt t with
    | Some i -> number_value t i
    | None ->
        (* CSS Text 4 (ED) sec. 4.4: [<number> | <length>], so a math function
           lands in whichever slot its result type names. *)
        Cursor.one_of
          [
            (fun t -> number_value t (Values.read_integer "tab-size" t));
            (fun t ->
              Length
                (Values.read_length ~allow_negative:false ~with_keywords:false
                   ~length_only:true t));
          ]
          t
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
  let read_calc t : vertical_align =
    Calc (read_calc ~result_type:`Value read_vertical_align t)
  in
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
      (* CSS Text Decoration 3 sec. 5: [<color>? && <length>{2,3}]. The && puts
         the colour on either side of the length run but never inside it, and
         caps the run at three: there is no spread slot to hold a fourth. *)
      let color : color option ref = ref (Option.None : color option) in
      let try_color () =
        match !color with
        | Option.Some _ -> ()
        | Option.None -> (
            match Cursor.option (fun t -> read_color t) t with
            | Option.Some c ->
                color := Option.Some c;
                Cursor.ws t
            | Option.None -> ())
      in
      try_color ();
      let lengths_rev = ref [] in
      let rec read_lengths_loop n =
        if n >= 3 then ()
        else
          match Cursor.option (fun t -> read_length t) t with
          | Option.Some l ->
              lengths_rev := l :: !lengths_rev;
              Cursor.ws t;
              read_lengths_loop (n + 1)
          | Option.None -> ()
      in
      read_lengths_loop 0;
      try_color ();
      match List.rev !lengths_rev with
      | h :: v :: rest ->
          let blur =
            match rest with b :: _ -> Option.Some b | _ -> Option.None
          in
          (Text_shadow { h_offset = h; v_offset = v; blur; color = !color }
            : text_shadow)
      | _ -> err_invalid_value t "text-shadow" "expected at least two lengths")
    t

let read_text_shadows t : text_shadow list =
  Cursor.list ~sep:Cursor.comma ~at_least:1 read_text_shadow t

let text_decoration_shorthand ?lines ?style ?color ?thickness () :
    text_decoration =
  Shorthand { lines = Option.value ~default:[] lines; style; color; thickness }

let normalize_vertical_align (va : vertical_align) : vertical_align =
  match va with
  | Calc c -> (
      match Values.eval_calc c with Values.Val v -> v | folded -> Calc folded)
  | _ -> va

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

let rec read_initial_letter t : initial_letter =
  let read_number t =
    let size = Cursor.number t in
    if size < 1. then Cursor.err_invalid t "initial-letter size must be >= 1";
    Cursor.ws t;
    if Cursor.is_done t then (Size size : initial_letter)
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
            List.length
              (List.filter
                 (equal_initial_letter_align_keyword keyword)
                 keywords)
            > 1)
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
