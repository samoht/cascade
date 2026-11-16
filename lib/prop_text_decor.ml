(* css-text-decor-4: the text-decoration and text-emphasis families,
   text-underline-position, text-shadow and -webkit-text-stroke. *)

open Common
open Values
open Properties_intf
open Prop_common
open Prop_background

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
        (* CSS Text Decoration 4 sec. 3 fills this slot with a
           [<'text-decoration-thickness'>], which sec. 2.3 spells [auto |
           from-font | <length-percentage>], so no sizing function belongs here.
           The longhand already reads it that way. *)
        (fun t ->
          Thickness
            (Cursor.enum "text-decoration-thickness"
               [ ("auto", (Auto : length)); ("from-font", From_font) ]
               ~default:(read_length ~with_keywords:false)
               t));
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

(* CSS Variables 1 sec. 3 syntax-checks a shorthand carrying a [var()] only
   after substitution, so which slot the substituted tokens fill is unknown
   until computed-value time and a slot holding its initial is not spare.
   [text-decoration: solid var(--x)] with [--x: dotted] substitutes to two
   [<text-decoration-style>] values and is invalid at computed-value time,
   leaving the longhands unset, where dropping the [solid] leaves
   [text-decoration: var(--x)], which substitutes to a valid [dotted] and sets
   the style. *)
let text_decoration_slot_is_var (s : text_decoration_shorthand) =
  List.exists
    (function (Var _ : text_decoration_line) -> true | _ -> false)
    s.lines
  || (match s.style with
    | Some (Var _ : text_decoration_style) -> true
    | _ -> false)
  || (match s.color with Some (Values.Var _) -> true | _ -> false)
  || match s.thickness with Some (Values.Var _) -> true | _ -> false

let normalize_text_decoration ?(lossless = false) :
    text_decoration -> text_decoration =
 fun value ->
  match value with
  | Shorthand s when text_decoration_slot_is_var s -> value
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
             (* The blur has a floor, so unwrapping a math function there could
                turn a value browsers take into one they drop. *)
             blur =
               drop_default ~is_default:is_zero_length
                 (option_map_preserve
                    (Values.normalize_length ~non_negative:true)
                    s.blur);
             color = option_map_preserve (normalize_color ~lossless) s.color;
           })
  | other -> other

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

(* The property is a [||] of one width and one colour, and CSS Values 4 sec. 2.2
   takes each option of a [||] at most once, so a second width fills no slot
   rather than replacing the first. *)
let read_webkit_text_stroke t : webkit_text_stroke =
  let width = ref Option.None and color = ref Option.None in
  let fill slot value =
    if Option.is_some !slot then
      Cursor.err_invalid t "-webkit-text-stroke names a component twice";
    slot := Some value
  in
  let read_one t =
    match Cursor.peek_keyword t with
    | Some ("thin" | "medium" | "thick") -> fill width (read_border_width t)
    | _ -> (
        let snap = Cursor.save t in
        match read_border_width t with
        | w -> fill width w
        | exception Cursor.Parse_error _ ->
            Cursor.restore t snap;
            fill color (Values.read_color t))
  in
  read_one t;
  Cursor.ws t;
  if not (Cursor.is_done t || Cursor.peek_comma t) then read_one t;
  { width = !width; color = !color }

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

(* The run is offsets then blur, and CSS Backgrounds 3 sec. 6.2 gives the
   [<shadow>] this one shares its slots a plain length in each, with a floor on
   the blur alone. A math function holds no value to compare, so only a literal
   is turned away there. The run caps at three: there is no spread slot to hold
   a fourth. *)
let read_shadow_lengths t =
  let lengths_rev = ref [] in
  let rec loop n =
    if n >= 3 then ()
    else
      let allow_negative = n <> 2 in
      match
        Cursor.option
          (fun t ->
            read_length ~allow_negative ~with_keywords:false ~length_only:true t)
          t
      with
      | Option.Some l ->
          lengths_rev := l :: !lengths_rev;
          Cursor.ws t;
          loop (n + 1)
      | Option.None -> ()
  in
  loop 0;
  List.rev !lengths_rev

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
         the colour on either side of the length run but never inside it. *)
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
      let lengths = read_shadow_lengths t in
      try_color ();
      let lengths, color = take_trailing_color_var lengths !color in
      match lengths with
      | h :: v :: rest ->
          let blur =
            match rest with b :: _ -> Option.Some b | _ -> Option.None
          in
          (Text_shadow { h_offset = h; v_offset = v; blur; color }
            : text_shadow)
      | _ -> err_invalid_value t "text-shadow" "expected at least two lengths")
    t

(* CSS Text Decoration 4 sec. 6.2 spells the property [none | <shadow>#], so
   [none] names the whole value and is no item of the list. *)
let read_text_shadows t : text_shadow list =
  let shadows = Cursor.list ~sep:Cursor.comma ~at_least:1 read_text_shadow t in
  let whole_value : text_shadow -> bool = function
    | None | Initial | Inherit | Unset | Revert | Revert_layer -> true
    | Text_shadow _ | Var _ -> false
  in
  (match shadows with
  | _ :: _ :: _ when List.exists whole_value shadows ->
      Cursor.err_invalid t "text-shadow keyword beside another shadow"
  | _ -> ());
  shadows

let text_decoration_shorthand ?lines ?style ?color ?thickness () :
    text_decoration =
  Shorthand { lines = Option.value ~default:[] lines; style; color; thickness }
