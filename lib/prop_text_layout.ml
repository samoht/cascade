(* css-text-4: text alignment, indent, transform, overflow and wrapping,
   white-space, word-break, overflow-wrap, hyphens, line-break, tab-size and
   text-size-adjust. *)

open Values
open Properties_intf

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
        match Cursor.peek_keyword t with
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
        match Cursor.peek_keyword t with
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
  (* Sec. 6.2 counts characters, so each slot is a [1,inf] [<integer>] and a
     call folding below it keeps its wrapper. *)
  | Chars n ->
      let n' = Values.normalize_number ~ctx ~non_negative:true n in
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

(* Sec. 4.2 gives the number a [0,inf] range, so a call folding below it keeps
   its wrapper rather than becoming a literal the reader refuses. *)
let normalize_tab_size ~ctx : tab_size -> tab_size =
 fun value ->
  match value with
  | Number n ->
      let n' = Values.normalize_number ~ctx ~non_negative:true n in
      if n' == n then value else Number n'
  (* The unit carries the meaning here: CSS Text 4 sec. 6.2 reads a bare number
     as a count of spaces and a length as a distance, so the spelling folds but
     no zero strip may reach this. *)
  | Length len ->
      let len' = Values.canonical_dimension len in
      if len' == len then value else Length len'
  | other -> other

let rec pp_tab_size : tab_size Pp.t =
 fun ctx -> function
  | Number n -> Values.pp_number ctx n
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

let rec pp_text_size_adjust : text_size_adjust Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_size_adjust ctx v
  | None -> Pp.string ctx "none"
  | Auto -> Pp.string ctx "auto"
  | Pct n -> Pp.pct ctx n
  (* CSS Size Adjustment 1 sec. 3 refuses a negative percentage written on its
     own, so the wrapper comes off only around a leaf that is one by itself. *)
  | Calc c ->
      pp_calc
        ~unwrap:(fun (v : text_size_adjust) ->
          match v with Pct n -> n >= 0. | _ -> true)
        pp_text_size_adjust ctx c
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
  match Cursor.peek_keyword t with
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

let rec read_text_size_adjust_with ~keywords t : text_size_adjust =
  Cursor.ws t;
  match Cursor.percentage_opt t with
  | Some n ->
      if n < 0.0 then
        Cursor.err t "text-size-adjust percentages cannot be negative"
      else Pct n
  | _ ->
      (* Sec. 10.1 puts a math function wherever the [<percentage>] stands,
         [calc()] being one of them rather than the gate to the rest, and sec.
         10.12 checks the [0,inf] range on the value it resolves to. CSS Size
         Adjustment 1 sec. 3 spells the adjustment a [<percentage>] with no
         [<number>] beside it, so a call answering a [<length>] or a bare
         coefficient is none of its types. Sec. 10.8 gives an operand no
         keyword, so [calc(inherit)] and [calc(2 * auto)] fail the way the
         browser drops them rather than surviving as a [Calc] the printer
         unwraps into a live value. *)
      let read_math t : text_size_adjust =
        Calc
          (Values.read_calc ~result_type:`Percentage
             (read_text_size_adjust_with ~keywords:false)
             t)
      in
      (* Sec. 10.12 puts the [0,inf] range on the value a math function resolves
         to, so a folded comparison answers to it the way a literal does. *)
      let checked t n : text_size_adjust =
        if n < 0.0 then
          Cursor.err t "text-size-adjust percentages cannot be negative"
        else Pct n
      in
      Cursor.enum_or_calls "text-size-adjust"
        (if keywords then
           [
             ("none", (None : text_size_adjust));
             ("auto", Auto);
             ("inherit", Inherit);
             ("initial", Initial);
             ("unset", Unset);
             ("revert", Revert);
             ("revert-layer", Revert_layer);
           ]
         else [])
        ~calls:
          (( "var",
             fun t ->
               Var (Values.read_var (read_text_size_adjust_with ~keywords) t) )
          :: ("calc", read_math)
          :: Values.percentage_math_function_calls ~pct:checked read_math)
        t

let read_text_size_adjust t : text_size_adjust =
  read_text_size_adjust_with ~keywords:true t

let rec read_tab_size (t : Cursor.t) : tab_size =
  let checked t (n : number) : tab_size =
    (match n with
    | Num f when f < 0. -> Cursor.err_invalid t "negative tab-size"
    | _ -> ());
    Number n
  in
  (* CSS Text 4 sec. 4.4: [<number [0,inf]> | <length [0,inf]>], so a math
     function lands in whichever slot its result type names, and only a literal
     has a value to turn away. The measure is a count of advance widths rather
     than an integer, so a fraction is one. *)
  let read_value t =
    Cursor.one_of
      [
        (fun t -> checked t (Values.read_number t));
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

(* CSS Size Adjustment 1 sec. 3 spells the adjustment [<percentage [0,inf]>],
   and CSS Values 4 sec. 10.12 clamps a math function past that range at
   computed-value time: unwrapping a negative one would write CSS a browser
   drops. *)
let rec normalize_text_size_adjust (value : text_size_adjust) : text_size_adjust
    =
  match value with
  | Calc c -> (
      match eval_calc c with
      | Val (Pct n as leaf) when n >= 0. -> normalize_text_size_adjust leaf
      | folded -> if folded == c then value else Calc folded)
  | value -> value
