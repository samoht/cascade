(* css-inline-3 and css-ruby-1: vertical-align, initial-letter, text-box,
   line-fit-edge and the ruby properties. *)

open Common
open Values
open Properties_intf

(* CSS Inline 3 sec. 4.2 gives the property a [<length-percentage>], so every
   length unit is valid here and the intrinsic-sizing keywords a bare length
   reader would take are not. *)
let read_vertical_align_length t : vertical_align =
  Length (read_length_percentage ~with_keywords:false t)

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
  (* CSS Inline 3 sec. 5.1 refuses a size below 1 written on its own, so the
     wrapper comes off only around a leaf that is a size by itself. *)
  | Calc (c, sink) ->
      pp_calc
        ~unwrap_num:(match c with Num n -> n >= 1. | _ -> true)
        pp_initial_letter ctx c;
      Option.iter
        (fun sink ->
          Pp.space ctx ();
          Pp.int ctx sink)
        sink
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

(* The wrap length is the one part of this value the printer respells under
   [--minify], so leaving the authored spelling here costs the merge that two
   rules writing one width should get on the first pass. *)
let normalize_initial_letter_wrap : initial_letter_wrap -> initial_letter_wrap =
 fun value ->
  match value with
  | Length (Length len) ->
      let len' = Values.canonical_dimension len in
      if len' == len then value else Length (Length len')
  | _ -> value

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
  | Spaces -> Pp.string ctx "spaces"
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
  | Length lp -> pp_length_percentage ~always:true ctx lp
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_vertical_align ctx v

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
  match Cursor.peek_keyword t with
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
      ("spaces", Spaces);
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
        List.length
          (List.filter
             (Prop_common.equal_ruby_position_keyword keyword)
             keywords)
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

let rec read_vertical_align t : vertical_align =
  let read_var t : vertical_align = Var (read_var read_vertical_align t) in
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
    ~calls:[ ("var", read_var) ]
    ~default:read_vertical_align_length t

let normalize_vertical_align (va : vertical_align) : vertical_align =
  match va with
  | Length lp ->
      let lp' = Values.normalize_length_percentage lp in
      if lp' == lp then va else Length lp'
  | _ -> va

let rec numeric_initial_letter_calc_leaves :
    initial_letter calc -> initial_letter calc = function
  | Val (Size n) -> Num n
  | Nested inner -> Nested (numeric_initial_letter_calc_leaves inner)
  | Parens inner -> Parens (numeric_initial_letter_calc_leaves inner)
  | Expr (left, op, right) ->
      Expr
        ( numeric_initial_letter_calc_leaves left,
          op,
          numeric_initial_letter_calc_leaves right )
  | other -> other

(* CSS Inline 3 sec. 5.1 spells the size [<number [1,inf]>], and CSS Values 4
   sec. 10.12 clamps a math function past that range at computed-value time:
   unwrapping one below 1 would write CSS a browser drops. *)
let normalize_initial_letter (value : initial_letter) : initial_letter =
  match value with
  | Calc (c, sink) -> (
      match (eval_calc (numeric_initial_letter_calc_leaves c), sink) with
      | Num n, Option.None when n >= 1. -> Size n
      | Num n, Option.Some sink when n >= 1. -> Size_sink (n, sink)
      | folded, _ -> if folded == c then value else Calc (folded, sink))
  | value -> value

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
  let read_sink t =
    Cursor.ws t;
    if Cursor.is_done t then Option.none
    else
      let sink = Cursor.int t in
      if sink < 1 then Cursor.err_invalid t "initial-letter sink must be >= 1";
      Cursor.ws t;
      Cursor.expect_eof t;
      Option.some sink
  in
  let read_number t =
    let size = Cursor.number t in
    if size < 1. then Cursor.err_invalid t "initial-letter size must be >= 1";
    match read_sink t with
    | Option.None -> (Size size : initial_letter)
    | Option.Some sink -> Size_sink (size, sink)
  in
  (* CSS Values 4 sec. 10.1 puts a math function wherever the size [<number>]
     stands, [calc()] being one of them rather than the gate to the rest, and
     sec. 10.12 checks the [1,inf] range on the value it resolves to: the call
     keeps out of [read_number], which is the literal's range check. *)
  let read_math t : initial_letter =
    let size = read_calc ~result_type:`Number read_initial_letter t in
    Calc (size, read_sink t)
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
    ~calls:
      (("var", fun t -> Var (Values.read_var read_initial_letter t))
      :: ("calc", read_math)
      :: Values.math_function_calls read_math)
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
                 (Prop_common.equal_initial_letter_align_keyword keyword)
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
