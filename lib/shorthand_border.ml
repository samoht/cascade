open Declaration
open Common
open Shorthand_common
open Shorthand_box

(* Compose [outline-width / -style / -color] into the [outline] shorthand when
   all three longhands appear contiguously with matching importance. *)
type line_part = Width | Style | Color

let outline_part_of : declaration -> line_part option = function
  | Declaration { property = Outline_width; _ } -> Some Width
  | Declaration { property = Outline_style; _ } -> Some Style
  | Declaration { property = Outline_color; _ } -> Some Color
  | _ -> None

let outline_width_value : declaration -> Properties.border_width option =
  function
  | Declaration { property = Outline_width; value; _ } -> Some value
  | _ -> None

let outline_style_value : declaration -> Properties.outline_style option =
  function
  | Declaration { property = Outline_style; value; _ } -> Some value
  | _ -> None

let outline_color_value : declaration -> Values.color option = function
  | Declaration { property = Outline_color; value; _ } -> Some value
  | _ -> None

(* Index-based composer: locate Outline_width / Outline_style / Outline_color in
   the rule, check that they form a contiguous run with matching importance,
   then absorb them into a single Outline shorthand. *)
let try_compose_outline_at idx i =
  let n = Rule_index.length idx in
  if i + 2 >= n then None
  else if
    Rule_index.is_absorbed idx i
    || Rule_index.is_absorbed idx (i + 1)
    || Rule_index.is_absorbed idx (i + 2)
  then None
  else
    let d1 = Rule_index.decl_at idx i in
    let d2 = Rule_index.decl_at idx (i + 1) in
    let d3 = Rule_index.decl_at idx (i + 2) in
    match (outline_part_of d1, outline_part_of d2, outline_part_of d3) with
    | Some p1, Some p2, Some p3
      when is_important d1 = is_important d2
           && is_important d2 = is_important d3
           && List.length (List.sort_uniq compare [ p1; p2; p3 ]) = 3 ->
        let triple = [ d1; d2; d3 ] in
        let width = List.find_map outline_width_value triple in
        let style = List.find_map outline_style_value triple in
        let color = List.find_map outline_color_value triple in
        let no_runtime =
          match width with
          | Some w -> not (Properties.border_width_has_runtime_subst w)
          | None -> true
        in
        if no_runtime then
          Some
            (Declaration.v ~important:(is_important d1) Outline
               (Shorthand { width; style; color }))
        else None
    | _ -> None

let compose_outline_via_index idx =
  compose_fixed3_via_index idx ~try_compose:try_compose_outline_at

(* CSS Backgrounds 3 sec. 3.4: [border] is the shorthand for [border-{top,
   right,bottom,left}-{width,style,color}]. Cascade composes when all 12
   longhands appear in a contiguous run with matching importance, every width /
   style / color is uniform across the four sides, and no runtime-substitution
   value would change the resolved shape. *)
let all_box_sides_present xs =
  List.length xs = 4
  &&
  let sides = List.map (fun (s, _, _) -> s) xs in
  List.sort_uniq compare sides = [ Top; Right; Bottom; Left ]

let uniform_side_value = function
  | [] -> false
  | (_, value, _) :: rest -> List.for_all (fun (_, v, _) -> v = value) rest

let border_parts_of raw_decls =
  let widths = List.filter_map border_width_of raw_decls in
  let styles = List.filter_map border_style_of raw_decls in
  let colors = List.filter_map border_color_of raw_decls in
  if
    all_box_sides_present widths
    && all_box_sides_present styles
    && all_box_sides_present colors
    && uniform_side_value widths && uniform_side_value styles
    && uniform_side_value colors
  then Some (widths, styles, colors)
  else None

let declaration_of_border_parts ~important widths styles colors =
  let _, width, _ = List.hd widths in
  let _, style, _ = List.hd styles in
  let _, color, _ = List.hd colors in
  Declaration.v ~important Border
    (Shorthand { width = Some width; style = Some style; color = Some color })

let try_compose_border_at ~bi_hazard idx i =
  let n = Rule_index.length idx in
  if i + 11 >= n then None
  else
    let absorbed =
      [
        i;
        i + 1;
        i + 2;
        i + 3;
        i + 4;
        i + 5;
        i + 6;
        i + 7;
        i + 8;
        i + 9;
        i + 10;
        i + 11;
      ]
    in
    if List.exists (Rule_index.is_absorbed idx) absorbed then None
    else
      let raw_decls = List.map (Rule_index.decl_at idx) absorbed in
      if not (same_importance raw_decls) then None
      else if List.exists has_runtime_substitution raw_decls then None
      else
        let important = is_important (List.hd raw_decls) in
        (* The shorthand resets the whole [border-image] family, so a value one
           of those longhands holds elsewhere would be dropped. *)
        if bi_hazard i ~important then None
        else
          Option.map
            (fun (widths, styles, colors) ->
              declaration_of_border_parts ~important widths styles colors)
            (border_parts_of raw_decls)

let compose_border_via_index ~bi_hazard idx =
  let n = Rule_index.length idx in
  let i = ref 0 in
  while !i + 11 < n do
    if Rule_index.is_absorbed idx !i then incr i
    else
      match try_compose_border_at ~bi_hazard idx !i with
      | None -> incr i
      | Some shorthand ->
          let absorbed =
            [
              !i;
              !i + 1;
              !i + 2;
              !i + 3;
              !i + 4;
              !i + 5;
              !i + 6;
              !i + 7;
              !i + 8;
              !i + 9;
              !i + 10;
              !i + 11;
            ]
          in
          if Rule_index.absorb idx ~at:!i ~absorbed ~shorthand then i := !i + 12
          else incr i
  done

(* CSS Backgrounds 3 sec. 3.5, CSS Logical 1 sec. 4.5 and CSS Multicol 1 sec.
   4.3: each of the eight border sides is [<line-width> || <line-style> ||
   <line-color>] over its own three longhands and resets nothing else, so a
   contiguous run of the three composes the way [outline] does. Each part
   carries the slot it fills, so the run may be written in any order. *)
let no_line : Properties.border_shorthand =
  { width = None; style = None; color = None }

(* CSS Cascade 5 sec. 7.3: [initial] is the property's initial value, which is
   what the shorthand assigns to a component left out, so the longhand fills its
   slot in the run and contributes no value to it. *)
let line_of_width v : line_part * Properties.border_shorthand =
  match (v : Properties.border_width) with
  | Initial -> (Width, no_line)
  | v -> (Width, { no_line with width = Some v })

let line_of_style v : line_part * Properties.border_shorthand =
  match (v : Properties.border_style) with
  | Initial -> (Style, no_line)
  | v -> (Style, { no_line with style = Some v })

let line_of_color v : line_part * Properties.border_shorthand =
  match (v : Values.color) with
  | Initial -> (Color, no_line)
  | v -> (Color, { no_line with color = Some v })

let merge_line (a : Properties.border_shorthand)
    (b : Properties.border_shorthand) : Properties.border_shorthand =
  let pick x y = if Option.is_none x then y else x in
  {
    width = pick a.width b.width;
    style = pick a.style b.style;
    color = pick a.color b.color;
  }

let border_top_part = function
  | Declaration { property = Border_top_width; value; _ } ->
      Some (line_of_width value)
  | Declaration { property = Border_top_style; value; _ } ->
      Some (line_of_style value)
  | Declaration { property = Border_top_color; value; _ } ->
      Some (line_of_color value)
  | _ -> None

let border_right_part = function
  | Declaration { property = Border_right_width; value; _ } ->
      Some (line_of_width value)
  | Declaration { property = Border_right_style; value; _ } ->
      Some (line_of_style value)
  | Declaration { property = Border_right_color; value; _ } ->
      Some (line_of_color value)
  | _ -> None

let border_bottom_part = function
  | Declaration { property = Border_bottom_width; value; _ } ->
      Some (line_of_width value)
  | Declaration { property = Border_bottom_style; value; _ } ->
      Some (line_of_style value)
  | Declaration { property = Border_bottom_color; value; _ } ->
      Some (line_of_color value)
  | _ -> None

let border_left_part = function
  | Declaration { property = Border_left_width; value; _ } ->
      Some (line_of_width value)
  | Declaration { property = Border_left_style; value; _ } ->
      Some (line_of_style value)
  | Declaration { property = Border_left_color; value; _ } ->
      Some (line_of_color value)
  | _ -> None

let border_block_start_part = function
  | Declaration { property = Border_block_start_width; value; _ } ->
      Some (line_of_width value)
  | Declaration { property = Border_block_start_style; value; _ } ->
      Some (line_of_style value)
  | Declaration { property = Border_block_start_color; value; _ } ->
      Some (line_of_color value)
  | _ -> None

let border_block_end_part = function
  | Declaration { property = Border_block_end_width; value; _ } ->
      Some (line_of_width value)
  | Declaration { property = Border_block_end_style; value; _ } ->
      Some (line_of_style value)
  | Declaration { property = Border_block_end_color; value; _ } ->
      Some (line_of_color value)
  | _ -> None

let border_inline_start_part = function
  | Declaration { property = Border_inline_start_width; value; _ } ->
      Some (line_of_width value)
  | Declaration { property = Border_inline_start_style; value; _ } ->
      Some (line_of_style value)
  | Declaration { property = Border_inline_start_color; value; _ } ->
      Some (line_of_color value)
  | _ -> None

let border_inline_end_part = function
  | Declaration { property = Border_inline_end_width; value; _ } ->
      Some (line_of_width value)
  | Declaration { property = Border_inline_end_style; value; _ } ->
      Some (line_of_style value)
  | Declaration { property = Border_inline_end_color; value; _ } ->
      Some (line_of_color value)
  | _ -> None

(* CSS Gaps 1 sec. 4 gives each longhand a list, and sec. 4.4 writes the
   shorthand over one line, so only a single-entry list composes. *)
let column_rule_part = function
  | Declaration { property = Column_rule_width; value = [ width ]; _ } ->
      Some (line_of_width width)
  | Declaration { property = Column_rule_style; value = [ style ]; _ } ->
      Some (line_of_style style)
  | Declaration { property = Column_rule_color; value = [ color ]; _ } ->
      Some (line_of_color color)
  | _ -> None

let try_compose_line_at ~part_of ~build idx i =
  let n = Rule_index.length idx in
  if i + 2 >= n then None
  else
    let positions = [ i; i + 1; i + 2 ] in
    if List.exists (Rule_index.is_absorbed idx) positions then None
    else
      let raw = List.map (Rule_index.decl_at idx) positions in
      if (not (same_importance raw)) || List.exists has_runtime_substitution raw
      then None
      else
        match List.map part_of raw with
        | [ Some (p1, s1); Some (p2, s2); Some (p3, s3) ]
          when List.length (List.sort_uniq compare [ p1; p2; p3 ]) = 3 ->
            Some
              (build
                 ~important:(is_important (List.hd raw))
                 (Shorthand (merge_line s1 (merge_line s2 s3))
                   : Properties.border))
        | _ -> None

(* CSS Logical 1 sec. 4.6: [border-block] and [border-inline] set the width,
   style and colour of both sides of their axis and reset nothing else, which is
   what the three axis shorthands set between them. An axis naming two different
   sides has no slot in the shorthand, so only a single-valued one takes
   part. *)
let border_block_axis_part = function
  | Declaration { property = Border_block_width; value = Single w; _ } ->
      Some (line_of_width w)
  | Declaration { property = Border_block_style; value = Single s; _ } ->
      Some (line_of_style s)
  | Declaration { property = Border_block_color; value = Single c; _ } ->
      Some (line_of_color c)
  | _ -> None

let border_inline_axis_part = function
  | Declaration { property = Border_inline_width; value = Single w; _ } ->
      Some (line_of_width w)
  | Declaration { property = Border_inline_style; value = Single s; _ } ->
      Some (line_of_style s)
  | Declaration { property = Border_inline_color; value = Single c; _ } ->
      Some (line_of_color c)
  | _ -> None

let line_families =
  let one property ~important value = Declaration.v ~important property value in
  (* Sec. 4.4 writes [column-rule] as a [<gap-rule>#], so one composed line is a
     one-entry list rather than a bare value. *)
  let listed property ~important value =
    Declaration.v ~important property [ value ]
  in
  Properties.
    [
      (border_top_part, one Border_top);
      (border_right_part, one Border_right);
      (border_bottom_part, one Border_bottom);
      (border_left_part, one Border_left);
      (border_block_start_part, one Border_block_start);
      (border_block_end_part, one Border_block_end);
      (border_inline_start_part, one Border_inline_start);
      (border_inline_end_part, one Border_inline_end);
      (column_rule_part, listed Column_rule);
      (border_block_axis_part, one Border_block);
      (border_inline_axis_part, one Border_inline);
    ]

let compose_line_via_index idx =
  List.iter
    (fun (part_of, build) ->
      compose_fixed3_via_index idx
        ~try_compose:(try_compose_line_at ~part_of ~build))
    line_families

(* Compose the [border] shorthand from the three whole-border longhands
   [border-width] / [border-style] / [border-color] when they appear as a
   contiguous run (any order, single-valued, same importance). The [border]
   shorthand also resets [border-image] to its initial, so only compose when no
   [border-image] declaration is present in the rule - otherwise the synthesised
   [border] would clobber it (the reset/reorder case is handled separately). *)
let try_compose_border_whole_at ~bi_hazard ~ctx idx i =
  let n = Rule_index.length idx in
  if i + 2 >= n then None
  else
    let positions = [ i; i + 1; i + 2 ] in
    if List.exists (Rule_index.is_absorbed idx) positions then None
    else
      let raw = List.map (Rule_index.decl_at idx) positions in
      if not (same_importance raw) then None
      else
        let width : Properties.border_width option ref = ref None in
        let style : Properties.border_style option ref = ref None in
        let color : Values.color option ref = ref None in
        List.iter
          (function
            | Declaration { property = Border_width; value = [ w ]; _ } ->
                width := Some w
            | Declaration { property = Border_style; value = [ s ]; _ } ->
                style := Some s
            | Declaration { property = Border_color; value = [ c ]; _ } ->
                color := Some c
            | _ -> ())
          raw;
        match (!width, !style, !color) with
        | Some width, Some style, Some color
          when foldable_border_width ~ctx width
               && foldable_border_style ~ctx style
               && foldable_border_color ~ctx color
               && not (bi_hazard i ~important:(is_important (List.hd raw))) ->
            Some
              (Declaration.v
                 ~important:(is_important (List.hd raw))
                 Border
                 (Shorthand
                    {
                      width = Some width;
                      style = Some style;
                      color = Some color;
                    }))
        | _ -> None

(* [border-image*] and [border-width/style/color] are independent, so a
   border-image declaration immediately preceding the whole-border longhands can
   move after them without changing any cascade. That lets
   [compose_border_whole_shorthand] synthesise [border] in place: its
   border-image reset is overridden back by the now-trailing declaration. Only
   swap when the following run carries the full width/style/color trio. *)
let is_border_image_decl = function
  | Declaration { property = Border_image; _ }
  | Declaration { property = Border_image_source; _ }
  | Declaration { property = Border_image_slice; _ }
  | Declaration { property = Border_image_width; _ }
  | Declaration { property = Border_image_outset; _ }
  | Declaration { property = Border_image_repeat; _ } ->
      true
  | _ -> false

let is_border_image_longhand d = is_border_image_decl (snd d)

let is_border_width_decl d =
  match snd d with
  | Declaration { property = Border_width; _ } -> true
  | _ -> false

let is_border_style_decl d =
  match snd d with
  | Declaration { property = Border_style; _ } -> true
  | _ -> false

let is_border_color_decl d =
  match snd d with
  | Declaration { property = Border_color; _ } -> true
  | _ -> false

let reorder_border_image_before_border decls =
  let is_border_longhand d =
    is_border_width_decl d || is_border_style_decl d || is_border_color_decl d
  in
  let rec span pred acc = function
    | d :: rest when pred d -> span pred (d :: acc) rest
    | rest -> (List.rev acc, rest)
  in
  let rec go acc = function
    | [] -> List.rev acc
    | d :: _ as l when is_border_image_longhand d ->
        let img_block, rest1 = span is_border_image_longhand [] l in
        let long_block, rest2 = span is_border_longhand [] rest1 in
        if
          List.exists is_border_width_decl long_block
          && List.exists is_border_style_decl long_block
          && List.exists is_border_color_decl long_block
        then
          go (List.rev_append img_block (List.rev_append long_block acc)) rest2
        else go (List.rev_append img_block acc) rest1
    | d :: rest -> go (d :: acc) rest
  in
  go [] decls

let compose_border_whole_via_index ~bi_hazard ~ctx idx =
  let n = Rule_index.length idx in
  (* [border] resets [border-image] to its initial, so the synthesised shorthand
     is only safe when it ends up before every [border-image] declaration. Walk
     positions; once we cross a border-image decl, stop trying to fold. *)
  let i = ref 0 in
  let seen_border_image = ref false in
  while !i < n do
    if Rule_index.is_absorbed idx !i then incr i
    else if !seen_border_image then (
      let d = Rule_index.decl_at idx !i in
      if is_border_image_decl d then ();
      incr i)
    else
      let step_over () =
        let d = Rule_index.decl_at idx !i in
        if is_border_image_decl d then seen_border_image := true;
        incr i
      in
      match try_compose_border_whole_at ~bi_hazard ~ctx idx !i with
      | Some shorthand ->
          let absorbed = [ !i; !i + 1; !i + 2 ] in
          if Rule_index.absorb idx ~at:!i ~absorbed ~shorthand then i := !i + 3
          else step_over ()
      | None -> step_over ()
  done

(* CSS Backgrounds 3: the [border] shorthand resets [border-image] to its
   initial. A [border-image*] declaration is therefore dead when a later
   [border] shorthand of at least equal importance resets it and no later
   [border-image*] re-establishes it. Intra-block source order is fixed
   regardless of any surrounding CSS, so the drop is safe in every scope. *)
let drop_bimg_shadowed_by_border kept =
  let is_bimg (_, d) = is_border_image_decl d in
  let is_border (_, d) =
    match d with Declaration { property = Border; _ } -> true | _ -> false
  in
  let arr = Array.of_list kept in
  let n = Array.length arr in
  let rec any_bimg_from k =
    k < n && (is_bimg arr.(k) || any_bimg_from (k + 1))
  in
  let dead i =
    let img_important = is_important (snd arr.(i)) in
    let rec scan j =
      if j >= n then false
      else if
        is_border arr.(j) && (is_important (snd arr.(j)) || not img_important)
      then if any_bimg_from (j + 1) then scan (j + 1) else true
      else scan (j + 1)
    in
    scan (i + 1)
  in
  List.filteri (fun i item -> not (is_bimg item && dead i)) kept

(* CSS Backgrounds 3 sec. 5.7: compose [border-image] from a contiguous run of
   its longhands ([source] / [slice] [/ width [/ outset]] / [repeat]). The
   longhands are unknown properties, so the shorthand value is rebuilt from
   their text and re-parsed. The shorthand resets any longhand the run omits, so
   this is closed-world ([`Stylesheet]) only. *)
let is_border_image_longhand_decl = function
  | Declaration { property = Border_image_source; _ }
  | Declaration { property = Border_image_slice; _ }
  | Declaration { property = Border_image_width; _ }
  | Declaration { property = Border_image_outset; _ }
  | Declaration { property = Border_image_repeat; _ } ->
      true
  | _ -> false

(* Collect contiguous border-image longhands starting at [i], returning the run
   and its length [k]. *)
let span_border_image_run_at idx i =
  let n = Rule_index.length idx in
  let rec aux j acc =
    if j >= n then (List.rev acc, j - i)
    else if Rule_index.is_absorbed idx j then (List.rev acc, j - i)
    else
      let d = Rule_index.decl_at idx j in
      if is_border_image_longhand_decl d then aux (j + 1) ((j, d) :: acc)
      else (List.rev acc, j - i)
  in
  aux i []

(* The shorthand resets all five longhands, so a run naming every one of them is
   the same declaration whatever else the sheet holds. A shorter run leaves the
   rest to the reset, which is only safe with the whole sheet in view. *)
let border_image_run_can_compose run ~allow_partial ~foldable ~source ~slice
    ~width ~outset ~repeat =
  let need_slice =
    (Option.is_some width || Option.is_some outset) && Option.is_none slice
  in
  let complete =
    Option.is_some source && Option.is_some slice && Option.is_some width
    && Option.is_some outset && Option.is_some repeat
  in
  List.length run >= 2
  && same_importance (List.map snd run)
  && foldable && (not need_slice)
  && (allow_partial || complete)

(* The record is built here rather than read, so the slot-initial fold the
   reader's values get has to be applied to it as well. *)
let border_image_shorthand run ~source ~slice ~width ~outset ~repeat =
  Declaration.v
    ~important:(is_important (snd (List.hd run)))
    Border_image
    (Properties.normalize_border_image
       { source; slice; width; outset; repeat; mode = None })

let record_border_image_longhand
    ~(source : Properties.background_image option ref)
    ~(slice : Properties.border_image_slice_offsets option ref)
    ~(width : Properties.border_image_width_item list option ref)
    ~(outset : Properties.border_image_outset_item list option ref)
    ~(repeat : Properties.border_image_repeat_keyword list option ref) ~foldable
    ((_, d) : int * declaration) =
  match d with
  | Declaration { property = Border_image_source; value; _ } ->
      source := Some value
  | Declaration { property = Border_image_slice; value = Slices offsets; _ } ->
      slice := Some offsets
  | Declaration { property = Border_image_slice; _ } -> foldable := false
  | Declaration { property = Border_image_width; value = Widths l; _ } ->
      width := Some l
  | Declaration { property = Border_image_width; _ } -> foldable := false
  | Declaration { property = Border_image_outset; value = Outsets l; _ } ->
      outset := Some l
  | Declaration { property = Border_image_outset; _ } -> foldable := false
  | Declaration { property = Border_image_repeat; value = Repeats l; _ } ->
      repeat := Some l
  | Declaration { property = Border_image_repeat; _ } -> foldable := false
  | _ -> ()

let compose_border_image_run ~allow_partial
    (run : (int * Declaration.declaration) list) :
    (int * Declaration.declaration) option =
  let source : Properties.background_image option ref = ref Option.None in
  let slice : Properties.border_image_slice_offsets option ref =
    ref Option.None
  in
  let width : Properties.border_image_width_item list option ref =
    ref Option.None
  in
  let outset : Properties.border_image_outset_item list option ref =
    ref Option.None
  in
  let repeat : Properties.border_image_repeat_keyword list option ref =
    ref Option.None
  in
  (* A CSS-wide keyword or [var()] in any longhand cannot be folded into the
     [border_image] record, which holds plain component values. *)
  let foldable = ref true in
  List.iter
    (record_border_image_longhand ~source ~slice ~width ~outset ~repeat
       ~foldable)
    run;
  if
    not
      (border_image_run_can_compose run ~allow_partial ~foldable:!foldable
         ~source:!source ~slice:!slice ~width:!width ~outset:!outset
         ~repeat:!repeat)
  then Option.None
  else
    let merged =
      border_image_shorthand run ~source:!source ~slice:!slice ~width:!width
        ~outset:!outset ~repeat:!repeat
    in
    Some (fst (List.hd run), merged)

let try_compose_border_image_at ~allow_partial idx i =
  let d = Rule_index.decl_at idx i in
  if not (is_border_image_longhand_decl d) then None
  else
    let run, len = span_border_image_run_at idx i in
    match compose_border_image_run ~allow_partial run with
    | Some (_, shorthand) -> Some (shorthand, len)
    | None -> None

let compose_border_image_via_index ~ctx idx =
  let allow_partial = scope ctx = `Stylesheet in
  compose_run_via_index idx
    ~try_compose:(try_compose_border_image_at ~allow_partial)

let compose_border_image_shorthand ~ctx decls =
  let idx = Rule_index.build (List.map snd decls) in
  compose_border_image_via_index ~ctx idx;
  List.mapi (fun i d -> (i, d)) (Rule_index.to_list idx)
