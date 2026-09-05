(* CSS Multicol 2, CSS Fragmentation 4 and CSS Paged Media 3: the [columns]
   shorthand and its longhands, fragmentation breaks (with the legacy
   [page-break-*] aliases), [box-decoration-break] and the paged-media [size]
   descriptor.

   [column-rule] is a <border> and [column-gap] a <length>, so both are read and
   printed by the border and gap machinery of their own families.

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name here stays visible under [Css.Properties] exactly as when
   it lived in properties.ml. *)

open Values
open Properties_intf

let rec pp_box_decoration_break : box_decoration_break Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_box_decoration_break ctx v
  | Clone -> Pp.string ctx "clone"
  | Slice -> Pp.string ctx "slice"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_break_value : break_value Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_break_value ctx v
  | Auto -> Pp.string ctx "auto"
  | Avoid -> Pp.string ctx "avoid"
  | All -> Pp.string ctx "all"
  | Avoid_page -> Pp.string ctx "avoid-page"
  | Page -> Pp.string ctx "page"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Recto -> Pp.string ctx "recto"
  | Verso -> Pp.string ctx "verso"
  | Avoid_column -> Pp.string ctx "avoid-column"
  | Column -> Pp.string ctx "column"
  | Avoid_region -> Pp.string ctx "avoid-region"
  | Region -> Pp.string ctx "region"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_break_inside_value : break_inside_value Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_break_inside_value ctx v
  | Auto -> Pp.string ctx "auto"
  | Avoid -> Pp.string ctx "avoid"
  | Avoid_page -> Pp.string ctx "avoid-page"
  | Avoid_column -> Pp.string ctx "avoid-column"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_page_break_value : page_break_value Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_page_break_value ctx v
  | Auto -> Pp.string ctx "auto"
  | Always -> Pp.string ctx "always"
  | Avoid -> Pp.string ctx "avoid"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Inherit -> Pp.string ctx "inherit"

let rec pp_page_break_inside_value : page_break_inside_value Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_page_break_inside_value ctx v
  | Auto -> Pp.string ctx "auto"
  | Avoid -> Pp.string ctx "avoid"
  | Inherit -> Pp.string ctx "inherit"

(* CSS Fragmentation 3 sec. 3.4 defines the page-break-* aliases by a value
   mapping table: [auto | left | right | avoid] map to themselves and [always]
   maps to [page]. The table is keyed on the value, so a value it does not name
   has no break-* spelling and declines. A [var()] is one: substitution happens
   at computed-value time, and [always], the one mapping that is not the
   identity, is not a break-* value to fall back on. *)
let break_of_page_break (value : page_break_value) : break_value option =
  match value with
  | Auto -> Some Auto
  | Always -> Some Page
  | Avoid -> Some Avoid
  | Left -> Some Left
  | Right -> Some Right
  | Inherit -> Some Inherit
  | Var _ -> None

let break_inside_of_page_break (value : page_break_inside_value) :
    break_inside_value option =
  match value with
  | Auto -> Some Auto
  | Avoid -> Some Avoid
  | Inherit -> Some Inherit
  | Var _ -> None

(* CSS Multicol 2 sec. 4.5 leaves an omitted component at its longhand's
   initial, and [auto] is the width's (sec. 4.1), so [auto <count>] names what
   [<count>] names and the shorter spelling wins. *)
let normalize_columns_value : columns_value -> columns_value =
 fun value -> match value with Auto_count n -> Count n | other -> other

let rec pp_columns_value : columns_value Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Count n -> Pp.int ctx n
  | Width len -> pp_length ctx len
  | Both (len, n) ->
      pp_length ctx len;
      Pp.space ctx ();
      Pp.int ctx n
  | Auto_count n ->
      Pp.string ctx "auto";
      Pp.space ctx ();
      Pp.int ctx n
  | Var v -> pp_var pp_columns_value ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_column_width : column_width Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Width len -> pp_length ctx len
  | Var v -> pp_var pp_column_width ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

(* CSS Multicol 2 sec. 4.2 and 4.4. *)
let rec pp_column_height : column_height Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Height len -> pp_length ctx len
  | Var v -> pp_var pp_column_height ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_column_wrap : column_wrap Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Nowrap -> Pp.string ctx "nowrap"
  | Wrap -> Pp.string ctx "wrap"
  | Var v -> pp_var pp_column_wrap ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_column_count : column_count Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Count n -> Pp.int ctx n
  | Var v -> pp_var pp_column_count ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_column_span : column_span Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | All -> Pp.string ctx "all"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_column_span ctx v

let rec pp_page_size_name : page_size_name Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_page_size_name ctx v
  | A5 -> Pp.string ctx "A5"
  | A4 -> Pp.string ctx "A4"
  | A3 -> Pp.string ctx "A3"
  | B5 -> Pp.string ctx "B5"
  | B4 -> Pp.string ctx "B4"
  | Jis_b5 -> Pp.string ctx "JIS-B5"
  | Jis_b4 -> Pp.string ctx "JIS-B4"
  | Letter -> Pp.string ctx "letter"
  | Legal -> Pp.string ctx "legal"
  | Ledger -> Pp.string ctx "ledger"

let rec pp_page_size_orientation : page_size_orientation Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_page_size_orientation ctx v
  | Portrait -> Pp.string ctx "portrait"
  | Landscape -> Pp.string ctx "landscape"

let rec pp_page_size : page_size Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Single len -> pp_length ctx len
  | Pair (width, height) ->
      pp_length ctx width;
      Pp.space ctx ();
      pp_length ctx height
  | Named name -> pp_page_size_name ctx name
  | Named_oriented (name, orientation) ->
      pp_page_size_name ctx name;
      Pp.space ctx ();
      pp_page_size_orientation ctx orientation
  | Oriented orientation -> pp_page_size_orientation ctx orientation
  | Inherit -> Pp.string ctx "inherit"
  | Var v -> pp_var pp_page_size ctx v

let rec read_break_value t : break_value =
  Cursor.enum_or_var "break"
    [
      ("auto", (Auto : break_value));
      ("avoid", Avoid);
      ("all", All);
      (* CSS Fragmentation 3 sec. 3.4: legacy [page-break-*: always] maps to
         [break-*: page]. The reader accepts the legacy spelling so the
         page-break alias dispatch (which routes to [Break_before/after]) can
         keep using this reader. *)
      ("always", Page);
      ("avoid-page", Avoid_page);
      ("page", Page);
      ("left", Left);
      ("right", Right);
      ("recto", Recto);
      ("verso", Verso);
      ("avoid-column", Avoid_column);
      ("column", Column);
      ("avoid-region", Avoid_region);
      ("region", Region);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_break_value t))
    t

let rec read_break_inside_value t : break_inside_value =
  Cursor.enum_or_var "break-inside"
    [
      ("auto", (Auto : break_inside_value));
      ("avoid", Avoid);
      ("avoid-page", Avoid_page);
      ("avoid-column", Avoid_column);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_break_inside_value t))
    t

let rec read_page_break_value t : page_break_value =
  Cursor.enum_or_var "page-break"
    [
      ("auto", (Auto : page_break_value));
      ("always", Always);
      ("avoid", Avoid);
      ("left", Left);
      ("right", Right);
      ("inherit", Inherit);
    ]
    ~var:(fun t -> Var (Values.read_var read_page_break_value t))
    t

let rec read_page_break_inside_value t : page_break_inside_value =
  Cursor.enum_or_var "page-break-inside"
    [
      ("auto", (Auto : page_break_inside_value));
      ("avoid", Avoid);
      ("inherit", Inherit);
    ]
    ~var:(fun t -> Var (Values.read_var read_page_break_inside_value t))
    t

let read_columns_count t =
  let n = read_integer "column-count" t in
  if n <= 0 then Cursor.err_invalid t "column count must be positive";
  n

let read_columns_component t =
  Cursor.one_of
    [
      (fun t ->
        Cursor.expect_string "auto" t;
        `Auto);
      (fun t -> `Count (read_columns_count t));
      (fun t -> `Width (read_length t));
    ]
    t

let combine_columns_components t a b : columns_value =
  match (a, b) with
  | `Auto, `Auto -> (Auto : columns_value)
  | `Auto, `Count n | `Count n, `Auto -> Auto_count n
  | `Auto, `Width w | `Width w, `Auto -> Width w
  | `Width w, `Count n | `Count n, `Width w -> Both (w, n)
  | `Count _, `Count _ -> Cursor.err_invalid t "duplicate column-count"
  | `Width _, `Width _ -> Cursor.err_invalid t "duplicate column-width"

let columns_value_of_component :
    [< `Auto | `Count of int | `Width of Values.length ] -> columns_value =
  function
  | `Auto -> Auto
  | `Count n -> (Count n : columns_value)
  | `Width w -> Width w

let read_columns_components t : columns_value =
  let first = read_columns_component t in
  Cursor.ws t;
  match Cursor.option read_columns_component t with
  | Some second -> combine_columns_components t first second
  | None -> columns_value_of_component first

let rec read_columns_value t : columns_value =
  (* CSS Multicol 2 sec. 4.5: [<'column-width'> || <'column-count'>], where
     column-width is [auto | <length>] and column-count is [auto | <integer>].
     Read up to two space-separated components in any order, then assign the
     length to the width slot and the integer to the count slot. An explicit
     [auto] keeps the width unset; [columns: auto 3] therefore differs from the
     bare [columns: 3] only in spelling, captured by [Auto_count]. *)
  Cursor.enum_or_var "columns"
    [
      ("inherit", (Inherit : columns_value));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_columns_value t))
    ~default:read_columns_components t

let rec read_column_width t : column_width =
  Cursor.enum_or_var "column-width"
    [
      ("auto", (Auto : column_width));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_column_width t))
    ~default:(fun t -> (Width (read_length t) : column_width))
    t

(* The height takes a length and no percentage, which Chrome 146 refuses. *)
let rec read_column_height t : column_height =
  Cursor.enum_or_var "column-height"
    [
      ("auto", (Auto : column_height));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_column_height t))
    ~default:(fun t ->
      (* The generic length reader carries a keyword list of its own -
         [min-content], [none] and [normal] among them - and the height takes
         none of those beside its own [auto]. *)
      (Height
         (read_length ~length_only:true ~allow_negative:false
            ~with_keywords:false t)
        : column_height))
    t

let rec read_column_wrap t : column_wrap =
  Cursor.enum_or_var "column-wrap"
    [
      ("auto", (Auto : column_wrap));
      ("nowrap", Nowrap);
      ("wrap", Wrap);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_column_wrap t))
    t

let rec read_column_count t : column_count =
  Cursor.enum_or_var "column-count"
    [
      ("auto", (Auto : column_count));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_column_count t))
    ~default:(fun t -> (Count (read_columns_count t) : column_count))
    t

let rec read_column_span t : column_span =
  Cursor.enum_or_var "column-span"
    [
      ("none", (None : column_span));
      ("all", All);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_column_span t))
    t

let read_page_size_name t : page_size_name =
  Cursor.enum "page-size name"
    [
      ("a5", A5);
      ("a4", A4);
      ("a3", A3);
      ("b5", B5);
      ("b4", B4);
      ("jis-b5", Jis_b5);
      ("jis-b4", Jis_b4);
      ("letter", Letter);
      ("legal", Legal);
      ("ledger", Ledger);
    ]
    t

let read_page_size_orientation t : page_size_orientation =
  Cursor.enum "page-size orientation"
    [ ("portrait", Portrait); ("landscape", Landscape) ]
    t

let rec read_page_size t : page_size =
  let read_var_ps t : page_size = Var (read_var read_page_size t) in
  let read_named t =
    let name = read_page_size_name t in
    Cursor.ws t;
    if Cursor.is_done t then Named name
    else
      let orientation = read_page_size_orientation t in
      Cursor.ws t;
      Cursor.expect_eof t;
      Named_oriented (name, orientation)
  in
  let read_oriented t =
    let orientation = read_page_size_orientation t in
    Cursor.ws t;
    Cursor.expect_eof t;
    Oriented orientation
  in
  let read_lengths t =
    let first = read_length t in
    Cursor.ws t;
    if Cursor.is_done t then Single first
    else
      let second = read_length t in
      Cursor.ws t;
      Cursor.expect_eof t;
      Pair (first, second)
  in
  Cursor.enum_or_calls "page-size"
    [ ("auto", Auto); ("inherit", Inherit) ]
    ~calls:[ ("var", read_var_ps) ]
    ~default:(Cursor.one_of [ read_named; read_oriented; read_lengths ])
    t

let rec read_box_decoration_break t : box_decoration_break =
  Cursor.enum_or_var "box-decoration-break"
    [
      ("clone", (Clone : box_decoration_break));
      ("slice", Slice);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_box_decoration_break t))
    t
