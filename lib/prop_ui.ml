(* css-ui-4, css-scrollbars-1 and css-color-adjust-1: [cursor], [caret] and its
   longhands, [outline], [appearance], [resize], [user-select],
   [pointer-events], [touch-action], [interactivity], the [nav-*] and
   [interest-delay*] properties, [field-sizing], the scrollbar properties,
   [color-scheme], the colour-adjust properties, [text-size-adjust] and the
   legacy [-webkit-box-orient] / [-webkit-line-clamp] aliases.

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name here stays visible under [Css.Properties] exactly as when
   it lived in properties.ml. *)

open Common
open Values
open Properties_intf
open Prop_common

module Cursor_prop = struct
  let read_keyword (t : Cursor.t) : cursor =
    Cursor.enum "cursor"
      [
        ("auto", (Auto : cursor));
        ("default", Default);
        ("none", None);
        ("context-menu", Context_menu);
        ("help", Help);
        ("pointer", Pointer);
        ("progress", Progress);
        ("wait", Wait);
        ("cell", Cell);
        ("crosshair", Crosshair);
        ("text", Text);
        ("vertical-text", Vertical_text);
        ("alias", Alias);
        ("copy", Copy);
        ("move", Move);
        ("no-drop", No_drop);
        ("not-allowed", Not_allowed);
        ("grab", Grab);
        ("grabbing", Grabbing);
        ("all-scroll", All_scroll);
        ("col-resize", Col_resize);
        ("row-resize", Row_resize);
        ("n-resize", N_resize);
        ("e-resize", E_resize);
        ("s-resize", S_resize);
        ("w-resize", W_resize);
        ("ne-resize", Ne_resize);
        ("nw-resize", Nw_resize);
        ("se-resize", Se_resize);
        ("sw-resize", Sw_resize);
        ("ew-resize", Ew_resize);
        ("ns-resize", Ns_resize);
        ("nesw-resize", Nesw_resize);
        ("nwse-resize", Nwse_resize);
        ("zoom-in", Zoom_in);
        ("zoom-out", Zoom_out);
        ("inherit", Inherit);
        ("initial", Initial);
        ("unset", Unset);
        ("revert", Revert);
        ("revert-layer", Revert_layer);
      ]
      t

  let read_url_with_hotspot (t : Cursor.t) : string * (float * float) option =
    Cursor.ws t;
    let url =
      match Cursor.string_opt t with
      | Some s -> String.trim s
      | None -> Cursor.consume_remaining_as_string ~trim:true t
    in
    Cursor.ws t;
    let hotspot =
      if not (Cursor.is_done t) then (
        let x = Cursor.number t in
        Cursor.ws t;
        let y = Cursor.number t in
        Some (x, y))
      else None
    in
    (url, hotspot)

  let read_optional_hotspot (t : Cursor.t) : (float * float) option =
    Cursor.option
      (fun t ->
        let x = Cursor.number t in
        Cursor.ws t;
        let y = Cursor.number t in
        Cursor.ws t;
        if x < 0. || y < 0. then
          Cursor.err t "cursor hotspot coordinates cannot be negative"
        else (x, y))
      t

  let or_else a b = match a with Some _ -> a | None -> b

  let rec read_url_cursor (t : Cursor.t) : cursor =
    let (url, hotspot) : string * (float * float) option =
      (* Bare [url(foo.cur)] is a [Token.Url]; quoted [url("foo.cur")] is a
         [Func "url"] -- handle both. *)
      match Cursor.url_opt t with
      | Some url -> (url, None)
      | None -> Cursor.call "url" t read_url_with_hotspot
    in
    Cursor.ws t;
    let hotspot = or_else (read_optional_hotspot t) hotspot in
    if not (Cursor.comma_opt t) then
      err_invalid_value t "cursor" "url without fallback keyword"
    else
      let fallback = read t in
      (Url (url, hotspot, fallback) : cursor)

  and read_var (t : Cursor.t) : cursor = Var (Values.read_var read t)

  and read (t : Cursor.t) : cursor =
    Cursor.ws t;
    Cursor.one_of [ read_url_cursor; read_var; read_keyword ] t
end

let read_cursor t : cursor = Cursor_prop.read t

let rec read_interactivity t : interactivity =
  Cursor.enum_or_var "interactivity"
    [
      ("auto", (Auto : interactivity));
      ("inert", Inert);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_interactivity t))
    t

let rec read_caret_animation t : caret_animation =
  Cursor.enum_or_var "caret-animation"
    [
      ("auto", (Auto : caret_animation));
      ("manual", Manual);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_caret_animation t))
    t

let rec read_caret_shape t : caret_shape =
  Cursor.enum_or_var "caret-shape"
    [
      ("auto", (Auto : caret_shape));
      ("bar", Bar);
      ("block", Block);
      ("underscore", Underscore);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_caret_shape t))
    t

let read_caret_animation_component t : caret_animation =
  Cursor.enum "caret-animation component"
    [ ("manual", (Manual : caret_animation)) ]
    t

let read_caret_shape_component t : caret_shape =
  Cursor.enum "caret-shape component"
    [
      ("auto", (Auto : caret_shape));
      ("bar", Bar);
      ("block", Block);
      ("underscore", Underscore);
    ]
    t

let read_caret_shorthand t : caret =
  let rec loop color animation shape count =
    if Cursor.is_done t then
      if count = 0 then Cursor.err_expected t "caret"
      else
        match (color, animation, shape) with
        | Some (Auto : color), Option.None, Option.None -> (Auto : caret)
        | _ -> Caret (color, animation, shape)
    else
      let try_each =
        let attempts =
          List.filter_map Fun.id
            [
              (if Option.is_none animation then
                 Some (fun t -> `Animation (read_caret_animation_component t))
               else None);
              (if Option.is_none color then
                 Some (fun t -> `Color (read_auto_color t))
               else None);
              (if Option.is_none shape then
                 Some (fun t -> `Shape (read_caret_shape_component t))
               else None);
            ]
        in
        Cursor.one_of attempts t
      in
      match try_each with
      | `Color value -> loop (Some value) animation shape (count + 1)
      | `Animation value -> loop color (Some value) shape (count + 1)
      | `Shape value -> loop color animation (Some value) (count + 1)
  in
  loop None None None 0

let rec read_caret t : caret =
  Cursor.enum_or_var "caret"
    [
      ("inherit", (Inherit : caret));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_caret t))
    ~default:read_caret_shorthand t

let read_non_negative_duration t =
  match Values.read_duration_preserve_ms t with
  | Ms f when f < 0. -> Cursor.err_invalid t "negative duration"
  | S f when f < 0. -> Cursor.err_invalid t "negative duration"
  | duration -> duration

let rec read_interest_delay ?(longhand = false) t : interest_delay =
  Cursor.enum_or_var "interest-delay"
    [
      ("normal", (Normal : interest_delay));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var (read_interest_delay ~longhand) t))
    ~default:(fun t ->
      let at_most = if longhand then 1 else 2 in
      Durations
        (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most
           read_non_negative_duration t))
    t

let rec normalize_interest_delay : interest_delay -> interest_delay =
 fun value ->
  match value with
  | Durations durations ->
      preserve_if_equal value
        (Durations
           (map_preserve
              (Values.normalize_duration ~canonicalize_ms:false)
              durations))
  | Var v ->
      let v' = map_var_preserve normalize_interest_delay v in
      if v' == v then value else Var v'
  | _ -> value

let read_nav_scope t : nav_scope =
  Cursor.one_of
    [
      (fun t ->
        match Cursor.ident t with
        | "current" -> Current
        | "root" -> Root
        | ident -> Cursor.err_invalid t ("nav scope: " ^ ident));
      (fun t ->
        let name = Cursor.string t in
        if String.equal name "_self" then Cursor.err_invalid t "nav target"
        else Named name);
    ]
    t

let rec read_nav t : nav =
  Cursor.enum_or_var "nav"
    [
      ("auto", (Auto : nav));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_nav t))
    ~default:(fun t ->
      match Cursor.hash_opt t with
      | None -> Cursor.err_expected t "nav target"
      | Some target ->
          Cursor.ws t;
          let scope = Cursor.option read_nav_scope t in
          Target (target, scope))
    t

(* CSS UI 4 (ED) sec. 3.3 gives outline-style the initial value [none]. *)
let drop_initial_outline_style (style : outline_style option) :
    outline_style option =
  match style with Some (None : outline_style) -> Option.None | style -> style

(* CSS UI 4 (ED) sec. 3.2 gives outline-width the values and meaning of
   border-width, so the width slot takes the longhand's fold and its initial
   [medium] drops the same way. sec. 3.1 makes the outline shorthand set all
   three longhands, and CSS Cascade 5 (ED) sec. 3 assigns an omitted
   sub-property its initial value, so a shorthand left with no slot at all
   declares what [outline: none] declares - the node the keyword parses to, so
   the two spellings meet there. [auto] is not the initial style and stays: a
   lone [auto] and an [auto] beside a width both set outline-style and
   outline-color to [auto]. *)
let normalize_outline ?(lossless = false) : outline -> outline =
 fun value ->
  match value with
  | Shorthand s ->
      let width =
        Prop_background.drop_initial_line_width
          (option_map_preserve Prop_background.normalize_border_width s.width)
      in
      let style = drop_initial_outline_style s.style in
      let color = option_map_preserve (normalize_color ~lossless) s.color in
      if width == s.width && style == s.style && color == s.color then value
      else if
        Option.is_none width && Option.is_none style && Option.is_none color
      then (None : outline)
      else Shorthand { width; style; color }
  | other -> other

let normalize_caret ?(lossless = false) : caret -> caret =
 fun value ->
  match value with
  | Caret (color, anim, shape) ->
      preserve_if_equal value
        (Caret
           (option_map_preserve (normalize_color ~lossless) color, anim, shape))
  | other -> other

let normalize_scrollbar_color ?(lossless = false) :
    scrollbar_color -> scrollbar_color =
 fun value ->
  match value with
  | Colors (a, b) ->
      preserve_if_equal value
        (Colors (normalize_color ~lossless a, normalize_color ~lossless b))
  | other -> other

let rec pp_appearance : appearance Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_appearance ctx v
  | None -> Pp.string ctx "none"
  | Auto -> Pp.string ctx "auto"
  | Button -> Pp.string ctx "button"
  | Textfield -> Pp.string ctx "textfield"
  | Menulist -> Pp.string ctx "menulist"
  | Base_select -> Pp.string ctx "base-select"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_color_scheme : color_scheme Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_color_scheme ctx v
  | Normal -> Pp.string ctx "normal"
  | Light -> Pp.string ctx "light"
  | Dark -> Pp.string ctx "dark"
  | Light_dark ->
      Pp.string ctx "light";
      Pp.space ctx ();
      Pp.string ctx "dark"
  (* Color Adjust 1 SS 2.1: [only] is unordered relative to the scheme keywords;
     the canonical serialization (CSSOM, browsers) puts the scheme first and
     [only] last. *)
  | Only_light ->
      Pp.string ctx "light";
      Pp.space ctx ();
      Pp.string ctx "only"
  | Only_dark ->
      Pp.string ctx "dark";
      Pp.space ctx ();
      Pp.string ctx "only"
  | Only_light_dark ->
      Pp.string ctx "light";
      Pp.space ctx ();
      Pp.string ctx "dark";
      Pp.space ctx ();
      Pp.string ctx "only"
  | Custom names -> Pp.list ~sep:Pp.space pp_ident ctx names
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_print_color_adjust : print_color_adjust Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_print_color_adjust ctx v
  | Economy -> Pp.string ctx "economy"
  | Exact -> Pp.string ctx "exact"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_field_sizing : field_sizing Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_field_sizing ctx v
  | Content -> Pp.string ctx "content"
  | Fixed -> Pp.string ctx "fixed"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_scrollbar_width : scrollbar_width Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_scrollbar_width ctx v
  | Auto -> Pp.string ctx "auto"
  | Thin -> Pp.string ctx "thin"
  | None -> Pp.string ctx "none"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_scrollbar_color : scrollbar_color Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_scrollbar_color ctx v
  | Auto -> Pp.string ctx "auto"
  | Colors (thumb, track) ->
      pp_color ctx thumb;
      Pp.space ctx ();
      pp_color ctx track
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_scrollbar_gutter : scrollbar_gutter Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_scrollbar_gutter ctx v
  | Auto -> Pp.string ctx "auto"
  | Stable -> Pp.string ctx "stable"
  | Stable_both_edges -> Pp.string ctx "stable both-edges"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_cursor : cursor Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Default -> Pp.string ctx "default"
  | Pointer -> Pp.string ctx "pointer"
  | Wait -> Pp.string ctx "wait"
  | Text -> Pp.string ctx "text"
  | Move -> Pp.string ctx "move"
  | Help -> Pp.string ctx "help"
  | Not_allowed -> Pp.string ctx "not-allowed"
  | None -> Pp.string ctx "none"
  | Context_menu -> Pp.string ctx "context-menu"
  | Progress -> Pp.string ctx "progress"
  | Cell -> Pp.string ctx "cell"
  | Crosshair -> Pp.string ctx "crosshair"
  | Vertical_text -> Pp.string ctx "vertical-text"
  | Alias -> Pp.string ctx "alias"
  | Copy -> Pp.string ctx "copy"
  | No_drop -> Pp.string ctx "no-drop"
  | Grab -> Pp.string ctx "grab"
  | Grabbing -> Pp.string ctx "grabbing"
  | All_scroll -> Pp.string ctx "all-scroll"
  | Col_resize -> Pp.string ctx "col-resize"
  | Row_resize -> Pp.string ctx "row-resize"
  | N_resize -> Pp.string ctx "n-resize"
  | E_resize -> Pp.string ctx "e-resize"
  | S_resize -> Pp.string ctx "s-resize"
  | W_resize -> Pp.string ctx "w-resize"
  | Ne_resize -> Pp.string ctx "ne-resize"
  | Nw_resize -> Pp.string ctx "nw-resize"
  | Se_resize -> Pp.string ctx "se-resize"
  | Sw_resize -> Pp.string ctx "sw-resize"
  | Ew_resize -> Pp.string ctx "ew-resize"
  | Ns_resize -> Pp.string ctx "ns-resize"
  | Nesw_resize -> Pp.string ctx "nesw-resize"
  | Nwse_resize -> Pp.string ctx "nwse-resize"
  | Zoom_in -> Pp.string ctx "zoom-in"
  | Zoom_out -> Pp.string ctx "zoom-out"
  | Url (url, coords, fallback) ->
      Pp.url ctx url;
      (match coords with
      | Some (x, y) ->
          Pp.char ctx ' ';
          Pp.float ctx x;
          Pp.char ctx ' ';
          Pp.float ctx y
      | None -> ());
      Pp.comma ctx ();
      pp_cursor ctx fallback
  | Var v -> pp_var pp_cursor ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_interactivity : interactivity Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Inert -> Pp.string ctx "inert"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_interactivity ctx v

let rec pp_caret_animation : caret_animation Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Manual -> Pp.string ctx "manual"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_caret_animation ctx v

let rec pp_caret_shape : caret_shape Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Bar -> Pp.string ctx "bar"
  | Block -> Pp.string ctx "block"
  | Underscore -> Pp.string ctx "underscore"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_caret_shape ctx v

let rec pp_caret : caret Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Caret (color, animation, shape) ->
      let items =
        ( ( [] |> fun items ->
            match color with
            | None -> items
            | Some value -> (fun ctx -> pp_color ctx value) :: items )
        |> fun items ->
          match animation with
          | None -> items
          | Some value -> (fun ctx -> pp_caret_animation ctx value) :: items )
        |> fun items ->
        match shape with
        | None -> items
        | Some value -> (fun ctx -> pp_caret_shape ctx value) :: items
      in
      Pp.list ~sep:Pp.space (fun ctx pp -> pp ctx) ctx (List.rev items)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_caret ctx v

let rec pp_interest_delay : interest_delay Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Durations durations ->
      Pp.list ~sep:Pp.space pp_duration_preserve_ms ctx durations
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_interest_delay ctx v

let pp_nav_scope ctx = function
  | Current -> Pp.string ctx "current"
  | Root -> Pp.string ctx "root"
  | Named name -> Pp.quoted_string ctx name

let rec pp_nav : nav Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Target (target, scope) ->
      Pp.char ctx '#';
      Pp.string ctx target;
      Option.iter
        (fun scope ->
          Pp.space ctx ();
          pp_nav_scope ctx scope)
        scope
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_nav ctx v

let rec pp_resize : resize Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_resize ctx v
  | None -> Pp.string ctx "none"
  | Both -> Pp.string ctx "both"
  | Horizontal -> Pp.string ctx "horizontal"
  | Vertical -> Pp.string ctx "vertical"
  | Block -> Pp.string ctx "block"
  | Inline -> Pp.string ctx "inline"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_outline_style : outline_style Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Solid -> Pp.string ctx "solid"
  | Dashed -> Pp.string ctx "dashed"
  | Dotted -> Pp.string ctx "dotted"
  | Double -> Pp.string ctx "double"
  | Groove -> Pp.string ctx "groove"
  | Ridge -> Pp.string ctx "ridge"
  | Inset -> Pp.string ctx "inset"
  | Outset -> Pp.string ctx "outset"
  | Auto -> Pp.string ctx "auto"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_outline_style ctx v

let pp_outline_shorthand : outline_shorthand Pp.t =
 fun ctx { width; style; color } ->
  let first = ref true in
  let add_space () = if !first then first := false else Pp.space ctx () in
  Option.iter
    (fun w ->
      add_space ();
      Prop_background.pp_border_width ctx w)
    width;
  Option.iter
    (fun s ->
      add_space ();
      pp_outline_style ctx s)
    style;
  Option.iter
    (fun c ->
      add_space ();
      pp_color ctx c)
    color;
  (* The record is public, so a caller can hand over a value with no slot
     filled. It declares nothing but the initial longhands, which is what
     [outline: none] declares (CSS UI 4 (ED) sec. 3.1); the empty string is not
     a value any parser reads back. *)
  if !first then Pp.string ctx "none"

let rec pp_outline : outline Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_outline ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | None -> Pp.string ctx "none"
  | Shorthand shorthand -> pp_outline_shorthand ctx shorthand

let rec pp_forced_color_adjust : forced_color_adjust Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_forced_color_adjust ctx v
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Preserve_parent_color -> Pp.string ctx "preserve-parent-color"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_touch_action : touch_action Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_touch_action ctx v
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Pan_x -> Pp.string ctx "pan-x"
  | Pan_y -> Pp.string ctx "pan-y"
  | Pan_left -> Pp.string ctx "pan-left"
  | Pan_right -> Pp.string ctx "pan-right"
  | Pan_up -> Pp.string ctx "pan-up"
  | Pan_down -> Pp.string ctx "pan-down"
  | Pinch_zoom -> Pp.string ctx "pinch-zoom"
  | Manipulation -> Pp.string ctx "manipulation"
  | Actions actions -> Pp.list ~sep:Pp.space pp_touch_action ctx actions
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Vars vars -> Pp.list ~sep:Pp.space (pp_var pp_touch_action) ctx vars

let rec pp_webkit_appearance : webkit_appearance Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_webkit_appearance ctx v
  | None -> Pp.string ctx "none"
  | Auto -> Pp.string ctx "auto"
  | Button -> Pp.string ctx "button"
  | Textfield -> Pp.string ctx "textfield"
  | Menulist -> Pp.string ctx "menulist"
  | Listbox -> Pp.string ctx "listbox"
  | Checkbox -> Pp.string ctx "checkbox"
  | Radio -> Pp.string ctx "radio"
  | Push_button -> Pp.string ctx "push-button"
  | Square_button -> Pp.string ctx "square-button"
  | Apple_pay_button -> Pp.string ctx "-apple-pay-button"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_pointer_events : pointer_events Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_pointer_events ctx v
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Visible_painted -> Pp.string ctx "visiblepainted"
  | Visible_fill -> Pp.string ctx "visiblefill"
  | Visible_stroke -> Pp.string ctx "visiblestroke"
  | Visible -> Pp.string ctx "visible"
  | Painted -> Pp.string ctx "painted"
  | Fill -> Pp.string ctx "fill"
  | Stroke -> Pp.string ctx "stroke"
  | All -> Pp.string ctx "all"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_user_select : user_select Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_user_select ctx v
  | None -> Pp.string ctx "none"
  | Auto -> Pp.string ctx "auto"
  | Text -> Pp.string ctx "text"
  | All -> Pp.string ctx "all"
  | Contain -> Pp.string ctx "contain"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_webkit_box_orient : webkit_box_orient Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_webkit_box_orient ctx v
  | Horizontal -> Pp.string ctx "horizontal"
  | Vertical -> Pp.string ctx "vertical"
  | Inline_axis -> Pp.string ctx "inline-axis"
  | Block_axis -> Pp.string ctx "block-axis"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_moz_orient : moz_orient Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_moz_orient ctx v
  | Inline -> Pp.string ctx "inline"
  | Block -> Pp.string ctx "block"
  | Horizontal -> Pp.string ctx "horizontal"
  | Vertical -> Pp.string ctx "vertical"
  | Inherit -> Pp.string ctx "inherit"

let rec pp_webkit_line_clamp : webkit_line_clamp Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Lines n -> Pp.int ctx n
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_webkit_line_clamp ctx v

let rec read_user_select t : user_select =
  Cursor.enum_or_var "user-select"
    [
      ("none", (None : user_select));
      ("auto", Auto);
      ("text", Text);
      ("all", All);
      ("contain", Contain);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_user_select t) : user_select))
    t

let rec read_pointer_events t : pointer_events =
  Cursor.enum_or_var "pointer-events"
    [
      ("auto", (Auto : pointer_events));
      ("none", None);
      ("visiblepainted", Visible_painted);
      ("visiblefill", Visible_fill);
      ("visiblestroke", Visible_stroke);
      ("visible", Visible);
      ("painted", Painted);
      ("fill", Fill);
      ("stroke", Stroke);
      ("all", All);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_pointer_events t))
    t

let touch_action_is_var t =
  match Cursor.peek t with
  | Some (Component.Func { node = { name; _ }; _ }) ->
      String.equal (String.lowercase_ascii name) "var"
  | _ -> false

let touch_action_starts_keyword t =
  match Cursor.peek_ident t with
  | Some
      ( "auto" | "none" | "manipulation" | "inherit" | "initial" | "unset"
      | "revert" | "revert-layer" ) ->
      true
  | _ -> false

let read_touch_action_keyword t =
  Cursor.enum "touch-action"
    [
      ("auto", (Auto : touch_action));
      ("none", None);
      ("manipulation", Manipulation);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    t

let read_touch_action_gesture t =
  Cursor.enum "touch-action gesture"
    [
      ("pan-x", (Pan_x : touch_action));
      ("pan-y", Pan_y);
      ("pan-left", Pan_left);
      ("pan-right", Pan_right);
      ("pan-up", Pan_up);
      ("pan-down", Pan_down);
      ("pinch-zoom", Pinch_zoom);
    ]
    t

let check_touch_action_seen t has_horizontal has_vertical has_pinch = function
  | Pan_x | Pan_left | Pan_right ->
      if !has_horizontal then
        Cursor.err t "duplicate horizontal touch-action gesture";
      has_horizontal := true
  | Pan_y | Pan_up | Pan_down ->
      if !has_vertical then
        Cursor.err t "duplicate vertical touch-action gesture";
      has_vertical := true
  | Pinch_zoom ->
      if !has_pinch then Cursor.err t "duplicate pinch-zoom touch-action";
      has_pinch := true
  | _ -> Cursor.err t "invalid touch-action gesture"

let validate_touch_actions t actions =
  let has_horizontal = ref false in
  let has_vertical = ref false in
  let has_pinch = ref false in
  List.iter
    (check_touch_action_seen t has_horizontal has_vertical has_pinch)
    actions;
  match actions with [ action ] -> action | _ -> Actions actions

let rec read_touch_action_var t : touch_action var =
  Values.read_var read_touch_action t

and read_touch_action t : touch_action =
  let rec read_vars acc =
    Cursor.ws t;
    if touch_action_is_var t then read_vars (read_touch_action_var t :: acc)
    else List.rev acc
  in
  if touch_action_is_var t then
    let first = read_touch_action_var t in
    Vars (first :: read_vars [])
  else if touch_action_starts_keyword t then read_touch_action_keyword t
  else
    validate_touch_actions t
      (Cursor.list ~at_least:1 read_touch_action_gesture t)

let rec read_resize t : resize =
  Cursor.enum_or_var "resize"
    [
      ("none", (None : resize));
      ("both", Both);
      ("horizontal", Horizontal);
      ("vertical", Vertical);
      ("block", Block);
      ("inline", Inline);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_resize t))
    t

let rec read_field_sizing t : field_sizing =
  Cursor.enum_or_var "field-sizing"
    [
      ("content", (Content : field_sizing));
      ("fixed", Fixed);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_field_sizing t))
    t

let rec read_scrollbar_width (t : Cursor.t) : scrollbar_width =
  let keywords : (string * scrollbar_width) list =
    [
      ("auto", Auto);
      ("thin", Thin);
      ("none", None);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "scrollbar-width" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_scrollbar_width t) : scrollbar_width))
     t
    : scrollbar_width)

let rec read_scrollbar_color (t : Cursor.t) : scrollbar_color =
  let keywords : (string * scrollbar_color) list =
    [
      ("auto", Auto);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "scrollbar-color" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_scrollbar_color t) : scrollbar_color))
     ~default:(fun t ->
       let thumb = Values.read_color t in
       Cursor.ws t;
       let track = Values.read_color t in
       (Colors (thumb, track) : scrollbar_color))
     t
    : scrollbar_color)

let rec read_scrollbar_gutter (t : Cursor.t) : scrollbar_gutter =
  let keywords : (string * scrollbar_gutter) list =
    [
      ("auto", Auto);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "scrollbar-gutter" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_scrollbar_gutter t) : scrollbar_gutter))
     ~default:(fun t ->
       Cursor.expect_string "stable" t;
       Cursor.ws t;
       match Cursor.peek_ident t with
       | Some "both-edges" ->
           let _ = Cursor.ident t in
           Stable_both_edges
       | Some s ->
           Cursor.err_invalid t
             (String.concat "" [ "unexpected scrollbar-gutter modifier: "; s ])
       | None -> Stable)
     t
    : scrollbar_gutter)

let rec read_webkit_appearance t : webkit_appearance =
  Cursor.enum_or_var "webkit-appearance"
    [
      ("none", (None : webkit_appearance));
      ("auto", Auto);
      ("button", Button);
      ("textfield", Textfield);
      ("menulist", Menulist);
      ("listbox", Listbox);
      ("checkbox", Checkbox);
      ("radio", Radio);
      ("push-button", Push_button);
      ("square-button", Square_button);
      ("-apple-pay-button", Apple_pay_button);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_webkit_appearance t) : webkit_appearance))
    t

let rec read_webkit_box_orient t : webkit_box_orient =
  Cursor.enum_or_var "webkit-box-orient"
    [
      ("horizontal", (Horizontal : webkit_box_orient));
      ("vertical", Vertical);
      ("inline-axis", Inline_axis);
      ("block-axis", Block_axis);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_webkit_box_orient t))
    t

let read_moz_orient t : moz_orient =
  Cursor.enum "moz-orient"
    [
      ("inline", (Inline : moz_orient));
      ("block", Block);
      ("horizontal", Horizontal);
      ("vertical", Vertical);
      ("inherit", Inherit);
    ]
    t

let rec read_webkit_line_clamp t : webkit_line_clamp =
  let read_var t : webkit_line_clamp =
    Var (read_var read_webkit_line_clamp t)
  in
  Cursor.enum_or_calls "-webkit-line-clamp"
    [
      ("none", (None : webkit_line_clamp));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:(fun t ->
      let n = Cursor.int t in
      if n <= 0 then Cursor.err_invalid t "-webkit-line-clamp must be positive";
      Lines n)
    t

let rec read_forced_color_adjust t : forced_color_adjust =
  Cursor.enum_or_var "forced-color-adjust"
    [
      ("auto", (Auto : forced_color_adjust));
      ("none", None);
      ("preserve-parent-color", Preserve_parent_color);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_forced_color_adjust t) : forced_color_adjust))
    t

let rec read_appearance t : appearance =
  Cursor.enum_or_var "appearance"
    [
      ("none", (None : appearance));
      ("auto", Auto);
      ("button", Button);
      ("textfield", Textfield);
      ("menulist", Menulist);
      ("base-select", Base_select);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_appearance t) : appearance))
    t

let color_scheme_of_idents t names : color_scheme =
  match names with
  | [ "normal" ] -> Normal
  | [ "light" ] -> Light
  | [ "dark" ] -> Dark
  | [ "light"; "dark" ] | [ "dark"; "light" ] -> Light_dark
  | [ "only"; "light" ] | [ "light"; "only" ] -> Only_light
  | [ "only"; "dark" ] | [ "dark"; "only" ] -> Only_dark
  | [ "only"; "light"; "dark" ]
  | [ "only"; "dark"; "light" ]
  | [ "light"; "dark"; "only" ]
  | [ "dark"; "light"; "only" ] ->
      Only_light_dark
  | [ "inherit" ] -> Inherit
  | [ "initial" ] -> Initial
  | [ "unset" ] -> Unset
  | [ "revert" ] -> Revert
  | [ "revert-layer" ] -> Revert_layer
  | [] -> Cursor.err t "empty color-scheme"
  | _ ->
      (* CSS Color Adjust 1 section 2.2: [color-scheme] is [normal | [light |
         dark | <custom-ident>]+ && only?]. [normal] is mutually exclusive with
         the list form; [only] is a modifier that must accompany a non-empty
         list; CSS-wide keywords can only stand alone. *)
      let has_normal = List.mem "normal" names in
      let has_css_wide =
        List.exists
          (fun n ->
            List.mem (String.lowercase_ascii n)
              [ "inherit"; "initial"; "unset"; "revert"; "revert-layer" ])
          names
      in
      if has_normal then
        Cursor.err_invalid t
          "color-scheme: [normal] cannot be mixed with other keywords";
      if has_css_wide then
        Cursor.err_invalid t
          "color-scheme: CSS-wide keyword cannot be mixed with other keywords";
      let is_only n = String.equal (String.lowercase_ascii n) "only" in
      let non_only_names = List.filter (fun n -> not (is_only n)) names in
      if non_only_names = [] then
        Cursor.err_invalid t
          "color-scheme: [only] must be combined with a color scheme";
      if List.length (List.filter is_only names) > 1 then
        Cursor.err_invalid t "color-scheme: [only] cannot be repeated";
      Custom names

let rec read_color_scheme t : color_scheme =
  let rec read_idents acc =
    Cursor.ws t;
    if Cursor.is_done t then List.rev acc
    else read_idents (Cursor.ident t :: acc)
  in
  match Cursor.peek t with
  | Some (Component.Func { node = { name; _ }; _ })
    when String.equal (String.lowercase_ascii name) "var" ->
      Var (Values.read_var read_color_scheme t)
  | _ -> color_scheme_of_idents t (read_idents [])

let rec read_print_color_adjust t : print_color_adjust =
  Cursor.enum_or_var "print-color-adjust"
    [
      ("economy", (Economy : print_color_adjust));
      ("exact", Exact);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_print_color_adjust t))
    t

let rec read_outline_style t : outline_style =
  let read_var t : outline_style = Var (read_var read_outline_style t) in
  Cursor.enum_or_calls "outline-style"
    [
      ("none", (None : outline_style));
      ("solid", Solid);
      ("dashed", Dashed);
      ("dotted", Dotted);
      ("double", Double);
      ("groove", Groove);
      ("ridge", Ridge);
      ("inset", Inset);
      ("outset", Outset);
      ("auto", Auto);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    t

let outline_style_keywords =
  [
    "none";
    "solid";
    "dashed";
    "dotted";
    "double";
    "groove";
    "ridge";
    "inset";
    "outset";
    "auto";
  ]

let outline_starts_style t =
  List.exists (fun kw -> Cursor.looking_at t kw) outline_style_keywords

let read_outline_part ~width ~style ~color t =
  if Cursor.looking_at_func "var" t then
    (* A var() is type-ambiguous; assign it to the next unfilled
       width/style/color slot, reading its fallback with that slot's reader. *)
    if Option.is_none !width then
      width :=
        Some (Var (read_var Prop_background.read_border_width t) : border_width)
    else if Option.is_none !style then
      style := Some (Var (read_var read_outline_style t) : outline_style)
    else if Option.is_none !color then color := Some (read_auto_color t)
    else Cursor.err_expected t "outline"
  else if Option.is_none !style && outline_starts_style t then
    style := Some (read_outline_style t)
  else
    (* The width slot is a [<line-width>], so it takes the thin/medium/thick
       keywords and the math functions as well as a length; bind it by trying
       that reader rather than by guessing from the first token. *)
    match
      if Option.is_none !width then
        Cursor.option Prop_background.read_border_width t
      else Option.None
    with
    | Some w -> width := Some w
    | Option.None ->
        if Option.is_none !color then color := Some (read_auto_color t)
        else Cursor.err_expected t "outline"

let read_outline_parts ~width ~style ~color t =
  let rec loop () =
    Cursor.ws t;
    if not (Cursor.is_done t) then (
      read_outline_part ~width ~style ~color t;
      loop ())
  in
  loop ()

(* CSS UI 4 (ED) sec. 3.1 writes the shorthand as [<'outline-width'> ||
   <'outline-style'> || <'outline-color'>], and CSS Values 4 (ED) sec. 2.2 has
   [||] require one or more of its options to occur, so an empty value matches
   no outline grammar and CSS Syntax 3 (ED) sec. 5.5.6 drops the declaration. *)
let read_outline_shorthand_value t : outline =
  let width = ref Option.None in
  let style = ref Option.None in
  let color = ref Option.None in
  read_outline_parts ~width ~style ~color t;
  match (!width, !style, !color) with
  | Option.None, Some (None : outline_style), Option.None -> None
  | Option.None, Option.None, Option.None ->
      Cursor.err_expected t "outline width, style or color"
  | _ -> Shorthand { width = !width; style = !style; color = !color }

let rec read_outline t : outline =
  Cursor.enum_or_var "outline"
    [
      ("inherit", (Inherit : outline));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (* A lone var() is the whole value; a var() followed by more components is
         a shorthand whose first slot happens to be a var. *)
      let snap = Cursor.save t in
      let v = Values.read_var read_outline t in
      Cursor.ws t;
      if Cursor.is_done t || Cursor.peek_comma t then (Var v : outline)
      else (
        Cursor.restore t snap;
        read_outline_shorthand_value t))
    ~default:read_outline_shorthand_value t

let read_outline_shorthand t : outline_shorthand =
  match read_outline t with
  | Shorthand s -> s
  | Inherit | Initial | Unset | Revert | Revert_layer | None ->
      { width = Option.None; style = Option.None; color = Option.None }
  | Var _ -> Cursor.err_invalid t "outline var shorthand"
