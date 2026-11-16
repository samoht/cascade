open Common
open Values
open Properties_intf
open Prop_image
open Prop_background_layer

let rec pp_border_image_slice_item ctx (value : border_image_slice_item) =
  match value with
  | Number n -> Values.pp_number ctx n
  | Pct n -> Pp.pct ctx n
  | Calc c ->
      (* CSS Values 4 sec. 10.13 keeps the call valid where the [0,inf] range is
         exceeded, so [calc(-10%)] computes and a bare [-10%] is dropped. *)
      Values.pp_calc ~unwrap_num:false
        ~unwrap:(fun v ->
          match (v : border_image_slice_item) with
          | Pct f -> f >= 0.
          | _ -> true)
        pp_border_image_slice_item ctx c

let pp_border_image_slice_offsets ctx { offsets; fill } =
  Pp.list ~sep:Pp.space pp_border_image_slice_item ctx offsets;
  if fill then (
    Pp.space ctx ();
    Pp.string ctx "fill")

let rec pp_border_image_slice ctx (value : border_image_slice) =
  match value with
  | Slices offsets -> pp_border_image_slice_offsets ctx offsets
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_border_image_slice ctx v

let pp_border_image_width_item ctx (value : border_image_width_item) =
  match value with
  | Number n -> Values.pp_number ctx n
  | Pct n -> Pp.pct ctx n
  | Length len -> pp_length ctx len
  | Auto -> Pp.string ctx "auto"

let pp_border_image_outset_item ctx (value : border_image_outset_item) =
  match value with
  | Number n -> Values.pp_number ctx n
  | Length len -> pp_length ctx len

let pp_border_image_repeat_keyword ctx (value : border_image_repeat_keyword) =
  match value with
  | Stretch -> Pp.string ctx "stretch"
  | Repeat -> Pp.string ctx "repeat"
  | Round -> Pp.string ctx "round"
  | Space -> Pp.string ctx "space"

let rec pp_border_image_repeat ctx : border_image_repeat -> unit = function
  | Repeats l -> Pp.list ~sep:Pp.space pp_border_image_repeat_keyword ctx l
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_border_image_repeat ctx v

let rec pp_border_image_width ctx : border_image_width -> unit = function
  | Widths l -> Pp.list ~sep:Pp.space pp_border_image_width_item ctx l
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_border_image_width ctx v

let rec pp_border_image_outset ctx : border_image_outset -> unit = function
  | Outsets l -> Pp.list ~sep:Pp.space pp_border_image_outset_item ctx l
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_border_image_outset ctx v

let pp_mask_border_mode ctx = function
  | (Alpha : mask_border_mode) -> Pp.string ctx "alpha"
  | Luminance -> Pp.string ctx "luminance"

(* CSS Backgrounds 3 sec. 6.1: a component the shorthand leaves out takes its
   longhand's initial - [none] for the source (sec. 5.1), [100%] for the slice
   (sec. 5.2), [1] for the width (sec. 5.3), [0] for the outset (sec. 5.4) and
   [stretch] for the repeat (sec. 5.5) - so writing one out names what leaving
   it out names. Drained of every slot the shorthand declares nothing but
   initials, which is what [none] declares, so the source stays to say it. *)
let normalize_border_image : border_image -> border_image =
 fun value ->
  let drop is_initial slot =
    match slot with Some v when is_initial v -> Option.None | slot -> slot
  in
  let source =
    drop
      (function (None : background_image) | Initial -> true | _ -> false)
      value.source
  in
  let slice_initial =
    drop
      (fun (s : border_image_slice_offsets) ->
        (not s.fill)
        &&
        match s.offsets with
        | [ (Pct 100. : border_image_slice_item) ] -> true
        | _ -> false)
      value.slice
  in
  let outset =
    drop
      (function
        | [ (Number (Num 0.) : border_image_outset_item) ] | [ Length Zero ] ->
            true
        | _ -> false)
      value.outset
  in
  (* The printer writes one [/] per slot, so a dropped width with a kept outset
     would put the outset in the width's place. The width only goes when the
     outset does. *)
  let width =
    if Option.is_some outset then value.width
    else
      drop
        (function
          | [ (Number (Num 1.) : border_image_width_item) ] -> true | _ -> false)
        value.width
  in
  let repeat =
    drop (fun r -> r = [ (Stretch : border_image_repeat_keyword) ]) value.repeat
  in
  (* The printer writes the slice before the first [/], so a kept width or
     outset needs it even at its initial. *)
  let slice =
    if Option.is_some width || Option.is_some outset then value.slice
    else slice_initial
  in
  let drained =
    Option.is_none source && Option.is_none slice && Option.is_none width
    && Option.is_none outset && Option.is_none repeat
  in
  let source = if drained then Some (None : background_image) else source in
  { value with source; slice; width; outset; repeat }

let pp_border_image : border_image Pp.t =
 fun ctx { source; slice; width; outset; repeat; mode } ->
  let first = ref true in
  (* CSS Syntax 3 (ED) sec. 9: tokens ending with [)] are self-delimiting, so
     the inter-slot space after [url(...)] / [<image>] can be elided under
     minify. *)
  let last_is_self_delim () =
    match Pp.last_char ctx with Some (')' | ']' | '}') -> true | _ -> false
  in
  let maybe_space () =
    if !first then first := false
    else if Pp.minified ctx && last_is_self_delim () then ()
    else Pp.space ctx ()
  in
  pp_bg_prop maybe_space pp_background_image ctx source;
  pp_bg_prop maybe_space pp_border_image_slice_offsets ctx slice;
  (match width with
  | None -> ()
  | Some width ->
      first := false;
      Pp.char ctx '/';
      Pp.list ~sep:Pp.space pp_border_image_width_item ctx width);
  (match outset with
  | None -> ()
  | Some outset ->
      first := false;
      Pp.char ctx '/';
      Pp.list ~sep:Pp.space pp_border_image_outset_item ctx outset);
  pp_bg_prop maybe_space
    (Pp.list ~sep:Pp.space pp_border_image_repeat_keyword)
    ctx repeat;
  pp_bg_prop maybe_space pp_mask_border_mode ctx mode;
  (* The record is public, so a caller can hand over a value with no slot
     filled. It declares nothing but the initial longhands, which is what
     [border-image: none] and [mask-border: none] declare (CSS Backgrounds 3
     (ED) sec. 5.7, CSS Masking 1 (ED) sec. 8.7); the empty string is not a
     value any parser reads back. *)
  if !first then Pp.string ctx "none"

(* A mask-border that fills no slot declares what [mask-border: none] declares,
   and CSS Masking 1 (ED) sec. 8.1 gives mask-border-source the initial value
   [none], so that keyword is where the two spellings meet. *)
let drained_mask_border : border_image =
  {
    source = Some (None : background_image);
    slice = None;
    width = None;
    outset = None;
    repeat = None;
    mode = None;
  }

(* CSS Masking 1 (ED) sec. 8.2 gives mask-border-mode the initial value [alpha],
   and sec. 8.7 sets an omitted shorthand slot to its initial value, so an
   explicit [alpha] declares what leaving the slot out declares. [luminance] is
   the other mode and stays. Written on its own the mode is the whole value, so
   dropping it drains the shorthand. *)
let normalize_mask_border (value : border_image) : border_image =
  match value.mode with
  | Some (Alpha : mask_border_mode) -> (
      match
        (value.source, value.slice, value.width, value.outset, value.repeat)
      with
      | None, None, None, None, None -> drained_mask_border
      | _ -> { value with mode = Option.None })
  | _ -> value

let read_border_image_repeat_keyword t : border_image_repeat_keyword =
  Cursor.enum "border-image-repeat"
    [
      ("stretch", (Stretch : border_image_repeat_keyword));
      ("repeat", Repeat);
      ("round", Round);
      ("space", Space);
    ]
    t

(* Sec. 5.2 to 5.4 keep the numeric halves at [0,inf]. A [calc()] holds no value
   to compare, so only a literal is turned away here. Each side is one component
   of a list, and the whole-value number reader refuses a number after the one
   it read, so the component is handed to it on its own. *)
let read_border_image_number t =
  let value : number =
    match Cursor.peek t with
    | Some (Component.Func _ as component) ->
        let _ = Cursor.next t in
        Values.read_number (Cursor.of_components [ component ])
    | _ -> Num (Cursor.number t)
  in
  (match value with
  | (Num n : number) when n < 0. ->
      Cursor.err_invalid t "border-image value cannot be negative"
  | _ -> ());
  value

let read_slice_percentage_leaf t : border_image_slice_item =
  Cursor.ws t;
  Pct (Cursor.pct t)

let read_border_image_slice_item t : border_image_slice_item =
  match Cursor.percentage_opt t with
  | Some n when n >= 0. -> Pct n
  | Some _ -> Cursor.err_invalid t "border-image value cannot be negative"
  | None ->
      (* The number side reads its own math; a percentage one reaches the second
         arm only because [read_border_image_number] refuses it. Sec. 6.2 spells
         the slot [<number> | <percentage>] with no length in it, so a call
         answering its arguments' type stands here only as a percentage. *)
      Cursor.one_of
        [
          (fun t ->
            (Number (read_border_image_number t) : border_image_slice_item));
          (fun t ->
            Calc
              (Values.read_calc ~result_type:`Number_or_percentage
                 read_slice_percentage_leaf t));
        ]
        t

let read_border_image_slice_value t values has_fill =
  match Cursor.option read_border_image_slice_item t with
  | Some value ->
      if List.length values >= 4 then
        Cursor.err_invalid t "too many border-image slice values";
      `Continue (value :: values, has_fill)
  | None -> `Stop

let read_border_image_slice_step t values has_fill =
  Cursor.ws t;
  if Cursor.is_done t || Cursor.peek_delim t = Some '/' then `Stop
  else
    match Cursor.peek_keyword t with
    | Some "fill" ->
        if has_fill then
          Cursor.err_invalid t "duplicate border-image fill keyword";
        let _ = Cursor.ident t in
        `Continue (values, true)
    | _ -> read_border_image_slice_value t values has_fill

let read_border_image_slice_offsets t : border_image_slice_offsets =
  let rec loop values has_fill =
    match read_border_image_slice_step t values has_fill with
    | `Stop -> (values, has_fill)
    | `Continue (values, has_fill) -> loop values has_fill
  in
  let values, has_fill = loop [] false in
  match (List.rev values, has_fill) with
  | [], true -> Cursor.err_invalid t "border-image fill requires slice values"
  | [], false -> Cursor.err_expected t "border-image slice"
  | offsets, fill -> { offsets; fill }

(* CSS Cascade 5 sec. 7.3 gives the longhand the CSS-wide keywords; the
   shorthand takes offsets alone. *)
let rec read_border_image_slice t : border_image_slice =
  match Cursor.peek_keyword t with
  | Some ("initial" | "inherit" | "unset" | "revert" | "revert-layer" | "var")
    ->
      Cursor.enum_or_var "border-image-slice"
        [
          ("initial", (Initial : border_image_slice));
          ("inherit", Inherit);
          ("unset", Unset);
          ("revert", Revert);
          ("revert-layer", Revert_layer);
        ]
        ~var:(fun t -> Var (Values.read_var read_border_image_slice t))
        t
  | Some _ | None -> Slices (read_border_image_slice_offsets t)

let read_border_image_width_item t : border_image_width_item =
  let auto t =
    Cursor.enum "border-image-width"
      [ ("auto", (Auto : border_image_width_item)) ]
      t
  in
  match Cursor.option auto t with
  | Some value -> value
  | None -> (
      match Cursor.percentage_opt t with
      | Some n when n >= 0. -> Pct n
      | Some _ -> Cursor.err_invalid t "border-image value cannot be negative"
      | None ->
          Cursor.one_of
            [
              (fun t ->
                (Number (read_border_image_number t) : border_image_width_item));
              (* Sec. 5.3 gives the width a number, a length-percentage or
                 [auto], read above. The generic length reader carries keywords
                 of its own - [stretch] and [contain] among them, which name a
                 repeat here - so this call takes none. *)
              (fun t ->
                Length
                  (read_length ~allow_negative:false ~with_keywords:false t));
            ]
            t)

(* Sec. 5.4 gives the outset a number or a length per side and no keyword. *)
let read_border_image_outset_item t : border_image_outset_item =
  Cursor.one_of
    [
      (fun t ->
        (Number (read_border_image_number t) : border_image_outset_item));
      (* Sec. 5.4 gives the outset a number or a length, and no percentage. *)
      (fun t ->
        Length
          (read_length ~allow_negative:false ~with_keywords:false
             ~length_only:true t));
    ]
    t

let read_border_image_box_step ~what read_item t acc =
  Cursor.ws t;
  if Cursor.is_done t || Cursor.peek_delim t = Some '/' then `Stop
  else
    match Cursor.option read_item t with
    | Some value ->
        if List.length acc >= 4 then
          Cursor.err_invalid t ("too many border-image " ^ what ^ " values");
        `Continue (value :: acc)
    | None -> `Stop

let read_border_image_box_values ~what read_item t =
  let rec loop acc =
    match read_border_image_box_step ~what read_item t acc with
    | `Stop -> List.rev acc
    | `Continue acc -> loop acc
  in
  match loop [] with
  | [] -> Cursor.err_expected t ("border-image " ^ what)
  | values -> values

let read_border_image_repeat_keywords t =
  let first = read_border_image_repeat_keyword t in
  Cursor.ws t;
  match Cursor.option read_border_image_repeat_keyword t with
  | None -> [ first ]
  | Some second -> [ first; second ]

let rec read_border_image_repeat t : border_image_repeat =
  Cursor.enum_or_var "border-image-repeat"
    [
      ("inherit", (Inherit : border_image_repeat));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_border_image_repeat t))
    ~default:(fun t -> Repeats (read_border_image_repeat_keywords t))
    t

let rec read_border_image_width t : border_image_width =
  Cursor.enum_or_whole_value_var "border-image-width"
    [
      ("inherit", (Inherit : border_image_width));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_border_image_width t))
    ~default:(fun t ->
      Widths
        (read_border_image_box_values ~what:"width" read_border_image_width_item
           t))
    t

let rec read_border_image_outset t : border_image_outset =
  Cursor.enum_or_whole_value_var "border-image-outset"
    [
      ("inherit", (Inherit : border_image_outset));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_border_image_outset t))
    ~default:(fun t ->
      Outsets
        (read_border_image_box_values ~what:"outset"
           read_border_image_outset_item t))
    t

let read_mask_border_mode t =
  Cursor.enum "mask-border-mode"
    [ ("alpha", (Alpha : mask_border_mode)); ("luminance", Luminance) ]
    t

(* CSS Backgrounds 3 (ED) sec. 5.7 gives border-image a source, a slice with its
   width and outset, and a repeat. CSS Masking 1 (ED) sec. 8.7 gives mask-border
   those same slots and one more, [|| <'mask-border-mode'>]. Nothing else tells
   the two grammars apart, so [mask_mode] is what says which one the reader is
   holding. CSS Values 4 (ED) sec. 2.2 has [||] ask for one or more of its
   options, so a value that fills no slot matches neither grammar. *)
(* CSS Backgrounds 3 sec. 5.1 gives border-image-source a single [<image>],
   and CSS Masking 1 (ED) sec. 8.2 gives mask-border-source the same, where
   background-image takes a comma-separated list of them. *)
let read_border_image_source t : background_image = read_bg_image t

(* The slice carries its slash-separated width and outset, so the three read as
   one member of the [||] rather than three that could be reordered apart. *)
let read_border_image_slice_group t =
  let slice = read_border_image_slice_offsets t in
  Cursor.ws t;
  if Cursor.slash_opt t then (
    let width =
      Some
        (read_border_image_box_values ~what:"width" read_border_image_width_item
           t)
    in
    Cursor.ws t;
    if Cursor.slash_opt t then
      ( slice,
        width,
        Some
          (read_border_image_box_values ~what:"outset"
             read_border_image_outset_item t) )
    else (slice, width, None))
  else (slice, None, None)

let read_border_image_shorthand ~mask_mode t : border_image =
  let source : background_image option ref = ref Option.None
  and slice : border_image_slice_offsets option ref = ref Option.None
  and width : border_image_width_item list option ref = ref Option.None
  and outset : border_image_outset_item list option ref = ref Option.None
  and repeat : border_image_repeat_keyword list option ref = ref Option.None
  and mode : mask_border_mode option ref = ref Option.None in
  let fill : 'a. filled:bool -> (Cursor.t -> 'a) -> ('a -> unit) -> bool =
   fun ~filled read set ->
    (not filled)
    &&
    match Cursor.option read t with
    | Some value ->
        set value;
        true
    | None -> false
  in
  (* Sec. 6.1 combines the source, the slice group and the repeat with [||], and
     CSS Masking 1 (ED) sec. 8.7 adds [mask-border-mode] to the same group, so
     each fills its slot wherever the author wrote it. *)
  let rec loop () =
    Cursor.ws t;
    let filled =
      fill ~filled:(Option.is_some !source) read_border_image_source (fun v ->
          source := Option.Some v)
      || fill ~filled:(Option.is_some !slice) read_border_image_slice_group
           (fun (s, w, o) ->
             slice := Option.Some s;
             width := w;
             outset := o)
      || fill ~filled:(Option.is_some !repeat) read_border_image_repeat_keywords
           (fun v -> repeat := Option.Some v)
      || mask_mode
         && fill ~filled:(Option.is_some !mode) read_mask_border_mode (fun v ->
             mode := Option.Some v)
    in
    if filled then loop ()
  in
  loop ();
  (match (!source, !slice, !repeat, !mode) with
  | Option.None, Option.None, Option.None, Option.None ->
      Cursor.err_expected t
        (if mask_mode then "mask-border source, slice, repeat, or mode"
         else "border-image source, slice, or repeat")
  | _ -> ());
  {
    source = !source;
    slice = !slice;
    width = !width;
    outset = !outset;
    repeat = !repeat;
    mode = !mode;
  }

let read_border_image t : border_image =
  read_border_image_shorthand ~mask_mode:false t

let read_mask_border t : border_image =
  read_border_image_shorthand ~mask_mode:true t
