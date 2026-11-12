open Declaration
open Common
open Shorthand_common
open Shorthand_coverage
open Shorthand_held

(* CSS Fonts 4 sec. 2.7: [font] reads [<style>? <weight>?
   <size>[/<line-height>]? <family>+]. Cascade stores [font] as a string, so
   composition renders each longhand and stitches them together; default-valued
   components ([normal] style, [400] weight, [normal] line-height) drop on emit.
   Requires font-size and font-family. *)
let is_font_longhand : declaration -> bool = function
  | Declaration { property = Font_style; _ } -> true
  | Declaration { property = Font_weight; _ } -> true
  | Declaration { property = Font_size; _ } -> true
  | Declaration { property = Line_height; _ } -> true
  | Declaration { property = Font_family; _ } -> true
  | _ -> false

(* Each helper returns [Some <typed value>] when the declaration is the relevant
   longhand; [None] otherwise. The pretty-printer drops default components on
   emit, so the composer doesn't normalise here. *)
let font_style_of : declaration -> Properties.font_style option = function
  | Declaration { property = Font_style; value; _ } -> Some value
  | _ -> Option.None

let font_weight_of : declaration -> Properties.font_weight option = function
  | Declaration { property = Font_weight; value; _ } -> Some value
  | _ -> Option.None

let line_height_of : declaration -> Properties.line_height option = function
  | Declaration { property = Line_height; value; _ } -> Some value
  | _ -> Option.None

let font_size_of : declaration -> Properties.font_size option = function
  | Declaration { property = Font_size; value; _ } -> Some value
  | _ -> Option.None

let font_family_of : declaration -> Properties.font_family option = function
  | Declaration { property = Font_family; value; _ } -> Some value
  | _ -> Option.None

(* CSS Fonts 4 (ED) sec. 5.3 splits the [font] sub-properties in two. The seven
   in the Set Explicitly group are the shorthand's own components; the thirteen
   in the Reset Implicitly group have no slot in it and are reset to their
   initials. So a run carrying one of the thirteen composes only when it holds
   that initial, which is what the shorthand writes there anyway.

   The bits below number the twenty in that order. They answer one question,
   whether the run writes every slot the shorthand resets, and are read nowhere
   else. *)
let font_slot_bits d =
  match unwrap_theme_guard d with
  | Declaration { property = Font_style; _ } -> 1 lsl 0
  | Declaration { property = Caps; _ } -> 1 lsl 1
  | Declaration { property = Font_weight; _ } -> 1 lsl 2
  | Declaration { property = Font_stretch; _ } -> 1 lsl 3
  | Declaration { property = Font_size; _ } -> 1 lsl 4
  | Declaration { property = Line_height; _ } -> 1 lsl 5
  | Declaration { property = Font_family; _ } -> 1 lsl 6
  | Declaration { property = Font_feature_settings; _ } -> 1 lsl 7
  | Declaration { property = Font_kerning; _ } -> 1 lsl 8
  | Declaration { property = Font_language_override; _ } -> 1 lsl 9
  | Declaration { property = Font_optical_sizing; _ } -> 1 lsl 10
  | Declaration { property = Font_size_adjust; _ } -> 1 lsl 11
  | Declaration { property = Font_variant_alternates; _ } -> 1 lsl 12
  | Declaration { property = East_asian; _ } -> 1 lsl 13
  | Declaration { property = Font_variant_emoji; _ } -> 1 lsl 14
  | Declaration { property = Font_variant_ligatures; _ } -> 1 lsl 15
  | Declaration { property = Numeric; _ } -> 1 lsl 16
  | Declaration { property = Font_variant_position; _ } -> 1 lsl 17
  | Declaration { property = Font_variation_settings; _ } -> 1 lsl 18
  (* [font-variant] writes seven of the thirteen at once. *)
  | Declaration { property = Font_variant; _ } ->
      (1 lsl 1) lor (1 lsl 12) lor (1 lsl 13) lor (1 lsl 14) lor (1 lsl 15)
      lor (1 lsl 16) lor (1 lsl 17)
  | Declaration { property = Font; _ } -> (1 lsl 19) - 1
  | Declaration { property = All; _ } -> (1 lsl 19) - 1
  | _ -> 0

let all_font_bits = (1 lsl 19) - 1

(* A Reset Implicitly longhand at the initial the shorthand would write there:
   [normal] for the feature and variation settings, the language override and
   the five font-variant longhands that take it, [auto] for the kerning and the
   optical sizing, [none] for the size adjust. *)
let font_reset_only_at_initial d =
  match unwrap_theme_guard d with
  | Declaration { property = Font_feature_settings; value = Normal; _ }
  | Declaration { property = Font_variation_settings; value = Normal; _ }
  | Declaration { property = Font_language_override; value = Normal; _ }
  | Declaration { property = Font_variant_alternates; value = Normal; _ }
  | Declaration { property = East_asian; value = Normal; _ }
  | Declaration { property = Font_variant_emoji; value = Normal; _ }
  | Declaration { property = Font_variant_ligatures; value = Normal; _ }
  | Declaration { property = Numeric; value = Normal; _ }
  | Declaration { property = Font_variant_position; value = Normal; _ }
  | Declaration { property = Font_kerning; value = Auto; _ }
  | Declaration { property = Font_optical_sizing; value = Auto; _ }
  | Declaration { property = Font_size_adjust; value = None; _ } ->
      true
  | _ -> false

(* sec. 5.3 lets the shorthand write only the CSS 2.1 subset of the caps
   longhand, so [normal] and [small-caps] are components and the rest is a value
   the shorthand cannot spell. *)
let font_caps_component : declaration -> Properties.font_variant_css21 option =
  function
  | Declaration { property = Caps; value = Normal; _ } -> Some Normal
  | Declaration { property = Caps; value = Small_caps; _ } -> Some Small_caps
  | _ -> None

(* sec. 5.3 gives [font-width] the initial [normal], which normalises to [100%],
   and an omitted slot names it, so both spellings leave the slot out. The slot
   itself is a [<font-width-css3>] keyword, so a width no keyword names has no
   spelling there and [None] here would reset it instead. *)
let font_stretch_component : declaration -> Properties.font_stretch option =
  function
  | Declaration { property = Font_stretch; value = Normal | Pct 100.; _ } ->
      Option.None
  | Declaration { property = Font_stretch; value; _ } -> Some value
  | _ -> Option.None

let font_stretch_has_slot : declaration -> bool = function
  | Declaration { property = Font_stretch; value = Normal | Pct 100.; _ } ->
      true
  | Declaration { property = Font_stretch; value; _ } ->
      Option.is_some (Properties.font_width_css3 value)
  | _ -> true

let font_stretch_slot : declaration -> bool = function
  | Declaration { property = Font_stretch; _ } -> true
  | _ -> false

let font_run_member d =
  (is_font_longhand d
  || Option.is_some (font_caps_component d)
  || font_stretch_slot d
  || font_reset_only_at_initial d)
  && font_stretch_has_slot d

let font_run_at idx i =
  let n = Rule_index.length idx in
  let rec aux j acc =
    if j >= n || Rule_index.is_absorbed idx j then (List.rev acc, j - i)
    else
      let d = Rule_index.decl_at idx j in
      if font_run_member d then aux (j + 1) (d :: acc) else (List.rev acc, j - i)
  in
  aux i []

let render_font_run decls : Properties.font option =
  let pick f = List.find_map f decls in
  match (pick font_size_of, pick font_family_of) with
  | Some size, Some family ->
      Some
        (Shorthand
           {
             style = pick font_style_of;
             variant = pick font_caps_component;
             weight = pick font_weight_of;
             stretch = pick font_stretch_component;
             size;
             line_height = pick line_height_of;
             family;
           })
  | _ -> Option.None

let try_compose_font_at ~allow_partial idx i =
  let raw_decls, len = font_run_at idx i in
  if len < 2 then None
  else if not (same_importance raw_decls) then None
  else
    let written =
      List.fold_left (fun acc d -> acc lor font_slot_bits d) 0 raw_decls
    in
    let unanswered =
      all_font_bits land lnot written
      land lnot
             (slots_written_after ~slots_of:font_slot_bits idx ~from:(i + len))
    in
    if unanswered <> 0 && not allow_partial then None
    else
      Option.map
        (fun font_value ->
          ( Declaration.v
              ~important:(is_important (List.hd raw_decls))
              Font font_value,
            len ))
        (render_font_run raw_decls)

let compose_font_via_index ~ctx idx =
  let allow_partial = scope ctx = `Stylesheet in
  compose_run_via_index idx ~try_compose:(try_compose_font_at ~allow_partial)

(* The property a declaration writes. A typed value reader that rejects its
   input leaves the declaration as [Unknown_property] under the property's own
   name ([font-kerning:var(--a) var(--b)] reads that way), and it still names
   that property, so the name is read back into its constructor. *)
let named_property decl =
  match unwrap_theme_guard decl with
  | Declaration { property = Unknown_property name; _ } -> property_of_name name
  | Declaration { property; _ } -> Some (Properties.Prop property)
  | _ -> None

(* CSS Fonts 4 sec. 2.7: [font] takes these six subproperties from its own value
   and returns every other one it covers to its initial. Both halves are [Font]
   arms of [covers_longhand]. *)
let is_font_slot_property : type a. a Properties.property -> bool = function
  | Font_style | Font_weight | Font_stretch | Font_size | Line_height
  | Font_family ->
      true
  | _ -> false

let is_font_reset_property : type a. a Properties.property -> bool = function
  | Font_variant_ligatures | Caps | Numeric | Font_variant_position | East_asian
  | Font_variant_emoji | Font_variation_settings | Font_feature_settings
  | Font_size_adjust | Font_kerning | Font_optical_sizing ->
      true
  | _ -> false

(* [font] resets the [font-variant-*] / [font-variation-settings] /
   [font-feature-settings] / [font-size-adjust] / [font-kerning] /
   [font-optical-sizing] subproperties to their initials. When such a reset
   precedes a foldable run of [font] longhands, move it after the run so the
   synthesised [font] does not clobber it (as
   [reorder_border_image_before_border]). *)
let reorder_font_resets_before_font decls =
  let is_font_reset d =
    match named_property (snd d) with
    | Some (Properties.Prop p) -> is_font_reset_property p
    | None -> false
  in
  let is_font_longhand d =
    match named_property (snd d) with
    | Some (Properties.Prop p) -> is_font_slot_property p
    | None -> false
  in
  let rec span pred acc = function
    | d :: rest when pred d -> span pred (d :: acc) rest
    | rest -> (List.rev acc, rest)
  in
  let rec go acc = function
    | [] -> List.rev acc
    | d :: _ as l when is_font_reset d ->
        let reset_block, rest1 = span is_font_reset [] l in
        let long_block, rest2 = span is_font_longhand [] rest1 in
        let has key =
          List.exists
            (fun d ->
              match named_property (snd d) with
              | Some (Properties.Prop p) -> equal_prop_key (Key p) key
              | None -> false)
            long_block
        in
        if has (Key Properties.Font_size) && has (Key Properties.Font_family)
        then
          (* [long_block ++ reset_block] reversed onto acc, tail-recursively and
             without (@) on a large LHS. *)
          go
            (List.rev_append reset_block (List.rev_append long_block acc))
            rest2
        else go (List.rev_append reset_block acc) rest1
    | d :: rest -> go (d :: acc) rest
  in
  go [] decls

(* CSS Lists 3 sec. 3.6: [list-style: <position> <image> <type>] in any order,
   any subset of components. Cascade stores [List_style] as a string. Drop
   defaults ([outside] / [none] / [disc]) on emit; if all three are defaulted,
   leave a single [outside] - never an empty value. *)
let is_list_style_longhand : declaration -> bool = function
  | Declaration { property = List_style_type; _ } -> true
  | Declaration { property = List_style_position; _ } -> true
  | Declaration { property = List_style_image; _ } -> true
  | _ -> false

(* Extract each list-style longhand's typed value, [None] when the declaration
   isn't the relevant longhand; the pretty-printer drops default components on
   emit. *)
let list_style_type_of : declaration -> Properties.list_style_type option =
  function
  | Declaration { property = List_style_type; value; _ } -> Some value
  | _ -> Option.None

let list_style_position_of :
    declaration -> Properties.list_style_position option = function
  | Declaration { property = List_style_position; value; _ } -> Some value
  | _ -> Option.None

let list_style_image_of : declaration -> Properties.list_style_image option =
  function
  | Declaration { property = List_style_image; value; _ } -> Some value
  | _ -> Option.None

let render_list_style decls : Properties.list_style =
  let pick f = List.find_map f decls in
  Shorthand
    {
      type_ = pick list_style_type_of;
      position = pick list_style_position_of;
      image = pick list_style_image_of;
    }

let try_compose_list_style_at idx i =
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
    if
      is_list_style_longhand d1 && is_list_style_longhand d2
      && is_list_style_longhand d3
      && is_important d1 = is_important d2
      && is_important d2 = is_important d3
    then
      Some
        (Declaration.v ~important:(is_important d1) List_style
           (render_list_style [ d1; d2; d3 ]))
    else None

let compose_list_style_via_index idx =
  compose_fixed3_via_index idx ~try_compose:try_compose_list_style_at

(* CSS Text Decoration 4 sec. 2: [text-decoration] shorthand carries line list,
   style, color, and optional thickness. The composition extracts the three
   required typed longhands; the pretty-printer drops default-valued style and
   color when emitting the shorthand. *)
type td_kind = Line | Style | Color | Thickness

let td_kind_of : declaration -> td_kind option = function
  | Declaration { property = Text_decoration_line; _ } -> Some Line
  | Declaration { property = Text_decoration_style; _ } -> Some Style
  | Declaration { property = Text_decoration_color; _ } -> Some Color
  | Declaration { property = Text_decoration_thickness; _ } -> Some Thickness
  | _ -> None

(* CSS Cascade 5 sec. 7.3 gives a longhand written [initial] the property's
   initial value, which is what leaving the component out of the shorthand
   names: [none] for the line (CSS Text Decoration 4 sec. 2.1), [solid] for the
   style (sec. 2.3) and [currentcolor] for the colour (sec. 2.4). The keyword
   itself is no shorthand component, so it folds to the value it stands for and
   the printer drops it with the other initials. *)
let td_line_of : declaration -> Properties.text_decoration_line list option =
  function
  | Declaration { property = Text_decoration_line; value = [ Initial ]; _ } ->
      Some [ (None : Properties.text_decoration_line) ]
  | Declaration { property = Text_decoration_line; value; _ } -> Some value
  | _ -> None

let td_style_of : declaration -> Properties.text_decoration_style option =
  function
  | Declaration { property = Text_decoration_style; value = Initial; _ } ->
      Some (Solid : Properties.text_decoration_style)
  | Declaration { property = Text_decoration_style; value; _ } -> Some value
  | _ -> None

let td_color_of : declaration -> Values.color option = function
  | Declaration { property = Text_decoration_color; value = Initial; _ } ->
      Some Values.current_color
  | Declaration { property = Text_decoration_color; value; _ } -> Some value
  | _ -> None

(* CSS Text Decoration 4 sec. 2.2 makes [auto] the thickness's initial, which is
   what leaving the component out names, so the shorter spelling wins. *)
let td_thickness_of : declaration -> Values.length option = function
  | Declaration { property = Text_decoration_thickness; value = Auto; _ }
  | Declaration { property = Text_decoration_thickness; value = Initial; _ } ->
      Option.None
  | Declaration { property = Text_decoration_thickness; value; _ } -> Some value
  | _ -> None

(* CSS Fonts 4 sec. 2.8.5: [font-synthesis] is [none | [weight || style ||
   small-caps || position]] over its four longhands, each [auto] or [none]. The
   shorthand names the ones set to [auto]; naming none of them is [none]. It
   resets the position longhand too, so a rule that writes one is left alone
   rather than have the synthesised shorthand clobber it. *)
type synthesis_part = Weight | Style | Small_caps

let synthesis_part_of : declaration -> (synthesis_part * bool) option = function
  | Declaration { property = Font_synthesis_weight; value = Auto; _ } ->
      Some (Weight, true)
  | Declaration { property = Font_synthesis_weight; value = None; _ } ->
      Some (Weight, false)
  | Declaration { property = Font_synthesis_style; value = Auto; _ } ->
      Some (Style, true)
  | Declaration { property = Font_synthesis_style; value = None; _ } ->
      Some (Style, false)
  | Declaration { property = Font_synthesis_small_caps; value = Auto; _ } ->
      Some (Small_caps, true)
  | Declaration { property = Font_synthesis_small_caps; value = None; _ } ->
      Some (Small_caps, false)
  | _ -> None

let font_synthesis_of_run parts : Properties.font_synthesis option =
  let slot = function Weight -> 0 | Style -> 1 | Small_caps -> 2 in
  let keys = List.sort_uniq compare (List.map (fun (p, _) -> slot p) parts) in
  if List.length keys <> 3 then Option.None
  else
    let feature : synthesis_part -> Properties.font_synthesis_feature = function
      | Weight -> Weight
      | Style -> Style
      | Small_caps -> Small_caps
    in
    let on =
      List.filter_map
        (fun (p, auto) -> if auto then Some (feature p) else Option.None)
        (List.sort (fun (a, _) (b, _) -> compare (slot a) (slot b)) parts)
    in
    Some (if on = [] then None else Features on)

let try_compose_font_synthesis_at idx i =
  let n = Rule_index.length idx in
  if i + 2 >= n then None
  else
    let positions = [ i; i + 1; i + 2 ] in
    if List.exists (Rule_index.is_absorbed idx) positions then None
    else
      let raw = List.map (Rule_index.decl_at idx) positions in
      if not (same_importance raw) then None
      else
        let parts = List.filter_map synthesis_part_of raw in
        if List.length parts <> 3 then None
        else
          Option.map
            (Declaration.v
               ~important:(is_important (List.hd raw))
               Font_synthesis)
            (font_synthesis_of_run parts)

let writes_synthesis_position = function
  | Declaration { property = Font_synthesis_position; _ } -> true
  | _ -> false

let compose_font_synthesis_via_index idx =
  let n = Rule_index.length idx in
  let rec has_position i =
    i < n
    && (writes_synthesis_position (Rule_index.decl_at idx i)
       || has_position (i + 1))
  in
  if not (has_position 0) then
    compose_fixed3_via_index idx ~try_compose:try_compose_font_synthesis_at

(* [text-decoration] resets the thickness slot, so a run that leaves the
   thickness out contracts into a declaration saying [auto] where the run said
   nothing. Composing the three is safe only when nothing else in reach holds
   that slot, the reading [transition] and [animation] already take. *)
let text_decoration_of_run raw k : Properties.text_decoration option =
  let kinds = List.map td_kind_of raw in
  if
    not
      (List.for_all Option.is_some kinds
      && List.length (List.sort_uniq compare kinds) = k)
  then Option.None
  else
    let lines = List.find_map td_line_of raw in
    let style = List.find_map td_style_of raw in
    let color = List.find_map td_color_of raw in
    let thickness = List.find_map td_thickness_of raw in
    match (lines, style, color) with
    | Some lines, Some _, Some _ ->
        Some (Shorthand { lines; style; color; thickness })
    | _ -> Option.None

let try_compose_text_decoration_run ~held idx i k =
  let n = Rule_index.length idx in
  if i + k > n then None
  else
    let positions = List.init k (fun j -> i + j) in
    if List.exists (Rule_index.is_absorbed idx) positions then None
    else
      let raw = List.map (Rule_index.decl_at idx) positions in
      if not (same_importance raw) then None
      else
        let important = is_important (List.hd raw) in
        (* [k] is 4 today, so [missing] is empty and the scan answers no; the
           slot is threaded so a shorter run can be judged rather than
           assumed. *)
        let missing = if k = 4 then 0 else td_thickness_bit in
        if td_reset_hazard idx i ~held ~missing ~important then None
        else
          Option.map
            (fun sh -> (Declaration.v ~important Text_decoration sh, k))
            (text_decoration_of_run raw k)

let try_compose_text_decoration_at ~held idx i =
  try_compose_text_decoration_run ~held idx i 4

let compose_text_decoration_via_index ~held idx =
  compose_run_via_index idx ~try_compose:(try_compose_text_decoration_at ~held)

(* CSS Fonts 4 (ED) sec. 6.10: [font-variant] writes its seven longhands, so the
   run naming the same declaration is all seven. A longhand at [normal] is the
   slot the shorthand leaves out; a CSS-wide keyword or a [var()] is the whole
   declaration value and no shorthand component. *)
(* [`None] is the [font-variant: none] form, which the ligatures longhand alone
   can ask for and which carries no other component. *)
let fv_ligatures_of : declaration -> [ `Slot of _ | `None ] option = function
  | Declaration { property = Font_variant_ligatures; value = Normal; _ } ->
      Some (`Slot [])
  | Declaration { property = Font_variant_ligatures; value = None; _ } ->
      Some `None
  | Declaration { property = Font_variant_ligatures; value = Ligatures l; _ } ->
      Some (`Slot l)
  | _ -> None

let fv_alternates_of : declaration -> _ list option = function
  | Declaration { property = Font_variant_alternates; value = Normal; _ } ->
      Some []
  | Declaration { property = Font_variant_alternates; value = Alternates l; _ }
    ->
      Some l
  | _ -> None

let fv_caps_of : declaration -> Properties.font_variant_caps option option =
  function
  | Declaration { property = Caps; value = Normal; _ } -> Some Option.None
  | Declaration
      {
        property = Caps;
        value =
          ( Small_caps | All_small_caps | Petite_caps | All_petite_caps
          | Unicase | Titling_caps ) as caps;
        _;
      } ->
      Some (Some caps)
  | _ -> None

let fv_numeric_of : declaration -> _ list option = function
  | Declaration { property = Numeric; value = Normal; _ } -> Some []
  | Declaration { property = Numeric; value = Tokens tokens; _ } -> Some tokens
  | Declaration
      {
        property = Numeric;
        value =
          Composed
            {
              ordinal;
              slashed_zero;
              numeric_figure;
              numeric_spacing;
              numeric_fraction;
            };
        _;
      } ->
      Some
        (List.filter_map Fun.id
           [
             numeric_figure;
             numeric_spacing;
             numeric_fraction;
             ordinal;
             slashed_zero;
           ])
  | _ -> None

let fv_east_asian_of : declaration -> _ list option = function
  | Declaration { property = East_asian; value = Normal; _ } -> Some []
  | Declaration { property = East_asian; value = Features f; _ } -> Some f
  | _ -> None

let fv_position_of :
    declaration -> Properties.font_variant_position option option = function
  | Declaration { property = Font_variant_position; value = Normal; _ } ->
      Some Option.None
  | Declaration
      { property = Font_variant_position; value = (Sub | Super) as p; _ } ->
      Some (Some p)
  | _ -> None

let fv_emoji_of : declaration -> Properties.font_variant_emoji option option =
  function
  | Declaration { property = Font_variant_emoji; value = Normal; _ } ->
      Some Option.None
  | Declaration
      {
        property = Font_variant_emoji;
        value = (Text | Emoji | Unicode) as e;
        _;
      } ->
      Some (Some e)
  | _ -> None

let font_variant_of_run raw : Properties.font_variant option =
  let ligatures = List.find_map fv_ligatures_of raw in
  let alternates = List.find_map fv_alternates_of raw in
  let caps = List.find_map fv_caps_of raw in
  let numeric = List.find_map fv_numeric_of raw in
  let east_asian = List.find_map fv_east_asian_of raw in
  let position = List.find_map fv_position_of raw in
  let emoji = List.find_map fv_emoji_of raw in
  match (ligatures, alternates, caps, numeric, east_asian, position, emoji) with
  | ( Some ligatures,
      Some alternates,
      Some caps,
      Some numeric,
      Some east_asian,
      Some position,
      Some emoji ) -> (
      let others_empty =
        alternates = [] && Option.is_none caps && numeric = []
        && east_asian = [] && Option.is_none position && Option.is_none emoji
      in
      match ligatures with
      | `None when others_empty -> Some (None : Properties.font_variant)
      | `None -> Option.None
      | `Slot ligatures ->
          if ligatures = [] && others_empty then Some Normal
          else
            Some
              (Shorthand
                 {
                   ligatures;
                   alternates;
                   caps;
                   numeric;
                   east_asian;
                   position;
                   emoji;
                 }))
  | _ -> Option.None

let try_compose_font_variant_at idx i =
  let n = Rule_index.length idx in
  if i + 6 >= n then None
  else
    let positions = List.init 7 (fun j -> i + j) in
    if List.exists (Rule_index.is_absorbed idx) positions then None
    else
      let raw = List.map (Rule_index.decl_at idx) positions in
      (* Each slot reader answers for one longhand, so a run missing one or
         naming one twice leaves a slot empty and does not compose. *)
      if not (same_importance raw) then None
      else
        Option.map
          (Declaration.v ~important:(is_important (List.hd raw)) Font_variant)
          (font_variant_of_run raw)

let compose_font_variant_via_index idx =
  compose_run_via_index idx ~try_compose:(fun idx i ->
      Option.map (fun d -> (d, 7)) (try_compose_font_variant_at idx i))
