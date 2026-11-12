open Declaration
open Common
open Shorthand_common
open Shorthand_coverage

let display_value_is_vendor : Properties.display -> bool = function
  | Webkit_flex | Webkit_inline_flex | Ms_flexbox | Webkit_box | Moz_box
  | Moz_inline_box ->
      true
  | _ -> false

let background_image_is_vendor : Properties.background_image -> bool = function
  | Webkit_linear_gradient _ | Webkit_repeating_linear_gradient _
  | Webkit_radial_gradient _ | Webkit_repeating_radial_gradient _
  | Moz_linear_gradient _ | Moz_repeating_linear_gradient _
  | Moz_radial_gradient _ | Moz_repeating_radial_gradient _
  | O_linear_gradient _ | O_repeating_linear_gradient _ | O_radial_gradient _
  | O_repeating_radial_gradient _ | Webkit_image_set _ | Webkit_gradient _ ->
      true
  | _ -> false

(* A shorthand carries the same image the longhand does, so the prefixed
   spelling is the same fallback whichever property writes it. [background] and
   [mask] carry one image per layer; [border-image] and [mask-border] carry a
   single source. *)
let background_layer_is_vendor (layer : Properties.background) =
  match layer with
  | Shorthand { image = Some image; _ } -> background_image_is_vendor image
  | _ -> false

let mask_layer_is_vendor (layer : Properties.mask_layer) =
  match layer.image with
  | Some image -> background_image_is_vendor image
  | _ -> false

let mask_value_is_vendor (value : Properties.mask) =
  match value with
  | Layer layer -> mask_layer_is_vendor layer
  | Layers layers -> List.exists mask_layer_is_vendor layers
  | _ -> false

let border_image_source_is_vendor (value : Properties.border_image) =
  match value.source with
  | Some image -> background_image_is_vendor image
  | _ -> false

(* A value whose rendering begins with a vendor prefix (-webkit-, -moz-, -ms-,
   -o-). Preserves legacy fallbacks like [display:-webkit-box;display:flex]: old
   browsers understand only the prefixed spelling, so dropping the earlier
   declaration removes a real compat fallback. *)
(* All the intrinsic sizing properties carry a [length_percentage] value. A GADT
   or-pattern does not refine the value type, so each constructor is matched on
   its own; everything else is not a sizing fallback. *)
let sizing_value_is_vendor_prefixed : type a. a Properties.property -> a -> bool
    =
 fun property value ->
  let pfx = Values.length_percentage_is_vendor_prefixed in
  match property with
  | Width -> pfx value
  | Height -> pfx value
  | Min_width -> pfx value
  | Min_height -> pfx value
  | Max_width -> pfx value
  | Max_height -> pfx value
  | Block_size -> pfx value
  | Inline_size -> pfx value
  | Min_block_size -> pfx value
  | Min_inline_size -> pfx value
  | Max_block_size -> pfx value
  | Max_inline_size -> pfx value
  | _ -> false

let rec value_is_vendor_prefixed decl =
  match decl with
  | Theme_guarded { decl; _ } -> value_is_vendor_prefixed decl
  | Declaration { property = Display; value; _ } ->
      display_value_is_vendor value
  | Declaration { property = Background_image; value; _ } ->
      List.exists background_image_is_vendor value
  | Declaration { property = Webkit_mask_image; value; _ } ->
      background_image_is_vendor value
  | Declaration { property = Mask_image; value; _ } ->
      background_image_is_vendor value
  | Declaration { property = Border_image_source; value; _ } ->
      background_image_is_vendor value
  | Declaration { property = Background; value; _ } ->
      List.exists background_layer_is_vendor value
  | Declaration { property = Mask; value; _ } -> mask_value_is_vendor value
  | Declaration { property = Border_image; value; _ } ->
      border_image_source_is_vendor value
  | Declaration { property = Mask_border; value; _ } ->
      border_image_source_is_vendor value
  (* [position:-webkit-sticky;position:sticky] and the [text-align] equivalent
     are the same browser-compat pattern: old Safari only understands the
     prefixed keyword, so the earlier declaration is a real fallback. *)
  | Declaration { property = Position; value = Webkit_sticky; _ } -> true
  | Declaration { property = Text_align; value = Webkit_match_parent; _ } ->
      true
  (* [width:-webkit-max-content;width:max-content] and the other intrinsic
     sizing properties: the prefixed keyword is a fallback for old Safari /
     Firefox, so the earlier declaration must survive minify dedup. *)
  | Declaration { property; value; _ } ->
      sizing_value_is_vendor_prefixed property value

let property_covered_by_important kept decl =
  List.exists
    (fun (_, existing) ->
      (not (is_intentionally_duplicated existing))
      && is_important existing
      && declaration_covers existing decl)
    kept

let same_property = Declaration.same_property

(* Rebuilt through the smart constructors: they recompute the cached
   [Declaration.hash] over the new payload, and a record update would leave it
   fingerprinting the importance that was just cleared. *)
let rec without_importance = function
  | Declaration { property; value; _ } ->
      Declaration.v ~important:false property value
  | Theme_guarded { var_name; decl; _ } ->
      Declaration.theme_guarded ~var_name (without_importance decl)

(* Value equality ignoring importance. Every caller establishes [same_property]
   first, so the two declarations share a value type and this is a structural
   value comparison: on canonical ASTs it matches minified-text equality without
   rendering. *)
let same_value a b =
  Declaration.equal_declaration (without_importance a) (without_importance b)

(* Only a pair that writes a common cascade slot at the same importance with
   different values constrains its own order: [!important] beats the plain
   declaration wherever the two sit, and an identical pair is its own winner
   either way. The footprint is computed once per declaration rather than once
   per pair, since the test below is quadratic. *)
type commute_fact = {
  decl : declaration;
  important : bool;
  keys : overlap_key list;
}

let commute_fact decl =
  {
    decl;
    important = Declaration.is_important decl;
    keys = declaration_overlap_keys decl;
  }

let commute_facts_conflict a b =
  a.important = b.important
  && (not (Declaration.same_minified a.decl b.decl))
  && declarations_overlap_with_keys a.decl a.keys b.decl b.keys

let declarations_commute left right =
  let left = List.map commute_fact left in
  let right = List.map commute_fact right in
  not
    (List.exists
       (fun a -> List.exists (fun b -> commute_facts_conflict a b) right)
       left)

let legacy_vendor_fallback new_decl existing =
  (* Different-value duplicates are kept when one value is vendor-prefixed: the
     cascade may pick whichever the browser understands. *)
  same_property new_decl existing
  && (not (same_value new_decl existing))
  && (value_is_vendor_prefixed existing || value_is_vendor_prefixed new_decl)

(* The earlier declaration is a real cascade fallback when the later one uses
   CSS Color 4 / 5 syntax that older browsers drop. With a target contract the
   browsers are named, and the fallback is one only where a target does not
   parse the later colour; every target parsing it makes the earlier declaration
   dead at cascade time. *)
let legacy_color_fallback ?targets new_decl existing =
  same_property new_decl existing
  && (not (same_value new_decl existing))
  && (match targets with
    | Some targets ->
        Declaration.value_uses_color_unimplemented_by targets new_decl
    | None -> Declaration.value_uses_color_4 new_decl)
  && not (Declaration.value_uses_color_4 existing)

(* Same shape: the later value uses a runtime substitution ([var()] / [env()] /
   [attr()]) and the earlier doesn't, so the earlier is a static fallback for
   browsers that can't resolve the substitution at parse time. Every target
   resolves one, and a declaration holding a substitution is never dropped at
   parse time, so under a target contract the later one always wins the cascade
   and the earlier is dead. *)
let legacy_runtime_subst_fallback ?targets new_decl existing =
  Option.is_none targets
  && same_property new_decl existing
  && (not (same_value new_decl existing))
  && Declaration.value_uses_runtime_subst new_decl
  && not (Declaration.value_uses_runtime_subst existing)

let same_property_value_declaration new_decl existing =
  same_property new_decl existing
  && same_value new_decl existing
  && (is_important new_decl || not (is_important existing))

let covered_by_new_declaration ?targets new_decl existing =
  (not (is_intentionally_duplicated existing))
  && declaration_covers new_decl existing
  && (is_important new_decl || not (is_important existing))
  && (not (legacy_vendor_fallback new_decl existing))
  && (not (legacy_color_fallback ?targets new_decl existing))
  && not (legacy_runtime_subst_fallback ?targets new_decl existing)

let filter_preserve = List.filter_preserve

let add_all_declaration_rev idx decl kept =
  let after, before =
    List.partition (fun (_, old) -> all_preserved_reorder_declaration old) kept
  in
  after @ ((idx, decl) :: before)

let is_all_declaration = function
  | Declaration { property = All; _ } -> true
  | Theme_guarded { decl; _ } -> (
      match unwrap_theme_guard decl with
      | Declaration { property = All; _ } -> true
      | _ -> false)
  | _ -> false

let deduplicate_step ?targets kept (idx, decl) =
  if is_intentionally_duplicated decl then
    let kept =
      filter_preserve
        (fun (_, old) -> not (same_property_value_declaration decl old))
        kept
    in
    (idx, decl) :: kept
  else if (not (is_important decl)) && property_covered_by_important kept decl
  then kept
  else
    let kept =
      filter_preserve
        (fun (_, old) -> not (covered_by_new_declaration ?targets decl old))
        kept
    in
    if is_all_declaration decl then add_all_declaration_rev idx decl kept
    else (idx, decl) :: kept

(* [vendor_alias_twin vendor twin] is [true] when [vendor] is the
   vendor-prefixed spelling of its unprefixed [twin], carrying the same value
   and importance. Each pair is matched on both property constructors so the two
   values share a type and compare with a typed (=) - no rendering. This
   relation records only which constructors alias each other; whether the prefix
   is then dead is a Baseline question, answered in [drop_vendor_aliases].

   Absent on purpose:
   - [-webkit-appearance]: its value type [webkit_appearance] is a superset of
     [appearance] (extra non-standard values like [listbox]/[checkbox]/[radio]),
     so there is no typed equality between the two.
   - [-webkit-text-decoration]: dropping the WebKit copy regresses documented
     inheritance quirks.
   - [-webkit-background-clip]: its prefix is value-dependent, not
     property-dependent. web-features tracks [background-clip-text] as its own
     feature, so [background-clip] reads widely available at property
     granularity while [-webkit-background-clip: text] is still the spelling
     WebKit honours. Baseline cannot see that at property granularity, so the
     pair stays out of the relation. *)
(* WebKit animation longhand vendor-alias pairs ([-webkit-] vs unprefixed). *)
let vendor_alias_twin_webkit_animation vendor twin =
  match (vendor, twin) with
  | ( Declaration { property = Webkit_animation; value = v1; important = i1; _ },
      Declaration { property = Animation; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_animation_delay; value = v1; important = i1; _ },
      Declaration { property = Animation_delay; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_animation_duration; value = v1; important = i1; _ },
      Declaration
        { property = Animation_duration; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_animation_direction; value = v1; important = i1; _ },
      Declaration
        { property = Animation_direction; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        {
          property = Webkit_animation_iteration_count;
          value = v1;
          important = i1;
          _;
        },
      Declaration
        { property = Animation_iteration_count; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_animation_name; value = v1; important = i1; _ },
      Declaration { property = Animation_name; value = v2; important = i2; _ } )
    ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        {
          property = Webkit_animation_timing_function;
          value = v1;
          important = i1;
          _;
        },
      Declaration
        { property = Animation_timing_function; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_animation_fill_mode; value = v1; important = i1; _ },
      Declaration
        { property = Animation_fill_mode; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        {
          property = Webkit_animation_play_state;
          value = v1;
          important = i1;
          _;
        },
      Declaration
        { property = Animation_play_state; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | _ -> false

(* Mozilla animation longhand vendor-alias pairs ([-moz-] vs unprefixed). *)
let vendor_alias_twin_moz_animation vendor twin =
  match (vendor, twin) with
  | ( Declaration { property = Moz_animation; value = v1; important = i1; _ },
      Declaration { property = Animation; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Moz_animation_delay; value = v1; important = i1; _ },
      Declaration { property = Animation_delay; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Moz_animation_duration; value = v1; important = i1; _ },
      Declaration
        { property = Animation_duration; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Moz_animation_direction; value = v1; important = i1; _ },
      Declaration
        { property = Animation_direction; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        {
          property = Moz_animation_iteration_count;
          value = v1;
          important = i1;
          _;
        },
      Declaration
        { property = Animation_iteration_count; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Moz_animation_name; value = v1; important = i1; _ },
      Declaration { property = Animation_name; value = v2; important = i2; _ } )
    ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        {
          property = Moz_animation_timing_function;
          value = v1;
          important = i1;
          _;
        },
      Declaration
        { property = Animation_timing_function; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Moz_animation_fill_mode; value = v1; important = i1; _ },
      Declaration
        { property = Animation_fill_mode; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Moz_animation_play_state; value = v1; important = i1; _ },
      Declaration
        { property = Animation_play_state; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | _ -> false

let vendor_alias_twin_animation vendor twin =
  vendor_alias_twin_webkit_animation vendor twin
  || vendor_alias_twin_moz_animation vendor twin

(* Transition longhand vendor-alias pairs ([-webkit-]/[-moz-]/[-o-] vs
   unprefixed). *)
let vendor_alias_twin_transition vendor twin =
  match (vendor, twin) with
  | ( Declaration { property = Webkit_transition; value = v1; important = i1; _ },
      Declaration { property = Transition; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_transition_delay; value = v1; important = i1; _ },
      Declaration { property = Transition_delay; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_transition_duration; value = v1; important = i1; _ },
      Declaration
        { property = Transition_duration; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_transition_property; value = v1; important = i1; _ },
      Declaration
        { property = Transition_property; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        {
          property = Webkit_transition_timing_function;
          value = v1;
          important = i1;
          _;
        },
      Declaration
        { property = Transition_timing_function; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Moz_transition; value = v1; important = i1; _ },
      Declaration { property = Transition; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Moz_transition_delay; value = v1; important = i1; _ },
      Declaration { property = Transition_delay; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Moz_transition_duration; value = v1; important = i1; _ },
      Declaration
        { property = Transition_duration; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Moz_transition_property; value = v1; important = i1; _ },
      Declaration
        { property = Transition_property; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        {
          property = Moz_transition_timing_function;
          value = v1;
          important = i1;
          _;
        },
      Declaration
        { property = Transition_timing_function; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = O_transition; value = v1; important = i1; _ },
      Declaration { property = Transition; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | _ -> false

(* Modern flexbox alignment vendor-alias pairs (-webkit- vs unprefixed). *)
let vendor_alias_twin_flex vendor twin =
  match (vendor, twin) with
  | ( Declaration
        { property = Webkit_flex_direction; value = v1; important = i1; _ },
      Declaration { property = Flex_direction; value = v2; important = i2; _ } )
    ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Webkit_flex_wrap; value = v1; important = i1; _ },
      Declaration { property = Flex_wrap; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Webkit_flex_flow; value = v1; important = i1; _ },
      Declaration { property = Flex_flow; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_justify_content; value = v1; important = i1; _ },
      Declaration { property = Justify_content; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_align_items; value = v1; important = i1; _ },
      Declaration { property = Align_items; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_align_content; value = v1; important = i1; _ },
      Declaration { property = Align_content; value = v2; important = i2; _ } )
    ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Webkit_align_self; value = v1; important = i1; _ },
      Declaration { property = Align_self; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | _ -> false

(* Border-radius / box-shadow / background-size / filter vendor-alias pairs. *)
let vendor_alias_twin_visual vendor twin =
  match (vendor, twin) with
  | ( Declaration
        { property = Webkit_border_radius; value = v1; important = i1; _ },
      Declaration { property = Border_radius; value = v2; important = i2; _ } )
    ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Moz_border_radius; value = v1; important = i1; _ },
      Declaration { property = Border_radius; value = v2; important = i2; _ } )
    ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Webkit_box_shadow; value = v1; important = i1; _ },
      Declaration { property = Box_shadow; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Moz_box_shadow; value = v1; important = i1; _ },
      Declaration { property = Box_shadow; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_background_size; value = v1; important = i1; _ },
      Declaration { property = Background_size; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Webkit_filter; value = v1; important = i1; _ },
      Declaration { property = Filter; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_backdrop_filter; value = v1; important = i1; _ },
      Declaration { property = Backdrop_filter; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | _ -> false

let decoration_color_alias_twin vendor twin =
  match (vendor, twin) with
  | ( Declaration
        {
          property = Webkit_text_decoration_color;
          value = v1;
          important = i1;
          _;
        },
      Declaration
        { property = Text_decoration_color; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | _ -> false

(* Two declarations spell one value when their value text is the same. A
   structural [=] on the two answers a different question: a [var()] fallback
   the slot cannot type keeps the component values it was written as, source
   spans included, so a prefix cascade synthesised beside its twin shares that
   node and compares equal where an authored pair sat at two offsets and does
   not. The drop below then depended on where the pair came from, and a
   [mask-image: var(--x, 10px)] settled one way when synthesised and another
   when read back. *)
let same_value_text vendor twin =
  String.equal
    (Declaration.string_of_value ~minify:true vendor)
    (Declaration.string_of_value ~minify:true twin)

let vendor_alias_twin vendor twin =
  match (vendor, twin) with
  | ( Declaration { property = Webkit_transform; important = i1; _ },
      Declaration { property = Transform; important = i2; _ } ) ->
      same_value_text vendor twin && Bool.equal i1 i2
  | ( Declaration { property = Webkit_box_sizing; important = i1; _ },
      Declaration { property = Box_sizing; important = i2; _ } ) ->
      same_value_text vendor twin && Bool.equal i1 i2
  | ( Declaration { property = Moz_box_sizing; important = i1; _ },
      Declaration { property = Box_sizing; important = i2; _ } ) ->
      same_value_text vendor twin && Bool.equal i1 i2
  | ( Declaration { property = Webkit_text_decoration_color; _ },
      Declaration { property = Text_decoration_color; _ } ) ->
      decoration_color_alias_twin vendor twin
  | ( Declaration { property = Webkit_mask_image; important = i1; _ },
      Declaration { property = Mask_image; important = i2; _ } ) ->
      same_value_text vendor twin && Bool.equal i1 i2
  | ( Declaration { property = Webkit_user_select; important = i1; _ },
      Declaration { property = User_select; important = i2; _ } ) ->
      same_value_text vendor twin && Bool.equal i1 i2
  | ( Declaration { property = Moz_user_select; important = i1; _ },
      Declaration { property = User_select; important = i2; _ } ) ->
      same_value_text vendor twin && Bool.equal i1 i2
  | ( Declaration { property = Webkit_hyphens; important = i1; _ },
      Declaration { property = Hyphens; important = i2; _ } ) ->
      same_value_text vendor twin && Bool.equal i1 i2
  | ( Declaration { property = Webkit_text_size_adjust; important = i1; _ },
      Declaration { property = Text_size_adjust; important = i2; _ } ) ->
      same_value_text vendor twin && Bool.equal i1 i2
  | ( Declaration { property = Webkit_print_color_adjust; important = i1; _ },
      Declaration { property = Print_color_adjust; important = i2; _ } ) ->
      same_value_text vendor twin && Bool.equal i1 i2
  | _ ->
      vendor_alias_twin_animation vendor twin
      || vendor_alias_twin_transition vendor twin
      || vendor_alias_twin_flex vendor twin
      || vendor_alias_twin_visual vendor twin

(* WHATWG Compatibility Standard sec. 3.4.1: these [-webkit-] properties "must
   be supported as legacy name aliases of the corresponding unprefixed
   property", and CSS Cascade 5 sec. 2.3 makes a legacy name alias the SAME
   property under a second name rather than a property of its own. A prefix the
   list does not name is a private extension in the vendor's namespace, so it is
   its own property and never an alias -- [-moz-] spellings and
   [-webkit-user-select] among them. *)
let legacy_name_alias_of = function
  | "-webkit-align-content" -> Some "align-content"
  | "-webkit-align-items" -> Some "align-items"
  | "-webkit-align-self" -> Some "align-self"
  | "-webkit-animation" -> Some "animation"
  | "-webkit-animation-delay" -> Some "animation-delay"
  | "-webkit-animation-direction" -> Some "animation-direction"
  | "-webkit-animation-duration" -> Some "animation-duration"
  | "-webkit-animation-fill-mode" -> Some "animation-fill-mode"
  | "-webkit-animation-iteration-count" -> Some "animation-iteration-count"
  | "-webkit-animation-name" -> Some "animation-name"
  | "-webkit-animation-play-state" -> Some "animation-play-state"
  | "-webkit-animation-timing-function" -> Some "animation-timing-function"
  | "-webkit-backface-visibility" -> Some "backface-visibility"
  | "-webkit-background-clip" -> Some "background-clip"
  | "-webkit-background-origin" -> Some "background-origin"
  | "-webkit-background-size" -> Some "background-size"
  | "-webkit-border-bottom-left-radius" -> Some "border-bottom-left-radius"
  | "-webkit-border-bottom-right-radius" -> Some "border-bottom-right-radius"
  | "-webkit-border-radius" -> Some "border-radius"
  | "-webkit-border-top-left-radius" -> Some "border-top-left-radius"
  | "-webkit-border-top-right-radius" -> Some "border-top-right-radius"
  | "-webkit-box-shadow" -> Some "box-shadow"
  | "-webkit-box-sizing" -> Some "box-sizing"
  | "-webkit-filter" -> Some "filter"
  | "-webkit-flex" -> Some "flex"
  | "-webkit-flex-basis" -> Some "flex-basis"
  | "-webkit-flex-direction" -> Some "flex-direction"
  | "-webkit-flex-flow" -> Some "flex-flow"
  | "-webkit-flex-grow" -> Some "flex-grow"
  | "-webkit-flex-shrink" -> Some "flex-shrink"
  | "-webkit-flex-wrap" -> Some "flex-wrap"
  | "-webkit-justify-content" -> Some "justify-content"
  | "-webkit-mask" -> Some "mask"
  | "-webkit-mask-box-image" -> Some "mask-border"
  | "-webkit-mask-box-image-outset" -> Some "mask-border-outset"
  | "-webkit-mask-box-image-repeat" -> Some "mask-border-repeat"
  | "-webkit-mask-box-image-slice" -> Some "mask-border-slice"
  | "-webkit-mask-box-image-source" -> Some "mask-border-source"
  | "-webkit-mask-box-image-width" -> Some "mask-border-width"
  | "-webkit-mask-clip" -> Some "mask-clip"
  | "-webkit-mask-composite" -> Some "mask-composite"
  | "-webkit-mask-image" -> Some "mask-image"
  | "-webkit-mask-origin" -> Some "mask-origin"
  | "-webkit-mask-position" -> Some "mask-position"
  | "-webkit-mask-repeat" -> Some "mask-repeat"
  | "-webkit-mask-size" -> Some "mask-size"
  | "-webkit-order" -> Some "order"
  | "-webkit-perspective" -> Some "perspective"
  | "-webkit-perspective-origin" -> Some "perspective-origin"
  | "-webkit-transform" -> Some "transform"
  | "-webkit-transform-origin" -> Some "transform-origin"
  | "-webkit-transform-style" -> Some "transform-style"
  | "-webkit-transition" -> Some "transition"
  | "-webkit-transition-delay" -> Some "transition-delay"
  | "-webkit-transition-duration" -> Some "transition-duration"
  | "-webkit-transition-property" -> Some "transition-property"
  | "-webkit-transition-timing-function" -> Some "transition-timing-function"
  | _ -> None

(* One property written twice with one value says what writing it once says, so
   the alias and its twin compare as the twin alone. This assumes nothing about
   which browsers are targeted -- the two names ARE one property -- so it holds
   where the target-dependent prefix drops do not. A differing value or
   importance is two writes of one property and stays. *)
let legacy_name_alias_twin vendor twin =
  match legacy_name_alias_of (Declaration.property_name vendor) with
  | None -> false
  | Some unprefixed ->
      String.equal unprefixed (Declaration.property_name twin)
      && same_value_text vendor twin
      && Bool.equal
           (Declaration.is_important vendor)
           (Declaration.is_important twin)

(* Canonical comparison follows the aliasing the specs prove on their own,
   without enabling every target-dependent optimizer rewrite. Sec. 3.4.1 above
   settles the [-webkit-] list; [-webkit-text-decoration-color] is not on it but
   every engine that ships the prefix aliases it, and Cascade normalizes it
   whatever the target. A differing fallback, mismatched importance, or prefix
   without a standard twin remains observable and is kept. *)
let drop_redundant_decoration_color_aliases declarations =
  filter_preserve
    (fun declaration ->
      not
        (List.exists
           (fun twin ->
             decoration_color_alias_twin declaration twin
             || legacy_name_alias_twin declaration twin)
           declarations))
    declarations

(* If [name] starts with a CSS vendor prefix ([-webkit-] / [-moz-] / [-ms-] /
   [-o-]) return the unprefixed remainder; otherwise [None]. *)
let strip_vendor_prefix name =
  let n = String.length name in
  let try_prefix p =
    let pn = String.length p in
    if n > pn && String.sub name 0 pn = p then
      Some (String.sub name pn (n - pn))
    else None
  in
  match try_prefix "-webkit-" with
  | Some _ as r -> r
  | None -> (
      match try_prefix "-moz-" with
      | Some _ as r -> r
      | None -> (
          match try_prefix "-ms-" with
          | Some _ as r -> r
          | None -> try_prefix "-o-"))

(* Structural vendor-alias drop for [Unknown_property] pairs. Both [vendor] and
   [twin] are [Unknown_property] declarations; we drop [vendor] when stripping
   its vendor prefix yields [twin]'s name and the component-value lists are
   structurally equal under matching importance. The typed-twin case (e.g.
   vendor [Unknown_property "-webkit-animation-delay"] + modern
   [Animation_delay]) needs typed vendor longhand properties to compare
   structurally; that remains a follow-up. *)
let text_vendor_alias_twin vendor twin =
  match (vendor, twin) with
  | ( Declaration
        { property = Unknown_property vname; value = vv; important = vi; _ },
      Declaration
        { property = Unknown_property tname; value = tv; important = ti; _ } )
    when Bool.equal vi ti -> (
      match strip_vendor_prefix vname with
      | Some modern -> String.equal tname modern && vv = tv
      | None -> false)
  | _ -> false

(* {!Baseline.greenfield_properties} is generated from web-features as names.
   Resolve each once into the property it spells, so the test below compares
   property tags instead of rendering one. A name the reader does not type keeps
   its [Unknown_property] tag, which is the shape it reaches
   [drop_vendor_aliases] in as a [text_vendor_alias_twin] twin. *)
let greenfield_keys =
  let tbl = Hashtbl.create 512 in
  let add key = Hashtbl.replace tbl key () in
  List.iter
    (fun name ->
      add (Key (Properties.Unknown_property name));
      match property_of_name name with
      | Some (Properties.Prop property) -> add (Key property)
      | None -> ())
    Baseline.greenfield_properties;
  tbl

(* An unprefixed property supersedes its prefix only once every maintained
   browser reads it. {!Baseline.greenfield_properties} lists the properties that
   are not yet Baseline "widely available", which is exactly the set whose
   prefix is still load-bearing: Safari reads only [-webkit-backdrop-filter] up
   to 17.6, and [-webkit-text-size-adjust] has no unprefixed Safari support at
   all. *)
let unprefixed_is_widely_available twin =
  let key =
    match Declaration.property_key twin with
    (* CSS Syntax 3 (ED) sec. 8.1: property names are case-insensitive. *)
    | Key (Unknown_property name) ->
        Key (Properties.Unknown_property (String.lowercase_ascii_preserve name))
    | key -> key
  in
  not (Hashtbl.mem greenfield_keys key)

(* Drop a vendor-prefixed declaration when its unprefixed sibling appears in the
   same rule with the same value and importance and is widely available. The
   name is only rendered once the cheap typed twin match has fired. *)
let drop_vendor_aliases ~ctx (kept : (int * declaration) list) :
    (int * declaration) list =
  if Ctx.enforce_spec ctx then kept (* spec-literal: keep every vendor prefix *)
  else
    let has_dead_prefix (_, decl) =
      List.exists
        (fun (_, other) ->
          (vendor_alias_twin decl other || text_vendor_alias_twin decl other)
          && unprefixed_is_widely_available other)
        kept
    in
    filter_preserve (fun item -> not (has_dead_prefix item)) kept

let drained_border_image : Properties.border_image =
  {
    source = None;
    slice = None;
    width = None;
    outset = None;
    repeat = None;
    mode = None;
  }

(* The longhand declaration a shorthand assigns to a slot it covers: the slot
   value if set, else that longhand's initial. A later longhand equal to this is
   a redundant no-op. [None] when the shorthand does not cover the longhand or
   the value cannot be proven. Extend per shorthand; background first. *)
let implied_longhand covering covered : Declaration.declaration option =
  match unwrap_theme_guard covering with
  | Declaration { property = Properties.Background; value; _ } -> (
      (* Single-layer only; a multi-layer background assigns per-layer values a
         single longhand rarely matches, so leave those conservative. *)
      match (value : Properties.background list) with
      | [ Properties.Shorthand s ] -> (
          match unwrap_theme_guard covered with
          | Declaration { property = Properties.Background_size; _ } ->
              Some
                (background_size (Option.value s.size ~default:Properties.Auto))
          | Declaration { property = Properties.Background_repeat; _ } ->
              Some
                (background_repeat
                   (Option.value s.repeat ~default:Properties.Repeat))
          | Declaration { property = Properties.Background_attachment; _ } ->
              Some
                (background_attachment
                   (Option.value s.attachment ~default:Properties.Scroll))
          | _ -> None)
      | _ -> None)
  | Declaration { property = Properties.Flex; value; _ } -> (
      (* [flex] always sets grow/shrink/basis; expand the keyword and numeric
         forms. flex-basis (the [0%] default) is left for a follow-up. *)
      let grow_shrink =
        match (value : Properties.flex) with
        | Initial -> Some (0., 1.)
        | Auto -> Some (1., 1.)
        | None -> Some (0., 0.)
        | Grow (Number g) -> Some (g, 1.)
        | Basis _ -> Some (1., 1.)
        | Grow_shrink (Number g, Number s) -> Some (g, s)
        | Full (Number g, Number s, _) -> Some (g, s)
        | _ -> Option.none
      in
      match (unwrap_theme_guard covered, grow_shrink) with
      | Declaration { property = Properties.Flex_grow; _ }, Some (g, _) ->
          Some (flex_grow g)
      | Declaration { property = Properties.Flex_shrink; _ }, Some (_, s) ->
          Some (flex_shrink s)
      | _ -> None)
  | Declaration { property = Properties.Transition; value; _ } -> (
      (* Single transition only; a comma list assigns per-item values. *)
      match (value : Properties.transition list) with
      | [ Properties.Shorthand s ] -> (
          match unwrap_theme_guard covered with
          | Declaration { property = Properties.Transition_duration; _ } ->
              Some
                (transition_duration
                   (Option.value s.duration ~default:(Values.S 0.)))
          | Declaration { property = Properties.Transition_delay; _ } ->
              Some
                (transition_delay (Option.value s.delay ~default:(Values.S 0.)))
          | Declaration { property = Properties.Transition_timing_function; _ }
            ->
              Some
                (transition_timing_function
                   (Option.value s.timing_function ~default:Properties.Ease))
          | Declaration { property = Properties.Transition_behavior; _ } ->
              Some
                (transition_behavior
                   (Option.value s.behavior ~default:Properties.Normal))
          | _ -> None)
      | _ -> None)
  | Declaration { property = Properties.Border; value; _ } -> (
      (* Only the slots [border] sets explicitly; its initials (medium / none /
         currentcolor) are rarely written back, so leave those. *)
      match (value : Properties.border) with
      | Properties.Shorthand s -> (
          match unwrap_theme_guard covered with
          | Declaration { property = Properties.Border_color; _ } ->
              Option.map border_color s.color
          | Declaration { property = Properties.Border_width; _ } ->
              Option.map border_width s.width
          | Declaration { property = Properties.Border_style; _ } ->
              Option.map border_style s.style
          (* sec. 3.4 also resets [border-image], so the shorthand says what an
             all-initial [border-image] says. *)
          | Declaration { property = Properties.Border_image; _ } ->
              Some (Declaration.v Border_image drained_border_image)
          | _ -> None)
      | _ -> None)
  | Declaration { property = Properties.Animation; value; _ } -> (
      (* Scroll-driven Animations 1 appendix A.3 and CSS Animations 2 sec. 4.12
         make the timeline and the two range ends reset-only sub-properties: the
         shorthand has no slot for a range, so it always writes [normal] there.
         [animation:none] is the same assignment with every slot left at its
         initial, so it implies the same longhands as the shorthand spelling of
         it and a later one of them is as redundant. Single animation only; a
         comma list assigns per-item values. *)
      let timeline : Properties.animation_timeline option =
        match (value : Properties.animation list) with
        | [ Properties.Shorthand s ] ->
            Some (Option.value s.timeline ~default:Properties.Auto)
        | [ Properties.None ] -> Some Properties.Auto
        | _ -> Option.none
      in
      match timeline with
      | Option.None -> None
      | Some timeline -> (
          match unwrap_theme_guard covered with
          | Declaration { property = Properties.Animation_timeline; _ } ->
              Some (Declaration.v Animation_timeline timeline)
          | Declaration { property = Properties.Animation_range_start; _ } ->
              Some
                (Declaration.v Animation_range_start
                   (Normal : Properties.animation_range_item))
          | Declaration { property = Properties.Animation_range_end; _ } ->
              Some
                (Declaration.v Animation_range_end
                   (Normal : Properties.animation_range_item))
          | Declaration { property = Properties.Animation_range; _ } ->
              Some
                (Declaration.v Animation_range
                   (Range (Normal, None) : Properties.animation_range))
          | _ -> None))
  | Declaration { property = Properties.Font; value; _ } -> (
      (* [font] resets style/weight/stretch/line-height to [normal] unless set;
         font-variant uses a narrower type in the shorthand, so leave it. *)
      match (value : Properties.font) with
      | Properties.Shorthand s -> (
          match unwrap_theme_guard covered with
          | Declaration { property = Properties.Font_style; _ } ->
              Some (font_style (Option.value s.style ~default:Normal))
          | Declaration { property = Properties.Font_weight; _ } ->
              Some (font_weight (Option.value s.weight ~default:Normal))
          | Declaration { property = Properties.Font_stretch; _ } ->
              Some (font_stretch (Option.value s.stretch ~default:Normal))
          | Declaration { property = Properties.Line_height; _ } ->
              Some (line_height (Option.value s.line_height ~default:Normal))
          | _ -> None)
      | _ -> None)
  | _ -> None

(* CSS Cascade: a shorthand sets every longhand it covers (to its slot value or
   the initial), so a later longhand in the same rule that writes the same value
   at the same importance is redundant and dropped. Guard on equal value, so
   [background:red;background-size:cover] keeps the override. *)
let drop_longhands_after_covering_shorthand props =
  let arr = Array.of_list props in
  let dropped = Array.make (Array.length arr) false in
  let redundant_via j li =
    (not (Declaration.same_property arr.(j) li))
    && Bool.equal (is_important arr.(j)) (is_important li)
    &&
    match implied_longhand arr.(j) li with
    | Some implied ->
        (* [implied] is built here from the shorthand's slots and the initials
           it resets the rest to, so it has not been through the normalisation
           every declaration in [props] already carries. Canonicalise it before
           comparing, or a slot filled by an initial reads as different from the
           same value written out. *)
        String.equal
          (string_of_value ~minify:true (Declaration.normalize implied))
          (string_of_value ~minify:true li)
    | None -> false
  in
  Array.iteri
    (fun i li ->
      let rec nearest j =
        if j < 0 then ()
        else if dropped.(j) then nearest (j - 1)
        else if declaration_covers arr.(j) li then
          if redundant_via j li then dropped.(i) <- true else ()
        else nearest (j - 1)
      in
      nearest (i - 1))
    arr;
  let kept = List.filteri (fun i _ -> not dropped.(i)) props in
  preserve_list props kept
