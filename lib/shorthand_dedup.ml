open Declaration
open Common
open Shorthand_common
open Shorthand_coverage

let property_covered_by_important kept decl =
  List.exists
    (fun (_, existing) ->
      (not (is_intentionally_duplicated existing))
      && is_important existing
      && declaration_covers existing decl)
    kept

(* Rebuilt through the smart constructors: they recompute the cached
   [Declaration.hash] over the new payload, and a record update would leave it
   fingerprinting the importance that was just cleared. *)
let rec without_importance = function
  | Declaration { property; value; _ } ->
      Declaration.v ~important:false property value
  | Theme_guarded { var_name; decl; _ } ->
      Declaration.theme_guarded ~var_name (without_importance decl)

(* Value equality ignoring importance. Every caller establishes
   [Declaration.same_property] first, so the two declarations share a value type
   and this is a structural value comparison: on canonical ASTs it matches
   minified-text equality without rendering. *)
let same_value a b =
  Declaration.equal_declaration (without_importance a) (without_importance b)

(* Two declaration lists commute when no pair across them conflicts. The
   footprint is computed once per declaration rather than once per pair, since
   the test below is quadratic. *)
let declarations_commute left right =
  let with_keys decls =
    List.map (fun decl -> (decl, declaration_overlap_keys decl)) decls
  in
  let left = with_keys left and right = with_keys right in
  not
    (List.exists
       (fun (a, a_keys) ->
         List.exists
           (fun (b, b_keys) ->
             declarations_conflict_with_keys a a_keys b b_keys)
           right)
       left)

(* An earlier declaration the later one covers is a fallback for a browser that
   drops the later one at parse time and reads it. The floors stand for their
   releases and every later one, so the earlier declaration is dead only when
   every browser at or above them that reads it also reads the later one
   ({!Declaration.fallback_for}). The minifier and a run that judges for
   browsers ask this one question of the same floors ({!Ctx.floors}), so
   whatever the minifier deletes the judge also counts dead. *)
let floor_fallback ~floors new_decl existing =
  (not (same_value new_decl existing))
  && Declaration.fallback_for ~floors ~later:new_decl existing

(* A run that does not judge also keeps the fallback before a CSS Color 4 / 5
   colour every floor reads, for a browser older than them. *)
let legacy_color_fallback ~judged new_decl existing =
  (not judged)
  && Declaration.same_property new_decl existing
  && (not (same_value new_decl existing))
  && Declaration.value_uses_color_4 new_decl
  && not (Declaration.value_uses_color_4 existing)

(* Same shape: the later value uses a runtime substitution ([var()] / [env()] /
   [attr()]) and the earlier doesn't, so the earlier is a static fallback for
   browsers that can't resolve the substitution at parse time. Every target
   resolves one, and a declaration holding a substitution is never dropped at
   parse time, so a run that judges for targets finds the later one always wins
   the cascade and the earlier dead. *)
let legacy_runtime_subst_fallback ~judged new_decl existing =
  (not judged)
  && Declaration.same_property new_decl existing
  && (not (same_value new_decl existing))
  && Declaration.value_uses_runtime_subst new_decl
  && not (Declaration.value_uses_runtime_subst existing)

let same_property_value_declaration new_decl existing =
  Declaration.same_property new_decl existing
  && same_value new_decl existing
  && (is_important new_decl || not (is_important existing))

let covered_by_new_declaration ~floors ~judged new_decl existing =
  (not (is_intentionally_duplicated existing))
  && declaration_covers new_decl existing
  && (is_important new_decl || not (is_important existing))
  && (not (Declaration.legacy_pair new_decl existing))
  && (not (floor_fallback ~floors new_decl existing))
  && (not (legacy_color_fallback ~judged new_decl existing))
  && not (legacy_runtime_subst_fallback ~judged new_decl existing)

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

(* A [content] value every target reads: nothing written before it is left to
   fall back to. An image, [attr()], a [var()] or a list holding one of those
   may be what some browser rejects, and keeps the declarations before it. *)
let rec plain_content : Properties.content -> bool = function
  | String _ | Quoted _ | None | Normal | Open_quote | Close_quote
  | No_open_quote | No_close_quote | Inherit | Initial | Unset | Revert
  | Revert_layer ->
      true
  (* Chrome 153 refuses a [symbols()] counter style. *)
  | Counter { style; _ } | Counters { style; _ } -> (
      match style with Some (Symbols _) -> false | _ -> true)
  | Content_list items -> List.for_all plain_content items
  (* A browser that predates the alternative text of CSS Generated Content 3
     sec. 1.1 drops the value that holds one. *)
  | Image _ | Attr _ | String_ref _ | Var _ | With_alt _ -> false

let superseding_content new_decl existing =
  match (new_decl, existing) with
  | ( Declaration { property = Content; value; important; _ },
      Declaration { property = Content; important = old_important; _ } ) ->
      plain_content value && (important || not old_important)
  | _ -> false

(* A normal [content] after an important plain one stands in for nothing: the
   important one wins whatever the order, and every target reads it. *)
let under_important_content kept decl =
  (not (is_important decl))
  && List.exists
       (fun (_, old) -> is_important old && superseding_content old decl)
       kept

let deduplicate_step ~floors ~judged kept (idx, decl) =
  if under_important_content kept decl then kept
  else if is_intentionally_duplicated decl then
    let kept =
      filter_preserve
        (fun (_, old) ->
          not
            (same_property_value_declaration decl old
            || superseding_content decl old))
        kept
    in
    (idx, decl) :: kept
  else if (not (is_important decl)) && property_covered_by_important kept decl
  then kept
  else
    let kept =
      filter_preserve
        (fun (_, old) ->
          not (covered_by_new_declaration ~floors ~judged decl old))
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

(* The legacy name aliases cascade types under their unprefixed property's
   grammar alone: beside that property with one value and importance, such an
   alias says nothing the property does not. *)
let types_only_as_alias : type a. a Properties.property -> bool = function
  | Webkit_backface_visibility | Webkit_border_top_left_radius
  | Webkit_border_top_right_radius | Webkit_border_bottom_right_radius
  | Webkit_border_bottom_left_radius | Webkit_flex | Webkit_flex_grow
  | Webkit_flex_shrink | Webkit_flex_basis | Webkit_order
  | Webkit_perspective_origin | Webkit_transform_origin | Webkit_transform_style
    ->
      true
  | _ -> false

let legacy_alias_twin vendor twin =
  match (vendor, twin) with
  | ( Declaration { property = alias; important = i1; _ },
      Declaration { property; important = i2; _ } )
    when types_only_as_alias alias -> (
      match Properties.legacy_alias alias with
      | Some (Prop aliased) ->
          Option.is_some (Properties.eq_property aliased property)
          && same_value_text vendor twin
          && Bool.equal i1 i2
      | None -> false)
  | _ -> false

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
      || legacy_alias_twin vendor twin

(* A legacy name alias is its property under a second name, so it reads as that
   property: its value under the property's grammar, its importance kept. Every
   pass after this one then sees two writes of one property where the sheet
   wrote the alias and the property, and the later wins whatever the values or
   the rules that hold them. A value the property's grammar refuses keeps the
   alias as written. *)
let as_aliased_property decl =
  match decl with
  | Declaration.Theme_guarded _ -> decl
  | Declaration.Declaration { property; _ } -> (
      match Properties.legacy_alias property with
      | None -> decl
      | Some (Prop unprefixed) -> Declaration.as_property unprefixed decl)

(* [-webkit-text-decoration-color] is not on the sec. 3.4.1 list, so
   {!as_aliased_property} leaves it as written, but every engine that ships the
   prefix aliases it and Cascade normalizes it whatever the target. Beside an
   identical unprefixed twin it says nothing the twin does not. A differing
   fallback, mismatched importance, or the prefix alone remains observable and
   is kept. *)
let drop_redundant_decoration_color_aliases declarations =
  filter_preserve
    (fun declaration ->
      not (List.exists (decoration_color_alias_twin declaration) declarations))
    declarations

(* Whether [decl]'s property is one no browser a judge can name reads. Chrome
   151 and WebKit 26.5 list no [-ms-] or [-o-] property and support none, and
   Firefox's property tables carry only the [moz] and [webkit] prefixes: those
   were Internet Explorer's and Presto's, and neither parses a [-khtml-] one,
   Konqueror's. Nor is [font-smooth] or [font-smoothing] a property of any
   target: neither engine parses either, Firefox's longhands.toml lists neither,
   and MDN's compatibility data records [font-smooth] only under the prefixed
   names the reader types. *)
let read_by_no_target decl =
  let name = String.lowercase_ascii (Declaration.property_name decl) in
  let has p =
    String.length name > String.length p
    && String.equal (String.sub name 0 (String.length p)) p
  in
  has "-ms-" || has "-o-" || has "-khtml-"
  || match name with "font-smooth" | "font-smoothing" -> true | _ -> false

(* Structural vendor-alias drop for an [Unknown_property] [vendor]: we drop
   [vendor] when stripping its vendor prefix yields [twin]'s name and the two
   values agree under matching importance. When [twin] is untyped too their
   component-value lists are compared; when the reader types [twin], [vendor]'s
   value is read under [twin]'s grammar and the two minified values compared, as
   the next pass would see them, so typing the unprefixed property does not keep
   a prefixed copy the untyped pair would drop. A typed vendor longhand
   ([-webkit-animation-delay] beside [animation-delay]) is not compared here. *)
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
  | ( Declaration { property = Unknown_property vname; important = vi; _ },
      Declaration { important = ti; _ } )
    when Bool.equal vi ti -> (
      match strip_vendor_prefix vname with
      | Some modern
        when String.equal
               (String.lowercase_ascii modern)
               (Declaration.property_name twin) -> (
          match
            Declaration.with_value twin
              (Declaration.string_of_value ~minify:true vendor)
          with
          | Ok read ->
              (* The value as read keeps the copy's spelling ([1 0 0%] for the
                 twin's [1 0]); its normal form is the one the twin holds. *)
              let value d =
                Declaration.string_of_value ~minify:true
                  (Declaration.normalize ~lossless:true d)
              in
              String.equal (value read) (value twin)
          | Error _ -> false)
      | _ -> false)
  | _ -> false

(* {!Baseline.greenfield_properties} is generated from web-features as names.
   Resolve each once into the property it spells, so the test below compares
   property tags instead of rendering one. A name the reader does not type keeps
   its [Unknown_property] tag, which is the shape it reaches
   [drop_vendor_aliases] in as a [text_vendor_alias_twin] twin. *)
module Key_set = Set.Make (struct
  type t = prop_key

  let compare = Declaration.compare_prop_key
end)

let greenfield_keys =
  List.fold_left
    (fun keys name ->
      let keys = Key_set.add (Key (Properties.Unknown_property name)) keys in
      match property_of_name name with
      | Some (Properties.Prop property) -> Key_set.add (Key property) keys
      | None -> keys)
    Key_set.empty Baseline.greenfield_properties

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
  not (Key_set.mem key greenfield_keys)

(* Drop a vendor-prefixed declaration when its unprefixed sibling appears in the
   same rule with the same value and importance and is widely available. The
   name is only rendered once the cheap typed twin match has fired. *)
let drop_vendor_aliases ~ctx (kept : (int * declaration) list) :
    (int * declaration) list =
  if Ctx.enforce_spec ctx then kept (* spec-literal: keep every vendor prefix *)
  else
    (* A comparison that judges for targets one of which reads only the prefixed
       spelling keeps that spelling there, as the minifier's synthesis writes it
       for those targets; another prefix of the same property is dead all the
       same. *)
    let target_needs decl other =
      match Ctx.judge ctx with
      | Option.None -> false
      | Option.Some targets -> (
          match Webkit_fallback.of_declaration targets other with
          | Option.Some needed -> Declaration.same_property needed decl
          | Option.None -> false)
    in
    let has_dead_prefix (_, decl) =
      List.exists
        (fun (_, other) ->
          (vendor_alias_twin decl other || text_vendor_alias_twin decl other)
          && unprefixed_is_widely_available other
          && not (target_needs decl other))
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

(* The slots [border] writes, each [None] where the shorthand leaves it at an
   initial value no slot spells: its initials (medium / none / currentcolor) are
   rarely written back. CSS Backgrounds 3 sec. 4.4 also resets [border-image],
   so the shorthand says what an all-initial [border-image] says. *)
let border_slots (s : Properties.border_shorthand) =
  [
    Option.map border_width s.width;
    Option.map border_style s.style;
    Option.map border_color s.color;
    Some (Declaration.v Border_image drained_border_image);
  ]

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
         forms of grow and shrink. The basis is not expanded. *)
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
      match (value : Properties.border) with
      | Properties.Shorthand s ->
          let covered = unwrap_theme_guard covered in
          List.find_map
            (function
              | Some slot when Declaration.same_property slot covered ->
                  Some slot
              | _ -> None)
            (border_slots s)
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

let implied_longhands covering =
  let slots =
    match unwrap_theme_guard covering with
    | Declaration
        { property = Properties.Border; value = Properties.Shorthand s; _ } ->
        border_slots s
    | _ -> [ None ]
  in
  let all_known = List.for_all Option.is_some slots in
  if not all_known then None
  else
    let slots = List.filter_map Fun.id slots in
    Some
      (if is_important covering then List.map Declaration.important slots
       else slots)

(* Whether [covering] sets every slot [covered] writes to the value [covered]
   writes there, so the two agree on every longhand they share and either order
   computes the same values. Only what {!implied_longhand} can prove counts. *)
let implies covering covered =
  (not (Declaration.same_property covering covered))
  && declaration_covers covering covered
  &&
  match implied_longhand covering covered with
  | Some implied ->
      (* [implied] is built here from the shorthand's slots and the initials it
         resets the rest to, so it has not been through the normalisation every
         parsed declaration carries. Canonicalise it before comparing, or a slot
         filled by an initial reads as different from the same value written
         out. *)
      String.equal
        (string_of_value ~minify:true (Declaration.normalize implied))
        (string_of_value ~minify:true covered)
  | None -> false

(* Source order decides between two declarations only where they write some
   shared longhand differently: [border:1px solid red] and [border-width:1px]
   agree on every [border-*-width] they share, so neither order wins anything.
   The footprint test alone reads them as a conflict because the two are spelt
   differently. *)
let declarations_conflict_with_keys a a_keys b b_keys =
  Shorthand_coverage.declarations_conflict_with_keys a a_keys b b_keys
  && not (implies a b || implies b a)

(* CSS Cascade: a shorthand sets every longhand it covers (to its slot value or
   the initial), so a later longhand in the same rule that writes the same value
   at the same importance is redundant and dropped. Guard on equal value, so
   [background:red;background-size:cover] keeps the override. *)
let drop_longhands_after_covering_shorthand props =
  let arr = Array.of_list props in
  let dropped = Array.make (Array.length arr) false in
  let redundant_via j li =
    Bool.equal (is_important arr.(j)) (is_important li) && implies arr.(j) li
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
