open Declaration_intf
open Common
open Properties
open Values

(* Re-export pp_property from Properties module *)
let pp_property = pp_property

(* Extract metadata from a declaration *)
let rec meta_of_declaration : declaration -> meta option = function
  | Declaration
      { property = Custom_property _; value = Custom_value { meta; _ }; _ } ->
      meta
  | Declaration _ -> None
  | Theme_guarded { decl; _ } -> meta_of_declaration decl

(* Smart constructor. The [hash] field is the stdlib bounded structural hash of
   the (property, value, important) triple, so structurally-equal declarations
   share a hash (the invariant [same_minified] uses for its O(1) inequality
   short-circuit). Bounded depth keeps construction O(1) on large value
   subtrees. *)
let v (type a) ?(important = false) (property : a Properties.property)
    (value : a) =
  let hash = Hashtbl.seeded_hash_param 30 100 0 (property, value, important) in
  Declaration { property; value; important; hash }

let theme_guarded ~var_name decl =
  let hash = Hashtbl.seeded_hash_param 30 100 0 (var_name, decl) in
  Theme_guarded { var_name; decl; hash }

(* Read the cached structural hash. Two declarations that minify to the same
   text always produce the same value here; the converse may fail on hash
   collisions, so callers still confirm with [equal_declaration] before treating
   two declarations as equal. *)
let hash = function
  | Declaration { hash; _ } -> hash
  | Theme_guarded { hash; _ } -> hash

let unknown_property ?(important = false) name value =
  let components = Cursor.remaining (Cursor.of_string value) in
  v ~important (Unknown_property name) components

(* Helper to mark a declaration as important *)
let rec important = function
  | Declaration { property; value; _ } -> v ~important:true property value
  | Theme_guarded g -> theme_guarded ~var_name:g.var_name (important g.decl)

(* Apply AST-level value normalisation so the optimizer holds a canonical AST.
   The pretty-printer is a pure serialiser of the result; this is where semantic
   value folds live (see [Properties.normalize_property_value]). *)
let rec normalize ?(lossless = false) ?(exact_srgb = false)
    ?(resolve_missing = false) ?(ctx = Values.default_calc_ctx) = function
  | Declaration { property; value; important; _ } as decl ->
      let value' =
        Properties.normalize_property_value ~lossless ~exact_srgb
          ~resolve_missing ~ctx property value
      in
      if value' == value then decl else v ~important property value'
  | Theme_guarded g as themed ->
      let decl = normalize ~lossless ~exact_srgb ~resolve_missing ~ctx g.decl in
      if decl == g.decl then themed else theme_guarded ~var_name:g.var_name decl

(** Check if a declaration is marked as important *)
let rec is_important = function
  | Declaration { important; _ } -> important
  | Theme_guarded { decl; _ } -> is_important decl

let is_decl_unknown_property_name name =
  let r = Cursor.of_string name in
  match read_any_property r with
  | Prop (Unknown_property _) ->
      Cursor.ws r;
      Cursor.is_done r
  | Prop _ -> false
  | exception Cursor.Parse_error _ -> (
      (* [read_any_property] now rejects unrecognized names; treat them as
         unknown if the input was otherwise a well-formed ident. *)
      let r = Cursor.of_string name in
      match Cursor.ident r with
      | _ ->
          Cursor.ws r;
          Cursor.is_done r
      | exception Cursor.Parse_error _ -> false)

(** [is_invalid decl] is [true] when [decl]'s typed value is a known spec
    violation detected at parse time; [Optimize.drop_invalid] removes such
    declarations on every serialisation. An unknown property is not invalid:
    browsers keep unrecognised declarations (CSS Syntax 3 (ED) sec. 5.5), and
    cascade emits them (and vendor-prefix extensions) as raw component lists. *)
let rec is_invalid = function
  | Declaration { property = Unknown_property _; _ } -> false
  | Declaration { property; value; _ } ->
      Properties.is_invalid_value property value
  | Theme_guarded { decl; _ } -> is_invalid decl

(* Generic colour traversal: extract the colours a declaration's value can carry
   and test them with a [color -> bool] predicate. *)
let color_opt_uses p = function Some c -> p c | None -> false

let rec shadow_uses_color p : Properties.shadow -> bool = function
  | Shadow { color; _ } -> color_opt_uses p color
  | List shs -> List.exists (shadow_uses_color p) shs
  | _ -> false

let text_shadow_uses_color p : Properties.text_shadow -> bool = function
  | Text_shadow { color; _ } -> color_opt_uses p color
  | _ -> false

let border_uses_color p : Properties.border -> bool = function
  | Shorthand { color; _ } -> color_opt_uses p color
  | _ -> false

let outline_uses_color p : Properties.outline -> bool = function
  | Shorthand { color; _ } -> color_opt_uses p color
  | _ -> false

let logical_color_uses p : Properties.logical_border_color -> bool = function
  | Single c -> p c
  | Pair (a, b) -> p a || p b
  | _ -> false

let rec gradient_stop_uses_color p : Properties.gradient_stop -> bool = function
  | Color_percentage (c, _, _) | Color_length (c, _, _) -> p c
  | List stops -> List.exists (gradient_stop_uses_color p) stops
  | _ -> false

let rec background_image_uses_color p : Properties.background_image -> bool =
  function
  | Linear_gradient (_, stops)
  | Repeating_linear_gradient (_, stops)
  | Webkit_linear_gradient (_, stops)
  | Webkit_repeating_linear_gradient (_, stops)
  | Moz_linear_gradient (_, stops)
  | Moz_repeating_linear_gradient (_, stops)
  | O_linear_gradient (_, stops)
  | O_repeating_linear_gradient (_, stops)
  | Radial_gradient (_, stops)
  | Repeating_radial_gradient (_, stops)
  | Webkit_radial_gradient (_, stops)
  | Webkit_repeating_radial_gradient (_, stops)
  | Moz_radial_gradient (_, stops)
  | Moz_repeating_radial_gradient (_, stops)
  | O_radial_gradient (_, stops)
  | O_repeating_radial_gradient (_, stops)
  | Conic_gradient (_, stops)
  | Repeating_conic_gradient (_, stops) ->
      List.exists (gradient_stop_uses_color p) stops
  | List imgs -> List.exists (background_image_uses_color p) imgs
  | _ -> false

let rec filter_uses_color p : Properties.filter -> bool = function
  | Drop_shadow sh -> shadow_uses_color p sh
  | List fs -> List.exists (filter_uses_color p) fs
  | _ -> false

let property_value_uses_color (type a) (p : Values.color -> bool)
    (property : a Properties.property) (value : a) : bool =
  match property with
  | Color -> p value
  | Background_color -> p value
  | Border_color -> List.exists p value
  | Border_top_color -> p value
  | Border_right_color -> p value
  | Border_bottom_color -> p value
  | Border_left_color -> p value
  | Border_inline_start_color -> p value
  | Border_inline_end_color -> p value
  | Border_block_start_color -> p value
  | Border_block_end_color -> p value
  | Outline_color -> p value
  | Text_decoration_color -> p value
  | Text_emphasis_color -> p value
  | Accent_color -> p value
  | Caret_color -> p value
  | Stop_color -> p value
  | Flood_color -> p value
  | Lighting_color -> p value
  | Webkit_tap_highlight_color -> p value
  | Webkit_text_decoration_color -> p value
  | Webkit_text_fill_color -> p value
  | Webkit_text_stroke_color -> p value
  | Column_rule_color -> List.exists p value
  | Border_inline_color -> logical_color_uses p value
  | Border_block_color -> logical_color_uses p value
  | Box_shadow -> shadow_uses_color p value
  | Text_shadow -> List.exists (text_shadow_uses_color p) value
  | Border -> border_uses_color p value
  | Border_top -> border_uses_color p value
  | Border_right -> border_uses_color p value
  | Border_bottom -> border_uses_color p value
  | Border_left -> border_uses_color p value
  | Border_block -> border_uses_color p value
  | Column_rule -> List.exists (border_uses_color p) value
  | Outline -> outline_uses_color p value
  | Background_image -> List.exists (background_image_uses_color p) value
  | Webkit_mask_image -> background_image_uses_color p value
  | Mask_image -> background_image_uses_color p value
  | Filter -> filter_uses_color p value
  | Webkit_filter -> filter_uses_color p value
  | Ms_filter -> filter_uses_color p value
  | Backdrop_filter -> filter_uses_color p value
  | Webkit_backdrop_filter -> filter_uses_color p value
  | _ -> false

let rec value_uses_color p = function
  | Theme_guarded { decl; _ } -> value_uses_color p decl
  | Declaration { property; value; _ } ->
      property_value_uses_color p property value

let value_uses_color_4 decl = value_uses_color Values.color_is_color_4 decl

(* Whether a browser in [targets] drops [decl] at parse time for a colour it
   does not read: the earlier declaration is a fallback for that browser and
   dead everywhere else. *)
let value_uses_color_unimplemented_by targets decl =
  value_uses_color
    (Values.color_uses_feature_where (Support.unimplemented_by targets))
    decl

let length_list_has_runtime_subst lengths =
  List.exists Values.length_has_runtime_subst lengths

let property_value_uses_runtime_subst (type a)
    (property : a Properties.property) (value : a) : bool =
  match property with
  | Margin_top -> Values.length_has_runtime_subst value
  | Margin_right -> Values.length_has_runtime_subst value
  | Margin_bottom -> Values.length_has_runtime_subst value
  | Margin_left -> Values.length_has_runtime_subst value
  | Padding_top -> Values.length_has_runtime_subst value
  | Padding_right -> Values.length_has_runtime_subst value
  | Padding_bottom -> Values.length_has_runtime_subst value
  | Padding_left -> Values.length_has_runtime_subst value
  | Border_top_left_radius -> length_list_has_runtime_subst value
  | Border_top_right_radius -> length_list_has_runtime_subst value
  | Border_bottom_left_radius -> length_list_has_runtime_subst value
  | Border_bottom_right_radius -> length_list_has_runtime_subst value
  | Outline_width -> Properties.border_width_has_runtime_subst value
  | Margin -> length_list_has_runtime_subst value
  | Padding -> length_list_has_runtime_subst value
  | Inset -> length_list_has_runtime_subst value
  | _ -> false

let rec value_uses_runtime_subst = function
  | Theme_guarded { decl; _ } -> value_uses_runtime_subst decl
  | Declaration { property; value; _ } ->
      property_value_uses_runtime_subst property value

(** Get the property name as a string from a declaration *)
let rec property_name decl =
  match decl with
  | Declaration { property; _ } ->
      Pp.to_string ~minify:true pp_property property
  | Theme_guarded { decl; _ } -> property_name decl

(* A property identity that compares without serialising to a string. The
   [property] GADT has only two payload-carrying constructors (Custom_property /
   Unknown_property, both string); the rest are nullary. Packing into [prop_key]
   unifies their existential types so stdlib structural equality answers "same
   property name" directly - no [Pp], no [Obj.repr]. *)
type prop_key = Key : 'a Properties.property -> prop_key [@@unboxed]

(* The pack hides the value's type, so nothing witnesses a per-property
   comparison and the value is compared by walking its representation - the same
   walk [v] hashes. That walk has to be [compare], not [(=)]: IEEE leaves a NaN
   equal to nothing, itself included, so [(=)] read [opacity:calc(NaN)] as
   different from itself while [hash], which normalises NaN, read it as the
   same. CSS has one NaN, a parse-time keyword of the <number> grammar (Values 4
   sec. 10.7.2) serialised as calc(NaN) (sec. 10.13). *)
let equal_declaration (a : declaration) b = Stdlib.compare a b = 0

(* Two declarations minify to the same text exactly when their canonical ASTs
   are equal (property, value, and importance). After the optimizer's
   canonicalisation passes the AST is canonical, so structural equality is the
   minified-equality test - and far cheaper than rendering both to strings. A
   pp-equal-but-structurally-different pair would be a canonicalisation bug, not
   something to paper over by comparing rendered text.

   [(==)] short-circuits the same-heap-object case. The cached [hash] (computed
   once at construction by [v]) short-circuits the (much more common)
   different-value case in one field-load + int compare without walking the AST;
   we only fall through to structural equality on a hash collision. *)
let same_minified a b = a == b || (hash a = hash b && equal_declaration a b)
let equal_prop_key (a : prop_key) b = a = b
let hash_prop_key (key : prop_key) = Hashtbl.hash key

(* The pack hides the value's type but not the property's identity, so the order
   is the one [Properties] gives that identity. *)
let compare_prop_key (a : prop_key) b =
  match (a, b) with Key p, Key q -> Properties.compare_property p q

let rec property_key decl =
  match decl with
  | Declaration { property; _ } -> Key property
  | Theme_guarded { decl; _ } -> property_key decl

(* Equality of the typed property tag. The [property] GADT has only two
   payload-carrying constructors ([Custom_property]/[Unknown_property], both
   string); every other is nullary and interned, so physical equality of the
   unboxed [prop_key] short-circuits the common case allocation-free.
   Existential [property] values are packed into [prop_key] to compare under one
   type. *)
let same_property d1 d2 =
  match (property_key d1, property_key d2) with
  | Key (Custom_property a), Key (Custom_property b) -> String.equal a b
  | Key (Unknown_property a), Key (Unknown_property b) -> String.equal a b
  | k1, k2 -> k1 == k2

let pp_value = Properties.pp_value

let rec value_of : type a. declaration -> a Properties.property -> a option =
 fun decl property ->
  match decl with
  | Declaration { property = p; value; _ } -> (
      match Properties.eq_property p property with
      | Some Equal -> Some value
      | None -> None)
  | Theme_guarded { decl; _ } -> value_of decl property

let rec string_of_value ?(minify = true) ?(inline = false) decl =
  match decl with
  | Declaration { property; value; _ } ->
      Pp.to_string ~minify ~inline pp_property_value (property, value)
  | Theme_guarded { decl; _ } -> string_of_value ~minify ~inline decl

(* Rewrite the value of a custom declaration through [f], which sees its
   minified serialisation. Everything else the declaration carries - its
   importance, its cascade layer, its metadata and any theme guard - belongs to
   the declaration and not to the value, so it is kept: rebuilding with
   [custom_property] instead silently drops all four. *)
let rec map_custom_value f decl =
  match decl with
  | Declaration
      {
        property = Custom_property _ as property;
        value = Custom_value cv;
        important;
        _;
      } ->
      let value = f (string_of_value ~minify:true decl) in
      let components = Cursor.remaining (Cursor.of_string value) in
      v ~important property (Custom_value { cv with value = Tokens components })
  | Declaration _ -> decl
  | Theme_guarded g ->
      let decl' = map_custom_value f g.decl in
      if decl' == g.decl then decl else theme_guarded ~var_name:g.var_name decl'

(* Byte length of [string_of_value] with no allocation; see
   [property_name_size]. *)
let rec value_size ?(minify = true) ?(inline = false) decl =
  match decl with
  | Declaration { property; value; _ } ->
      Pp.size ~minify ~inline pp_property_value (property, value)
  | Theme_guarded { decl; _ } -> value_size ~minify ~inline decl

(* The writer side of the rule [validate_regular_property_components] enforces
   on input. The reader's exemptions are for keeping an authored value verbatim,
   so they do not apply here: nothing licenses building such a value. *)
let value_has_css_wide_mix decl =
  Properties.value_has_css_wide_mix (string_of_value ~minify:true decl)
