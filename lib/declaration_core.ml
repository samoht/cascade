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

(* The cached structural hash. Two declarations equal under [compare] always
   share it; the converse may fail on hash collisions. *)
let hash = function
  | Declaration { hash; _ } -> hash
  | Theme_guarded { hash; _ } -> hash

(* The live declarations of this domain, one per class of [compare]. The smart
   constructors return the member equal to what they build, so declarations
   equal in one domain are one object and [equal_declaration] answers them with
   a pointer compare. The set holds its members weakly: one nothing else holds
   is collected. *)
module Same = struct
  type t = declaration

  (* [Stdlib.compare a b = 0] but for the cached [size], which follows from the
     rest and which a lookup leaves unset, with the types doing what they can
     before the walk: the hashes and the flags settle most pairs that differ,
     and the property is compared by identity, which also proves the two values
     of one type. Only the values are walked. *)
  let rec equal a b =
    a == b
    ||
    match (a, b) with
    | ( Declaration { property = p; value = v; important = i; hash = h; _ },
        Declaration { property = q; value = w; important = j; hash = k; _ } )
      -> (
        Int.equal h k && Bool.equal i j
        &&
        match Properties.eq_property p q with
        | Some Equal -> Stdlib.compare v w = 0
        | None -> false)
    | ( Theme_guarded { var_name = n; decl = d; hash = h; _ },
        Theme_guarded { var_name = m; decl = e; hash = k; _ } ) ->
        Int.equal h k && String.equal n m && equal d e
    | Declaration _, Theme_guarded _ | Theme_guarded _, Declaration _ -> false

  let hash = hash
end

module Interned = Weak.Make (Same)

let interned = Domain.DLS.new_key (fun () -> Interned.create 4096)
let intern decl = Interned.merge (Domain.DLS.get interned) decl

(* The spelling a property parses back from, and the one a diagnostic names: a
   minified [page-break-*] is the CSS Fragmentation 3 sec. 3.4 [break-*] alias
   of a different property, which reports the failure against a property the
   declaration never wrote. *)
let canonical_prop_name (type a) (prop : a property) =
  Pp.to_string ~minify:false pp_property prop

let pp_important ctx important =
  if important then
    Pp.string ctx (if ctx.Pp.minify then "!important" else " !important")

let pp_fields (type a) ctx (property : a property) (value : a) important =
  match (property, value) with
  | Custom_property name, Custom_value { value; layer; _ } ->
      pp_property ctx (Custom_property name);
      Pp.string ctx ":";
      Pp.space_if_pretty ctx ();
      pp_property_value ctx
        (Custom_property name, Custom_value { value; layer; meta = None });
      pp_important ctx important
  | _ ->
      (* Under minify [pp_property] gives a [page-break-*] property the
         [break-*] property it aliases, which CSS Fragmentation 3 sec. 3.4
         defines by a value mapping table. A value the table does not name has
         no alias spelling, so the name follows the value back to the legacy
         property rather than writing it under one that cannot express it. *)
      if minified_name_carries property value then pp_property ctx property
      else Pp.string ctx (canonical_prop_name property);
      Pp.string ctx ":";
      Pp.space_if_pretty ctx ();
      pp_property_value ctx (property, value);
      pp_important ctx important

let rec pp : declaration Pp.t =
 fun ctx -> function
  | Declaration { property; value; important; _ } ->
      pp_fields ctx property value important
  | Theme_guarded { decl; _ } ->
      (* Theme guards are resolved by the transform layer; if one survives to
         print time, emit the wrapped declaration. *)
      pp ctx decl

let rec size = function
  | Declaration { size; _ } -> size
  | Theme_guarded { decl; _ } -> size decl

(* Smart constructors. The [hash] field is the stdlib bounded structural hash of
   the payload, so declarations equal under [compare] share a hash, and bounded
   depth keeps construction O(1) on large value subtrees. The [size] field is
   the length of the minified rendering, so the optimizer's cost models read a
   declaration's size instead of printing it each time they weigh it. *)
(* README: source positions are not kept through formatting, and CSS Custom
   Properties 1 sec. 2 makes a custom property's value its token sequence, so
   where the tokens were read is no part of the value, nor which quote a string
   token was written with. Both are dropped here, where every declaration is
   built, so equality, the hash and interning never see them. *)
let as_value (type a) (property : a Properties.property) (value : a) : a =
  match (property, value) with
  | Custom_property _, Custom_value ({ value = Tokens tokens; _ } as cv) ->
      let tokens' = Component.as_values tokens in
      if tokens' == tokens then value
      else Custom_value { cv with value = Tokens tokens' }
  | _ -> value

let v (type a) ?(important = false) (property : a Properties.property)
    (value : a) =
  let value = as_value property value in
  let hash = Hashtbl.seeded_hash_param 30 100 0 (property, value, important) in
  (* Most declarations a parse builds are already interned, so the set is asked
     first and only a new member is measured. *)
  let set = Domain.DLS.get interned in
  match
    Interned.find_opt set
      (Declaration { property; value; important; hash; size = 0 })
  with
  | Some decl -> decl
  | None ->
      let size =
        Pp.size ~minify:true
          (fun ctx () -> pp_fields ctx property value important)
          ()
      in
      let decl = Declaration { property; value; important; hash; size } in
      Interned.add set decl;
      decl

let theme_guarded ~var_name decl =
  let hash = Hashtbl.seeded_hash_param 30 100 0 (var_name, decl) in
  intern (Theme_guarded { var_name; decl; hash })

let unknown_property ?(important = false) name value =
  let components = Cursor.remaining (Cursor.of_string value) in
  v ~important (Unknown_property name) components

(* Helper to mark a declaration as important *)
let rec important = function
  | Declaration { property; value; _ } -> v ~important:true property value
  | Theme_guarded g -> theme_guarded ~var_name:g.var_name (important g.decl)

(* CSS Values 5 (ED) sec. 11 groups [var()], [env()] and [attr()] as arbitrary
   substitution functions: each carries an unknown token stream into the value,
   so a declaration holding one is valid at parse time and only decided once
   substitution has run. A call with an empty argument list matches no
   substitution grammar and so defers nothing. *)
let is_substitution_call name arguments =
  (match String.lowercase_ascii_preserve name with
    | "var" | "env" | "attr" -> true
    | _ -> false)
  && List.exists
       (fun component -> not (Component.is_whitespace component))
       arguments

(* The fallback of a substitution call's [arguments], after the first comma. *)
let rec substitution_fallback = function
  | [] -> []
  | Component.Preserved { kind = Token.Comma; _ } :: fallback -> fallback
  | _ :: rest -> substitution_fallback rest

let rec slotted_substitution = function
  | Component.Func { node = { name; arguments; _ }; _ } ->
      is_substitution_call name arguments
      || List.exists
           (if Parser.is_math_function name then math_operand_slot
            else slotted_substitution)
           arguments
  | Component.Block { node = { value; _ }; _ } ->
      List.exists slotted_substitution value
  | Component.Flat { node; _ } ->
      Component.flat_calls [ "var"; "env"; "attr" ] node
  | Component.Preserved _ -> false

and math_operand_slot = function
  | Component.Func { node = { name; arguments; _ }; _ } ->
      (not (is_substitution_call name arguments))
      && List.exists
           (if Parser.is_math_function name then math_operand_slot
            else slotted_substitution)
           arguments
  | Component.Block { node = { value; _ }; _ } ->
      List.exists math_operand_slot value
  | (Component.Flat _ | Component.Preserved _) as c -> slotted_substitution c

(* CSS Variables 1 sec. 3 checks a declaration holding a substitution against
   its property's grammar only after substitution, and the call may supply any
   number of components, commas and slashes included. A typed reader gives the
   call one slot, so that slot and every default, elision or reordering decided
   around it is a guess: [flex: var(--g) 0px] read as grow, shrink 1 and basis
   0px prints [var(--g) 1 0], which [--g: 1 0] makes invalid. A substitution
   takes no slot only as the whole value, whose fallback is a whole value in
   turn, or as an operand of a math function, which yields one value whatever it
   substitutes. *)
let rec substitution_takes_a_slot components =
  match List.filter (Fun.negate Component.is_whitespace) components with
  | [ Component.Func { node = { name; arguments; _ }; _ } ]
    when is_substitution_call name arguments ->
      substitution_takes_a_slot (substitution_fallback arguments)
  | components -> List.exists slotted_substitution components

let rec substitution_name = function
  | [] | Component.Preserved { kind = Token.Comma; _ } :: _ -> []
  | c :: rest -> c :: substitution_name rest

let is_substitution = function
  | Component.Func { node = { name; arguments; _ }; _ } ->
      is_substitution_call name arguments
  | _ -> false

let significant = List.filter (Fun.negate Component.is_whitespace)

let rec same_tokens a b = List.equal same_token (significant a) (significant b)

and same_token a b =
  match (a, b) with
  | Component.Func { node = f; _ }, Component.Func { node = g; _ } ->
      String.equal
        (String.lowercase_ascii_preserve f.name)
        (String.lowercase_ascii_preserve g.name)
      && same_tokens f.arguments g.arguments
  | Component.Block { node = x; _ }, Component.Block { node = y; _ } ->
      Token.equal_bracket x.opening y.opening && same_tokens x.value y.value
  | _ ->
      String.equal
        (Parser.string_of_components [ a ])
        (Parser.string_of_components [ b ])

(* A separator or operator token places the components around it, so it is part
   of the slot structure rather than a value in a slot. *)
let is_structural = function
  | Component.Preserved
      { kind = Token.Comma | Token.Delim _ | Token.Colon | Token.Semicolon; _ }
    ->
      true
  | _ -> false

(* CSS Color 4 sec. 5.1 and 7 give [rgba()] and [hsla()] the grammar and
   behaviour of [rgb()] and [hsl()], so either name is the same function. *)
let function_identity name =
  match String.lowercase_ascii_preserve name with
  | "rgba" -> "rgb"
  | "hsla" -> "hsl"
  | name -> name

(* A comma list read as runs: each segment holding a substitution on its own,
   and each maximal run of segments holding none as one [Plain]. *)
type run = Plain | Holding of Component.t list

let rec comma_segments acc segment = function
  | [] -> List.rev (List.rev segment :: acc)
  | Component.Preserved { kind = Token.Comma; _ } :: rest ->
      comma_segments (List.rev segment :: acc) [] rest
  | c :: rest -> comma_segments acc (c :: segment) rest

let holds_substitution =
  List.exists (fun c -> is_substitution c || slotted_substitution c)

let runs components =
  comma_segments [] [] (significant components)
  |> List.fold_left
       (fun acc segment ->
         match (holds_substitution segment, acc) with
         | false, Plain :: _ -> acc
         | false, _ -> Plain :: acc
         | true, _ -> Holding segment :: acc)
       []
  |> List.rev

(* [slots_kept source print] is whether [print] keeps every slot of [source]
   that a substitution may move. A substitution may supply commas, so a comma
   list is compared run by run: each segment holding a substitution keeps each
   call as written, in the same place among the same separators, inside the same
   functions, and a component with no substitution in it may be respelled in its
   place ([0px] as [0], [#ff0000] as [red]) but not added, dropped or moved. A
   run of segments holding none stays on each side of such a segment where it
   was, so what precedes and follows the substitution is still a segment of the
   same kind, and inside it the folds apply: [linear-gradient(to bottom, red,
   var(--x))] may drop its direction, [linear-gradient(to bottom, var(--x))] may
   not, since [--x: to left, red] gives it a second one. *)
let rec slots_kept source print = List.equal run_kept (runs source) (runs print)

and run_kept a b =
  match (a, b) with
  | Plain, Plain -> true
  | Holding a, Holding b -> List.equal slot_kept a b
  | Plain, Holding _ | Holding _, Plain -> false

and slot_kept a b =
  match (a, b) with
  | Component.Func { node = f; _ }, Component.Func { node = g; _ }
    when is_substitution a && is_substitution b ->
      (* The name stays as written; the fallback is a value in the slot. *)
      String.equal
        (String.lowercase_ascii_preserve f.name)
        (String.lowercase_ascii_preserve g.name)
      && same_tokens
           (substitution_name f.arguments)
           (substitution_name g.arguments)
      && slots_kept
           (substitution_fallback f.arguments)
           (substitution_fallback g.arguments)
  | _ when is_substitution a || is_substitution b -> false
  | Component.Func { node = f; _ }, Component.Func { node = g; _ }
    when slotted_substitution a ->
      String.equal (function_identity f.name) (function_identity g.name)
      && slots_kept f.arguments g.arguments
  | Component.Block { node = x; _ }, Component.Block { node = y; _ }
    when slotted_substitution a ->
      Token.equal_bracket x.opening y.opening && slots_kept x.value y.value
  | _ when slotted_substitution a || slotted_substitution b -> false
  | _ when is_structural a || is_structural b -> same_token a b
  | _ -> true

let value_components_of : type a. a property -> a -> Component.t list =
 fun property value ->
  let buf = Buffer.create 32 in
  Properties.pp_property_value (Pp.ctx buf) (property, value);
  Cursor.remaining (Cursor.of_string (Buffer.contents buf))

let rec value_components = function
  | Declaration { property; value; _ } -> value_components_of property value
  | Theme_guarded { decl; _ } -> value_components decl

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
      if value' == value then decl
      else
        (* A fold may not move what a substitution can supply. *)
        let source = value_components_of property value in
        if
          substitution_takes_a_slot source
          && not (slots_kept source (value_components_of property value'))
        then decl
        else v ~important property value'
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
  | Color_percentage (c, _, _) | Color_length (c, _, _) | Color_angle (c, _, _)
    ->
      p c
  | List stops -> List.exists (gradient_stop_uses_color p) stops
  | Length _ | Angle_hint _ | Channel _ | Percentage _ | Position _
  | Direction _ | Var _ ->
      false

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
  | Webkit_cross_fade (first, second, _) ->
      background_image_uses_color p first
      || background_image_uses_color p second
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
  | Webkit_border_top_left_radius -> length_list_has_runtime_subst value
  | Border_top_right_radius -> length_list_has_runtime_subst value
  | Webkit_border_top_right_radius -> length_list_has_runtime_subst value
  | Border_bottom_left_radius -> length_list_has_runtime_subst value
  | Webkit_border_bottom_left_radius -> length_list_has_runtime_subst value
  | Border_bottom_right_radius -> length_list_has_runtime_subst value
  | Webkit_border_bottom_right_radius -> length_list_has_runtime_subst value
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

(* No value type has a comparison of its own, so once [Same.equal] has matched
   the property the value is compared by walking its representation - the same
   walk [v] hashes. That walk has to be [compare], not [(=)]: IEEE leaves a NaN
   equal to nothing, itself included, so [(=)] read [opacity:calc(NaN)] as
   different from itself while [hash], which normalises NaN, read it as the
   same. CSS has one NaN, a parse-time keyword of the <number> grammar (Values 4
   sec. 10.7.2) serialised as calc(NaN) (sec. 10.13).

   Interning makes an equal pair built in one domain one object, so the walk is
   left to a pair built in two domains or one whose hashes collide; differing
   hashes answer that the pair differs.

   Two declarations minify to the same text exactly when their canonical ASTs
   are equal (property, value, and importance), so this is also the
   minified-equality test, far cheaper than rendering both. A
   pp-equal-but-structurally-different pair would be a canonicalisation bug, not
   something to paper over by comparing rendered text. *)
let equal_declaration = Same.equal
let same_minified = equal_declaration
let hash_prop_key (key : prop_key) = Hashtbl.hash key

(* The pack hides the value's type but not the property's identity, so the order
   is the one [Properties] gives that identity. *)
let compare_prop_key (a : prop_key) b =
  match (a, b) with Key p, Key q -> Properties.compare_property p q

(* A nullary property is an immediate, so the pointer test answers most equal
   pairs; the tag order answers the rest without a runtime comparison. *)
let equal_prop_key (a : prop_key) b = a == b || compare_prop_key a b = 0

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

let later_version a b = if Support.compare_version a b >= 0 then a else b

(* What one need asks of a release: [Some (Some v)] parses from [v], [Some None]
   is shipped by no release of the data, and [None] is a production the data
   does not name, which counts as parsed. *)
let combine acc need =
  match (acc, need) with
  | Option.None, _ -> Option.None
  | acc, Option.None -> acc
  | Option.Some _, Option.Some Option.None -> Option.None
  | Option.Some a, Option.Some (Option.Some v) ->
      Option.Some (later_version a v)

(* BCD files a keyword a property takes as [css.properties.<property>.<word>]
   and a function as the same key, or as [<word>_function] where a keyword
   shares its name. A function no key of the property names is filed under the
   value type that takes it, [css.types.<function>] or
   [css.types.<type>.<function>]; a deeper key is a detail of some production
   rather than the production. A name two value types share, such as the
   [rect()] of [<shape>] and of [<basic-shape>], is parsed from the first
   release that ships either. *)
let type_keys name =
  List.filter
    (fun key ->
      String.starts_with ~prefix:"css.types." key
      && List.length (String.split_on_char '.' key) <= 4)
    (Support.keys_named name)

let earliest releases =
  List.fold_left
    (fun acc release ->
      match (acc, release) with
      | Option.None, r | r, Option.None -> r
      | Option.Some a, Option.Some b ->
          Option.Some (if Support.compare_version a b <= 0 then a else b))
    Option.None releases

let word_need ~release property (kind : Token.kind) =
  let of_property word =
    String.concat "" [ "css.properties."; property; "."; word ]
  in
  match kind with
  | Ident word -> release (of_property (String.lowercase_ascii word))
  | Function name -> (
      let name = String.lowercase_ascii name in
      match
        List.find_map release
          [
            of_property (String.concat "" [ name; "_function" ]);
            of_property name;
          ]
      with
      | Option.Some _ as need -> need
      | Option.None -> (
          match List.filter_map release (type_keys name) with
          | [] -> Option.None
          | releases -> Option.Some (earliest releases)))
  | _ -> Option.None

let value_tokens decl =
  let lexer = Lexer.of_string (string_of_value ~minify:true decl) in
  let rec go acc =
    match (Lexer.next lexer).kind with Eof -> acc | kind -> go (kind :: acc)
  in
  go []

let is_var : Token.kind -> bool = function
  | Function name -> String.equal (String.lowercase_ascii name) "var"
  | _ -> false

(* The productions a value uses that no word of it names, by the key each is
   filed under: the [/ <'column-height'>] of [columns] (CSS Multicol 2 sec. 4.5)
   is the [column-height] longhand's production. Each key names its production
   exactly, so the measured supplement answers for it as well. *)
let rec structure_keys decl =
  match decl with
  | Theme_guarded { decl; _ } -> structure_keys decl
  | Declaration { property = Columns; value = With_height _; _ } ->
      [ "css.properties.column-height" ]
  | Declaration _ -> []

(* CSS 2.1 sec. 4.2 has a browser ignore a declaration whose value it does not
   parse. Which productions a declaration uses is read off its property, its
   colours and the words of its value, and a release parses the declaration once
   it parses every one of them, as the data records it. A custom property takes
   any value, and CSS Custom Properties 1 sec. 3 has a declaration holding
   [var()] assumed valid at parse time, so neither waits on its value. The
   colour constructs are gathered from the typed value. *)
let first_release_with ~tokens ~floor engine decl =
  let release = Support.parse_release engine in
  let property = String.lowercase_ascii (property_name decl) in
  let acc =
    combine (Option.Some floor)
      (release (String.concat "" [ "css.properties."; property ]))
  in
  let acc =
    List.fold_left
      (fun acc key -> combine acc (Support.engine_shipped engine key))
      acc (structure_keys decl)
  in
  let tokens = Lazy.force tokens in
  if List.exists is_var tokens then acc
  else
    let colour_keys = ref [] in
    ignore
      (value_uses_color
         (Values.color_uses_feature_where (fun key ->
              colour_keys := key :: !colour_keys;
              false))
         decl);
    let acc =
      List.fold_left (fun acc key -> combine acc (release key)) acc !colour_keys
    in
    List.fold_left
      (fun acc kind -> combine acc (word_need ~release property kind))
      acc tokens

let rec first_release ~floor engine decl =
  match decl with
  | Theme_guarded { decl; _ } -> first_release ~floor engine decl
  | Declaration { property = Custom_property _; _ } -> Option.Some floor
  | Declaration _ ->
      first_release_with ~tokens:(lazy (value_tokens decl)) ~floor engine decl

(* The browsers judged for are each engine from its floor to the newest release
   the data records, and a shipped production stays. [earlier] is what a browser
   paints in place of [later] only where it parses [earlier] and drops [later]:
   in an engine, from the release that first parses [earlier] until the one that
   first parses [later], if that comes after it. A browser dropping both, as
   Safari does [user-select:auto] and [user-select:none], paints neither, and
   one parsing both paints [later]. *)
let fallback_for ~floors ~later earlier =
  List.exists
    (fun (engine, floor) ->
      match first_release ~floor engine earlier with
      | Option.None -> false
      | Option.Some reads_earlier -> (
          match first_release ~floor engine later with
          | Option.None -> true
          | Option.Some reads_later ->
              Support.compare_version reads_later reads_earlier > 0))
    (Support.floors floors)

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
