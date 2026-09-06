(** CSS declaration types and parser. *)

include Declaration_intf
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

let is_custom_property_name = Custom_property_name.is_valid

(* CSS Syntax 3 (ED) sec. 7.2: a [<declaration-value>] is one or more component
   values with no [<bad-string-token>], no [<bad-url-token>] and no unmatched
   closing bracket. A component breaking one of those does not stay inside the
   declaration it is written into: it closes the enclosing block, or turns the
   rest of the stylesheet into a string. *)
let rec component_stays_in_declaration = function
  (* sec. 4.3.5 ends a string at EOF as the string it read and only a newline
     makes it a [<bad-string-token>], so the first stays in the value. *)
  | Component.Preserved
      { kind = Token.Close _ | Token.Bad_string | Token.Bad_url; _ } ->
      false
  | Component.Preserved _ -> true
  | Component.Block { node = { closed; value; _ }; _ } ->
      closed && List.for_all component_stays_in_declaration value
  | Component.Func { node = { terminated; arguments; _ }; _ } ->
      terminated && List.for_all component_stays_in_declaration arguments

(* A top-level [;] ends the declaration, so the tail becomes a second one. It is
   only a stop at top level: [(a;b)] keeps it inside the block. Sec. 7.2 keeps a
   top-level [!] out of the production for the same reason: sec. 5.5.6 takes the
   [!important] flag off the end before the value is read, so a [!] still in it
   belongs to no flag and to no value. *)
let is_top_level_stop = function
  | Component.Preserved { kind = Token.Semicolon; _ } -> true
  | _ -> false

let is_top_level_bang = function
  | Component.Preserved { kind = Token.Delim "!"; _ } -> true
  | _ -> false

let components_stay_in_declaration components =
  List.for_all
    (fun c ->
      (not (is_top_level_stop c))
      && (not (is_top_level_bang c))
      && component_stays_in_declaration c)
    components

(* CSS Variables 1 sec. 2 gives a custom property the value grammar
   [<declaration-value>?], so the empty value is one of its values. *)
let is_optional_declaration_value value =
  components_stay_in_declaration (Cursor.remaining (Cursor.of_string value))

let is_declaration_value value =
  match Cursor.remaining (Cursor.of_string value) with
  | [] -> false
  | components -> components_stay_in_declaration components

let refuse name detail =
  failwith (String.concat "" [ "custom_property: "; name; ": "; detail ])

(* Helper for raw custom properties - primarily for internal use *)

let custom_property ?layer name value =
  (* The pair binds only when it makes the one declaration it names: a
     [<dashed-ident>] name, and the [<declaration-value>?] CSS Variables 1 sec.
     2 gives a custom property. A value outside that - a top-level [;] or [}],
     an unmatched closing bracket, an unterminated function, block or string -
     leaves the declaration as soon as the text is read back. A name carrying
     one of those is written back with the escapes that read it (CSS Syntax 3
     (ED) sec. 4.3.7), so it stays the name of this declaration. *)
  if not (is_custom_property_name name) then
    refuse name "not a custom-property name";
  if not (is_optional_declaration_value value) then
    refuse name (String.concat "" [ value; " is not a declaration value" ]);
  (* Parse the value into a CSS Syntax 3 component stream so the declaration
     never carries a raw author string; the printer can then re-serialise with
     the active [Pp] context (handles minification). *)
  let components = Cursor.remaining (Cursor.of_string value) in
  v (Custom_property name)
    (Custom_value { value = Tokens components; layer; meta = None })

(* Access the layer associated with a custom declaration, if any *)
let rec custom_declaration_layer = function
  | Declaration
      { property = Custom_property _; value = Custom_value { layer; _ }; _ } ->
      layer
  | Declaration _ -> None
  | Theme_guarded { decl; _ } -> custom_declaration_layer decl

(* Equivalence-only normalisation for structural diffing: rewrite a quoted
   family name in a custom-property stream as the equivalent unquoted [<ident>]
   sequence, one word or several. The forms substitute identically into
   [font-family] so cascade treats them as equal, but keeps both verbatim on
   output (unquoting an opaque custom property could corrupt a [content] use).
   Gated on a generic family being present, since an unregistered property is
   otherwise type-unknown. *)
let unquote_custom_font_strings = function
  | Declaration
      {
        property = Custom_property _ as property;
        value = Custom_value ({ value = Tokens components; _ } as cv);
        important;
        _;
      }
    when Properties.components_have_generic_family components ->
      v ~important property
        (Custom_value
           {
             cv with
             value = Tokens (Properties.unquote_font_family_strings components);
           })
  | decl -> decl

(* Equivalence-only normalisation for structural diffing: drop the whitespace of
   a custom-property stream that CSS reads as nothing. Cascade keeps that
   whitespace verbatim on output, since the stream is opaque and the author's
   bytes are what a [var()] substitutes; two streams that differ only there are
   still the same value. *)
let canonicalize_custom_whitespace = function
  | Declaration
      {
        property = Custom_property _ as property;
        value = Custom_value ({ value = Tokens components; _ } as cv);
        important;
        _;
      } ->
      v ~important property
        (Custom_value
           {
             cv with
             value =
               Tokens
                 (Properties.canonicalize_math_whitespace_components components);
           })
  | decl -> decl

(* Parser functions *)

(** Parse a property name. Property names are plain idents in the component
    stream ([--custom] idents include the leading [--]). *)
let read_property_name t =
  Cursor.ws t;
  Cursor.ident ~keep_case:true t

(** Parse property value. Components up to the next [;] or [!important] mark the
    value. Whitespace is preserved in the drained value so multi-token values
    like "10px 20px" serialise back with their spaces. *)
let read_property_value t =
  Cursor.with_context t "property-value" @@ fun () ->
  Cursor.consume_to_decl_end ~trim:true t

let is_decl_value_stop = function
  | Component.Preserved { kind = Token.Semicolon | Token.Delim "!"; _ } -> true
  | _ -> false

let components_before stop t =
  let rec take acc = function
    | [] -> List.rev acc
    | cv :: _ when stop cv -> List.rev acc
    | cv :: rest -> take (cv :: acc) rest
  in
  take [] (Cursor.remaining t)

let value_components t = components_before is_decl_value_stop t

let rec component_is_complete = function
  | Component.Preserved _ -> true
  | Component.Block { node = { closed; value; _ }; _ } ->
      closed && List.for_all component_is_complete value
  | Component.Func { node = { terminated; arguments; _ }; _ } ->
      terminated && List.for_all component_is_complete arguments

let validate_complete_declaration_value t =
  if not (List.for_all component_is_complete (value_components t)) then
    Cursor.err_invalid t "unterminated component value"

let rec component_has_curly_block = function
  | Component.Block { node = { opening = Token.Curly; _ }; _ } -> true
  | Component.Block { node = { value; _ }; _ } ->
      List.exists component_has_curly_block value
  | Component.Func { node = { arguments; _ }; _ } ->
      List.exists component_has_curly_block arguments
  | Component.Preserved _ -> false

let reject_curly_block_value t =
  if List.exists component_has_curly_block (value_components t) then
    Cursor.err_invalid t "curly block in declaration value"

(* CSS Syntax 3 (ED) sec. 4.3.5 ends a string at a newline as a
   [<bad-string-token>] and at EOF as the [<string-token>] it read, and sec. 7.2
   excludes only the first from a [<declaration-value>]. So a value the input
   ended in the middle of a string carries a string like any other, and Chrome
   151 keeps the declaration. *)
let rec component_has_unterminated_string = function
  | Component.Preserved { kind = Token.Bad_string; _ } -> true
  | Component.Block { node = { value; _ }; _ } ->
      List.exists component_has_unterminated_string value
  | Component.Func { node = { arguments; _ }; _ } ->
      List.exists component_has_unterminated_string arguments
  | Component.Preserved _ -> false

let reject_unterminated_string_value t =
  if List.exists component_has_unterminated_string (value_components t) then
    Cursor.err_invalid t "unterminated string in declaration value"

(* CSS Syntax 3 (ED) sec. 7.2: a [<declaration-value>] is any token sequence
   excluding a [<bad-string-token>], a [<bad-url-token>], and an unmatched [)],
   []] or [}]. A custom property takes [<declaration-value>?] and nothing wider,
   so a value carrying one of those is invalid rather than opaque. *)
let rec component_leaves_declaration_value = function
  | Component.Preserved
      { kind = Token.Bad_string | Token.Bad_url | Token.Close _; _ } ->
      true
  | Component.Block { node = { value; _ }; _ } ->
      List.exists component_leaves_declaration_value value
  | Component.Func { node = { arguments; _ }; _ } ->
      List.exists component_leaves_declaration_value arguments
  | Component.Preserved _ -> false

let reject_custom_bad_string t =
  if
    List.exists component_leaves_declaration_value
      (components_before is_top_level_stop t)
  then Cursor.err_invalid t "custom property value is no <declaration-value>"

(* The same bar for a value the typed readers hand to the opaque path: a [var()]
   postpones the property grammar to computed-value time, not the token
   grammar. *)
let reject_opaque_bad_token t =
  if List.exists component_leaves_declaration_value (value_components t) then
    Cursor.err_invalid t "declaration value is no <declaration-value>"

let invalid_var_arguments arguments =
  match
    List.filter
      (fun component -> not (Component.is_whitespace component))
      arguments
  with
  | Component.Preserved { kind = Token.Ident name; _ } :: rest
    when Custom_property_name.is_valid name -> (
      (* CSS Custom Properties 1 sec. 3: nothing follows the name but a comma
         opening the fallback. This raw-component check gates the opaque
         keep-verbatim path, so it has to agree with the typed reader; without
         the comma case [var(--x 10px)] took that path and was preserved instead
         of invalidating the declaration. *)
      match rest with
      | [] -> false
      | Component.Preserved { kind = Token.Comma; _ } :: _ -> false
      | _ -> true)
  | _ -> true

let rec components_have_invalid_substitution components =
  List.exists component_has_invalid_substitution components

and component_has_invalid_substitution = function
  | Component.Func { node = { name; arguments; terminated; _ }; _ } ->
      let invalid_call =
        match String.lowercase_ascii_preserve name with
        | "var" -> (not terminated) || invalid_var_arguments arguments
        | "env" | "attr" ->
            (not terminated)
            || not
                 (List.exists
                    (fun component -> not (Component.is_whitespace component))
                    arguments)
        | _ -> false
      in
      invalid_call || components_have_invalid_substitution arguments
  | Component.Block { node = { value; _ }; _ } ->
      components_have_invalid_substitution value
  | Component.Preserved _ -> false

let rec components_contain_var components =
  List.exists component_contains_var components

and component_contains_var = function
  | Component.Func { node = { name; arguments; _ }; _ } ->
      String.lowercase_ascii_preserve name = "var"
      || components_contain_var arguments
  | Component.Block { node = { value; _ }; _ } -> components_contain_var value
  | Component.Preserved _ -> false

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

let rec components_have_substitution components =
  List.exists component_has_substitution components

and component_has_substitution = function
  | Component.Func { node = { name; arguments; _ }; _ } ->
      is_substitution_call name arguments
      || components_have_substitution arguments
  | Component.Block { node = { value; _ }; _ } ->
      components_have_substitution value
  | Component.Preserved _ -> false

let raw_value_has_invalid_substitution raw_value =
  Cursor.of_string raw_value |> Cursor.remaining
  |> components_have_invalid_substitution

let raw_value_contains_substitution raw_value =
  (* CSS Variables 1 sec. 3: "if a property contains one or more var()
     functions, and those functions are syntactically valid, the entire
     property's grammar must be assumed to be valid at parse time". A call
     written inside another function is one the property contains, so the
     leniency reaches the tokens around it too. *)
  Cursor.of_string raw_value |> Cursor.remaining |> components_have_substitution

(** Check for and consume [!important] (case-insensitive per CSS Syntax). *)
let read_importance t =
  Cursor.ws t;
  match Cursor.peek_delim t with
  | Some '!' ->
      Cursor.skip t;
      Cursor.ws t;
      let ident = Cursor.ident t in
      if String.lowercase_ascii_preserve ident = "important" then true
      else Cursor.err_invalid t ("invalid !important declaration: !" ^ ident)
  | _ -> false

let is_plain_property_name name =
  let is_start = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | '-' -> true
    | _ -> false
  in
  let is_continue = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
    | _ -> false
  in
  let len = String.length name in
  len > 0
  && is_start name.[0]
  && (not (len = 1 && name.[0] = '-'))
  && (not (len >= 2 && name.[0] = '-' && name.[1] >= '0' && name.[1] <= '9'))
  && String.for_all is_continue name

let validate_printable_property_name t name =
  if not (is_plain_property_name name) then
    Cursor.err_invalid t ("unprintable property name: " ^ name)

let is_vendor_extension_property_name name =
  let len = String.length name in
  let rec has_vendor_separator i =
    i < len
    &&
    if name.[i] = '-' then i > 1 && i < len - 1 else has_vendor_separator (i + 1)
  in
  len > 0 && name.[0] = '-' && has_vendor_separator 1

let validate_unknown_property_name t name =
  validate_printable_property_name t name;
  if
    String.length name > 0
    && name.[0] = '-'
    && not (is_vendor_extension_property_name name)
  then Cursor.err_invalid t ("invalid vendor extension property name: " ^ name)

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
  | Column_rule -> border_uses_color p value
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

(* Helper to validate no extra tokens remain *)
let validate_no_extra_tokens t =
  Cursor.ws t;
  match Cursor.peek_head_shape t with
  | `Eof | `Semicolon | `Bang -> ()
  | _ ->
      (* The trailing tokens are consumed before they are judged, so the span is
         theirs rather than that of the terminator they stop at. *)
      let loc = Cursor.decl_value_loc t in
      let trimmed = Cursor.consume_to_decl_end ~trim:true t in
      if trimmed <> "" then
        Cursor.err_invalid ~loc t
          ("unexpected tokens after property value: " ^ trimmed)

let read_length_box ?(allow_negative = true) t =
  let values =
    Cursor.list ~at_least:1 ~at_most:4
      (fun r -> read_length ~allow_negative r)
      t
  in
  if values = [] then Cursor.err_expected t "length value";
  values

let read_inset_longhand t = [ read_length t ]
let read_inset_axis t = Cursor.list ~at_least:1 ~at_most:2 read_length t

let read_border_width_box t =
  Cursor.list ~at_least:1 ~at_most:4 read_border_width t

let read_text_decoration_lines t =
  let lines = Cursor.list ~at_least:1 read_text_decoration_line t in
  let is_none (line : text_decoration_line) =
    match line with None -> true | _ -> false
  in
  let is_error_kind (line : text_decoration_line) =
    match line with Spelling_error | Grammar_error -> true | _ -> false
  in
  if List.exists is_none lines && List.length lines > 1 then
    Cursor.err_invalid t "none cannot be combined with text-decoration lines";
  (* CSS Text Decoration 4 section 2.1: [<text-decoration-line>] is [none |
     [underline || overline || line-through || blink] | spelling-error |
     grammar-error]. [spelling-error] and [grammar-error] are alternatives to
     the [||] group, not members of it, so they cannot be combined with any
     other line. *)
  if List.exists is_error_kind lines && List.length lines > 1 then
    Cursor.err_invalid t
      "spelling-error and grammar-error cannot be combined with other \
       text-decoration lines";
  let rec duplicates = function
    | [] -> false
    | x :: xs -> List.mem x xs || duplicates xs
  in
  if duplicates lines then Cursor.err_invalid t "duplicate text-decoration-line";
  lines

(* CSS Shapes 1 sec. 5: [<shape-box>] is [<visual-box> | margin-box], so the
   three SVG boxes [<geometry-box>] adds are not valid here. *)
let check_shape_box t (box : clip_geometry_box) =
  match box with
  | Margin_box | Border_box | Padding_box | Content_box -> ()
  | Fill_box | Stroke_box | View_box ->
      Cursor.err_invalid t "shape-outside takes a <shape-box>"

(* [read_clip_path] reads [<basic-shape> || <geometry-box>], the same double-bar
   pair shape-outside uses, along with [none], [url()] and the CSS-wide
   keywords. Shapes 1 sec. 6.1 differs on two points, checked here: the box is a
   [<shape-box>], and everything outside [<basic-shape>] is an alternative to
   the pair rather than a member of it. *)
let check_shape_outside_shape t =
  let is_basic_shape (shape : clip_path) =
    match shape with
    | Clip_path_inset _ | Clip_path_circle _ | Clip_path_ellipse _
    | Clip_path_polygon _ | Clip_path_path _ | Clip_path_shape _
    | Clip_path_xywh _ | Clip_path_rect _ ->
        true
    (* A shape the [clip_path] reader kept verbatim as spec-invalid: the raw
       text survives here too rather than costing the whole declaration. *)
    | Invalid _ -> true
    | _ -> false
  in
  match read_clip_path t with
  | Clip_path_box box -> check_shape_box t box
  | Clip_path_with_box { shape; box; _ } when is_basic_shape shape ->
      check_shape_box t box
  | Clip_path_with_box _ ->
      Cursor.err_invalid t "shape-outside pairs a <shape-box> with a shape"
  | _ -> ()

(* CSS Shapes 1 sec. 6.1: [shape-outside] is [none | [<basic-shape> ||
   <shape-box>] | <image>]. The value is the raw source text ([Shape_outside :
   string property]): typing it would mean a sum of the [clip_path] shapes and
   the whole [background_image] type for [<image>], so the reader validates the
   grammar and hands the text back verbatim. *)
let read_shape_outside t =
  let raw = Cursor.lookahead (Cursor.consume_to_decl_end ~trim:true) t in
  let read_shape_image t =
    ignore (read_background_image t : background_image)
  in
  (match Cursor.peek t with
  | Some
      (Component.Func
         { node = { name; terminated = true; arguments = []; _ }; _ })
    when String.lowercase_ascii_preserve name = "var" ->
      Cursor.err_invalid t "empty var()"
  | Some (Component.Func { node = { name; terminated = true; _ }; _ })
    when String.lowercase_ascii_preserve name = "var" ->
      let _ : string var =
        Values.read_var
          (fun inner -> Cursor.consume_remaining_as_string ~trim:true inner)
          t
      in
      ()
  | _ ->
      Cursor.one_of
        [
          check_shape_outside_shape;
          read_shape_image;
          (fun t -> Cursor.err_invalid t ("invalid shape-outside: " ^ raw));
        ]
        t);
  Cursor.expect_eof t;
  raw

(* Some properties (shape-margin, scroll-padding, padding, etc.) require a
   non-negative length-percentage. Detect a leading [-] number/percentage and
   reject before delegating to the typed reader. *)
let read_non_negative_length_percentage t =
  Values.read_length_percentage ~allow_negative:false ~with_keywords:false t

let read_nn_lp_or_global t =
  Cursor.enum "non-negative length-percentage"
    [
      ("inherit", (Length Inherit : length_percentage));
      ("initial", Length Initial);
      ("unset", Length Unset);
      ("revert", Length Revert);
      ("revert-layer", Length Revert_layer);
    ]
    ~default:read_non_negative_length_percentage t

(* CSS Motion Path 1 sec. 2.2 gives [offset-distance] a plain
   [<length-percentage>], with no range restriction on it. *)
let read_lp_or_global t =
  Cursor.enum "length-percentage"
    [
      ("inherit", (Length Inherit : length_percentage));
      ("initial", Length Initial);
      ("unset", Length Unset);
      ("revert", Length Revert);
      ("revert-layer", Length Revert_layer);
    ]
    ~default:(Values.read_length_percentage ~with_keywords:false)
    t

let read_nn_length_or_global ?(length_only = false) t =
  Cursor.enum "non-negative length"
    [
      ("inherit", (Inherit : length));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~default:
      (read_length ~allow_negative:false ~with_keywords:false ~length_only)
    t

(* CSS Backgrounds 3 (ED) sec. 4.1: each [border-*-radius] corner longhand is
   [<length-percentage [0,inf]>{1,2}], the horizontal radius then the vertical
   one; a single value sets both. *)
let read_corner_radius t =
  Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2 read_nn_length_or_global t

let read_gap_longhand t =
  Cursor.enum "gap"
    [ ("normal", (Normal : length)) ]
    ~default:read_nn_length_or_global t

(* CSS Animations 2 sec. 4.1: [animation-duration] is [ auto | <time [0s,inf]>
   ]#. The transition durations and both delays keep the time grammar alone. *)
let read_animation_duration t =
  Cursor.enum "animation-duration"
    [ ("auto", (Auto : duration)) ]
    ~default:read_duration t

(* CSS Text 3 section 7: [letter-spacing] is [normal | <length>], [word-spacing]
   is [normal | <length-percentage>]; both accept negative values. *)
let read_normal_or_length name t =
  Cursor.enum name
    [
      ("normal", (Normal : length));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~default:(Values.read_length ~with_keywords:false)
    t

(* CSS Transforms 2 sec. 3: [perspective] is [none | <length [0,inf]>]; the
   keyword is its initial value, so it has to read. *)
let read_perspective_value t =
  Cursor.enum "perspective"
    [
      ("none", (None : length));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    (* CSS Transforms 2 (ED) sec. 3: [perspective] is [none | <length [0,inf]>],
       so it takes no percentage; Chrome 146 refuses one. *)
    ~default:(read_non_negative_length ~with_keywords:false ~length_only:true)
    t

(* CSS Text Decoration 4 sec. 2.8: [text-underline-offset] is [auto |
   <length-percentage>], and unlike the lengths above it may be negative. *)
let read_underline_offset t =
  Cursor.enum "text-underline-offset"
    [
      ("auto", (Auto : length));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~default:(Values.read_length ~with_keywords:false)
    t

let read_letter_spacing t = read_normal_or_length "letter-spacing" t
let read_word_spacing t = read_normal_or_length "word-spacing" t

(* CSS Logical Properties 1 3.5: [padding-block] / [padding-inline] are 2-value
   shorthands. A bare CSS-wide keyword counts as a single value (not a list
   element). *)
let read_padding_logical_shorthand t =
  let read_global_singleton t =
    let kw : length =
      Cursor.enum "padding logical"
        [
          ("inherit", (Inherit : length));
          ("initial", Initial);
          ("unset", Unset);
          ("revert", Revert);
          ("revert-layer", Revert_layer);
        ]
        t
    in
    [ kw ]
  in
  let read_lengths t =
    Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
      (Values.read_length ~allow_negative:false ~with_keywords:false)
      t
  in
  Cursor.one_of [ read_global_singleton; read_lengths ] t

(* CSS Scroll Snap 1 sec. 5.1: the [scroll-margin] longhands are [<length>] - an
   unrestricted range, so an outset may be negative just as a margin may. Only
   [scroll-padding] (sec. 4.2) says "Negative values are invalid". "Percentages:
   n/a" still rules a percentage out. *)
let read_scroll_margin_length t =
  Cursor.enum "scroll-margin length"
    [
      ("inherit", (Inherit : length));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~default:(fun t ->
      match read_length ~with_keywords:false t with
      | Pct _ -> Cursor.err_invalid t "scroll-margin percentage"
      | length -> length)
    t

let read_scroll_padding_length t =
  Cursor.enum "scroll-padding length"
    [
      ("auto", (Auto : length));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~default:(read_non_negative_length ~with_keywords:false)
    t

let rec read_length_or_css_wide t =
  Cursor.enum_or_calls "length"
    [
      ("inherit", (Inherit : length));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (read_var read_length_or_css_wide t)) ]
    ~default:(read_length ~with_keywords:false ~length_only:true)
    t

let rec read_text_decoration_thickness t =
  Cursor.enum_or_calls "text-decoration-thickness"
    [
      ("auto", (Auto : length));
      ("from-font", From_font);
      ("hairline", Hairline);
      ("thin", Thin);
      ("medium", Medium);
      ("thick", Thick);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:
      [
        ("var", fun t -> Var (read_var read_text_decoration_thickness t));
        ( "calc",
          fun t ->
            Calc
              (read_calc ~result_type:`Value
                 (read_length ~with_keywords:false)
                 t) );
      ]
    ~default:(read_length ~with_keywords:false)
    t

(* Delegate to the proper reader in Properties *)
let read_translate_value t : Properties_intf.translate_value =
  Properties.read_translate_value t

let read_transform_value t = v Transform (read_transforms t)
let read_webkit_transform_value t = v Webkit_transform (read_transforms t)

let rec justify_self_of_align_self (a : align_self) : justify_self =
  match a with
  | Auto -> Auto
  | Normal -> Normal
  | Stretch -> Stretch
  | Baseline -> Baseline
  | First_baseline -> First_baseline
  | Last_baseline -> Last_baseline
  | Center -> Center
  | Start -> Start
  | End -> End
  | Self_start -> Self_start
  | Self_end -> Self_end
  | Flex_start -> Flex_start
  | Flex_end -> Flex_end
  | Safe_center -> Safe_center
  | Safe_start -> Safe_start
  | Safe_end -> Safe_end
  | Safe_flex_start -> Safe_flex_start
  | Safe_flex_end -> Safe_flex_end
  | Unsafe_center -> Unsafe_center
  | Unsafe_start -> Unsafe_start
  | Unsafe_end -> Unsafe_end
  | Unsafe_self_start -> Unsafe_self_start
  | Unsafe_self_end -> Unsafe_self_end
  | Unsafe_flex_start -> Unsafe_flex_start
  | Unsafe_flex_end -> Unsafe_flex_end
  | Inherit -> Inherit
  | Initial -> Initial
  | Unset -> Unset
  | Revert -> Revert
  | Revert_layer -> Revert_layer
  | Var v -> Var (justify_var_of_align_var v)

and justify_var_of_align_var (v : align_self var) : justify_self var =
  let fallback : justify_self fallback =
    match v.fallback with
    | None -> None
    | Empty -> Empty
    | Empty2 -> Empty2
    | Var_fallback name -> Var_fallback name
    | Syntax_fallback value -> Syntax_fallback value
    | Fallback value -> Fallback (justify_self_of_align_self value)
  in
  {
    name = v.name;
    fallback;
    default = Option.map justify_self_of_align_self v.default;
    layer = v.layer;
    meta = v.meta;
    runtime = v.runtime;
  }

let read_place_self_value t =
  let a = read_align_self t in
  Cursor.ws t;
  let j = Cursor.option read_justify_self t in
  (* Per CSS spec, when only one value is given, both values are set to it. *)
  let pair =
    match j with None -> (a, justify_self_of_align_self a) | Some jj -> (a, jj)
  in
  v Place_self pair

let read_background_blend_mode_value t =
  v Background_blend_mode (Cursor.list ~sep:Cursor.comma read_blend_mode t)

(* The spelling a property parses back from, and the one a diagnostic names: a
   minified [page-break-*] is the CSS Fragmentation 3 sec. 3.4 [break-*] alias
   of a different property, which reports the failure against a property the
   declaration never wrote. *)
let canonical_prop_name (type a) (prop : a property) =
  Pp.to_string ~minify:false pp_property prop

type value_reader = {
  read_value_opt : 'a. 'a property -> Cursor.t -> declaration option;
}

let read_color_value : type a. a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Color -> Some (v Color (read_color t))
  | Background_color -> Some (v Background_color (read_color t))
  | Border_color ->
      (* CSS Backgrounds 3 sec. 3.1: [border-color] is a 1-4 value box shorthand
         (top / right / bottom / left). *)
      Some (v Border_color (Cursor.list ~at_least:1 ~at_most:4 read_color t))
  | Outline_color -> Some (v Outline_color (Prop_common.read_auto_color t))
  | Border_top_color -> Some (v Border_top_color (read_color t))
  | Border_right_color -> Some (v Border_right_color (read_color t))
  | Border_bottom_color -> Some (v Border_bottom_color (read_color t))
  | Border_left_color -> Some (v Border_left_color (read_color t))
  | Text_decoration_color -> Some (v Text_decoration_color (read_color t))
  | Text_emphasis_color -> Some (v Text_emphasis_color (read_color t))
  | Accent_color -> Some (v Accent_color (Prop_common.read_auto_color t))
  | Caret_color -> Some (v Caret_color (Prop_common.read_auto_color t))
  | Stop_color -> Some (v Stop_color (read_color t))
  | Flood_color -> Some (v Flood_color (read_color t))
  | Lighting_color -> Some (v Lighting_color (read_color t))
  | Webkit_tap_highlight_color ->
      Some (v Webkit_tap_highlight_color (read_color t))
  | Webkit_text_decoration_color ->
      Some (v Webkit_text_decoration_color (read_color t))
  | _ -> None

(* CSS Sizing 3 sec. 5: the box sizes are the properties the intrinsic sizes
   belong to, so this reader asks for them. *)
let read_box_size ?(maximum = false) t =
  match read_length_percentage ~allow_negative:false ~sizing:true t with
  | Length (Normal | From_font | Size) ->
      Cursor.err_invalid t "expected a box size"
  | Length Auto when maximum ->
      Cursor.err_invalid t "maximum sizes do not accept auto"
  | Length None when not maximum ->
      Cursor.err_invalid t "only maximum sizes accept none"
  | size -> size

let read_sizing_value : type a. a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Width -> Some (v Width (read_box_size t))
  | Height -> Some (v Height (read_box_size t))
  | Min_width -> Some (v Min_width (read_box_size t))
  | Min_height -> Some (v Min_height (read_box_size t))
  | Max_width -> Some (v Max_width (read_box_size ~maximum:true t))
  | Max_height -> Some (v Max_height (read_box_size ~maximum:true t))
  | Inline_size -> Some (v Inline_size (read_box_size t))
  | Min_inline_size -> Some (v Min_inline_size (read_box_size t))
  | Max_inline_size -> Some (v Max_inline_size (read_box_size ~maximum:true t))
  | Block_size -> Some (v Block_size (read_box_size t))
  | Min_block_size -> Some (v Min_block_size (read_box_size t))
  | Max_block_size -> Some (v Max_block_size (read_box_size ~maximum:true t))
  | Font_size -> Some (v Font_size (Properties.read_font_size t))
  | Perspective -> Some (v Perspective (read_perspective_value t))
  | Offset_distance -> Some (v Offset_distance (read_lp_or_global t))
  | Shape_margin -> Some (v Shape_margin (read_nn_lp_or_global t))
  | Line_height_step ->
      Some (v Line_height_step (read_nn_length_or_global ~length_only:true t))
  | _ -> None

let read_radius_gap_value : type a. a property -> Cursor.t -> declaration option
    =
 fun prop t ->
  match prop with
  | Border_radius -> Some (v Border_radius (read_border_radius t))
  | Border_top_left_radius ->
      Some (v Border_top_left_radius (read_corner_radius t))
  | Border_top_right_radius ->
      Some (v Border_top_right_radius (read_corner_radius t))
  | Border_bottom_left_radius ->
      Some (v Border_bottom_left_radius (read_corner_radius t))
  | Border_bottom_right_radius ->
      Some (v Border_bottom_right_radius (read_corner_radius t))
  | Border_start_start_radius ->
      Some (v Border_start_start_radius (read_corner_radius t))
  | Border_start_end_radius ->
      Some (v Border_start_end_radius (read_corner_radius t))
  | Border_end_start_radius ->
      Some (v Border_end_start_radius (read_corner_radius t))
  | Border_end_end_radius ->
      Some (v Border_end_end_radius (read_corner_radius t))
  | Gap -> Some (v Gap (Properties.read_gap t))
  | Column_gap -> Some (v Column_gap (read_gap_longhand t))
  | Row_gap -> Some (v Row_gap (read_gap_longhand t))
  | _ -> None

let read_layout_value : type a. a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Display -> Some (v Display (read_display t))
  | Position -> Some (v Position (read_position t))
  | Visibility -> Some (v Visibility (read_visibility t))
  | Baseline_source -> Some (v Baseline_source (read_baseline_source t))
  | Alignment_baseline ->
      Some (v Alignment_baseline (read_alignment_baseline t))
  | Baseline_shift -> Some (v Baseline_shift (read_baseline_shift t))
  | Overflow -> Some (v Overflow (read_overflow t))
  | Overflow_x -> Some (v Overflow_x (read_overflow_single t))
  | Overflow_y -> Some (v Overflow_y (read_overflow_single t))
  | Overflow_block -> Some (v Overflow_block (read_overflow_single t))
  | Overflow_inline -> Some (v Overflow_inline (read_overflow_single t))
  | Content_visibility ->
      Some (v Content_visibility (read_content_visibility t))
  | Aspect_ratio -> Some (v Aspect_ratio (read_aspect_ratio t))
  | Vertical_align -> Some (v Vertical_align (read_vertical_align t))
  | _ -> None

let read_box_edge_value : type a. a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Padding -> Some (v Padding (read_padding_shorthand t))
  | Margin -> Some (v Margin (read_margin_shorthand t))
  | Border_style -> Some (v Border_style (read_border_style_box t))
  | Border_width -> Some (v Border_width (read_border_width_box t))
  | Border_top_width -> Some (v Border_top_width (read_border_width t))
  | Border_right_width -> Some (v Border_right_width (read_border_width t))
  | Border_bottom_width -> Some (v Border_bottom_width (read_border_width t))
  | Border_left_width -> Some (v Border_left_width (read_border_width t))
  | Border_top_style -> Some (v Border_top_style (read_border_style t))
  | Border_right_style -> Some (v Border_right_style (read_border_style t))
  | Border_bottom_style -> Some (v Border_bottom_style (read_border_style t))
  | Border_left_style -> Some (v Border_left_style (read_border_style t))
  | Border_inline_start_width ->
      Some (v Border_inline_start_width (read_border_width t))
  | Border_inline_end_width ->
      Some (v Border_inline_end_width (read_border_width t))
  | Border_block_start_width ->
      Some (v Border_block_start_width (read_border_width t))
  | Border_block_end_width ->
      Some (v Border_block_end_width (read_border_width t))
  | Border_inline_start_color ->
      Some (v Border_inline_start_color (read_color t))
  | Border_inline_end_color -> Some (v Border_inline_end_color (read_color t))
  | Border_block_start_color -> Some (v Border_block_start_color (read_color t))
  | Border_block_end_color -> Some (v Border_block_end_color (read_color t))
  | Border_inline_color ->
      Some (v Border_inline_color (read_logical_border_color t))
  | Border_block_color ->
      Some (v Border_block_color (read_logical_border_color t))
  | Border_inline_width ->
      Some (v Border_inline_width (read_logical_border_width t))
  | Border_block_width ->
      Some (v Border_block_width (read_logical_border_width t))
  | Border_inline_style ->
      Some (v Border_inline_style (read_logical_border_style t))
  | Border_block_style ->
      Some (v Border_block_style (read_logical_border_style t))
  | Border_inline_start_style ->
      Some (v Border_inline_start_style (read_border_style t))
  | Border_inline_end_style ->
      Some (v Border_inline_end_style (read_border_style t))
  | Border_block_start_style ->
      Some (v Border_block_start_style (read_border_style t))
  | Border_block_end_style ->
      Some (v Border_block_end_style (read_border_style t))
  | _ -> None

let read_type_value : type a. a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Line_height -> Some (v Line_height (read_line_height t))
  | Font_weight -> Some (v Font_weight (read_font_weight t))
  | Font_style -> Some (v Font_style (read_font_style t))
  | Font_family -> Some (v Font_family (read_font_family t))
  | Font -> Some (v Font (Properties.read_font t))
  | Source -> Some (v Source (Properties.read_font_src t))
  | Text_align -> Some (v Text_align (read_text_align t))
  | Text_transform -> Some (v Text_transform (read_text_transform t))
  | White_space -> Some (v White_space (read_white_space t))
  | White_space_collapse ->
      Some (v White_space_collapse (read_white_space_collapse t))
  | Font_variant_alternates ->
      Some (v Font_variant_alternates (read_font_variant_alternates t))
  | Font_variant -> Some (v Font_variant (read_font_variant t))
  | Text_decoration -> Some (v Text_decoration (read_text_decoration t))
  | Text_decoration_line ->
      Some (v Text_decoration_line (read_text_decoration_lines t))
  | Text_decoration_style ->
      Some (v Text_decoration_style (read_text_decoration_style t))
  | Text_underline_offset ->
      Some (v Text_underline_offset (read_underline_offset t))
  | Text_emphasis -> Some (v Text_emphasis (read_text_emphasis t))
  | Text_emphasis_style ->
      Some (v Text_emphasis_style (read_text_emphasis_style t))
  | Text_emphasis_position ->
      Some (v Text_emphasis_position (read_text_emphasis_position t))
  | Text_emphasis_skip ->
      Some (v Text_emphasis_skip (read_text_emphasis_skip t))
  | Text_orientation -> Some (v Text_orientation (read_text_orientation t))
  | Letter_spacing -> Some (v Letter_spacing (read_letter_spacing t))
  | Transform_origin -> Some (v Transform_origin (read_transform_origin t))
  | Transform_box -> Some (v Transform_box (read_transform_box t))
  | _ -> None

let read_flex_effect_value : type a.
    a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Flex_direction -> Some (v Flex_direction (read_flex_direction t))
  | Flex_wrap -> Some (v Flex_wrap (read_flex_wrap t))
  | Flex_flow -> Some (v Flex_flow (read_flex_flow t))
  | Flex -> Some (v Flex (read_flex t))
  | Flex_grow -> Some (v Flex_grow (Properties.read_flex_factor t))
  | Flex_shrink -> Some (v Flex_shrink (Properties.read_flex_factor t))
  | Flex_basis -> Some (v Flex_basis (read_flex_basis t))
  | Align_items -> Some (v Align_items (read_align_items t))
  | Justify_content -> Some (v Justify_content (read_justify_content t))
  | Transform -> Some (read_transform_value t)
  | Translate -> Some (v Translate (read_translate_value t))
  | Webkit_transform -> Some (read_webkit_transform_value t)
  | Moz_transform -> Some (v Moz_transform (read_transforms t))
  | Ms_transform -> Some (v Ms_transform (read_transforms t))
  | O_transform -> Some (v O_transform (read_transforms t))
  | Webkit_transition -> Some (v Webkit_transition (read_transitions t))
  | Webkit_animation -> Some (v Webkit_animation (read_animations t))
  | Webkit_filter -> Some (v Webkit_filter (read_filter t))
  | Moz_appearance -> Some (v Moz_appearance (read_appearance t))
  | Ms_filter -> Some (v Ms_filter (read_filter t))
  | O_transition -> Some (v O_transition (read_transitions t))
  | Filter -> Some (v Filter (read_filter t))
  | Appearance -> Some (v Appearance (read_appearance t))
  | Color_scheme -> Some (v Color_scheme (read_color_scheme t))
  | _ -> None

let read_vendor_alias_value : type a.
    a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Webkit_transition_delay ->
      Some (v Webkit_transition_delay (read_duration_list read_time t))
  | Webkit_transition_duration ->
      Some (v Webkit_transition_duration (read_duration_list read_duration t))
  | Webkit_transition_property ->
      Some (v Webkit_transition_property (read_transition_property t))
  | Webkit_transition_timing_function ->
      Some (v Webkit_transition_timing_function (read_timing_function_list t))
  | Webkit_animation_delay ->
      Some (v Webkit_animation_delay (read_duration_list read_time t))
  | Webkit_animation_duration ->
      Some
        (v Webkit_animation_duration
           (read_duration_list read_animation_duration t))
  | Webkit_animation_direction ->
      Some (v Webkit_animation_direction (read_animation_direction t))
  | Webkit_animation_iteration_count ->
      Some
        (v Webkit_animation_iteration_count (read_animation_iteration_count t))
  | Webkit_animation_name ->
      Some (v Webkit_animation_name (read_animation_name t))
  | Webkit_animation_timing_function ->
      Some (v Webkit_animation_timing_function (read_timing_function_list t))
  | Webkit_animation_fill_mode ->
      Some (v Webkit_animation_fill_mode (read_animation_fill_mode t))
  | Webkit_animation_play_state ->
      Some (v Webkit_animation_play_state (read_animation_play_state t))
  | Webkit_flex_direction ->
      Some (v Webkit_flex_direction (read_flex_direction t))
  | Webkit_flex_wrap -> Some (v Webkit_flex_wrap (read_flex_wrap t))
  | Webkit_flex_flow -> Some (v Webkit_flex_flow (read_flex_flow t))
  | Webkit_justify_content ->
      Some (v Webkit_justify_content (read_justify_content t))
  | Webkit_align_content -> Some (v Webkit_align_content (read_align_content t))
  | Webkit_align_items -> Some (v Webkit_align_items (read_align_items t))
  | Webkit_align_self -> Some (v Webkit_align_self (read_align_self t))
  | Webkit_border_radius -> Some (v Webkit_border_radius (read_border_radius t))
  | Webkit_box_shadow -> Some (v Webkit_box_shadow (read_shadow t))
  | Webkit_background_size ->
      Some (v Webkit_background_size (read_background_size_list t))
  | Moz_animation -> Some (v Moz_animation (read_animations t))
  | Moz_animation_delay ->
      Some (v Moz_animation_delay (read_duration_list read_time t))
  | Moz_animation_duration ->
      Some
        (v Moz_animation_duration
           (read_duration_list read_animation_duration t))
  | Moz_animation_direction ->
      Some (v Moz_animation_direction (read_animation_direction t))
  | Moz_animation_iteration_count ->
      Some (v Moz_animation_iteration_count (read_animation_iteration_count t))
  | Moz_animation_name -> Some (v Moz_animation_name (read_animation_name t))
  | Moz_animation_timing_function ->
      Some (v Moz_animation_timing_function (read_timing_function_list t))
  | Moz_animation_fill_mode ->
      Some (v Moz_animation_fill_mode (read_animation_fill_mode t))
  | Moz_animation_play_state ->
      Some (v Moz_animation_play_state (read_animation_play_state t))
  | Moz_transition -> Some (v Moz_transition (read_transitions t))
  | Moz_transition_delay ->
      Some (v Moz_transition_delay (read_duration_list read_time t))
  | Moz_transition_duration ->
      Some (v Moz_transition_duration (read_duration_list read_duration t))
  | Moz_transition_property ->
      Some (v Moz_transition_property (read_transition_property t))
  | Moz_transition_timing_function ->
      Some (v Moz_transition_timing_function (read_timing_function_list t))
  | Moz_border_radius -> Some (v Moz_border_radius (read_border_radius t))
  | Moz_box_shadow -> Some (v Moz_box_shadow (read_shadow t))
  | _ -> None

let read_background_value : type a. a property -> Cursor.t -> declaration option
    =
 fun prop t ->
  match prop with
  | Background_image ->
      let images = read_background_images t in
      Some (v Background_image images)
  | Background -> Some (v Background (read_backgrounds t))
  | Border -> Some (v Border (read_border t))
  | Background_attachment ->
      Some (v Background_attachment (read_background_attachment t))
  | Background_origin -> Some (v Background_origin (read_background_box_list t))
  | Background_clip -> Some (v Background_clip (read_background_box_list t))
  | Webkit_background_clip ->
      Some (v Webkit_background_clip (read_background_box_list t))
  | Background_position ->
      Some (v Background_position (read_background_position t))
  | Background_position_x ->
      Some (v Background_position_x (read_background_position_x t))
  | Background_position_y ->
      Some (v Background_position_y (read_background_position_y t))
  | Webkit_mask_position_x ->
      Some (v Webkit_mask_position_x (read_background_position_x t))
  | Webkit_mask_position_y ->
      Some (v Webkit_mask_position_y (read_background_position_y t))
  | Background_repeat ->
      Some (v Background_repeat (read_background_repeat_list t))
  | Background_size -> Some (v Background_size (read_background_size_list t))
  | Background_blend_mode -> Some (read_background_blend_mode_value t)
  | _ -> None

(* CSS Grid 2 sec. 7.4 spells [grid-template] as [none], a
   [<'grid-template-rows'> / <'grid-template-columns'>] pair, or a track list
   built from strings. A bare track list is a [grid-template-rows] value and no
   [grid-template] one, and sec. 7.8 gives [grid] the same forms plus the two
   [auto-flow] ones. *)
let names_both_axes : Properties.grid_template -> bool = function
  | None | Inherit | Initial | Unset | Revert | Revert_layer | Var _ | Split _
  | Auto_flow_columns _ | Auto_flow_rows _ | Named_tracks _ | Template _ ->
      true
  | Px _ | Rem _ | Em _ | Pct _ | Vw _ | Vh _ | Vmin _ | Vmax _ | Zero
  | Length _ | Fr _ | Flex_math _ | Auto | Min_content | Max_content | Min_max _
  | Fit_content _ | Repeat _ | Tracks _ | Line_names _ | Subgrid | Masonry ->
      false

let read_grid_template_shorthand ~name t =
  let value =
    if String.equal name "grid" then read_grid t else read_grid_template t
  in
  if not (names_both_axes value) then
    Cursor.err_invalid t (name ^ " names one axis only");
  value

let read_grid_value : type a. a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Grid_template_columns ->
      Some (v Grid_template_columns (read_grid_template_tracks t))
  | Grid_template_rows ->
      Some (v Grid_template_rows (read_grid_template_tracks t))
  | Grid_row_start -> Some (v Grid_row_start (read_grid_line t))
  | Grid_row_end -> Some (v Grid_row_end (read_grid_line t))
  | Grid_column_start -> Some (v Grid_column_start (read_grid_line t))
  | Grid_column_end -> Some (v Grid_column_end (read_grid_line t))
  | Grid_auto_flow -> Some (v Grid_auto_flow (read_grid_auto_flow t))
  | Grid_template_areas ->
      Some (v Grid_template_areas (read_grid_template_areas t))
  | Grid_template ->
      Some
        (v Grid_template (read_grid_template_shorthand ~name:"grid-template" t))
  | Grid -> Some (v Grid (read_grid_template_shorthand ~name:"grid" t))
  | Grid_area -> Some (v Grid_area (read_grid_area t))
  | Grid_auto_columns -> Some (v Grid_auto_columns (read_grid_auto_tracks t))
  | Grid_auto_rows -> Some (v Grid_auto_rows (read_grid_auto_tracks t))
  | Grid_column -> Some (v Grid_column (read_grid_line_pair t))
  | Grid_row -> Some (v Grid_row (read_grid_line_pair t))
  | _ -> None

let read_shadow_content_value : type a.
    a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Box_shadow -> Some (v Box_shadow (read_shadow t))
  | Text_shadow -> Some (v Text_shadow (read_text_shadows t))
  | Content -> Some (v Content (read_content t))
  | Counter_reset -> Some (v Counter_reset (read_counter_set t))
  | Counter_increment -> Some (v Counter_increment (read_counter_set t))
  | Z_index -> Some (v Z_index (Properties.read_z_index t))
  | Opacity -> Some (v Opacity (Properties.read_opacity t))
  | Fill_opacity -> Some (v Fill_opacity (Properties.read_opacity t))
  | Stroke_opacity -> Some (v Stroke_opacity (Properties.read_opacity t))
  | Stop_opacity -> Some (v Stop_opacity (Properties.read_opacity t))
  | Flood_opacity -> Some (v Flood_opacity (Properties.read_opacity t))
  | Cursor -> Some (v Cursor (read_cursor t))
  | Interactivity -> Some (v Interactivity (read_interactivity t))
  | Caret_animation -> Some (v Caret_animation (read_caret_animation t))
  | Caret_shape -> Some (v Caret_shape (read_caret_shape t))
  | Caret -> Some (v Caret (read_caret t))
  | Interest_delay -> Some (v Interest_delay (read_interest_delay t))
  | Interest_delay_start ->
      Some (v Interest_delay_start (read_interest_delay ~longhand:true t))
  | Interest_delay_end ->
      Some (v Interest_delay_end (read_interest_delay ~longhand:true t))
  | Nav_up -> Some (v Nav_up (read_nav t))
  | Nav_right -> Some (v Nav_right (read_nav t))
  | Nav_down -> Some (v Nav_down (read_nav t))
  | Nav_left -> Some (v Nav_left (read_nav t))
  | Box_sizing -> Some (v Box_sizing (read_box_sizing t))
  | Webkit_box_sizing -> Some (v Webkit_box_sizing (read_box_sizing t))
  | Moz_box_sizing -> Some (v Moz_box_sizing (read_box_sizing t))
  | Field_sizing -> Some (v Field_sizing (read_field_sizing t))
  | Caption_side -> Some (v Caption_side (read_caption_side t))
  | _ -> None

let read_spacing_value : type a. a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Padding_left -> Some (v Padding_left (read_nn_length_or_global t))
  | Padding_right -> Some (v Padding_right (read_nn_length_or_global t))
  | Padding_top -> Some (v Padding_top (read_nn_length_or_global t))
  | Padding_bottom -> Some (v Padding_bottom (read_nn_length_or_global t))
  | Padding_inline -> Some (v Padding_inline (read_padding_logical_shorthand t))
  | Padding_inline_start ->
      Some (v Padding_inline_start (read_nn_length_or_global t))
  | Padding_inline_end ->
      Some (v Padding_inline_end (read_nn_length_or_global t))
  | Padding_block -> Some (v Padding_block (read_padding_logical_shorthand t))
  | Padding_block_start ->
      Some (v Padding_block_start (read_nn_length_or_global t))
  | Padding_block_end -> Some (v Padding_block_end (read_nn_length_or_global t))
  | Margin_left -> Some (v Margin_left (read_length t))
  | Margin_right -> Some (v Margin_right (read_length t))
  | Margin_top -> Some (v Margin_top (read_length t))
  | Margin_bottom -> Some (v Margin_bottom (read_length t))
  | Margin_inline ->
      Some
        (v Margin_inline
           (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2 read_length t))
  | Margin_inline_start -> Some (v Margin_inline_start (read_length t))
  | Margin_inline_end -> Some (v Margin_inline_end (read_length t))
  | Margin_block ->
      Some
        (v Margin_block
           (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2 read_length t))
  | Margin_block_start -> Some (v Margin_block_start (read_length t))
  | Margin_block_end -> Some (v Margin_block_end (read_length t))
  | _ -> None

let read_list_align_value : type a. a property -> Cursor.t -> declaration option
    =
 fun prop t ->
  match prop with
  | List_style_type -> Some (v List_style_type (read_list_style_type t))
  | List_style_position ->
      Some (v List_style_position (read_list_style_position t))
  | List_style_image -> Some (v List_style_image (read_list_style_image t))
  | List_style -> Some (v List_style (Properties.read_list_style t))
  | Order -> Some (v Order (Properties.read_order t))
  | Justify_items -> Some (v Justify_items (read_justify_items t))
  | Justify_self -> Some (v Justify_self (read_justify_self t))
  | Align_content -> Some (v Align_content (read_align_content t))
  | Align_self -> Some (v Align_self (read_align_self t))
  | Place_content -> Some (v Place_content (read_place_content t))
  | Place_items -> Some (v Place_items (read_place_items t))
  | Place_self -> Some (read_place_self_value t)
  | _ -> None

let read_position_value_prop : type a.
    a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Inset -> Some (v Inset (read_length_box t))
  | Inset_inline -> Some (v Inset_inline (read_inset_axis t))
  | Inset_inline_start -> Some (v Inset_inline_start (read_inset_longhand t))
  | Inset_inline_end -> Some (v Inset_inline_end (read_inset_longhand t))
  | Inset_block -> Some (v Inset_block (read_inset_axis t))
  | Inset_block_start -> Some (v Inset_block_start (read_inset_longhand t))
  | Inset_block_end -> Some (v Inset_block_end (read_inset_longhand t))
  | Top -> Some (v Top (read_inset_longhand t))
  | Right -> Some (v Right (read_inset_longhand t))
  | Bottom -> Some (v Bottom (read_inset_longhand t))
  | Left -> Some (v Left (read_inset_longhand t))
  | Outline -> Some (v Outline (read_outline t))
  | Outline_style -> Some (v Outline_style (read_outline_style t))
  | Outline_width -> Some (v Outline_width (read_border_width t))
  | Outline_offset -> Some (v Outline_offset (read_length_or_css_wide t))
  | Forced_color_adjust ->
      Some (v Forced_color_adjust (read_forced_color_adjust t))
  | Scroll_snap_type -> Some (v Scroll_snap_type (read_scroll_snap_type t))
  | Tab_size -> Some (v Tab_size (read_tab_size t))
  | Zoom -> Some (v Zoom (read_zoom t))
  | _ -> None

let read_vendor_font_value : type a.
    a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Webkit_text_size_adjust ->
      Some (v Webkit_text_size_adjust (read_text_size_adjust t))
  | Webkit_text_decoration ->
      Some (v Webkit_text_decoration (read_text_decoration t))
  | Webkit_appearance -> Some (v Webkit_appearance (read_webkit_appearance t))
  | Webkit_font_smoothing ->
      Some (v Webkit_font_smoothing (read_webkit_font_smoothing t))
  | Webkit_line_clamp -> Some (v Webkit_line_clamp (read_webkit_line_clamp t))
  | Webkit_box_orient -> Some (v Webkit_box_orient (read_webkit_box_orient t))
  | Moz_orient -> Some (v Moz_orient (read_moz_orient t))
  | Webkit_hyphens -> Some (v Webkit_hyphens (read_hyphens t))
  | Font_feature_settings ->
      Some (v Font_feature_settings (read_font_feature_settings t))
  | Font_variation_settings ->
      Some (v Font_variation_settings (read_font_variation_settings t))
  | Font_stretch -> Some (v Font_stretch (read_font_stretch t))
  | Font_optical_sizing ->
      Some (v Font_optical_sizing (read_font_optical_sizing t))
  | Font_kerning -> Some (v Font_kerning (read_font_kerning t))
  | Font_language_override ->
      Some (v Font_language_override (read_font_language_override t))
  | Font_synthesis_style ->
      Some (v Font_synthesis_style (read_font_synthesis_style t))
  | Font_synthesis_weight ->
      Some (v Font_synthesis_weight (read_font_synthesis_weight t))
  | Font_synthesis_small_caps ->
      Some (v Font_synthesis_small_caps (read_font_synthesis_small_caps t))
  | Font_synthesis_position ->
      Some (v Font_synthesis_position (read_font_synthesis_position t))
  | Font_variant_ligatures ->
      Some (v Font_variant_ligatures (read_font_variant_ligatures t))
  | Caps -> Some (v Caps (read_font_variant_caps t))
  | Numeric -> Some (v Numeric (read_font_variant_numeric t))
  | Font_variant_position ->
      Some (v Font_variant_position (read_font_variant_position t))
  | East_asian -> Some (v East_asian (read_font_variant_east_asian t))
  | _ -> None

let read_text_flow_value : type a. a property -> Cursor.t -> declaration option
    =
 fun prop t ->
  match prop with
  | Text_indent -> Some (v Text_indent (read_text_indent_value t))
  | Text_overflow -> Some (v Text_overflow (read_text_overflow t))
  | Text_wrap -> Some (v Text_wrap (read_text_wrap t))
  | Text_decoration_thickness ->
      Some (v Text_decoration_thickness (read_text_decoration_thickness t))
  | Text_size_adjust -> Some (v Text_size_adjust (read_text_size_adjust t))
  | Text_decoration_skip_ink ->
      Some (v Text_decoration_skip_ink (read_text_decoration_skip_ink t))
  | Text_decoration_skip ->
      Some (v Text_decoration_skip (read_text_decoration_skip t))
  | Text_decoration_skip_self ->
      Some (v Text_decoration_skip_self (read_text_decoration_skip_self t))
  | Text_decoration_skip_box ->
      Some (v Text_decoration_skip_box (read_text_decoration_skip_box t))
  | Text_decoration_skip_inset ->
      Some (v Text_decoration_skip_inset (read_text_decoration_skip_inset t))
  | Text_decoration_skip_spaces ->
      Some (v Text_decoration_skip_spaces (read_text_decoration_skip_spaces t))
  | Word_break -> Some (v Word_break (read_word_break t))
  | Overflow_wrap -> Some (v Overflow_wrap (read_overflow_wrap t))
  | Line_break -> Some (v Line_break (read_line_break t))
  | Hyphens -> Some (v Hyphens (read_hyphens t))
  | Word_spacing -> Some (v Word_spacing (read_word_spacing t))
  | _ -> None

let read_modern_layout_value : type a.
    a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Container_type -> Some (v Container_type (read_container_type t))
  | Container_name -> Some (v Container_name (read_container_name t))
  | Container -> Some (v Container (read_container_shorthand t))
  | Anchor_name -> Some (v Anchor_name (read_anchor_name t))
  | Position_anchor -> Some (v Position_anchor (read_position_anchor t))
  | Position_try_fallbacks ->
      Some (v Position_try_fallbacks (Properties.read_position_try_fallbacks t))
  | Position_try_order ->
      Some (v Position_try_order (read_position_try_order t))
  | Position_try -> Some (v Position_try (read_position_try t))
  | Position_visibility ->
      Some (v Position_visibility (read_position_visibility t))
  | Position_area -> Some (v Position_area (read_position_area t))
  | Shape_outside -> Some (v Shape_outside (read_shape_outside t))
  | Shape_image_threshold ->
      Some (v Shape_image_threshold (read_shape_image_threshold t))
  | Overflow_clip_margin ->
      Some (v Overflow_clip_margin (read_overflow_clip_margin t))
  | Overflow_anchor -> Some (v Overflow_anchor (read_overflow_anchor t))
  | Scrollbar_width -> Some (v Scrollbar_width (read_scrollbar_width t))
  | Scrollbar_color -> Some (v Scrollbar_color (read_scrollbar_color t))
  | Scrollbar_gutter -> Some (v Scrollbar_gutter (read_scrollbar_gutter t))
  | Font_palette -> Some (v Font_palette (read_font_palette t))
  | Font_synthesis -> Some (v Font_synthesis (read_font_synthesis t))
  | _ -> None

let read_inline_text_value : type a.
    a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Text_wrap_mode -> Some (v Text_wrap_mode (read_text_wrap_mode t))
  | Text_wrap_style -> Some (v Text_wrap_style (read_text_wrap_style t))
  | Text_box_trim -> Some (v Text_box_trim (read_text_box_trim t))
  | Text_underline_position ->
      Some (v Text_underline_position (read_text_underline_position t))
  | Text_box_edge -> Some (v Text_box_edge (read_text_box_edge t))
  | Text_box -> Some (v Text_box (read_text_box t))
  | Inline_sizing -> Some (v Inline_sizing (read_inline_sizing t))
  | Line_fit_edge -> Some (v Line_fit_edge (read_line_fit_edge t))
  | Interpolate_size -> Some (v Interpolate_size (read_interpolate_size t))
  | Min_intrinsic_sizing ->
      Some (v Min_intrinsic_sizing (read_min_intrinsic_sizing t))
  | Ruby_align -> Some (v Ruby_align (read_ruby_align t))
  | Ruby_merge -> Some (v Ruby_merge (read_ruby_merge t))
  | Ruby_overhang -> Some (v Ruby_overhang (read_ruby_overhang t))
  | Ruby_position -> Some (v Ruby_position (read_ruby_position t))
  | Glyph_orientation_vertical ->
      Some (v Glyph_orientation_vertical (read_glyph_orientation_vertical t))
  | _ -> None

let read_timeline_image_value : type a.
    a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Animation_timeline ->
      Some (v Animation_timeline (read_animation_timeline t))
  | Animation_range -> Some (v Animation_range (read_animation_range t))
  | Animation_range_start ->
      Some (v Animation_range_start (read_animation_range_item t))
  | Animation_range_end ->
      Some (v Animation_range_end (read_animation_range_item t))
  | Scroll_timeline -> Some (v Scroll_timeline (read_timeline_shorthand t))
  | Scroll_timeline_name -> Some (v Scroll_timeline_name (read_timeline_name t))
  | Scroll_timeline_axis -> Some (v Scroll_timeline_axis (read_timeline_axis t))
  | View_transition_name ->
      Some (v View_transition_name (read_view_transition_name t))
  | View_transition_class ->
      Some (v View_transition_class (read_view_transition_class t))
  | Image_orientation -> Some (v Image_orientation (read_image_orientation t))
  | Image_rendering -> Some (v Image_rendering (read_image_rendering t))
  | Image_resolution -> Some (v Image_resolution (read_image_resolution t))
  | Contain_intrinsic_size ->
      Some (v Contain_intrinsic_size (read_contain_intrinsic_size t))
  | Contain_intrinsic_width ->
      Some (v Contain_intrinsic_width (read_contain_intrinsic_longhand t))
  | Contain_intrinsic_height ->
      Some (v Contain_intrinsic_height (read_contain_intrinsic_longhand t))
  | Contain_intrinsic_block_size ->
      Some (v Contain_intrinsic_block_size (read_contain_intrinsic_longhand t))
  | Contain_intrinsic_inline_size ->
      Some (v Contain_intrinsic_inline_size (read_contain_intrinsic_longhand t))
  | _ -> None

let read_motion_value : type a. a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Margin_trim -> Some (v Margin_trim (read_margin_trim t))
  | Offset_path -> Some (v Offset_path (read_offset_path t))
  | Offset -> Some (v Offset (read_offset t))
  | Offset_anchor -> Some (v Offset_anchor (read_offset_anchor t))
  | Offset_position -> Some (v Offset_position (read_offset_position t))
  | Offset_rotate -> Some (v Offset_rotate (read_offset_rotate t))
  | Font_size_adjust -> Some (v Font_size_adjust (read_font_size_adjust t))
  | Font_variant_emoji ->
      Some (v Font_variant_emoji (read_font_variant_emoji t))
  | Text_spacing_trim -> Some (v Text_spacing_trim (read_text_spacing_trim t))
  | Hyphenate_limit_chars ->
      Some (v Hyphenate_limit_chars (read_hyphenate_limit_chars t))
  | Initial_letter -> Some (v Initial_letter (read_initial_letter t))
  | Initial_letter_align ->
      Some (v Initial_letter_align (read_initial_letter_align t))
  | Initial_letter_wrap ->
      Some (v Initial_letter_wrap (read_initial_letter_wrap t))
  | Dominant_baseline -> Some (v Dominant_baseline (read_dominant_baseline t))
  | View_timeline_name -> Some (v View_timeline_name (read_timeline_name t))
  | View_timeline_axis -> Some (v View_timeline_axis (read_timeline_axis t))
  | View_timeline_inset -> Some (v View_timeline_inset (read_timeline_inset t))
  | View_timeline -> Some (v View_timeline (read_view_timeline_shorthand t))
  | Timeline_scope -> Some (v Timeline_scope (read_timeline_name t))
  | Perspective_origin ->
      Some (v Perspective_origin (read_perspective_origin t))
  | Transform_style -> Some (v Transform_style (read_transform_style t))
  | Backface_visibility ->
      Some (v Backface_visibility (read_backface_visibility t))
  | Rotate -> Some (v Rotate (read_rotate_value t))
  | Scale -> Some (v Scale (read_scale t))
  | _ -> None

let read_object_transition_value : type a.
    a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Object_position -> Some (v Object_position (read_position_value t))
  | Object_fit -> Some (v Object_fit (read_object_fit t))
  | Object_view_box -> Some (v Object_view_box (read_object_view_box t))
  | Transition_duration ->
      Some (v Transition_duration (read_duration_list read_duration t))
  | Transition_timing_function ->
      Some (v Transition_timing_function (read_timing_function_list t))
  | Transition_delay ->
      Some (v Transition_delay (read_duration_list read_time t))
  | Transition_property ->
      Some (v Transition_property (read_transition_property t))
  | Transition_behavior ->
      Some (v Transition_behavior (Properties.read_transition_behavior t))
  | Overlay -> Some (v Overlay (read_overlay t))
  | Will_change -> Some (v Will_change (read_will_change t))
  | Contain -> Some (v Contain (read_contain t))
  | Isolation -> Some (v Isolation (read_isolation t))
  | Break_before -> Some (v Break_before (read_break_value t))
  | Break_after -> Some (v Break_after (read_break_value t))
  | Break_inside -> Some (v Break_inside (read_break_inside_value t))
  | Page_break_before -> Some (v Page_break_before (read_page_break_value t))
  | Page_break_after -> Some (v Page_break_after (read_page_break_value t))
  | Page_break_inside ->
      Some (v Page_break_inside (read_page_break_inside_value t))
  | Page_size -> Some (v Page_size (read_page_size t))
  | Columns -> Some (v Columns (read_columns_value t))
  | Column_width -> Some (v Column_width (read_column_width t))
  | Column_height -> Some (v Column_height (read_column_height t))
  | Column_wrap -> Some (v Column_wrap (read_column_wrap t))
  | Column_count -> Some (v Column_count (read_column_count t))
  | Column_rule -> Some (v Column_rule (read_border t))
  (* CSS Gaps 1 sec. 4 gives each gap decoration longhand a comma-separated
     list, one entry per rule line. *)
  | Column_rule_color ->
      Some
        (v Column_rule_color
           (Cursor.list ~sep:Cursor.comma ~at_least:1 read_color t))
  | Column_rule_width ->
      Some
        (v Column_rule_width
           (Cursor.list ~sep:Cursor.comma ~at_least:1 read_border_width t))
  | Column_rule_style ->
      Some
        (v Column_rule_style
           (Cursor.list ~sep:Cursor.comma ~at_least:1 read_border_style t))
  | Column_span -> Some (v Column_span (read_column_span t))
  | _ -> None

let read_border_clip_value : type a.
    a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Border_top -> Some (v Border_top (read_border t))
  | Border_right -> Some (v Border_right (read_border t))
  | Border_bottom -> Some (v Border_bottom (read_border t))
  | Border_left -> Some (v Border_left (read_border t))
  | Border_block -> Some (v Border_block (read_border t))
  | Border_block_start -> Some (v Border_block_start (read_border t))
  | Border_block_end -> Some (v Border_block_end (read_border t))
  | Border_inline -> Some (v Border_inline (read_border t))
  | Border_inline_start -> Some (v Border_inline_start (read_border t))
  | Border_inline_end -> Some (v Border_inline_end (read_border t))
  | Border_spacing -> Some (v Border_spacing (read_border_spacing t))
  | Border_image -> Some (v Border_image (read_border_image t))
  | Border_collapse -> Some (v Border_collapse (read_border_collapse t))
  | Clip_path -> Some (v Clip_path (read_clip_path t))
  | Mask -> Some (v Mask (read_mask t))
  | Clip -> Some (v Clip (read_clip t))
  | Moz_osx_font_smoothing ->
      Some (v Moz_osx_font_smoothing (read_moz_osx_font_smoothing t))
  | Backdrop_filter -> Some (v Backdrop_filter (read_filter t))
  | Webkit_backdrop_filter -> Some (v Webkit_backdrop_filter (read_filter t))
  | _ -> None

let read_scroll_value : type a. a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Scroll_snap_align -> Some (v Scroll_snap_align (read_scroll_snap_align t))
  | Scroll_snap_stop -> Some (v Scroll_snap_stop (read_scroll_snap_stop t))
  | Scroll_behavior -> Some (v Scroll_behavior (read_scroll_behavior t))
  | Scroll_margin ->
      Some
        (v Scroll_margin
           (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:4
              read_scroll_margin_length t))
  | Scroll_margin_top ->
      Some (v Scroll_margin_top (read_scroll_margin_length t))
  | Scroll_margin_right ->
      Some (v Scroll_margin_right (read_scroll_margin_length t))
  | Scroll_margin_bottom ->
      Some (v Scroll_margin_bottom (read_scroll_margin_length t))
  | Scroll_margin_left ->
      Some (v Scroll_margin_left (read_scroll_margin_length t))
  | Scroll_margin_inline ->
      Some
        (v Scroll_margin_inline
           (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
              read_scroll_margin_length t))
  | Scroll_margin_inline_start ->
      Some (v Scroll_margin_inline_start (read_scroll_margin_length t))
  | Scroll_margin_inline_end ->
      Some (v Scroll_margin_inline_end (read_scroll_margin_length t))
  | Scroll_margin_block ->
      let lengths =
        Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
          read_scroll_margin_length t
      in
      (match lengths with
      | [ Zero; Zero ] -> Cursor.err_invalid t "duplicate zero scroll margin"
      | _ -> ());
      Some (v Scroll_margin_block lengths)
  | Scroll_margin_block_start ->
      Some (v Scroll_margin_block_start (read_scroll_margin_length t))
  | Scroll_margin_block_end ->
      Some (v Scroll_margin_block_end (read_scroll_margin_length t))
  | Scroll_padding ->
      Some
        (v Scroll_padding
           (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:4
              read_scroll_padding_length t))
  | Scroll_padding_top ->
      Some (v Scroll_padding_top (read_scroll_padding_length t))
  | Scroll_padding_right ->
      Some (v Scroll_padding_right (read_scroll_padding_length t))
  | Scroll_padding_bottom ->
      Some (v Scroll_padding_bottom (read_scroll_padding_length t))
  | Scroll_padding_left ->
      Some (v Scroll_padding_left (read_scroll_padding_length t))
  | Scroll_padding_inline ->
      Some
        (v Scroll_padding_inline
           (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
              read_scroll_padding_length t))
  | Scroll_padding_inline_start ->
      Some (v Scroll_padding_inline_start (read_scroll_padding_length t))
  | Scroll_padding_inline_end ->
      Some (v Scroll_padding_inline_end (read_scroll_padding_length t))
  | Scroll_padding_block ->
      Some
        (v Scroll_padding_block
           (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
              read_scroll_padding_length t))
  | Scroll_padding_block_start ->
      Some (v Scroll_padding_block_start (read_scroll_padding_length t))
  | Scroll_padding_block_end ->
      Some (v Scroll_padding_block_end (read_scroll_padding_length t))
  | Overscroll_behavior ->
      let xs =
        Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
          read_overscroll_behavior t
      in
      Some (v Overscroll_behavior xs)
  | Overscroll_behavior_x ->
      Some (v Overscroll_behavior_x (read_overscroll_behavior t))
  | Overscroll_behavior_y ->
      Some (v Overscroll_behavior_y (read_overscroll_behavior t))
  | Overscroll_behavior_block ->
      Some (v Overscroll_behavior_block (read_overscroll_behavior t))
  | Overscroll_behavior_inline ->
      Some (v Overscroll_behavior_inline (read_overscroll_behavior t))
  | _ -> None

let read_interaction_value : type a.
    a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | User_select -> Some (v User_select (read_user_select t))
  | Webkit_user_select -> Some (v Webkit_user_select (read_user_select t))
  | Ms_user_select -> Some (v Ms_user_select (read_user_select t))
  | Webkit_text_fill_color -> Some (v Webkit_text_fill_color (read_color t))
  | Webkit_text_stroke_color -> Some (v Webkit_text_stroke_color (read_color t))
  | Webkit_text_stroke_width ->
      Some (v Webkit_text_stroke_width (read_border_width t))
  | Webkit_text_stroke ->
      Some (v Webkit_text_stroke (read_webkit_text_stroke t))
  | Moz_user_select -> Some (v Moz_user_select (read_user_select t))
  | Pointer_events -> Some (v Pointer_events (read_pointer_events t))
  | Resize -> Some (v Resize (read_resize t))
  | Transition -> Some (v Transition (read_transitions t))
  | Animation -> Some (v Animation (read_animations t))
  | Quotes -> Some (v Quotes (read_quotes t))
  | Touch_action -> Some (v Touch_action (read_touch_action t))
  | Clear -> Some (v Clear (read_clear t))
  | Float -> Some (v Float (read_float_side t))
  | Fill -> Some (v Fill (read_svg_paint t))
  | Stroke -> Some (v Stroke (read_svg_paint t))
  | Direction -> Some (v Direction (read_direction t))
  | Fill_rule -> Some (v Fill_rule (Properties.read_fill_rule t))
  | Clip_rule -> Some (v Clip_rule (Properties.read_fill_rule t))
  | Stroke_linecap -> Some (v Stroke_linecap (Properties.read_stroke_linecap t))
  | Stroke_linejoin ->
      Some (v Stroke_linejoin (Properties.read_stroke_linejoin t))
  | Stroke_miterlimit ->
      Some (v Stroke_miterlimit (Properties.read_stroke_miterlimit t))
  | Stroke_width -> Some (v Stroke_width (Properties.read_stroke_width t))
  | Stroke_dashoffset ->
      Some (v Stroke_dashoffset (Properties.read_stroke_dashoffset t))
  | Stroke_dasharray ->
      Some (v Stroke_dasharray (Properties.read_stroke_dasharray t))
  | Paint_order -> Some (v Paint_order (Properties.read_paint_order t))
  | Vector_effect -> Some (v Vector_effect (Properties.read_vector_effect t))
  | Unicode_bidi -> Some (v Unicode_bidi (read_unicode_bidi t))
  | Writing_mode -> Some (v Writing_mode (read_writing_mode t))
  | Text_combine_upright ->
      Some (v Text_combine_upright (read_text_combine_upright t))
  | Animation_name -> Some (v Animation_name (read_animation_name t))
  | Animation_duration ->
      Some (v Animation_duration (read_duration_list read_animation_duration t))
  | Animation_timing_function ->
      Some (v Animation_timing_function (read_timing_function_list t))
  | Animation_delay -> Some (v Animation_delay (read_duration_list read_time t))
  | Animation_iteration_count ->
      Some (v Animation_iteration_count (read_animation_iteration_count t))
  | Animation_direction ->
      Some (v Animation_direction (read_animation_direction t))
  | Animation_fill_mode ->
      Some (v Animation_fill_mode (read_animation_fill_mode t))
  | Animation_play_state ->
      Some (v Animation_play_state (read_animation_play_state t))
  | Animation_composition ->
      Some (v Animation_composition (read_animation_composition t))
  | Mix_blend_mode -> Some (v Mix_blend_mode (read_blend_mode t))
  | Table_layout -> Some (v Table_layout (read_table_layout t))
  | Print_color_adjust ->
      Some (v Print_color_adjust (read_print_color_adjust t))
  | Webkit_print_color_adjust ->
      Some (v Webkit_print_color_adjust (read_print_color_adjust t))
  | Box_decoration_break ->
      Some (v Box_decoration_break (read_box_decoration_break t))
  | Webkit_box_decoration_break ->
      Some (v Webkit_box_decoration_break (read_box_decoration_break t))
  | _ -> None

let read_mask_value : type a. a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Webkit_mask_image -> Some (v Webkit_mask_image (read_background_image t))
  | Webkit_mask_composite ->
      Some (v Webkit_mask_composite (read_webkit_mask_composite t))
  | Webkit_mask_source_type ->
      Some (v Webkit_mask_source_type (read_webkit_mask_source_type t))
  | Webkit_mask_size -> Some (v Webkit_mask_size (read_background_size_list t))
  | Webkit_mask_position ->
      Some (v Webkit_mask_position (read_background_position t))
  | Webkit_mask_repeat ->
      Some (v Webkit_mask_repeat (read_background_repeat_list t))
  | Webkit_mask_clip -> Some (v Webkit_mask_clip (read_mask_box_list t))
  | Webkit_mask_origin -> Some (v Webkit_mask_origin (read_mask_box_list t))
  | Border_image_source ->
      Some (v Border_image_source (read_background_image t))
  | Border_image_slice ->
      Some (v Border_image_slice (read_border_image_slice t))
  | Border_image_repeat ->
      Some (v Border_image_repeat (read_border_image_repeat t))
  | Border_image_width ->
      Some (v Border_image_width (read_border_image_width t))
  | Border_image_outset ->
      Some (v Border_image_outset (read_border_image_outset t))
  | Mask_image -> Some (v Mask_image (read_background_image t))
  | Mask_composite -> Some (v Mask_composite (read_mask_composite_list t))
  | Mask_mode -> Some (v Mask_mode (read_mask_mode t))
  | Mask_border -> Some (v Mask_border (read_mask_border t))
  | Mask_size -> Some (v Mask_size (read_background_size_list t))
  | Mask_position -> Some (v Mask_position (read_background_position t))
  | Mask_repeat -> Some (v Mask_repeat (read_background_repeat_list t))
  | Mask_clip -> Some (v Mask_clip (read_mask_box_list t))
  | Mask_origin -> Some (v Mask_origin (read_mask_box_list t))
  | Mask_type -> Some (v Mask_type (read_mask_type t))
  | All -> Some (v All (Properties.read_css_wide t))
  | _ -> None

let read_value_readers =
  [
    { read_value_opt = read_color_value };
    { read_value_opt = read_sizing_value };
    { read_value_opt = read_radius_gap_value };
    { read_value_opt = read_layout_value };
    { read_value_opt = read_box_edge_value };
    { read_value_opt = read_type_value };
    { read_value_opt = read_flex_effect_value };
    { read_value_opt = read_vendor_alias_value };
    { read_value_opt = read_background_value };
    { read_value_opt = read_grid_value };
    { read_value_opt = read_shadow_content_value };
    { read_value_opt = read_spacing_value };
    { read_value_opt = read_list_align_value };
    { read_value_opt = read_position_value_prop };
    { read_value_opt = read_vendor_font_value };
    { read_value_opt = read_text_flow_value };
    { read_value_opt = read_modern_layout_value };
    { read_value_opt = read_inline_text_value };
    { read_value_opt = read_timeline_image_value };
    { read_value_opt = read_motion_value };
    { read_value_opt = read_object_transition_value };
    { read_value_opt = read_border_clip_value };
    { read_value_opt = read_scroll_value };
    { read_value_opt = read_interaction_value };
    { read_value_opt = read_mask_value };
  ]

let rec read_value_from : type a.
    value_reader list -> a property -> Cursor.t -> declaration =
 fun readers prop t ->
  match readers with
  | [] ->
      Cursor.err_invalid t
        ("unsupported property reader: " ^ canonical_prop_name prop)
  | reader :: rest -> (
      match reader.read_value_opt prop t with
      | Some decl -> decl
      | None -> read_value_from rest prop t)

let read_value (type a) (prop : a property) t : declaration =
  Cursor.with_context t (canonical_prop_name prop) @@ fun () ->
  match prop with
  | Custom_property name ->
      Cursor.err_invalid t
        ("custom property read through regular property: " ^ name)
  | Unknown_property name ->
      Cursor.err_invalid t
        ("unknown property read through typed parser: " ^ name)
  | _ -> read_value_from read_value_readers prop t

(* Check if a custom property name is a font-family variable *)

(** Parse a custom property (--name: value) *)
let is_font_family_var name =
  let bare = Custom_property_name.strip_prefix name in
  let starts_with prefix s =
    String.length s >= String.length prefix
    && String.sub s 0 (String.length prefix) = prefix
  in
  starts_with "font-" bare
  || starts_with "default-font-family" bare
  || starts_with "default-mono-font-family" bare

(* CSS Syntax 3 (ED) sec. 5.5.6 takes the flag off when the last two
   non-whitespace values of the declaration are a [!] delim and an ident
   matching [important], so the whitespace [! important] writes between the two
   is inside the flag. Chrome 151 reads it that way, and what is left is the
   value. *)
let is_ws_char = function
  | ' ' | '\t' | '\n' | '\r' | '\012' -> true
  | _ -> false

let trim_end s =
  let n = ref (String.length s) in
  while !n > 0 && is_ws_char s.[!n - 1] do
    decr n
  done;
  String.sub s 0 !n

let split_important value =
  let trimmed = String.trim value in
  let len = String.length trimmed in
  let word = "important" in
  let word_len = String.length word in
  if
    len > word_len
    && String.lowercase_ascii (String.sub trimmed (len - word_len) word_len)
       = word
  then
    let head = trim_end (String.sub trimmed 0 (len - word_len)) in
    let head_len = String.length head in
    if head_len > 0 && Char.equal head.[head_len - 1] '!' then
      (String.trim (String.sub head 0 (head_len - 1)), true)
    else (trimmed, false)
  else (trimmed, false)

let read_custom_property_payload name value_str =
  if is_font_family_var name then
    let trimmed = String.trim value_str in
    if String.length trimmed >= 4 && String.sub trimmed 0 4 = "var(" then
      read_custom_property_value (Cursor.of_string value_str)
    else
      read_custom_property_value ~font_family:true (Cursor.of_string value_str)
  else read_custom_property_value (Cursor.of_string value_str)

let whitespace_only_custom_property_value =
  Tokens [ Component.Preserved (Token.synthetic Token.Whitespace) ]

(* Keep a single space when the raw declaration value was whitespace-only - that
   one space is the spec-required token sequence (CSS Custom Properties for
   Cascading Variables 1 sec. 2.1). *)
let read_custom_value name ~raw_is_whitespace_only value_str =
  let value_str =
    if value_str = "" && raw_is_whitespace_only then " " else value_str
  in
  if value_str = " " && raw_is_whitespace_only then
    whitespace_only_custom_property_value
  else read_custom_property_payload name value_str

(* The [name] declaration formed over the value at [t]; split from
   [read_custom_property_declaration] for a caller that already holds the
   name. *)
let read_custom_value_declaration t name : declaration =
  (* CSS Custom Properties 1 sec. 2.1: [<declaration-value>] matches "any
     sequence of one or more tokens", so the whitespace after [:] IS the value
     when nothing else follows ([--foo: ;]). Don't skip it before
     [consume_until_semicolon], or the token count becomes input-dependent. *)
  reject_custom_bad_string t;
  let raw_value = Cursor.consume_until_semicolon ~trim:false t in
  let raw_is_whitespace_only = raw_value <> "" && String.trim raw_value = "" in
  let value_str, is_important = split_important raw_value in
  if not (is_optional_declaration_value value_str) then
    Cursor.err_invalid t "custom property value is no <declaration-value>";
  (* custom_property may raise Failure for invalid names like "--" *)
  try
    let custom_value =
      read_custom_value name ~raw_is_whitespace_only value_str
    in
    let decl =
      v (Custom_property name)
        (Custom_value { value = custom_value; layer = None; meta = None })
    in
    if is_important then important decl else decl
  with Failure msg -> Cursor.err_invalid t msg

let read_custom_property_declaration t : declaration =
  let name = read_property_name t in
  (* CSS Syntax 3 (ED) sec. 4.3.7 lets [\X] escapes carry any code point into an
     ident, so the name may contain characters ([/], whitespace, etc.) that
     don't tokenize as a bare ident on a string round-trip. We trust the
     original lexer's tokenization: the only validation we still run is the
     [<dashed-ident>] prefix check. *)
  if String.length name <= 2 || name.[0] <> '-' || name.[1] <> '-' then
    Cursor.err_invalid t ("expected <dashed-ident>, got: " ^ name);
  Cursor.ws t;
  if not (Cursor.colon t) then Cursor.err_expected t "':'";
  read_custom_value_declaration t name

(* Properties whose grammar allows multi-token values where a CSS-wide keyword
   can legitimately appear as a non-special ident. [animation-name] /
   [will-change] / etc. accept arbitrary ident lists. [font-family] also takes a
   [<custom-ident>#] list, so a CSS-wide keyword inside the list is invalid CSS
   (CSS Cascade 5 sec. 7.3) but upstream tools (lightningcss, csso) preserve the
   source verbatim. *)
let property_allows_keyword_as_ident = function
  | "animation-name" | "will-change" | "view-transition-name" | "font-family"
  | "font" ->
      true
  | _ -> false

(* [all] is the only property whose value must be a lone CSS-wide keyword (or a
   [var()] that may resolve to one); detect that directly on the component list,
   ignoring leading and trailing whitespace, without rebuilding a string. *)
let components_are_lone_css_wide cvs =
  let non_ws =
    List.filter
      (function
        | Component.Preserved { kind = Token.Whitespace; _ } -> false
        | _ -> true)
      cvs
  in
  match non_ws with
  | [ Component.Preserved { kind = Token.Ident ident; _ } ] ->
      Properties.is_css_wide_keyword ident
  | [ Component.Func { node = { name; _ }; _ } ] ->
      let n = String.lowercase_ascii_preserve name in
      String.equal n "var" || String.equal n "env" || String.equal n "attr"
  | _ -> false

(* CSS Anchor Positioning 1 sec. 3.2 allows [anchor()] in the inset properties
   and nowhere else, and sec. 5.1 allows [anchor-size()] in the properties
   [@position-try] accepts: the inset, margin, sizing and self-alignment ones,
   plus [position-anchor] and [position-area]. A call written anywhere else is
   invalid, so the declaration goes. *)
let inset_properties =
  [
    "top";
    "right";
    "bottom";
    "left";
    "inset";
    "inset-block";
    "inset-block-start";
    "inset-block-end";
    "inset-inline";
    "inset-inline-start";
    "inset-inline-end";
  ]

let anchor_size_properties =
  inset_properties
  @ [
      "margin";
      "margin-top";
      "margin-right";
      "margin-bottom";
      "margin-left";
      "margin-block";
      "margin-block-start";
      "margin-block-end";
      "margin-inline";
      "margin-inline-start";
      "margin-inline-end";
      "width";
      "height";
      "min-width";
      "min-height";
      "max-width";
      "max-height";
      "block-size";
      "inline-size";
      "min-block-size";
      "min-inline-size";
      "max-block-size";
      "max-inline-size";
      "align-self";
      "justify-self";
      "place-self";
      "position-anchor";
      "position-area";
    ]

let rec components_call_named names components =
  List.exists (component_calls_named names) components

and component_calls_named names = function
  | Component.Func { node = { name; arguments; _ }; _ } ->
      List.exists (String.equal (String.lowercase_ascii_preserve name)) names
      || components_call_named names arguments
  | Component.Block { node = { value; _ }; _ } ->
      components_call_named names value
  | Component.Preserved _ -> false

let validate_anchor_queries t name components =
  let allowed table = List.exists (String.equal name) table in
  if
    components_call_named [ "anchor" ] components
    && not (allowed inset_properties)
  then Cursor.err_invalid t ("anchor() is not a value of " ^ name);
  if
    components_call_named [ "anchor-size" ] components
    && not (allowed anchor_size_properties)
  then Cursor.err_invalid t ("anchor-size() is not a value of " ^ name)

let validate_regular_property_components t name components =
  (* CSS Syntax 3 (ED) sec. 5.5.6 gives a declaration a value of one or more
     component values, and only a custom property takes the empty one (CSS
     Variables 1 sec. 2). *)
  if List.for_all Component.is_whitespace components then
    Cursor.err_expected t "a value";
  validate_anchor_queries t name components;
  (* A substitution function makes the declaration's grammar a computed-value
     question. This includes [all]: the substitution result can be empty and
     leave its neighbouring CSS-wide keyword as the whole value. *)
  let has_substitution = components_have_substitution components in
  if
    (not (property_allows_keyword_as_ident name))
    && (not has_substitution)
    && Properties.components_have_css_wide_mix components
  then Cursor.err_invalid t "CSS-wide keyword mixed with other values";
  if
    String.equal name "all" && (not has_substitution)
    && not (components_are_lone_css_wide components)
  then Cursor.err_invalid t "all accepts only CSS-wide keywords"

(* Color functions cascade types directly. A vendor one it does not is kept
   verbatim: the prefix says the vocabulary is an engine's own, so no
   specification settles it and no browser but that engine reads it either. An
   unprefixed name cascade does not know is no colour - CSS Color 5 sec. 3
   closes the production - and every browser drops the declaration. *)
let color_fallback_function raw_value =
  let components =
    Cursor.of_string raw_value |> Cursor.remaining
    |> List.filter (fun component -> not (Component.is_whitespace component))
  in
  match components with
  | [ Component.Func { node = { name; _ }; _ } ] ->
      let fn = String.lowercase_ascii_preserve name in
      String.length fn > 0
      && Char.equal fn.[0] '-'
      && not
           (List.mem fn
              [
                "rgb";
                "rgba";
                "hsl";
                "hsla";
                "hwb";
                "lab";
                "lch";
                "oklab";
                "oklch";
                "color";
                "color-mix";
                "light-dark";
                "var";
              ])
  | _ -> false

(* A colour function whose arguments contain a [var()] can't be validated until
   substitution (CSS Variables L1 section 3): the var() may stand for several
   channels or the alpha, so the arity and legacy/modern separator style aren't
   known at parse time. [rgba(var(--rgb), var(--a))] with a comma-list [--rgb]
   only stays valid as the legacy comma form, so cascade keeps the declaration
   verbatim. The function may be nested (a gradient stop, a shadow colour), so
   the search recurses. (A math function whose return type is wrong regardless
   of the var, e.g. [flex-basis: sign(var(--x))], is still rejected by the
   reader.) *)
let rec components_have_var_color_function components =
  List.exists component_has_var_color_function components

and component_has_var_color_function = function
  | Component.Func { node = { name; arguments; _ }; _ } ->
      List.mem
        (String.lowercase_ascii_preserve name)
        [ "rgb"; "rgba"; "hsl"; "hsla"; "hwb"; "lab"; "lch"; "oklab"; "oklch" ]
      && components_contain_var arguments
      || components_have_var_color_function arguments
  | Component.Block { node = { value; _ }; _ } ->
      components_have_var_color_function value
  | Component.Preserved _ -> false

let has_var_color_function raw_value =
  Cursor.of_string raw_value |> Cursor.remaining
  |> components_have_var_color_function

let is_color_property = function
  | "color" | "background-color" | "border-color" | "border-top-color"
  | "border-right-color" | "border-bottom-color" | "border-left-color"
  | "border-inline-start-color" | "border-inline-end-color"
  | "border-block-start-color" | "border-block-end-color"
  | "text-decoration-color" | "-webkit-text-decoration-color"
  | "-webkit-tap-highlight-color" | "outline-color" | "accent-color"
  | "caret-color" | "fill" | "stroke" ->
      true
  | _ -> false

let is_unsupported_color_fallback name raw_value =
  color_fallback_function raw_value && is_color_property name

let is_unknown_property_name = is_decl_unknown_property_name

(* The typed readers carry their own [Invalid] arms for spec-violations they
   detect ([Values.angle.Invalid] / [Properties.clip_path.Invalid]), so the
   declaration-level unknown fallback only needs to handle truly unknown
   properties or property-specific colour fallback edges. *)
let allows_unknown_fallback name raw_value =
  (not (raw_value_has_invalid_substitution raw_value))
  && (is_unknown_property_name name
     || is_unsupported_color_fallback name raw_value
     || has_var_color_function raw_value
     || raw_value_contains_substitution raw_value)

let read_font_src_declaration t raw_value =
  ignore raw_value;
  let value = Cursor.declaration_value t in
  let decl = v Source (Properties.read_font_src value) in
  validate_no_extra_tokens value;
  let is_important = read_importance t in
  validate_no_extra_tokens t;
  if is_important then important decl else decl

let read_unknown_property_declaration t name =
  validate_unknown_property_name t name;
  validate_complete_declaration_value t;
  reject_curly_block_value t;
  reject_unterminated_string_value t;
  reject_opaque_bad_token t;
  let raw_value = Cursor.consume_to_decl_end ~trim:true t in
  let is_important = read_importance t in
  validate_no_extra_tokens t;
  (match Cursor.peek_delim t with
  | Some '!' -> Cursor.err_invalid t "duplicate !important"
  | _ -> ());
  unknown_property ~important:is_important name raw_value

(* The declaration [prop_type] forms over the value at [t]. Takes the property
   rather than reading it, so a caller holding one does not have to spell it out
   for the parser to read back: [pp_property] under minify writes [page-break-*]
   as its CSS Fragmentation 3 sec. 3.4 [break-*] alias, which names a different
   property. *)
let read_typed_value_declaration : type a. a property -> Cursor.t -> declaration
    =
 fun prop_type t ->
  (* CSS Syntax 3 (ED) sec. 2.2 / sec. 4.3.5 auto-close unterminated functions,
     brackets and strings at EOF. Typed readers consume the spec-recovered
     tokens; the declaration survives with the auto-closed shape. *)
  match prop_type with
  | Unknown_property name -> read_unknown_property_declaration t name
  | _ -> (
      (* The typed value readers and [validate_no_extra_tokens] reject a
         right-hand side without knowing which property they serve, so a
         [Bad_value] surfaces with an empty property name ([bad value for :]).
         Stamp the property back on so the warning names it ([bad value for
         background-image:]). *)
      let read () =
        let value = Cursor.declaration_value t in
        let decl = read_value prop_type value in
        validate_no_extra_tokens value;
        let is_important = read_importance t in
        validate_no_extra_tokens t;
        (match Cursor.peek_delim t with
        | Some '!' -> Cursor.err_invalid t "duplicate !important"
        | _ -> ());
        if is_important then important decl else decl
      in
      try read ()
      with Cursor.Parse_error e ->
        Error.fail (Error.with_property (canonical_prop_name prop_type) e))

let read_typed_property_declaration t start =
  Cursor.restore t start;
  let (Prop prop_type) = read_any_property t in
  Cursor.ws t;
  if not (Cursor.colon t) then Cursor.err_expected t "':'";
  Cursor.ws t;
  read_typed_value_declaration prop_type t

(** Parse a regular property (name: value) *)
let read_regular_property_declaration t : declaration =
  let start = Cursor.save t in
  let name = String.lowercase_ascii_preserve (read_property_name t) in
  Cursor.ws t;
  if not (Cursor.colon t) then Cursor.err_expected t "':'";
  Cursor.ws t;
  let components = Cursor.lookahead Cursor.drain_to_decl_end t in
  validate_regular_property_components t name components;
  if String.equal name "src" then
    read_font_src_declaration t
      (String.trim (Parser.string_of_components components))
  else
    try read_typed_property_declaration t start
    with Cursor.Parse_error _ as exn ->
      (* A declaration-shaped token stream is not a valid declaration: a value
         the typed reader rejects is one a browser rejects, and README states
         that those are dropped. The fallback carries the values that are valid
         CSS this reader cannot represent, which is what
         [allows_unknown_fallback] names. *)
      let raw_value = String.trim (Parser.string_of_components components) in
      if not (allows_unknown_fallback name raw_value) then raise exn;
      Cursor.restore t start;
      let name = String.lowercase_ascii_preserve (read_property_name t) in
      Cursor.ws t;
      if not (Cursor.colon t) then Cursor.err_expected t "':'";
      Cursor.ws t;
      read_unknown_property_declaration t name

(* [read_regular_property_declaration] with the name step replaced by the
   property itself. [name] serves the validation and the opaque fallback; the
   identity of what is read back comes from [prop]. *)
let read_regular_value_declaration : type a.
    a property -> Cursor.t -> declaration =
 fun prop t ->
  let name = canonical_prop_name prop in
  let start = Cursor.save t in
  let components = Cursor.lookahead Cursor.drain_to_decl_end t in
  validate_regular_property_components t name components;
  match prop with
  | Source ->
      read_font_src_declaration t
        (String.trim (Parser.string_of_components components))
  | _ -> (
      try read_typed_value_declaration prop t
      with Cursor.Parse_error _ as exn ->
        let raw_value = String.trim (Parser.string_of_components components) in
        if not (allows_unknown_fallback name raw_value) then raise exn;
        Cursor.restore t start;
        read_unknown_property_declaration t name)

(* CSS Nesting 1: distinguish a declaration ([<ident> : <value>]) from a nested
   rule whose selector starts with an ident ([html &:hover { ... }]). The
   starts-with-ident shape isn't conclusive on its own, so look at the next
   non-ident component: if it's [:] this is a declaration, otherwise walk the
   lookahead window for a [{ ... }] block before the next [;]. *)
let rec scan_for_curly_block t =
  match Cursor.peek_head_shape t with
  | `Eof | `Semicolon -> false
  | `Curly_block -> true
  | _ ->
      Cursor.skip t;
      scan_for_curly_block t

let is_nested_rule_inner t =
  (match Cursor.peek_head_shape t with `Ident -> Cursor.skip t | _ -> ());
  match Cursor.peek_head_shape t with
  | `Colon -> false
  | _ -> scan_for_curly_block t

let is_nested_rule t = Cursor.lookahead is_nested_rule_inner t

(** Parse a single declaration directly from stream - no string roundtrips *)
let read_declaration t : declaration option =
  let read_one () =
    Cursor.with_context t "read_declaration" @@ fun () ->
    (* Custom properties are idents starting with [--]. *)
    let is_custom =
      match Cursor.peek t with
      | Some (Component.Preserved { kind = Token.Ident s; _ }) ->
          Custom_property_name.has_prefix s
      | _ -> false
    in
    if is_custom then read_custom_property_declaration t
    else read_regular_property_declaration t
  in
  Cursor.ws t;
  match Cursor.peek t with
  | None -> None (* EOF is acceptable at top-level parsing *)
  | Some (Component.Preserved { kind = Token.Colon; _ })
  | Some (Component.Preserved { kind = Token.Hash _; _ })
  | Some (Component.Block { node = { opening = Token.Square; _ }; _ })
  | Some (Component.Preserved { kind = Token.Delim ("." | "*" | "&"); _ }) ->
      (* Selector-like components indicate a nested rule. *)
      None
  | Some _ ->
      (* CSS Nesting 1: a nested rule may start with an ident-shaped selector
         ([html &:hover], [li:nth-child(2)]). A declaration always starts with
         [<ident> :]; anything else followed by a [{ ... }] block (before the
         next [;]) is a nested rule. *)
      if is_nested_rule t then None else Some (read_one ())

type parse_step =
  | Done of declaration list
  | Continue of declaration list
  | Recover of {
      decls : declaration list;
      error : Error.t;
      from : Cursor.snapshot;
          (* Where the item that is about to be dropped starts. A separator that
             fails comes after a declaration already committed, so the two
             recovery arms carry different snapshots. *)
    }

let check_declaration_separator t acc =
  Cursor.ws t;
  match Cursor.peek t with
  | None -> Done (List.rev acc)
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
      Cursor.skip t;
      Continue acc
  | Some (Component.Preserved { kind = Token.Ident _; _ }) ->
      Cursor.err t "missing semicolon between declarations"
  | _ -> Done (List.rev acc)

let read_declaration_no_recovery t acc =
  match read_declaration t with
  | None -> Done (List.rev acc)
  | Some decl -> check_declaration_separator t (decl :: acc)

let read_declaration_with_recovery t acc =
  let start = Cursor.save t in
  match read_declaration t with
  | None -> Done (List.rev acc)
  | Some decl -> (
      let acc = decl :: acc in
      let after = Cursor.save t in
      match check_declaration_separator t acc with
      | step -> step
      | exception Error.Parse_error e ->
          Recover { decls = acc; error = e; from = after })
  | exception Error.Parse_error e ->
      Recover { decls = acc; error = e; from = start }

let read_declaration_step t acc =
  if Cursor.recover t then read_declaration_with_recovery t acc
  else read_declaration_no_recovery t acc

(* CSS Syntax 3 (ED) sec. 5.5.5 ("consume a block's contents"): the invalid
   declaration is dropped, reading resumes past the next [;], and the
   surrounding rule survives. *)
let recover_declaration_step t acc e ~from =
  Cursor.skip_past_semicolon t;
  Cursor.push_warning t
    ~recovery:(Cursor.dropped_since t from Error.Recovery.Declaration)
    e;
  acc

let rec read_declarations_loop t acc =
  Cursor.ws t;
  match Cursor.peek t with
  | None -> List.rev acc
  | _ -> (
      match read_declaration_step t acc with
      | Done decls -> decls
      | Continue acc -> read_declarations_loop t acc
      | Recover { decls; error; from } ->
          read_declarations_loop t
            (recover_declaration_step t decls error ~from))

let read_declarations t =
  Cursor.with_context t "declarations" @@ fun () -> read_declarations_loop t []

let read_block t =
  Cursor.ws t;
  Cursor.braces (fun inner -> read_declarations inner) t

let of_string s =
  let r = Cursor.of_string s in
  match read_declaration r with
  | Some d -> d
  (* [read_declaration] answers [None] for text a declaration cannot start with,
     which here is the whole input: report it where the reader stopped rather
     than as a second, position-free exception. *)
  | None -> Cursor.err_expected r "a declaration"

(* The declaration [property] and [value] name, as the tokens they are. The name
   is one [<ident-token>] by construction and the value is parsed on its own, so
   neither reaches the other's position: written into one text, a name carrying
   a [;] or a [}] (CSS Syntax 3 (ED) sec. 4.3.7 puts either there through an
   escape) would end its own declaration, and one carrying a [:] would name the
   property in front of it. *)
let name_value_components property value =
  Component.Preserved (Token.synthetic (Token.Ident property))
  :: Component.Preserved (Token.synthetic Token.Colon)
  :: Cursor.remaining (Cursor.of_string value)

let parse_declaration ?layer property value =
  (* Read [property] and [value] with the full declaration parser: a known
     property (e.g. [mask-type], [display]) becomes a typed declaration, a
     custom property ([--x]) or an unknown property keeps its parsed component
     stream (so [vars_of_declarations] still finds its [var()] references),
     unlike [custom_property] which forces an opaque [Tokens] value. A [layer]
     only applies to a custom property; [custom_property] attaches it and yields
     the same parsed token stream the declaration parser would. *)
  match layer with
  | Some _ when is_custom_property_name property -> (
      try Some (custom_property ?layer property value) with Failure _ -> None)
  | _ -> (
      try
        read_declaration
          (Cursor.of_components (name_value_components property value))
      with Cursor.Parse_error _ -> None)

(* [parse_declaration] with the typed grammar skipped: the value stays the
   component stream it was written as, the way an unknown property's does. A
   custom property already reads that way, so it takes the ordinary path. *)
let parse_opaque_declaration property value =
  if is_custom_property_name property then parse_declaration property value
  else
    let t = Cursor.of_components (name_value_components property value) in
    try
      let name = String.lowercase_ascii_preserve (read_property_name t) in
      Cursor.ws t;
      if not (Cursor.colon t) then Cursor.err_expected t "':'";
      Cursor.ws t;
      Some (read_unknown_property_declaration t name)
    with Cursor.Parse_error _ -> None

(* CSS Variables 1 sec. 2 gives a custom property the value grammar
   [<declaration-value>?], the empty value included, so this takes the pairs
   [custom_property] takes and answers with an option rather than raising. *)
let parse_custom_property name value =
  if is_custom_property_name name && is_optional_declaration_value value then
    parse_declaration name value
  else None

let read t =
  match read_declaration t with
  | Some d -> d
  | None -> Cursor.err_expected t "declaration"

(* A cascade layer and caller metadata belong to the declaration, not to its
   value, so a value read back from text starts without them. A value-only
   rewrite copies over what [from] holds, the way [map_custom_value] keeps them
   when it rewrites in place. *)
let keep_custom_fields ~from decl =
  match decl with
  | Declaration
      {
        property = Custom_property _ as property;
        value = Custom_value cv;
        important;
        _;
      } ->
      let layer = custom_declaration_layer from in
      let meta = meta_of_declaration from in
      v ~important property (Custom_value { cv with layer; meta })
  | Declaration _ | Theme_guarded _ -> decl

let with_value decl value =
  let important = if is_important decl then "!important" else "" in
  let tail () = Cursor.of_string (String.concat "" [ value; important ]) in
  match property_key decl with
  | Key (Custom_property name) ->
      keep_custom_fields ~from:decl
        (read_custom_value_declaration (tail ()) name)
  | Key (Unknown_property name) ->
      (* An unknown property is its name, and a substituted value may now be one
         a typed reader accepts, so read the pair back whole: that is the step
         that types [--x: 1px] once [width: var(--x)] has folded. *)
      read (Cursor.of_string (String.concat "" [ name; ":"; value; important ]))
  | Key prop -> read_regular_value_declaration prop (tail ())

let with_opaque_value decl value =
  let (Key prop) = property_key decl in
  let opaque =
    let name = canonical_prop_name prop in
    v (Unknown_property name) (Cursor.remaining (Cursor.of_string value))
  in
  if is_important decl then important opaque else opaque

(* Pretty printer for declarations *)
let rec pp : declaration Pp.t =
 fun ctx -> function
  | Declaration
      {
        property = Custom_property name;
        value = Custom_value { value; layer; _ };
        important;
        _;
      } ->
      pp_property ctx (Custom_property name);
      Pp.string ctx ":";
      Pp.space_if_pretty ctx ();
      pp_property_value ctx
        (Custom_property name, Custom_value { value; layer; meta = None });
      if important then
        Pp.string ctx (if ctx.minify then "!important" else " !important")
  | Declaration { property; value; important; _ } ->
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
      if important then
        Pp.string ctx (if ctx.minify then "!important" else " !important")
  | Theme_guarded { decl; _ } ->
      (* Theme guards are resolved by the transform layer; if one survives to
         print time, emit the wrapped declaration. *)
      pp ctx decl

(* A declaration feature query hands the authored declaration to another parser.
   Minify its separators, but keep opaque numeric token spellings: the spelling
   itself is the compatibility question. *)
let rec pp_opaque : declaration Pp.t =
 fun ctx decl ->
  let pp_components property components important =
    pp_property ctx property;
    Pp.char ctx ':';
    Pp.space_if_pretty ctx ();
    Pp.string ctx
      (if Pp.minified ctx then Parser.to_string_minified components
       else Parser.string_of_components components);
    if important then
      Pp.string ctx (if ctx.minify then "!important" else " !important")
  in
  match decl with
  | Declaration
      { property = Unknown_property _ as property; value; important; _ } ->
      pp_components property value important
  | Declaration
      {
        property = Custom_property _ as property;
        value = Custom_value { value = Tokens components; _ };
        important;
        _;
      } ->
      pp_components property components important
  | Declaration _ -> pp ctx decl
  | Theme_guarded { decl; _ } -> pp_opaque ctx decl

(* Convert a declaration to its string representation *)
let to_string ?(minify = false) (decl : t) =
  let buf = Buffer.create 32 in
  let ctx = Pp.ctx ~minify buf in
  pp ctx decl;
  Buffer.contents buf

(* Single-to-list property helpers *)
let require_nonempty name = function [] -> invalid_arg name | values -> values
let background_image value = v Background_image [ value ]
let text_shadow value = v Text_shadow [ value ]

let text_shadows values =
  v Text_shadow (require_nonempty "text_shadows: empty list" values)

let transition value = v Transition [ value ]

let transitions values =
  v Transition (require_nonempty "transitions: empty list" values)

let animation value = v Animation [ value ]
let box_shadow value = v Box_shadow value

let box_shadows values =
  v Box_shadow (List (require_nonempty "box_shadows: empty list" values))

(* Special helpers *)
let z_index_auto = v Z_index Auto

(* Font variant helpers *)
let font_variant_numeric_tokens tokens = (Tokens tokens : font_variant_numeric)

let font_variant_numeric_composed ?ordinal ?slashed_zero ?numeric_figure
    ?numeric_spacing ?numeric_fraction () =
  Composed
    { ordinal; slashed_zero; numeric_figure; numeric_spacing; numeric_fraction }

(* Property constructors with typed values *)
let background bg = v Background [ bg ]
let background_color c = v Background_color c
let color c = v Color c
let border_color c = v Border_color [ c ]
let border_style bs = v Border_style [ bs ]
let border_top_style bs = v Border_top_style bs
let border_right_style bs = v Border_right_style bs
let border_bottom_style bs = v Border_bottom_style bs
let border_left_style bs = v Border_left_style bs
let text_decoration td = v Text_decoration td
let font_style fs = v Font_style fs
let list_style_type lst = v List_style_type lst
let list_style_position ls = v List_style_position ls
let list_style_image is = v List_style_image is
let padding values = v Padding (require_nonempty "padding: empty list" values)
let padding_left len = v Padding_left len
let padding_right len = v Padding_right len
let padding_bottom len = v Padding_bottom len
let padding_top len = v Padding_top len
let margin values = v Margin (require_nonempty "margin: empty list" values)
let margin_left len = v Margin_left len
let margin_right len = v Margin_right len
let margin_top len = v Margin_top len
let margin_bottom len = v Margin_bottom len

(* Remove deprecated string-based versions *)
let gap len = v Gap len
let column_gap len = v Column_gap len
let row_gap len = v Row_gap len

(* Grid functions *)
let grid_template_areas value = v Grid_template_areas value
let grid_template template = v Grid_template template
let grid_auto_columns size = v Grid_auto_columns size
let grid_auto_rows size = v Grid_auto_rows size
let grid_row_start value = v Grid_row_start value
let grid_row_end value = v Grid_row_end value
let grid_column_start value = v Grid_column_start value
let grid_column_end value = v Grid_column_end value
let grid_row (start, end_) = v Grid_row (Lines (start, end_))
let grid_column (start, end_) = v Grid_column (Lines (start, end_))
let grid_area value = v Grid_area value
let width len = v Width (Length len)
let height len = v Height (Length len)

(* Remove deprecated string-based versions *)
let min_width len = v Min_width (Length len)
let min_height len = v Min_height (Length len)
let max_width len = v Max_width (Length len)
let max_height len = v Max_height (Length len)
let inline_size len = v Inline_size (Length len)
let min_inline_size len = v Min_inline_size (Length len)
let max_inline_size len = v Max_inline_size (Length len)
let block_size len = v Block_size (Length len)
let min_block_size len = v Min_block_size (Length len)
let max_block_size len = v Max_block_size (Length len)
let font_size len = v Font_size (Length len)
let font_size_kw fs = v Font_size fs
let line_height len = v Line_height len
let font_weight w = v Font_weight w
let text_align a = v Text_align a
let text_decoration_style value = v Text_decoration_style value
let text_decoration_line value = v Text_decoration_line [ value ]
let text_underline_offset value = v Text_underline_offset value
let text_decoration_skip value = v Text_decoration_skip value
let text_decoration_skip_self value = v Text_decoration_skip_self value
let text_decoration_skip_box value = v Text_decoration_skip_box value
let text_decoration_skip_inset value = v Text_decoration_skip_inset value
let text_decoration_skip_spaces value = v Text_decoration_skip_spaces value
let text_emphasis value = v Text_emphasis value
let text_emphasis_style value = v Text_emphasis_style value
let text_emphasis_color value = v Text_emphasis_color value
let text_emphasis_position value = v Text_emphasis_position value
let text_emphasis_skip value = v Text_emphasis_skip value
let text_orientation value = v Text_orientation value
let text_transform value = v Text_transform value
let letter_spacing len = v Letter_spacing len
let white_space value = v White_space value
let display d = v Display d
let position p = v Position p
let visibility p = v Visibility p
let inset values = v Inset (require_nonempty "inset: empty list" values)

let inset_inline values =
  v Inset_inline (require_nonempty "inset_inline: empty list" values)

let inset_inline_start len = v Inset_inline_start [ len ]
let inset_inline_end len = v Inset_inline_end [ len ]

let inset_block values =
  v Inset_block (require_nonempty "inset_block: empty list" values)

let inset_block_start len = v Inset_block_start [ len ]
let inset_block_end len = v Inset_block_end [ len ]
let top len = v Top [ len ]
let right len = v Right [ len ]
let bottom len = v Bottom [ len ]
let left len = v Left [ len ]
let opacity value = v Opacity value

(* Remove deprecated string-based versions *)
let flex_direction d = v Flex_direction d
let flex value = v Flex value
let flex_grow value = v Flex_grow (Number value : flex_factor)
let flex_shrink value = v Flex_shrink (Number value : flex_factor)
let flex_basis value = v Flex_basis value
let flex_wrap value = v Flex_wrap value
let flex_flow value = v Flex_flow value
let order value = v Order value
let align_items a = v Align_items a
let align_content a = v Align_content a
let align_self a = v Align_self a
let justify_content a = v Justify_content a
let justify_items a = v Justify_items a
let justify_self a = v Justify_self a
let place_content value = v Place_content value
let place_items value = v Place_items value
let place_self value = v Place_self value
let border_width len = v Border_width [ len ]
let border_radius len = v Border_radius len
let border_top_left_radius len = v Border_top_left_radius [ len ]
let border_top_right_radius len = v Border_top_right_radius [ len ]
let border_bottom_left_radius len = v Border_bottom_left_radius [ len ]
let border_bottom_right_radius len = v Border_bottom_right_radius [ len ]
let fill value = v Fill value
let stroke value = v Stroke value
let stroke_width value = v Stroke_width value
let fill_rule value = v Fill_rule value
let clip_rule value = v Clip_rule value
let fill_opacity value = v Fill_opacity value
let stroke_opacity value = v Stroke_opacity value
let stroke_linecap value = v Stroke_linecap value
let stroke_linejoin value = v Stroke_linejoin value
let stroke_miterlimit value = v Stroke_miterlimit value
let stroke_dashoffset value = v Stroke_dashoffset value
let stroke_dasharray value = v Stroke_dasharray value
let paint_order value = v Paint_order value
let vector_effect value = v Vector_effect value
let stop_color value = v Stop_color value
let stop_opacity value = v Stop_opacity value
let flood_color value = v Flood_color value
let flood_opacity value = v Flood_opacity value
let lighting_color value = v Lighting_color value
let dominant_baseline value = v Dominant_baseline value
let alignment_baseline value = v Alignment_baseline value
let baseline_shift value = v Baseline_shift value
let baseline_source value = v Baseline_source value
let outline_style o = v Outline_style o
let outline_width len = v Outline_width len
let outline_color c = v Outline_color c
let forced_color_adjust c = v Forced_color_adjust c
let table_layout value = v Table_layout value
let border_spacing lens = v Border_spacing lens
let overflow o = v Overflow o
let object_fit value = v Object_fit value
let object_view_box value = v Object_view_box value
let clip value = v Clip value
let clear value = v Clear value
let float value = v Float value
let interactivity value = v Interactivity value
let caret_animation value = v Caret_animation value
let caret_shape value = v Caret_shape value
let caret value = v Caret value
let interest_delay value = v Interest_delay value
let interest_delay_start value = v Interest_delay_start value
let interest_delay_end value = v Interest_delay_end value
let nav_up value = v Nav_up value
let nav_right value = v Nav_right value
let nav_down value = v Nav_down value
let nav_left value = v Nav_left value
let touch_action value = v Touch_action value
let direction value = v Direction value
let unicode_bidi value = v Unicode_bidi value
let writing_mode value = v Writing_mode value
let text_combine_upright value = v Text_combine_upright value
let text_decoration_skip_ink value = v Text_decoration_skip_ink value
let animation_name value = v Animation_name value
let animation_duration value = v Animation_duration value
let animation_timing_function value = v Animation_timing_function value
let animation_delay value = v Animation_delay value
let animation_iteration_count value = v Animation_iteration_count value
let animation_direction value = v Animation_direction value
let animation_fill_mode value = v Animation_fill_mode value
let animation_play_state value = v Animation_play_state value
let background_blend_mode value = v Background_blend_mode [ value ]

let scroll_margin values =
  v Scroll_margin (require_nonempty "scroll_margin: empty list" values)

let scroll_margin_top value = v Scroll_margin_top value
let scroll_margin_right value = v Scroll_margin_right value
let scroll_margin_bottom value = v Scroll_margin_bottom value
let scroll_margin_left value = v Scroll_margin_left value

let scroll_margin_inline values =
  v Scroll_margin_inline
    (require_nonempty "scroll_margin_inline: empty list" values)

let scroll_margin_inline_start value = v Scroll_margin_inline_start value
let scroll_margin_inline_end value = v Scroll_margin_inline_end value

let scroll_margin_block values =
  v Scroll_margin_block
    (require_nonempty "scroll_margin_block: empty list" values)

let scroll_margin_block_start value = v Scroll_margin_block_start value
let scroll_margin_block_end value = v Scroll_margin_block_end value

let scroll_padding values =
  v Scroll_padding (require_nonempty "scroll_padding: empty list" values)

let scroll_padding_top value = v Scroll_padding_top value
let scroll_padding_right value = v Scroll_padding_right value
let scroll_padding_bottom value = v Scroll_padding_bottom value
let scroll_padding_left value = v Scroll_padding_left value

let scroll_padding_inline values =
  v Scroll_padding_inline
    (require_nonempty "scroll_padding_inline: empty list" values)

let scroll_padding_inline_start value = v Scroll_padding_inline_start value
let scroll_padding_inline_end value = v Scroll_padding_inline_end value

let scroll_padding_block values =
  v Scroll_padding_block
    (require_nonempty "scroll_padding_block: empty list" values)

let scroll_padding_block_start value = v Scroll_padding_block_start value
let scroll_padding_block_end value = v Scroll_padding_block_end value

let overscroll_behavior values =
  v Overscroll_behavior
    (require_nonempty "overscroll_behavior: empty list" values)

let overscroll_behavior_x value = v Overscroll_behavior_x value
let overscroll_behavior_y value = v Overscroll_behavior_y value
let accent_color value = v Accent_color value
let caret_color value = v Caret_color value
let text_decoration_color value = v Text_decoration_color value
let text_decoration_thickness value = v Text_decoration_thickness value
let text_size_adjust value = v Text_size_adjust value
let aspect_ratio a = v Aspect_ratio a
let filter value = v Filter value

let filter_var_empty name : filter =
  Var
    {
      name;
      fallback = Empty;
      default = None;
      layer = None;
      meta = None;
      runtime = false;
    }

let background_image_var_none name : background_image =
  Var
    {
      name;
      fallback = None;
      default = None;
      layer = None;
      meta = None;
      runtime = false;
    }

let word_spacing value = v Word_spacing value
let quotes value = v Quotes value

let border ?width ?style ?color () =
  let border_value : border =
    match (width, style, color) with
    | None, None, None -> None
    | _ -> Shorthand { width; style; color }
  in
  v Border border_value

let border_block value = v Border_block value
let tab_size value = v Tab_size (Int value : tab_size)
let tab_size_value value = v Tab_size (value : tab_size)
let scrollbar_width value = v Scrollbar_width value
let scrollbar_color value = v Scrollbar_color value
let scrollbar_gutter value = v Scrollbar_gutter value
let zoom value = v Zoom value
let webkit_text_size_adjust value = v Webkit_text_size_adjust value
let font_feature_settings value = v Font_feature_settings value
let font_variation_settings value = v Font_variation_settings value
let webkit_tap_highlight_color value = v Webkit_tap_highlight_color value
let webkit_text_decoration value = v Webkit_text_decoration value
let webkit_text_decoration_color value = v Webkit_text_decoration_color value
let text_indent value = v Text_indent value
let border_collapse value = v Border_collapse value
let list_style value = v List_style value
let font value = v Font value
let webkit_appearance value = v Webkit_appearance value
let transform_style value = v Transform_style value
let backface_visibility value = v Backface_visibility value
let object_position value = v Object_position value
let transition_duration value = v Transition_duration value
let transition_timing_function value = v Transition_timing_function value
let transition_delay value = v Transition_delay value
let transition_behavior value = v Transition_behavior value
let transition_property value = v Transition_property value

(* Additional v constructors to match the interface *)
let mix_blend_mode value = v Mix_blend_mode value
let grid_template_columns value = v Grid_template_columns value
let grid_template_rows value = v Grid_template_rows value
let grid_auto_flow value = v Grid_auto_flow value
let pointer_events value = v Pointer_events value
let z_index value = v Z_index value
let appearance value = v Appearance value
let overflow_x value = v Overflow_x value
let overflow_y value = v Overflow_y value
let resize value = v Resize value
let vertical_align value = v Vertical_align value
let box_sizing value = v Box_sizing value
let field_sizing value = v Field_sizing value
let caption_side value = v Caption_side value
let font_family value = v Font_family value
let print_color_adjust value = v Print_color_adjust value
let webkit_print_color_adjust value = v Webkit_print_color_adjust value
let box_decoration_break value = v Box_decoration_break value
let webkit_box_decoration_break value = v Webkit_box_decoration_break value
let background_origin value = v Background_origin value
let background_clip value = v Background_clip value
let webkit_background_clip value = v Webkit_background_clip value

let font_families fonts =
  v Font_family (List (require_nonempty "font_families: empty list" fonts))

let background_attachment value = v Background_attachment value
let border_top value = v Border_top value
let border_right value = v Border_right value
let border_bottom value = v Border_bottom value
let border_left value = v Border_left value
let transform_origin value = v Transform_origin value
let transform_box value = v Transform_box value
let clip_path value = v Clip_path value
let mask value = v Mask value
let webkit_mask_image value = v Webkit_mask_image value
let mask_image value = v Mask_image value
let webkit_mask_composite value = v Webkit_mask_composite value
let mask_composite value = v Mask_composite value
let webkit_mask_source_type value = v Webkit_mask_source_type value
let mask_mode value = v Mask_mode value
let mask_type value = v Mask_type value
let webkit_mask_size value = v Webkit_mask_size value
let mask_size value = v Mask_size value

let webkit_mask_position values =
  v Webkit_mask_position
    (require_nonempty "webkit_mask_position: empty list" values)

let mask_position values =
  v Mask_position (require_nonempty "mask_position: empty list" values)

let webkit_mask_repeat value = v Webkit_mask_repeat value
let mask_repeat value = v Mask_repeat value
let webkit_mask_clip value = v Webkit_mask_clip value
let mask_clip value = v Mask_clip value
let webkit_mask_origin value = v Webkit_mask_origin value
let mask_origin value = v Mask_origin value
let content_visibility value = v Content_visibility value
let moz_osx_font_smoothing value = v Moz_osx_font_smoothing value
let webkit_line_clamp value = v Webkit_line_clamp value
let webkit_box_orient value = v Webkit_box_orient value
let text_overflow value = v Text_overflow value
let text_wrap value = v Text_wrap value
let text_wrap_mode value = v Text_wrap_mode value
let text_underline_position value = v Text_underline_position value
let text_box_edge value = v Text_box_edge value
let inline_sizing value = v Inline_sizing value
let line_fit_edge value = v Line_fit_edge value
let interpolate_size value = v Interpolate_size value
let min_intrinsic_sizing value = v Min_intrinsic_sizing value
let ruby_align value = v Ruby_align value
let ruby_merge value = v Ruby_merge value
let ruby_overhang value = v Ruby_overhang value
let ruby_position value = v Ruby_position value
let glyph_orientation_vertical value = v Glyph_orientation_vertical value
let word_break value = v Word_break value
let overflow_wrap value = v Overflow_wrap value
let line_break value = v Line_break value
let hyphens value = v Hyphens value
let webkit_hyphens value = v Webkit_hyphens value
let font_stretch value = v Font_stretch value
let font_optical_sizing value = v Font_optical_sizing value
let font_kerning value = v Font_kerning value
let font_language_override value = v Font_language_override value
let font_synthesis_style value = v Font_synthesis_style value
let font_synthesis_weight value = v Font_synthesis_weight value
let font_synthesis_small_caps value = v Font_synthesis_small_caps value
let font_synthesis_position value = v Font_synthesis_position value
let font_variant_ligatures value = v Font_variant_ligatures value
let font_variant_caps value = v Caps value
let font_variant_numeric value = v Numeric value
let font_variant_position value = v Font_variant_position value
let font_variant_east_asian value = v East_asian value
let backdrop_filter value = v Backdrop_filter value
let webkit_backdrop_filter value = v Webkit_backdrop_filter value

let background_position values =
  v Background_position
    (require_nonempty "background_position: empty list" values)

let background_repeat value = v Background_repeat value
let background_size value = v Background_size value
let content value = v Content value
let counter_reset value = v Counter_reset value
let counter_increment value = v Counter_increment value
let border_left_width value = v Border_left_width value
let border_inline_start_width value = v Border_inline_start_width value
let border_inline_end_width value = v Border_inline_end_width value
let border_block_start_width value = v Border_block_start_width value
let border_block_end_width value = v Border_block_end_width value
let border_bottom_width value = v Border_bottom_width value
let border_top_width value = v Border_top_width value
let border_right_width value = v Border_right_width value
let border_top_color value = v Border_top_color value
let border_right_color value = v Border_right_color value
let border_bottom_color value = v Border_bottom_color value
let border_left_color value = v Border_left_color value
let border_inline_start_color value = v Border_inline_start_color value
let border_block_start_color value = v Border_block_start_color value
let border_block_end_color value = v Border_block_end_color value
let border_inline_end_color value = v Border_inline_end_color value
let border_inline_color value = v Border_inline_color value
let border_block_color value = v Border_block_color value
let border_inline_width value = v Border_inline_width value
let border_block_width value = v Border_block_width value
let border_inline_style value = v Border_inline_style value
let border_block_style value = v Border_block_style value
let border_inline_start_style value = v Border_inline_start_style value
let border_inline_end_style value = v Border_inline_end_style value
let border_block_start_style value = v Border_block_start_style value
let border_block_end_style value = v Border_block_end_style value
let border_start_start_radius value = v Border_start_start_radius [ value ]
let border_start_end_radius value = v Border_start_end_radius [ value ]
let border_end_start_radius value = v Border_end_start_radius [ value ]
let border_end_end_radius value = v Border_end_end_radius [ value ]
let webkit_font_smoothing value = v Webkit_font_smoothing value
let cursor value = v Cursor value
let user_select value = v User_select value
let webkit_user_select value = v Webkit_user_select value
let container_type value = v Container_type value

let container_name_of_string value : container_name =
  let names = String.split_on_char ' ' value |> List.filter (( <> ) "") in
  match names with [ "none" ] -> None | _ -> Names names

let container_name value = v Container_name (container_name_of_string value)

let container ?type_ name =
  v Container
    (Shorthand { name = container_name_of_string name; ctype = type_ })

let transform value = v Transform [ value ]

let transforms values =
  v Transform (require_nonempty "transforms: empty list" values)

let rotate (value : Properties_intf.rotate_value) = v Rotate value
let scale (value : Properties_intf.scale) = v Scale value
let translate (value : Properties_intf.translate_value) = v Translate value
let perspective value = v Perspective value
let perspective_origin value = v Perspective_origin value

let padding_inline values =
  v Padding_inline (require_nonempty "padding_inline: empty list" values)

let padding_inline_start value = v Padding_inline_start value
let padding_inline_end value = v Padding_inline_end value

let padding_block values =
  v Padding_block (require_nonempty "padding_block: empty list" values)

let padding_block_start value = v Padding_block_start value
let padding_block_end value = v Padding_block_end value
let margin_inline value = v Margin_inline [ value ]
let margin_inline_start value = v Margin_inline_start value
let margin_inline_end value = v Margin_inline_end value
let margin_block value = v Margin_block [ value ]
let margin_block_start value = v Margin_block_start value
let margin_block_end value = v Margin_block_end value
let will_change value = v Will_change value
let contain value = v Contain value
let isolation value = v Isolation value
let break_before value = v Break_before value
let break_after value = v Break_after value
let break_inside value = v Break_inside value
let page_break_before value = v Page_break_before value
let page_break_after value = v Page_break_after value
let page_break_inside value = v Page_break_inside value
let columns value = v Columns value
let column_rule value = v Column_rule value
let column_span value = v Column_span value
let outline value = v Outline value
let outline_offset len = v Outline_offset len
let scroll_snap_type value = v Scroll_snap_type value
let scroll_snap_align value = v Scroll_snap_align value
let scroll_snap_stop value = v Scroll_snap_stop value
let scroll_behavior value = v Scroll_behavior value
let color_scheme value = v Color_scheme value
let image_rendering value = v Image_rendering value
let image_resolution value = v Image_resolution value

(* Alignment constructor helpers (declarations) *)
