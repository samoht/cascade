(** Structured media conditions for type-safe media query construction.

    Implements CSS Media Queries Level 4/5:
    {v
    <media-query> = <media-condition>
                  | [ not | only ]? <media-type> [ and <media-condition-without-or> ]?
    <media-condition> = <media-not> | <media-in-parens> [ <media-and>* | <media-or>* ]
    <media-condition-without-or> = <media-not> | <media-in-parens> <media-and>*
    <media-not> = not <media-in-parens>
    <media-and> = and <media-in-parens>
    <media-or>  = or  <media-in-parens>
    <media-in-parens> = ( <media-condition> ) | <media-feature> | <general-enclosed>
    <media-feature> = ( <mf-plain> | <mf-boolean> | <mf-range> )
    <mf-range> = <mf-name> <op> <mf-value>
               | <mf-value> <op> <mf-name>
               | <mf-value> <lt-op> <mf-name> <lt-op> <mf-value>
               | <mf-value> <gt-op> <mf-name> <gt-op> <mf-value>
    v} *)

type cmp = Lt | Le | Eq | Gt | Ge

type name =
  | Width
  | Height
  | Inline_size
  | Block_size
  | Aspect_ratio
  | Resolution
  | Color
  | Color_index
  | Monochrome
  | Grid
  | Horizontal_viewport_segments
  | Vertical_viewport_segments
  | Orientation
  | Hover
  | Any_hover
  | Pointer
  | Any_pointer
  | Update
  | Overflow_block
  | Overflow_inline
  | Scan
  | Color_gamut
  | Video_color_gamut
  | Dynamic_range
  | Video_dynamic_range
  | Display_mode
  | Environment_blending
  | Prefers_color_scheme
  | Prefers_reduced_motion
  | Prefers_reduced_transparency
  | Prefers_reduced_data
  | Prefers_contrast
  | Forced_colors
  | Inverted_colors
  | Nav_controls
  | Scripting
  | Min of name
  | Max of name
  | Other of string

type ident =
  | Infinite
  | Portrait
  | Landscape
  | None
  | Hover
  | Coarse
  | Fine
  | Slow
  | Fast
  | Interlace
  | Progressive
  | Srgb
  | P3
  | Rec2020
  | Standard
  | High
  | Optional_paged
  | Paged
  | Scroll
  | Fullscreen
  | Standalone
  | Minimal_ui
  | Browser
  | Picture_in_picture
  | Opaque
  | Additive
  | Subtractive
  | Light
  | Dark
  | No_preference
  | Reduce
  | Less
  | More
  | Custom
  | Active
  | Inverted
  | Back
  | Initial_only
  | Enabled
  | Other of string

type value =
  | Length of Values_intf.length
  | Integer of int
  | Number of float
  | Ratio of int * int
  | Resolution_value of float * string
  | Ident of ident
  | Function of string * string
      (** [env(--name)] / [var(--name)] / [attr(...)] / [calc(...)] etc.
          captured as a function name plus its raw argument body for round-trip;
          cascade doesn't yet evaluate them at parse time. *)

let equal_value (a : value) b = a = b

type feature =
  | Plain of name * value
  | Boolean of name
  | Range of name * cmp * value
  | Range_rev of value * cmp * name
  | Interval of value * cmp * name * cmp * value
  | General_enclosed of string
      (** Media Queries 4 sec. 3.1 [<general-enclosed>]: a grammatical but
          unrecognised query, kept verbatim. Its result is [unknown], which
          becomes false wherever a boolean is expected. *)

type condition =
  | Feature of feature
  | Not of condition
  | And of condition * condition
  | Or of condition * condition

type medium = All | Screen | Print | Other of string
type prefix = Not | Only

let equal_name (a : name) b = a = b

type t =
  | Cond of condition
  | Type of {
      prefix : prefix option;
      type_ : medium;
      trailing : condition option;
    }
  | List of t list  (** Comma-separated media query list. *)

(* ===== Formatting helpers ===== *)

let format_float f =
  if Float.is_integer f then Int.to_string (Float.to_int f)
  else Float.to_string f

let string_of_cmp = function
  | Lt -> "<"
  | Le -> "<="
  | Eq -> "="
  | Gt -> ">"
  | Ge -> ">="

let string_of_medium : medium -> string = function
  | All -> "all"
  | Screen -> "screen"
  | Print -> "print"
  | Other s -> Parser.escape_ident s

let rec string_of_name : name -> string = function
  | Width -> "width"
  | Height -> "height"
  | Inline_size -> "inline-size"
  | Block_size -> "block-size"
  | Aspect_ratio -> "aspect-ratio"
  | Resolution -> "resolution"
  | Color -> "color"
  | Color_index -> "color-index"
  | Monochrome -> "monochrome"
  | Grid -> "grid"
  | Horizontal_viewport_segments -> "horizontal-viewport-segments"
  | Vertical_viewport_segments -> "vertical-viewport-segments"
  | Orientation -> "orientation"
  | Hover -> "hover"
  | Any_hover -> "any-hover"
  | Pointer -> "pointer"
  | Any_pointer -> "any-pointer"
  | Update -> "update"
  | Overflow_block -> "overflow-block"
  | Overflow_inline -> "overflow-inline"
  | Scan -> "scan"
  | Color_gamut -> "color-gamut"
  | Video_color_gamut -> "video-color-gamut"
  | Dynamic_range -> "dynamic-range"
  | Video_dynamic_range -> "video-dynamic-range"
  | Display_mode -> "display-mode"
  | Environment_blending -> "environment-blending"
  | Prefers_color_scheme -> "prefers-color-scheme"
  | Prefers_reduced_motion -> "prefers-reduced-motion"
  | Prefers_reduced_transparency -> "prefers-reduced-transparency"
  | Prefers_reduced_data -> "prefers-reduced-data"
  | Prefers_contrast -> "prefers-contrast"
  | Forced_colors -> "forced-colors"
  | Inverted_colors -> "inverted-colors"
  | Nav_controls -> "nav-controls"
  | Scripting -> "scripting"
  | Min name -> "min-" ^ string_of_name name
  | Max name -> "max-" ^ string_of_name name
  | Other s -> Parser.escape_ident s

let rec name_of_string s : name =
  let s = String.lowercase_ascii s in
  let starts_with ~prefix =
    let len = String.length prefix in
    String.length s > len && String.sub s 0 len = prefix
  in
  if starts_with ~prefix:"min-" then
    Min (name_of_string (String.sub s 4 (String.length s - 4)))
  else if starts_with ~prefix:"max-" then
    Max (name_of_string (String.sub s 4 (String.length s - 4)))
  else
    match s with
    | "width" -> Width
    | "height" -> Height
    | "inline-size" -> Inline_size
    | "block-size" -> Block_size
    | "aspect-ratio" -> Aspect_ratio
    | "resolution" -> Resolution
    | "color" -> Color
    | "color-index" -> Color_index
    | "monochrome" -> Monochrome
    | "grid" -> Grid
    | "horizontal-viewport-segments" -> Horizontal_viewport_segments
    | "vertical-viewport-segments" -> Vertical_viewport_segments
    | "orientation" -> Orientation
    | "hover" -> Hover
    | "any-hover" -> Any_hover
    | "pointer" -> Pointer
    | "any-pointer" -> Any_pointer
    | "update" -> Update
    | "overflow-block" -> Overflow_block
    | "overflow-inline" -> Overflow_inline
    | "scan" -> Scan
    | "color-gamut" -> Color_gamut
    | "video-color-gamut" -> Video_color_gamut
    | "dynamic-range" -> Dynamic_range
    | "video-dynamic-range" -> Video_dynamic_range
    | "display-mode" -> Display_mode
    | "environment-blending" -> Environment_blending
    | "prefers-color-scheme" -> Prefers_color_scheme
    | "prefers-reduced-motion" -> Prefers_reduced_motion
    | "prefers-reduced-transparency" -> Prefers_reduced_transparency
    | "prefers-reduced-data" -> Prefers_reduced_data
    | "prefers-contrast" -> Prefers_contrast
    | "forced-colors" -> Forced_colors
    | "inverted-colors" -> Inverted_colors
    | "nav-controls" -> Nav_controls
    | "scripting" -> Scripting
    | _ -> Other s

let string_of_ident : ident -> string = function
  | Infinite -> "infinite"
  | Portrait -> "portrait"
  | Landscape -> "landscape"
  | None -> "none"
  | Hover -> "hover"
  | Coarse -> "coarse"
  | Fine -> "fine"
  | Slow -> "slow"
  | Fast -> "fast"
  | Interlace -> "interlace"
  | Progressive -> "progressive"
  | Srgb -> "srgb"
  | P3 -> "p3"
  | Rec2020 -> "rec2020"
  | Standard -> "standard"
  | High -> "high"
  | Optional_paged -> "optional-paged"
  | Paged -> "paged"
  | Scroll -> "scroll"
  | Fullscreen -> "fullscreen"
  | Standalone -> "standalone"
  | Minimal_ui -> "minimal-ui"
  | Browser -> "browser"
  | Picture_in_picture -> "picture-in-picture"
  | Opaque -> "opaque"
  | Additive -> "additive"
  | Subtractive -> "subtractive"
  | Light -> "light"
  | Dark -> "dark"
  | No_preference -> "no-preference"
  | Reduce -> "reduce"
  | Less -> "less"
  | More -> "more"
  | Custom -> "custom"
  | Active -> "active"
  | Inverted -> "inverted"
  | Back -> "back"
  | Initial_only -> "initial-only"
  | Enabled -> "enabled"
  | Other s -> Parser.escape_ident s

let ident_of_string s : ident =
  match String.lowercase_ascii s with
  | "infinite" -> Infinite
  | "portrait" -> Portrait
  | "landscape" -> Landscape
  | "none" -> None
  | "hover" -> Hover
  | "coarse" -> Coarse
  | "fine" -> Fine
  | "slow" -> Slow
  | "fast" -> Fast
  | "interlace" -> Interlace
  | "progressive" -> Progressive
  | "srgb" -> Srgb
  | "p3" -> P3
  | "rec2020" -> Rec2020
  | "standard" -> Standard
  | "high" -> High
  | "optional-paged" -> Optional_paged
  | "paged" -> Paged
  | "scroll" -> Scroll
  | "fullscreen" -> Fullscreen
  | "standalone" -> Standalone
  | "minimal-ui" -> Minimal_ui
  | "browser" -> Browser
  | "picture-in-picture" -> Picture_in_picture
  | "opaque" -> Opaque
  | "additive" -> Additive
  | "subtractive" -> Subtractive
  | "light" -> Light
  | "dark" -> Dark
  | "no-preference" -> No_preference
  | "reduce" -> Reduce
  | "less" -> Less
  | "more" -> More
  | "custom" -> Custom
  | "active" -> Active
  | "inverted" -> Inverted
  | "back" -> Back
  | "initial-only" -> Initial_only
  | "enabled" -> Enabled
  | other -> Other other

(* ===== Pretty printing ===== *)

let pp_length ctx l = Values.pp_length ~always:true ctx l

let pp_value : value Pp.t =
 fun ctx -> function
  | Length l -> pp_length ctx l
  | Integer i -> Pp.string ctx (Int.to_string i)
  | Number f -> Pp.string ctx (format_float f)
  | Ratio (n, 1) when Pp.minified ctx -> Pp.string ctx (Int.to_string n)
  | Ratio (n, 1) ->
      Pp.string ctx (Int.to_string n);
      Pp.string ctx "/1"
  | Ratio (n, d) ->
      Pp.string ctx (Int.to_string n);
      Pp.char ctx '/';
      Pp.string ctx (Int.to_string d)
  | Resolution_value (n, unit) ->
      Pp.string ctx (format_float n);
      Pp.string ctx unit
  | Ident s -> Pp.string ctx (string_of_ident s)
  | Function (name, args) ->
      Pp.string ctx name;
      Pp.char ctx '(';
      Pp.string ctx args;
      Pp.char ctx ')'

let pp_feature : feature Pp.t =
 fun ctx -> function
  | General_enclosed raw -> Pp.string ctx raw
  | Plain (name, value) when Pp.minified ctx ->
      Pp.char ctx '(';
      Pp.string ctx (string_of_name name);
      Pp.char ctx ':';
      pp_value ctx value;
      Pp.char ctx ')'
  | Plain (name, value) ->
      Pp.char ctx '(';
      Pp.string ctx (string_of_name name);
      Pp.char ctx ':';
      Pp.space_if_pretty ctx ();
      pp_value ctx value;
      Pp.char ctx ')'
  | Boolean name ->
      Pp.char ctx '(';
      Pp.string ctx (string_of_name name);
      Pp.char ctx ')'
  | Range (name, op, value) ->
      (* CSS Media Queries 4 3.2: relational operators are their own tokens, so
         the surrounding whitespace is optional and minify drops it. *)
      Pp.char ctx '(';
      Pp.string ctx (string_of_name name);
      Pp.sp ctx ();
      Pp.string ctx (string_of_cmp op);
      Pp.sp ctx ();
      pp_value ctx value;
      Pp.char ctx ')'
  | Range_rev (value, op, name) ->
      Pp.char ctx '(';
      pp_value ctx value;
      Pp.sp ctx ();
      Pp.string ctx (string_of_cmp op);
      Pp.sp ctx ();
      Pp.string ctx (string_of_name name);
      Pp.char ctx ')'
  | Interval (a, op1, name, op2, b) ->
      Pp.char ctx '(';
      pp_value ctx a;
      Pp.sp ctx ();
      Pp.string ctx (string_of_cmp op1);
      Pp.sp ctx ();
      Pp.string ctx (string_of_name name);
      Pp.sp ctx ();
      Pp.string ctx (string_of_cmp op2);
      Pp.sp ctx ();
      pp_value ctx b;
      Pp.char ctx ')'

(* CSS Media Queries 4 sec. 2.4.3: a lower and an upper bound on the same
   feature combine into the two-sided [<value> <op> <name> <op> <value>]
   interval. [feature_bound] normalises a single-bound feature into a [(name,
   side, op, value)] view so two bounds can be paired. *)
type bound_side = Lower | Upper

let feature_bound (f : feature) : (name * bound_side * cmp * value) option =
  let norm : feature option =
    match f with
    | Plain (Min base, v) -> Some (Range (base, Ge, v))
    | Plain (Max base, v) -> Some (Range (base, Le, v))
    | (Range _ | Range_rev _) as r -> Some r
    | _ -> None
  in
  match norm with
  | Some (Range (name, Ge, v)) -> Some (name, Lower, Le, v)
  | Some (Range (name, Gt, v)) -> Some (name, Lower, Lt, v)
  | Some (Range (name, Le, v)) -> Some (name, Upper, Le, v)
  | Some (Range (name, Lt, v)) -> Some (name, Upper, Lt, v)
  | Some (Range_rev (v, Le, name)) -> Some (name, Lower, Le, v)
  | Some (Range_rev (v, Lt, name)) -> Some (name, Lower, Lt, v)
  | Some (Range_rev (v, Ge, name)) -> Some (name, Upper, Le, v)
  | Some (Range_rev (v, Gt, name)) -> Some (name, Upper, Lt, v)
  | _ -> None

let merge_interval_bounds (a : feature) (b : feature) : feature option =
  match (feature_bound a, feature_bound b) with
  | Some (n1, Lower, lop, lv), Some (n2, Upper, uop, uv) when n1 = n2 ->
      Some (Interval (lv, lop, n1, uop, uv))
  | Some (n1, Upper, uop, uv), Some (n2, Lower, lop, lv) when n1 = n2 ->
      Some (Interval (lv, lop, n1, uop, uv))
  | _ -> None

(* CSS Media Queries 4 sec. 3: every operand of [not], [and] and [or] is a
   [<media-in-parens>], so a compound operand keeps its wrapper. A feature is
   already parenthesised and a [<general-enclosed>] is its own block, so both
   stand alone. The left of a chain is the exception: [<media-in-parens>
   <media-and>*] lets a same-operator chain continue unwrapped. *)
let rec pp_condition : condition Pp.t =
 fun ctx -> function
  | Feature f -> pp_feature ctx f
  | Not c ->
      Pp.string ctx "not ";
      pp_in_parens ctx c
  | And (a, b) ->
      (match a with And _ -> pp_condition ctx a | _ -> pp_in_parens ctx a);
      Pp.string ctx (if Pp.minified ctx then "and " else " and ");
      pp_in_parens ctx b
  | Or (a, b) ->
      (match a with Or _ -> pp_condition ctx a | _ -> pp_in_parens ctx a);
      Pp.string ctx (if Pp.minified ctx then "or " else " or ");
      pp_in_parens ctx b

and pp_in_parens : condition Pp.t =
 fun ctx -> function
  | Feature f -> pp_feature ctx f
  | (Not _ | And _ | Or _) as c ->
      Pp.char ctx '(';
      pp_condition ctx c;
      Pp.char ctx ')'

let rec pp : t Pp.t =
 fun ctx -> function
  | Cond c -> pp_condition ctx c
  (* CSS Mediaqueries 4 sec. 3: [all and X] without an explicit prefix ([not] /
     [only]) is equivalent to just [X]. *)
  | Type { prefix = None; type_ = All; trailing = Some c } when Pp.minified ctx
    ->
      pp_condition ctx c
  | Type { prefix; type_; trailing } -> (
      (match prefix with
      | None -> ()
      | Some Not -> Pp.string ctx "not "
      | Some Only -> Pp.string ctx "only ");
      Pp.string ctx (string_of_medium type_);
      match trailing with
      | None -> ()
      | Some c -> (
          Pp.string ctx " and ";
          (* CSS Media Queries 4 sec. 3 [media-condition-without-or]: Or
             sub-expressions need explicit parens in this context. *)
          match c with
          | Or _ -> pp_in_parens ctx c
          | Feature _ | Not _ | And _ -> pp_condition ctx c))
  | List qs ->
      let rec loop = function
        | [] -> ()
        | [ q ] -> pp ctx q
        | q :: rest ->
            pp ctx q;
            Pp.char ctx ',';
            Pp.space_if_pretty ctx ();
            loop rest
      in
      loop qs

let to_string ?(minify = false) t = Pp.to_string ~minify pp t

(* ===== Parser ===== *)

type recovery_scope = Branch | Query_list

(* A branch failure is buffered rather than raised straight out: [of_components]
   decides from [recover] whether to swallow it into [not all] or re-raise it,
   so the error travels as a value with the span already attached. *)
exception Parse_error of recovery_scope * Error.t

let fail_parse ?(scope = Branch) e = raise (Parse_error (scope, e))

(* Anchoring a failure on the components that failed puts the caret on that
   slice of the query. [t] anchors the smallest enclosing construct, for a
   failure that has no components of its own. *)
let err ?scope t cvs reason =
  let at = match cvs with [] -> t | _ :: _ -> Cursor.sub t cvs in
  fail_parse ?scope (Cursor.condition_error at ~at_rule:"@media" reason)

let rec drop_whitespace = function
  | component :: rest when Component.is_whitespace component ->
      drop_whitespace rest
  | components -> components

let trim_components components =
  components |> drop_whitespace |> List.rev |> drop_whitespace |> List.rev

let non_whitespace_components = List.filter (Fun.negate Component.is_whitespace)

let components_empty components =
  match trim_components components with [] -> true | _ :: _ -> false

let string_of_components components =
  Cursor.string_of_components ~trim:true components

let ident_component = function
  | Component.Preserved { kind = Token.Ident name; _ } -> Some name
  | _ -> Option.None

let ident_is keyword component =
  match ident_component component with
  | Some name -> String.equal (String.lowercase_ascii name) keyword
  | Option.None -> false

let length_of_value f unit : Values_intf.length option =
  let module L = Values_intf in
  match String.lowercase_ascii unit with
  | "px" -> Some (L.Px f)
  | "em" -> Some (L.Em f)
  | "rem" -> Some (L.Rem f)
  | "%" -> Some (L.Pct f)
  | "vw" -> Some (L.Vw f)
  | "vh" -> Some (L.Vh f)
  | "vmin" -> Some (L.Vmin f)
  | "vmax" -> Some (L.Vmax f)
  | "ch" -> Some (L.Ch f)
  | "ex" -> Some (L.Ex f)
  | "cm" -> Some (L.Cm f)
  | "mm" -> Some (L.Mm f)
  | "in" -> Some (L.In f)
  | "pt" -> Some (L.Pt f)
  | "pc" -> Some (L.Pc f)
  | "lh" -> Some (L.Lh f)
  | "q" -> Some (L.Q f)
  | "cap" -> Some (L.Cap f)
  | "ic" -> Some (L.Ic f)
  | "rlh" -> Some (L.Rlh f)
  | "vi" -> Some (L.Vi f)
  | "vb" -> Some (L.Vb f)
  | "dvh" -> Some (L.Dvh f)
  | "dvw" -> Some (L.Dvw f)
  | "dvmin" -> Some (L.Dvmin f)
  | "dvmax" -> Some (L.Dvmax f)
  | "lvh" -> Some (L.Lvh f)
  | "lvw" -> Some (L.Lvw f)
  | "lvmin" -> Some (L.Lvmin f)
  | "lvmax" -> Some (L.Lvmax f)
  | "svh" -> Some (L.Svh f)
  | "svw" -> Some (L.Svw f)
  | "svmin" -> Some (L.Svmin f)
  | "svmax" -> Some (L.Svmax f)
  | _ -> Option.None

let resolution_units = [ "dpi"; "dpcm"; "dppx"; "x" ]

let typed_function_value component : value option =
  let cursor = Cursor.of_components [ component ] in
  match Values.read_length cursor with
  | length ->
      Cursor.ws cursor;
      Cursor.expect_eof cursor;
      Some (Length length)
  | exception Cursor.Parse_error _ -> Option.None

let value_of_number (number : Token.number) =
  match number.number_flag with
  | Token.Integer ->
      Option.map (fun value -> Integer value) (Token.integer_opt number)
  | Token.Number -> Some (Number number.value)

let read_value_with_unit value unit : value option =
  match length_of_value value unit with
  | Some l -> Some (Length l)
  | Option.None ->
      if List.mem (String.lowercase_ascii unit) resolution_units then
        Some (Resolution_value (value, unit))
      else Option.None

let value_of_components_opt components =
  match non_whitespace_components components with
  | [ Component.Preserved { kind = Token.Number_tok number; _ } ] ->
      value_of_number number
  | [ Component.Preserved { kind = Token.Percentage number; _ } ] ->
      Some (Length (Values_intf.Pct number.value))
  | [ Component.Preserved { kind = Token.Dimension { number; unit_ }; _ } ] ->
      read_value_with_unit number.value unit_
  | [
   Component.Preserved { kind = Token.Number_tok numerator; _ };
   Component.Preserved { kind = Token.Delim "/"; _ };
   Component.Preserved
     {
       kind =
         Token.Number_tok
           { value = denominator; number_flag = Token.Integer; _ };
       _;
     };
  ] ->
      Some (Ratio (int_of_float numerator.value, int_of_float denominator))
  | [ Component.Preserved { kind = Token.Ident name; _ } ] ->
      Some (Ident (ident_of_string name))
  | [
   (Component.Func { node = { name; arguments; terminated = true }; _ } as
    component);
  ] -> (
      match typed_function_value component with
      | Some _ as value -> value
      | Option.None -> Some (Function (name, string_of_components arguments)))
  | _ -> Option.None

let value_of_string s =
  let cursor = Cursor.of_string s in
  match value_of_components_opt (Cursor.remaining cursor) with
  | Some value -> value
  | Option.None -> Cursor.err_invalid cursor "media value"

let boolean_feature name : feature = Boolean name

let interval_ops_compatible op1 op2 =
  match (op1, op2) with
  | (Lt | Le), (Lt | Le) | (Gt | Ge), (Gt | Ge) -> true
  | _ -> false

let range_feature_name (name : name) =
  match name with
  | Width | Height | Inline_size | Block_size | Aspect_ratio | Resolution
  | Color | Color_index | Monochrome | Horizontal_viewport_segments
  | Vertical_viewport_segments ->
      true
  | _ -> false

let prefixed_range_feature_name name : name option =
  match name with Min base | Max base -> Some base | _ -> Option.None

(* CSS Values 4 sec. 5 writes a zero [<length>] as the unitless number [0], and
   Media Queries 4 sec. 1.3 takes its units from CSS Values, so a length-typed
   feature accepts a bare zero. The allowance is [<length>]-only: a
   [<resolution>] keeps its unit, which is why [(min-resolution: 0)] stays
   invalid. *)
let length_feature_name (name : name) =
  match name with
  | Width | Height | Inline_size | Block_size -> true
  | _ -> false

let unitless_zero_length (name : name) value =
  let base =
    match prefixed_range_feature_name name with
    | Some base -> base
    | None -> name
  in
  match value with
  | (Integer 0 | Number 0.) when length_feature_name base ->
      Length (Values_intf.Px 0.)
  | _ -> value

let validate_plain_feature (name : name) value =
  let plain_name =
    match prefixed_range_feature_name name with
    | Some base when range_feature_name base -> base
    | Some _ -> name
    | None -> name
  in
  (* [env()] / [var()] / [calc()] resolve their type at use time, so accept them
     wherever a typed numeric is allowed and let the consumer validate. *)
  let is_typed_function = function Function _ -> true | _ -> false in
  let valid_numeric_value (name : name) value =
    is_typed_function value
    ||
    match (name, value) with
    | (Width | Height | Inline_size | Block_size), Length _ -> true
    | Aspect_ratio, (Ratio _ | Integer _) -> true
    | Resolution, Resolution_value _ -> true
    | Resolution, Ident Infinite -> true
    | (Color | Color_index | Monochrome), Integer n -> n >= 0
    | (Horizontal_viewport_segments | Vertical_viewport_segments), Integer n ->
        n >= 0
    | _ -> false
  in
  let valid_plain_numeric_value (name : name) value =
    is_typed_function value
    ||
    match (name, value) with
    | ( (Width | Height | Min Width | Max Width | Inline_size | Block_size),
        Length _ ) ->
        true
    | Aspect_ratio, (Ratio _ | Integer _) -> true
    | Resolution, Resolution_value _ -> true
    | Resolution, Ident Infinite -> true
    | (Color | Color_index | Monochrome), Integer n -> n >= 0
    | Grid, Integer (0 | 1) -> true
    | (Horizontal_viewport_segments | Vertical_viewport_segments), Integer n ->
        n >= 0
    | _ -> false
  in
  let ident_value = match value with Ident s -> Some s | _ -> None in
  let one_of values =
    match ident_value with Some s -> List.mem s values | None -> false
  in
  match prefixed_range_feature_name name with
  | Some base when range_feature_name base -> valid_numeric_value base value
  | Some _ -> false
  | None -> (
      match plain_name with
      | Width | Height | Inline_size | Block_size | Aspect_ratio | Resolution
      | Color | Color_index | Monochrome | Grid | Horizontal_viewport_segments
      | Vertical_viewport_segments
      | Min Width
      | Max Width ->
          valid_plain_numeric_value name value
      | Orientation -> one_of [ Portrait; Landscape ]
      | Hover | Any_hover -> one_of [ None; Hover ]
      | Pointer | Any_pointer -> one_of [ None; Coarse; Fine ]
      | Update -> one_of [ None; Slow; Fast ]
      | Overflow_block -> one_of [ None; Scroll; Paged ]
      | Overflow_inline -> one_of [ None; Scroll ]
      | Scan -> one_of [ Interlace; Progressive ]
      | Color_gamut | Video_color_gamut -> one_of [ Srgb; P3; Rec2020 ]
      | Dynamic_range | Video_dynamic_range -> one_of [ Standard; High ]
      | Display_mode ->
          one_of
            [ Fullscreen; Standalone; Minimal_ui; Browser; Picture_in_picture ]
      | Environment_blending -> one_of [ Opaque; Additive; Subtractive ]
      | Prefers_color_scheme -> one_of [ Light; Dark ]
      | Prefers_reduced_motion | Prefers_reduced_transparency
      | Prefers_reduced_data ->
          one_of [ No_preference; Reduce ]
      | Prefers_contrast -> one_of [ No_preference; Less; More; Custom ]
      | Forced_colors -> one_of [ None; Active ]
      | Inverted_colors -> one_of [ None; Inverted ]
      | Nav_controls -> one_of [ None; Back ]
      | Scripting -> one_of [ None; Initial_only; Enabled ]
      | _ -> true)

let validate_range_feature name value =
  range_feature_name name && validate_plain_feature name value

(* Smart constructor for [Plain] features: rejects values that
   {!validate_plain_feature} reports as outside the feature's grammar (e.g.
   [feature "orientation" (Ident "sideways")]). *)
let plain_feature name value : feature =
  if not (validate_plain_feature name value) then
    invalid_arg
      ("Media.feature: value rejected by " ^ string_of_name name
     ^ "'s grammar (see Media.validate_plain_feature)");
  Plain (name, value)

let take_cmp = function
  | Component.Preserved { kind = Token.Delim "<"; _ }
    :: Component.Preserved { kind = Token.Delim "="; _ }
    :: rest ->
      Some (Le, rest)
  | Component.Preserved { kind = Token.Delim ">"; _ }
    :: Component.Preserved { kind = Token.Delim "="; _ }
    :: rest ->
      Some (Ge, rest)
  | Component.Preserved { kind = Token.Delim "<"; _ } :: rest -> Some (Lt, rest)
  | Component.Preserved { kind = Token.Delim ">"; _ } :: rest -> Some (Gt, rest)
  | Component.Preserved { kind = Token.Delim "="; _ } :: rest -> Some (Eq, rest)
  | _ -> Option.None

let split_cmp components =
  let rec loop before rest =
    match take_cmp rest with
    | Some (op, after) -> Some (List.rev before, op, after)
    | Option.None -> (
        match rest with
        | [] -> Option.None
        | component :: after -> loop (component :: before) after)
  in
  loop [] (non_whitespace_components components)

type parsed_feature = Valid_feature of feature | Invalid_feature | Not_feature

let valid_or_invalid feature =
  match feature with
  | Some feature -> Valid_feature feature
  | Option.None -> Invalid_feature

let boolean_feature_of_name name =
  let name = name_of_string name in
  if Option.is_some (prefixed_range_feature_name name) then Option.None
  else Some (boolean_feature name)

let plain_feature_of_components name components =
  match value_of_components_opt components with
  | Some value ->
      let name = name_of_string name in
      let value = unitless_zero_length name value in
      if validate_plain_feature name value then Some (plain_feature name value)
      else Option.None
  | Option.None -> Option.None

let name_first_range name op components =
  match (split_cmp components, value_of_components_opt components) with
  | Option.None, Some value ->
      let name = name_of_string name in
      let value = unitless_zero_length name value in
      if validate_range_feature name value then Some (Range (name, op, value))
      else Option.None
  | Some _, _ | Option.None, Option.None -> Option.None

let range_rev_of_components lower op1 = function
  | [ name_component ] -> (
      match ident_component name_component with
      | Some name ->
          let name = name_of_string name in
          let lower = unitless_zero_length name lower in
          if validate_range_feature name lower then
            Some (Range_rev (lower, op1, name))
          else Option.None
      | Option.None -> Option.None)
  | _ -> Option.None

let interval_of_components lower op1 name_components op2 upper_components =
  match (name_components, value_of_components_opt upper_components) with
  | [ name_component ], Some upper -> (
      match ident_component name_component with
      | Some name ->
          let name = name_of_string name in
          let lower = unitless_zero_length name lower in
          let upper = unitless_zero_length name upper in
          if
            interval_ops_compatible op1 op2
            && validate_range_feature name lower
            && validate_range_feature name upper
          then Some (Interval (lower, op1, name, op2, upper))
          else Option.None
      | Option.None -> Option.None)
  | _ -> Option.None

let value_first_range_or_interval lhs op1 rhs =
  match value_of_components_opt lhs with
  | Option.None -> Option.None
  | Some lower -> (
      match split_cmp rhs with
      | Option.None -> range_rev_of_components lower op1 rhs
      | Some (name_components, op2, upper_components) ->
          interval_of_components lower op1 name_components op2 upper_components)

let parse_feature_components components =
  let components = non_whitespace_components components in
  match components with
  | [] -> Invalid_feature
  | [ name_component ] -> (
      match ident_component name_component with
      | Some name -> valid_or_invalid (boolean_feature_of_name name)
      | Option.None -> Not_feature)
  | Component.Preserved { kind = Token.Ident name; _ }
    :: Component.Preserved { kind = Token.Colon; _ }
    :: value_components ->
      valid_or_invalid (plain_feature_of_components name value_components)
  | _ -> (
      match split_cmp components with
      | Some ([ name_component ], op, rhs) -> (
          match ident_component name_component with
          | Some name -> valid_or_invalid (name_first_range name op rhs)
          | Option.None ->
              valid_or_invalid
                (value_first_range_or_interval [ name_component ] op rhs))
      | Some (lhs, op, rhs) ->
          valid_or_invalid (value_first_range_or_interval lhs op rhs)
      | Option.None -> (
          match components with
          | Component.Preserved { kind = Token.Ident _; _ } :: _ -> Not_feature
          | _ -> Not_feature))

let feature_in_components components =
  match parse_feature_components components with
  | Valid_feature feature -> Some feature
  | Invalid_feature | Not_feature -> Option.None

let rec condition_of_components t components : condition =
  let components = non_whitespace_components components in
  match components with
  | first :: rest when ident_is "not" first -> (
      match rest with
      | [ operand ] -> Not (condition_in_parens t operand)
      | _ -> err t rest "trailing content after 'not <media-in-parens>'")
  | first :: rest ->
      let left = condition_in_parens t first in
      condition_chain t Option.None left rest
  | [] -> err t components "empty media condition"

and condition_in_parens t component : condition =
  let general_enclosed () =
    if not (Component.is_any_value [ component ]) then
      err t [ component ] "invalid general-enclosed media condition";
    Feature (General_enclosed (string_of_components [ component ]))
  in
  match component with
  | Component.Block
      { node = { opening = Token.Paren; value; closed = true }; _ } -> (
      match parse_feature_components value with
      | Valid_feature feature -> Feature feature
      | Invalid_feature -> general_enclosed ()
      | Not_feature -> (
          try condition_of_components t value
          with Parse_error _ -> general_enclosed ()))
  | Component.Block { node = { opening = Token.Paren; closed = false; _ }; _ }
    ->
      err t [ component ] "unmatched parenthesis in @media condition"
  | Component.Func { node = { terminated = true; _ }; _ } -> general_enclosed ()
  | Component.Func { node = { terminated = false; _ }; _ } ->
      err t [ component ] "unmatched function in @media condition"
  | Component.Block _ | Component.Preserved _ ->
      err t [ component ] "expected media-in-parens"

and condition_chain t operator acc components : condition =
  match components with
  | [] -> acc
  | keyword :: operand :: rest when ident_is "and" keyword ->
      (match operator with
      | Some `Or -> err t [ keyword ] "mixed 'and'/'or' media condition"
      | Some `And | Option.None -> ());
      let right = condition_in_parens t operand in
      condition_chain t (Some `And) (And (acc, right)) rest
  | keyword :: operand :: rest when ident_is "or" keyword ->
      (match operator with
      | Some `And -> err t [ keyword ] "mixed 'and'/'or' media condition"
      | Some `Or | Option.None -> ());
      let right = condition_in_parens t operand in
      condition_chain t (Some `Or) (Or (acc, right)) rest
  | _ -> err t components "trailing content in media condition"

let medium_of_ident s : medium =
  match String.lowercase_ascii s with
  | "all" -> All
  | "screen" -> Screen
  | "print" -> Print
  | other -> Other other

let is_reserved_media_type_keyword s =
  match String.lowercase_ascii s with "and" | "or" -> true | _ -> false

let is_media_in_parens = function
  | Component.Block { node = { opening = Token.Paren; _ }; _ }
  | Component.Func _ ->
      true
  | Component.Block _ | Component.Preserved _ -> false

let starts_with_condition components =
  match non_whitespace_components components with
  | first :: _ when is_media_in_parens first -> true
  | first :: second :: _ when ident_is "not" first -> is_media_in_parens second
  | _ -> false

let read_query_prefix = function
  | first :: rest when ident_is "not" first -> (Some Not, rest)
  | first :: rest when ident_is "only" first -> (Some Only, rest)
  | components -> (Option.None, components)

let read_media_type_query t components =
  let components = non_whitespace_components components in
  let prefix, components = read_query_prefix components in
  (match (prefix, components) with
  | Some _, (first :: _ as rest)
    when ident_is "not" first || ident_is "only" first ->
      err ~scope:Query_list t rest "duplicate media query prefix"
  | _ -> ());
  match components with
  | name_component :: rest -> (
      match ident_component name_component with
      | Option.None ->
          err t [ name_component ] "expected media type or condition"
      | Some name when is_reserved_media_type_keyword name ->
          err t [ name_component ]
            "reserved media condition keyword cannot be a media type"
      | Some name -> (
          let type_ = medium_of_ident name in
          match rest with
          | [] -> Type { prefix; type_; trailing = Option.None }
          | keyword :: condition when ident_is "and" keyword ->
              Type
                {
                  prefix;
                  type_;
                  trailing = Some (condition_of_components t condition);
                }
          | _ -> err t rest "expected 'and' or end of query after media type"))
  | [] -> err t components "expected media type or condition"

let single_query t components =
  if components_empty components then err t components "empty media query"
  else if starts_with_condition components then
    Cond (condition_of_components t components)
  else read_media_type_query t components

let not_all_query : t =
  Type { prefix = Some Not; type_ = All; trailing = Option.None }

let is_comma_component = function
  | Component.Preserved { kind = Token.Comma; _ } -> true
  | _ -> false

let split_query_list components =
  let rec loop current branches saw_comma = function
    | [] -> (List.rev (List.rev current :: branches), saw_comma)
    | component :: rest when is_comma_component component ->
        loop [] (List.rev current :: branches) true rest
    | component :: rest -> loop (component :: current) branches saw_comma rest
  in
  loop [] [] false components

let parse_query_branch t components =
  (* Anchor the branch, not the whole query list, so a fallback with no
     components of its own still lands inside the branch that failed. *)
  let t = Cursor.sub t components in
  try Ok (single_query t components)
  with Parse_error (scope, e) -> Error (scope, e)

let read ?(recover = true) t =
  let components = Cursor.remaining t in
  let branches, saw_comma = split_query_list components in
  if (not saw_comma) && List.for_all components_empty branches then
    (List [] : t)
  else
    let rec parse acc = function
      | [] -> List.rev acc
      | [ branch ] -> (
          match parse_query_branch t branch with
          | Ok query -> List.rev (query :: acc)
          | Error (_, e) -> if recover then [ not_all_query ] else Error.fail e)
      | branch :: rest -> (
          match parse_query_branch t branch with
          | Ok query -> parse (query :: acc) rest
          | Error (Query_list, e) ->
              if recover then [ not_all_query ] else Error.fail e
          | Error (Branch, e) ->
              if recover then parse (not_all_query :: acc) rest
              else Error.fail e)
    in
    match (saw_comma, parse [] branches) with
    | false, [ query ] -> query
    | false, [] -> List []
    | false, _ :: _ :: _ -> assert false
    | true, [ Type { prefix = Some Not; type_ = All; trailing = Option.None } ]
      ->
        not_all_query
    | true, queries -> List queries

let of_string s = read (Cursor.of_string s)
let of_string_strict s = read ~recover:false (Cursor.of_string s)

let of_function_components components =
  match feature_in_components components with
  | Some feature -> Cond (Feature feature)
  | Option.None -> read (Cursor.of_components components)

let of_function_body s =
  of_function_components (Cursor.remaining (Cursor.of_string s))

let feature name value : t =
  Cond (Feature (plain_feature (name_of_string name) value))

let boolean name : t = Cond (Feature (Boolean (name_of_string name)))

(* CSS Media Queries 4 sec. 2.4.4 / 2.4.3: the optimizer rewrites
   [min-X]/[max-X] into the range form and pairs a lower and upper bound into
   the two-sided interval. Target-fact grammar upgrades, so they live in
   optimize (gated by [~enforce_spec]), not the printer. *)
let lower_feature : feature -> feature = function
  | Plain (Min base, v) -> Range (base, Ge, v)
  | Plain (Max base, v) -> Range (base, Le, v)
  | f -> f

let rec lower_condition : condition -> condition = function
  | Feature f as cond ->
      let f' = lower_feature f in
      if f' == f then cond else Feature f'
  | Not c as cond ->
      let c' = lower_condition c in
      if c' == c then cond else Not c'
  | And (Feature a, Feature b) -> (
      match merge_interval_bounds a b with
      | Some interval -> Feature interval
      | None ->
          let a' = lower_feature a in
          let b' = lower_feature b in
          if a' == a && b' == b then And (Feature a, Feature b)
          else And (Feature a', Feature b'))
  | And (a, b) as cond ->
      let a' = lower_condition a in
      let b' = lower_condition b in
      if a' == a && b' == b then cond else And (a', b')
  | Or (a, b) as cond ->
      let a' = lower_condition a in
      let b' = lower_condition b in
      if a' == a && b' == b then cond else Or (a', b')

let rec lower_for_minify : t -> t = function
  | Cond c as query ->
      let c' = lower_condition c in
      if c' == c then query else Cond c'
  (* CSS Media Queries 4 sec. 2.3: [all] is the identity media type, so [not all
     and (X)] is the Level 3 spelling of [not (X)]. Bare [not all] has no
     condition form and stays. *)
  | Type { prefix = Some Not; type_ = All; trailing = Some c } ->
      Cond (Not (lower_condition c))
  | Type ({ trailing; _ } as r) as query -> (
      match trailing with
      | None -> query
      | Some c ->
          let c' = lower_condition c in
          if c' == c then query else Type { r with trailing = Some c' })
  | List qs as query ->
      let rec loop changed acc = function
        | [] -> if changed then List (List.rev acc) else query
        | q :: rest ->
            let q' = lower_for_minify q in
            loop (changed || not (q' == q)) (q' :: acc) rest
      in
      loop false [] qs

(* ===== Canonical form ===== *)

(* [normalize] is the one place that decides which spellings name one query, and
   its invariant runs one way only:

   normalize a = normalize b => a and b select the same media

   Nothing promises the converse, and nothing should: two equivalent queries may
   normalise apart, which costs a merge and never correctness. So every rewrite
   below is one of the spec's own equivalences, never a heuristic. *)

(* Media Queries 4 sec. 2.4.4: a [min-] prefix on a range feature is the [>=]
   comparison and [max-] is [<=]; sec. 2.4.3 adds the value-first spelling. One
   bound, four spellings. [feature_bound] already projects all four onto a
   common view for the interval fold, so reading that view back as [name op
   value] is the whole rewrite. The prefix leg is gated on a range feature
   because that is the feature class sec. 2.4.4 speaks about, and the parser
   rejects the prefix anywhere else. *)
let canonical_bound (f : feature) : feature =
  let is_bound =
    match f with
    | Plain (Min base, _) | Plain (Max base, _) -> range_feature_name base
    | Range _ | Range_rev _ -> true
    | Plain _ | Boolean _ | Interval _ | General_enclosed _ -> false
  in
  if not is_bound then f
  else
    match feature_bound f with
    | Some (name, Lower, Le, v) -> Range (name, Ge, v)
    | Some (name, Lower, Lt, v) -> Range (name, Gt, v)
    | Some (name, Upper, Le, v) -> Range (name, Le, v)
    | Some (name, Upper, Lt, v) -> Range (name, Lt, v)
    (* [feature_bound] reports every bound it recognises as [Lt] or [Le]. *)
    | Some _ | None -> f

(* Media Queries 4 sec. 2.4.3: an interval is written in either direction, so
   [(20em >= width >= 10em)] and [(10em <= width <= 20em)] are one query. Keep
   the ascending direction. *)
let canonical_interval a op1 name op2 b : feature =
  match (op1, op2) with
  | (Gt | Ge), (Gt | Ge) ->
      let flip = function Gt -> Lt | Ge -> Le | (Lt | Le | Eq) as op -> op in
      Interval (b, flip op2, name, flip op1, a)
  | _ -> Interval (a, op1, name, op2, b)

let normalize_feature (f : feature) : feature =
  match f with
  | Interval (a, op1, name, op2, b) -> canonical_interval a op1 name op2 b
  | Plain _ | Boolean _ | Range _ | Range_rev _ -> canonical_bound f
  (* A [<general-enclosed>] carries no structure to read, so its text is the
     whole of what it means and none of it is rewritten. *)
  | General_enclosed _ -> f

(* Rebuild only what changed, as [lower_condition] does: [equal] runs this on
   both sides of every merge test, and the queries it is asked about are usually
   already normal. *)
let rec normalize_condition (c : condition) : condition =
  match c with
  | Feature f as cond ->
      let f' = normalize_feature f in
      if f' == f then cond else Feature f'
  | Not c as cond ->
      let c' = normalize_condition c in
      if c' == c then cond else Not c'
  | And (a, b) as cond ->
      let a' = normalize_condition a and b' = normalize_condition b in
      if a' == a && b' == b then cond else And (a', b')
  | Or (a, b) as cond ->
      let a' = normalize_condition a and b' = normalize_condition b in
      if a' == a && b' == b then cond else Or (a', b')

let rec normalize (query : t) : t =
  match query with
  | Cond c ->
      let c' = normalize_condition c in
      if c' == c then query else Cond c'
  (* Media Queries 4 sec. 2.3: [all] matches every media type, so it drops out
     of an unprefixed [all and X], and [not all and X] is the Level 3 spelling
     of [not (X)]. Bare [all] has no condition form and stays. *)
  | Type { prefix = None; type_ = All; trailing = Some c } ->
      Cond (normalize_condition c)
  | Type { prefix = Some Not; type_ = All; trailing = Some c } ->
      Cond (Not (normalize_condition c))
  | Type { trailing = None; _ } -> query
  | Type ({ trailing = Some c; _ } as r) ->
      let c' = normalize_condition c in
      if c' == c then query else Type { r with trailing = Some c' }
  (* A query list matches when any member does, so a one-member list is that
     member and a nested list flattens. *)
  | List [ q ] -> normalize q
  | List qs ->
      List
        (List.concat_map
           (fun q -> match normalize q with List qs -> qs | q -> [ q ])
           qs)

(* Written out rather than left to the structural operators: [equal] is the gate
   on block merging, and a comparison that walks a runtime representation is how
   a spelling difference becomes a merge. *)
let equal_cmp (a : cmp) b =
  match (a, b) with
  | Lt, Lt | Le, Le | Eq, Eq | Gt, Gt | Ge, Ge -> true
  | (Lt | Le | Eq | Gt | Ge), _ -> false

let equal_medium (a : medium) b =
  match (a, b) with
  | All, All | Screen, Screen | Print, Print -> true
  | Other a, Other b -> String.equal a b
  | (All | Screen | Print | Other _), _ -> false

let equal_prefix (a : prefix) b =
  match (a, b) with Not, Not | Only, Only -> true | (Not | Only), _ -> false

let equal_feature (a : feature) b =
  match (a, b) with
  | Plain (n1, v1), Plain (n2, v2) -> equal_name n1 n2 && equal_value v1 v2
  | Boolean n1, Boolean n2 -> equal_name n1 n2
  | Range (n1, o1, v1), Range (n2, o2, v2) ->
      equal_name n1 n2 && equal_cmp o1 o2 && equal_value v1 v2
  | Range_rev (v1, o1, n1), Range_rev (v2, o2, n2) ->
      equal_value v1 v2 && equal_cmp o1 o2 && equal_name n1 n2
  | Interval (a1, o1, n1, p1, b1), Interval (a2, o2, n2, p2, b2) ->
      equal_value a1 a2 && equal_cmp o1 o2 && equal_name n1 n2
      && equal_cmp p1 p2 && equal_value b1 b2
  | General_enclosed a, General_enclosed b -> String.equal a b
  | ( ( Plain _ | Boolean _ | Range _ | Range_rev _ | Interval _
      | General_enclosed _ ),
      _ ) ->
      false

let rec equal_condition (a : condition) b =
  match (a, b) with
  | Feature a, Feature b -> equal_feature a b
  | Not a, Not b -> equal_condition a b
  | And (a1, b1), And (a2, b2) | Or (a1, b1), Or (a2, b2) ->
      equal_condition a1 a2 && equal_condition b1 b2
  | (Feature _ | Not _ | And _ | Or _), _ -> false

let rec equal_normalized (a : t) b =
  match (a, b) with
  | Cond a, Cond b -> equal_condition a b
  | Type a, Type b ->
      Option.equal equal_prefix a.prefix b.prefix
      && equal_medium a.type_ b.type_
      && Option.equal equal_condition a.trailing b.trailing
  | List a, List b -> List.equal equal_normalized a b
  | (Cond _ | Type _ | List _), _ -> false

let equal a b = equal_normalized (normalize a) (normalize b)

(* ===== Sorting / classification ===== *)

type kind =
  | Hover
  | Responsive of int * float
  | Responsive_max of int * float
  | Preference_accessibility
  | Preference_appearance
  | Other

let equal_kind (a : kind) b = a = b

let length_sort_key (l : Values_intf.length) =
  match l with
  | Calc _ -> (-2, 0.)
  | Em v -> (-1, v)
  | Px v -> (0, v)
  | Rem v -> (1, v)
  | Vh v -> (2, v)
  | Vw v -> (3, v)
  | Cm v -> (4, v)
  | Mm v -> (5, v)
  | In v -> (6, v)
  | Pt v -> (7, v)
  | _ -> (100, 0.)

let value_sort_key = function
  | Length l -> length_sort_key l
  | Integer i -> (0, float_of_int i)
  | Number f -> (0, f)
  | Ratio (n, d) when d <> 0 -> (0, float_of_int n /. float_of_int d)
  | Ratio _ -> (0, 0.)
  | Resolution_value (f, _) -> (0, f)
  | Ident _ -> (100, 0.)
  | Function _ -> (200, 0.)

(* A width feature buckets by which side it bounds: a lower bound ([min-width] /
   [width>=]) is [Responsive], an upper bound ([max-width] / [width<=]) is
   [Responsive_max]. Other width plains and [height] stay length-sorted
   [Responsive]. *)
let width_bound_kind name side value : kind option =
  match name with
  | Width ->
      let u, v = value_sort_key value in
      Some
        (match side with
        | Lower -> Responsive (u, v)
        | Upper -> Responsive_max (u, v))
  | _ -> None

let preference_kind (name : name) : kind option =
  match name with
  | Prefers_reduced_motion | Prefers_contrast | Forced_colors | Inverted_colors
  | Pointer | Any_pointer | Scripting ->
      Some Preference_accessibility
  | Prefers_color_scheme -> Some Preference_appearance
  | _ -> None

let plain_feature_kind name value =
  match preference_kind name with
  | Some k -> k
  | None -> (
      match name with
      | Width | Height ->
          let u, v = value_sort_key value in
          Responsive (u, v)
      | _ -> Other)

let interval_feature_kind name lo =
  match name with
  | Width ->
      let u, v = value_sort_key lo in
      Responsive (u, v)
  | _ -> Other

let feature_kind (f : feature) : kind =
  match f with
  | Boolean (Hover | Any_hover) -> Hover
  | Plain ((Hover | Any_hover), _) -> Hover
  | _ -> (
      match feature_bound f with
      | Some (name, side, _, value) -> (
          match width_bound_kind name side value with
          | Some k -> k
          | None -> Other)
      | None -> (
          match f with
          | Plain (name, value) -> plain_feature_kind name value
          | Boolean name -> Option.value (preference_kind name) ~default:Other
          | Interval (lo, _, name, _, _) -> interval_feature_kind name lo
          | Range _ | Range_rev _ | General_enclosed _ -> Other))

(* Media Queries 4 sec. 3: [not] matches the complement, so negating a width
   bound swaps the side it bounds. Only [width] records a side; the other
   buckets name a feature rather than a bound, so a negated query keeps the
   bucket its feature picks. *)
let negated_feature_kind (f : feature) : kind =
  let flipped =
    match feature_bound f with
    | Some (name, Lower, _, value) -> width_bound_kind name Upper value
    | Some (name, Upper, _, value) -> width_bound_kind name Lower value
    | None -> None
  in
  Option.value flipped ~default:(feature_kind f)

let rec condition_kind (c : condition) : kind =
  match c with
  | Feature f -> feature_kind f
  | Not (Not c) -> condition_kind c
  (* The complement of a width range is the two ranges outside it, and an
     [and]/[or] takes its kind from one operand, so the complement of that
     leaves the other operand's range uncovered. Neither is a single bound. *)
  | Not (Feature (Interval _)) | Not (And _ | Or _) -> Other
  | Not (Feature f) -> negated_feature_kind f
  | And (a, b) | Or (a, b) -> (
      match (condition_kind a, condition_kind b) with
      | Other, other | other, Other -> other
      | ka, _ -> ka)

let kind : t -> kind = function
  | Cond c -> condition_kind c
  (* [all] matches every medium, so [not all and X] matches the complement of
     [X], exactly like the bare [not X]. *)
  | Type { prefix = Some Not; type_ = All; trailing = Some c } ->
      condition_kind (Not c)
  | Type _ | List _ -> Other

let group_order = function
  | Hover -> (0, 0.)
  | Other -> (500, 0.)
  | Preference_accessibility -> (1000, 0.)
  | Responsive_max _ -> (1999, 0.)
  | Responsive _ -> (2000, 0.)
  | Preference_appearance -> (3000, 0.)

let feature_preference_order (f : feature) : int option =
  let by_name name v =
    match (name, v) with
    | Prefers_reduced_motion, No_preference -> Some 0
    | Prefers_reduced_motion, Reduce -> Some 1
    | Prefers_contrast, More -> Some 2
    | Prefers_contrast, Less -> Some 3
    | Prefers_color_scheme, _ -> Some 4
    | Forced_colors, _ -> Some 5
    | Inverted_colors, _ -> Some 6
    | Pointer, None -> Some 7
    | Pointer, Coarse -> Some 8
    | Pointer, Fine -> Some 9
    | Any_pointer, None -> Some 10
    | Any_pointer, Coarse -> Some 11
    | Any_pointer, Fine -> Some 12
    | Scripting, None -> Some 13
    | Scripting, Initial_only -> Some 14
    | Scripting, Enabled -> Some 15
    | (Hover | Any_hover), _ -> Some 16
    | Prefers_reduced_transparency, No_preference -> Some 17
    | Prefers_reduced_transparency, Reduce -> Some 18
    | Prefers_reduced_data, No_preference -> Some 19
    | Prefers_reduced_data, Reduce -> Some 20
    | Nav_controls, _ -> Some 21
    | _ -> None
  in
  match f with
  | Plain (name, Ident v) -> by_name name v
  | Boolean (Hover | Any_hover) -> Some 16
  | _ -> None

let rec condition_preference_order (c : condition) : int =
  match c with
  | Feature f -> (
      match feature_preference_order f with Some n -> n | None -> 30)
  | Not c -> condition_preference_order c
  | And (a, _) | Or (a, _) -> condition_preference_order a

let preference_order : t -> int = function
  | Cond c -> condition_preference_order c
  | Type { trailing = Some c; _ } -> condition_preference_order c
  | Type _ | List _ -> 30

(* Within a responsive bucket, an upper bound written as a negated lower bound
   sorts first, then a plain upper bound, then a lower bound. *)
let condition_subkind (c : condition) : int =
  match (c, condition_kind c) with
  | Not _, Responsive_max _ -> 0
  | _, Responsive_max _ -> 1
  | _ -> 2

let responsive_subkind : t -> int = function
  | Cond c -> condition_subkind c
  | Type { prefix = Some Not; type_ = All; trailing = Some c } ->
      condition_subkind (Not c)
  | Type { trailing = Some c; _ } -> condition_subkind c
  | Type _ | List _ -> 2

(* Within a responsive bucket, sort by decreasing specificity: the largest [min]
   (and smallest [max]) is most specific and sorts last. [Responsive_max] is
   negated so one ascending comparator yields largest-first ([1024, 768,
   640]). *)
let responsive_value k =
  match k with
  | Responsive (unit_ord, value) -> (Float.of_int unit_ord *. 1e9) +. value
  | Responsive_max (unit_ord, value) -> (Float.of_int unit_ord *. 1e9) -. value
  | _ -> 0.

(* Sort by four cheap ordinal keys, then serialized text on a full tie.
   Extracting kinds and serializing allocate, and breakpoint-sharing queries tie
   on every ordinal key so they always reach the text; [sort_key] computes the
   five components once per query and [compare_keys] compares them
   alloc-free. *)
type key = {
  group : int;
  subkind : int;
  responsive : float;
  preference : int;
  text : string;
}

let sort_key t =
  let k = kind t in
  let group, _ = group_order k in
  {
    group;
    subkind = responsive_subkind t;
    responsive = responsive_value k;
    preference = preference_order t;
    text = to_string t;
  }

(* A plain [if c <> 0 then c else ...] chain: it short-circuits like the thunk
   combinator but allocates no closures, which matters because a sort calls this
   O(n log n) times. *)
let compare_keys a b =
  let c = Int.compare a.group b.group in
  if c <> 0 then c
  else
    let c = Int.compare a.subkind b.subkind in
    if c <> 0 then c
    else
      let c = Float.compare a.responsive b.responsive in
      if c <> 0 then c
      else
        let c = Int.compare a.preference b.preference in
        if c <> 0 then c else String.compare a.text b.text

let sort_by project items =
  List.map (fun x -> (sort_key (project x), x)) items
  |> List.stable_sort (fun (k1, _) (k2, _) -> compare_keys k1 k2)
  |> List.map snd

let compare a b =
  let ka, kb = (kind a, kind b) in
  let ga, _ = group_order ka and gb, _ = group_order kb in
  let c = Int.compare ga gb in
  if c <> 0 then c
  else
    let c = Int.compare (responsive_subkind a) (responsive_subkind b) in
    if c <> 0 then c
    else
      let c = Float.compare (responsive_value ka) (responsive_value kb) in
      if c <> 0 then c
      else
        let c = Int.compare (preference_order a) (preference_order b) in
        if c <> 0 then c else String.compare (to_string a) (to_string b)
