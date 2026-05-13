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

open Syntax

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

type feature =
  | Plain of name * value
  | Boolean of name
  | Range of name * cmp * value
  | Range_rev of value * cmp * name
  | Interval of value * cmp * name * cmp * value

type condition =
  | Feature of feature
  | Not of condition
  | And of condition * condition
  | Or of condition * condition

type medium = All | Screen | Print | Other of string
type prefix = Not | Only

type query =
  | Cond of condition
  | Type of {
      prefix : prefix option;
      type_ : medium;
      trailing : condition option;
    }
  | List of query list  (** Comma-separated media query list. *)

type t =
  | Width of Values_intf.length
  | Height of Values_intf.length
  | Min_width of float
  | Max_width of float
  | Not_min_width of float
  | Min_width_rem of float
  | Not_min_width_rem of float
  | Min_width_length of Values_intf.length
  | Not_min_width_length of Values_intf.length
  | Aspect_ratio of int * int
  | Resolution of float * string
  | Color of int
  | Color_index of int
  | Monochrome of int
  | Color_gamut of ident
  | Video_color_gamut of ident
  | Dynamic_range of ident
  | Video_dynamic_range of ident
  | Scan of ident
  | Update of ident
  | Overflow_block of ident
  | Overflow_inline of ident
  | Prefers_reduced_motion of ident
  | Prefers_reduced_transparency of ident
  | Prefers_reduced_data of ident
  | Prefers_contrast of ident
  | Prefers_color_scheme of ident
  | Forced_colors of ident
  | Inverted_colors of ident
  | Pointer of ident
  | Any_pointer of ident
  | Hover of ident
  | Any_hover of ident
  | Scripting of ident
  | Nav_controls of ident
  | Print
  | Orientation of ident
  | And of t * t
  | Or of t * t
  | Negated of t
  | Range of name * cmp * value
  | Range_rev of value * cmp * name
  | Interval of value * cmp * name * cmp * value
  | Type_query of {
      prefix : prefix option;
      type_ : medium;
      trailing : t option;
    }
  | Plain of name * value
  | Boolean of name
  | List of t list

(* ===== Formatting helpers ===== *)

let format_float f =
  if Float.is_integer f then Int.to_string (Float.to_int f)
  else Float.to_string f

let format_px = format_float
let format_rem = format_float

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
  | Other s -> s

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
  | Other s -> s

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
  | Other s -> s

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

(* CSS Media Queries 4 3.4: a [min-X] / [max-X] feature name maps onto the range
   form [X >= V] / [X <= V]. [as_min_max] returns the comparison and stripped
   name in one place so the typed and string-valued printers below share the
   same parsing - the only [String.sub] / [String.length] arithmetic in the
   file. *)
type min_max_view = Range_view of cmp * name | Plain_view

let as_min_max name =
  match name with
  | Min base -> Range_view (Ge, base)
  | Max base -> Range_view (Le, base)
  | _ -> Plain_view

let rec pp_feature : feature Pp.t =
 fun ctx -> function
  | Plain (name, value) when Pp.minified ctx -> (
      match as_min_max name with
      | Range_view (op, base) -> pp_feature ctx (Range (base, op, value))
      | Plain_view ->
          Pp.char ctx '(';
          Pp.string ctx (string_of_name name);
          Pp.char ctx ':';
          pp_value ctx value;
          Pp.char ctx ')')
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

let rec pp_condition : condition Pp.t =
 fun ctx -> function
  | Feature f -> pp_feature ctx f
  | Not c ->
      Pp.string ctx "not ";
      pp_condition ctx c
  | And (a, b) ->
      pp_condition ctx a;
      Pp.string ctx (if Pp.minified ctx then "and " else " and ");
      pp_condition ctx b
  | Or (a, b) ->
      pp_condition ctx a;
      Pp.string ctx (if Pp.minified ctx then "or " else " or ");
      pp_condition ctx b

let rec pp_query : query Pp.t =
 fun ctx -> function
  | Cond c -> pp_condition ctx c
  | Type { prefix; type_; trailing } -> (
      (match prefix with
      | None -> ()
      | Some Not -> Pp.string ctx "not "
      | Some Only -> Pp.string ctx "only ");
      Pp.string ctx (string_of_medium type_);
      match trailing with
      | None -> ()
      | Some c ->
          Pp.string ctx " and ";
          pp_condition ctx c)
  | List qs ->
      let rec loop = function
        | [] -> ()
        | [ q ] -> pp_query ctx q
        | q :: rest ->
            pp_query ctx q;
            Pp.char ctx ',';
            Pp.space_if_pretty ctx ();
            loop rest
      in
      loop qs

(* The named/length feature printers route through [as_min_max] under minify so
   [(min-width: 768px)] emerges as [(width>=768px)] - same rule the typed
   [pp_feature] applies above. *)
let pp_feature_with pp_value ctx name value =
  match as_min_max (name_of_string name) with
  | Range_view (op, base) when Pp.minified ctx ->
      Pp.char ctx '(';
      Pp.string ctx (string_of_name base);
      Pp.string ctx (string_of_cmp op);
      pp_value ctx value;
      Pp.char ctx ')'
  | _ ->
      Pp.char ctx '(';
      Pp.string ctx name;
      Pp.char ctx ':';
      Pp.space_if_pretty ctx ();
      pp_value ctx value;
      Pp.char ctx ')'

let pp_named_feature ctx = pp_feature_with Pp.string ctx
let pp_length_feature ctx = pp_feature_with pp_length ctx
let pp_min_width_length ctx l = pp_length_feature ctx "min-width" l

let rec pp ctx = function
  | Width l -> pp_length_feature ctx "width" l
  | Height l -> pp_length_feature ctx "height" l
  | Min_width px -> pp_named_feature ctx "min-width" (format_px px ^ "px")
  | Max_width px -> pp_named_feature ctx "max-width" (format_px px ^ "px")
  | Not_min_width px ->
      Pp.string ctx "not all and ";
      pp_named_feature ctx "min-width" (format_px px ^ "px")
  | Min_width_rem rem ->
      pp_named_feature ctx "min-width" (format_rem rem ^ "rem")
  | Not_min_width_rem rem ->
      Pp.string ctx "not all and ";
      pp_named_feature ctx "min-width" (format_rem rem ^ "rem")
  | Min_width_length l -> pp_min_width_length ctx l
  | Not_min_width_length l ->
      Pp.string ctx "not all and ";
      pp_min_width_length ctx l
  | Aspect_ratio (a, 1) -> pp_named_feature ctx "aspect-ratio" (Int.to_string a)
  | Aspect_ratio (a, b) ->
      pp_named_feature ctx "aspect-ratio"
        (Int.to_string a ^ "/" ^ Int.to_string b)
  | Resolution (n, unit) ->
      pp_named_feature ctx "resolution" (format_float n ^ unit)
  | Color n -> pp_named_feature ctx "color" (Int.to_string n)
  | Color_index n -> pp_named_feature ctx "color-index" (Int.to_string n)
  | Monochrome n -> pp_named_feature ctx "monochrome" (Int.to_string n)
  | Color_gamut ident ->
      pp_named_feature ctx "color-gamut" (string_of_ident ident)
  | Video_color_gamut ident ->
      pp_named_feature ctx "video-color-gamut" (string_of_ident ident)
  | Dynamic_range ident ->
      pp_named_feature ctx "dynamic-range" (string_of_ident ident)
  | Video_dynamic_range ident ->
      pp_named_feature ctx "video-dynamic-range" (string_of_ident ident)
  | Scan ident -> pp_named_feature ctx "scan" (string_of_ident ident)
  | Update ident -> pp_named_feature ctx "update" (string_of_ident ident)
  | Overflow_block ident ->
      pp_named_feature ctx "overflow-block" (string_of_ident ident)
  | Overflow_inline ident ->
      pp_named_feature ctx "overflow-inline" (string_of_ident ident)
  | Prefers_reduced_motion ident ->
      pp_named_feature ctx "prefers-reduced-motion" (string_of_ident ident)
  | Prefers_reduced_transparency ident ->
      pp_named_feature ctx "prefers-reduced-transparency"
        (string_of_ident ident)
  | Prefers_reduced_data ident ->
      pp_named_feature ctx "prefers-reduced-data" (string_of_ident ident)
  | Prefers_contrast ident ->
      pp_named_feature ctx "prefers-contrast" (string_of_ident ident)
  | Prefers_color_scheme ident ->
      pp_named_feature ctx "prefers-color-scheme" (string_of_ident ident)
  | Forced_colors ident ->
      pp_named_feature ctx "forced-colors" (string_of_ident ident)
  | Inverted_colors ident ->
      pp_named_feature ctx "inverted-colors" (string_of_ident ident)
  | Pointer ident -> pp_named_feature ctx "pointer" (string_of_ident ident)
  | Any_pointer ident ->
      pp_named_feature ctx "any-pointer" (string_of_ident ident)
  | Hover ident -> pp_named_feature ctx "hover" (string_of_ident ident)
  | Any_hover ident -> pp_named_feature ctx "any-hover" (string_of_ident ident)
  | Scripting ident -> pp_named_feature ctx "scripting" (string_of_ident ident)
  | Nav_controls ident ->
      pp_named_feature ctx "nav-controls" (string_of_ident ident)
  | Print -> Pp.string ctx "print"
  | Orientation ident ->
      pp_named_feature ctx "orientation" (string_of_ident ident)
  | And (a, b) ->
      pp ctx a;
      Pp.string ctx (if Pp.minified ctx then "and " else " and ");
      pp ctx b
  | Or (a, b) ->
      pp ctx a;
      Pp.string ctx (if Pp.minified ctx then "or " else " or ");
      pp ctx b
  | Negated Print -> Pp.string ctx "not print"
  | Negated inner ->
      (* MQ4 [not <media-in-parens>]: the legacy [not all and <feature>] rewrite
         reassociates against any surrounding [and]/[or] and only survives
         intact for feature leaves at the query top level. Emit the MQ4 form and
         self-wrap when the inner condition is not already a parenthesised
         feature. *)
      let rendered = Pp.to_string ~minify:ctx.Pp.minify pp inner in
      Pp.string ctx "not ";
      if String.length rendered > 0 && rendered.[0] = '(' then
        Pp.string ctx rendered
      else (
        Pp.char ctx '(';
        Pp.string ctx rendered;
        Pp.char ctx ')')
  | Range (name, op, value) -> pp_feature ctx (Range (name, op, value))
  | Range_rev (value, op, name) -> pp_feature ctx (Range_rev (value, op, name))
  | Interval (a, op1, name, op2, b) ->
      pp_feature ctx (Interval (a, op1, name, op2, b))
  | Type_query { prefix; type_; trailing } -> (
      (match prefix with
      | None -> ()
      | Some Not -> Pp.string ctx "not "
      | Some Only -> Pp.string ctx "only ");
      Pp.string ctx (string_of_medium type_);
      match trailing with
      | None -> ()
      | Some cond -> (
          Pp.string ctx " and ";
          (* CSS Media Queries 4 §3 [media-condition-without-or]: Or
             sub-expressions need explicit parens in this context. *)
          match cond with
          | Or _ ->
              Pp.char ctx '(';
              pp ctx cond;
              Pp.char ctx ')'
          | _ -> pp ctx cond))
  | Plain (name, value) -> pp_feature ctx (Plain (name, value))
  | Boolean name -> pp_feature ctx (Boolean name)
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

(* A lightweight character scanner sufficient for media-query syntax. *)
type scanner = { s : string; mutable pos : int }
type recovery_scope = Branch | Query_list

exception Parse_error of recovery_scope * string

let fail_parse ?(scope = Branch) reason = raise (Parse_error (scope, reason))
let mk_scanner s = { s = String.trim s; pos = 0 }
let at_end sc = sc.pos >= String.length sc.s

let peek sc : char option =
  if at_end sc then Option.None else Some sc.s.[sc.pos]

let advance sc = sc.pos <- sc.pos + 1

let skip_ws sc =
  while
    (not (at_end sc))
    &&
    let c = sc.s.[sc.pos] in
    c = ' ' || c = '\t' || c = '\n' || c = '\r' || c = '\012'
  do
    advance sc
  done

let read_ident sc =
  skip_ws sc;
  let start = sc.pos in
  if at_end sc then ""
  else if not (is_ascii_ident_start sc.s.[sc.pos]) then ""
  else (
    advance sc;
    while (not (at_end sc)) && is_ascii_ident_continue sc.s.[sc.pos] do
      advance sc
    done;
    String.sub sc.s start (sc.pos - start))

let lookahead_ident sc kw =
  let kw_len = String.length kw in
  let s_len = String.length sc.s in
  if sc.pos + kw_len > s_len then false
  else
    let ok = ref true in
    for k = 0 to kw_len - 1 do
      if Char.lowercase_ascii sc.s.[sc.pos + k] <> Char.lowercase_ascii kw.[k]
      then ok := false
    done;
    !ok
    && (sc.pos + kw_len >= s_len
       ||
       let c = sc.s.[sc.pos + kw_len] in
       not (is_ascii_ident_continue c))

let consume_keyword sc kw = sc.pos <- sc.pos + String.length kw

(* Parse an mf-value: number/integer + optional unit, ratio, or ident. *)
let is_digit c = c >= '0' && c <= '9'

let consume_digits sc =
  let saw_digit = ref false in
  while (not (at_end sc)) && is_digit sc.s.[sc.pos] do
    saw_digit := true;
    advance sc
  done;
  !saw_digit

let consume_sign sc =
  match peek sc with Some ('+' | '-') -> advance sc | _ -> ()

let consume_exponent sc =
  match peek sc with
  | Some ('e' | 'E') ->
      let mark = sc.pos in
      advance sc;
      consume_sign sc;
      if not (consume_digits sc) then sc.pos <- mark
  | _ -> ()

let read_number_lit sc : ([ `Int of int | `Float of float ] * string) option =
  let start = sc.pos in
  consume_sign sc;
  let saw_int = consume_digits sc in
  let saw_dot =
    match peek sc with
    | Some '.' ->
        advance sc;
        true
    | _ -> false
  in
  let saw_frac = saw_dot && consume_digits sc in
  consume_exponent sc;
  if not (saw_int || saw_frac) then (
    sc.pos <- start;
    None)
  else
    let txt = String.sub sc.s start (sc.pos - start) in
    let is_int =
      (not saw_dot) && not (String.contains txt 'e' || String.contains txt 'E')
    in
    if is_int then Some (`Int (int_of_string txt), txt)
    else Some (`Float (float_of_string txt), txt)

let read_unit sc =
  let start = sc.pos in
  if at_end sc then ""
  else if sc.s.[sc.pos] = '%' then (
    advance sc;
    "%")
  else if is_ascii_ident_start sc.s.[sc.pos] then (
    advance sc;
    while (not (at_end sc)) && is_ascii_ident_continue sc.s.[sc.pos] do
      advance sc
    done;
    String.sub sc.s start (sc.pos - start))
  else ""

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
  | _ -> None

let resolution_units = [ "dpi"; "dpcm"; "dppx"; "x" ]

(* Read balanced parens content into a string. Assumes '(' already consumed. *)
let read_balanced sc =
  let buf = Buffer.create 32 in
  let depth = ref 1 in
  let continue = ref true in
  while !continue do
    match peek sc with
    | None -> failwith "Unmatched parenthesis in @media condition"
    | Some '(' ->
        incr depth;
        Buffer.add_char buf '(';
        advance sc
    | Some ')' ->
        decr depth;
        if !depth = 0 then (
          advance sc;
          continue := false)
        else (
          Buffer.add_char buf ')';
          advance sc)
    | Some c ->
        Buffer.add_char buf c;
        advance sc
  done;
  Buffer.contents buf

let typed_function_value name args : value option =
  let raw = name ^ "(" ^ args ^ ")" in
  let cursor = Cursor.of_string raw in
  match Values.read_length cursor with
  | length ->
      Cursor.ws cursor;
      Cursor.expect_eof cursor;
      Some (Length length)
  | exception Cursor.Parse_error _ -> None

let number_as_scalar : [ `Int of int | `Float of float ] -> value = function
  | `Int n -> Integer n
  | `Float f -> Number f

let number_as_int : [ `Int of int | `Float of float ] -> int = function
  | `Int n -> n
  | `Float f -> int_of_float f

let read_value_with_unit num unit : value option =
  let f = match num with `Int n -> float_of_int n | `Float f -> f in
  match length_of_value f unit with
  | Some l -> Some (Length l)
  | None ->
      if List.mem (String.lowercase_ascii unit) resolution_units then
        Some (Resolution_value (f, unit))
      else None

let read_ratio_tail sc num : value option =
  (* Could be a ratio: "n / m" *)
  let mark = sc.pos in
  skip_ws sc;
  match peek sc with
  | Some '/' -> (
      advance sc;
      skip_ws sc;
      match read_number_lit sc with
      | Some (`Int d, _) -> Some (Ratio (number_as_int num, d))
      | _ ->
          sc.pos <- mark;
          Some (number_as_scalar num))
  | _ ->
      sc.pos <- mark;
      Some (number_as_scalar num)

let read_numeric_value sc : value option =
  match read_number_lit sc with
  | None -> None
  | Some (num, _) ->
      let unit = read_unit sc in
      if unit = "" then read_ratio_tail sc num
      else read_value_with_unit num unit

let read_ident_value sc : value option =
  let id = read_ident sc in
  if id = "" then None
  else (
    skip_ws sc;
    match peek sc with
    | Some '(' -> (
        advance sc;
        let args = read_balanced sc in
        match typed_function_value id args with
        | Some _ as value -> value
        | None -> Some (Function (id, args)))
    | _ -> Some (Ident (ident_of_string id)))

let read_value sc : value option =
  skip_ws sc;
  match peek sc with
  | None -> None
  | Some c when (c >= '0' && c <= '9') || c = '.' || c = '+' || c = '-' ->
      read_numeric_value sc
  | Some _ -> read_ident_value sc

let value_of_string s =
  let sc = mk_scanner s in
  match read_value sc with
  | Some value ->
      skip_ws sc;
      if at_end sc then value else failwith ("invalid media value: " ^ s)
  | None -> failwith ("invalid media value: " ^ s)

let boolean_feature name : feature = Boolean name

let read_cmp sc : cmp option =
  skip_ws sc;
  match peek sc with
  | Some '<' ->
      advance sc;
      if peek sc = Some '=' then (
        advance sc;
        Some Le)
      else Some Lt
  | Some '>' ->
      advance sc;
      if peek sc = Some '=' then (
        advance sc;
        Some Ge)
      else Some Gt
  | Some '=' ->
      advance sc;
      Some Eq
  | _ -> None

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

let validate_plain_feature (name : name) value =
  let plain_name =
    match prefixed_range_feature_name name with
    | Some base when range_feature_name base -> base
    | Some _ -> name
    | None -> name
  in
  (* [env()] / [var()] / [calc()] etc. produce a typed value at use time;
     cascade can't determine the resolved type at parse time, so accept them
     wherever a typed numeric is allowed and let the consumer do its own
     validation. *)
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

(* Parse content already inside parens (no surrounding parens). *)
let value_first_interval_tail sc v1 op1 name op2 : feature option =
  skip_ws sc;
  match read_value sc with
  | None -> None
  | Some v2 ->
      skip_ws sc;
      if
        at_end sc
        && interval_ops_compatible op1 op2
        && validate_range_feature name v1
        && validate_range_feature name v2
      then Some (Interval (v1, op1, name, op2, v2))
      else None

let value_first_range_or_interval sc v1 op1 name : feature option =
  match read_cmp sc with
  | Some op2 -> value_first_interval_tail sc v1 op1 name op2
  | None ->
      skip_ws sc;
      if at_end sc && validate_range_feature name v1 then
        Some (Range_rev (v1, op1, name))
      else None

let value_first_after_op sc ~mark v1 op1 : feature option =
  skip_ws sc;
  let name = read_ident sc in
  if name = "" then (
    sc.pos <- mark;
    None)
  else
    let name = name_of_string name in
    value_first_range_or_interval sc v1 op1 name

let value_first_feature content : feature option =
  let sc = mk_scanner content in
  skip_ws sc;
  (* Try value-first form: V op name [op V] *)
  let mark = sc.pos in
  match read_value sc with
  | None -> None
  | Some v1 -> (
      match read_cmp sc with
      | Some op1 -> value_first_after_op sc ~mark v1 op1
      | None ->
          sc.pos <- mark;
          None)

let boolean_or_none_feature id : feature option =
  if Option.is_some (prefixed_range_feature_name (name_of_string id)) then None
  else Some (boolean_feature (name_of_string id))

let plain_feature_after_colon sc id : feature option =
  advance sc;
  skip_ws sc;
  match read_value sc with
  | None -> None
  | Some value ->
      skip_ws sc;
      let name = name_of_string id in
      if at_end sc && validate_plain_feature name value then
        Some (plain_feature name value)
      else None

let range_after_value sc id op v2 : feature option =
  match read_cmp sc with
  | Some _ -> None
  | None ->
      skip_ws sc;
      let name = name_of_string id in
      if at_end sc && validate_range_feature name v2 then
        Some (Range (name, op, v2))
      else None

let range_after_op sc id op : feature option =
  skip_ws sc;
  match read_value sc with
  | None -> None
  | Some v2 -> range_after_value sc id op v2

let name_first_range sc id content : feature option =
  match read_cmp sc with
  | None -> value_first_feature content
  | Some op -> range_after_op sc id op

let feature_after_ident sc id content : feature option =
  skip_ws sc;
  match peek sc with
  | None -> boolean_or_none_feature id
  | Some ':' -> plain_feature_after_colon sc id
  | Some _ -> name_first_range sc id content

let feature_in_parens content : feature option =
  let sc = mk_scanner content in
  skip_ws sc;
  if at_end sc then None
  else
    let id = read_ident sc in
    if id = "" then value_first_feature content
    else feature_after_ident sc id content

let extract_feature_or_fail content =
  match feature_in_parens content with
  | Some f -> f
  | None -> failwith ("invalid media feature: " ^ content)

let condition_from_paren_content parse_condition content =
  let trimmed = String.trim content in
  (* Could be either ( <condition> ) or ( <feature> ). *)
  let inner = mk_scanner trimmed in
  skip_ws inner;
  if lookahead_ident inner "not" then parse_condition trimmed
  else if lookahead_ident inner "and" || lookahead_ident inner "or" then
    Feature (extract_feature_or_fail trimmed)
  else if peek inner = Some '(' then parse_condition trimmed
  else Feature (extract_feature_or_fail trimmed)

(* Parser for media-condition (sequence of (...) with and/or/not). *)
let rec condition_in_parens sc =
  skip_ws sc;
  match peek sc with
  | Some '(' ->
      advance sc;
      condition_from_paren_content condition_of_string (read_balanced sc)
  | _ -> failwith "expected '(' in media condition"

and condition_of_string s =
  let sc = mk_scanner s in
  condition sc

and condition sc =
  skip_ws sc;
  if lookahead_ident sc "not" then (
    consume_keyword sc "not";
    skip_ws sc;
    let inner = condition_in_parens sc in
    skip_ws sc;
    if at_end sc then Not inner
    else failwith "trailing content after 'not <media-in-parens>'")
  else
    let left = condition_in_parens sc in
    chain sc left

and chain sc (acc : condition) =
  let rec loop op (acc : condition) : condition =
    skip_ws sc;
    if at_end sc then acc
    else if lookahead_ident sc "and" then (
      (match op with
      | Some `Or -> failwith "mixed 'and'/'or' media condition"
      | _ -> ());
      consume_keyword sc "and";
      let right = condition_in_parens sc in
      loop (Some `And) (And (acc, right) : condition))
    else if lookahead_ident sc "or" then (
      (match op with
      | Some `And -> failwith "mixed 'and'/'or' media condition"
      | _ -> ());
      consume_keyword sc "or";
      let right = condition_in_parens sc in
      loop (Some `Or) (Or (acc, right) : condition))
    else acc
  in
  loop None acc

let medium_of_ident s : medium =
  match String.lowercase_ascii s with
  | "all" -> All
  | "screen" -> Screen
  | "print" -> Print
  | other -> Other other

let is_reserved_media_type_keyword s =
  match String.lowercase_ascii s with "and" | "or" -> true | _ -> false

let rec next_non_ws s len i : char option =
  if i >= len then None
  else match s.[i] with ' ' | '\t' -> next_non_ws s len (i + 1) | c -> Some c

(* [<media-query>] starts as [<media-condition>] (rather than as a media type)
   when its first non-space token is '(' or "not (". *)
let starts_with_condition sc =
  match (peek sc, lookahead_ident sc "not") with
  | Some '(', _ -> true
  | _, true -> next_non_ws sc.s (String.length sc.s) (sc.pos + 3) = Some '('
  | _ -> false

let read_query_prefix sc =
  if lookahead_ident sc "not" then (
    consume_keyword sc "not";
    skip_ws sc;
    Some Not)
  else if lookahead_ident sc "only" then (
    consume_keyword sc "only";
    skip_ws sc;
    Some Only)
  else None

let read_media_type_query sc =
  let prefix = read_query_prefix sc in
  (match prefix with
  | Some _ when lookahead_ident sc "not" || lookahead_ident sc "only" ->
      fail_parse ~scope:Query_list "duplicate media query prefix"
  | _ -> ());
  let id = read_ident sc in
  if id = "" then failwith "expected media type or condition"
  else if is_reserved_media_type_keyword id then
    failwith "reserved media condition keyword cannot be a media type"
  else
    let type_ = medium_of_ident id in
    skip_ws sc;
    if at_end sc || peek sc = Some ',' then
      Type { prefix; type_; trailing = None }
    else if lookahead_ident sc "and" then (
      consume_keyword sc "and";
      let cond = condition_in_parens sc in
      let cond = chain sc cond in
      Type { prefix; type_; trailing = Some cond })
    else
      failwith
        (String.concat ""
           [
             "expected 'and' or end of query after media type, got: ";
             String.sub sc.s sc.pos (String.length sc.s - sc.pos);
           ])

let single_query sc =
  skip_ws sc;
  if at_end sc then failwith "empty media query"
  else if starts_with_condition sc then Cond (condition sc)
  else read_media_type_query sc

let not_all_query : query =
  Type { prefix = Some Not; type_ = All; trailing = None }

let skip_recovery_branch sc =
  let depth = ref 0 in
  let continue = ref true in
  while (not (at_end sc)) && !continue do
    match peek sc with
    | Some '(' ->
        incr depth;
        advance sc
    | Some ')' ->
        if !depth > 0 then decr depth;
        advance sc
    | Some ',' when !depth = 0 -> continue := false
    | Some _ -> advance sc
    | None -> continue := false
  done

let query_branch ~recover ~recovered_at_eof sc =
  let mark = sc.pos in
  try
    let query = single_query sc in
    skip_ws sc;
    match peek sc with
    | None | Some ',' -> query
    | _ -> failwith "trailing content in media query branch"
  with
  | Parse_error (scope, reason) ->
      if not recover then raise (Failure reason);
      if scope = Query_list then sc.pos <- String.length sc.s
      else (
        sc.pos <- mark;
        skip_recovery_branch sc);
      skip_ws sc;
      if at_end sc then recovered_at_eof := true;
      not_all_query
  | Failure _ ->
      if not recover then raise (Failure "invalid media query branch");
      sc.pos <- mark;
      skip_recovery_branch sc;
      skip_ws sc;
      if at_end sc then recovered_at_eof := true;
      not_all_query

let trailing_content_failure sc =
  failwith
    (String.concat ""
       [
         "trailing content in media query: ";
         String.sub sc.s sc.pos (String.length sc.s - sc.pos);
       ])

let query_of_string ?(recover = true) s : query =
  let comma_query_list sc branch first recovered_at_eof =
    let rec loop acc =
      match peek sc with
      | Some ',' ->
          advance sc;
          skip_ws sc;
          let q = branch () in
          skip_ws sc;
          loop (q :: acc)
      | _ -> List.rev acc
    in
    let rest = loop [] in
    skip_ws sc;
    if not (at_end sc) then trailing_content_failure sc;
    if !recovered_at_eof then not_all_query else List (first :: rest)
  in
  let sc = mk_scanner s in
  if at_end sc then (List [] : query)
  else
    let recovered_at_eof = ref false in
    let branch () = query_branch ~recover ~recovered_at_eof sc in
    let first = branch () in
    skip_ws sc;
    match (at_end sc, peek sc) with
    | true, _ -> first
    | false, Some ',' -> comma_query_list sc branch first recovered_at_eof
    | false, _ -> trailing_content_failure sc

let normalise_preference_value name s =
  match name with
  | "prefers-reduced-motion" -> (
      match s with
      | "no-preference" -> Some (Prefers_reduced_motion No_preference)
      | "reduce" -> Some (Prefers_reduced_motion Reduce)
      | _ -> None)
  | "prefers-contrast" -> (
      match s with
      | "more" -> Some (Prefers_contrast More)
      | "less" -> Some (Prefers_contrast Less)
      | _ -> None)
  | "prefers-color-scheme" -> (
      match s with
      | "dark" -> Some (Prefers_color_scheme Dark)
      | "light" -> Some (Prefers_color_scheme Light)
      | _ -> None)
  | "prefers-reduced-transparency" -> (
      match s with
      | "no-preference" -> Some (Prefers_reduced_transparency No_preference)
      | "reduce" -> Some (Prefers_reduced_transparency Reduce)
      | _ -> None)
  | "prefers-reduced-data" -> (
      match s with
      | "no-preference" -> Some (Prefers_reduced_data No_preference)
      | "reduce" -> Some (Prefers_reduced_data Reduce)
      | _ -> None)
  | "forced-colors" -> (
      match s with
      | "active" -> Some (Forced_colors Active)
      | "none" -> Some (Forced_colors None)
      | _ -> None)
  | "inverted-colors" -> (
      match s with
      | "inverted" -> Some (Inverted_colors Inverted)
      | "none" -> Some (Inverted_colors None)
      | _ -> None)
  | _ -> None

let normalise_capability_value name s =
  match name with
  | "pointer" -> (
      match s with
      | "none" -> Some (Pointer None)
      | "coarse" -> Some (Pointer Coarse)
      | "fine" -> Some (Pointer Fine)
      | _ -> None)
  | "any-pointer" -> (
      match s with
      | "none" -> Some (Any_pointer None)
      | "coarse" -> Some (Any_pointer Coarse)
      | "fine" -> Some (Any_pointer Fine)
      | _ -> None)
  | "hover" -> (
      match s with
      | "none" -> Some (Hover None)
      | "hover" -> Some (Hover Hover)
      | _ -> None)
  | "any-hover" -> (
      match s with
      | "none" -> Some (Any_hover None)
      | "hover" -> Some (Any_hover Hover)
      | _ -> None)
  | "scripting" -> (
      match s with
      | "none" -> Some (Scripting None)
      | "initial-only" -> Some (Scripting Initial_only)
      | "enabled" -> Some (Scripting Enabled)
      | _ -> None)
  | "nav-controls" -> (
      match s with
      | "none" -> Some (Nav_controls None)
      | "back" -> Some (Nav_controls Back)
      | _ -> None)
  | "orientation" -> (
      match s with
      | "portrait" -> Some (Orientation Portrait)
      | "landscape" -> Some (Orientation Landscape)
      | _ -> None)
  | _ -> None

let normalise_display_value name s =
  match name with
  | "color-gamut" -> (
      match s with
      | "srgb" -> Some (Color_gamut Srgb)
      | "p3" -> Some (Color_gamut P3)
      | "rec2020" -> Some (Color_gamut Rec2020)
      | _ -> None)
  | "video-color-gamut" -> (
      match s with
      | "srgb" -> Some (Video_color_gamut Srgb)
      | "p3" -> Some (Video_color_gamut P3)
      | "rec2020" -> Some (Video_color_gamut Rec2020)
      | _ -> None)
  | "dynamic-range" -> (
      match s with
      | "standard" -> Some (Dynamic_range Standard)
      | "high" -> Some (Dynamic_range High)
      | _ -> None)
  | "video-dynamic-range" -> (
      match s with
      | "standard" -> Some (Video_dynamic_range Standard)
      | "high" -> Some (Video_dynamic_range High)
      | _ -> None)
  | "scan" -> (
      match s with
      | "interlace" -> Some (Scan Interlace)
      | "progressive" -> Some (Scan Progressive)
      | _ -> None)
  | "update" -> (
      match s with
      | "none" -> Some (Update None)
      | "slow" -> Some (Update Slow)
      | "fast" -> Some (Update Fast)
      | _ -> None)
  | "overflow-block" -> (
      match s with
      | "none" -> Some (Overflow_block None)
      | "scroll" -> Some (Overflow_block Scroll)
      | "optional-paged" -> Some (Overflow_block Optional_paged)
      | "paged" -> Some (Overflow_block Paged)
      | _ -> None)
  | "overflow-inline" -> (
      match s with
      | "none" -> Some (Overflow_inline None)
      | "scroll" -> Some (Overflow_inline Scroll)
      | _ -> None)
  | _ -> None

let normalise_ident_value name s =
  let s = String.lowercase_ascii s in
  match normalise_preference_value name s with
  | Some _ as v -> v
  | None -> (
      match normalise_capability_value name s with
      | Some _ as v -> v
      | None -> normalise_display_value name s)

(* Smart constructor: try to recognise a known shorthand, otherwise wrap as
   Custom. *)
let normalise_value name value =
  match (String.lowercase_ascii name, value) with
  | "min-width", Length (Values_intf.Px f) -> Some (Min_width f)
  | "max-width", Length (Values_intf.Px f) -> Some (Max_width f)
  | "min-width", Length (Values_intf.Rem f) -> Some (Min_width_rem f)
  | "min-width", Length l -> Some (Min_width_length l)
  | "width", Length l -> Some (Width l)
  | "height", Length l -> Some (Height l)
  | "aspect-ratio", Ratio (a, b) -> Some (Aspect_ratio (a, b))
  | "aspect-ratio", Integer n -> Some (Aspect_ratio (n, 1))
  | "resolution", Resolution_value (n, u) -> Some (Resolution (n, u))
  | "color", Integer n -> Some (Color n)
  | "color-index", Integer n -> Some (Color_index n)
  | "monochrome", Integer n -> Some (Monochrome n)
  | name, Ident s -> normalise_ident_value name (string_of_ident s)
  | _ -> None

let feature_to_t : feature -> t = function
  | Plain (name, value) -> (
      match normalise_value (string_of_name name) value with
      | Some t -> t
      | None -> Plain (name, value))
  | Boolean name -> (Boolean name : t)
  | Range (name, op, value) -> Range (name, op, value)
  | Range_rev (value, op, name) -> Range_rev (value, op, name)
  | Interval (a, op1, name, op2, b) -> Interval (a, op1, name, op2, b)

let rec condition_to_t : condition -> t = function
  | Feature f -> feature_to_t f
  | Not cond -> Negated (condition_to_t cond)
  | And (a, b) -> And (condition_to_t a, condition_to_t b)
  | Or (a, b) -> Or (condition_to_t a, condition_to_t b)

let rec collapse_query (q : query) : t =
  match q with
  | Cond cond -> condition_to_t cond
  | Type { prefix = Some Not; type_ = All; trailing = Some cond } -> (
      match condition_to_t cond with
      | Min_width f -> Not_min_width f
      | Min_width_rem f -> Not_min_width_rem f
      | Min_width_length l -> Not_min_width_length l
      | other -> Negated other)
  | Type { prefix = Some Not; type_ = Print; trailing = None } -> Negated Print
  | Type { prefix = None; type_ = Print; trailing = None } -> Print
  | Type { prefix; type_; trailing } ->
      Type_query
        { prefix; type_; trailing = Option.map condition_to_t trailing }
  | List qs -> List (List.map collapse_query qs)

let of_string s = collapse_query (query_of_string s)
let of_string_strict s = collapse_query (query_of_string ~recover:false s)

let of_function_body s =
  match feature_in_parens s with
  | Some feature -> feature_to_t feature
  | None -> of_string s

let feature name value : t =
  feature_to_t (plain_feature (name_of_string name) value)

let boolean name : t = feature_to_t (Boolean (name_of_string name) : feature)

(* ===== Sorting / classification ===== *)

type kind =
  | Hover
  | Responsive of int * float
  | Responsive_max of int * float
  | Preference_accessibility
  | Preference_appearance
  | Other

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

let rec kind : t -> kind = function
  | Hover _ | Any_hover _ -> Hover
  | Min_width px -> Responsive (0, px)
  | Max_width px -> Responsive_max (0, px)
  | Not_min_width px -> Responsive_max (0, px)
  | Min_width_rem rem -> Responsive (0, rem *. 16.)
  | Not_min_width_rem rem -> Responsive_max (0, rem *. 16.)
  | Min_width_length l ->
      let u, v = length_sort_key l in
      Responsive (u, v)
  | Not_min_width_length l ->
      let u, v = length_sort_key l in
      Responsive_max (u, v)
  | Prefers_reduced_motion _ | Prefers_contrast _ | Forced_colors _
  | Inverted_colors _ | Pointer _ | Any_pointer _ | Scripting _ ->
      Preference_accessibility
  | Prefers_color_scheme _ -> Preference_appearance
  | Width l | Height l ->
      let u, v = length_sort_key l in
      Responsive (u, v)
  | Aspect_ratio _ | Resolution _ | Color _ | Color_index _ | Monochrome _
  | Color_gamut _ | Video_color_gamut _ | Dynamic_range _
  | Video_dynamic_range _ | Scan _ | Update _ | Overflow_block _
  | Overflow_inline _ | Prefers_reduced_transparency _ | Prefers_reduced_data _
  | Nav_controls _ | Print | Orientation _ | Type_query _ | Boolean _ | List _
    ->
      Other
  | Plain (name, value) -> (
      match name with
      | Min Width | Max Width | Width ->
          let u, v = value_sort_key value in
          Responsive (u, v)
      | Prefers_color_scheme -> Preference_appearance
      | Prefers_reduced_motion | Prefers_contrast | Forced_colors
      | Inverted_colors | Pointer | Any_pointer | Scripting ->
          Preference_accessibility
      | Hover -> Hover
      | _ -> Other)
  | Range (name, _, value) | Range_rev (value, _, name) -> (
      match name with
      | Width | Min Width | Max Width ->
          let u, v = value_sort_key value in
          Responsive (u, v)
      | _ -> Other)
  | Interval (lo, _, name, _, _) -> (
      match name with
      | Width | Min Width | Max Width ->
          let u, v = value_sort_key lo in
          Responsive (u, v)
      | _ -> Other)
  | And (a, b) | Or (a, b) -> (
      match (kind a, kind b) with
      | Other, other | other, Other -> other
      | ka, _ -> ka)
  | Negated inner -> kind inner

let group_order = function
  | Hover -> (0, 0.)
  | Other -> (500, 0.)
  | Preference_accessibility -> (1000, 0.)
  | Responsive_max (unit_ord, value) ->
      (1999, (Float.of_int unit_ord *. 1e9) +. value)
  | Responsive (unit_ord, value) ->
      (2000, (Float.of_int unit_ord *. 1e9) +. value)
  | Preference_appearance -> (3000, 0.)

let rec preference_order = function
  | Prefers_reduced_motion No_preference -> 0
  | Prefers_reduced_motion Reduce -> 1
  | Prefers_contrast More -> 2
  | Prefers_contrast Less -> 3
  | Prefers_color_scheme _ -> 4
  | Forced_colors _ -> 5
  | Inverted_colors _ -> 6
  | Pointer None -> 7
  | Pointer Coarse -> 8
  | Pointer Fine -> 9
  | Any_pointer None -> 10
  | Any_pointer Coarse -> 11
  | Any_pointer Fine -> 12
  | Scripting None -> 13
  | Scripting Initial_only -> 14
  | Scripting Enabled -> 15
  | Hover _ | Any_hover _ -> 16
  | Prefers_reduced_transparency No_preference -> 17
  | Prefers_reduced_transparency Reduce -> 18
  | Prefers_reduced_data No_preference -> 19
  | Prefers_reduced_data Reduce -> 20
  | Nav_controls _ -> 21
  | And (a, _) | Or (a, _) -> preference_order a
  | Negated inner -> preference_order inner
  | _ -> 30

let rec responsive_subkind = function
  | Not_min_width _ | Not_min_width_rem _ | Not_min_width_length _ -> 0
  | Max_width _ -> 1
  | Min_width _ | Min_width_rem _ | Min_width_length _ -> 2
  | And (a, _) | Or (a, _) -> responsive_subkind a
  | Negated inner -> responsive_subkind inner
  | _ -> 2

let compare a b =
  let ka, kb = (kind a, kind b) in
  let ga, va = group_order ka and gb, vb = group_order kb in
  let comparisons =
    [
      Int.compare ga gb;
      Float.compare va vb;
      Int.compare (responsive_subkind a) (responsive_subkind b);
      Int.compare (preference_order a) (preference_order b);
      String.compare (to_string a) (to_string b);
    ]
  in
  List.find_opt (( <> ) 0) comparisons |> Option.value ~default:0

let equal a b = compare a b = 0
