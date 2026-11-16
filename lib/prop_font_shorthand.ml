open Common
open Values
open Properties_intf
open Prop_common
open Prop_font_family

let read_line_height_length ?(allow_negative = false) t : line_height =
  let n, repr, unit = Cursor.number_repr_with_unit t in
  let n, repr = normalize_signed_zero n repr in
  if n < 0. && not allow_negative then
    Cursor.err_invalid t "line-height cannot be negative"
  else
    let authored () : line_height = Number { value = n; unit; repr } in
    match unit with
    | Some "px" -> if Pp.string_of_float n = repr then Px n else authored ()
    | Some "rem" -> if Pp.string_of_float n = repr then Rem n else authored ()
    | Some "em" -> if Pp.string_of_float n = repr then Em n else authored ()
    | Some "%" -> if Pp.string_of_float n = repr then Pct n else authored ()
    | None -> if Pp.string_of_float n = repr then Num n else authored ()
    (* CSS Inline 3 sec. 5.1 spells the property [normal | <number [0,inf]> |
       <length-percentage [0,inf]>], so a length unit the cases above do not
       carry still reads and a time, an angle or a name no unit table holds does
       not. *)
    | Some u when Values.is_length_unit u -> authored ()
    | Some u -> Cursor.err_invalid t ("line-height unit: " ^ u)

let rec numeric_line_height_calc_leaves : line_height calc -> line_height calc =
  function
  | Val (Num n) | Val (Number { value = n; unit = None; _ }) -> Num n
  | Nested inner -> Nested (numeric_line_height_calc_leaves inner)
  | Parens inner -> Parens (numeric_line_height_calc_leaves inner)
  | Expr (left, op, right) ->
      Expr
        ( numeric_line_height_calc_leaves left,
          op,
          numeric_line_height_calc_leaves right )
  | leaf -> leaf

let rec read_font_weight t : font_weight =
  let read_var t : font_weight = Var (read_var read_font_weight t) in
  (* A bare call carries no [calc()] wrapper for a printer to put back, so one
     that folds to a weight the literal grammar takes is that weight. CSS Values
     4 sec. 10.12 keeps a call whose value leaves [1,1000] instead of
     invalidating it, and only the call round-trips. *)
  let read_math t : font_weight =
    match read_calc ~result_type:`Number read_font_weight t with
    | Num n when n >= 1. && n <= 1000. -> Weight n
    | expr -> Calc expr
  in
  Cursor.ws t;
  Cursor.enum_or_calls "font-weight"
    [
      ("normal", Normal);
      ("bold", Bold);
      ("bolder", Bolder);
      ("lighter", Lighter);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    (* CSS Values 4 sec. 10.1 allows a math function wherever a [<number>] is
       allowed, [calc()] being one of them rather than the gate to the rest. One
       that folds to a constant becomes that weight and is range-checked with
       it; one that does not stays a calc. *)
    ~calls:
      (("var", read_var) :: ("calc", read_math)
      :: Values.math_function_calls read_math)
    ~default:(fun t ->
      let weight = Cursor.number t in
      if weight >= 1. && weight <= 1000. then (Weight weight : font_weight)
      else
        err_invalid_value t "font-weight"
          (Pp.string_of_float ~drop_leading_zero:true weight))
    t

let rec read_font_style t : font_style =
  Cursor.enum_or_var "font-style"
    [
      ("normal", (Normal : font_style));
      ("italic", (Italic : font_style));
      ("inherit", (Inherit : font_style));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_style t))
    ~default:(fun t ->
      Cursor.expect_string "oblique" t;
      Cursor.ws t;
      if Cursor.is_done t then Oblique
      else
        (* CSS Values 4 sec. 6 drops the unit on a zero [<length>] and on
           nothing else, so an angle slot that takes a bare zero spells it
           [<angle> | <zero>] the way Transforms 1 sec. 11 does. CSS Fonts 4
           sec. 2.4 writes [oblique <angle [-90deg,90deg]>?] and grants none. *)
        let first = read_angle_unit_required t in
        Cursor.ws t;
        if Cursor.is_done t then Oblique_angle first
        else
          (* CSS Fonts 4 sec. 4.4 swaps the endpoints of a descending [oblique
             <angle> <angle>] range rather than rejecting it, so the reader
             keeps the order it was written in. *)
          let second = read_angle_unit_required t in
          Oblique_range (first, second))
    t

(* CSS Values 4 sec. 10.8 gives an operand no keyword: the absolute and relative
   sizes and the CSS-wide keywords are not [<calc-value>]s, so [keywords] is off
   here and [calc(medium)] fails the way the browser drops it. *)
let rec read_font_size_in_math t : font_size =
  read_font_size_with ~keywords:false t

and read_font_size_with ?(keywords = true) t : font_size =
  let read_var t : font_size =
    Var (read_var (read_font_size_with ~keywords) t)
  in
  let read_calc t : font_size =
    Calc (read_calc ~result_type:`Value read_font_size_in_math t)
  in
  let read_length t : font_size =
    let len = read_non_negative_length ~with_keywords:false t in
    Length len
  in
  let read_pct t : font_size =
    let n = Cursor.pct t in
    if n < 0. then Cursor.err_invalid t "negative font-size percentage";
    Pct n
  in
  Cursor.enum_or_calls "font-size"
    (if keywords then
       [
         ("xx-small", (Xx_small : font_size));
         ("x-small", X_small);
         ("small", Small);
         ("medium", Medium);
         ("large", Large);
         ("x-large", X_large);
         ("xx-large", Xx_large);
         ("xxx-large", Xxx_large);
         ("larger", Larger);
         ("smaller", Smaller);
         ("math", Math);
         ("inherit", Inherit);
         ("initial", Initial);
         ("unset", Unset);
         ("revert", Revert);
         ("revert-layer", Revert_layer);
       ]
     else [])
    ~calls:[ ("var", read_var); ("calc", read_calc) ]
    ~default:(fun t ->
      (* Try percentage first, then length *)
      Cursor.one_of [ read_pct; read_length ] t)
    t

let read_font_size t : font_size = read_font_size_with t

let normalize_font_style : font_style -> font_style =
  let na = Values.normalize_angle in
  fun value ->
    match value with
    | Oblique_angle a -> preserve_if_equal value (Oblique_angle (na a))
    | Oblique_range (a, b) ->
        preserve_if_equal value (Oblique_range (na a, na b))
    | other -> other

let rec pp_font_style : font_style Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_font_style ctx v
  | Normal -> Pp.string ctx "normal"
  | Italic -> Pp.string ctx "italic"
  | Oblique -> Pp.string ctx "oblique"
  | Oblique_angle angle ->
      Pp.string ctx "oblique";
      Pp.space ctx ();
      pp_angle ctx angle
  | Oblique_range (first, second) ->
      Pp.string ctx "oblique";
      Pp.space ctx ();
      pp_angle ctx first;
      Pp.space ctx ();
      pp_angle ctx second
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

(* CSS Fonts 4 sec. 2.3 defines each keyword as a percentage, and the percentage
   is never longer, so minified output uses it. *)
let font_stretch_pct = function
  | Ultra_condensed -> Some 50.
  | Extra_condensed -> Some 62.5
  | Condensed -> Some 75.
  | Semi_condensed -> Some 87.5
  | Normal -> Some 100.
  | Semi_expanded -> Some 112.5
  | Expanded -> Some 125.
  | Extra_expanded -> Some 150.
  | Ultra_expanded -> Some 200.
  | _ -> None

let rec pp_font_stretch : font_stretch Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_font_stretch ctx v
  | Pct f -> Pp.pct ctx f
  (* Sec. 2.3 refuses a negative width written on its own, so the wrapper comes
     off only around a leaf that is a width by itself. *)
  | Calc c ->
      pp_calc
        ~unwrap:(fun (v : font_stretch) ->
          match v with Pct f -> f >= 0. | _ -> true)
        pp_font_stretch ctx c
  | Ultra_condensed -> Pp.string ctx "ultra-condensed"
  | Extra_condensed -> Pp.string ctx "extra-condensed"
  | Condensed -> Pp.string ctx "condensed"
  | Semi_condensed -> Pp.string ctx "semi-condensed"
  | Normal -> Pp.string ctx "normal"
  | Semi_expanded -> Pp.string ctx "semi-expanded"
  | Expanded -> Pp.string ctx "expanded"
  | Extra_expanded -> Pp.string ctx "extra-expanded"
  | Ultra_expanded -> Pp.string ctx "ultra-expanded"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_font_size_adjust_metric : font_size_adjust_metric Pp.t =
 fun ctx -> function
  | Ex_height -> Pp.string ctx "ex-height"
  | Cap_height -> Pp.string ctx "cap-height"
  | Ch_width -> Pp.string ctx "ch-width"
  | Ic_width -> Pp.string ctx "ic-width"
  | Ic_height -> Pp.string ctx "ic-height"

let rec pp_font_size_adjust : font_size_adjust Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Number f -> Pp.float ctx f
  (* CSS Fonts 5 sec. 2.5 refuses a negative aspect value written on its own, so
     the wrapper comes off only around a leaf that is one by itself. *)
  | Calc c ->
      pp_calc
        ~unwrap_num:(match c with Num n -> n >= 0. | _ -> true)
        pp_font_size_adjust ctx c
  | From_font -> Pp.string ctx "from-font"
  | Metric_number (metric, f) ->
      pp_font_size_adjust_metric ctx metric;
      Pp.space ctx ();
      Pp.float ctx f
  | Metric_from_font metric ->
      pp_font_size_adjust_metric ctx metric;
      Pp.space ctx ();
      Pp.string ctx "from-font"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_size_adjust ctx v

(* [<length-percentage>] is the whole of [font-size]'s numeric grammar, so a
   calc built from those leaves is a [<length>] calc and can be handed to the
   length simplifier. The trip back restores the public [font_size.Pct] node for
   a percentage leaf. A [var()] leaf carries a [font_size]-typed fallback, which
   does not survive the trip either way. *)
let rec length_of_font_size_calc : font_size calc -> length calc option =
  function
  | Val (Length l) -> Some (Val l)
  | Val (Pct n) -> Some (Val (Pct n : length))
  | Num n -> Some (Num n)
  | Math_const c -> Some (Math_const c)
  | Math_fn fn -> Some (Math_fn fn)
  | Var _ | Sibling_index | Sibling_count | Val _ -> None
  | Nested inner ->
      Option.map (fun c -> Nested c) (length_of_font_size_calc inner)
  | Parens inner ->
      Option.map (fun c -> Parens c) (length_of_font_size_calc inner)
  | Expr (left, op, right) -> (
      match (length_of_font_size_calc left, length_of_font_size_calc right) with
      | Some left, Some right -> Some (Expr (left, op, right))
      | _ -> None)

let font_size_of_length (l : length) : font_size =
  match l with Pct n -> Pct n | l -> Length l

let rec font_size_of_length_calc : length calc -> font_size calc option =
  function
  | Val l -> Some (Val (font_size_of_length l))
  | Num n -> Some (Num n)
  | Math_const c -> Some (Math_const c)
  | Math_fn fn -> Some (Math_fn fn)
  | Var _ | Sibling_index | Sibling_count -> None
  | Nested inner ->
      Option.map (fun c -> Nested c) (font_size_of_length_calc inner)
  | Parens inner ->
      Option.map (fun c -> Parens c) (font_size_of_length_calc inner)
  | Expr (left, op, right) -> (
      match (font_size_of_length_calc left, font_size_of_length_calc right) with
      | Some left, Some right -> Some (Expr (left, op, right))
      | _ -> None)

let rec pp_font_size : font_size Pp.t =
 fun ctx -> function
  | Length l -> pp_length ctx l
  | Pct f -> Pp.pct ctx f
  | Var v -> pp_var pp_font_size ctx v
  | Calc c -> (
      match (Pp.minified ctx, length_of_font_size_calc c) with
      | true, Some c -> pp_length ctx (Calc c)
      | _ -> pp_calc pp_font_size ctx c)
  | Xx_small -> Pp.string ctx "xx-small"
  | X_small -> Pp.string ctx "x-small"
  | Small -> Pp.string ctx "small"
  | Medium -> Pp.string ctx "medium"
  | Large -> Pp.string ctx "large"
  | X_large -> Pp.string ctx "x-large"
  | Xx_large -> Pp.string ctx "xx-large"
  | Xxx_large -> Pp.string ctx "xxx-large"
  | Larger -> Pp.string ctx "larger"
  | Smaller -> Pp.string ctx "smaller"
  | Math -> Pp.string ctx "math"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

(* CSS Values 4 sec. 10.12 keeps a math function valid past the [0,inf] range
   CSS Inline 3 sec. 5.1 gives line-height, and clamps at used-value time, so
   [calc(-10%)] computes and a bare [-10%] is dropped. The call stays on a
   negative for that reason. *)
let negative_line_height : line_height -> bool = function
  | Num f | Px f | Rem f | Em f | Pct f | Number { value = f; _ } -> f < 0.
  | _ -> false

let rec pp_line_height : line_height Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Px f -> Pp.unit ctx f "px"
  | Rem f -> Pp.unit ctx f "rem"
  | Em f -> Pp.unit ctx f "em"
  | Pct p -> Pp.pct ctx p
  | Num n -> Pp.float ctx n
  | Number { value; unit; repr } -> (
      match (ctx.minify, unit) with
      | false, None -> Pp.string ctx repr
      | false, Some unit ->
          Pp.string ctx repr;
          Pp.string ctx unit
      | true, None -> Pp.float ctx value
      | true, Some "%" -> Pp.pct ctx value
      (* Values 4 sec. 6.7.2 serialises a unit lowercase, as the constructors
         above print it, so the author's case ends at the unminified branch. A
         unit none of them names has no other spelling to agree with. *)
      | true, Some unit ->
          Pp.unit ctx value
            (if Values.is_length_unit unit then String.lowercase_ascii unit
             else unit))
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_line_height ctx v
  | Min args -> Pp.call "min" (Pp.list ~sep:Pp.comma pp_line_height) ctx args
  | Max args -> Pp.call "max" (Pp.list ~sep:Pp.comma pp_line_height) ctx args
  | Clamp (low, value, high) ->
      Pp.call "clamp"
        (Pp.list ~sep:Pp.comma pp_line_height)
        ctx [ low; value; high ]
  | Calc c ->
      pp_calc
        ~unwrap_num:(match c with Num f -> f >= 0. | _ -> true)
        ~unwrap:(fun v -> not (negative_line_height v))
        pp_line_height ctx c

let rec pp_font_weight : font_weight Pp.t =
 fun ctx -> function
  | Weight n -> Pp.float ctx n
  | Normal -> Pp.string ctx "normal"
  | Bold -> Pp.string ctx "bold"
  | Bolder -> Pp.string ctx "bolder"
  | Lighter -> Pp.string ctx "lighter"
  (* CSS Values 4 sec. 10.12 clamps a math function's result to the range at
     computed-value time, so [calc(0)] is a weight where the bare [0] is not:
     the wrapper comes off only when what is inside is a value on its own. *)
  | Calc c ->
      let in_range n = n >= 1. && n <= 1000. in
      pp_calc
        ~unwrap_num:(match c with Num n -> in_range n | _ -> true)
        ~unwrap:(fun (v : font_weight) ->
          match v with Weight n -> in_range n | _ -> true)
        pp_font_weight ctx c
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_weight ctx v

(* CSS Fonts 4 sec. 2.7: the shorthand body is [<size> [/<line-height>]?
   <family>+], so size and family are always emitted and the four prefix slots
   only when the value carries them. *)
let pp_font_variant_css21 ctx = function
  | (Normal : font_variant_css21) -> Pp.string ctx "normal"
  | Small_caps -> Pp.string ctx "small-caps"

let read_font_variant_css21 t : font_variant_css21 =
  Cursor.enum "font-variant-css21"
    [ ("normal", (Normal : font_variant_css21)); ("small-caps", Small_caps) ]
    t

(* CSS Fonts 4 (ED) sec. 5.3 writes the shorthand's width slot as
   [<font-width-css3>], the nine keywords and no percentage, so a width the
   longhand normalised to its percentage prints as the keyword it stands for.
   Chrome 146 refuses `font: 125% 12px serif` and reads `font: expanded 12px
   serif`. sec. 4.5 pairs each keyword with its percentage. *)
let font_width_css3 : font_stretch -> font_stretch option = function
  | Pct 50. | Ultra_condensed -> Some (Ultra_condensed : font_stretch)
  | Pct 62.5 | Extra_condensed -> Some Extra_condensed
  | Pct 75. | Condensed -> Some Condensed
  | Pct 87.5 | Semi_condensed -> Some Semi_condensed
  | Pct 100. | Normal -> Some Normal
  | Pct 112.5 | Semi_expanded -> Some Semi_expanded
  | Pct 125. | Expanded -> Some Expanded
  | Pct 150. | Extra_expanded -> Some Extra_expanded
  | Pct 200. | Ultra_expanded -> Some Ultra_expanded
  | Pct _ | Calc _ | Var _ | Inherit | Initial | Unset | Revert | Revert_layer
    ->
      None

let pp_font_width_css3 : font_stretch Pp.t =
 fun ctx width ->
  match font_width_css3 width with
  | Some keyword -> pp_font_stretch ctx keyword
  | None -> pp_font_stretch ctx width

let pp_font_prefix ctx style variant weight stretch =
  let first = ref true in
  let emit pp opt =
    Option.iter
      (fun v ->
        if not !first then Pp.space ctx ();
        first := false;
        pp ctx v)
      opt
  in
  emit pp_font_style style;
  emit pp_font_variant_css21 variant;
  emit pp_font_weight weight;
  emit pp_font_width_css3 stretch;
  !first

let pp_font_shorthand : font_shorthand Pp.t =
 fun ctx { style; variant; weight; stretch; size; line_height; family } ->
  if not (pp_font_prefix ctx style variant weight stretch) then Pp.space ctx ();
  pp_font_size ctx size;
  Option.iter
    (fun lh ->
      Pp.char ctx '/';
      pp_line_height ctx lh)
    line_height;
  Pp.space ctx ();
  pp_font_family ctx family

let rec pp_font : font Pp.t =
 fun ctx -> function
  | Shorthand sh -> pp_font_shorthand ctx sh
  | Caption -> Pp.string ctx "caption"
  | Icon -> Pp.string ctx "icon"
  | Menu -> Pp.string ctx "menu"
  | Message_box -> Pp.string ctx "message-box"
  | Small_caption -> Pp.string ctx "small-caption"
  | Status_bar -> Pp.string ctx "status-bar"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font ctx v

(* CSS Values 4 sec. 10.12: a math function is valid wherever its type is, and
   the [0,inf] range of sec. 5.1 is checked on the value it resolves to, not on
   each operand, so [calc(-10%)] reads where a literal [-10%] does not. Sec.
   10.8 gives an operand no keyword, so [normal] and the CSS-wide keywords are
   left out and [calc(normal)] fails the way the browser drops it; the unitless
   [<number>] of sec. 5.1 is a [<calc-value>] and [calc(1.5)] still reads. *)
(* A bare call carries no [calc()] wrapper for a printer to put back, so one
   that folds to a factor the literal grammar takes is that factor. CSS Values 4
   sec. 10.12 keeps a call whose value leaves the [0,inf] range of sec. 5.1
   instead of invalidating it, and only the call round-trips. *)
let read_bare_math read t : line_height =
  match (read t : line_height) with
  | Calc (Num n) when n >= 0. -> Num n
  | value -> value

(* CSS Values 4 sec. 10.2 gives min(), max() and clamp() their arguments' own
   type, so they read at either half of sec. 5.1's [<number> |
   <length-percentage>]. Over numbers the calc path folds the call to the
   coefficient it answers; over a length there is no coefficient to fold to,
   since a percentage resolves only at used-value time, so the call stands the
   way it does at a [<length>] slot.

   Sec. 10.2 also requires the arguments to have a consistent type. The number
   path answers for a comparison of numbers, so an argument here carries a unit
   and a bare number beside one is a mix of the two types. *)
let read_line_height_comparison_arg t : line_height =
  match read_line_height_length ~allow_negative:true t with
  | Num _ | Number { unit = Option.None; _ } ->
      Cursor.err_expected t "length or percentage"
  | value -> value

let read_line_height_list_call name mk t : line_height =
  Cursor.call name t (fun inner ->
      let args =
        Cursor.list ~sep:Cursor.comma ~at_least:1
          read_line_height_comparison_arg inner
      in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      mk args)

let read_line_height_clamp t : line_height =
  Cursor.call "clamp" t (fun inner ->
      let low = read_line_height_comparison_arg inner in
      Cursor.ws inner;
      Cursor.comma inner;
      let value = read_line_height_comparison_arg inner in
      Cursor.ws inner;
      Cursor.comma inner;
      let high = read_line_height_comparison_arg inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      (Clamp (low, value, high) : line_height))

(* The number path first, so a comparison whose arguments are all numbers keeps
   folding to the coefficient it always did. *)
let line_height_math_calls number =
  ( "min",
    fun t ->
      Cursor.one_of
        [ number; read_line_height_list_call "min" (fun a -> Min a) ]
        t )
  :: ( "max",
       fun t ->
         Cursor.one_of
           [ number; read_line_height_list_call "max" (fun a -> Max a) ]
           t )
  :: ("clamp", fun t -> Cursor.one_of [ number; read_line_height_clamp ] t)
  :: Values.math_function_calls_beside_comparisons number

let rec read_line_height_in_math t : line_height =
  let read_var t : line_height = Var (read_var read_line_height_in_math t) in
  let read_calc t : line_height =
    Calc
      (read_calc ~result_type:`Number_or_value read_line_height_in_math t
      |> numeric_line_height_calc_leaves)
  in
  Cursor.enum_or_calls "line-height" []
    ~calls:
      (("var", read_var) :: ("calc", read_calc)
      :: line_height_math_calls (read_bare_math read_calc))
    ~default:(read_line_height_length ~allow_negative:true)
    t

let rec read_line_height t : line_height =
  let read_var t : line_height = Var (read_var read_line_height t) in
  let read_calc t : line_height =
    Calc
      (read_calc ~result_type:`Number_or_value read_line_height_in_math t
      |> numeric_line_height_calc_leaves)
  in
  Cursor.enum_or_calls "line-height"
    [
      ("normal", Normal);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:
      (("var", read_var) :: ("calc", read_calc)
      :: line_height_math_calls (read_bare_math read_calc))
    ~default:read_line_height_length t

let read_shorthand_line_height_typed r : line_height =
  let before = Cursor.save r in
  match read_line_height r with
  | lh -> lh
  | exception Cursor.Parse_error _ ->
      Cursor.restore r before;
      Cursor.err_invalid r "invalid line-height in font shorthand"

let font_shorthand_prefix_ident = function
  | Some
      ( "italic" | "oblique" | "normal" | "small-caps" | "bold" | "bolder"
      | "lighter" | "ultra-condensed" | "extra-condensed" | "condensed"
      | "semi-condensed" | "semi-expanded" | "expanded" | "extra-expanded"
      | "ultra-expanded" ) ->
      true
  | _ -> false

(* Keyword -> which prefix slot it fills in the [font] shorthand. [Normal] is
   the absence keyword (style / variant / weight / stretch each have their own
   [normal]); we accept it and move on. *)
type font_prefix_slot =
  | Style of font_style
  | Variant of font_variant_css21
  | Weight of font_weight
  | Stretch of font_stretch
  | No_op

let font_prefix_slot_of = function
  | "italic" -> Style Italic
  | "oblique" -> Style Oblique
  | "small-caps" -> Variant Small_caps
  | "bold" -> Weight Bold
  | "bolder" -> Weight Bolder
  | "lighter" -> Weight Lighter
  (* CSS Fonts 4 (ED) sec. 5.3 writes the width slot as [<font-width-css3>],
     which is all nine keywords; Chrome 146 reads every one of them. *)
  | "ultra-condensed" -> Stretch Ultra_condensed
  | "extra-condensed" -> Stretch Extra_condensed
  | "condensed" -> Stretch Condensed
  | "semi-condensed" -> Stretch Semi_condensed
  | "semi-expanded" -> Stretch Semi_expanded
  | "expanded" -> Stretch Expanded
  | "extra-expanded" -> Stretch Extra_expanded
  | "ultra-expanded" -> Stretch Ultra_expanded
  | "normal" | _ -> No_op

let assign_font_prefix_slot ~(style : font_style option ref)
    ~(variant : font_variant_css21 option ref)
    ~(weight : font_weight option ref) ~(stretch : font_stretch option ref) =
  function
  | Style s -> if !style = None then style := Some s
  | Variant v -> if !variant = None then variant := Some v
  | Weight w -> if !weight = None then weight := Some w
  | Stretch st -> if !stretch = None then stretch := Some st
  | No_op -> ()

let try_numeric_font_weight r (weight : font_weight option ref) =
  let before = Cursor.save r in
  match read_font_weight r with
  | w when !weight = None ->
      weight := Some w;
      true
  | _ ->
      Cursor.restore r before;
      false
  | exception Cursor.Parse_error _ ->
      Cursor.restore r before;
      false

let read_optional_line_height r =
  Cursor.ws r;
  match Cursor.peek_delim r with
  | Some '/' ->
      Cursor.skip r;
      Cursor.ws r;
      Some (read_shorthand_line_height_typed r)
  | _ -> None

(* Parse the [font] shorthand body: a keyword prefix loop fills style / variant
   / weight / stretch (a [normal] binds no slot), then the required [size
   [/<line-height>]? <family>+] tail. *)
let read_font_shorthand r : font_shorthand =
  let style : font_style option ref = ref Option.None in
  let variant : font_variant_css21 option ref = ref Option.None in
  let weight : font_weight option ref = ref Option.None in
  let stretch : font_stretch option ref = ref Option.None in
  let assign = assign_font_prefix_slot ~style ~variant ~weight ~stretch in
  let rec consume_prefix () =
    Cursor.ws r;
    if Cursor.is_done r then ()
    else if font_shorthand_prefix_ident (Cursor.peek_keyword r) then (
      assign (font_prefix_slot_of (Cursor.ident ~keep_case:false r));
      consume_prefix ())
    else if try_numeric_font_weight r weight then consume_prefix ()
  in
  consume_prefix ();
  Cursor.ws r;
  let size = read_font_size r in
  let line_height = read_optional_line_height r in
  if long_generic_family_start r then
    Cursor.err_invalid r "generic font family must be a standalone family item";
  let family = read_font_family r in
  Cursor.ws r;
  Cursor.expect_eof r;
  {
    style = !style;
    variant = !variant;
    weight = !weight;
    stretch = !stretch;
    size;
    line_height;
    family;
  }

let rec read_font t : font =
  (* The shorthand is read whole, so a failure is the whole value's; [t] has
     passed the end of it by the time one is raised. *)
  let loc = Cursor.decl_value_loc t in
  let raw = Cursor.consume_to_decl_end ~trim:true t in
  let lower = String.lowercase_ascii (String.trim raw) in
  match lower with
  | "inherit" -> Inherit
  | "initial" -> Initial
  | "unset" -> Unset
  | "revert" -> Revert
  | "revert-layer" -> Revert_layer
  | "caption" -> Caption
  | "icon" -> Icon
  | "menu" -> Menu
  | "message-box" -> Message_box
  | "small-caption" -> Small_caption
  | "status-bar" -> Status_bar
  | _ ->
      let is_valid_var () =
        let r = Cursor.of_string raw in
        match Values.read_var (fun r -> read_font r) r with
        | (_ : font var) ->
            Cursor.ws r;
            Cursor.is_done r
        | exception Cursor.Parse_error _ -> false
      in
      if is_valid_var () then
        let r = Cursor.of_string raw in
        Var (Values.read_var (fun r -> read_font r) r)
      else if value_has_css_wide_mix raw then
        Cursor.err_invalid ~loc t "CSS-wide keyword mixed with other values"
      else
        let body =
          try read_font_shorthand (Cursor.of_string raw)
          with Cursor.Parse_error _ ->
            Cursor.err_invalid ~loc t "invalid font shorthand"
        in
        Shorthand body

let rec read_font_stretch_with ~keywords t : font_stretch =
  (* CSS Fonts 4 sec. 2.3: font-stretch percentage is non-negative. Sec. 10.12
     puts the range on the value a math function resolves to, so a folded
     comparison answers to it the way a literal does. *)
  let checked t n : font_stretch =
    if n < 0. then err_invalid_value t "font-stretch" (Pp.string_of_float n);
    Pct n
  in
  let read_percentage t : font_stretch = checked t (Cursor.pct t) in
  (* Sec. 10.1 puts a math function wherever the [<percentage>] stands, [calc()]
     being one of them rather than the gate to the rest, and sec. 10.12 checks
     the [0,inf] range on the value it resolves to: the call keeps out of
     [read_percentage], which is the literal's range check. Sec. 2.3.1 spells
     the width a [<percentage>] with no [<number>] beside it, so a call
     answering a [<length>] or a bare coefficient is none of its types. Sec.
     10.8 gives an operand no keyword, so [calc(inherit)] and [calc(condensed)]
     fail the way the browser drops them rather than surviving as a [Calc] the
     printer unwraps into a live value. *)
  let read_math t : font_stretch =
    Calc
      (read_calc ~result_type:`Percentage
         (read_font_stretch_with ~keywords:false)
         t)
  in
  Cursor.enum_or_calls "font-stretch"
    (if keywords then
       [
         ("ultra-condensed", (Ultra_condensed : font_stretch));
         ("extra-condensed", Extra_condensed);
         ("condensed", Condensed);
         ("semi-condensed", Semi_condensed);
         ("normal", Normal);
         ("semi-expanded", Semi_expanded);
         ("expanded", Expanded);
         ("extra-expanded", Extra_expanded);
         ("ultra-expanded", Ultra_expanded);
         ("inherit", Inherit);
         ("initial", Initial);
         ("unset", Unset);
         ("revert", Revert);
         ("revert-layer", Revert_layer);
       ]
     else [])
    ~calls:
      (("var", fun t -> Var (read_var (read_font_stretch_with ~keywords) t))
      :: ("calc", read_math)
      :: Values.percentage_math_function_calls ~pct:checked read_math)
    ~default:read_percentage t

let read_font_stretch t : font_stretch = read_font_stretch_with ~keywords:true t

(* CSS Values 4 (ED) sec. 10.10.1 sums a calc's same-unit children and returns
   the lone remaining child, which needs the typed length simplifier: the
   untyped one cannot add two [Val] leaves. [em] and [%] resolve against the
   parent font size rather than the element's own, but every term of one
   declaration shares that reference, so their sum is the same value whatever
   the reference turns out to be. *)
let normalize_font_size (fs : font_size) : font_size =
  match fs with
  | Length l -> preserve_if_equal fs (Length (Values.normalize_length l))
  | Calc c -> (
      match length_of_font_size_calc c with
      | None -> (
          match Values.eval_calc c with
          | Values.Val v -> v
          | folded -> Calc folded)
      | Some lc -> (
          match Values.normalize_length (Calc lc) with
          | Calc folded -> (
              match font_size_of_length_calc folded with
              | Some folded -> Calc folded
              | None -> fs)
          | length -> font_size_of_length length))
  | _ -> fs

let rec calc_has_computed_input : line_height calc -> bool = function
  | Math_const _ | Math_fn _ -> true
  | Nested inner | Parens inner -> calc_has_computed_input inner
  | Expr (left, _, right) ->
      calc_has_computed_input left || calc_has_computed_input right
  | Num _ | Val _ | Var _ | Sibling_index | Sibling_count -> false

(* CSS Values 4 sec. 10.10 simplifies arithmetic over numeric literals to one
   number. Normal minification emits a non-terminating result at Cascade's
   six-significant-figure computed-value budget. Lossless mode keeps it for the
   browser to evaluate, while [eval_calc] still performs exact folds. Math
   constants and functions retain the existing computed-value path, which owns
   their precision provenance separately. *)
let normalize_line_height ?(lossless = false) (lh : line_height) : line_height =
  match lh with
  | Calc c -> (
      match
        if lossless || calc_has_computed_input c then Option.None
        else Option.map (Pp.round_sig 6) (Values.eval_numeric_calc c)
      with
      (* The [0,inf] range is checked on what the call resolves to, so the
         six-figure result takes the same guard the exact fold below takes. *)
      | Option.Some f when f >= 0. -> Num f
      | Option.Some f -> Calc (Num f)
      | Option.None -> (
          match Values.eval_calc c with
          | Values.Num f when f >= 0. -> Num f
          | Values.Val v when not (negative_line_height v) -> v
          | folded -> Calc folded))
  (* The reader keeps [+120%] and [12.0px] in [Number] so an unminified print
     gives the bytes back, and [pp_line_height] writes the constructor's own
     spelling under [--minify]. Fold onto the constructor where there is one, or
     the two spellings reach one minified text through two nodes. *)
  | Number { value; unit; _ } -> (
      match Option.map String.lowercase_ascii unit with
      | Option.None -> Num value
      | Option.Some "%" -> Pct value
      | Option.Some "px" -> Px value
      | Option.Some "rem" -> Rem value
      | Option.Some "em" -> Em value
      | Option.Some _ -> lh)
  | _ -> lh

(* CSS Fonts 4 (ED) sec. 2.2 defines [normal] as "Same as 400" and [bold] as
   "Same as 700", so each keyword and its number name one weight and the number
   is the shorter spelling. *)
let rec numeric_font_weight_calc_leaves : font_weight calc -> font_weight calc =
  function
  | Val (Weight n) -> Num n
  | Nested inner -> Nested (numeric_font_weight_calc_leaves inner)
  | Parens inner -> Parens (numeric_font_weight_calc_leaves inner)
  | Expr (left, op, right) ->
      Expr
        ( numeric_font_weight_calc_leaves left,
          op,
          numeric_font_weight_calc_leaves right )
  | other -> other

let rec normalize_font_weight : font_weight -> font_weight = function
  | Normal -> Weight 400.
  | Bold -> Weight 700.
  (* CSS Values 4 sec. 10.12 clamps a math function's result to the range at
     computed-value time, so [calc(0)] is a weight and the literal [0] is not:
     unwrapping one outside [1,1000] would write CSS a browser drops. *)
  | Calc c as value -> (
      match eval_calc (numeric_font_weight_calc_leaves c) with
      | Num n when n >= 1. && n <= 1000. -> Weight n
      | Val v -> normalize_font_weight v
      | folded -> if folded == c then value else Calc folded)
  | value -> value

let rec numeric_size_adjust_calc_leaves :
    font_size_adjust calc -> font_size_adjust calc = function
  | Val (Number n) -> Num n
  | Nested inner -> Nested (numeric_size_adjust_calc_leaves inner)
  | Parens inner -> Parens (numeric_size_adjust_calc_leaves inner)
  | Expr (left, op, right) ->
      Expr
        ( numeric_size_adjust_calc_leaves left,
          op,
          numeric_size_adjust_calc_leaves right )
  | other -> other

let rec normalize_font_size_adjust : font_size_adjust -> font_size_adjust =
  function
  (* CSS Fonts 5 sec. 2.5 spells the aspect value [<number [0,inf]>], and CSS
     Values 4 sec. 10.12 clamps a math function past that range at
     computed-value time: unwrapping a negative one would write CSS a browser
     drops. *)
  | Calc c as value -> (
      match eval_calc (numeric_size_adjust_calc_leaves c) with
      | Num n when n >= 0. -> Number n
      | Val v -> normalize_font_size_adjust v
      | folded -> if folded == c then value else Calc folded)
  | value -> value

(* sec. 2.3 maps each width keyword onto a percentage, and getComputedStyle()
   serializes the property as a percentage however the value was written, so the
   keyword and its percentage name one width and the percentage is never
   longer. *)
let rec normalize_font_stretch (value : font_stretch) : font_stretch =
  match value with
  (* Sec. 2.3 spells the width [<percentage [0,inf]>], and CSS Values 4 sec.
     10.12 clamps a math function past that range at computed-value time:
     unwrapping a negative one would write CSS a browser drops. *)
  | Calc c -> (
      match eval_calc c with
      | Val (Pct p as leaf) when p >= 0. -> normalize_font_stretch leaf
      | folded -> if folded == c then value else Calc folded)
  | value -> (
      match font_stretch_pct value with Some pct -> Pct pct | None -> value)

let drop_font_initial_slot (type a) ~(is_initial : a -> bool) (opt : a option) :
    a option =
  match opt with Some v when is_initial v -> None | _ -> opt

(* sec. 2.7 gives the [font] shorthand [<'font-weight'>], [<'font-size'>] and
   [<'font-family'>#] slots, so each slot takes its longhand's fold. Its width
   slot is [<font-width-css3>], the keywords alone, so the percentage the
   longhand folds to is not a value the slot can hold.

   sec. 2.7 also resets every subproperty to its initial value before applying
   the slots given explicitly, so a slot holding its longhand's initial says
   what leaving it out says: drop it. The initials are [normal] for style,
   variant and width, [normal] (that is 400) for weight and [normal] for
   line-height; size and family are required and stay. *)
let normalize_font : font -> font =
 fun value ->
  match value with
  | Shorthand s ->
      let style =
        drop_font_initial_slot s.style ~is_initial:(function
          | (Normal : font_style) -> true
          | _ -> false)
      in
      let variant =
        drop_font_initial_slot s.variant ~is_initial:(function
          | (Normal : font_variant_css21) -> true
          | _ -> false)
      in
      let weight =
        option_map_preserve normalize_font_weight s.weight
        |> drop_font_initial_slot ~is_initial:(function
          | (Normal : font_weight) | Weight 400. -> true
          | _ -> false)
      in
      let stretch =
        drop_font_initial_slot s.stretch ~is_initial:(function
          | (Normal : font_stretch) -> true
          | _ -> false)
      in
      let line_height =
        drop_font_initial_slot s.line_height ~is_initial:(function
          | (Normal : line_height) -> true
          | _ -> false)
      in
      let family = normalize_font_family s.family in
      let size = normalize_font_size s.size in
      if
        style == s.style && variant == s.variant && weight == s.weight
        && stretch == s.stretch
        && line_height == s.line_height
        && family == s.family && size == s.size
      then value
      else
        Shorthand { style; variant; weight; stretch; size; line_height; family }
  | other -> other

let read_font_size_adjust_metric t : font_size_adjust_metric =
  Cursor.enum "font-size-adjust metric"
    [
      ("ex-height", (Ex_height : font_size_adjust_metric));
      ("cap-height", Cap_height);
      ("ch-width", Ch_width);
      ("ic-width", Ic_width);
      ("ic-height", Ic_height);
    ]
    t

let rec read_font_size_adjust t : font_size_adjust =
  let read_non_negative_number t =
    let n = Cursor.number t in
    if n < 0. then Cursor.err_invalid t "font-size-adjust must be non-negative";
    n
  in
  let read_metric_value t =
    let metric = read_font_size_adjust_metric t in
    Cursor.ws t;
    match Cursor.peek_keyword t with
    | Some "from-font" ->
        let _ = Cursor.ident t in
        Metric_from_font metric
    | _ -> Metric_number (metric, read_non_negative_number t)
  in
  (* CSS Values 4 sec. 10.1 puts a math function wherever the [<number>] stands,
     [calc()] being one of them rather than the gate to the rest, and sec. 10.12
     checks the [0,inf] range on the value it resolves to: the call keeps out of
     [read_non_negative_number], which is the literal's range check. *)
  let read_math t : font_size_adjust =
    Calc (read_calc ~result_type:`Number read_font_size_adjust t)
  in
  Cursor.enum_or_calls "font-size-adjust"
    [
      ("none", (None : font_size_adjust));
      ("from-font", From_font);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:
      (("var", fun t -> Var (Values.read_var read_font_size_adjust t))
      :: ("calc", read_math)
      :: Values.math_function_calls read_math)
    ~default:(fun t ->
      match Cursor.peek_ident t with
      | Some _ -> read_metric_value t
      | None -> Number (read_non_negative_number t))
    t
