(** Tests for CSS pretty-printing module *)

open Alcotest
open Cascade
open Css

(* Helper function for checking pp output *)
let check_pp ?(minify = false) name pp ?expected input =
  let expected = Option.value ~default:input expected in
  let result = Pp.to_string ~minify pp input in
  check string name expected result

let check_pp_minified name pp ?expected input =
  check_pp ~minify:true name pp ?expected input

let check_pp_pretty name pp ?expected input =
  check_pp ~minify:false name pp ?expected input

let check_float ?(minify = false) ?expected input =
  let expected = Option.value ~default:(string_of_float input) expected in
  let result = Pp.to_string ~minify Pp.float input in
  check string (Fmt.str "float %f" input) expected result

let check_float_n ?(minify = false) n ?expected input =
  let expected = Option.value ~default:(string_of_float input) expected in
  let result = Pp.to_string ~minify (Pp.float_n n) input in
  check string (Fmt.str "float_n %d %f" n input) expected result

let minified () = check_pp_minified "string output" Pp.string "test"
let pretty () = check_pp_pretty "string output" Pp.string "test"

let indent_case () =
  (* indent only outputs spaces when ctx.indent > 0, so we need to nest first *)
  check_pp_pretty "indented string"
    (Pp.nest 1 (Pp.indent Pp.string))
    ~expected:"  test" "test"

let block_case () =
  check_pp_minified "braces" (Pp.braces Pp.string) ~expected:"{content}"
    "content";
  check_pp_pretty "braces pretty" (Pp.braces Pp.string)
    ~expected:"{\n  content\n}" "content"

let list_case () =
  let pp_list = Pp.list ~sep:Pp.comma Pp.string in
  let result = Pp.to_string ~minify:true pp_list [ "a"; "b"; "c" ] in
  check string "comma list" "a,b,c" result;

  let result = Pp.to_string ~minify:false pp_list [ "a"; "b"; "c" ] in
  check string "comma list pretty" "a, b, c" result;

  (* Empty list *)
  let result = Pp.to_string ~minify:true pp_list [] in
  check string "empty list" "" result;

  (* Single item *)
  let result = Pp.to_string ~minify:true pp_list [ "single" ] in
  check string "single item" "single" result

let float_case () =
  (* Basic floats *)
  check_float 3.14159 ~expected:"3.14159";
  check_float 42.0 ~expected:"42";
  check_float 1.0 ~expected:"1";
  check_float (-3.14) ~expected:"-3.14"

let float_leading_zero () =
  (* CSSOM-style serialization drops leading zeros in both pretty and minified
     modes; pretty mode controls whitespace, not numeric source-token
     fidelity. *)
  check_float 0.5 ~expected:".5";
  check_float 0.25 ~expected:".25";
  check_float 0.125 ~expected:".125";

  check_float ~minify:true 0.5 ~expected:".5";
  check_float ~minify:true 0.25 ~expected:".25";
  check_float ~minify:true 0.125 ~expected:".125"

let float_negative_leading_zero () =
  check_float (-0.5) ~expected:"-.5";
  check_float (-0.25) ~expected:"-.25";

  check_float ~minify:true (-0.5) ~expected:"-.5";
  check_float ~minify:true (-0.25) ~expected:"-.25"

let float_zero_and_nan_inf () =
  (* Zero handling *)
  check_float 0.0 ~expected:"0";
  check_float (-0.0) ~expected:"0";

  (* Infinity clamping *)
  check_float infinity ~expected:"3.40282e38";
  check_float neg_infinity ~expected:"-3.40282e38";

  (* A bare [NaN] is not a CSS token. CSS Values 4 sec. 10.7.2 puts the [NaN]
     keyword in the math-function grammar and nowhere else, and sec. 10.13
     serialises a NaN-valued number as [calc(NaN)]; Chrome drops [opacity: NaN]
     and keeps [opacity: calc(NaN)]. *)
  check_float nan ~expected:"calc(NaN)"

let float_rounding_and_trim () =
  (* Rounding with float_n *)
  check_float_n 3 1.23456 ~expected:"1.235";
  (* Round up *)
  check_float_n 3 1.23444 ~expected:"1.234";
  (* Round down *)
  check_float_n 4 1.2300 ~expected:"1.23";
  (* Trim trailing zeros *)
  check_float_n 2 1.995 ~expected:"2";

  (* Round to integer *)

  (* Minified with float_n *)
  check_float_n ~minify:true 3 0.25 ~expected:".25";
  check_float_n ~minify:true 2 0.999 ~expected:"1"

let float_large_decimal () =
  (* A non-integer whose scaled magnitude passes [max_int] stringifies through
     exponent notation, and the decimal point is then spliced into what is no
     longer a pure digit string. Print the magnitude itself instead. *)
  check_float (1e11 +. 0.5) ~expected:"100000000000";
  check_float 123456789012.25 ~expected:"123456789012";
  check_float 999999999999.75 ~expected:"1e12";
  check_float (-123456789012.25) ~expected:"-123456789012";
  (* Below the overflow the scaled-integer path stays byte for byte the same. *)
  check_float (1e10 +. 0.5) ~expected:"10000000000.5"

let float_large_decimal_reparses () =
  let check_reparse f =
    let printed = Pp.to_string ~minify:true Pp.float f in
    match float_of_string_opt printed with
    | None -> failf "%S is not a number" printed
    | Some v ->
        check bool
          (Fmt.str "%s reads back as %f" printed f)
          true
          (Float.abs (v -. f) <= Float.abs f *. 1e-9)
  in
  List.iter check_reparse
    [ 1e11 +. 0.5; 123456789012.25; 999999999999.75; -123456789012.25 ]

let float_large_decimal_in_stylesheet () =
  let check_sheet name input expected =
    match of_string input with
    | Error e -> failf "%s: %s" name (Pp.to_string Error.pp e)
    | Ok { stylesheet; _ } ->
        check string name expected (to_string ~minify:true stylesheet)
  in
  check_sheet "scale" "a{transform:scale(123456789012.25)}"
    "a{transform:scale(123456789012)}";
  check_sheet "opacity" "b{opacity:calc(999999999999.75)}" "b{opacity:1e12}"

(* CSS Values 4 sec. 10.7.2 puts the [NaN] keyword in the math-function grammar
   and nowhere else, and sec. 10.9.2 keeps NaN inside a calculation tree, so a
   value that is NaN has no bare spelling. Chrome drops [width: NaNpx],
   [opacity: NaN] and [rotate: NaNdeg] as invalid, and computes each expected
   form below to the same value as its input. Whatever cascade prints has to
   survive its own reader unchanged. *)
let nan_never_prints_as_a_bare_token () =
  let check_sheet name input expected =
    match of_string ~strict:true input with
    | Error e -> failf "%s: %s" name (Pp.to_string Error.pp e)
    | Ok { stylesheet; _ } -> (
        let printed = to_string ~minify:true (optimize stylesheet) in
        check string name expected printed;
        match of_string ~strict:true printed with
        | Error e ->
            failf "%s: %S does not re-read: %s" name printed
              (Pp.to_string Error.pp e)
        | Ok { stylesheet = again; _ } ->
            check string
              (Fmt.str "%s is a fixpoint" name)
              printed
              (to_string ~minify:true (optimize again)))
  in
  check_sheet "length" ".a{width:calc(sqrt(-1) * 1px)}"
    ".a{width:calc(NaN*1px)}";
  check_sheet "relative length" ".a{width:calc(log(-1) * 1em)}"
    ".a{width:calc(NaN*1em)}";
  check_sheet "percentage" ".a{width:calc(sqrt(-1) * 1%)}"
    ".a{width:calc(NaN*1%)}";
  check_sheet "comparison function" ".a{opacity:min(sqrt(-1),1)}"
    ".a{opacity:calc(NaN)}";
  check_sheet "stepped value function" ".a{opacity:calc(rem(1,0))}"
    ".a{opacity:calc(NaN)}";
  (* A function the fold leaves alone keeps the form the author wrote, which is
     the same calculation tree and the same value. *)
  check_sheet "number" ".a{opacity:calc(sqrt(-1))}" ".a{opacity:calc(sqrt(-1))}";
  check_sheet "time" ".a{transition-duration:calc(sqrt(-1) * 1s)}"
    ".a{transition-duration:calc(sqrt(-1)*1s)}";
  check_sheet "number in a function" ".a{transform:scale(calc(sqrt(-1)))}"
    ".a{transform:scale(calc(sqrt(-1)))}";
  check_sheet "inverse trigonometry" ".a{rotate:asin(-20)}"
    ".a{rotate:asin(-20)}";
  check_sheet "custom property" ".a{--x:calc(sqrt(-1))}"
    ".a{--x:calc(sqrt(-1))}"

(* A NaN reaching the printer from the typed API still has to come out as CSS.
   Sec. 10.13 serialises it as [calc(NaN)], with [* 1<unit>] appended once the
   value carries a unit. *)
let nan_leaf_serializes_as_a_math_function () =
  let check_decl name declaration expected =
    let printed =
      to_string ~minify:true
        (v [ rule ~selector:(Selector.class_ "a") [ declaration ] ])
    in
    check string name expected printed;
    match of_string ~strict:true printed with
    | Error e ->
        failf "%s: %S does not re-read: %s" name printed
          (Pp.to_string Error.pp e)
    | Ok { stylesheet; _ } ->
        check string
          (Fmt.str "%s is a fixpoint" name)
          printed
          (to_string ~minify:true stylesheet)
  in
  check_decl "length" (width (Px Float.nan)) ".a{width:calc(NaN*1px)}";
  check_decl "relative length" (width (Em Float.nan)) ".a{width:calc(NaN*1em)}";
  check_decl "percentage" (width (Pct Float.nan)) ".a{width:calc(NaN*1%)}";
  check_decl "number"
    (opacity (Opacity_number Float.nan))
    ".a{opacity:calc(NaN)}";
  check string "bare number" "calc(NaN)"
    (Pp.to_string ~minify:true Pp.float Float.nan);
  check string "angle" "calc(NaN*1deg)"
    (Pp.to_string ~minify:true Css.pp_angle (Deg Float.nan))

let cond_case () =
  let pp = Pp.cond (fun ctx -> not (Pp.minified ctx)) Pp.string Pp.nop in

  (* Conditional shows in pretty mode *)
  check_pp_pretty "conditional pretty" pp ~expected:"test" "test";

  (* Conditional hidden in minified mode *)
  check_pp_minified "conditional minified" pp ~expected:"" "test"

let space_if_pretty_case () =
  (* No space in minified *)
  let minified = Pp.to_string ~minify:true Pp.space_if_pretty () in
  check string "minified space" "" minified;

  (* Space in pretty *)
  let pretty = Pp.to_string ~minify:false Pp.space_if_pretty () in
  check string "pretty space" " " pretty

let combinations () =
  (* Combined formatters *)
  let pp_indented_braces = Pp.nest 1 (Pp.indent (Pp.braces Pp.string)) in
  check_pp_pretty "indented braces" pp_indented_braces
    ~expected:"  {\n    content\n  }" "content";

  (* List with indent - need to nest first to increase indent level *)
  let pp_indented_list =
    Pp.nest 1 (Pp.indent (Pp.list ~sep:Pp.comma Pp.string))
  in
  let result = Pp.to_string ~minify:false pp_indented_list [ "a"; "b"; "c" ] in
  check string "indented list" "  a, b, c" result;

  (* Nested braces *)
  let pp_nested = Pp.braces (Pp.braces Pp.string) in
  check_pp_minified "nested braces" pp_nested ~expected:"{{inner}}" "inner"

let suite =
  ( "pp",
    [
      Alcotest.test_case "minified" `Quick minified;
      Alcotest.test_case "pretty" `Quick pretty;
      Alcotest.test_case "indent" `Quick indent_case;
      Alcotest.test_case "block" `Quick block_case;
      Alcotest.test_case "list" `Quick list_case;
      Alcotest.test_case "float" `Quick float_case;
      Alcotest.test_case "float leading zero" `Quick float_leading_zero;
      Alcotest.test_case "float negative leading zero" `Quick
        float_negative_leading_zero;
      Alcotest.test_case "float zero nan inf" `Quick float_zero_and_nan_inf;
      Alcotest.test_case "float rounding and trim" `Quick
        float_rounding_and_trim;
      Alcotest.test_case "float large decimal" `Quick float_large_decimal;
      Alcotest.test_case "float large decimal reparses" `Quick
        float_large_decimal_reparses;
      Alcotest.test_case "float large decimal in stylesheet" `Quick
        float_large_decimal_in_stylesheet;
      Alcotest.test_case "nan never prints as a bare token" `Quick
        nan_never_prints_as_a_bare_token;
      Alcotest.test_case "nan leaf serializes as a math function" `Quick
        nan_leaf_serializes_as_a_math_function;
      Alcotest.test_case "cond" `Quick cond_case;
      Alcotest.test_case "space if pretty" `Quick space_if_pretty_case;
      Alcotest.test_case "combinations" `Quick combinations;
    ] )
