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

let list_wrap_separator_case () =
  let pp_list threshold =
    Pp.list_wrap ~threshold ~sep:Pp.semicolon ~wrap_indent:2 Pp.string
  in
  check string "pretty separator" "a;b;c"
    (Pp.to_string ~minify:false (pp_list 80) [ "a"; "b"; "c" ]);
  check string "wrapped separator" "a;\n  bb"
    (Pp.to_string ~minify:false (pp_list 3) [ "a"; "bb" ]);
  check string "minified separator" "a;b;c"
    (Pp.to_string ~minify:true (pp_list 3) [ "a"; "b"; "c" ])

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
  (* A non-integer whose magnitude, scaled to the eight decimal places, passes
     [2^62] has fewer than eight decimals in its shortest decimal, so the places
     keep every digit it reads back from. *)
  check_float (1e11 +. 0.5) ~expected:"100000000000.5";
  check_float 123456789012.25 ~expected:"123456789012.25";
  check_float 999999999999.75 ~expected:"999999999999.75";
  check_float (-123456789012.25) ~expected:"-123456789012.25";
  check_float 46116860184.27387904 ~expected:"46116860184.27388";
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
          true (Float.equal v f)
  in
  List.iter check_reparse
    [
      1e11 +. 0.5;
      123456789012.25;
      999999999999.75;
      -123456789012.25;
      46116860184.27387904;
      0x1p62;
      -0x1p62;
      0x1p62 +. 1024.;
      1.7976931348623157e308;
      0x1p52 -. 0.5;
    ]

let float_large_decimal_in_stylesheet () =
  let check_sheet name input expected =
    match of_string input with
    | Error e -> failf "%s: %s" name (Pp.to_string Error.pp e)
    | Ok { stylesheet; _ } ->
        check string name expected (to_string ~minify:true stylesheet)
  in
  check_sheet "scale" "a{transform:scale(123456789012.25)}"
    "a{transform:scale(123456789012.25)}";
  check_sheet "opacity" "b{opacity:calc(999999999999.75)}"
    "b{opacity:999999999999.75}";
  (* A browser subtracts a calc()'s operands in double precision before it
     clamps a length: with [--b:46116860184px], Chrome computes the first width
     to 0.27388px and its 12-digit print [46116860184.3px] to 0.3px, two layout
     units apart, and the second to 0.5px against 0px. *)
  check_sheet "calc operand" "a{width:calc(46116860184.27387904px - var(--b))}"
    "a{width:calc(46116860184.27388px - var(--b))}";
  check_sheet "half pixel" "a{width:calc(123456789012.5px - var(--b))}"
    "a{width:calc(123456789012.5px - var(--b))}";
  check_sheet "integral operand"
    "a{width:calc(4611686018427387904px - var(--b))}"
    "a{width:calc(4.611686018427388e18px - var(--b))}"

(* A magnitude past the [int] range prints in exponent notation. The exponent
   carries no sign and no leading zeros whatever the C library's printf pads it
   to, so the bytes are the same on every platform. CSS Syntax 3 sec. 4.3.13
   reads the [+] after [e] as optional, so [1e30] and [1e+30] are one value and
   the shorter is the minifier's. This is not CSSOM serialisation: CSSOM sec.
   6.7.2 rounds a <number> to six decimals and notes that "scientific notation
   is not used". *)
let float_exponent_digits () =
  check_float 1e19 ~expected:"1e19";
  check_float (-1e20) ~expected:"-1e20";
  check_float 3.40282e38 ~expected:"3.40282e38";
  check_float 1e308 ~expected:"1e308";
  match of_string "a{width:3.40282e38px}" with
  | Error e -> failf "width: %s" (Pp.to_string Error.pp e)
  | Ok { stylesheet; _ } ->
      check string "width" "a{width:3.40282e38px}"
        (to_string ~minify:true stylesheet)

(* CSS Syntax 3 sec. 4.3.13 reads a number's exponent as well as its digits, so
   a magnitude too small for the fixed decimal places still has a spelling of
   its own value, and printing it as [0] or [-0] would change the value. The
   rule [Pp.string_of_float] states: such a magnitude keeps [max_decimals]
   significant digits, as decimals while it has at most 24 leading zeros and in
   exponent notation past that. *)
let float_sub_floor_magnitude () =
  let zeros n = String.make n '0' in
  check_float 1e-9 ~expected:".000000001";
  check_float 1.5e-25 ~expected:(String.concat "" [ "."; zeros 24; "15" ]);
  check_float 1.5e-26 ~expected:"1.5e-26";
  check_float 1e-30 ~expected:"1e-30";
  check_float (-1e-30) ~expected:"-1e-30";
  check_float 4e-33 ~expected:"4e-33";
  check_float (-4e-33) ~expected:"-4e-33";
  check_float 1e-40 ~expected:"1e-40";
  check_float 1.23456789012345e-30 ~expected:"1.2345679e-30";
  (* The least subnormal double, whose shortest decimal is one digit. *)
  check_float 5e-324 ~expected:"5e-324";
  check string "string_of_float keeps the sign" "-0.0000000012"
    (Pp.string_of_float (-1.2e-9));
  check_float_n 2 0.001 ~expected:".001"

(* The rule [Pp.string_of_float] states. A decimal place rounds the number the
   author wrote, the shortest decimal that reads back as the double (CSS Syntax
   3 sec. 4.3.13 gives a number the value of its decimal digits), with a half
   going away from zero. That is what the css-minify-tests corpus expects:
   colors/0048 and colors/0049 print [0.8765] as [.877] at three places, though
   its double, 0.87649999999999995, lies below the half. No digit comes from the
   C library, so they are the same on every platform: mingw's runtime printed
   [100000000000.5] as [100000000001]. *)
let float_rounding_rule () =
  let at d f = Pp.string_of_float ~max_decimals:d f in
  check string "0.125 at 2" "0.13" (at 2 0.125);
  check string "0.375 at 2" "0.38" (at 2 0.375);
  check string "-0.125 at 2" "-0.13" (at 2 (-0.125));
  check string "2.5 at 0" "3" (at 0 2.5);
  check string "-2.5 at 0" "-3" (at 0 (-2.5));
  check string "0.8765 at 3" "0.877" (at 3 0.8765);
  check string "0.0045 at 3" "0.005" (at 3 0.0045);
  check string "0.416666875 at 8" "0.41666688" (at 8 0.416666875);
  check_float_n 3 (-0.8765) ~expected:"-.877";
  (* [0.1 +. 0.2] reads back from 0.30000000000000004, which is below the half
     at 16 places and exact at 17. *)
  check string "0.1 + 0.2 at 16" "0.3" (at 16 (0.1 +. 0.2));
  check string "0.1 + 0.2 at 17" "0.30000000000000004" (at 17 (0.1 +. 0.2));
  (* Seventeen significant digits are what these doubles read back from; the
     eight places keep every one of them. *)
  check_float 123456789.123456789 ~expected:"123456789.12345679";
  check_float 98765432.987654321 ~expected:"98765432.98765433";
  (* Past the scaled [int] range the shortest decimal has fewer than eight
     places, and an exact half is a digit like any other. *)
  check_float 100000000000.5 ~expected:"100000000000.5";
  check_float 100000000001.5 ~expected:"100000000001.5";
  check_float (-100000000000.5) ~expected:"-100000000000.5";
  check_float (-100000000001.5) ~expected:"-100000000001.5";
  check_float 50000000000.25 ~expected:"50000000000.25";
  check_float 50000000000.75 ~expected:"50000000000.75"

(* The [int] boundary: an integral value below 2^62 prints every digit, one at
   or past it its shortest decimal in exponent notation, which reads back as the
   same double (CSS Syntax 3 sec. 4.3.13). Twelve digits would read back as
   4611686018430000000, 2,612,096 away. *)
let float_int_boundary () =
  check_float (0x1p62 -. 512.) ~expected:"4611686018427387392";
  check_float 0x1p62 ~expected:"4.611686018427388e18";
  check_float (-0x1p62) ~expected:"-4.611686018427388e18";
  check_float (0x1p62 +. 1024.) ~expected:"4.611686018427389e18"

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

(* [rename_custom_property] is the one hook that answers for both sites a custom
   property's name is written: the declaration and every [var()] that reads it,
   fallbacks included. A rename over the AST instead would have to rebuild every
   typed value a reference sits inside. *)
let printed ?rename css =
  match Cascade.Css.of_string css with
  | Error _ -> Alcotest.failf "could not parse %S" css
  | Ok p ->
      Cascade.Css.to_string ~minify:true ?rename_custom_property:rename
        p.Cascade.Css.stylesheet

let rename_custom_property_case () =
  let ns name = "ns-" ^ name in
  check string "the declaration and the reference move together"
    ":root{--ns-a:1px}.x{width:var(--ns-a)}"
    (printed ~rename:ns ":root{--a:1px}.x{width:var(--a)}");
  check string "inside calc" ".x{width:calc(var(--ns-a)*4)}"
    (printed ~rename:ns ".x{width:calc(var(--a) * 4)}");
  check string "a fallback reference too" ".x{color:var(--ns-a,var(--ns-b))}"
    (printed ~rename:ns ".x{color:var(--a,var(--b))}");
  (* The caller decides what moves: one that leaves a prefix alone keeps it. *)
  let keep_tw name =
    if String.length name > 3 && String.sub name 0 3 = "tw-" then name
    else "tw-" ^ name
  in
  check string "a name the rename returns unchanged stays put"
    ":root{--tw-a:1px;--tw-shadow:0 \
     0}.x{width:var(--tw-a);box-shadow:var(--tw-shadow)}"
    (printed ~rename:keep_tw
       ":root{--a:1px;--tw-shadow:0 \
        0}.x{width:var(--a);box-shadow:var(--tw-shadow)}");
  check string "identity by default" ":root{--a:1px}.x{width:var(--a)}"
    (printed ":root{--a:1px}.x{width:var(--a)}")

(* CSSOM 1 sec. 2.1 (serialize a string, serialize a URL): a printer of a
   [<string>] or a url writes the text the reader takes back to the same string,
   whatever it holds. Each case builds a node around [s], prints it pretty and
   minified, reads the text back and takes the strings out of the result. *)
type string_case =
  | Case : {
      name : string;
      make : string -> 'a;
      pp : 'a Pp.t;
      read : Cursor.t -> 'a;
      strings : 'a -> string list;
    }
      -> string_case

let awkward_strings =
  [
    "\"";
    "'";
    "\\";
    "\n";
    "\"\"";
    "a\"b'c\\d\ne";
    "a b";
    "a\tb";
    "a(b)";
    "\\b c.woff";
    "x\\";
    "\r\012";
    "\127";
  ]

let quotes_case =
  Case
    {
      name = "quotes";
      make = (fun s : Properties.quotes -> Pairs [ (s, ""); ("", s) ]);
      pp = Properties.pp_quotes;
      read = Properties.read_quotes;
      strings =
        (function
        | Pairs pairs -> List.concat_map (fun (o, c) -> [ o; c ]) pairs
        | _ -> []);
    }

let content_strings : Properties.content -> string list = function
  | String s | Quoted { value = s; _ } | Counters { separator = s; _ } -> [ s ]
  | _ -> []

let content_cases =
  [
    Case
      {
        name = "content string";
        make = (fun s : Properties.content -> String s);
        pp = Properties.pp_content;
        read = Properties.read_content;
        strings = content_strings;
      };
    Case
      {
        name = "content single-quoted string";
        make =
          (fun s : Properties.content ->
            Quoted { value = s; quote = '\''; repr = None });
        pp = Properties.pp_content;
        read = Properties.read_content;
        strings = content_strings;
      };
    Case
      {
        name = "counters separator";
        make =
          (fun s : Properties.content ->
            Counters { name = "c"; separator = s; style = None });
        pp = Properties.pp_content;
        read = Properties.read_content;
        strings = content_strings;
      };
  ]

let image_strings : Properties.background_image -> string list = function
  | Url u | Quoted (u, _) -> [ u ]
  | Image_set options ->
      List.concat_map
        (fun (o : Properties.image_set_option) ->
          (match o.source with Url u | String u -> [ u ])
          @ Option.to_list o.mime_type)
        options
  | _ -> []

let image_case name make =
  Case
    {
      name;
      make;
      pp = Properties.pp_background_image;
      read = Properties.read_background_image;
      strings = image_strings;
    }

let image_cases =
  [
    image_case "url" (fun s : Properties.background_image -> Url s);
    image_case "single-quoted url" (fun s : Properties.background_image ->
        Quoted (s, '\''));
    image_case "image-set string and type" (fun s ->
        Image_set
          [ { source = String s; resolution = Some "1x"; mime_type = Some s } ]);
    image_case "image-set url" (fun s ->
        Image_set
          [ { source = Url s; resolution = Some "1x"; mime_type = None } ]);
  ]

let clip_path_case =
  Case
    {
      name = "clip-path url";
      make = (fun s : Properties.clip_path -> Clip_path_url s);
      pp = Properties.pp_clip_path;
      read = Properties.read_clip_path;
      strings = (function Clip_path_url u -> [ u ] | _ -> []);
    }

let src_strings (src : Font_face.src) =
  List.concat_map
    (function
      | Font_face.Url { url; format; _ } | Quoted_url { url; format; _ } ->
          url :: Option.to_list format
      | Local name -> [ name ]
      | Var _ -> [])
    src

let src_case name make =
  Case
    {
      name;
      make;
      pp = Font_face.pp;
      read = Font_face.read_src;
      strings = src_strings;
    }

let font_face_cases =
  [
    src_case "font src url and format" (fun s ->
        [ Font_face.Url { url = s; format = Some s; tech = None } ]);
    src_case "font src single-quoted url" (fun s ->
        [ Quoted_url { url = s; quote = '\''; format = None; tech = None } ]);
    src_case "font src local" (fun s -> [ Local s ]);
  ]

let import_case =
  Case
    {
      name = "@import";
      make =
        (fun s : Stylesheet.import_rule ->
          { url = s; layer = None; supports = None; media = None });
      pp = Stylesheet.pp_import_rule;
      read = Stylesheet.read_import_rule;
      strings = (fun (r : Stylesheet.import_rule) -> [ r.url ]);
    }

let namespace_case name make =
  Case
    {
      name;
      make = (fun s -> Stylesheet.v [ Namespace (Some "p", make s) ]);
      pp = Stylesheet.pp;
      read = Stylesheet.read;
      strings =
        List.concat_map (function
          | Stylesheet.Namespace (_, (Url (u, _) | Quoted u)) -> [ u ]
          | _ -> []);
    }

let namespace_cases =
  [
    namespace_case "@namespace string" (fun s : Stylesheet.namespace_url ->
        Quoted s);
    namespace_case "@namespace url" (fun s -> Url (s, Bare));
    namespace_case "@namespace single-quoted url" (fun s ->
        Url (s, Quoted '\''));
  ]

let other_cases =
  [
    Case
      {
        name = "keyframes name";
        make = (fun s : Properties.animation_name -> Quoted s);
        pp = Properties.pp_animation_name;
        read = Properties.read_animation_name;
        strings =
          (function Quoted s | Name s | Ambiguous s -> [ s ] | _ -> []);
      };
    Case
      {
        name = "registered string value";
        make = Fun.id;
        pp = Variables.pp_value Variables.String;
        read = (fun t -> Variables.read_value t Variables.String);
        strings = (fun s -> [ s ]);
      };
    Case
      {
        name = "registered custom-ident value";
        make = Fun.id;
        pp = Variables.pp_value Variables.Custom_ident;
        read = (fun t -> Variables.read_value t Variables.Custom_ident);
        strings = (fun s -> [ s ]);
      };
    Case
      {
        name = "string_of_kind_value content";
        make = (fun s : Properties.content -> String s);
        pp =
          (fun ctx v ->
            Pp.string ctx (Properties.string_of_kind_value Properties.Content v));
        read = Properties.read_content;
        strings = content_strings;
      };
  ]

let string_cases =
  (quotes_case :: content_cases)
  @ image_cases
  @ (clip_path_case :: font_face_cases)
  @ (import_case :: namespace_cases)
  @ other_cases

let check_string_case s minify (Case { name; make; pp; read; strings }) =
  let text = Pp.to_string ~minify pp (make s) in
  let label = Fmt.str "%s %S (minify=%b) printed %S" name s minify text in
  match read (Cursor.of_string text) with
  | v -> check (list string) label (strings (make s)) (strings v)
  | exception Error.Parse_error _ ->
      Alcotest.failf "%s does not read back" label

let string_printers_round_trip () =
  List.iter
    (fun s ->
      List.iter
        (fun minify -> List.iter (check_string_case s minify) string_cases)
        [ true; false ])
    awkward_strings

(* The same holds where the printer writes tokens rather than a typed node: a
   custom property's [url("...")] drops its quotes only when the bare body can
   hold the string, and two [quotes] values that differ print apart. *)
let token_strings_round_trip () =
  check string "a tab keeps the url quoted" {|a{--x:url("a\9 b")}|}
    (printed {|a{--x:url("a\9 b")}|});
  check string "a plain url drops its quotes" "a{--x:url(a.png)}"
    (printed {|a{--x:url("a.png")}|});
  let a = printed {|q{quotes:'"' '"'}|} and b = printed {|q{quotes:'""' ''}|} in
  check bool "two quotes values print apart" false (String.equal a b)

let suite =
  ( "pp",
    [
      Alcotest.test_case "minified" `Quick minified;
      Alcotest.test_case "pretty" `Quick pretty;
      Alcotest.test_case "indent" `Quick indent_case;
      Alcotest.test_case "block" `Quick block_case;
      Alcotest.test_case "list" `Quick list_case;
      Alcotest.test_case "list wrap separator" `Quick list_wrap_separator_case;
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
      Alcotest.test_case "float exponent digits" `Quick float_exponent_digits;
      Alcotest.test_case "float sub-floor magnitude" `Quick
        float_sub_floor_magnitude;
      Alcotest.test_case "float rounding rule" `Quick float_rounding_rule;
      Alcotest.test_case "float int boundary" `Quick float_int_boundary;
      Alcotest.test_case "nan never prints as a bare token" `Quick
        nan_never_prints_as_a_bare_token;
      Alcotest.test_case "nan leaf serializes as a math function" `Quick
        nan_leaf_serializes_as_a_math_function;
      Alcotest.test_case "cond" `Quick cond_case;
      Alcotest.test_case "space if pretty" `Quick space_if_pretty_case;
      Alcotest.test_case "combinations" `Quick combinations;
      Alcotest.test_case "rename custom property" `Quick
        rename_custom_property_case;
      Alcotest.test_case "string printers round-trip" `Quick
        string_printers_round_trip;
      Alcotest.test_case "token strings round-trip" `Quick
        token_strings_round_trip;
    ] )
