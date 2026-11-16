(** Property tests: backgrounds, gradients, masks, clipping, filters and
    shadows. *)

open Alcotest
open Cascade
open Css.Properties
open Css_test_helpers
open Properties_helpers

let check_background_shorthand =
  check_value_cursor "background_shorthand" read_background_shorthand
    pp_background_shorthand

let check_background_box =
  check_value_cursor "background_box" read_background_box pp_background_box

let check_background =
  check_value_cursor "background" read_background pp_background

let check_blend_mode =
  check_value_cursor "mix-blend-mode" read_blend_mode pp_blend_mode

let check_text_shadow =
  check_value_cursor "text-shadow" read_text_shadow pp_text_shadow

let check_shadow = check_value_cursor "shadow" read_shadow pp_shadow
let check_filter = check_value_cursor "filter" read_filter pp_filter

let check_filter_function =
  check_value_cursor "filter function" read_filter_function pp_filter_function

let check_background_attachment =
  check_value_cursor "background-attachment" read_background_attachment
    pp_background_attachment

let check_background_repeat =
  check_value_cursor "background-repeat" read_background_repeat
    pp_background_repeat

(* The standalone longhand (comma-separated layer list) goes through the [_list]
   readers; the single-value [check_*] above is the per-layer / shorthand
   path. *)
let check_background_size =
  check_value_cursor "background-size" read_background_size pp_background_size

let check_background_image =
  check_value_cursor "background-image" read_background_image
    pp_background_image

let check_background_position =
  check_value_cursor "background-position" read_background_position
    pp_background_position

let check_gradient_direction =
  check_value_cursor "gradient-direction" read_gradient_direction
    pp_gradient_direction

let check_gradient_position =
  check_value_cursor "gradient-position" read_gradient_position
    pp_gradient_position

let check_gradient_stop =
  check_value_cursor "gradient-stop" read_gradient_stop pp_gradient_stop

let check_radial_shape =
  check_value_cursor "radial_shape" read_radial_shape pp_radial_shape

let check_radial_size =
  check_value_cursor "radial_size" read_radial_size pp_radial_size

let check_radial_gradient_config =
  check_value_cursor "radial_gradient_config" read_radial_gradient_config
    pp_radial_gradient_config

let check_conic_gradient_config =
  check_value_cursor "conic_gradient_config" read_conic_gradient_config
    pp_conic_gradient_config

let check_color_interpolation =
  check_value_cursor "color_interpolation" read_color_interpolation
    pp_color_interpolation

let read_required_hue_interpolation_method t =
  match read_hue_interpolation_method t with
  | Some value -> value
  | None -> Cursor.err_expected t "hue-interpolation-method"

let check_hue_interpolation_method =
  check_value_cursor "hue_interpolation_method"
    read_required_hue_interpolation_method pp_hue_interpolation_method

let check_mask_border_mode =
  check_value_cursor "mask_border_mode" read_mask_border_mode
    pp_mask_border_mode

let check_clip_geometry_box =
  check_value_cursor "clip_geometry_box" read_clip_geometry_box
    pp_clip_geometry_box

let check_clip_path_extent =
  check_value_cursor "clip_path_extent" read_clip_path_extent
    pp_clip_path_extent

let check_clip_path_fill_rule =
  check_value_cursor "clip_path_fill_rule" read_clip_path_fill_rule
    pp_clip_path_fill_rule

let check_clip = check_value_cursor "clip" read_clip pp_clip

let check_webkit_mask_box =
  check_value_cursor "webkit_mask_box" read_webkit_mask_box pp_webkit_mask_box

let check_mask_type = check_value_cursor "mask_type" read_mask_type pp_mask_type

let test_background_box () =
  check_background_box "border-box";
  check_background_box "padding-box";
  check_background_box "content-box";
  (* Comma-separated layers are valid; space-separated is not. *)
  decl_optimizes ~prop:"background-clip"
    ~into:"border-box,padding-box,content-box"
    "border-box,padding-box,content-box";
  neg_cursor read_background_box "invalid-box";
  neg_cursor read_background_box "margin-box";
  (* doesn't exist for background *)
  neg_cursor ~allow_partial:true read_background_box "border-box padding-box";
  (* multiple *)
  neg_cursor read_background_box "borderbox";
  (* missing hyphen *)
  (* SVG value, not background *)
  neg_cursor read_background_box "fill-box"

let test_background () =
  check_background "red";
  check_background "url(image.png)";
  (* pp holds the authored side keyword and Named blue; the side->angle and
     Named->hex folds are optimize transforms. *)
  check_background ~expected:"linear-gradient(to right,red,blue)"
    "linear-gradient(to right, red, blue)";
  decl_optimizes ~prop:"background" ~into:"linear-gradient(90deg,red,#00f)"
    "linear-gradient(to right, red, blue)";
  check_background ~expected:"url(image.png)center/cover no-repeat fixed red"
    "red url(image.png) center/cover no-repeat fixed";
  (* Multi-layer shorthand (CSS Backgrounds 3 sec. 2.1): the layer comma
     separates layers and must not be eaten by a per-component reader. [repeat]
     is the default and folds away; the second layer's [space] stays. *)
  decl_optimizes ~prop:"background" ~into:"url(a.png),url(b.png)space"
    "url(a.png) repeat,url(b.png) space";
  (* pp holds the [none] keyword; the fold to the equivalent, and shorter, [0 0]
     layer is an optimize transform (test_declaration pins it). *)
  check_background "none";
  neg_cursor read_background "invalid-background";
  neg_cursor ~allow_partial:true read_background "red blue";
  (* multiple colors without gradient *)
  (* Per CSS Images Module, [url()] with an empty URL is spec-valid. *)
  neg_cursor read_background "center center center";
  (* too many positions *)
  (* invalid position syntax *)
  neg_cursor read_background "10px 20px 30px 40px 50px"

let test_background_shorthand () =
  check_background_shorthand "red";
  check_background_shorthand "url(image.png)";
  check_background_shorthand "center";
  check_background_shorthand "no-repeat";
  (* pp prints the axis pair it was handed; the fold to the one-keyword spelling
     is an optimize step (test_declaration pins it). *)
  check_background_shorthand "repeat repeat";
  check_background_shorthand ~expected:"url(image.png)red" "red url(image.png)";
  check_background_shorthand ~expected:"url(image.png)center"
    "url(image.png) center";
  check_background_shorthand ~expected:"url(image.png)center no-repeat red"
    "red url(image.png) center no-repeat";
  neg_cursor read_background_shorthand "invalid invalid";
  neg_cursor ~allow_partial:true read_background_shorthand "red blue green";
  (* multiple colors *)
  (* incomplete size syntax *)
  neg_cursor read_background_shorthand "center/";
  (* a single background layer accepts only one repeat-style *)
  neg_cursor read_background_shorthand "repeat repeat repeat"

let test_blend_mode () =
  check_blend_mode "normal";
  check_blend_mode "multiply";
  check_blend_mode "screen";
  check_blend_mode "overlay";
  check_blend_mode "darken";
  check_blend_mode "lighten";
  check_blend_mode "color-dodge";
  check_blend_mode "color-burn";
  check_blend_mode "hard-light";
  check_blend_mode "soft-light";
  check_blend_mode "difference";
  check_blend_mode "exclusion";
  check_blend_mode "hue";
  check_blend_mode "saturation";
  check_blend_mode "color";
  check_blend_mode "luminosity";
  neg_cursor read_blend_mode "invalid-blend"

let test_background_attachment () =
  check_background_attachment "scroll";
  check_background_attachment "fixed";
  check_background_attachment "local";
  check_background_attachment "inherit";
  check_background_attachment "scroll,fixed,local";
  neg_cursor read_background_attachment "invalid-attachment"

let test_background_repeat () =
  check_background_repeat "repeat";
  check_background_repeat "space";
  check_background_repeat "round";
  check_background_repeat "no-repeat";
  check_background_repeat "repeat-x";
  check_background_repeat "repeat-y";
  check_background_repeat "inherit";
  (* CSS Backgrounds 3 sec. 2.6: the longhand is a comma-separated layer
     list. *)
  decl_optimizes ~prop:"background-repeat" ~into:"no-repeat,repeat-y,no-repeat"
    "no-repeat,repeat-y,no-repeat";
  decl_optimizes ~prop:"background-repeat" ~into:"repeat-x,repeat-y"
    "repeat-x,repeat-y";
  decl_optimizes ~prop:"background-repeat" ~into:"repeat space,no-repeat"
    "repeat space,no-repeat";
  neg_cursor read_background_repeat "invalid-repeat"

let test_background_size () =
  check_background_size "auto";
  check_background_size "cover";
  check_background_size "contain";
  check_background_size "50px";
  check_background_size "50%";
  check_background_size "inherit";
  (* <bg-size> components are <length-percentage> (CSS Backgrounds 3), so var()
     and calc() are valid; a non-reducible calc() is preserved, including in the
     two-value form. *)
  check_background_size "var(--s)";
  check_background_size "calc(50% + 10px)";
  check_background_size "calc(50% + 10px) auto";
  (* CSS Backgrounds 3 sec. 3.9 fills each of the two slots with a length or
     [auto], and no sizing function stands in either. *)
  check_background_size "auto 300px";
  neg_cursor read_background_size "fit-content(20rem)";
  (* Comma-separated layer list (CSS Backgrounds 3 sec. 2.9). *)
  decl_optimizes ~prop:"background-size" ~into:"cover,contain" "cover,contain";
  decl_optimizes ~prop:"background-size" ~into:"100px 200px,auto"
    "100px 200px,auto";
  neg_cursor read_background_size "invalid-size"

let test_gradient_direction () =
  (* "to <side>" is a <side-or-corner>, a distinct node from the <angle> it
     equals (corners like "to top right" are not fixed angles), so pp holds the
     authored keyword; converting a side to its angle is an optimize
     transform. *)
  check_gradient_direction "to top";
  check_gradient_direction "to right";
  check_gradient_direction "to bottom";
  check_gradient_direction "to left";
  (* optimize+minify converts side keywords to angles, then elides the default
     bottom direction. Stops are already-canonical colours so only the direction
     changes. *)
  let optimizes ~into dir =
    let into =
      if into = "" then "linear-gradient(red,#123456)"
      else "linear-gradient(" ^ into ^ ",red,#123456)"
    in
    decl_optimizes ~prop:"background" ~into
      ("linear-gradient(" ^ dir ^ ",red,#123456)")
  in
  optimizes ~into:"90deg" "to right";
  optimizes ~into:"" "to bottom";
  optimizes ~into:"270deg" "to left";
  (* [to top] is the one side keyword whose angle also disappears: it is the
     default direction turned 180 degrees, so reversing the stops absorbs it. *)
  decl_optimizes ~prop:"background" ~into:"linear-gradient(#123456,red)"
    "linear-gradient(to top,red,#123456)";
  neg_cursor read_gradient_direction "invalid-direction"

(* CSS Images 4 sections 6.1-6.3: the gradient prelude is a linear direction, a
   radial shape/size/position, or a conic angle/position; a typed custom
   property holding a gradient prelude accepts any of the three. *)
let test_gradient_position () =
  check_gradient_position "to right";
  check_gradient_position "45deg";
  check_gradient_position "to right in oklab";
  (* No explicit direction: the interpolation prints without a redundant [to
     bottom], distinct from [to bottom in <interp>]. *)
  check_gradient_position "in oklab";
  check_gradient_position "in oklch shorter hue";
  check_gradient_position "circle at center";
  check_gradient_position "closest-side";
  check_gradient_position "at center";
  check_gradient_position "from 45deg";
  check_gradient_position "from 90deg at left top";
  check_gradient_position "var(--gradient-position)";
  neg_cursor ~allow_partial:true read_gradient_position "invalid-position"

let test_gradient_stop () =
  (* Basic color stops *)
  check_gradient_stop "red";
  check_gradient_stop "blue 50%";
  check_gradient_stop ~expected:"#ff5733 25%" "#ff5733 25%";
  check_gradient_stop ~expected:"rgb(255 0 0) 10px" "rgb(255,0,0) 10px";
  decl_optimizes ~prop:"background-image" ~into:"linear-gradient(#00f 50%,red)"
    "linear-gradient(blue 50%,red)";
  decl_optimizes ~prop:"background-image" ~into:"linear-gradient(red 10px,#00f)"
    "linear-gradient(rgb(255,0,0) 10px,blue)";

  (* Double position stops *)
  check_gradient_stop ~expected:"green 20% 40%" "green 20% 40%";

  (* Hint positions *)
  check_gradient_stop "50%";
  check_gradient_stop "10px";

  (* CSS variables in gradient stops *)
  check_gradient_stop "var(--tw-gradient-from)";
  check_gradient_stop "var(--color-blue-500) 50%";

  (* Complex var with fallback *)
  check_gradient_stop "var(--tw-gradient-from) var(--tw-gradient-from-position)";

  (* Multiple vars in sequence (as used in Tailwind gradients) *)
  check_gradient_stop ~expected:"var(--tw-gradient-position)"
    "var(--tw-gradient-position)";

  (* Nested var with complex fallback *)
  check_gradient_stop
    ~expected:
      "var(--tw-gradient-via-stops,var(--tw-gradient-position),var(--tw-gradient-from) \
       var(--tw-gradient-from-position),var(--tw-gradient-to) \
       var(--tw-gradient-to-position))"
    "var(--tw-gradient-via-stops, var(--tw-gradient-position), \
     var(--tw-gradient-from) var(--tw-gradient-from-position), \
     var(--tw-gradient-to) var(--tw-gradient-to-position))";

  (* Invalid stops *)
  neg_cursor read_gradient_stop "invalid-stop";
  neg_cursor ~allow_partial:true read_gradient_stop "red blue";
  (* Two colors without position *)
  neg_cursor ~allow_partial:true read_gradient_stop
    "50% 25%" (* Two percentages without color *)

let test_color_interpolation () =
  check_color_interpolation "in oklab";
  check_color_interpolation "in oklch";
  check_color_interpolation "in srgb";
  check_color_interpolation "in hsl";
  check_color_interpolation "in lab";
  check_color_interpolation "in lch";
  (* CSS Color 5 sec. 9.1 writes the method [in [ <rectangular-color-space> |
     <polar-color-space> <hue-interpolation-method>? ]], and sec. 9 gives the
     same fifteen spaces [color-mix()] takes. Chrome 153 reads each of these,
     serialising [xyz] as [xyz-d65], and drops a hue method after a rectangular
     space. *)
  check_color_interpolation "in srgb-linear";
  check_color_interpolation "in display-p3";
  check_color_interpolation "in a98-rgb";
  check_color_interpolation "in prophoto-rgb";
  check_color_interpolation "in rec2020";
  check_color_interpolation "in xyz-d50";
  (* CSS Color 4 sec. 10.2 makes [xyz] and [xyz-d65] two names for one space, so
     the shorter one is the spelling of both. *)
  check_color_interpolation ~expected:"in xyz" "in xyz-d65";
  check_color_interpolation "in xyz";
  check_color_interpolation "in hwb";
  check_color_interpolation "in hwb longer hue";
  check_color_interpolation "in oklch increasing hue";
  (* A rectangular space never looks for the hue method, so the two words are
     left standing and the declaration around them is dropped. *)
  neg_cursor ~allow_partial:true read_color_interpolation "in srgb shorter hue";
  neg_cursor ~allow_partial:true read_color_interpolation "in oklab longer hue";
  neg_cursor read_color_interpolation "oklab";
  neg_cursor read_color_interpolation "in unknown";
  neg_cursor read_color_interpolation "in"

let test_hue_interpolation_method () =
  check_hue_interpolation_method "shorter hue";
  check_hue_interpolation_method "longer hue";
  check_hue_interpolation_method "increasing hue";
  check_hue_interpolation_method "decreasing hue";
  neg_cursor ~allow_partial:true read_hue_interpolation_method "shorter";
  neg_cursor ~allow_partial:true read_hue_interpolation_method "hue";
  neg_cursor ~allow_partial:true read_hue_interpolation_method "unknown hue"

(* ignore-test: url() escaping spans the printers, not a single property. *)
let test_url_escaping () =
  (* CSS Syntax 4.3.5: a quoted url() must escape the backslash and the
     delimiter quote it wraps, or the re-parse drops or mangles those bytes.
     Both the minify printer (Pp.url) and the pretty printer (pp_quoted_url)
     route through the same escaper. *)
  let render ~minify css =
    match Css.of_string css with
    | Ok { Css.stylesheet; _ } ->
        Css.to_string ~minify stylesheet |> String.trim
    | Error _ -> Alcotest.failf "parse failed: %s" css
  in
  let minify = render ~minify:true in
  let stable css =
    let canon = minify css in
    (* The minified output and the pretty output must both re-parse to the same
       value, i.e. neither printer lost or mangled a byte. *)
    Alcotest.(check string) ("minify idempotent " ^ css) canon (minify canon);
    Alcotest.(check string)
      ("pretty preserves " ^ css)
      canon
      (minify (render ~minify:false css))
  in
  Alcotest.(check string)
    "minify escapes the delimiter quote" {|.a{background:url("ab\"cd")}|}
    (minify {|.a{background:url("ab\"cd")}|});
  Alcotest.(check string)
    "minify escapes the backslash" {|.a{background:url("a\\b")}|}
    (minify {|.a{background:url("a\\b")}|});
  stable {|.a{background:url("ab\"cd")}|};
  stable {|.a{background:url("a\\b")}|};
  stable {|.a{background:url('x\\y')}|}

let test_background_image () =
  check_background_image "none";
  check_background_image "url(image.jpg)";
  (* CSS Images 4 linear-gradient prelude: [ <angle> | to <side-or-corner> ]? ||
     <color-interpolation-method>. A direction, an interpolation method, or both
     in either order are valid. *)
  (* pp holds the authored side keyword and Named blue; side->angle and
     Named->hex are optimize folds. The direction/interpolation pair is stored
     order-independently, so pp emits the canonical direction-first order. *)
  check_background_image ~expected:"linear-gradient(in oklab,red,blue)"
    "linear-gradient(in oklab, red, blue)";
  check_background_image ~expected:"linear-gradient(to right,red,blue)"
    "linear-gradient(to right, red, blue)";
  check_background_image ~expected:"linear-gradient(to right in oklab,red,blue)"
    "linear-gradient(to right in oklab, red, blue)";
  check_background_image ~expected:"linear-gradient(to right in oklab,red,blue)"
    "linear-gradient(in oklab to right, red, blue)";
  (* optimize+minify folds the side keyword to its angle and Named blue to
     hex. *)
  decl_optimizes ~prop:"background-image"
    ~into:"linear-gradient(in oklab,red,#00f)"
    "linear-gradient(in oklab, red, blue)";
  decl_optimizes ~prop:"background-image"
    ~into:"linear-gradient(90deg,red,#00f)"
    "linear-gradient(to right, red, blue)";
  decl_optimizes ~prop:"background-image"
    ~into:"linear-gradient(90deg in oklab,red,#00f)"
    "linear-gradient(to right in oklab, red, blue)";
  decl_optimizes ~prop:"background-image"
    ~into:"linear-gradient(90deg in oklab,red,#00f)"
    "linear-gradient(in oklab to right, red, blue)";
  check_background_image ~minify:false
    ~expected:"linear-gradient(in oklab, red, blue)"
    "linear-gradient(in oklab, red, blue)";
  check_background_image ~minify:false
    ~expected:"linear-gradient(to right in oklab, red, blue)"
    "linear-gradient(in oklab to right, red, blue)";
  check_background_image ~expected:"radial-gradient(in oklab,red,blue)"
    "radial-gradient(in oklab, red, blue)";
  check_background_image
    ~expected:"radial-gradient(in oklab circle at center,red,blue)"
    "radial-gradient(in oklab circle at center, red, blue)";
  check_background_image
    ~expected:"radial-gradient(in oklab circle at center,red,blue)"
    "radial-gradient(circle at center in oklab, red, blue)";
  decl_optimizes ~prop:"background-image"
    ~into:"radial-gradient(in oklab,red,#00f)"
    "radial-gradient(in oklab, red, blue)";
  decl_optimizes ~prop:"background-image"
    ~into:"radial-gradient(in oklab circle,red,#00f)"
    "radial-gradient(in oklab circle at center, red, blue)";
  decl_optimizes ~prop:"background-image"
    ~into:"radial-gradient(in oklab circle,red,#00f)"
    "radial-gradient(circle at center in oklab, red, blue)";
  check_background_image ~expected:"conic-gradient(in hsl longer hue,red,blue)"
    "conic-gradient(in hsl longer hue, red, blue)";
  check_background_image
    ~expected:"conic-gradient(in hsl longer hue from 45deg at center,red,blue)"
    "conic-gradient(in hsl longer hue from 45deg at center, red, blue)";
  check_background_image
    ~expected:"conic-gradient(in hsl longer hue from 45deg at center,red,blue)"
    "conic-gradient(from 45deg at center in hsl longer hue, red, blue)";
  decl_optimizes ~prop:"background-image"
    ~into:"conic-gradient(in hsl longer hue,red,#00f)"
    "conic-gradient(in hsl longer hue, red, blue)";
  decl_optimizes ~prop:"background-image"
    ~into:"conic-gradient(in hsl longer hue from 45deg at 50%,red,#00f)"
    "conic-gradient(in hsl longer hue from 45deg at center, red, blue)";
  decl_optimizes ~prop:"background-image"
    ~into:"conic-gradient(in hsl longer hue from 45deg at 50%,red,#00f)"
    "conic-gradient(from 45deg at center in hsl longer hue, red, blue)";
  check_background_image ~minify:false
    ~expected:"radial-gradient(in oklab, red, blue)"
    "radial-gradient(in oklab, red, blue)";
  check_background_image ~minify:false
    ~expected:"conic-gradient(in hsl longer hue, red, blue)"
    "conic-gradient(in hsl longer hue, red, blue)";
  (* CSS Images 4 sec. 3.5.6 gives the color-stop-list a comma-separated list of
     stops and hints, so a trailing item outside that grammar is invalid (CSS
     Syntax 3 sec. 8.2). [Cursor.call] did not require [linear-gradient()] /
     [radial-gradient()]'s reader to consume its whole sub-cursor, so the list
     was read up to the first unreadable stop and answered with a truncated
     gradient instead. *)
  check_background_image "linear-gradient(red,blue)";
  check_background_image "radial-gradient(red,blue)";
  check_background_image "repeating-linear-gradient(var(--stops))";
  check_background_image "repeating-radial-gradient(var(--stops))";
  check_background_image "repeating-conic-gradient(var(--stops))";
  check_background_image "-webkit-radial-gradient(var(--stops))";
  check_background_image "-webkit-repeating-radial-gradient(var(--stops))";
  check_background_image "-moz-radial-gradient(var(--stops))";
  check_background_image "-moz-repeating-radial-gradient(var(--stops))";
  check_background_image "-o-radial-gradient(var(--stops))";
  check_background_image "-o-repeating-radial-gradient(var(--stops))";
  neg_cursor read_background_image "linear-gradient(red, blue, bogus-token)";
  neg_cursor read_background_image "radial-gradient(red, blue, bogus-token)";
  (* CSS Images 4 sec. 6.2 gives [image-set()] a comma-separated
     [<image-set-option>]#; same [Cursor.call] gap in the option list. *)
  check_background_image ~expected:"image-set(\"a.png\"1x)"
    "image-set(\"a.png\" 1x)";
  neg_cursor read_background_image "image-set(\"a.png\" 1x, bogus-token)";
  (* [conic-gradient()] (sec. 3.6) reads its stop list the same way. *)
  neg_cursor read_background_image
    "conic-gradient(from 45deg, red, bogus-token)";
  (* Controls: whitespace around the commas and just inside the parens is not
     trailing content, so each call still stands. *)
  check_background_image ~expected:"linear-gradient(to right,red,blue)"
    "linear-gradient( to right , red , blue )";
  check_background_image ~expected:"image-set(\"a.png\"1x,\"b.png\"2x)"
    "image-set( \"a.png\" 1x , \"b.png\" 2x )";
  neg_cursor read_background_image "invalid-image"

let test_radial_shape () =
  check_radial_shape "circle";
  check_radial_shape "ellipse";
  neg_cursor read_radial_shape "square"

let test_radial_size () =
  check_radial_size "closest-side";
  check_radial_size "farthest-side";
  check_radial_size "closest-corner";
  check_radial_size "farthest-corner";
  check_radial_size "10px";
  check_radial_size ~expected:"50% 25%" "50% 25%";
  neg_cursor read_radial_size "invalid-size"

let test_radial_gradient_config () =
  check_radial_gradient_config "circle";
  check_radial_gradient_config "ellipse";
  check_radial_gradient_config "circle closest-side";
  check_radial_gradient_config "circle at center";
  neg_cursor ~allow_partial:true read_radial_gradient_config "invalid-config";
  (* pp holds the authored defaults; the optimizer elides them (CSS Images 4
     section 3.2: ellipse / farthest-corner / center are implied). *)
  decl_optimizes ~prop:"background"
    ~held:"radial-gradient(circle at center,red,#123456)"
    ~into:"radial-gradient(circle,red,#123456)"
    "radial-gradient(circle at center, red, #123456)";
  decl_optimizes ~prop:"background"
    ~held:"radial-gradient(ellipse farthest-corner,red,#123456)"
    ~into:"radial-gradient(red,#123456)"
    "radial-gradient(ellipse farthest-corner, red, #123456)"

let test_conic_gradient_config () =
  check_conic_gradient_config "from 45deg";
  check_conic_gradient_config "at center";
  check_conic_gradient_config "from 90deg at left top";
  neg_cursor read_conic_gradient_config "";
  neg_cursor read_conic_gradient_config "center";
  neg_cursor read_conic_gradient_config "from"

(* CSS Syntax 3 sec. 4.3.3 consumes the [%] into the percentage token, so
   whatever follows starts a fresh token and the separating space carries no
   meaning: it elides under minify, as it already does inside polygon() and
   animation-range. A unit ends in an ident instead ([10px 0] would re-tokenise
   as the single dimension [10px0]), so that space stays. *)
let test_background_position () =
  let assert_complete input =
    let t = Cursor.of_string input in
    ignore (read_background_position t);
    Cursor.ws t;
    if not (Cursor.is_done t) then
      Alcotest.failf "background-position left input after %S" input
  in
  let assert_axis_edge input axis edge =
    let t = Cursor.of_string input in
    match read_background_position t with
    | [ Axis_edge_offset (got_axis, got_edge, _) ] ->
        Alcotest.(check string) input axis got_axis;
        Alcotest.(check string) input edge got_edge
    | _ -> Alcotest.failf "background-position lost edge offset in %S" input
  in
  check_background_position "center";
  check_background_position "left top";
  check_background_position ~roundtrip:true ~expected:"100%0" "right 0";
  check_background_position ~roundtrip:true ~expected:"100%-15.625rem"
    "right -15.625rem";
  check_background_position "right .5rem center";
  check_background_position "left top 10px";
  check_background_position "left top 10%";
  check_background_position "center top 10px";
  check_background_position "center left 10px";
  check_background_position "top 10px left";
  List.iter assert_complete
    [
      "left top 10px";
      "left top 10%";
      "center top 10px";
      "center left 10px";
      "top 10px left";
    ];
  assert_axis_edge "center top 10px" "center" "top";
  assert_axis_edge "center left 10px" "center" "left";
  check_background_position ~roundtrip:true ~expected:"50%25%" "50% 25%";
  check_background_position "inherit";
  neg_cursor ~allow_partial:true read_background_position "invalid-position"

let test_object_fit () =
  check_object_fit "fill";
  check_object_fit "contain";
  check_object_fit "cover";
  check_object_fit "none";
  check_object_fit "scale-down";
  check_object_fit "inherit";
  neg_cursor read_object_fit "invalid-fit"

let test_text_shadow () =
  check_text_shadow "none";
  check_text_shadow "inherit";
  check_text_shadow "2px 2px";
  check_text_shadow "2px 2px 4px";
  check_text_shadow "2px 2px red";
  check_text_shadow "2px 2px 4px red";
  check_text_shadow "red 2px 2px" ~expected:"2px 2px red";
  check_text_shadow "red 2px 2px 4px" ~expected:"2px 2px 4px red";
  check_text_shadow "-2px -2px";
  check_text_shadow "0 0 10px";
  neg_cursor read_text_shadow "invalid-shadow";
  (* CSS Text Decoration 4 sec. 6.2 puts the colour on either side of the length
     run, so a [var()] ending the run with a colour for its fallback is the
     colour slot's reference, typed. *)
  (match
     (read_text_shadow (Cursor.of_string "2px 2px var(--c, rgb(1 2 3 / 25%))")
       : text_shadow)
   with
  | Text_shadow
      {
        blur = None;
        color = Some (Values.Var { fallback = Values.Fallback _; _ });
        _;
      } ->
      ()
  | _ -> Alcotest.fail "a colour fallback ends the run as the colour");
  check_text_shadow "2px 2px var(--c, #0004)" ~expected:"2px 2px var(--c,#0004)"

let test_filter_function () =
  List.iter
    (fun name ->
      check_filter_function ~roundtrip:true name;
      check_filter_function ~expected:name (String.uppercase_ascii name))
    [
      "blur";
      "brightness";
      "contrast";
      "grayscale";
      "hue-rotate";
      "invert";
      "opacity";
      "saturate";
      "sepia";
    ];
  neg_cursor read_filter_function "drop-shadow";
  neg_cursor read_filter_function "unknown";
  neg_cursor read_filter_function "blur()"

let test_filter () =
  check_filter "none";
  check_filter "blur(5px)";
  check_filter ~expected:"blur(5px)contrast(1.2)" "blur(5px) contrast(1.2)";
  check_filter ~expected:"hue-rotate(30deg)opacity(.5)"
    "hue-rotate(30deg) opacity(0.5)";
  check_filter ~expected:"drop-shadow(2px 4px 6px red)"
    "drop-shadow(2px 4px 6px red)";
  (* <url> reference to an SVG filter, in both the url-token and quoted forms,
     standalone and mixed with filter functions *)
  check_filter "url(#liquid)";
  check_filter "url(foo.svg#x)";
  check_filter ~expected:"url(#liquid)" "url(\"#liquid\")";
  check_filter ~expected:"url(#blur)blur(2px)" "url(#blur) blur(2px)";
  neg_cursor read_filter "invalid-filter";
  (* CSS Filter Effects 1 sec. 6.1 gives each of these one operand ([<length>],
     [<number>]/[<percentage>], or [<color> && <length>{2,3}] for
     [drop-shadow]); [Cursor.call] did not require the reader to consume its
     whole sub-cursor, so e.g. [blur(2px foo)] read only [2px] and answered
     [blur(2px)] instead of invalidating the whole [filter] value ([filter]'s
     top-level list is space-separated, so the truncated function was simply
     followed by the next one). *)
  List.iter (neg_cursor read_filter)
    [
      "blur(5px 5px)";
      "brightness(1.2 1.2)";
      "contrast(1.2 1.2)";
      "grayscale(.5 .5)";
      "hue-rotate(30deg 30deg)";
      "invert(.5 .5)";
      "opacity(.5 .5)";
      "saturate(1.5 1.5)";
      "sepia(.5 .5)";
      "drop-shadow(2px 4px 6px red foo)";
    ];
  (* Controls: whitespace directly inside the parens still stands. *)
  check_filter "blur( 5px )" ~expected:"blur(5px)";
  check_filter "opacity( .5 )" ~expected:"opacity(.5)";
  check_filter "drop-shadow( 2px 4px 6px red )"
    ~expected:"drop-shadow(2px 4px 6px red)"

let test_shadow () =
  check_shadow "none";
  check_shadow "2px 2px";
  check_shadow "2px 2px 4px";
  check_shadow "2px 2px 4px 1px";
  check_shadow "2px 2px red";
  check_shadow "2px 2px 4px red";
  check_shadow "2px 2px 4px 1px red";
  check_shadow "inset 2px 2px";
  check_shadow "inset 2px 2px 4px";
  check_shadow "inset 2px 2px 4px 1px";
  check_shadow "inset 2px 2px red";
  check_shadow "inset 2px 2px 4px red";
  check_shadow "inset 2px 2px 4px 1px red";
  check_shadow "red 2px 2px" ~expected:"2px 2px red";
  check_shadow "red 2px 2px 4px" ~expected:"2px 2px 4px red";
  check_shadow "red 2px 2px 4px 1px" ~expected:"2px 2px 4px 1px red";
  check_shadow "red inset 2px 2px" ~expected:"inset 2px 2px red";
  check_shadow "red inset 2px 2px 4px" ~expected:"inset 2px 2px 4px red";
  check_shadow "red inset 2px 2px 4px 1px" ~expected:"inset 2px 2px 4px 1px red";
  (* [inset var(--x)]: the [<length>{2,4} && <color>?] body supplied wholesale
     by one var, which the concrete-offset [Shadow] record cannot hold. *)
  check_shadow "inset var(--shadow)";
  check_shadow "var(--a),inset var(--b)";
  (* Test compact printing - when blur and spread are not provided, should print
     compactly. Cross-form colour canonicalization is an optimize transform. *)
  check_shadow "0 0 #0000" ~expected:"0 0 #0000";
  check_shadow "0 0 rgba(0,0,0,0)" ~expected:"0 0 rgb(0 0 0/0)";
  decl_optimizes ~prop:"box-shadow" ~held:"0 0 rgb(0 0 0/0)" ~into:"0 0 #0000"
    "0 0 rgba(0,0,0,0)";
  check_shadow "inherit";
  neg_cursor read_shadow "invalid-shadow";
  neg_cursor read_shadow "10px";
  (* A [var(--name,)] inset prefix stands in for the optional [inset] keyword,
     so the separator before the offset is load-bearing: if the var resolves to
     [inset] the result must read [inset 0 ...], never [inset0 ...]. The space
     must survive minification. *)
  let inset_var_shadow =
    shadow ~inset_var:"tw-ring-inset" ~h_offset:Zero ~v_offset:Zero ~blur:Zero
      ~spread:(Px 1.) ~color:(Values.hex "#000") ()
  in
  Alcotest.(check string)
    "inset-var shadow keeps the separator after the var prefix when minified"
    "var(--tw-ring-inset,) 0 0 0 1px #000"
    (Css.Pp.to_string ~minify:true pp_shadow inset_var_shadow);
  (* A var() colour with an unspecified blur (blur = None) serialises to [12px
     12px var(--c)]: pp does not pad in a [0] the author never wrote. An
     authored zero blur (Some Zero) before a var colour is the opposite case -
     it must keep the [0] (see the optimize test below), because a var() can
     resolve to a length, so dropping it would let [12px 12px var(--c)] re-bind
     the var as the blur. Some Zero and None are not interchangeable here. *)
  let var_color_shadow =
    shadow ~h_offset:(Px 12.) ~v_offset:(Px 12.)
      ~color:(Values.Var (Values.var_ref "c"))
      ()
  in
  Alcotest.(check string)
    "unspecified blur before a var colour is not padded with a 0"
    "12px 12px var(--c)"
    (Css.Pp.to_string ~minify:true pp_shadow var_color_shadow);
  (* [normalize_shadow] drops a zero blur when no spread follows, but not before
     a var() colour: dropping it there changes the value. *)
  let some_zero_var_shadow =
    Css.box_shadow
      (shadow ~h_offset:Zero ~v_offset:(Px 3.) ~blur:Zero
         ~color:(Values.Var (Values.var_ref "c"))
         ())
  in
  let optimized =
    Css.v
      [ Css.rule ~selector:(Css.Selector.class_ "k") [ some_zero_var_shadow ] ]
    |> Css.optimize |> Css.to_string ~minify:true
  in
  Alcotest.(check string)
    "an authored zero blur before a var colour survives optimization"
    ".k{box-shadow:0 3px 0 var(--c)}" optimized;
  (* CSS Backgrounds 3 sec. 6.2 puts the colour on either side of the length
     run, so a [var()] read into that run is a length until its fallback says
     otherwise. A fallback the colour reader takes whole says so: the reference
     ends the run and takes the colour slot, typed, where a length fallback, a
     bare reference or a reference another length follows all stay lengths. *)
  let typed_color_var name value =
    match (read_shadow (Cursor.of_string value) : shadow) with
    | Shadow
        {
          blur = None;
          spread = None;
          color =
            Some (Values.Var { name = "c"; fallback = Values.Fallback _; _ });
          _;
        } ->
        ()
    | _ -> Alcotest.failf "%s: %s does not end in a typed colour var" name value
  in
  typed_color_var "hex fallback" "2px 2px var(--c, #0004)";
  typed_color_var "alpha fallback" "2px 2px var(--c, rgb(1 2 3 / 25%))";
  typed_color_var "relative fallback"
    "2px 2px var(--c, oklab(from red l a b / 25%))";
  (match
     (read_shadow (Cursor.of_string "inset 2px 2px 4px var(--c, red)") : shadow)
   with
  | Inset
      (Body
         {
           blur = Some _;
           spread = None;
           color = Some (Values.Var { fallback = Values.Fallback _; _ });
           _;
         }) ->
      ()
  | _ -> Alcotest.fail "blur then a colour fallback under inset");
  let length_var name value =
    match (read_shadow (Cursor.of_string value) : shadow) with
    | Shadow { color = None; _ } -> ()
    | _ -> Alcotest.failf "%s: %s took a colour" name value
  in
  length_var "length fallback" "2px 2px var(--c, 3px)";
  length_var "bare reference" "2px 2px var(--c)";
  length_var "a length follows" "2px 2px var(--c, red) 1px";
  length_var "one offset before" "2px var(--c, red)";
  (* The reference prints where it was read, so the value round-trips. *)
  check_shadow "2px 2px var(--c, #0004)" ~expected:"2px 2px var(--c,#0004)";
  check_shadow "2px 2px var(--c, red) 1px" ~expected:"2px 2px var(--c,red) 1px"

let test_mask_border_mode () =
  check_mask_border_mode "alpha";
  check_mask_border_mode "luminance";
  neg_cursor read_mask_border_mode "match-source"

let test_clip_geometry_box () =
  check_clip_geometry_box "margin-box";
  check_clip_geometry_box "border-box";
  check_clip_geometry_box "view-box";
  neg_cursor read_clip_geometry_box "closest-side"

let test_clip_path_extent () =
  check_clip_path_extent "closest-side";
  check_clip_path_extent "farthest-side";
  check_clip_path_extent "10px";
  neg_cursor read_clip_path_extent "border-box"

let test_clip_path_fill_rule () =
  check_clip_path_fill_rule "nonzero";
  check_clip_path_fill_rule "evenodd";
  neg_cursor read_clip_path_fill_rule "winding"

let test_mask_box () =
  check_mask_box "border-box";
  check_mask_box "content-box";
  check_mask_box "padding-box";
  check_mask_box "fill-box";
  check_mask_box "inherit";
  (* Measured on Chrome 153: -webkit-mask-clip is WebKit's own property, not an
     alias of mask-clip, and takes neither fill-box nor no-clip. One layer it
     cannot spell costs the whole fallback. *)
  decl_optimizes_to ~into:"mask-clip:border-box,fill-box,no-clip"
    "mask-clip:border-box,fill-box,no-clip";
  decl_optimizes_to
    ~into:
      "-webkit-mask-clip:border-box,content-box;mask-clip:border-box,content-box"
    "mask-clip:border-box,content-box";
  neg_cursor read_mask_box "invalid-mask-box"

let test_webkit_mask_box () =
  check_webkit_mask_box "content";
  check_webkit_mask_box "padding";
  check_webkit_mask_box "border";
  check_webkit_mask_box "border-box";
  neg_cursor read_webkit_mask_box "fill-box"

let test_mask_composite () =
  check_mask_composite "add";
  check_mask_composite "subtract";
  check_mask_composite "intersect";
  check_mask_composite "exclude";
  check_mask_composite "inherit";
  (* The evergreen target adds WebKit's spelling of each operator, layer by
     layer, ahead of the standard one. *)
  decl_optimizes_to
    ~into:
      "-webkit-mask-composite:source-over,source-out,source-in;mask-composite:add,subtract,intersect"
    "mask-composite:add,subtract,intersect";
  neg_cursor read_mask_composite "invalid-composite"

let test_mask_mode () =
  check_mask_mode "alpha";
  check_mask_mode "luminance";
  check_mask_mode "match-source";
  check_mask_mode "inherit";
  neg_cursor read_mask_mode "invalid-mode"

let test_mask_type () =
  check_mask_type "alpha";
  check_mask_type "luminance";
  check_mask_type "inherit";
  neg_cursor read_mask_type "invalid-mask-type"

let test_webkit_mask_composite () =
  check_webkit_mask_composite "source-over";
  check_webkit_mask_composite "xor";
  check_webkit_mask_composite "source-in";
  check_webkit_mask_composite "source-out";
  check_webkit_mask_composite "inherit";
  neg_cursor read_webkit_mask_composite "invalid-composite"

let test_webkit_mask_source_type () =
  check_webkit_mask_source_type "alpha";
  check_webkit_mask_source_type "luminance";
  check_webkit_mask_source_type "auto";
  check_webkit_mask_source_type "inherit";
  neg_cursor read_webkit_mask_source_type "invalid-source-type"

let test_clip () =
  check_clip "auto";
  check_clip "rect(0px,10px,20px,30px)";
  check_clip "rect(0px 10px 20px 30px)" ~expected:"rect(0px,10px,20px,30px)";
  decl_optimizes ~prop:"clip" ~held:"rect(0px,10px,20px,30px)"
    ~into:"rect(0px,10px,20px,30px)" "rect(0px,10px,20px,30px)";
  neg_cursor read_clip "invalid-clip";
  (* CSS Masking 1 Appendix A gives the deprecated [clip] property's [rect()]
     exactly four [<top>, <right>, <bottom>, <left>]; a fifth value is invalid
     (CSS Syntax 3 sec. 8.2). [Cursor.call] did not require the reader to
     consume its whole sub-cursor, so it stopped after the fourth length and
     answered with a truncated [rect()] instead. *)
  neg_cursor read_clip "rect(0px,10px,20px,30px,40px)"

let test_clip_path () =
  check_clip_path "none";
  check_clip_path "url(clip.svg)";
  (* inset() with 1-4 values like margin/padding shorthand *)
  check_clip_path "inset(50%)";
  (* 1 value: all sides *)
  check_clip_path "inset(10px)";
  check_clip_path ~expected:"inset(10%20%)" "inset(10% 20%)";
  (* 2 values: top/bottom, left/right *)
  check_clip_path ~expected:"inset(10%20%30%)" "inset(10% 20% 30%)";
  (* 3 values: top, left/right, bottom *)
  check_clip_path "inset(0px 10px 20px 30px)";
  (* CSS Values L4 sec. 6: a zero in <length>/<length-percentage> position drops
     its unit under canonical minification; the fold is a node-changing rewrite
     (Length{Px,0} -> Length{None,0}) and so lives in normalize, not pp. The
     held (~held) form stays pp-faithful with the unit; only the canonical
     (~into) form drops it. Same fold applies recursively inside basic shapes
     (inset, polygon, rect, etc.) - all <length-percentage> arg positions, none
     of them inside a math context. *)
  decl_optimizes ~prop:"clip-path" ~held:"inset(0px 10px 20px 30px)"
    ~into:"inset(0 10px 20px 30px)" "inset(0px 10px 20px 30px)";
  (* 4 values *)
  check_clip_path "circle(50px)";
  check_clip_path "ellipse(25px 50px)";
  check_clip_path "polygon(0px 0px,100px 0px,50px 100px)";
  decl_optimizes ~prop:"clip-path" ~held:"polygon(0px 0px,100px 0px,50px 100px)"
    ~into:"polygon(0 0,100px 0,50px 100px)"
    "polygon(0px 0px,100px 0px,50px 100px)";
  neg_cursor read_clip_path "";
  neg_cursor read_clip_path "invalid"

let suite =
  ( "properties_background",
    [
      test_case "object-fit" `Quick test_object_fit;
      test_case "gradient direction" `Quick test_gradient_direction;
      test_case "gradient position" `Quick test_gradient_position;
      test_case "gradient stop" `Quick test_gradient_stop;
      test_case "color interpolation" `Quick test_color_interpolation;
      test_case "background_box" `Quick test_background_box;
      test_case "background shorthand" `Quick test_background_shorthand;
      test_case "background-attachment" `Quick test_background_attachment;
      test_case "background-repeat" `Quick test_background_repeat;
      test_case "background-size" `Quick test_background_size;
      test_case "background-image" `Quick test_background_image;
      test_case "url escaping" `Quick test_url_escaping;
      test_case "filter" `Quick test_filter;
      test_case "filter function" `Quick test_filter_function;
      test_case "clip" `Quick test_clip;
      test_case "clip_path" `Quick test_clip_path;
      test_case "background" `Quick test_background;
      test_case "text_shadow" `Quick test_text_shadow;
      test_case "mask_border_mode" `Quick test_mask_border_mode;
      test_case "clip_geometry_box" `Quick test_clip_geometry_box;
      test_case "clip_path_extent" `Quick test_clip_path_extent;
      test_case "clip_path_fill_rule" `Quick test_clip_path_fill_rule;
      test_case "background_shorthand" `Quick test_background_shorthand;
      test_case "blend_mode" `Quick test_blend_mode;
      test_case "background_attachment" `Quick test_background_attachment;
      test_case "background_repeat" `Quick test_background_repeat;
      test_case "background_size" `Quick test_background_size;
      test_case "gradient_direction" `Quick test_gradient_direction;
      test_case "gradient_stop" `Quick test_gradient_stop;
      test_case "hue_interpolation_method" `Quick test_hue_interpolation_method;
      test_case "conic_gradient_config" `Quick test_conic_gradient_config;
      test_case "radial_shape" `Quick test_radial_shape;
      test_case "radial_size" `Quick test_radial_size;
      test_case "radial_gradient_config" `Quick test_radial_gradient_config;
      test_case "background_image" `Quick test_background_image;
      test_case "background_position" `Quick test_background_position;
      test_case "object_fit" `Quick test_object_fit;
      test_case "shadow" `Quick test_shadow;
      test_case "shadow" `Quick test_shadow;
      test_case "mask_box" `Quick test_mask_box;
      test_case "webkit_mask_box" `Quick test_webkit_mask_box;
      test_case "mask_composite" `Quick test_mask_composite;
      test_case "mask_mode" `Quick test_mask_mode;
      test_case "mask_type" `Quick test_mask_type;
      test_case "webkit_mask_composite" `Quick test_webkit_mask_composite;
      test_case "webkit_mask_source_type" `Quick test_webkit_mask_source_type;
    ] )
