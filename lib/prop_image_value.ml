(* CSS Images 4: the <image> value (url(), image-set(), cross-fade() and the
   gradient functions), and image-orientation, image-rendering and
   image-resolution. *)

open Common
open Values
open Properties_intf
open Prop_common
open Prop_image_gradient

let pp_quoted_url quote ctx url =
  Pp.string ctx "url(";
  Pp.quoted_string ~quote ctx url;
  Pp.char ctx ')'

let pp_image_set_option ctx { source; resolution; mime_type } =
  (match source with Url u -> Pp.url ctx u | String s -> Pp.quoted ctx s);
  Option.iter
    (fun mime ->
      Pp.space ctx ();
      Pp.string ctx "type(";
      Pp.quoted ctx mime;
      Pp.char ctx ')')
    mime_type;
  Option.iter
    (fun res ->
      Pp.sp ctx ();
      Pp.string ctx res)
    resolution

let rec pp_background_image : background_image Pp.t =
 fun ctx (image : background_image) ->
  match image with
  | Url url -> Pp.url ctx url
  | Quoted (url, quote) ->
      if Pp.minified ctx then Pp.url ctx url else pp_quoted_url quote ctx url
  | Linear_gradient (dir, stops) ->
      pp_linear_gradient_named "linear-gradient" ctx (dir, stops)
  | Linear_gradient_var var_ref ->
      Pp.call "linear-gradient"
        (fun ctx v -> pp_var pp_gradient_stop ctx v)
        ctx var_ref
  | Repeating_linear_gradient (dir, stops) ->
      pp_linear_gradient_named "repeating-linear-gradient" ctx (dir, stops)
  | Radial_gradient (config, stops) ->
      pp_radial_gradient_named "radial-gradient" ctx (config, stops)
  | Radial_gradient_var var_ref ->
      Pp.call "radial-gradient"
        (fun ctx v -> pp_var pp_gradient_stop ctx v)
        ctx var_ref
  | Repeating_radial_gradient (config, stops) ->
      pp_radial_gradient_named "repeating-radial-gradient" ctx (config, stops)
  | Conic_gradient (config, stops) ->
      pp_conic_gradient_named "conic-gradient" ctx (config, stops)
  | Conic_gradient_var var_ref ->
      Pp.call "conic-gradient"
        (fun ctx v -> pp_var pp_gradient_stop ctx v)
        ctx var_ref
  | Repeating_conic_gradient (config, stops) ->
      pp_conic_gradient_named "repeating-conic-gradient" ctx (config, stops)
  | Webkit_linear_gradient (dir, stops) ->
      pp_webkit_linear_gradient_named "-webkit-linear-gradient" ctx (dir, stops)
  | Webkit_repeating_linear_gradient (dir, stops) ->
      pp_webkit_linear_gradient_named "-webkit-repeating-linear-gradient" ctx
        (dir, stops)
  | Webkit_radial_gradient (config, stops) ->
      pp_radial_gradient_named "-webkit-radial-gradient" ctx (config, stops)
  | Webkit_repeating_radial_gradient (config, stops) ->
      pp_radial_gradient_named "-webkit-repeating-radial-gradient" ctx
        (config, stops)
  | Moz_linear_gradient (dir, stops) ->
      pp_webkit_linear_gradient_named "-moz-linear-gradient" ctx (dir, stops)
  | Moz_repeating_linear_gradient (dir, stops) ->
      pp_webkit_linear_gradient_named "-moz-repeating-linear-gradient" ctx
        (dir, stops)
  | Moz_radial_gradient (config, stops) ->
      pp_radial_gradient_named "-moz-radial-gradient" ctx (config, stops)
  | Moz_repeating_radial_gradient (config, stops) ->
      pp_radial_gradient_named "-moz-repeating-radial-gradient" ctx
        (config, stops)
  | O_linear_gradient (dir, stops) ->
      pp_webkit_linear_gradient_named "-o-linear-gradient" ctx (dir, stops)
  | O_repeating_linear_gradient (dir, stops) ->
      pp_webkit_linear_gradient_named "-o-repeating-linear-gradient" ctx
        (dir, stops)
  | O_radial_gradient (config, stops) ->
      pp_radial_gradient_named "-o-radial-gradient" ctx (config, stops)
  | O_repeating_radial_gradient (config, stops) ->
      pp_radial_gradient_named "-o-repeating-radial-gradient" ctx (config, stops)
  | Image_set options ->
      Pp.call "image-set"
        (fun ctx os -> Pp.list ~sep:Pp.comma pp_image_set_option ctx os)
        ctx options
  | Webkit_image_set options ->
      Pp.call "-webkit-image-set"
        (fun ctx os -> Pp.list ~sep:Pp.comma pp_image_set_option ctx os)
        ctx options
  | Cross_fade options ->
      Pp.call "cross-fade"
        (fun ctx os -> Pp.list ~sep:Pp.comma pp_cross_fade_option ctx os)
        ctx options
  | Webkit_gradient gradient -> pp_webkit_gradient ctx gradient
  | Var v -> pp_var pp_background_image ctx v
  | List images -> Pp.list ~sep:Pp.comma pp_background_image ctx images
  | None -> Pp.string ctx "none"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

and pp_cross_fade_option ctx { image; percent } =
  pp_background_image ctx image;
  Option.iter
    (fun pct ->
      Pp.space ctx ();
      Values.pp_percentage ~always:true ctx pct)
    percent

let rec normalize_background_image ?(lossless = false) :
    background_image -> background_image =
 fun value ->
  let stops s =
    fold_double_position_stops
      (map_preserve (normalize_gradient_stop ~lossless) s)
  in
  match value with
  | Linear_gradient (d, s) ->
      let d = normalize_gradient_direction d in
      let s = stops s in
      let d, s =
        match drop_to_top_direction d s with
        | Option.Some (d, s) -> (d, s)
        | Option.None -> (d, s)
      in
      preserve_if_equal value (Linear_gradient (d, s))
  | Radial_gradient (c, s) ->
      preserve_if_equal value
        (Radial_gradient (normalize_radial_config c, stops s))
  | Conic_gradient (c, s) ->
      preserve_if_equal value
        (Conic_gradient (normalize_conic_config c, stops s))
  | Repeating_linear_gradient (d, s) ->
      preserve_if_equal value
        (Repeating_linear_gradient (normalize_gradient_direction d, stops s))
  | Repeating_radial_gradient (c, s) ->
      preserve_if_equal value
        (Repeating_radial_gradient (normalize_radial_config c, stops s))
  | Repeating_conic_gradient (c, s) ->
      preserve_if_equal value
        (Repeating_conic_gradient (normalize_conic_config c, stops s))
  | Webkit_linear_gradient (d, s) ->
      preserve_if_equal value
        (Webkit_linear_gradient (normalize_gradient_direction d, stops s))
  | Webkit_repeating_linear_gradient (d, s) ->
      preserve_if_equal value
        (Webkit_repeating_linear_gradient
           (normalize_gradient_direction d, stops s))
  | Webkit_radial_gradient (c, s) ->
      preserve_if_equal value
        (Webkit_radial_gradient (normalize_radial_config c, stops s))
  | Webkit_repeating_radial_gradient (c, s) ->
      preserve_if_equal value
        (Webkit_repeating_radial_gradient (normalize_radial_config c, stops s))
  | Moz_linear_gradient (d, s) ->
      preserve_if_equal value
        (Moz_linear_gradient (normalize_gradient_direction d, stops s))
  | Moz_repeating_linear_gradient (d, s) ->
      preserve_if_equal value
        (Moz_repeating_linear_gradient (normalize_gradient_direction d, stops s))
  | Moz_radial_gradient (c, s) ->
      preserve_if_equal value
        (Moz_radial_gradient (normalize_radial_config c, stops s))
  | Moz_repeating_radial_gradient (c, s) ->
      preserve_if_equal value
        (Moz_repeating_radial_gradient (normalize_radial_config c, stops s))
  | O_linear_gradient (d, s) ->
      preserve_if_equal value
        (O_linear_gradient (normalize_gradient_direction d, stops s))
  | O_repeating_linear_gradient (d, s) ->
      preserve_if_equal value
        (O_repeating_linear_gradient (normalize_gradient_direction d, stops s))
  | O_radial_gradient (c, s) ->
      preserve_if_equal value
        (O_radial_gradient (normalize_radial_config c, stops s))
  | O_repeating_radial_gradient (c, s) ->
      preserve_if_equal value
        (O_repeating_radial_gradient (normalize_radial_config c, stops s))
  | Webkit_gradient g ->
      preserve_if_equal value
        (Webkit_gradient (normalize_webkit_gradient ~lossless g))
  | Cross_fade opts ->
      let normalize_opt (o : cross_fade_option) =
        let image = normalize_background_image ~lossless o.image in
        if image == o.image then o else { o with image }
      in
      preserve_if_equal value (Cross_fade (map_preserve normalize_opt opts))
  | List imgs ->
      preserve_if_equal value
        (List (map_preserve (normalize_background_image ~lossless) imgs))
  | other -> other

let rec pp_image_orientation : image_orientation Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_image_orientation ctx v
  | None -> Pp.string ctx "none"
  | From_image -> Pp.string ctx "from-image"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_image_rendering : image_rendering Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_image_rendering ctx v
  | Auto -> Pp.string ctx "auto"
  | Smooth -> Pp.string ctx "smooth"
  | High_quality -> Pp.string ctx "high-quality"
  | Crisp_edges -> Pp.string ctx "crisp-edges"
  | Pixelated -> Pp.string ctx "pixelated"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_resolution ctx = function
  | Dpi f -> Pp.unit ctx f "dpi"
  | Dpcm f -> Pp.unit ctx f "dpcm"
  | Dppx f -> Pp.unit ctx f "dppx"
  | X f -> Pp.unit ctx f "x"

let rec pp_image_resolution : image_resolution Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_image_resolution ctx v
  | Resolution r -> pp_resolution ctx r
  | From_image -> Pp.string ctx "from-image"
  | From_image_resolution r ->
      Pp.string ctx "from-image";
      Pp.space ctx ();
      pp_resolution ctx r
  | Snap r ->
      pp_resolution ctx r;
      Pp.space ctx ();
      Pp.string ctx "snap"
  | From_image_snap -> Pp.string ctx "from-image snap"
  | From_image_snap_resolution r ->
      Pp.string ctx "from-image";
      Pp.space ctx ();
      pp_resolution ctx r;
      Pp.space ctx ();
      Pp.string ctx "snap"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec read_image_orientation (t : Cursor.t) : image_orientation =
  let keywords : (string * image_orientation) list =
    [
      ("none", None);
      ("from-image", From_image);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "image-orientation" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_image_orientation t) : image_orientation))
     t
    : image_orientation)

let rec read_image_rendering (t : Cursor.t) : image_rendering =
  let keywords : (string * image_rendering) list =
    [
      ("auto", Auto);
      ("smooth", Smooth);
      ("high-quality", High_quality);
      ("crisp-edges", Crisp_edges);
      ("pixelated", Pixelated);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "image-rendering" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_image_rendering t) : image_rendering))
     t
    : image_rendering)

let read_resolution t : resolution =
  match Cursor.dimension_opt t with
  | None -> Cursor.err_expected t "<resolution>"
  | Some (value, unit_) -> (
      if value < 0. then Cursor.err_invalid t "negative resolution";
      match String.lowercase_ascii unit_ with
      | "dpi" -> Dpi value
      | "dpcm" -> Dpcm value
      | "dppx" -> Dppx value
      | "x" -> (X value : resolution)
      | _ -> Cursor.err_expected t "<resolution>")

let image_resolution_of_parts (t : Cursor.t)
    ~(parts : [ `From_image | `Snap ] list) (resolution : resolution option) :
    image_resolution =
  let from_image = List.mem `From_image parts in
  let snap = List.mem `Snap parts in
  match (from_image, snap, resolution) with
  | false, false, None -> Cursor.err_invalid t "image-resolution"
  | false, false, Some r -> Resolution r
  | true, false, None -> From_image
  | true, false, Some r -> From_image_resolution r
  | false, true, None -> Cursor.err_invalid t "image-resolution"
  | false, true, Some r -> Snap r
  | true, true, None -> From_image_snap
  | true, true, Some r -> From_image_snap_resolution r

let image_resolution_has_dimension t =
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Dimension _; _ }) -> true
  | _ -> false

let read_image_resolution_from_image t parts resolution =
  if List.mem `From_image parts then Cursor.err_invalid t "duplicate from-image";
  if parts <> [] || Option.is_some resolution then
    Cursor.err_invalid t "from-image must precede image resolution";
  ignore (Cursor.ident t : string);
  (`Continue (`From_image :: parts, resolution)
    : [ `Continue of [ `From_image | `Snap ] list * resolution option
      | `Done of image_resolution ])

let read_image_resolution_snap t parts resolution =
  if List.mem `Snap parts then Cursor.err_invalid t "duplicate snap";
  if (not (List.mem `From_image parts)) && Option.is_none resolution then
    Cursor.err_invalid t "snap must follow image resolution";
  ignore (Cursor.ident t : string);
  (`Continue (`Snap :: parts, resolution)
    : [ `Continue of [ `From_image | `Snap ] list * resolution option
      | `Done of image_resolution ])

let read_image_resolution_dimension t (parts : [ `From_image | `Snap ] list)
    (resolution : resolution option) =
  match resolution with
  | None ->
      if List.mem `Snap parts then
        Cursor.err_invalid t "image resolution must precede snap";
      `Continue (parts, Some (read_resolution t))
  | Some _ -> Cursor.err_invalid t "duplicate resolution"

let read_image_resolution_step t parts resolution =
  Cursor.ws t;
  match Cursor.peek_keyword t with
  | Some "from-image" -> read_image_resolution_from_image t parts resolution
  | Some "snap" -> read_image_resolution_snap t parts resolution
  | _ when image_resolution_has_dimension t ->
      read_image_resolution_dimension t parts resolution
  | _ -> `Done (image_resolution_of_parts t ~parts resolution)

let read_image_resolution_value t =
  let rec loop parts resolution =
    match read_image_resolution_step t parts resolution with
    | `Continue (parts, resolution) -> loop parts resolution
    | `Done result -> result
  in
  loop [] (None : resolution option)

let rec read_image_resolution (t : Cursor.t) : image_resolution =
  Cursor.enum_or_var "image-resolution"
    [
      ("initial", (Initial : image_resolution));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_image_resolution t))
    ~default:read_image_resolution_value t

let read_bg_url_arg inner =
  Cursor.ws inner;
  match Cursor.string_with_quote_opt inner with
  | Some (url, quote) ->
      Cursor.ws inner;
      Cursor.expect_eof inner;
      (Quoted (url, quote) : background_image)
  | None ->
      let url = Cursor.consume_remaining_as_string ~trim:true inner in
      if url = "" then Cursor.err_expected inner "url argument" else Url url

let read_bg_url_call t terminated =
  if not terminated then Cursor.err_expected t "terminated url";
  Cursor.call "url" t read_bg_url_arg

let read_bg_url t : background_image =
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Url _; _ }) -> Url (Cursor.url t)
  | Some (Component.Func { node = { name; terminated; _ }; _ })
    when String.lowercase_ascii_preserve name = "url" ->
      read_bg_url_call t terminated
  | _ -> Cursor.err_expected t "url"

let read_image_set_option t : image_set_option =
  let source : image_set_source =
    Cursor.one_of
      [
        (fun t -> (Url (Cursor.url t) : image_set_source));
        (fun t -> String (Cursor.string t));
      ]
      t
  in
  (* [<resolution> || type(<string>)] in any order; both optional. Recurse,
     accepting whichever modifier hasn't been seen yet, until neither
     matches. *)
  let read_resolution_opt t : string option =
    match Cursor.dimension_opt t with
    | None -> None
    | Some (value, unit_) ->
        let buf = Buffer.create 8 in
        let pp_ctx = Pp.ctx ~minify:true buf in
        Pp.float pp_ctx value;
        Buffer.add_string buf unit_;
        Some (Buffer.contents buf)
  in
  let rec loop (mime : string option) (resolution : string option) =
    Cursor.ws t;
    match (mime, Cursor.function_call "type" Cursor.string t) with
    | None, Some s -> loop (Some s) resolution
    | _ -> (
        match (resolution, read_resolution_opt t) with
        | None, Some r -> loop mime (Some r)
        | _ -> (mime, resolution))
  in
  let mime, resolution = loop None None in
  if Option.is_none mime && Option.is_none resolution then
    Cursor.err_invalid t
      "image-set option requires a <resolution> or type(<string>)";
  { source; resolution; mime_type = mime }

let read_image_set_body t : background_image =
  Image_set (Cursor.list ~sep:Cursor.comma ~at_least:1 read_image_set_option t)

let webkit_image_set_of_set = function
  | Image_set options -> Webkit_image_set options
  | other -> other

let read_bg_image_standard_calls =
  [
    ( "linear-gradient",
      fun t -> Cursor.call "linear-gradient" t read_linear_gradient_body );
    ( "radial-gradient",
      fun t -> Cursor.call "radial-gradient" t read_radial_gradient_body );
    ( "conic-gradient",
      fun t -> Cursor.call "conic-gradient" t read_conic_gradient_body );
    ("repeating-linear-gradient", read_repeating_linear_gradient);
    ("repeating-radial-gradient", read_repeating_radial_gradient);
    ("repeating-conic-gradient", read_repeating_conic_gradient);
  ]

let webkit_bg_image_calls =
  [
    ( "-webkit-linear-gradient",
      fun t ->
        Cursor.call "-webkit-linear-gradient" t read_webkit_linear_gradient_body
    );
    ("-webkit-repeating-linear-gradient", read_webkit_repeating_linear_gradient);
    ( "-webkit-radial-gradient",
      fun t ->
        Cursor.call "-webkit-radial-gradient" t read_radial_gradient_body_stops
        |> webkit_radial_gradient_of_radial );
    ( "-webkit-repeating-radial-gradient",
      fun t ->
        Cursor.call "-webkit-repeating-radial-gradient" t
          read_radial_gradient_body_stops
        |> webkit_repeat_radial_of_radial );
  ]

let legacy_bg_image_calls =
  [
    ( "-moz-linear-gradient",
      fun t ->
        Cursor.call "-moz-linear-gradient" t read_webkit_linear_gradient_body
        |> moz_linear_gradient_of_webkit );
    ( "-moz-repeating-linear-gradient",
      fun t ->
        Cursor.call "-moz-repeating-linear-gradient" t
          read_webkit_linear_gradient_body
        |> moz_repeat_linear_of_webkit );
    ( "-moz-radial-gradient",
      fun t ->
        Cursor.call "-moz-radial-gradient" t read_radial_gradient_body_stops
        |> moz_radial_gradient_of_radial );
    ( "-moz-repeating-radial-gradient",
      fun t ->
        Cursor.call "-moz-repeating-radial-gradient" t
          read_radial_gradient_body_stops
        |> moz_repeat_radial_of_radial );
    ( "-o-linear-gradient",
      fun t ->
        Cursor.call "-o-linear-gradient" t read_webkit_linear_gradient_body
        |> o_linear_gradient_of_webkit );
    ( "-o-repeating-linear-gradient",
      fun t ->
        Cursor.call "-o-repeating-linear-gradient" t
          read_webkit_linear_gradient_body
        |> o_repeat_linear_of_webkit );
    ( "-o-radial-gradient",
      fun t ->
        Cursor.call "-o-radial-gradient" t read_radial_gradient_body_stops
        |> o_radial_gradient_of_radial );
    ( "-o-repeating-radial-gradient",
      fun t ->
        Cursor.call "-o-repeating-radial-gradient" t
          read_radial_gradient_body_stops
        |> o_repeat_radial_of_radial );
  ]

let read_bg_image_vendor_calls = webkit_bg_image_calls @ legacy_bg_image_calls

let rec read_bg_image t : background_image =
  (* Bare [url(foo)] is a single [Token.Url] component, not a [Func]; handle it
     explicitly before dispatching on the function/ident shape. *)
  Cursor.one_of
    [
      read_bg_url;
      (fun t ->
        Cursor.enum_or_calls "background-image"
          [
            ("none", (None : background_image));
            ("initial", Initial);
            ("inherit", Inherit);
            ("unset", Unset);
            ("revert", Revert);
            ("revert-layer", Revert_layer);
          ]
          ~calls:(read_bg_image_calls ()) t);
    ]
    t

and read_bg_image_misc_calls () =
  [
    ("image-set", fun t -> Cursor.call "image-set" t read_image_set_body);
    ( "-webkit-image-set",
      fun t ->
        Cursor.call "-webkit-image-set" t read_image_set_body
        |> webkit_image_set_of_set );
    ("cross-fade", fun t -> Cursor.call "cross-fade" t read_cross_fade_body);
    ( "-webkit-gradient",
      fun t -> Cursor.call "-webkit-gradient" t read_webkit_gradient_body );
    ("var", fun t -> Var (Values.read_var read_bg_image t));
  ]

and read_bg_image_calls () =
  read_bg_image_standard_calls @ read_bg_image_vendor_calls
  @ read_bg_image_misc_calls ()

and read_cross_fade_body t : background_image =
  (* Parse a non-empty comma-separated list where every comma must be followed
     by another option. Cursor.list is permissive about trailing separators
     (rollback on failure leaves the comma silently consumed), so the spec's
     [<cf-mixing-image>#] grammar is enforced manually. *)
  let first = read_cross_fade_option t in
  let rec loop acc =
    Cursor.ws t;
    if Cursor.peek_comma t then (
      Cursor.skip t;
      loop (read_cross_fade_option t :: acc))
    else List.rev acc
  in
  let opts = loop [ first ] in
  Cursor.ws t;
  if not (Cursor.is_done t) then
    Cursor.err_expected t "end of cross-fade() arguments";
  Cross_fade opts

and read_cross_fade_option t : cross_fade_option =
  Cursor.ws t;
  (* [<cf-mixing-image> = <percentage>? && <image>]. The two parts are
     order-independent in the spec; try percentage first, then image, and pick
     up a trailing percentage if the image came first. *)
  let pct =
    match Cursor.peek t with
    | Some (Component.Preserved { kind = Token.Percentage _; _ }) ->
        Some (Values.read_percentage t)
    | _ -> None
  in
  Cursor.ws t;
  let image = read_bg_image t in
  Cursor.ws t;
  let pct =
    match pct with
    | Some _ -> pct
    | None -> (
        match Cursor.peek t with
        | Some (Component.Preserved { kind = Token.Percentage _; _ }) ->
            Some (Values.read_percentage t)
        | _ -> None)
  in
  { image; percent = pct }

let read_background_image t : background_image =
  let first = read_bg_image t in
  Cursor.ws t;
  if Cursor.comma_opt t then
    let rest = Cursor.list ~sep:Cursor.comma ~at_least:1 read_bg_image t in
    List (first :: rest)
  else first

let read_background_images t : background_image list =
  Cursor.list ~sep:Cursor.comma read_background_image t

let minify_background_image : background_image -> background_image = function
  | Linear_gradient (dir, stops) ->
      Linear_gradient (dir, List.map minify_gradient_stop stops)
  | Repeating_linear_gradient (dir, stops) ->
      Repeating_linear_gradient (dir, List.map minify_gradient_stop stops)
  | Webkit_linear_gradient (dir, stops) ->
      Webkit_linear_gradient (dir, List.map minify_gradient_stop stops)
  | Webkit_repeating_linear_gradient (dir, stops) ->
      Webkit_repeating_linear_gradient (dir, List.map minify_gradient_stop stops)
  | Moz_linear_gradient (dir, stops) ->
      Moz_linear_gradient (dir, List.map minify_gradient_stop stops)
  | Moz_repeating_linear_gradient (dir, stops) ->
      Moz_repeating_linear_gradient (dir, List.map minify_gradient_stop stops)
  | O_linear_gradient (dir, stops) ->
      O_linear_gradient (dir, List.map minify_gradient_stop stops)
  | O_repeating_linear_gradient (dir, stops) ->
      O_repeating_linear_gradient (dir, List.map minify_gradient_stop stops)
  | Radial_gradient (config, stops) ->
      Radial_gradient (config, List.map minify_gradient_stop stops)
  | Repeating_radial_gradient (config, stops) ->
      Repeating_radial_gradient (config, List.map minify_gradient_stop stops)
  | Webkit_radial_gradient (config, stops) ->
      Webkit_radial_gradient (config, List.map minify_gradient_stop stops)
  | Webkit_repeating_radial_gradient (config, stops) ->
      Webkit_repeating_radial_gradient
        (config, List.map minify_gradient_stop stops)
  | Moz_radial_gradient (config, stops) ->
      Moz_radial_gradient (config, List.map minify_gradient_stop stops)
  | Moz_repeating_radial_gradient (config, stops) ->
      Moz_repeating_radial_gradient (config, List.map minify_gradient_stop stops)
  | O_radial_gradient (config, stops) ->
      O_radial_gradient (config, List.map minify_gradient_stop stops)
  | O_repeating_radial_gradient (config, stops) ->
      O_repeating_radial_gradient (config, List.map minify_gradient_stop stops)
  | Conic_gradient (config, stops) ->
      Conic_gradient
        ( minify_conic_gradient_config config,
          List.map minify_gradient_stop stops )
  | Repeating_conic_gradient (config, stops) ->
      Repeating_conic_gradient
        ( minify_conic_gradient_config config,
          List.map minify_gradient_stop stops )
  | Webkit_gradient (Webkit_gradient.Linear ({ stops; _ } as gradient)) ->
      Webkit_gradient
        (Webkit_gradient.Linear
           { gradient with stops = List.map minify_webkit_gradient_stop stops })
  | Webkit_gradient (Webkit_gradient.Radial ({ stops; _ } as gradient)) ->
      Webkit_gradient
        (Webkit_gradient.Radial
           { gradient with stops = List.map minify_webkit_gradient_stop stops })
  | img -> img
