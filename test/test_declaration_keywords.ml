(** Declaration tests: the keyword and name domains of each property. *)

open Alcotest
open Cascade
open Css.Declaration
open Css_test_helpers
open Declaration_helpers

let list_properties () =
  (* Box shadow *)
  check_declaration ~expected:"box-shadow:none" "box-shadow: none";
  (* CSS Backgrounds 3 sec. 6.1: <shadow> is [<color>? && [<length>{2,4}] &&
     inset?], so the two offsets alone are a whole shadow whatever they hold,
     and one length is not. *)
  check_declaration ~expected:"box-shadow:0 0" "box-shadow: 0 0";
  check_declaration ~expected:"box-shadow:inset 0 0" "box-shadow: inset 0 0";
  check_declaration ~expected:"text-shadow:0 0" "text-shadow: 0 0";
  neg_cursor read_declaration "box-shadow: 0";
  neg_cursor read_declaration "text-shadow: 1px";
  neg_cursor read_declaration "box-shadow: inset inset 0 0 1px";
  (* Sec. 6.1 and CSS Text Decoration 4 sec. 6.2 both spell the property [none |
     <shadow>#], so [none] is the whole value and never one item of the list. *)
  neg_cursor read_declaration "box-shadow: none, none";
  neg_cursor read_declaration "text-shadow: none, none";
  neg_cursor read_declaration "box-shadow: 1px 1px red, none";
  neg_cursor read_declaration "text-shadow: none, 1px 1px red";
  check_declaration ~expected:"text-shadow:none" "text-shadow: none";
  check_declaration ~expected:"box-shadow:1px 1px red" "box-shadow: 1px 1px red";
  check_declaration ~expected:"text-shadow:1px 1px red,2px 2px blue"
    "text-shadow: 1px 1px red, 2px 2px blue";
  (* Sec. 6.2 writes the run [<length>{2} [ <length [0,inf]> <length>? ]?]: a
     plain length in every slot, and a floor on the blur alone. Chrome 153
     agrees on each of these. *)
  neg_cursor read_declaration "box-shadow: 10% 20%";
  neg_cursor read_declaration "box-shadow: 20px 10%";
  neg_cursor read_declaration "box-shadow: auto 10%";
  neg_cursor read_declaration "text-shadow: 10px 20%";
  neg_cursor read_declaration "text-shadow: auto 10px";
  neg_cursor read_declaration "box-shadow: 10px 20px -5px";
  neg_cursor read_declaration "text-shadow: 10px 20px -5px";
  check_declaration ~expected:"box-shadow:10px 20px 5px -3px"
    "box-shadow: 10px 20px 5px -3px";
  (* A math function holds no value to compare, so the blur takes one and keeps
     it: unwrapping [calc(-5px)] would emit the literal the reader refuses. *)
  check_declaration ~expected:"box-shadow:10px 20px calc(-5px)"
    ~optimized:"box-shadow:10px 20px calc(-5px)"
    "box-shadow: 10px 20px calc(-5px)";
  check_declaration ~expected:"text-shadow:10px 20px calc(-5px)"
    ~optimized:"text-shadow:10px 20px calc(-5px)"
    "text-shadow: 10px 20px calc(-5px)";
  check_declaration ~expected:"box-shadow:10px 20px calc(2px + 3px)"
    ~optimized:"box-shadow:10px 20px 5px"
    "box-shadow: 10px 20px calc(2px + 3px)";

  (* CSS Transitions 1 sec. 3: the shorthand takes a
     <single-transition-property>, and none is one of them, so it is the whole
     value only when the item ends there. The optimizer contracts a none
     property beside a duration and a timing function into this spelling. *)
  check_declaration ~expected:"transition:none" "transition: none";
  check_declaration ~expected:"transition:none 1s" "transition: none 1s";
  check_declaration ~expected:"transition:none 1s steps(2,end)"
    "transition: none 1s steps(2, end)";
  check_declaration ~expected:"transition:none,opacity 1s"
    "transition: none, opacity 1s";

  (* CSS Text Decoration 4 sec. 2.5: the shorthand is a || of its longhands and
     [none] is a <text-decoration-line>, so it is the whole value only when
     nothing follows it. The optimizer contracts a none line beside a style and
     a colour into exactly this spelling. *)
  check_declaration ~expected:"text-decoration:none" "text-decoration: none";
  check_declaration ~expected:"text-decoration:none dotted"
    "text-decoration: none dotted";
  check_declaration ~expected:"text-decoration:none dotted #12345680"
    "text-decoration: none dotted #12345680";
  check_declaration ~expected:"text-decoration:underline dotted red"
    "text-decoration: underline dotted red";

  (* CSS Align 3 sec. 5.2: [place-items] is [<'align-items'>
     <'justify-items'>?], so it takes every <self-position> keyword align-items
     does, and a lone value sets both axes. The optimizer contracts
     [align-items: flex-start; justify-items: baseline] into this spelling. *)
  check_declaration ~expected:"place-items:flex-start baseline"
    "place-items: flex-start baseline";
  check_declaration ~expected:"place-items:self-start end"
    "place-items: self-start end";
  check_declaration ~expected:"place-items:start baseline"
    "place-items: start baseline";

  (* CSS Color 5 sec. 5.1: [color(from <origin> srgb r g b)] is the origin's own
     sRGB channels, so it folds to the origin whether that was written as a hex
     or as a [color()] the same conversion reaches. *)
  check_declaration ~expected:"color:rgb(179 128 77)"
    "color: color(from #b3804d srgb r g b)";
  check_declaration ~expected:"color:rgb(179 128 77)"
    "color: color(from color(srgb .7 .5 .3) srgb r g b)";
  check_declaration ~expected:"color:rgb(179 128 77/.4)"
    "color: color(from color(srgb .7 .5 .3/40%) srgb r g b)";
  check_declaration ~expected:"color:rgb(179 128 77)"
    "color: color(from color(from color(srgb .7 .5 .3) srgb r g b) srgb r g b)";

  (* CSS Color 5 sec. 4.1: the lab family's own keywords in order are the origin
     in that space, with the alpha the call names or the origin's when the slot
     is omitted. The conversion is the browser's, so it is a value fold the
     optimizer makes and the printer keeps the spelling. *)
  check_declaration ~expected:"color:oklab(from #0000001a l a b/.2)"
    ~optimized:"color:#0003" "color: oklab(from #0000001a l a b / .2)";
  check_declaration ~expected:"color:oklab(from rgb(0 0 0/.1) l a b/.2)"
    ~optimized:"color:#0003" "color: oklab(from rgb(0 0 0 / .1) l a b / 20%)";
  check_declaration ~expected:"color:oklch(from #123456 l c h)"
    ~optimized:"color:#123456" "color: oklch(from #123456 l c h)";
  check_declaration ~expected:"color:lab(from #0000001a l a b)"
    ~optimized:"color:#0000001a" "color: lab(from #0000001a l a b)";
  check_declaration ~expected:"color:oklab(from #0000001a calc(l + .5) a b)"
    ~optimized:"color:oklab(from #0000001a calc(l + .5) a b)"
    "color: oklab(from #0000001a calc(l + .5) a b)";

  (* CSS Color 4 sec. 9.4: an out-of-range Oklab lightness clamps rather than
     invalidating the colour, and 0% to 100% is the same 0 to 1 a bare number
     names, so both spellings clamp alike. *)
  (* pp holds the percentage spelling; each pair asserts that the out-of-range
     value reads as the in-range one beside it. *)
  check_declaration ~expected:"color:oklab(100%0 0)" "color: oklab(100% 0 0)";
  check_declaration ~expected:"color:oklab(100%0 0)" "color: oklab(200% 0 0)";
  check_declaration ~expected:"color:oklab(0%0 0)" "color: oklab(0% 0 0)";
  check_declaration ~expected:"color:oklab(0%0 0)" "color: oklab(-200% 0 0)";
  check_declaration ~expected:"color:oklch(100%0 0)" "color: oklch(100% 0 0)";
  check_declaration ~expected:"color:oklch(100%0 0)" "color: oklch(200% 0 0)";
  check_declaration ~expected:"color:oklch(0%0 0)" "color: oklch(0% 0 0)";
  check_declaration ~expected:"color:oklch(0%0 0)" "color: oklch(-200% 0 0)";

  (* [center] minifies to the same pair of percentages a written-out position
     does, and a percentage delimits itself, so neither spelling keeps the space
     between them. *)
  check_declaration
    ~expected:
      "background:-webkit-gradient(radial,50%50%,0,50%50%,100,from(blue),to(yellow))"
    "background: -webkit-gradient(radial, center center, 0, center center, \
     100, from(blue), to(yellow))";
  check_declaration
    ~expected:
      "background:-webkit-gradient(radial,50%50%,0,50%50%,100,from(#00f),to(#ff0))"
    "background: -webkit-gradient(radial, 50% 50%, 0, 50% 50%, 100, \
     from(#00f), to(#ff0))";
  (* A [-webkit-gradient] stop position is a number or a percentage, and the
     minified spelling is the number, so both read back. *)
  check_declaration
    ~expected:
      "background:-webkit-gradient(linear,0 0,0 \
       100%,from(#00f),color-stop(.5,red),to(#ff0))"
    "background: -webkit-gradient(linear, 0 0, 0 100%, from(#00f), \
     color-stop(.5, red), to(#ff0))";
  check_declaration
    ~expected:
      "background:-webkit-gradient(linear,0 0,0 \
       100%,from(#00f),color-stop(.5,red),to(#ff0))"
    "background: -webkit-gradient(linear, 0 0, 0 100%, from(#00f), \
     color-stop(50%, red), to(#ff0))";

  (* CSS Transforms 2 sec. 5: minified [rotate] leads with the angle, and a
     following axis number that starts with a sign needs no separator. The first
     one does: the angle ends in its unit, so [0deg-1] is the one dimension
     [0deg-1]. *)
  check_declaration ~expected:"rotate:0deg -1 0 0" "rotate: -1 0 0 0deg";
  check_declaration ~expected:"rotate:45deg 1 0-1" "rotate: 1 0 -1 45deg";
  check_declaration ~expected:"rotate:x 45deg" "rotate: 1 0 0 45deg";

  (* CSS Syntax 3 sec. 4.3.6 ends a url token at EOF as it does at [)]: the
     missing closer is a parse error and the token still reads. *)
  check_declaration ~expected:"background-image:url()" "background-image:url(";
  check_declaration ~expected:"background-image:url(foo)"
    "background-image:url(foo";
  check_declaration ~expected:"background-image:url()" "background-image:url()";
  (* CSS Backgrounds 3 sec. 6.1 and CSS Text Decoration 3 sec. 5 both build a
     shadow with &&, so the length run is contiguous and neither the colour nor
     inset may sit inside it. text-shadow takes three lengths, not four, and one
     colour. *)
  check_declaration ~expected:"text-shadow:1px 1px red"
    "text-shadow: red 1px 1px";
  check_declaration ~expected:"text-shadow:1px 1px 2px"
    "text-shadow: 1px 1px 2px";
  neg_cursor read_declaration "text-shadow: 1px red 1px";
  neg_cursor read_declaration "text-shadow: 1px 1px 1px 1px";
  neg_cursor read_declaration "text-shadow: 1px 1px red 1px";
  neg_cursor read_declaration "text-shadow: red 1px 1px blue";
  check_declaration ~expected:"box-shadow:inset 1px 1px"
    "box-shadow: 1px 1px inset";
  neg_cursor read_declaration "box-shadow: 1px inset 1px";
  neg_cursor read_declaration "box-shadow: 1px red 1px";
  check_declaration ~expected:"box-shadow:0 1px 3px rgb(0 0 0/.12)"
    ~optimized:"box-shadow:0 1px 3px #0000001f"
    "box-shadow: 0 1px 3px rgba(0,0,0,0.12)";
  (* Faithful minify keeps an author-explicit zero spread (pp serializes the
     [Some Zero] node); optimize drops it as redundant since spread defaults to
     0, and also folds the colour to hex. *)
  check_declaration ~expected:"box-shadow:0 1px 3px 0 rgb(0 0 0/10%)"
    ~optimized:"box-shadow:0 1px 3px #0000001a"
    "box-shadow: 0 1px 3px 0 rgb(0 0 0 / 10%)";
  check_declaration ~expected:"box-shadow:0 1px 3px 0"
    ~optimized:"box-shadow:0 1px 3px" "box-shadow: 0 1px 3px 0";
  (* A zero blur is positional - it cannot be dropped while a non-zero spread
     follows, or the spread would rebind as the blur. Held and canonical
     match. *)
  check_declaration ~expected:"box-shadow:0 1px 0 5px" "box-shadow: 0 1px 0 5px";
  check_declaration
    ~expected:"box-shadow:0 1px 3px rgb(0 0 0/.12),0 1px 2px rgb(0 0 0/.24)"
    ~optimized:"box-shadow:0 1px 3px #0000001f,0 1px 2px #0000003d"
    "box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24)";
  check_declaration ~expected:"box-shadow:inset 0 2px 4px rgb(0 0 0/.06)"
    ~optimized:"box-shadow:inset 0 2px 4px #0000000f"
    "box-shadow: inset 0 2px 4px rgba(0,0,0,0.06)";

  (* Text shadow *)
  check_declaration ~expected:"text-shadow:none" "text-shadow: none";
  check_declaration ~expected:"text-shadow:1px 1px 2px black"
    ~optimized:"text-shadow:1px 1px 2px #000" "text-shadow: 1px 1px 2px black";
  check_declaration ~expected:"text-shadow:0 0 10px blue,0 0 20px red"
    ~optimized:"text-shadow:0 0 10px #00f,0 0 20px red"
    "text-shadow: 0 0 10px blue, 0 0 20px red";
  (* CSS Text Decoration 4 (ED) sec. 4 reads a text shadow as a [<shadow>] "as
     for box-shadow", and CSS Backgrounds 3 (ED) sec. 6.1 says "Omitted lengths
     are 0". The blur is the last length here, so a zero blur is the spelled-out
     form of leaving it off, and dropping it is a node change. *)
  check_declaration ~expected:"text-shadow:1px 1px 0"
    ~optimized:"text-shadow:1px 1px" "text-shadow: 1px 1px 0";
  check_declaration ~expected:"text-shadow:1px 1px 0 red"
    ~optimized:"text-shadow:1px 1px red" "text-shadow: 1px 1px 0 red";
  check_declaration ~expected:"text-shadow:1px 1px 2px"
    ~optimized:"text-shadow:1px 1px 2px" "text-shadow: 1px 1px 2px";

  (* Background image *)
  check_declaration ~expected:"background-image:none" "background-image: none";
  check_declaration ~expected:"background-image:url(image.png)"
    "background-image: url(image.png)";
  check_declaration
    ~expected:"background-image:linear-gradient(to right,red,blue)"
    "background-image: linear-gradient(to right, red, blue)";
  decl_optimizes ~prop:"background-image"
    ~into:"linear-gradient(90deg,red,#00f)"
    "linear-gradient(to right, red, blue)";
  check_declaration ~expected:"background-image:url(a.png),url(b.png)"
    "background-image: url(a.png), url(b.png)";

  (* Transition *)
  check_declaration ~expected:"transition:none" "transition: none";
  (* CSS Transitions 1 (ED) sec. 2.5 builds a [<single-transition>] out of
     components that each fall back to their longhand initial when left out:
     [0s] for the duration (sec. 2.2), [ease] for the easing (sec. 2.3), [0s]
     for the delay (sec. 2.4). Spelling an initial out therefore names the value
     the shorter form already names, and swapping the two is a node change, so
     pp prints what it parsed and the optimizer folds. *)
  check_declaration ~expected:"transition:all .3s ease"
    ~optimized:"transition:all .3s" "transition: all 0.3s ease";
  check_declaration ~expected:"transition:all 1s ease 0s"
    ~optimized:"transition:all 1s" "transition: all 1s ease 0s";
  check_declaration ~expected:"transition:opacity 1s normal"
    ~optimized:"transition:opacity 1s" "transition: opacity 1s normal";
  check_declaration ~expected:"transition:opacity 0s 0s"
    ~optimized:"transition:opacity" "transition: opacity 0s 0s";
  (* sec. 2.5: "the first value that can be parsed as a time is assigned to the
     transition-duration, and the second value that can be parsed as a time is
     assigned to transition-delay". A delay is only reachable behind a duration,
     so a zero duration that carries a non-zero delay has to stay written out:
     dropping it hands the delay to the duration slot and starts the transition
     immediately instead of two seconds late. *)
  check_declaration ~expected:"transition:opacity 0s 2s"
    ~optimized:"transition:opacity 0s 2s" "transition: opacity 0s 2s";
  check_declaration ~expected:"transition:opacity 0s linear 2s"
    ~optimized:"transition:opacity 0s linear 2s"
    "transition: opacity 0s linear 2s";
  check_declaration ~expected:"transition:all .3s linear"
    "transition: all .3s linear";
  check_declaration ~expected:"transition:opacity 1s ease-in .5s"
    "transition: opacity 1s ease-in .5s";
  check_declaration ~expected:"transition:opacity .3s,transform .3s"
    "transition: opacity 0.3s, transform 0.3s";
  check_declaration ~expected:"transition:all .5s"
    ~optimized:"transition:all .5s" "transition: all 500ms";

  (* Animation *)
  check_declaration ~expected:"animation:none" "animation: none";
  check_declaration ~expected:"animation:spin 1s linear infinite"
    "animation: spin 1s linear infinite";
  check_declaration ~expected:"animation:slide .5s ease-out"
    "animation: slide 0.5s ease-out";
  check_declaration ~expected:"animation:spin .5s"
    ~optimized:"animation:spin .5s" "animation: spin 500ms"

let animation_infinite_name () =
  (* CSS Animations 1 section 3.10 assigns a keyword to another property only
     when that property has not already been set. Section 3 makes keyframe names
     case-sensitive, even when their spelling is also a keyword. *)
  let open Css.Properties in
  let check_slots input expected_name expected_count =
    let cursor = Cursor.of_string input in
    let value = read_animation cursor in
    Alcotest.(check bool) "consumes animation" true (Cursor.is_done cursor);
    (match value with
    | Shorthand { name = Some (Ambiguous name | Name name); iteration_count; _ }
      ->
        Alcotest.(check string) "animation name" expected_name name;
        let shown c =
          Css.Pp.to_string ~minify:true pp_animation_iteration_count c
        in
        Alcotest.(check (option string))
          "iteration count"
          (Some (shown expected_count))
          (Option.map shown iteration_count)
    | _ -> Alcotest.failf "missing animation name in %s" input);
    value
  in
  List.iter
    (fun (input, name, count) ->
      let value = check_slots input name count in
      List.iter
        (fun minify ->
          let printed = Css.Pp.to_string ~minify pp_animation value in
          ignore (check_slots printed name count))
        [ false; true ])
    [
      ("infinite infinite", "infinite", Infinite);
      ("infinite INFINITE", "INFINITE", Infinite);
      ("INFINITE infinite", "infinite", Infinite);
      ("2 infinite", "infinite", Count (Num 2.));
      ("2 InFiNiTe", "InFiNiTe", Count (Num 2.));
    ];
  List.iter
    (fun value -> none_cursor read_declaration ("animation:" ^ value))
    [
      "2 3";
      "infinite 2";
      "infinite infinite infinite";
      "spin infinite infinite";
    ]

let animation_keyword_names () =
  (* CSS Animations 1 sections 3 and 3.10: names are case-sensitive and
     colliding keywords fill the other shorthand slots before the name. *)
  let open Css.Properties in
  let read input =
    let cursor = Cursor.of_string input in
    match read_animation cursor with
    | Shorthand value when Cursor.is_done cursor -> value
    | _ -> Alcotest.failf "expected one animation: %s" input
  in
  let check_slots expected name input =
    let actual = read input in
    (match actual.name with
    | Some (Name value | Ambiguous value) ->
        Alcotest.(check string) input name value
    | _ -> Alcotest.failf "missing name in %s" input);
    (* The slots read the same when the value they print does, and the printed
       form says which slot differs when they do not. *)
    let without_name v =
      Css.Pp.to_string ~minify:true pp_animation
        (Shorthand { v with name = Option.None })
    in
    Alcotest.(check string)
      (input ^ " keeps non-name slots")
      (without_name expected) (without_name actual)
  in
  let check_print expected name value =
    List.iter
      (fun minify ->
        check_slots expected name (Css.Pp.to_string ~minify pp_animation value))
      [ false; true ]
  in
  List.iter
    (fun (prefix, names) ->
      let expected = read (prefix ^ " keyframe") in
      List.iter
        (fun name ->
          List.iter
            (fun name ->
              let input = prefix ^ " " ^ name in
              check_slots expected name input;
              check_print expected name (Shorthand (read input));
              check_print expected name
                (animation_shorthand ~name ?duration:expected.duration
                   ?timing_function:expected.timing_function
                   ?delay:expected.delay
                   ?iteration_count:expected.iteration_count
                   ?direction:expected.direction ?fill_mode:expected.fill_mode
                   ?play_state:expected.play_state ?timeline:expected.timeline
                   ()))
            [ name; String.uppercase_ascii name ])
        names)
    [
      ( "linear",
        [
          "ease";
          "linear";
          "ease-in";
          "ease-out";
          "ease-in-out";
          "step-start";
          "step-end";
        ] );
      ("reverse", [ "normal"; "reverse"; "alternate"; "alternate-reverse" ]);
      ("forwards", [ "forwards"; "backwards"; "both" ]);
      ("paused", [ "running"; "paused" ]);
      ("2", [ "infinite" ]);
    ];
  let expected = read "keyframe" in
  List.iter
    (fun name -> check_print expected name (animation_shorthand ~name ()))
    [ "EASE"; "LINEAR"; "NORMAL"; "BOTH"; "PAUSED"; "INFINITE" ]

let list_style_custom_names () =
  (* CSS Lists 3 sections 3.4 and 3.6 allow custom counter-style names,
     including names that collide with an already-filled position slot. The
     referenced counter style need not be defined in this stylesheet. *)
  (* CSS Values 4 sec. 4.2 reserves [default] from the <custom-ident> arm in
     every ASCII case permutation, leaving the string spelling. *)
  List.iter
    (fun name -> check_declaration ~roundtrip:true ("list-style-type:" ^ name))
    [
      "footsteps"; "FootSteps"; "inside"; "OUTSIDE"; "--markers"; "\"default\"";
    ];
  List.iter
    (fun (value, expected) ->
      check_declaration ~roundtrip:true ~expected:("list-style:" ^ expected)
        ~optimized:("list-style:" ^ expected) ("list-style:" ^ value))
    [
      ("FootSteps inside", "FootSteps inside");
      ("inside outside", "inside outside");
      ("inside OUTSIDE", "inside OUTSIDE");
      ("outside inside", "outside inside");
      ("outside OUTSIDE", "outside OUTSIDE");
      ("inside inside", "inside inside");
    ];
  List.iter
    (none_cursor read_declaration)
    [
      "list-style-type:default";
      "list-style-type:DEFAULT";
      "list-style-type:FootSteps Other";
      "list-style:inside outside outside";
      "list-style:inside FootSteps Other";
    ]

let auto_is_not_a_color () =
  (* CSS Color 4 section 4.1 excludes auto from <color>. SVG 2 section 13.2 uses
     <color> both as paint and as a paint-server fallback. *)
  List.iter
    (fun property ->
      List.iter
        (fun value -> none_cursor read_declaration (property ^ ":" ^ value))
        [
          "auto";
          "AUTO";
          "color-mix(in srgb,auto,red)";
          "light-dark(auto,red)";
          "rgb(from auto r g b)";
        ])
    [
      "color";
      "background-color";
      "border-color";
      "text-decoration-color";
      "fill";
      "stroke";
      "stop-color";
    ];
  List.iter
    (fun property ->
      none_cursor read_declaration (property ^ ":url(#paint) auto");
      check_declaration ~roundtrip:true (property ^ ":none");
      check_declaration ~roundtrip:true (property ^ ":context-fill");
      check_declaration ~roundtrip:true (property ^ ":url(#paint)red"))
    [ "fill"; "stroke" ];
  (* CSS UI 4 gives these properties an explicit auto alternative. *)
  List.iter
    (fun property ->
      check_declaration ~roundtrip:true (property ^ ":auto");
      check_declaration ~roundtrip:true (property ^ ":red");
      check_declaration ~roundtrip:true (property ^ ":var(--color,auto)"))
    [ "accent-color"; "caret-color"; "outline-color" ];
  (* A var() fallback is a token stream; validity waits for substitution. *)
  check_declaration ~roundtrip:true "color:var(--color,auto)";
  check_declaration ~roundtrip:true "caret:auto";
  check_declaration ~roundtrip:true "scrollbar-color:auto"

let outline_offset_length_only () =
  (* CSS UI 4 section 3.5 takes <length>, with no percentage basis, and
     explicitly permits negative offsets. Section 2.1 admits CSS-wide values. *)
  List.iter
    (fun value -> check_declaration ~roundtrip:true ("outline-offset:" ^ value))
    [
      "0";
      "2px";
      "-2px";
      "-1em";
      "calc(1em - 2px)";
      "min(-1em,2px)";
      "inherit";
      "initial";
      "unset";
      "revert";
      "revert-layer";
      "var(--offset,10%)";
    ];
  List.iter
    (fun value -> none_cursor read_declaration ("outline-offset:" ^ value))
    [
      "10%";
      "0%";
      "-10%";
      "calc(1% + 2px)";
      "min(1%,2px)";
      "max(1px,2%)";
      "clamp(0px,10%,2px)";
      "auto";
      "min-content";
      "fit-content(2px)";
      "1s";
      "45deg";
    ]

let line_height_step_length_only () =
  (* CSS Rhythmic Sizing 1 section 3 takes <length [0,infinity]> with no
     percentage basis; section 1.2 also admits CSS-wide keywords. *)
  List.iter
    (fun value ->
      check_declaration ~roundtrip:true ("line-height-step:" ^ value))
    [
      "0";
      "4px";
      "1em";
      "calc(1em - 2px)";
      "min(1em,2px)";
      "inherit";
      "initial";
      "unset";
      "revert";
      "revert-layer";
      "var(--step,10%)";
    ];
  List.iter
    (fun value -> none_cursor read_declaration ("line-height-step:" ^ value))
    [
      "10%";
      "0%";
      "calc(1% + 2px)";
      "min(1%,2px)";
      "max(1px,2%)";
      "clamp(0px,10%,2px)";
      "-4px";
      "auto";
      "normal";
      "none";
      "min-content";
      "fit-content(2px)";
      "1s";
      "45deg";
    ]

let gap_normal () =
  (* CSS Box Alignment 3 sections 8.1 and 8.2 admit normal in both gap longhands
     and either shorthand slot. Its used value depends on layout, so normal
     cannot be replaced with zero without a layout context. *)
  List.iter
    (fun property ->
      List.iter
        (fun value ->
          let css = property ^ ":" ^ value in
          check_declaration ~roundtrip:true ~optimized:css css)
        [ "normal"; "10%"; "2px" ];
      check_declaration ~roundtrip:true (property ^ ":var(--gap,normal)");
      check_declaration ~roundtrip:true ~expected:(property ^ ":normal")
        (property ^ ":NORMAL");
      List.iter
        (fun value -> none_cursor read_declaration (property ^ ":" ^ value))
        [ "normal normal"; "normal 2px"; "auto"; "-2px" ])
    [ "row-gap"; "column-gap" ];
  List.iter
    (fun value ->
      let css = "gap:" ^ value in
      check_declaration ~roundtrip:true ~optimized:css css)
    [ "normal"; "normal 2px"; "2px normal"; "normal 10%" ];
  check_declaration ~roundtrip:true ~expected:"gap:normal"
    ~optimized:"gap:normal" "gap:normal normal";
  List.iter
    (fun value -> none_cursor read_declaration ("gap:" ^ value))
    [ "normal inherit"; "inherit normal"; "normal normal normal" ]

let animation_auto_duration () =
  (* CSS Animations 2 sections 4.1 and 4.12: auto is a duration, not zero
     seconds on every timeline. CSS Transitions 1 sections 2.2 and 2.4 and CSS
     Animations 2 section 4.6 restrict durations/delays to time values. *)
  List.iter
    (fun (input, expected) ->
      let property value = String.concat "" [ "animation-duration:"; value ] in
      check_declaration ~roundtrip:true ~expected:(property expected)
        ~optimized:(property expected) (property input))
    [
      ("auto", "auto");
      ("AUTO", "auto");
      ("auto,2s,auto", "auto,2s,auto");
      ("var(--duration,auto)", "var(--duration,auto)");
    ];
  List.iter
    (none_cursor read_declaration)
    [
      "animation-delay:auto";
      "animation-delay:1s,auto";
      "transition-duration:auto";
      "transition-duration:1s,auto";
      "transition-delay:auto";
      "animation-duration:calc(auto)";
      "animation-duration:auto auto";
    ];
  let open Css.Properties in
  let check_slots input =
    let cursor = Cursor.of_string input in
    match read_animation cursor with
    | Shorthand value as animation when Cursor.is_done cursor ->
        Alcotest.(check (option string))
          "auto duration" (Some "auto")
          (Option.map (Css.Pp.to_string Css.pp_duration) value.duration);
        Alcotest.(check (option string))
          "zero delay" (Some "0s")
          (Option.map (Css.Pp.to_string Css.pp_duration) value.delay);
        (match value.name with
        | Some (Name "spin") -> ()
        | _ -> Alcotest.fail "animation name changed");
        animation
    | _ -> Alcotest.failf "expected one animation: %s" input
  in
  List.iter
    (fun input ->
      let value = check_slots input in
      List.iter
        (fun minify ->
          ignore (check_slots (Css.Pp.to_string ~minify pp_animation value)))
        [ false; true ])
    [ "auto spin"; "spin auto"; "AUTO spin" ]

let sizing_keyword_domains () =
  (* CSS Sizing 3 sections 3.1.1-3.1.3 distinguish auto sizes from none
     maximums. CSS Logical Properties 1 section 4.1 uses the same grammars for
     the corresponding flow-relative dimensions. *)
  List.iter
    (fun dimension ->
      List.iter
        (fun (prefix, valid, invalid) ->
          let property = prefix ^ dimension in
          List.iter
            (fun value ->
              check_declaration ~roundtrip:true (property ^ ":" ^ value))
            [
              valid;
              "min-content";
              "max-content";
              "fit-content(10%)";
              "0";
              "2px";
              "10%";
              "calc(1em + 2px)";
              "inherit";
              "initial";
              "unset";
              "revert";
              "revert-layer";
              "var(--size," ^ invalid ^ ")";
            ];
          List.iter
            (fun value -> none_cursor read_declaration (property ^ ":" ^ value))
            [
              invalid;
              String.uppercase_ascii invalid;
              "normal";
              "from-font";
              "size";
            ])
        [
          ("", "auto", "none");
          ("min-", "auto", "none");
          ("max-", "none", "auto");
        ])
    [ "width"; "height"; "inline-size"; "block-size" ]

let suite =
  ( "declaration_keywords",
    [
      test_case "list properties" `Quick list_properties;
      test_case "animation infinite name" `Quick animation_infinite_name;
      test_case "animation keyword names" `Quick animation_keyword_names;
      test_case "list-style custom names" `Quick list_style_custom_names;
      test_case "auto is not a color" `Quick auto_is_not_a_color;
      test_case "outline-offset length only" `Quick outline_offset_length_only;
      test_case "line-height-step length only" `Quick
        line_height_step_length_only;
      test_case "gap normal" `Quick gap_normal;
      test_case "sizing keyword domains" `Quick sizing_keyword_domains;
      test_case "animation auto duration" `Quick animation_auto_duration;
    ] )
