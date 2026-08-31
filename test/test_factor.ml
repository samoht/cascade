open Cascade

let rules css =
  match Css.of_string css with
  | Ok { stylesheet; _ } ->
      List.filter_map
        (function Stylesheet.Rule r -> Some r | _ -> None)
        (Css.statements stylesheet)
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let render rules =
  String.concat ""
    (List.map (Pp.to_string ~minify:true Stylesheet.pp_rule) rules)

let optimize_str css =
  match Css.of_string css with
  | Ok { stylesheet; _ } ->
      Css.optimize stylesheet |> Css.to_string ~minify:true
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

(* A selector whose box shorthand is split across rules (its base plus a corner
   longhand, as Tailwind emits for [.form-select]) must optimise to the same
   output as the already consolidated form the optimiser itself produces:
   consolidation runs before factoring, so a partial group cannot absorb the
   shared base before the selector's own rules are merged. *)
let test_split_shorthand_confluence () =
  let split =
    ".i{color:red;margin:0;padding:5px}.s{color:red;margin:0;padding:5px}.s{background-repeat:no-repeat;padding-right:8px}.t{color:red;margin:0;padding:5px}"
  in
  let consolidated =
    ".i{color:red;margin:0;padding:5px}.s{color:red;margin:0;padding:5px;background-repeat:no-repeat;padding-right:8px}.t{color:red;margin:0;padding:5px}"
  in
  Alcotest.(check string)
    "split and consolidated shorthands optimise identically"
    (optimize_str consolidated)
    (optimize_str split)

let test_run_reaches_fixpoint_with_finalizer () =
  let optimized =
    Factor.run ~ctx:Ctx.fragment
      ~finalize:(Rule.finalize ~ctx:Ctx.fragment)
      (rules
         ".a{display:flex;margin-top:1px;margin-right:1px;margin-bottom:1px;margin-left:1px}.b{display:flex;margin-top:2px;margin-right:2px;margin-bottom:2px;margin-left:2px}.c{display:flex;margin-top:3px;margin-right:3px;margin-bottom:3px;margin-left:3px}")
  in
  Alcotest.(check string)
    "run factors and finalizes leftovers"
    ".a,.b,.c{display:flex;margin:1px}.b{margin:2px}.c{margin:3px}"
    (render optimized)

let fixpoints_run stats = (Stats.snapshot stats).counters.factor_fixpoints_run

let test_cache_reuses_identical_rule_run () =
  let stats = Stats.v () in
  let ctx = Ctx.v ~stats `Fragment in
  let cache = Factor.cache () in
  let input =
    rules
      ".a{display:flex;margin:1px}.b{display:flex;margin:2px}.c{display:flex;margin:3px}"
  in
  let finalize = Rule.finalize ~ctx in
  let first = Factor.run ~cache ~ctx ~finalize input in
  let fixpoints = fixpoints_run stats in
  ignore (Factor.run ~cache ~ctx ~finalize input);
  Alcotest.(check int)
    "cached run does not start another fixpoint" fixpoints (fixpoints_run stats);
  Alcotest.(check string)
    "first run still optimizes"
    ".a,.b,.c{display:flex;margin:1px}.b{margin:2px}.c{margin:3px}"
    (render first)

let read_example name =
  let candidates = [ "examples/" ^ name; "test/examples/" ^ name ] in
  match List.find_opt Sys.file_exists candidates with
  | Some path ->
      let ic = open_in path in
      Fun.protect
        ~finally:(fun () -> close_in ic)
        (fun () -> really_input_string ic (in_channel_length ic))
  | None -> Alcotest.failf "fixture %s not found (tried examples/)" name

(* Factoring must not depend on unrelated content: two sheets that differ only
   in whether an unrelated pair of rules is written grouped or split must
   optimise identically. The transfer-objective gate scores a segment by its
   estimated compressed size, which a distant grouping shifts by a byte or two;
   reverting on that noise once flipped a far-away, otherwise raw-smaller
   factoring (the prose list-item rules here) depending only on the shadow
   spelling. The fixture holds the shared prose; the two spellings of the
   unrelated [.shadow]/[.shadow-sm] pair are appended here. *)
let test_factoring_stable_under_unrelated_grouping () =
  let prose = read_example "prose_lg_prefix.css" in
  let shadow =
    "{--tw-shadow:0 1px 3px 0 var(--tw-shadow-color,#0000001a),0 1px 2px -1px \
     var(--tw-shadow-color,#0000001a);box-shadow:var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)}"
  in
  let separate = prose ^ ".shadow" ^ shadow ^ ".shadow-sm" ^ shadow ^ "}" in
  let grouped = prose ^ ".shadow,.shadow-sm" ^ shadow ^ "}" in
  Alcotest.(check string)
    "unrelated grouping does not change factoring" (optimize_str separate)
    (optimize_str grouped)

(* Two rules that declare the same thing must factor together, whatever spelling
   the author used. CSS Backgrounds 3 (ED) sec. 3.4 and CSS UI 4 (ED) sec. 3.1
   make every slot of these shorthands optional, and an omitted one takes its
   initial value (CSS Cascade 5 (ED) sec. 3), which for the width slot is
   [medium] (CSS Backgrounds 3 sec. 3.3, CSS UI 4 sec. 3.2). An explicit
   [medium] and an absent width are therefore the same declaration, and
   factoring compares nodes, so the drop has to have happened before the two
   rules meet. *)
let test_initial_line_width_spellings_factor () =
  Alcotest.(check string)
    "border medium factors with the omitted width" ".a,.b{border:solid red}"
    (optimize_str ".a{border:medium solid red}.b{border:solid red}");
  Alcotest.(check string)
    "outline medium factors with the omitted width" ".a,.b{outline:solid red}"
    (optimize_str ".a{outline:medium solid red}.b{outline:solid red}")

(* The same argument one slot over. CSS Backgrounds 3 (ED) sec. 3.2 gives the
   border-style properties the initial value [none], and CSS UI 4 (ED) sec. 3.3
   gives outline-style the same one, so an explicit [none] and an absent style
   are one declaration. A shorthand left holding nothing but initial values is
   the [none] keyword, which is the other spelling these pairs meet on. *)
let test_initial_line_style_spellings_factor () =
  Alcotest.(check string)
    "border medium none factors with the none keyword" ".a,.b{border:none}"
    (optimize_str ".a{border:medium none}.b{border:none}");
  Alcotest.(check string)
    "outline medium none factors with the none keyword" ".c,.d{outline:none}"
    (optimize_str ".c{outline:medium none}.d{outline:none}");
  Alcotest.(check string)
    "border none red factors with the omitted style" ".a,.b{border:red}"
    (optimize_str ".a{border:none red}.b{border:red}");
  Alcotest.(check string)
    "outline none red factors with the omitted style" ".c,.d{outline:red}"
    (optimize_str ".c{outline:none red}.d{outline:red}");
  (* css-logical-1 sec. 4.4 and CSS Multi-column 1 (ED) sec. 4.5 route the other
     border-family shorthands through the same production. *)
  Alcotest.(check string)
    "border-top medium none factors with the none keyword"
    ".a,.b{border-top:none}"
    (optimize_str ".a{border-top:medium none}.b{border-top:none}");
  Alcotest.(check string)
    "border-inline medium none factors with the none keyword"
    ".a,.b{border-inline:none}"
    (optimize_str ".a{border-inline:medium none}.b{border-inline:none}");
  Alcotest.(check string)
    "border-block-start medium none factors with the none keyword"
    ".a,.b{border-block-start:none}"
    (optimize_str
       ".a{border-block-start:medium none}.b{border-block-start:none}");
  Alcotest.(check string)
    "column-rule medium none factors with the none keyword"
    ".a,.b{column-rule:none}"
    (optimize_str ".a{column-rule:medium none}.b{column-rule:none}")

(* The same argument one slot further over. CSS Backgrounds 3 (ED) sec. 3.1
   gives the border-color properties the initial value [currentColor], and CSS
   Multi-column 1 (ED) sec. 4.2 and css-logical-1 sec. 4.5.3 repeat it for
   column-rule and the logical borders, so an explicit [currentColor] and an
   absent colour are one declaration. CSS UI 4 (ED) sec. 3.4 gives outline-color
   the initial value [auto] instead, so those two rules name different colours
   and must stay apart. *)
let test_initial_line_color_spellings_factor () =
  Alcotest.(check string)
    "border currentColor factors with the omitted colour" ".a,.b{border:solid}"
    (optimize_str ".a{border:solid currentcolor}.b{border:solid}");
  Alcotest.(check string)
    "border currentColor alone factors with the none keyword"
    ".a,.b{border:none}"
    (optimize_str ".a{border:currentcolor}.b{border:none}");
  Alcotest.(check string)
    "column-rule currentColor factors with the omitted colour"
    ".a,.b{column-rule:solid}"
    (optimize_str ".a{column-rule:solid currentcolor}.b{column-rule:solid}");
  Alcotest.(check string)
    "border-inline currentColor factors with the omitted colour"
    ".a,.b{border-inline:solid}"
    (optimize_str ".a{border-inline:solid currentcolor}.b{border-inline:solid}");
  Alcotest.(check string)
    "outline currentColor does not factor with the omitted colour"
    ".a{outline:solid currentColor}.b{outline:solid}"
    (optimize_str ".a{outline:solid currentcolor}.b{outline:solid}")

(* The [background] shorthand resets every longhand it covers (CSS Backgrounds 3
   (ED) sec. 2.10), so a slot written at its own initial value is the longer
   spelling of the same declaration, and a layer that fills no slot is what
   [background: none] declares. Factoring compares nodes, so the fold has to
   have happened before the two rules meet. *)
let test_background_initial_slot_spellings_factor () =
  Alcotest.(check string)
    "an initial position factors with the omitted one" ".a,.b{background:red}"
    (optimize_str ".a{background:red 0 0}.b{background:red}");
  Alcotest.(check string)
    "an initial repeat factors with the omitted one" ".a,.b{background:red}"
    (optimize_str ".a{background:red repeat}.b{background:red}");
  Alcotest.(check string)
    "the none keyword factors with the drained layer" ".a,.b{background:0 0}"
    (optimize_str ".a{background:none}.b{background:0 0}");
  (* sec. 2.6 reads a lone position value as [<x> center], which is not the
     initial [0% 0%], so these two layers put the image in different places. *)
  Alcotest.(check string)
    "a lone position does not factor with the omitted one"
    ".a{background:url(a.png)0}.b{background:url(a.png)}"
    (optimize_str ".a{background:url(a.png) 0}.b{background:url(a.png)}");
  (* sec. 2.10 reads a single [<box>] as both origin and clip, so dropping the
     clip alone would repaint the layer over a smaller area. *)
  Alcotest.(check string)
    "an initial clip beside a non-initial origin does not factor"
    ".a{background:content-box border-box red}.b{background:content-box red}"
    (optimize_str
       ".a{background:red content-box border-box}.b{background:red content-box}")

(* CSS Backgrounds 3 (ED) sec. 2.4 gives each single [<repeat-style>] keyword
   the axis pair it computes to, so the pair and the keyword name one value.
   Factoring compares nodes, so the two rules meet only if the fold ran
   first. *)
let test_repeat_style_spellings_factor () =
  Alcotest.(check string)
    "repeat repeat factors with repeat" ".a,.b{background-repeat:repeat}"
    (optimize_str
       ".a{background-repeat:repeat repeat}.b{background-repeat:repeat}");
  Alcotest.(check string)
    "repeat no-repeat factors with repeat-x" ".a,.b{background-repeat:repeat-x}"
    (optimize_str
       ".a{background-repeat:repeat no-repeat}.b{background-repeat:repeat-x}");
  (* Two different axes have no one-keyword spelling, so these stay apart. *)
  Alcotest.(check string)
    "repeat space does not factor with repeat"
    ".a{background-repeat:repeat space}.b{background-repeat:repeat}"
    (optimize_str
       ".a{background-repeat:repeat space}.b{background-repeat:repeat}")

(* CSS Box 4 (ED) sec. 3.2 fills the four sides from one, two or three values,
   so a list that repeats what those rules already supply is the longer spelling
   of the same declaration. Factoring compares nodes, so the two rules meet only
   if the collapse ran first. *)
let test_box_shorthand_repeats_factor () =
  Alcotest.(check string)
    "four equal sides factor with the single value" ".a,.b{inset:0}"
    (optimize_str ".a{inset:0 0 0 0}.b{inset:0}");
  Alcotest.(check string)
    "a repeated axis factors with the pair" ".a,.b{padding:1px 2px}"
    (optimize_str ".a{padding:1px 2px 1px 2px}.b{padding:1px 2px}");
  Alcotest.(check string)
    "border-color takes the same collapse" ".a,.b{border-color:red}"
    (optimize_str ".a{border-color:red red red red}.b{border-color:red}");
  Alcotest.(check string)
    "scroll-margin factors with the single value" ".a,.b{scroll-margin:1px}"
    (optimize_str ".a{scroll-margin:1px 1px 1px 1px}.b{scroll-margin:1px}");
  Alcotest.(check string)
    "scroll-margin-inline factors with the single value"
    ".a,.b{scroll-margin-inline:2px}"
    (optimize_str ".a{scroll-margin-inline:2px 2px}.b{scroll-margin-inline:2px}");
  Alcotest.(check string)
    "scroll-margin-block factors with the single value"
    ".a,.b{scroll-margin-block:3px}"
    (optimize_str ".a{scroll-margin-block:3px 3px}.b{scroll-margin-block:3px}");
  Alcotest.(check string)
    "scroll-padding factors with the repeated axis"
    ".a,.b{scroll-padding:4px 5px}"
    (optimize_str ".a{scroll-padding:4px 5px 4px 5px}.b{scroll-padding:4px 5px}");
  Alcotest.(check string)
    "scroll-padding-inline factors with the single value"
    ".a,.b{scroll-padding-inline:6px}"
    (optimize_str
       ".a{scroll-padding-inline:6px 6px}.b{scroll-padding-inline:6px}");
  Alcotest.(check string)
    "scroll-padding-block factors with the single value"
    ".a,.b{scroll-padding-block:7px}"
    (optimize_str ".a{scroll-padding-block:7px 7px}.b{scroll-padding-block:7px}");
  (* CSS Backgrounds 3 (ED) sec. 4.1 collapses each radii group on its own, and
     with no slash the values set both axes equally. *)
  Alcotest.(check string)
    "border-radius collapses each group" ".a,.b{border-radius:5px/10px}"
    (optimize_str
       ".a{border-radius:5px 5px 5px 5px/10px 10px 10px \
        10px}.b{border-radius:5px/10px}");
  Alcotest.(check string)
    "equal radii axes factor with the omitted group" ".a,.b{border-radius:5px}"
    (optimize_str ".a{border-radius:5px/5px}.b{border-radius:5px}");
  (* CSS Tables 3 (ED) reads a single border-spacing length as both axes. *)
  Alcotest.(check string)
    "equal border-spacing axes factor with the single length"
    ".a,.b{border-spacing:1px}"
    (optimize_str ".a{border-spacing:1px 1px}.b{border-spacing:1px}");
  (* A list the rules cannot rebuild stays as written, and the two stay
     apart. *)
  Alcotest.(check string)
    "four distinct sides do not factor with three"
    ".a{margin:1px 2px 3px 4px}.b{margin:1px 2px 3px}"
    (optimize_str ".a{margin:1px 2px 3px 4px}.b{margin:1px 2px 3px}")

(* CSS Masking 1 (ED) sec. 8.2 gives mask-border-mode the initial value [alpha],
   so an explicit [alpha] and an absent mode are one declaration. Factoring
   compares nodes, so the drop has to have happened before the two rules
   meet. *)
let test_mask_border_mode_spellings_factor () =
  Alcotest.(check string)
    "an explicit alpha factors with the omitted mode"
    ".a,.b{mask-border:url(a.png)}"
    (optimize_str ".a{mask-border:url(a.png) alpha}.b{mask-border:url(a.png)}");
  Alcotest.(check string)
    "luminance does not factor with the omitted mode"
    ".a{mask-border:url(a.png)luminance}.b{mask-border:url(a.png)}"
    (optimize_str
       ".a{mask-border:url(a.png) luminance}.b{mask-border:url(a.png)}")

(* CSS Fonts 4 (ED) sec. 2.2 defines [normal] as "Same as 400" and [bold] as
   "Same as 700", so the keyword and the number name one weight. Factoring
   compares nodes, so the two rules only meet as one declaration if the fold
   happened before they were compared. *)
let test_font_weight_spellings_factor () =
  Alcotest.(check string)
    "bold factors with 700" ".a,.b{font-weight:700}"
    (optimize_str ".a{font-weight:bold}.b{font-weight:700}");
  Alcotest.(check string)
    "normal factors with 400" ".a,.b{font-weight:400}"
    (optimize_str ".a{font-weight:normal}.b{font-weight:400}")

(* CSS Display 3 (ED) sec. 2.1, 2.2 and 2.6 give most two-value [display] forms
   a single-keyword spelling of the same value, and sec. 2.3 gives [block flow
   list-item] one. Factoring compares nodes, so the two rules meet as one
   declaration only if the fold happened first. *)
let test_display_spellings_factor () =
  Alcotest.(check string)
    "block flow factors with block" ".a,.b{display:block}"
    (optimize_str ".a{display:block flow}.b{display:block}");
  Alcotest.(check string)
    "inline flow-root factors with inline-block" ".a,.b{display:inline-block}"
    (optimize_str ".a{display:inline flow-root}.b{display:inline-block}");
  Alcotest.(check string)
    "block flow list-item factors with list-item" ".a,.b{display:list-item}"
    (optimize_str ".a{display:block flow list-item}.b{display:list-item}");
  (* sec. 2.2 reads [ruby] as [inline ruby], so [block ruby] names a different
     value and the two rules stay apart. *)
  Alcotest.(check string)
    "block ruby does not factor with ruby"
    ".a{display:block ruby}.b{display:ruby}"
    (optimize_str ".a{display:block ruby}.b{display:ruby}")

(* CSS Overflow 3 (ED) sec. 3.1: the omitted second value is copied from the
   first, so a repeated axis is the longer spelling of the single value. *)
let test_overflow_pair_spellings_factor () =
  Alcotest.(check string)
    "auto auto factors with auto" ".a,.b{overflow:auto}"
    (optimize_str ".a{overflow:auto auto}.b{overflow:auto}")

(* CSS Fonts 4 (ED) sec. 2.1 makes [font-family] a prioritized list the user
   agent walks until a family matches, so a repeated entry is unreachable and
   [Arial, Arial] names the value [Arial] names. Factoring compares nodes, so
   the two rules only meet as one declaration if the fold happened before they
   were compared. *)
let test_font_family_duplicate_factors () =
  Alcotest.(check string)
    "a repeated family factors with the single one" ".a,.b{font-family:Arial}"
    (optimize_str ".a{font-family:Arial,Arial}.b{font-family:Arial}")

(* CSS Fonts 4 (ED) sec. 2.3 maps [condensed] onto 75%, and getComputedStyle()
   serializes the property as a percentage whichever spelling was authored, so
   the keyword and the percentage name one width. Factoring compares nodes, so
   the two rules only meet as one declaration if the fold happened before they
   were compared. *)
let test_font_stretch_spellings_factor () =
  Alcotest.(check string)
    "condensed factors with 75%" ".a,.b{font-stretch:75%}"
    (optimize_str ".a{font-stretch:condensed}.b{font-stretch:75%}")

(* CSS Values 4 (ED) sec. 10.10.1 sums a calc's same-unit children and returns
   the lone remaining child, so [calc(1px + 1px)] and [2px] name one size.
   Factoring compares nodes, so the two rules only meet as one declaration if
   the fold happened before they were compared. *)
let test_font_size_calc_factors () =
  Alcotest.(check string)
    "a folded calc factors with the length" ".a,.b{font-size:2px}"
    (optimize_str ".a{font-size:calc(1px + 1px)}.b{font-size:2px}");
  Alcotest.(check string)
    "a folded calc factors with the percentage" ".a,.b{font-size:100%}"
    (optimize_str ".a{font-size:calc(50% + 50%)}.b{font-size:100%}")

(* CSS Fonts 4 (ED) sec. 2.7 resets every subproperty of [font] to its initial
   value before applying the slots given explicitly, so a slot holding its
   longhand's initial and no slot at all name one value. Factoring compares
   nodes, so the three rules only meet as one declaration if the drop happened
   before they were compared. *)
let test_font_shorthand_default_slots_factor () =
  Alcotest.(check string)
    "an initial slot factors with the slot left out" ".a,.b,.c{font:12px serif}"
    (optimize_str
       ".a{font:400 12px serif}.b{font:12px/normal serif}.c{font:12px serif}")

(* CSS Transitions 1 (ED) sec. 2.5 lets every component of a
   [<single-transition>] fall back to its longhand initial, so a transition that
   writes [ease] and [0s] out is the one the bare duration already names.
   Factoring compares nodes, so the two rules meet as one declaration only if
   the fold happened first. *)
let test_transition_default_spellings_factor () =
  Alcotest.(check string)
    "spelled-out initials factor with the omitted form"
    ".a,.b{transition:all 1s}"
    (optimize_str ".a{transition:all 1s ease 0s}.b{transition:1s}");
  (* The second time is the delay, so a zero duration in front of one is not a
     default that can go: the two rules name different transitions. *)
  Alcotest.(check string)
    "a delayed transition does not factor with a plain one"
    ".a{transition:opacity 0s 2s}.b{transition:opacity 2s}"
    (optimize_str ".a{transition:opacity 0s 2s}.b{transition:opacity 2s}")

(* CSS Easing 1 (ED) sec. 2.2 and sec. 2.3 give the named curves and the
   one-step easings a keyword spelling of the same function, so the two rules
   set the same easing and have to reach factoring as one node. *)
let test_timing_function_spellings_factor () =
  Alcotest.(check string)
    "the named curve factors with its keyword"
    ".a,.b{transition-timing-function:ease-in}"
    (optimize_str
       ".a{transition-timing-function:cubic-bezier(.42,0,1,1)}.b{transition-timing-function:ease-in}");
  Alcotest.(check string)
    "the one-step easing factors with its keyword"
    ".a,.b{transition-timing-function:step-end}"
    (optimize_str
       ".a{transition-timing-function:steps(1,end)}.b{transition-timing-function:step-end}")

(* CSS Box 4 (ED) sec. 3.2 and sec. 4.2 assign one to four values around the
   box, so a shorthand whose sides repeat names what the shorter spelling names.
   Factoring compares nodes, so the two rules meet as one declaration only if
   the fold happened first. *)
let test_box_shorthand_spellings_factor () =
  Alcotest.(check string)
    "four equal sides factor with the single value" ".a,.b{margin:2px}"
    (optimize_str ".a{margin:2px 2px 2px 2px}.b{margin:2px}");
  Alcotest.(check string)
    "an axis pair factors with the two-value form" ".a,.b{padding:1px 2px}"
    (optimize_str ".a{padding:1px 2px 1px 2px}.b{padding:1px 2px}")

(* CSS Lists 3 (ED) sec. 3.6 lets each component of [list-style] fall back to
   its longhand initial, so the marker written with its initials spelled out is
   the one the single keyword already names. *)
let test_list_style_default_spellings_factor () =
  Alcotest.(check string)
    "spelled-out initials factor with the bare type" ".a,.b{list-style:disc}"
    (optimize_str ".a{list-style:disc outside none}.b{list-style:disc}")

(* CSS Text Decoration 4 (ED) sec. 2.6 sets an omitted shorthand component to
   its initial, and sec. 4 with CSS Backgrounds 3 (ED) sec. 6.1 reads an omitted
   shadow length as zero, so each pair below is one value under two
   spellings. *)
let test_text_decoration_default_spellings_factor () =
  Alcotest.(check string)
    "the initial style factors away" ".a,.b{text-decoration:underline}"
    (optimize_str
       ".a{text-decoration:underline solid}.b{text-decoration:underline}");
  Alcotest.(check string)
    "a zero blur factors with the two-length shadow"
    ".a,.b{text-shadow:1px 1px}"
    (optimize_str ".a{text-shadow:1px 1px 0}.b{text-shadow:1px 1px}")

(* A typed caller can build transform-origin with the shared [position_value]
   nodes even though authored CSS uses the property's narrower grammar. Those
   nodes must canonicalise to XY/XYZ before declarations are hashed. *)
let test_transform_origin_position_nodes_factor () =
  let optimize_with parsed value =
    match Css.of_string parsed with
    | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)
    | Ok { stylesheet; _ } ->
        let declaration = Declaration.v Properties.Transform_origin value in
        Css.concat
          [
            stylesheet;
            Css.v
              [ Css.rule ~selector:(Selector.of_string ".b") [ declaration ] ];
          ]
        |> Css.optimize |> Css.to_string ~minify:true
  in
  let offset edge amount = (edge, (Length amount : Values.length_percentage)) in
  let position x_edge x y_edge y =
    let x_edge, x = offset x_edge x and y_edge, y = offset y_edge y in
    (Properties.Edge_offset_edge_offset (x_edge, x, y_edge, y)
      : Properties.position_value)
  in
  Alcotest.(check string)
    "position node factors with an XY origin"
    ".a,.b{transform-origin:10px 20px}"
    (optimize_with ".a{transform-origin:10px 20px}"
       (Properties.Position (position "left" (Px 10.) "top" (Px 20.))));
  Alcotest.(check string)
    "position node factors with an XYZ origin"
    ".a,.b{transform-origin:10px 20px 30px}"
    (optimize_with ".a{transform-origin:10px 20px 30px}"
       (Properties.Position_z (position "left" (Px 10.) "top" (Px 20.), Px 30.)))

(* These equivalent spellings were still canonicalised by their printers, too
   late for declaration hashes to meet during factoring. *)
let test_remaining_printer_fold_spellings_factor () =
  Alcotest.(check string)
    "logical minimum initial factors with auto" ".a,.b{min-inline-size:auto}"
    (optimize_str ".a{min-inline-size:initial}.b{min-inline-size:auto}");
  Alcotest.(check string)
    "logical minimum initial stays distinct from zero"
    ".a{min-inline-size:auto}.b{min-inline-size:0}"
    (optimize_str ".a{min-inline-size:initial}.b{min-inline-size:0}");
  Alcotest.(check string)
    "logical block minimum initial factors with auto"
    ".a,.b{min-block-size:auto}"
    (optimize_str ".a{min-block-size:initial}.b{min-block-size:auto}");
  Alcotest.(check string)
    "milliseconds factor with seconds" ".a,.b{transition-duration:.5s}"
    (optimize_str ".a{transition-duration:500ms}.b{transition-duration:.5s}");
  Alcotest.(check string)
    "origin keyword factors with percentage" ".a,.b{transform-origin:50%}"
    (optimize_str ".a{transform-origin:center}.b{transform-origin:50%}");
  Alcotest.(check string)
    "stepped duration factors with its result" ".a,.b{transition-duration:1s}"
    (optimize_str
       ".a{transition-duration:round(1.1s,.5s)}.b{transition-duration:1s}");
  Alcotest.(check string)
    "angle hue factors with bare degrees"
    ".a,.b{color:hsl(180 50%50%/var(--a))}"
    (optimize_str
       ".a{color:hsl(.5turn 50% 50%/var(--a))}.b{color:hsl(180 50% \
        50%/var(--a))}")

let suite =
  ( "factor",
    [
      Alcotest.test_case "run reaches fixpoint with finalizer" `Quick
        test_run_reaches_fixpoint_with_finalizer;
      Alcotest.test_case "cache reuses identical rule run" `Quick
        test_cache_reuses_identical_rule_run;
      Alcotest.test_case "split shorthand optimises like consolidated" `Quick
        test_split_shorthand_confluence;
      Alcotest.test_case "factoring stable under unrelated grouping" `Quick
        test_factoring_stable_under_unrelated_grouping;
      Alcotest.test_case "initial line-width spellings factor together" `Quick
        test_initial_line_width_spellings_factor;
      Alcotest.test_case "initial line-style spellings factor together" `Quick
        test_initial_line_style_spellings_factor;
      Alcotest.test_case "initial line-color spellings factor together" `Quick
        test_initial_line_color_spellings_factor;
      Alcotest.test_case "initial background slot spellings factor together"
        `Quick test_background_initial_slot_spellings_factor;
      Alcotest.test_case "repeat-style spellings factor together" `Quick
        test_repeat_style_spellings_factor;
      Alcotest.test_case "box shorthand repeats factor together" `Quick
        test_box_shorthand_repeats_factor;
      Alcotest.test_case "mask-border mode spellings factor together" `Quick
        test_mask_border_mode_spellings_factor;
      Alcotest.test_case "font-weight keyword and number factor together" `Quick
        test_font_weight_spellings_factor;
      Alcotest.test_case "font-family duplicate factors with the single family"
        `Quick test_font_family_duplicate_factors;
      Alcotest.test_case "font-stretch keyword and percentage factor together"
        `Quick test_font_stretch_spellings_factor;
      Alcotest.test_case "font-size calc factors with the folded length" `Quick
        test_font_size_calc_factors;
      Alcotest.test_case "font shorthand initial slots factor together" `Quick
        test_font_shorthand_default_slots_factor;
      Alcotest.test_case "display two-value and legacy spellings factor" `Quick
        test_display_spellings_factor;
      Alcotest.test_case "equal overflow axes factor with the single value"
        `Quick test_overflow_pair_spellings_factor;
      Alcotest.test_case "spelled-out transition initials factor away" `Quick
        test_transition_default_spellings_factor;
      Alcotest.test_case "easing curves factor with their keywords" `Quick
        test_timing_function_spellings_factor;
      Alcotest.test_case "repeated box sides factor with the short form" `Quick
        test_box_shorthand_spellings_factor;
      Alcotest.test_case "spelled-out list-style initials factor away" `Quick
        test_list_style_default_spellings_factor;
      Alcotest.test_case "spelled-out text decoration defaults factor away"
        `Quick test_text_decoration_default_spellings_factor;
      Alcotest.test_case "transform origin position nodes factor" `Quick
        test_transform_origin_position_nodes_factor;
      Alcotest.test_case "remaining printer folds factor together" `Quick
        test_remaining_printer_fold_spellings_factor;
    ] )
