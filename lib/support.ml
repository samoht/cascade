type version = int * int

type targets = {
  chrome : version;
  firefox : version;
  safari : version;
  ios_safari : version;
}

let evergreen =
  {
    chrome = (111, 0);
    firefox = (128, 0);
    safari = (16, 4);
    ios_safari = (16, 4);
  }

type measurement = {
  key : string;
  support : Baseline.support;
  why : string;
  measured : string;
}

(* Measured by hand where web-features records no key. Each is a production a
   specification grants and a browser has not shipped, so cascade reading it is
   right and the browser's answer says nothing about the value.

   This is library knowledge rather than a test fixture: it is a fact about CSS
   in the world, which is what cascade models. A harness carrying it would be
   excusing its own failures; one that asks {!implemented} is reading what the
   library knows.

   The bar for an entry is the bar for a generated row: the specification
   section that grants the grammar, and the build the disagreement was measured
   on. A key the dataset later carries should be deleted from here rather than
   left to drift. *)
let measured =
  [
    {
      key = "css.properties.background-blend-mode.plus-lighter";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "Compositing 2 sec. 3.4.3 spells background-blend-mode \
         <'mix-blend-mode'>#, and sec. 3.4.1 gives mix-blend-mode <blend-mode> \
         | plus-lighter, so the value is granted on both. web-features records \
         the mix-blend-mode key alone";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.transition.none_in_a_list";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Transitions 1 sec. 2.4 spells the shorthand <single-transition>#, \
         and sec. 2.3 gives <single-transition> a [ none | \
         <single-transition-property> ], so none is one entry of the list. \
         Chrome reads it alone and refuses every list holding one";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.text-decoration-thickness.hairline";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Text Decoration 4 sec. 2.4 takes <line-width>, and CSS Borders 4 \
         sec. 2.3 defines <line-width> = <length [0,inf]> | hairline | thin | \
         medium | thick";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.text-decoration-thickness.thin";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why = "CSS Borders 4 sec. 2.3: thin is a <line-width>";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.text-decoration-thickness.thick";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why = "CSS Borders 4 sec. 2.3: thick is a <line-width>";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.text-transform.full_width";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Text 4 sec. 2.1: none | [ capitalize | uppercase | lowercase ] || \
         full-width || full-size-kana | math-auto";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.text-combine-upright.digits";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Writing Modes 4 sec. 9.1: none | all | [ digits <integer [2,4]>? \
         ]; Chrome has only none and all";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.text-combine-upright.digits_2";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why = "CSS Writing Modes 4 sec. 9.1: the integer ranges over [2,4]";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.text-combine-upright.digits_4";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why = "CSS Writing Modes 4 sec. 9.1: the integer ranges over [2,4]";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.alignment-baseline.text_bottom";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Inline 3 sec. 4.2.2: baseline | <baseline-metric>, and \
         <baseline-metric> begins text-bottom | alphabetic | ideographic; \
         Chrome implements the SVG 1.1 keyword set";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.baseline-shift.top";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Inline 3 sec. 4.2.3: <length-percentage> | sub | super | top | \
         center | bottom";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.baseline-shift.center";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why = "CSS Inline 3 sec. 4.2.3 lists center";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.baseline-shift.bottom";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why = "CSS Inline 3 sec. 4.2.3 lists bottom";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.grid-template-rows.masonry";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "the CSS Grid 3 Working Draft of 2024 added masonry to \
         grid-template-rows, and Firefox ships it; the current draft has \
         replaced it with display: grid-lanes, so this row is the one entry \
         here that wants a decision rather than a browser";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.outline-color.auto";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS UI 4 sec. 3.4: auto | <'border-top-color'>, and auto is the \
         initial value; Chrome computes that initial value without accepting \
         the keyword";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.user-select.contain";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS UI 4 sec. 6.1: auto | text | none | contain | all; \
         -webkit-user-select is the browser's legacy name for the same \
         property";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.font-synthesis-style.oblique_only";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why = "CSS Fonts 4 sec. 2.8.2: auto | none | oblique-only";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.ruby-position.inter_character";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why = "CSS Ruby 1 sec. 4.1 lists inter-character";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.stroke-linejoin.miter_clip";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "SVG Strokes sec. 2.6: miter | miter-clip | round | bevel | arcs; \
         Chrome has miter, round and bevel";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.stroke-linejoin.arcs";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why = "SVG Strokes sec. 2.6 lists arcs";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.vector-effect.non_scaling_size";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "SVG 2 sec. 8.13: none | [ non-scaling-stroke | non-scaling-size | \
         non-rotation | fixed-position ]+ [ viewport | screen ]?; Chrome has \
         only non-scaling-stroke";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.vector-effect.non_rotation";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why = "SVG 2 sec. 8.13 lists non-rotation";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.vector-effect.fixed_position";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why = "SVG 2 sec. 8.13 lists fixed-position";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.vector-effect.non_scaling_stroke_screen";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why = "SVG 2 sec. 8.13: the effect list is followed by viewport | screen";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.vector-effect.non_scaling_stroke_fixed_position";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why = "SVG 2 sec. 8.13: the effects themselves repeat with +";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.resize.auto";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS UI 4 sec. 4.1: none | both | horizontal | vertical | block | \
         inline. Chrome accepts auto, no specification defines it";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.text-orientation.sideways_right";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Writing Modes 4 sec. 5.1: mixed | upright | sideways. \
         sideways-right is a compatibility alias browsers may keep, not \
         grammar";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.alignment-baseline.auto";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Inline 3 sec. 4.2.2: baseline | <baseline-metric>, and no arm is \
         auto. Chrome accepts it from the SVG 1.1 grammar";
      measured = "Chrome 153";
    };
  ]

let measured_table =
  lazy
    (let t = Hashtbl.create (List.length measured) in
     List.iter (fun m -> Hashtbl.replace t m.key m.support) measured;
     t)

let table =
  lazy
    (let table = Hashtbl.create (List.length Baseline.support) in
     List.iter
       (fun (key, support) -> Hashtbl.replace table key support)
       Baseline.support;
     table)

let at_least (major, minor) (target_major, target_minor) =
  target_major > major || (target_major = major && target_minor >= minor)

(* An engine answers yes when the dataset names the version that shipped the key
   and the target is at or past it. A [None] shipped version is an engine that
   does not implement the key at all, which no target version satisfies. *)
let engine_has shipped target =
  match shipped with None -> false | Some shipped -> at_least shipped target

let self_measured key =
  (not (Hashtbl.mem (Lazy.force table) key))
  && Hashtbl.mem (Lazy.force measured_table) key

(* The generated table answers first; [measured] is the supplement for keys
   web-features gives none, so a fact leaves here of its own accord when the
   dataset catches up. *)
let implemented targets key =
  match
    match Hashtbl.find_opt (Lazy.force table) key with
    | Some s -> Some s
    | None -> Hashtbl.find_opt (Lazy.force measured_table) key
  with
  | None -> None
  | Some (support : Baseline.support) ->
      Some
        (engine_has support.chrome targets.chrome
        && engine_has support.firefox targets.firefox
        && engine_has support.safari targets.safari
        && engine_has support.safari_ios targets.ios_safari)

type engine = Chrome | Firefox | Safari | Ios_safari

let engine_implements engine version key =
  match Hashtbl.find_opt (Lazy.force table) key with
  | None -> None
  | Some (support : Baseline.support) ->
      let shipped =
        match engine with
        | Chrome -> support.chrome
        | Firefox -> support.firefox
        | Safari -> support.safari
        | Ios_safari -> support.safari_ios
      in
      Some (engine_has shipped version)

let unimplemented_by targets key =
  match implemented targets key with
  | Some false -> true
  | Some true | None -> false
