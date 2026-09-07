(* What a headless Chrome cannot arbitrate, and the vectors where it and the
   specifications disagree.

   Shared by the harnesses in this directory. Both ask the same browser the same
   question, so a browser that catches up, or a specification that moves, is
   recorded once and both runs see it. Every entry carries the spec text that
   justifies it: without one an entry is a place for a mistake to hide.

   The two lists are the two directions. [spec_ahead] is grammar a specification
   defines and Chrome has not implemented, so Chrome rejecting it says nothing
   about the value. [lenient] is a value Chrome accepts that no specification
   grants, so Chrome accepting it says nothing either. *)

(* Chrome answers nothing about these, so the spec is their only oracle. Most
   are grammar it has not implemented; a few are names it has retired or never
   had, and [src] is a descriptor rather than a property, so asking about it as
   one gets a rejection that says nothing.

   Each harness checks the list against its own population in both directions: a
   name that becomes implemented, and a name that stops being, are both reported
   rather than skipped in silence. *)
let unimplemented =
  [
    "caret";
    "nav-up";
    "nav-down";
    "nav-left";
    "nav-right";
    "initial-letter-align";
    "initial-letter-wrap";
    "inline-sizing";
    "line-fit-edge";
    "line-height-step";
    "margin-trim";
    "mask-border";
    "min-intrinsic-sizing";
    "ruby-merge";
    "text-decoration-skip";
    "text-decoration-skip-box";
    "text-decoration-skip-inset";
    "text-decoration-skip-self";
    "text-decoration-skip-spaces";
    "text-emphasis-skip";
    "glyph-orientation-vertical";
    "image-resolution";
    "font-synthesis-position";
    "-moz-appearance";
    "-moz-osx-font-smoothing";
    "-ms-filter";
    "-o-transition";
    "-o-transform";
    (* Chrome has never carried Gecko's prefixed names, and dropped the
       Microsoft ones with the Trident engine. *)
    "-moz-animation";
    "-moz-animation-delay";
    "-moz-animation-direction";
    "-moz-animation-duration";
    "-moz-animation-fill-mode";
    "-moz-animation-iteration-count";
    "-moz-animation-name";
    "-moz-animation-play-state";
    "-moz-animation-timing-function";
    "-moz-border-radius";
    "-moz-box-shadow";
    "-moz-box-sizing";
    "-moz-orient";
    "-moz-transform";
    "-moz-transition";
    "-moz-transition-delay";
    "-moz-transition-duration";
    "-moz-transition-property";
    "-moz-transition-timing-function";
    "-moz-user-select";
    "-ms-transform";
    "-ms-user-select";
    (* A descriptor of @font-face, not a property: setProperty and CSS.supports
       both take a property name, so neither can be asked about it. *)
    "src";
    "-webkit-backdrop-filter";
    "-webkit-hyphens";
    "-webkit-mask-source-type";
    "-webkit-text-decoration";
    "-webkit-text-decoration-color";
  ]

(* One vector the browser and the manifest disagree about, and the spec text
   that decides it. Every entry has to be used: an entry that excuses nothing is
   reported, so a browser that catches up, or a row that drops the value, takes
   its excuse with it. *)
(* The reason an entry gives is the library's, where the library has one: a
   browser gap is a fact about CSS in the world, and {!Cascade.Support.measured}
   is where this project records the ones web-features does not model. An entry
   naming a key it does not carry is a mistake this catches at run time rather
   than letting prose drift from the fact. *)
let library_says key =
  match
    List.find_opt
      (fun (m : Cascade.Support.measurement) -> String.equal m.key key)
      Cascade.Support.measured
  with
  | Some m -> String.concat "" [ m.why; " (measured on "; m.measured; ")" ]
  | None ->
      failwith (String.concat "" [ "no measurement in the library for "; key ])

type excuse = {
  properties : string list;
  key : string option;
  value : string;
  why : string;
}

let sizing =
  [
    "width";
    "height";
    "min-width";
    "min-height";
    "max-width";
    "max-height";
    "inline-size";
    "min-inline-size";
    "max-inline-size";
    "block-size";
    "min-block-size";
    "max-block-size";
    "flex-basis";
  ]

(* Positives Chrome rejects. Each is grammar a specification defines and Chrome
   has not implemented, so the manifest is ahead of the browser rather than
   wrong. The citation is the whole justification: without it an entry is a
   place for a mistaken row to hide. *)
let spec_ahead : excuse list =
  [
    {
      properties = sizing;
      key = None;
      value = "fit-content(20rem)";
      why =
        "CSS Sizing 4 sec. 3.2 adds fit-content() to <box-size>, which every \
         sizing property takes; Chrome has only the bare fit-content keyword";
    };
    {
      properties =
        [
          "background";
          "background-image";
          "border-image";
          "border-image-source";
          "mask-image";
          "-webkit-mask-image";
          "list-style";
          "list-style-image";
          "content";
        ];
      key = None;
      value = "cross-fade(url(a.png) 40%, url(b.png))";
      why =
        "CSS Images 4 sec. 2.6: cross-fade() = cross-fade( <cf-image># ); \
         Chrome ships only -webkit-cross-fade()";
    };
    {
      properties = [ "text-decoration-thickness" ];
      key = None;
      value = "hairline";
      why =
        "CSS Text Decoration 4 sec. 2.4 takes <line-width>, and CSS Borders 4 \
         sec. 2.3 defines <line-width> = <length [0,inf]> | hairline | thin | \
         medium | thick";
    };
    {
      properties = [ "text-decoration-thickness" ];
      key = None;
      value = "thin";
      why = "CSS Borders 4 sec. 2.3: thin is a <line-width>";
    };
    {
      properties = [ "text-decoration-thickness" ];
      key = None;
      value = "thick";
      why = "CSS Borders 4 sec. 2.3: thick is a <line-width>";
    };
    {
      properties = [ "overflow-clip-margin" ];
      key = Some "css.properties.overflow-clip-margin.border-box";
      value = "calc(1rem + 2px)";
      why =
        "CSS Values 4 sec. 10.1 admits a math function wherever a <length> is \
         accepted, which CSS Overflow 4 sec. 3.2 is. Measured on Chrome 153: \
         it takes a literal length and refuses every math function there, \
         calc(1px) and min(1px,2px) included, so this is not about the \
         negative a value happens to carry";
    };
    {
      properties = [ "overflow-clip-margin" ];
      key = Some "css.properties.overflow-clip-margin.border-box";
      value = "0";
      why =
        "CSS Overflow 4 sec. 3.2: <visual-box> || <length>, and a unitless \
         zero is a <length>; Chrome takes only a dimension";
    };
    {
      properties = [ "text-align" ];
      key = Some "css.properties.text-align.match-parent";
      value = "match-parent";
      why =
        "CSS Text 4 sec. 7.1 lists match-parent; Chrome ships only \
         -webkit-match-parent";
    };
    {
      properties = [ "text-transform" ];
      key = None;
      value = "full-width";
      why =
        "CSS Text 4 sec. 2.1: none | [ capitalize | uppercase | lowercase ] || \
         full-width || full-size-kana | math-auto";
    };
    {
      properties = [ "background-blend-mode" ];
      (* The fact is {!Cascade.Support.measured}, so the entry names the key and
         carries no prose of its own. *)
      key = Some "css.properties.background-blend-mode.plus-lighter";
      value = "plus-lighter";
      why = library_says "css.properties.background-blend-mode.plus-lighter";
    };
    {
      properties = [ "text-overflow" ];
      key = Some "css.properties.text-overflow.string";
      value = "\"...\"";
      why =
        "CSS Overflow 4 sec. 4.1: [ clip | ellipsis | <string> | fade | \
         <fade()> ]{1,2}";
    };
    {
      properties = [ "text-overflow" ];
      key = Some "css.properties.text-overflow.two_value_syntax";
      value = "clip ellipsis";
      why = "CSS Overflow 4 sec. 4.1: the production repeats {1,2}";
    };
    {
      properties = [ "text-combine-upright" ];
      key = None;
      value = "digits";
      why =
        "CSS Writing Modes 4 sec. 9.1: none | all | [ digits <integer [2,4]>? \
         ]; Chrome has only none and all";
    };
    {
      properties = [ "text-combine-upright" ];
      key = None;
      value = "digits 2";
      why = "CSS Writing Modes 4 sec. 9.1: the integer ranges over [2,4]";
    };
    {
      properties = [ "text-combine-upright" ];
      key = None;
      value = "digits 4";
      why = "CSS Writing Modes 4 sec. 9.1: the integer ranges over [2,4]";
    };
    {
      properties = [ "alignment-baseline" ];
      key = None;
      value = "text-bottom";
      why =
        "CSS Inline 3 sec. 4.2.2: baseline | <baseline-metric>, and \
         <baseline-metric> begins text-bottom | alphabetic | ideographic; \
         Chrome implements the SVG 1.1 keyword set";
    };
    {
      properties = [ "baseline-shift" ];
      key = None;
      value = "top";
      why =
        "CSS Inline 3 sec. 4.2.3: <length-percentage> | sub | super | top | \
         center | bottom";
    };
    {
      properties = [ "baseline-shift" ];
      key = None;
      value = "center";
      why = "CSS Inline 3 sec. 4.2.3 lists center";
    };
    {
      properties = [ "baseline-shift" ];
      key = None;
      value = "bottom";
      why = "CSS Inline 3 sec. 4.2.3 lists bottom";
    };
    {
      properties = [ "grid-template-rows" ];
      key = None;
      value = "masonry";
      why =
        "the CSS Grid 3 Working Draft of 2024 added masonry to \
         grid-template-rows, and Firefox ships it; the current draft has \
         replaced it with display: grid-lanes, so this row is the one entry \
         here that wants a decision rather than a browser";
    };
    {
      properties = [ "outline-color" ];
      key = None;
      value = "auto";
      why =
        "CSS UI 4 sec. 3.4: auto | <'border-top-color'>, and auto is the \
         initial value; Chrome computes that initial value without accepting \
         the keyword";
    };
    {
      properties = [ "user-select"; "-webkit-user-select" ];
      key = None;
      value = "contain";
      why =
        "CSS UI 4 sec. 6.1: auto | text | none | contain | all; \
         -webkit-user-select is the browser's legacy name for the same \
         property";
    };
    {
      properties = [ "font-synthesis" ];
      key = Some "css.properties.font-synthesis.position";
      value = "style small-caps position";
      why =
        "CSS Fonts 4 sec. 2.8.5: none | [ weight || style || small-caps || \
         position ]; Chrome has no font-synthesis-position";
    };
    {
      properties = [ "font-synthesis-style" ];
      key = None;
      value = "oblique-only";
      why = "CSS Fonts 4 sec. 2.8.2: auto | none | oblique-only";
    };
    {
      properties = [ "ruby-position" ];
      key = Some "css.properties.ruby-position.alternate";
      value = "alternate";
      why =
        "CSS Ruby 1 sec. 4.1: [ alternate || [ over | under ] ] | \
         inter-character; Chrome has only over and under";
    };
    {
      properties = [ "ruby-position" ];
      key = Some "css.properties.ruby-position.alternate";
      value = "alternate over";
      why = "CSS Ruby 1 sec. 4.1: alternate combines with over under ||";
    };
    {
      properties = [ "ruby-position" ];
      key = None;
      value = "inter-character";
      why = "CSS Ruby 1 sec. 4.1 lists inter-character";
    };
    {
      properties = [ "image-rendering" ];
      key = Some "css.properties.image-rendering.smooth";
      value = "smooth";
      why =
        "CSS Images 3 sec. 5.2: auto | smooth | high-quality | pixelated | \
         crisp-edges";
    };
    {
      properties = [ "stroke-linejoin" ];
      key = None;
      value = "miter-clip";
      why =
        "SVG Strokes sec. 2.6: miter | miter-clip | round | bevel | arcs; \
         Chrome has miter, round and bevel";
    };
    {
      properties = [ "stroke-linejoin" ];
      key = None;
      value = "arcs";
      why = "SVG Strokes sec. 2.6 lists arcs";
    };
    {
      properties = [ "vector-effect" ];
      key = None;
      value = "non-scaling-size";
      why =
        "SVG 2 sec. 8.13: none | [ non-scaling-stroke | non-scaling-size | \
         non-rotation | fixed-position ]+ [ viewport | screen ]?; Chrome has \
         only non-scaling-stroke";
    };
    {
      properties = [ "vector-effect" ];
      key = None;
      value = "non-rotation";
      why = "SVG 2 sec. 8.13 lists non-rotation";
    };
    {
      properties = [ "vector-effect" ];
      key = None;
      value = "fixed-position";
      why = "SVG 2 sec. 8.13 lists fixed-position";
    };
    {
      properties = [ "vector-effect" ];
      key = None;
      value = "non-scaling-stroke screen";
      why = "SVG 2 sec. 8.13: the effect list is followed by viewport | screen";
    };
    {
      properties = [ "vector-effect" ];
      key = None;
      value = "non-scaling-stroke fixed-position";
      why = "SVG 2 sec. 8.13: the effects themselves repeat with +";
    };
  ]

(* Negatives Chrome accepts. Each is a value no specification grants, kept
   invalid on purpose. *)
(* An unquoted font family is a [<custom-ident>+], and CSS Fonts 4 sec. 2.1.1
   excludes an identifier that "could be misinterpreted as a pre-defined
   keyword ... or the CSS-wide keywords". Measured on Chrome 153: it applies
   that only to a family of ONE identifier ([default] alone is refused), and
   reads a reserved word inside a longer name ([default Arial], [x default],
   [none default] all compute as one quoted family). No specification grants
   that, so the reader is right and Chrome is lenient. *)
let unquoted_family_reserved_word s =
  let words = String.split_on_char ' ' (String.trim s) in
  let reserved =
    [
      "inherit";
      "initial";
      "unset";
      "revert";
      "revert-layer";
      "default";
      "none";
      "currentcolor";
    ]
  in
  (* A generic family is a different exclusion and Chrome does apply that one,
     refusing [system-ui default], so a name holding one is not this shape. *)
  let generic =
    [
      "serif";
      "sans-serif";
      "monospace";
      "cursive";
      "fantasy";
      "system-ui";
      "math";
      "ui-serif";
      "ui-sans-serif";
      "ui-monospace";
      "ui-rounded";
    ]
  in
  List.length words > 1
  && List.exists (fun w -> List.exists (String.equal w) reserved) words
  && not (List.exists (fun w -> List.exists (String.equal w) generic) words)

let lenient : excuse list =
  [
    {
      properties = [ "resize" ];
      key = None;
      value = "auto";
      why =
        "CSS UI 4 sec. 4.1: none | both | horizontal | vertical | block | \
         inline. Chrome accepts auto, no specification defines it";
    };
    {
      properties = [ "text-orientation" ];
      key = None;
      value = "sideways-right";
      why =
        "CSS Writing Modes 4 sec. 5.1: mixed | upright | sideways. \
         sideways-right is a compatibility alias browsers may keep, not \
         grammar";
    };
    {
      properties = [ "alignment-baseline" ];
      key = None;
      value = "auto";
      why =
        "CSS Inline 3 sec. 4.2.2: baseline | <baseline-metric>, and no arm is \
         auto. Chrome accepts it from the SVG 1.1 grammar";
    };
  ]

let unimplemented_property name = List.exists (String.equal name) unimplemented

(* The entry covering [property]: [value], when one exists. *)
(* An entry asserts that Chrome implements the production not at all, so the
   question the dataset answers is whether Chrome has shipped it yet, and no
   version of the running browser is needed to ask it. *)
let chrome_ships key =
  Cascade.Support.engine_implements Cascade.Support.Chrome (max_int, 0) key
  = Some true

type verdict = Browser_behind | Cascade_wrong | Needs_measurement

(* The key is what makes this answerable: #1091 gave an excuse the BCD compat
   key web-features records the production under, and {!Cascade.Support} answers
   from the generated table. Prose cannot be classified, so an entry without a
   key is [Needs_measurement] however convincing its [why] reads. *)
let verdict_of targets (e : excuse) =
  match e.key with
  | None -> Needs_measurement
  | Some key -> (
      match Cascade.Support.implemented targets key with
      | None -> Needs_measurement
      | Some true -> Cascade_wrong
      | Some false -> Browser_behind)

let verdict_name = function
  | Browser_behind -> "cascade right, the browser has not shipped it"
  | Cascade_wrong -> "cascade wrong, every target ships it"
  | Needs_measurement -> "not modelled, needs measuring"

let overtaken table =
  List.filter
    (fun (e : excuse) -> Option.fold ~none:false ~some:chrome_ships e.key)
    table

let find table ~property ~value =
  let covers (e : excuse) =
    String.equal e.value value
    && List.exists (String.equal property) e.properties
  in
  List.find_opt covers table

type shape = {
  shape_properties : string list;
  shape_name : string;
  matches : string -> bool;
  shape_why : string;
}

(* A bare [<number>] in any spelling. The generator writes these from a seeded
   stream, so [-1], [1], [.5], [4] and [1000] are one fact about the browser
   sampled five ways, not five facts. *)
let is_bare_number s =
  let s = String.trim s in
  s <> ""
  &&
  let ok = ref true and digits = ref false in
  String.iteri
    (fun i c ->
      match c with
      | '0' .. '9' -> digits := true
      | '.' -> ()
      | ('-' | '+') when i = 0 -> ()
      | _ -> ok := false)
    s;
  !ok && !digits

let has_comma s = String.contains s ','

(* Splits on whitespace outside a quoted string, so a <string> arm carrying a
   space stays one part. *)
let value_parts s =
  let parts = ref [] and buf = Buffer.create 16 and quote = ref None in
  let flush () =
    if Buffer.length buf > 0 then (
      parts := Buffer.contents buf :: !parts;
      Buffer.clear buf)
  in
  String.iter
    (fun c ->
      match (!quote, c) with
      | Some q, _ when Char.equal c q ->
          quote := None;
          Buffer.add_char buf c
      | Some _, _ -> Buffer.add_char buf c
      | None, ('"' | '\'') ->
          quote := Some c;
          Buffer.add_char buf c
      | None, (' ' | '\t' | '\n' | '\r' | '\012') -> flush ()
      | None, _ -> Buffer.add_char buf c)
    s;
  flush ();
  List.rev !parts

let is_quoted p =
  String.length p >= 2
  && (Char.equal p.[0] '"' || Char.equal p.[0] '\'')
  && Char.equal p.[String.length p - 1] p.[0]

(* Every value CSS Overflow 4 sec. 4.1 grants but the one-value [clip |
   ellipsis] half Chrome implements. *)
let unimplemented_text_overflow s =
  let arm p =
    List.exists (String.equal p) [ "clip"; "ellipsis"; "fade" ]
    || is_quoted p
    || String.starts_with ~prefix:"fade(" p
  in
  match value_parts s with
  | [] | [ "clip" ] | [ "ellipsis" ] -> false
  | parts -> List.length parts <= 2 && List.for_all arm parts

let lenient_family_shape =
  {
    shape_properties = [ "font-family"; "font" ];
    shape_name = "a reserved word inside a longer unquoted family";
    matches = unquoted_family_reserved_word;
    shape_why =
      "CSS Fonts 4 sec. 2.1.1 gives an unquoted family a <custom-ident>+ and \
       excludes an identifier that could be misinterpreted as a pre-defined \
       keyword or a CSS-wide keyword. Chrome applies that to a one-identifier \
       family alone and reads a reserved word inside a longer name";
  }

let lenient_shapes =
  [
    lenient_family_shape;
    {
      (* Measured on Chrome 153: [border: 1px, dashed, red] sets the same twelve
         longhands [border: 1px dashed red] does, so both sides of each comma
         are read and the comma is the separator between them. It is not a list:
         [border: red, red] and [border: dashed, solid] fill one slot twice and
         are refused, as they are without the comma. A leading comma ([border: ,
         red]) and a doubled one ([border: red,,dashed]) are refused. *)
      shape_properties =
        [
          "border";
          "border-block";
          "border-inline";
          "border-block-start";
          "border-block-end";
          "border-inline-start";
          "border-inline-end";
          "border-top";
          "border-right";
          "border-bottom";
          "border-left";
        ];
      shape_name = "a comma in a border shorthand";
      matches = has_comma;
      shape_why =
        "CSS Backgrounds 3 sec. 4.5 and CSS Logical 1 sec. 4.6 build these \
         from a [||] of a width, a style and a colour, and no arm of either is \
         a comma. Chrome reads one between two components as the whitespace \
         separating them, so the declaration sets what the same value without \
         the comma sets";
    };
    {
      shape_properties = [ "baseline-shift" ];
      shape_name = "a bare <number>";
      matches = is_bare_number;
      shape_why =
        "CSS Inline 3 sec. 4.2.3: <length-percentage> | sub | super | top | \
         center | bottom, and no arm is a bare <number>. Chrome reads one as \
         the unitless length SVG presentation attributes take";
    };
  ]

let words s = String.split_on_char ' ' (String.trim s)

(* CSS Animations 2 sec. 4.12 ends <single-animation> in [ none |
   <keyframes-name> ] || <single-animation-timeline>, so a dashed-ident timeline
   and a keyframes name fill two slots of one [||]. Measured on Chrome 153: it
   takes the timeline alone ([animation: --t], [animation: 1s --t]) and refuses
   it beside a name ([spin --t], [flip-block --fallback]), so it took the
   timeline back out of the shorthand rather than never having it. *)
let animation_name_beside_timeline s =
  let ws = List.filter (fun w -> w <> "") (words s) in
  let dashed w = String.length w > 2 && String.sub w 0 2 = "--" in
  let timing w =
    (not (dashed w))
    && (String.contains w 's' || String.contains w '%'
       || String.exists (fun c -> c >= '0' && c <= '9') w)
  in
  List.exists dashed ws
  && List.exists (fun w -> (not (dashed w)) && not (timing w)) ws

(* CSS Values 4 sec. 10.1 puts a math function wherever its type is, and CSS
   Overflow 4 sec. 3.2 gives overflow-clip-margin a <length>. Measured on Chrome
   153: it takes a literal length and refuses every math function there,
   [calc(1px)] and [min(1px,2px)] included, so this is not about the negative
   the value happens to carry. *)
let math_function s =
  List.exists
    (fun fn ->
      String.length s >= String.length fn
      && String.sub s 0 (String.length fn) = fn)
    [ "calc("; "min("; "max("; "clamp(" ]

let spec_ahead_shapes =
  [
    {
      shape_properties = [ "overflow-clip-margin" ];
      shape_name = "a math function in an overflow clip margin";
      matches = math_function;
      shape_why =
        "CSS Values 4 sec. 10.1 puts a math function wherever its type is, and \
         CSS Overflow 4 sec. 3.2 gives the property a <length>. Chrome takes a \
         literal length and refuses every math function there";
    };
    {
      shape_properties = [ "animation"; "-webkit-animation" ];
      shape_name = "a keyframes name beside a dashed-ident timeline";
      matches = animation_name_beside_timeline;
      shape_why =
        "CSS Animations 2 sec. 4.12 ends <single-animation> in [ none | \
         <keyframes-name> ] || <single-animation-timeline>, so the two fill \
         two slots of one [||]. Chrome takes the timeline alone and refuses it \
         beside a name";
    };
    {
      shape_properties = [ "text-overflow" ];
      shape_name = "a text-overflow Chrome does not implement";
      matches = unimplemented_text_overflow;
      shape_why =
        "CSS Overflow 4 sec. 4.1: [ clip | ellipsis | <string> | fade | \
         <fade()> ]{1,2}, and Chrome implements the one-value [ clip | \
         ellipsis ] half of it";
    };
  ]

let shape_covering table ~property ~value =
  let covers (s : shape) =
    List.exists (String.equal property) s.shape_properties && s.matches value
  in
  List.find_opt covers table
