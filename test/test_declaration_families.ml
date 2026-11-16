(** Declaration tests: values per property family, from colours to grid. *)

open Alcotest
open Cascade
open Css.Declaration
open Css_test_helpers
open Declaration_helpers

let colors () =
  (* Same-node spellings the printer keeps (case-fold, hex shorten). The
     cross-node folds - named<->hex, rgb()/hsl()->hex, transparent->#0000 - are
     optimize rewrites (checked via [decl_optimizes_to]). *)
  check_declaration ~expected:"color:red" "color: red";
  check_declaration ~expected:"color:green" "color: green";
  check_declaration ~expected:"color:#0f0" "color: #00ff00";
  check_declaration ~expected:"color:#00f" "color: #0000ff";
  check_declaration ~expected:"color:#fff" "color: #fff";
  check_declaration ~expected:"color:#000" "color: #000";

  (* Cross-node colour folds (named<->hex, rgb/hsl->hex/named, transparent,
     opaque rgb()->named) happen in optimize; pp holds the authored node. *)
  decl_optimizes_to ~held:"color:blue" ~into:"color:#00f" "color: blue";
  decl_optimizes_to ~held:"color:black" ~into:"color:#000" "color: black";
  decl_optimizes_to ~held:"color:white" ~into:"color:#fff" "color: white";
  decl_optimizes_to ~held:"color:transparent" ~into:"color:#0000"
    "color: transparent";
  decl_optimizes_to ~held:"color:#f00" ~into:"color:red" "color: #ff0000";
  decl_optimizes_to ~held:"color:rgb(255 0 0)" ~into:"color:red"
    "color: rgb(255, 0, 0)";
  decl_optimizes_to ~held:"color:rgb(0 255 0)" ~into:"color:#0f0"
    "color: rgb(0, 255, 0)";
  decl_optimizes_to ~held:"color:rgb(255 0 0/.5)" ~into:"color:#ff000080"
    "color: rgba(255, 0, 0, 0.5)";
  decl_optimizes_to ~held:"color:hsl(0 100%50%)" ~into:"color:red"
    "color: hsl(0, 100%, 50%)";
  decl_optimizes_to ~held:"color:hsl(120 100%50%/.5)" ~into:"color:#00ff0080"
    "color: hsla(120, 100%, 50%, 0.5)";
  decl_optimizes_to ~held:"color:hsl(.5turn 50%50%/var(--a))"
    ~into:"color:hsl(180 50%50%/var(--a))"
    "color: hsl(.5turn 50% 50% / var(--a))";

  check_declaration ~expected:"background-color:red" "background-color: red";
  decl_optimizes_to ~held:"border-color:blue" ~into:"border-color:#00f"
    "border-color: blue";
  decl_optimizes_to ~held:"outline-color:#f00" ~into:"outline-color:red"
    "outline-color: #ff0000"

let lengths () =
  (* Pixels *)
  check_declaration ~expected:"width:100px" "width: 100px";
  check_declaration ~expected:"height:50px" "height: 50px";
  check_declaration ~expected:"margin:10px" "margin: 10px";
  check_declaration ~expected:"padding:20px" "padding: 20px";

  (* Percentages *)
  check_declaration ~expected:"width:100%" "width: 100%";
  check_declaration ~expected:"width:0%" "width: 0%";
  check_declaration ~expected:"height:50%" "height: 50%";

  (* Em and rem *)
  check_declaration ~expected:"font-size:1.5em" "font-size: 1.5em";
  check_declaration ~expected:"font-size:2rem" "font-size: 2rem";
  check_declaration ~expected:"margin:1.5rem" "margin: 1.5rem";

  (* Zero. pp holds the authored unit (0px is the shortest <length> spelling of
     zero); stripping it to the unitless 0 changes <length> to <number>, a typed
     rewrite that optimize does. 0% stays a percentage. *)
  check_declaration ~expected:"width:0px" "width: 0px";
  decl_optimizes ~prop:"width" ~into:"0" "0px";
  check_declaration ~expected:"margin:0" "margin: 0";
  check_declaration ~expected:"padding:0" "padding: 0";

  (* Auto keyword *)
  check_declaration ~expected:"margin:auto" "margin: auto";
  check_declaration ~expected:"width:auto" "width: auto";
  check_declaration ~expected:"height:auto" "height: auto";
  (* CSS Logical 1 sec. 4 pairs each logical minimum-size property with its
     physical counterpart and gives the pair one shared computed value, so
     [initial] resolves through min-width / min-height to CSS Sizing 3 sec.
     3.1.2's [auto]. [expected] pins the pp-held form (the keyword is not folded
     at print time); [optimized] pins the normalize fold. *)
  check_declaration ~expected:"min-inline-size:initial"
    ~optimized:"min-inline-size:auto" "min-inline-size: initial";
  check_declaration ~expected:"min-block-size:initial"
    ~optimized:"min-block-size:auto" "min-block-size: initial";

  (* Min/max content *)
  check_declaration ~expected:"width:min-content" "width: min-content";
  check_declaration ~expected:"width:max-content" "width: max-content";
  check_declaration ~expected:"width:fit-content" "width: fit-content";

  (* Viewport units *)
  check_declaration ~expected:"width:100vw" "width: 100vw";
  check_declaration ~expected:"height:100vh" "height: 100vh";
  check_declaration ~expected:"width:50vmin" "width: 50vmin";
  check_declaration ~expected:"height:50vmax" "height: 50vmax"

let display () =
  let c = check_declaration in
  c ~expected:"display:none" "display: none";
  c ~expected:"display:block" "display: block";
  c ~expected:"display:inline" "display: inline";
  c ~expected:"display:inline-block" "display: inline-block";
  c ~expected:"display:flex" "display: flex";
  c ~expected:"display:inline-flex" "display: inline-flex";
  c ~expected:"display:grid" "display: grid";
  c ~expected:"display:inline-grid" "display: inline-grid";
  (* CSS Grid 3 (ED, 2 September 2026) sec. 2.2: "New values: grid-lanes |
     inline-grid-lanes". *)
  c ~expected:"display:grid-lanes" "display: grid-lanes";
  c ~expected:"display:inline-grid-lanes" "display: inline-grid-lanes";
  c ~expected:"display:table" "display: table";
  c ~expected:"display:table-row" "display: table-row";
  c ~expected:"display:table-cell" "display: table-cell";
  c ~expected:"display:list-item" "display: list-item";
  c ~expected:"display:contents" "display: contents";
  c ~expected:"display:flow-root" "display: flow-root";
  (* CSS Display 3 (ED) sec. 2.1: a [<display-outside>] written without an
     inside defaults the inner type to [flow]. sec. 2.2: an inside written alone
     defaults the outer type to [block], except [ruby], which defaults to
     [inline]. sec. 2.6 names the four precomposed inline-level keywords. Each
     two-value form below therefore has a single-keyword spelling of the same
     value, and the keyword is the shorter one; swapping them is a node change,
     so pp prints what it parsed and the optimizer folds. *)
  c ~expected:"display:block flow" ~optimized:"display:block"
    "display: block flow";
  c ~expected:"display:inline flow" ~optimized:"display:inline"
    "display: inline flow";
  c ~expected:"display:run-in flow" ~optimized:"display:run-in"
    "display: run-in flow";
  c ~expected:"display:block flow-root" ~optimized:"display:flow-root"
    "display: block flow-root";
  c ~expected:"display:inline flow-root" ~optimized:"display:inline-block"
    "display: inline flow-root";
  c ~expected:"display:block flex" ~optimized:"display:flex"
    "display: block flex";
  c ~expected:"display:inline flex" ~optimized:"display:inline-flex"
    "display: inline flex";
  c ~expected:"display:block grid" ~optimized:"display:grid"
    "display: block grid";
  c ~expected:"display:inline grid" ~optimized:"display:inline-grid"
    "display: inline grid";
  c ~expected:"display:block table" ~optimized:"display:table"
    "display: block table";
  c ~expected:"display:inline table" ~optimized:"display:inline-table"
    "display: inline table";
  c ~expected:"display:inline ruby" ~optimized:"display:ruby"
    "display: inline ruby";
  (* sec. 2.2's ruby exception cuts the other way too: the bare [ruby] keyword
     means [inline ruby], so [block ruby] is the one two-value form with no
     shorter spelling and both layers hold it. *)
  c ~expected:"display:block ruby" ~optimized:"display:block ruby"
    "display: block ruby";
  (* sec. 2.3: a [list-item] with no inside takes [flow] and with no outside
     takes [block], so [block flow list-item] is what [list-item] alone says.
     Dropping the [flow] alone names the same value either way, which is why pp
     may do it and the outer keyword has to wait for the optimizer. *)
  c ~expected:"display:block list-item" ~optimized:"display:list-item"
    "display: block flow list-item";
  c ~expected:"display:inline list-item" ~optimized:"display:inline list-item"
    "display: inline flow list-item";
  (* sec. 2 orders none of the three: [<display-outside>? && [ flow | flow-root
     ]? && list-item] reads the inside keyword on either side of the
     [list-item]. sec. 2.3 defaults the unwritten inside to [flow] and the
     unwritten outside to [block], so [flow-root list-item] is what [block
     flow-root list-item] says and pp writes the shorter one. *)
  c ~expected:"display:flow-root list-item"
    ~optimized:"display:flow-root list-item" "display: flow-root list-item";
  c ~expected:"display:flow-root list-item"
    ~optimized:"display:flow-root list-item" "display: list-item flow-root";
  c ~expected:"display:flow-root list-item"
    ~optimized:"display:flow-root list-item"
    "display: block flow-root list-item";
  c ~expected:"display:block list-item" ~optimized:"display:list-item"
    "display: flow list-item";
  c ~expected:"display:block list-item" ~optimized:"display:list-item"
    "display: list-item flow"

let position () =
  let c = check_declaration in
  c ~expected:"position:static" "position: static";
  c ~expected:"position:relative" "position: relative";
  c ~expected:"position:absolute" "position: absolute";
  c ~expected:"position:fixed" "position: fixed";
  c ~expected:"position:sticky" "position: sticky"

let font_properties () =
  (* CSS Fonts 4 (ED) sec. 2.2 defines [normal] as "Same as 400" and [bold] as
     "Same as 700", so each keyword and its number are one value with two
     spellings, and the number is the shorter one. Swapping them is a node
     change, so pp holds what it was handed and the optimizer folds. *)
  check_declaration ~expected:"font-weight:normal" ~optimized:"font-weight:400"
    "font-weight: normal";
  check_declaration ~expected:"font-weight:bold" ~optimized:"font-weight:700"
    "font-weight: bold";
  (* sec. 2.7 gives the [font] shorthand a [<'font-weight'>] slot, so the slot
     takes the longhand's fold. *)
  check_declaration ~expected:"font:bold 12px serif"
    ~optimized:"font:700 12px serif" "font: bold 12px serif";
  check_declaration ~expected:"font-weight:lighter" "font-weight: lighter";
  check_declaration ~expected:"font-weight:bolder" "font-weight: bolder";
  check_declaration ~expected:"font-weight:100" "font-weight: 100";
  check_declaration ~expected:"font-weight:400" "font-weight: 400";
  check_declaration ~expected:"font-weight:700" "font-weight: 700";
  check_declaration ~expected:"font-weight:900" "font-weight: 900";

  (* CSS Fonts 4 (ED) sec. 2.3 maps every [font-width] keyword onto a percentage
     ([condensed] is 75%, [normal] is 100%) and has getComputedStyle() serialize
     the property as a percentage whichever spelling was authored, so keyword
     and percentage are one value and the percentage is the shorter one.
     Swapping them is a node change, so pp holds what it parsed and the
     optimizer folds. *)
  check_declaration ~expected:"font-stretch:condensed"
    ~optimized:"font-stretch:75%" "font-stretch: condensed";
  check_declaration ~expected:"font-stretch:normal"
    ~optimized:"font-stretch:100%" "font-stretch: normal";
  check_declaration ~expected:"font-stretch:semi-expanded"
    ~optimized:"font-stretch:112.5%" "font-stretch: semi-expanded";
  check_declaration ~expected:"font-stretch:ultra-expanded"
    ~optimized:"font-stretch:200%" "font-stretch: ultra-expanded";
  check_declaration ~expected:"font-stretch:75%" ~optimized:"font-stretch:75%"
    "font-stretch: 75%";
  (* sec. 2.7 gives the [font] shorthand's width slot the grammar
     [<font-width-css3>], which is the keywords alone: the percentage the
     longhand folds to is not a value the slot accepts, so neither layer folds
     it here. *)
  check_declaration ~expected:"font:condensed 12px serif"
    ~optimized:"font:condensed 12px serif" "font: condensed 12px serif";

  (* CSS Values 4 (ED) sec. 10.10.1 simplifies a Sum by replacing each set of
     children with identical units by their sum, and returns the lone remaining
     child, so a same-unit [font-size] calc folds the way a [width] one does.
     [em] and [%] resolve against the parent font size here rather than the
     element's own, but both terms of one declaration share that reference, so
     the sum is the same value whatever it turns out to be. *)
  check_declaration ~expected:"font-size:calc(1px + 1px)"
    ~optimized:"font-size:2px" "font-size: calc(1px + 1px)";
  check_declaration ~expected:"font-size:calc(1em + 1em)"
    ~optimized:"font-size:2em" "font-size: calc(1em + 1em)";
  check_declaration ~expected:"font-size:calc(50% + 50%)"
    ~optimized:"font-size:100%" "font-size: calc(50% + 50%)";
  check_declaration ~expected:"font-size:calc(2*3px)" ~optimized:"font-size:6px"
    "font-size: calc(2 * 3px)";
  (* Terms of different units carry no conversion factor until layout, so the
     Sum keeps both children and both layers hold the call. *)
  check_declaration ~expected:"font-size:calc(1em + 1px)"
    ~optimized:"font-size:calc(1em + 1px)" "font-size: calc(1em + 1px)";
  (* sec. 2.7 of CSS Fonts 4 gives the [font] shorthand a [<'font-size'>] slot,
     so the slot takes the longhand's fold. *)
  check_declaration ~expected:"font:calc(1px + 1px) serif"
    ~optimized:"font:2px serif" "font: calc(1px + 1px) serif";

  (* sec. 2.7 first resets every subproperty of [font] to its initial value and
     then applies the slots given explicitly, so a slot carrying its longhand's
     initial says exactly what leaving it out says. The initials are [normal]
     for style, variant and width, [normal] (that is 400) for weight, and
     [normal] for line-height. Dropping a slot is a node change, so pp holds
     every slot it parsed and the optimizer drops. *)
  check_declaration ~expected:"font:400 12px serif" ~optimized:"font:12px serif"
    "font: 400 12px serif";
  check_declaration ~expected:"font:12px/normal serif"
    ~optimized:"font:12px serif" "font: 12px/normal serif";
  check_declaration ~expected:"font:small-caps 400 12px/normal serif"
    ~optimized:"font:small-caps 12px serif"
    "font: normal small-caps 400 normal 12px/normal serif";
  (* A bare [normal] in the prefix names the initial of whichever of the four
     slots is still open, so the reader binds no slot for it and the two layers
     agree from the start. *)
  check_declaration ~expected:"font:12px serif" ~optimized:"font:12px serif"
    "font: normal 12px serif";
  (* A slot that is not its longhand's initial stays in both layers. *)
  check_declaration ~expected:"font:italic 12px serif"
    ~optimized:"font:italic 12px serif" "font: italic 12px serif";
  check_declaration ~expected:"font:12px/1.5 serif"
    ~optimized:"font:12px/1.5 serif" "font: 12px/1.5 serif";

  (* Font style *)
  check_declaration ~expected:"font-style:normal" "font-style: normal";
  check_declaration ~expected:"font-style:italic" "font-style: italic";
  check_declaration ~expected:"font-style:oblique" "font-style: oblique";
  (* CSS Values 4 sec. 6 drops the unit on a zero <length> and on nothing else.
     A slot taking a bare zero angle says so, as Transforms 1 sec. 11 and Filter
     Effects 1 sec. 8 do with [<angle> | <zero>]; CSS Fonts 4 sec. 2.4 writes
     [oblique <angle [-90deg,90deg]>?] and grants no zero, so the unit is
     required here. Chrome 153 drops the bare zero and keeps the other three. *)
  neg_cursor read_declaration "font-style: oblique 0";
  neg_cursor read_declaration "font-style: oblique 0 10deg";
  check_declaration ~expected:"font-style:oblique 0deg"
    "font-style: oblique 0deg";
  check_declaration ~expected:"transform:rotate(0deg)" "transform: rotate(0)";
  (* Sec. 8 makes [hue-rotate()]'s argument optional and zero by default, so the
     shortest spelling of a zero rotation is the empty call, which Chrome reads
     back as the same filter. *)
  check_declaration ~expected:"filter:hue-rotate()" "filter: hue-rotate(0)";

  (* Font family - list type *)
  check_declaration ~expected:"font-family:Arial" "font-family: Arial";
  check_declaration
    ~expected:"font-family:Helvetica Neue,Helvetica,Arial,sans-serif"
    "font-family: \"Helvetica Neue\", Helvetica, Arial, sans-serif";
  check_declaration ~expected:"font-family:Georgia,serif"
    "font-family: Georgia, serif";
  (* CSS Fonts 4 (ED) sec. 2.1 has the user agent iterate the list until a
     family matches, so a repeat of an earlier entry is unreachable and names
     the same value as the list without it. Dropping it is a node change, so pp
     holds the list it parsed and the optimizer folds. A one-entry list is the
     entry, so the fold has to land on the node [font-family: Arial] parses to.
     The exception is a list that would collapse to a lone generic keyword:
     [monospace, monospace] opts a bare generic back into the normal UA size, so
     dropping the duplicate would shrink the text. *)
  check_declaration ~expected:"font-family:Arial,Helvetica,Arial"
    ~optimized:"font-family:Arial,Helvetica"
    "font-family: Arial, Helvetica, Arial";
  check_declaration ~expected:"font-family:Arial,Arial"
    ~optimized:"font-family:Arial" "font-family: Arial, Arial";
  check_declaration ~expected:"font-family:monospace,monospace"
    ~optimized:"font-family:monospace,monospace"
    "font-family: monospace, monospace";
  check_declaration ~expected:"font-family:serif,serif"
    ~optimized:"font-family:serif,serif" "font-family: serif, serif";
  (* sec. 2.7 gives the [font] shorthand a [<'font-family'>#] slot, so the slot
     takes the longhand's fold. *)
  check_declaration ~expected:"font:12px Arial,Arial"
    ~optimized:"font:12px Arial" "font: 12px Arial, Arial";
  (* CSS Fonts 4 defines font-family names as <custom-ident> sequences. Quoted
     reserved words are family names; unquoted CSS-wide keywords remain CSS-wide
     keywords and must not be reinterpreted as family names. *)
  check_declaration ~expected:"font-family:\"default\"" "font-family: 'default'";
  check_declaration ~expected:"font-family:\"revert\"" "font-family: 'revert'";
  check_declaration ~expected:"font-family:revert" "font-family: revert";
  check_declaration ~expected:"font-family:revert-layer"
    "font-family: revert-layer";
  (* Sec. 2.1.1 keeps a [<generic-font-family>] out of [<font-family-name>], and
     the property reads the generic only where that alternative starts, so one
     is turned away as the first word of a sequence and reads as an ordinary
     [<custom-ident>] after it. Chrome 153 takes each of these as a name, and
     they are installed fonts, so dropping them changes what a page renders. *)
  check_declaration ~expected:"font-family:\"Cambria Math\""
    "font-family: Cambria Math";
  check_declaration ~expected:"font-family:\"Cambria math\""
    "font-family: Cambria math";
  check_declaration ~expected:"font-family:\"Foo serif\""
    "font-family: Foo serif";
  check_declaration ~expected:"font-family:\"system-ui system-ui\""
    "font-family: 'system-ui system-ui'";
  check_declaration ~expected:"font-family:My Font" "font-family: My Font";
  check_declaration ~expected:"font-family:Noto Color Emoji"
    "font-family: Noto Color Emoji";
  check_declaration ~expected:"font-family:system-ui" "font-family: system-ui";
  check_declaration ~expected:"font-family:math" "font-family: Math";

  (* Line height *)
  check_declaration ~expected:"line-height:1.5" "line-height: 1.5";
  check_declaration ~expected:"line-height:2" "line-height: 2";
  check_declaration ~expected:"line-height:normal" "line-height: normal";
  check_declaration ~expected:"line-height:20px" "line-height: 20px";
  check_declaration ~expected:"line-height:1.5em" "line-height: 1.5em"

let text_properties () =
  (* Text align *)
  check_declaration ~expected:"text-align:left" "text-align: left";
  check_declaration ~expected:"text-align:right" "text-align: right";
  check_declaration ~expected:"text-align:center" "text-align: center";
  check_declaration ~expected:"text-align:justify" "text-align: justify";
  check_declaration ~expected:"text-align:start" "text-align: start";
  check_declaration ~expected:"text-align:end" "text-align: end";

  (* Text decoration *)
  check_declaration ~expected:"text-decoration:none" "text-decoration: none";
  check_declaration ~expected:"text-decoration:underline"
    "text-decoration: underline";
  check_declaration ~expected:"text-decoration:overline"
    "text-decoration: overline";
  check_declaration ~expected:"text-decoration:line-through"
    "text-decoration: line-through";
  (* CSS Text Decoration 4 (ED) sec. 2.6: the [text-decoration] shorthand sets
     the line, thickness, style and colour longhands, and "Omitted values are
     set to their initial values" - [solid] (sec. 2.2) and [currentcolor] (sec.
     2.3). Writing one out names what leaving it out names, and dropping it is a
     node change, so pp holds it and the optimizer folds. *)
  check_declaration ~expected:"text-decoration:underline solid"
    ~optimized:"text-decoration:underline" "text-decoration: underline solid";
  check_declaration ~expected:"text-decoration:underline currentColor"
    ~optimized:"text-decoration:underline"
    "text-decoration: underline currentcolor";
  check_declaration ~expected:"text-decoration:underline solid currentColor"
    ~optimized:"text-decoration:underline"
    "text-decoration: underline solid currentcolor";
  (* A non-initial style or colour stands. *)
  check_declaration ~expected:"text-decoration:underline dotted"
    ~optimized:"text-decoration:underline dotted"
    "text-decoration: underline dotted";
  check_declaration ~expected:"text-decoration:underline red"
    ~optimized:"text-decoration:underline red" "text-decoration: underline red";

  (* Text transform *)
  check_declaration ~expected:"text-transform:none" "text-transform: none";
  check_declaration ~expected:"text-transform:uppercase"
    "text-transform: uppercase";
  check_declaration ~expected:"text-transform:lowercase"
    "text-transform: lowercase";
  check_declaration ~expected:"text-transform:capitalize"
    "text-transform: capitalize";

  (* White space *)
  check_declaration ~expected:"white-space:normal" "white-space: normal";
  check_declaration ~expected:"white-space:nowrap" "white-space: nowrap";
  check_declaration ~expected:"white-space:pre" "white-space: pre";
  check_declaration ~expected:"white-space:pre-wrap" "white-space: pre-wrap";
  check_declaration ~expected:"white-space:pre-line" "white-space: pre-line";
  check_declaration ~expected:"white-space:break-spaces"
    "white-space: break-spaces";

  (* CSS Text 4 sec. 5.5: [text-wrap] is [<'text-wrap-mode'> ||
     <'text-wrap-style'>], with sec. 5.1 giving the mode [wrap | nowrap] and
     sec. 5.4 the style [auto | balance | stable | pretty]. Either component may
     be omitted and they may appear in either order, but neither may repeat. *)
  check_declaration ~expected:"text-wrap:wrap" "text-wrap: wrap";
  check_declaration ~expected:"text-wrap:nowrap" "text-wrap: nowrap";
  check_declaration ~expected:"text-wrap:auto" "text-wrap: auto";
  check_declaration ~expected:"text-wrap:balance" "text-wrap: balance";
  check_declaration ~expected:"text-wrap:pretty" "text-wrap: pretty";
  check_declaration ~expected:"text-wrap:stable" "text-wrap: stable";
  check_declaration ~expected:"text-wrap:nowrap balance"
    "text-wrap: nowrap balance";
  (* The two components print mode-first whichever order they arrived in, which
     is how Chrome serialises them too. *)
  check_declaration ~expected:"text-wrap:nowrap balance"
    "text-wrap: balance nowrap";
  check_declaration ~expected:"text-wrap:wrap auto" "text-wrap: wrap auto";
  neg_cursor read_declaration "text-wrap: wrap nowrap";
  neg_cursor read_declaration "text-wrap: wrap wrap";
  neg_cursor read_declaration "text-wrap: balance pretty";
  neg_cursor read_declaration "text-wrap: wrap balance pretty"

let flexbox_direction () =
  (* Flex direction *)
  check_declaration ~expected:"flex-direction:row" "flex-direction: row";
  check_declaration ~expected:"flex-direction:row-reverse"
    "flex-direction: row-reverse";
  check_declaration ~expected:"flex-direction:column" "flex-direction: column";
  check_declaration ~expected:"flex-direction:column-reverse"
    "flex-direction: column-reverse"

let flexbox_wrap () =
  check_declaration ~expected:"flex-wrap:nowrap" "flex-wrap: nowrap";
  check_declaration ~expected:"flex-wrap:wrap" "flex-wrap: wrap";
  check_declaration ~expected:"flex-wrap:wrap-reverse" "flex-wrap: wrap-reverse";
  (* CSS Flexbox 2 sec. 5.2: nowrap | [ wrap | wrap-reverse ] || balance, so the
     pair reads either way round and [balance] alone carries [wrap]. *)
  check_declaration ~expected:"flex-wrap:balance" "flex-wrap: balance";
  check_declaration ~expected:"flex-wrap:balance" "flex-wrap: wrap balance";
  check_declaration ~expected:"flex-wrap:wrap-reverse balance"
    "flex-wrap: balance wrap-reverse";
  check_declaration ~expected:"flex-flow:row balance" "flex-flow: row balance";
  neg_cursor read_declaration "flex-wrap: nowrap balance";
  neg_cursor read_declaration "flex-wrap: balance balance"

let flexbox_flex_and_basis () =
  (* Flex shorthand *)
  check_declaration ~expected:"flex:1" "flex: 1";
  check_declaration ~expected:"flex:1 auto" "flex: 1 1 auto";
  check_declaration ~expected:"flex:0 auto" "flex: 0 1 auto";
  check_declaration ~expected:"flex:initial" "flex: initial";
  check_declaration ~expected:"flex:none" "flex: none";
  check_declaration ~expected:"flex:auto" "flex: auto";
  (* Flex grow/shrink *)
  check_declaration ~expected:"flex-grow:0" "flex-grow: 0";
  check_declaration ~expected:"flex-grow:1" "flex-grow: 1";
  check_declaration ~expected:"flex-grow:2" "flex-grow: 2";
  check_declaration ~expected:"flex-shrink:0" "flex-shrink: 0";
  check_declaration ~expected:"flex-shrink:1" "flex-shrink: 1";
  (* Flex basis *)
  check_declaration ~expected:"flex-basis:auto" "flex-basis: auto";
  check_declaration ~expected:"flex-basis:0px" "flex-basis: 0px";
  (* CSS Flexbox 1 sec. 7.2 takes a <'width'>, which is every length the
     document can write, and cascade keeps the authored spelling of the ones no
     typed constructor carries. *)
  check_declaration ~expected:"flex-basis:1000px" "flex-basis: 1e3px";
  check_declaration ~expected:"flex-basis:10px" "flex-basis: 10.0px";
  check_declaration ~expected:"flex:3.40282e38px" "flex: 1e999px";
  decl_optimizes ~prop:"flex-basis" ~into:"0" "0px";
  (* CSS Values 4 sec. 10.10.1: a static product over a percentage folds as it
     does in [width], so [calc(.5 * 100%)] is [50%]. Tailwind writes [basis-1/2]
     with the division and lightningcss with the percentage. *)
  decl_optimizes ~prop:"flex-basis" ~into:"50%" "calc(.5 * 100%)";
  decl_optimizes ~prop:"flex-basis" ~into:"50%" "calc(1 / 2 * 100%)";
  check_declaration ~expected:"flex-basis:0%" "flex-basis: 0%";
  check_declaration ~expected:"flex-basis:100px" "flex-basis: 100px";
  check_declaration ~expected:"flex-basis:50%" "flex-basis: 50%";
  (* Canonical form for [flex-basis: calc(<var> * <number>)] - pins the output
     that a typed-constructor path must produce. When [Calc.float : float -> 'a
     calc] is generalised (CSS Values L4 sec. 10: a bare <number> is
     dimensionally neutral and unifies with any typed calc context), this is
     what the typed flex-basis Calc emitter has to round-trip to. *)
  check_declaration ~expected:"flex-basis:calc(var(--spacing)*4)"
    "flex-basis: calc(var(--spacing) * 4)";
  (* Math functions over <length-percentage> are valid flex-basis values and
     round-trip; sign() yields a <number> and is rejected, unless a var() in it
     defers the whole grammar to computed-value time (CSS Variables 1 sec.
     3). *)
  check_declaration ~expected:"flex-basis:min(10px,100%)"
    "flex-basis: min(10px, 100%)";
  check_declaration ~expected:"flex-basis:clamp(1px,var(--spacing),100%)"
    "flex-basis: clamp(1px, var(--spacing), 100%)";
  check_declaration ~expected:"flex-basis:abs(var(--x))"
    "flex-basis: abs(var(--x))";
  check_declaration ~expected:"flex-basis:sign(var(--x))"
    "flex-basis: sign(var(--x))";
  neg_cursor read_declaration "flex-basis: sign(1px)";
  (* order: a calc carrying a var stays a typed calc and round-trips; an
     all-numeric order calc folds to an integer. *)
  check_declaration ~expected:"order:2" "order: 2";
  check_declaration ~expected:"order:calc(var(--o)*-1)"
    "order: calc(var(--o) * -1)";
  check_declaration ~expected:"order:6" "order: calc(3 * 2)"

let flexbox_alignment () =
  (* Align items *)
  check_declaration ~expected:"align-items:stretch" "align-items: stretch";
  check_declaration ~expected:"align-items:flex-start" "align-items: flex-start";
  check_declaration ~expected:"align-items:flex-end" "align-items: flex-end";
  check_declaration ~expected:"align-items:center" "align-items: center";
  check_declaration ~expected:"align-items:baseline" "align-items: baseline";
  (* Justify content *)
  check_declaration ~expected:"justify-content:flex-start"
    "justify-content: flex-start";
  check_declaration ~expected:"justify-content:flex-end"
    "justify-content: flex-end";
  check_declaration ~expected:"justify-content:center" "justify-content: center";
  check_declaration ~expected:"justify-content:space-between"
    "justify-content: space-between";
  check_declaration ~expected:"justify-content:space-around"
    "justify-content: space-around";
  check_declaration ~expected:"justify-content:space-evenly"
    "justify-content: space-evenly";
  (* CSS Box Alignment 3 sec. 4.2 keeps [left] and [right] out of
     [<content-position>], so only the inline axis takes them, and sec. 4.4
     pairs an overflow keyword with either. *)
  check_declaration ~expected:"justify-content:safe left"
    "justify-content: safe left";
  check_declaration ~expected:"justify-content:safe right"
    "justify-content: safe right";
  neg_cursor read_declaration "align-content: right";
  neg_cursor read_declaration "align-content: unsafe right";
  neg_cursor read_declaration "place-content: unsafe right"

let borders () =
  (* Border style *)
  check_declaration ~expected:"border-style:none" "border-style: none";
  check_declaration ~expected:"border-style:solid" "border-style: solid";
  check_declaration ~expected:"border-style:dashed" "border-style: dashed";
  check_declaration ~expected:"border-style:dotted" "border-style: dotted";
  check_declaration ~expected:"border-style:double" "border-style: double";
  check_declaration ~expected:"border-style:groove" "border-style: groove";
  check_declaration ~expected:"border-style:ridge" "border-style: ridge";
  check_declaration ~expected:"border-style:inset" "border-style: inset";
  check_declaration ~expected:"border-style:outset" "border-style: outset";

  (* Border width *)
  check_declaration ~expected:"border-width:1px" "border-width: 1px";
  check_declaration ~expected:"border-width:.0625rem" "border-width: .0625rem";
  check_declaration ~expected:"border-width:2px" "border-width: 2px";
  check_declaration ~expected:"border-width:thin" "border-width: thin";
  check_declaration ~expected:"border-width:medium" "border-width: medium";
  check_declaration ~expected:"border-width:thick" "border-width: thick";

  (* Border radius *)
  check_declaration ~expected:"border-radius:0" "border-radius: 0";
  check_declaration ~expected:"border-radius:5px" "border-radius: 5px";
  check_declaration ~expected:"border-radius:50%" "border-radius: 50%";
  check_declaration ~expected:"border-radius:10px" "border-radius: 10px";

  (* Individual borders *)
  check_declaration ~expected:"border-top-style:solid" "border-top-style: solid";
  check_declaration ~expected:"border-right-style:dashed"
    "border-right-style: dashed";
  check_declaration ~expected:"border-bottom-style:dotted"
    "border-bottom-style: dotted";
  check_declaration ~expected:"border-left-style:double"
    "border-left-style: double";

  check_declaration ~expected:"border-top-width:1px" "border-top-width: 1px";
  check_declaration ~expected:"border-right-width:2px" "border-right-width: 2px";
  check_declaration ~expected:"border-bottom-width:3px"
    "border-bottom-width: 3px";
  check_declaration ~expected:"border-left-width:4px" "border-left-width: 4px";
  check_declaration ~expected:"border:.0625rem solid#333"
    "border: .0625rem solid #333";

  check_declaration ~expected:"border-top-color:red" "border-top-color: red";
  check_declaration ~expected:"border-right-color:blue"
    ~optimized:"border-right-color:#00f" "border-right-color: blue";
  check_declaration ~expected:"border-bottom-color:green"
    "border-bottom-color: green";
  check_declaration ~expected:"border-left-color:yellow"
    ~optimized:"border-left-color:#ff0" "border-left-color: yellow"

let overflow () =
  check_declaration ~expected:"overflow:visible" "overflow: visible";
  check_declaration ~expected:"overflow:visible!important"
    "overflow: visible !important";
  check_declaration ~expected:"overflow:hidden" "overflow: hidden";
  check_declaration ~expected:"overflow:scroll" "overflow: scroll";
  check_declaration ~expected:"overflow:auto" "overflow: auto";
  check_declaration ~expected:"overflow:clip" "overflow: clip";
  (* CSS Overflow 3 (ED) sec. 3.1: [overflow] is [<'overflow-block'>{1,2}] and
     "sets the specified values of overflow-x and overflow-y in that order. If
     the second value is omitted, it is copied from the first." A pair of equal
     values therefore says what the single value says, and dropping the second
     is a node change, so pp holds the pair and the optimizer folds. *)
  check_declaration ~expected:"overflow:auto auto" ~optimized:"overflow:auto"
    "overflow: auto auto";
  check_declaration ~expected:"overflow:hidden hidden"
    ~optimized:"overflow:hidden" "overflow: hidden hidden";
  (* Two different values name two axes, so the pair stands. *)
  check_declaration ~expected:"overflow:visible hidden"
    ~optimized:"overflow:visible hidden" "overflow: visible hidden";

  check_declaration ~expected:"overflow-x:visible" "overflow-x: visible";
  check_declaration ~expected:"overflow-x:hidden" "overflow-x: hidden";
  check_declaration ~expected:"overflow-x:scroll" "overflow-x: scroll";
  check_declaration ~expected:"overflow-x:auto" "overflow-x: auto";

  check_declaration ~expected:"overflow-y:visible" "overflow-y: visible";
  check_declaration ~expected:"overflow-y:hidden" "overflow-y: hidden";
  check_declaration ~expected:"overflow-y:scroll" "overflow-y: scroll";
  check_declaration ~expected:"overflow-y:auto" "overflow-y: auto"

let animations_timing () =
  (* Animation properties *)
  check_declaration ~expected:"animation-name:slide-in"
    "animation-name: slide-in";
  check_declaration ~expected:"animation-name:none" "animation-name: none";
  (* CSS Animations 1 sec. 3: a <keyframes-name> is a <custom-ident> or a
     <string>, and only the ident arm excludes none, default and the CSS-wide
     keywords. Unquoting such a name is a different declaration, and in a list
     it is no declaration at all. *)
  check_declaration ~expected:"animation-name:\"unset\""
    "animation-name: \"unset\"";
  check_declaration ~expected:"animation-name:\"none\""
    "animation-name: \"none\"";
  check_declaration ~expected:"animation-name:\"default\""
    "animation-name: \"default\"";
  check_declaration ~expected:"animation-name:\"UNSET\""
    "animation-name: \"UNSET\"";
  check_declaration ~expected:"animation-name:\"unset\",\"revert\""
    "animation-name: \"unset\", \"revert\"";
  check_declaration ~expected:"animation-name:slide" "animation-name: \"slide\"";
  check_declaration ~expected:"animation-name:a,b"
    "animation-name: \"a\", \"b\"";

  check_declaration ~expected:"animation-duration:1s" "animation-duration: 1s";
  check_declaration ~expected:"animation-duration:.5s"
    ~optimized:"animation-duration:.5s" "animation-duration: 500ms";
  check_declaration ~expected:"transition-duration:round(1.1s,.5s)"
    ~optimized:"transition-duration:1s" "transition-duration: round(1.1s, .5s)";
  check_declaration ~expected:"animation-duration:2.5s"
    "animation-duration: 2.5s";
  (* CSS Animations 2 sec. 4.1: [ auto | <time [0s,inf]> ]#, with auto the
     initial value. The time arm keeps its lower bound, and the delays and
     transition-duration have no auto arm at all. *)
  check_declaration ~expected:"animation-duration:auto"
    "animation-duration: auto";
  check_declaration ~expected:"animation-duration:auto,1s"
    "animation-duration: auto, 1s";
  neg_cursor read_declaration "animation-duration: -1s";
  neg_cursor read_declaration "transition-duration: auto";
  neg_cursor read_declaration "animation-delay: auto";
  neg_cursor read_declaration "transition-delay: auto";

  check_declaration ~expected:"animation-timing-function:ease"
    "animation-timing-function: ease";
  check_declaration ~expected:"animation-timing-function:ease-in"
    "animation-timing-function: ease-in";
  check_declaration ~expected:"animation-timing-function:ease-out"
    "animation-timing-function: ease-out";
  check_declaration ~expected:"animation-timing-function:ease-in-out"
    "animation-timing-function: ease-in-out";
  check_declaration ~expected:"animation-timing-function:linear"
    "animation-timing-function: linear";
  check_declaration
    ~expected:"animation-timing-function:cubic-bezier(.4,0,.2,1)"
    "animation-timing-function: cubic-bezier(0.4, 0, 0.2, 1)";
  (* CSS Easing 1 (ED) sec. 2.2 gives each cubic bezier keyword its equivalent
     curve, and sec. 2.3 computes [step-start] to [steps(1, start)] and
     [step-end] to [steps(1, end)], with [start] behaving as [jump-start], [end]
     as [jump-end], and an omitted step position assumed to be [end]. Every pair
     below is one easing under two spellings and the keyword is the shorter one,
     so pp prints what it parsed and the optimizer folds. *)
  check_declaration
    ~expected:"animation-timing-function:cubic-bezier(.25,.1,.25,1)"
    ~optimized:"animation-timing-function:ease"
    "animation-timing-function: cubic-bezier(0.25, 0.1, 0.25, 1)";
  check_declaration
    ~expected:"animation-timing-function:cubic-bezier(.42,0,1,1)"
    ~optimized:"animation-timing-function:ease-in"
    "animation-timing-function: cubic-bezier(0.42, 0, 1, 1)";
  check_declaration
    ~expected:"animation-timing-function:cubic-bezier(0,0,.58,1)"
    ~optimized:"animation-timing-function:ease-out"
    "animation-timing-function: cubic-bezier(0, 0, 0.58, 1)";
  check_declaration
    ~expected:"animation-timing-function:cubic-bezier(.42,0,.58,1)"
    ~optimized:"animation-timing-function:ease-in-out"
    "animation-timing-function: cubic-bezier(0.42, 0, 0.58, 1)";
  check_declaration ~expected:"animation-timing-function:cubic-bezier(0,0,1,1)"
    ~optimized:"animation-timing-function:linear"
    "animation-timing-function: cubic-bezier(0, 0, 1, 1)";
  check_declaration ~expected:"transition-timing-function:steps(1,jump-start)"
    ~optimized:"transition-timing-function:step-start"
    "transition-timing-function: steps(1, jump-start)";
  check_declaration ~expected:"transition-timing-function:steps(1,start)"
    ~optimized:"transition-timing-function:step-start"
    "transition-timing-function: steps(1, start)";
  check_declaration ~expected:"transition-timing-function:steps(1,jump-end)"
    ~optimized:"transition-timing-function:step-end"
    "transition-timing-function: steps(1, jump-end)";
  check_declaration ~expected:"transition-timing-function:steps(1)"
    ~optimized:"transition-timing-function:step-end"
    "transition-timing-function: steps(1)";
  (* [jump-none] and [jump-both] have no keyword spelling, and neither does a
     step count above one. *)
  check_declaration ~expected:"transition-timing-function:steps(1,jump-both)"
    ~optimized:"transition-timing-function:steps(1,jump-both)"
    "transition-timing-function: steps(1, jump-both)";
  check_declaration ~expected:"transition-timing-function:steps(2,jump-start)"
    ~optimized:"transition-timing-function:steps(2,jump-start)"
    "transition-timing-function: steps(2, jump-start)";

  (* Per CSS Values 4 section 7.2 the time unit ([s] or [ms]) is required for
     [<time>]. [0s] does not drop the unit. *)
  check_declaration ~expected:"animation-delay:0s" "animation-delay: 0s";
  check_declaration ~expected:"animation-delay:1s" "animation-delay: 1s";
  check_declaration ~expected:"animation-delay:-.5s"
    ~optimized:"animation-delay:-.5s" "animation-delay: -500ms";
  (* CSS Values 4 sec. 10.3 gives these functions the type of their arguments,
     so time-valued calls fit the delay longhands' [<time>] grammar. *)
  check_declaration ~expected:"transition-delay:round(1.1s,.5s)"
    ~optimized:"transition-delay:1s" "transition-delay:round(1.1s,.5s)";
  check_declaration ~expected:"animation-delay:mod(1.1s,.5s)"
    ~optimized:"animation-delay:.1s" "animation-delay:mod(1.1s,.5s)";
  check_declaration ~expected:"animation-delay:rem(1.1s,.5s)"
    ~optimized:"animation-delay:.1s" "animation-delay:rem(1.1s,.5s)";
  (* CSS Values 4 (ED) sec. 10.8 Syntax: "<calc-value> = <number> | <dimension>
     | <percentage> | <calc-keyword> | ( <calc-sum> )" over "<calc-keyword> = e
     | pi | infinity | -infinity | NaN". A CSS-wide keyword is none of those, so
     it is a whole <time> and never an operand of one. *)
  check_declaration ~expected:"animation-delay:revert" "animation-delay: revert";
  neg_cursor read_declaration "animation-delay: calc(revert)";
  neg_cursor read_declaration "animation-delay: calc(revert + 1s)";
  neg_cursor read_declaration "animation-delay: calc(1s + revert)";
  neg_cursor read_declaration "animation-delay: calc(revert * 2)";
  neg_cursor read_declaration "animation-delay: calc(revert-layer + 1s)";
  neg_cursor read_declaration "animation-delay: calc(inherit + 1s)";
  neg_cursor read_declaration "animation-delay: calc(initial + 1s)";
  neg_cursor read_declaration "animation-delay: calc(unset + 1s)";
  (* CSS Grid 2 sec. 8.3 spells a <grid-line> index and its name with [&&], so
     the name sits on either side of the index, and sec. 10 puts a math function
     wherever an <integer> goes. Chrome 153 keeps all four and serialises them
     index-first. *)
  check_declaration ~expected:"grid-row-start:2 center"
    ~optimized:"grid-row-start:2 center" "grid-row-start: center calc(2)";
  check_declaration ~expected:"grid-row-start:2 center"
    ~optimized:"grid-row-start:2 center" "grid-row-start: calc(2) center";
  check_declaration ~expected:"grid-row-start:calc(.5) center"
    ~optimized:"grid-row-start:calc(.5) center"
    "grid-row-start: center calc(.5)";
  check_declaration ~expected:"grid-area:2 center"
    ~optimized:"grid-area:2 center" "grid-area: center calc(2)";
  (* sec. 8.3 writes the index [<integer [-inf,-1]> | <integer [1,inf]>], and
     zero is in neither range whichever way it is spelled. Chrome refuses both
     of these too. *)
  neg_cursor read_declaration "grid-row-start: calc(0)";
  neg_cursor read_declaration "grid-row-start: center calc(0)";
  neg_cursor read_declaration "grid-row-start: center 0";
  (* sec. 10.12 rounds a call at an <integer> slot to the nearest integer, ties
     toward positive infinity, so a call that resolves reaches sec. 8.3's range
     as the integer it rounds to and the ones rounding to zero are no line.
     Chrome 153 refuses each of these and keeps calc(-.6), which rounds to
     -1. *)
  neg_cursor read_declaration "grid-area: calc(1/2/3/4/5)";
  neg_cursor read_declaration "grid-area: calc(2 * 1/2/3/4/5)";
  neg_cursor read_declaration "grid-row-start: calc(-1/2)";
  neg_cursor read_declaration "grid-row-start: calc(.4)";
  neg_cursor read_declaration "grid-row-start: calc(-.4)";
  neg_cursor read_declaration "grid-row-start: calc(-.5)";
  check_declaration ~expected:"grid-row-start:calc(-.6)"
    ~optimized:"grid-row-start:calc(-.6)" "grid-row-start: calc(-.6)";

  (* sec. 10 allows a math function wherever an <integer> is allowed, and sec.
     10.12 rounds the call and clamps it, so a fractional or out-of-range call
     keeps its wrapper where the bare literal is refused. Chrome 153 agrees on
     each of these. *)
  check_declaration ~expected:"-webkit-line-clamp:2"
    ~optimized:"-webkit-line-clamp:2" "-webkit-line-clamp: calc(2)";
  check_declaration ~expected:"-webkit-line-clamp:calc(1 + 1)"
    ~optimized:"-webkit-line-clamp:2" "-webkit-line-clamp: calc(1 + 1)";
  check_declaration ~expected:"-webkit-line-clamp:calc(.5)"
    ~optimized:"-webkit-line-clamp:calc(.5)" "-webkit-line-clamp: calc(.5)";
  check_declaration ~expected:"-webkit-line-clamp:calc(0)"
    ~optimized:"-webkit-line-clamp:calc(0)" "-webkit-line-clamp: calc(0)";
  neg_cursor read_declaration "-webkit-line-clamp: 0";
  neg_cursor read_declaration "-webkit-line-clamp: .5";
  (* CSS Values 4 (ED) sec. 10: "Math functions can be used ... wherever
     <length>, <frequency>, <angle>, <time>, <percentage>, <number>, or
     <integer> values are allowed", and CSS Fonts 4 sec. 2.2 spells font-weight
     over <number [1,1000]>. Chrome 153 keeps these. *)
  check_declaration ~expected:"font-weight:400" ~optimized:"font-weight:400"
    "font-weight: calc(400)";
  check_declaration ~expected:"font-weight:calc(100 + 300)"
    ~optimized:"font-weight:400" "font-weight: calc(100 + 300)";
  check_declaration ~expected:"font-weight:calc(var(--w))"
    ~optimized:"font-weight:calc(var(--w))" "font-weight: calc(var(--w))";
  (* sec. 10.12 clamps the call's result rather than dropping the declaration,
     so the wrapper is what makes an out-of-range weight a weight: unwrapping it
     would write the literal a browser refuses. *)
  check_declaration ~expected:"font-weight:calc(0)"
    ~optimized:"font-weight:calc(0)" "font-weight: calc(0)";
  check_declaration ~expected:"font-weight:calc(1001)"
    ~optimized:"font-weight:calc(1001)" "font-weight: calc(1001)";
  neg_cursor read_declaration "font-weight: 0";
  neg_cursor read_declaration "font-weight: 1001";
  (* CSS Transitions 1 (ED) sec. 2.5 assigns the first <time> of a
     <single-transition> to transition-duration and the second to
     transition-delay, and only the duration is [0s,inf]: a negative delay
     starts the transition partway through. Chrome 153 keeps all three. *)
  check_declaration ~expected:"transition:all 1s -1s" "transition: 1s -1s";
  check_declaration ~expected:"transition:opacity 1s -1s"
    "transition: opacity 1s -1s";
  check_declaration ~expected:"transition:opacity 1s ease-in -1s"
    "transition: opacity 1s ease-in -1s";
  (* The duration slot keeps its range, so a lone negative has nowhere to go. *)
  neg_cursor read_declaration "transition: -1s";
  neg_cursor read_declaration "transition: -1s 1s";
  check_declaration ~expected:"transition-duration:var(--d,.5s)"
    ~optimized:"transition-duration:var(--d,.5s)"
    "transition-duration:var(--d,500ms)";
  check_declaration ~expected:"interest-delay:round(1100ms,500ms)"
    ~optimized:"interest-delay:1000ms" "interest-delay:round(1100ms,500ms)";
  (* CSS Values 4 sec. 7.2: [ms] and [s] are interchangeable, and the shorter
     spelling is picked by the AST normalisation, which canonicalises a typed
     [<time>] - a [round()] operand, a [var()] fallback - but leaves the
     operands of a [calc()] that survives to the output as authored. The printer
     mirrors that split at every depth, so a fallback nested inside a [calc()]
     keeps its unit exactly like the operand beside it: two declarations that
     print alike must be structurally alike. *)
  check_declaration ~expected:"transition-duration:calc(var(--d,1000ms))"
    ~optimized:"transition-duration:calc(var(--d,1000ms))"
    "transition-duration:calc(var(--d,1000ms))";
  check_declaration
    ~expected:"transition-duration:calc(var(--a,var(--b,1000ms)))"
    ~optimized:"transition-duration:calc(var(--a,var(--b,1000ms)))"
    "transition-duration:calc(var(--a,var(--b,1000ms)))";
  check_declaration ~expected:"transition-duration:calc(var(--x) + 1500ms)"
    ~optimized:"transition-duration:calc(var(--x) + 1500ms)"
    "transition-duration:calc(var(--x) + 1500ms)";
  check_declaration ~expected:"transition-duration:round(var(--d,1s),1s)"
    ~optimized:"transition-duration:round(var(--d,1s),1s)"
    "transition-duration:round(var(--d,1000ms),1s)";
  (* [interest-delay] keeps authored milliseconds for its own operands, so a
     fallback nested in one of its [calc()] operands keeps them too. *)
  check_declaration ~expected:"interest-delay:calc(var(--a,var(--b,1000ms)))"
    ~optimized:"interest-delay:calc(var(--a,var(--b,1000ms)))"
    "interest-delay:calc(var(--a,var(--b,1000ms)))";
  let c = Cursor.of_string "transition-delay:bogus" in
  match read_declaration c with
  | exception
      Error.Parse_error { kind = Error.Bad_value { property; reason }; _ } ->
      Alcotest.(check string) "diagnostic property" "transition-delay" property;
      Alcotest.(check string) "diagnostic reason" "expected time value" reason
  | exception Error.Parse_error e ->
      Alcotest.failf "unexpected diagnostic: %s" (Error.to_string e)
  | _ -> Alcotest.fail "expected an invalid delay to be rejected"

let animations_state () =
  check_declaration ~expected:"animation-iteration-count:1"
    "animation-iteration-count: 1";
  check_declaration ~expected:"animation-iteration-count:3"
    "animation-iteration-count: 3";
  check_declaration ~expected:"animation-iteration-count:infinite"
    "animation-iteration-count: infinite";

  check_declaration ~expected:"animation-direction:normal"
    "animation-direction: normal";
  check_declaration ~expected:"animation-direction:reverse"
    "animation-direction: reverse";
  check_declaration ~expected:"animation-direction:alternate"
    "animation-direction: alternate";
  check_declaration ~expected:"animation-direction:alternate-reverse"
    "animation-direction: alternate-reverse";

  check_declaration ~expected:"animation-fill-mode:none"
    "animation-fill-mode: none";
  check_declaration ~expected:"animation-fill-mode:forwards"
    "animation-fill-mode: forwards";
  check_declaration ~expected:"animation-fill-mode:backwards"
    "animation-fill-mode: backwards";
  check_declaration ~expected:"animation-fill-mode:both"
    "animation-fill-mode: both";

  check_declaration ~expected:"animation-play-state:running"
    "animation-play-state: running";
  check_declaration ~expected:"animation-play-state:paused"
    "animation-play-state: paused"

(* CSS Animations 1 (ED) sec. 4.9 joins the eight components of a
   [<single-animation>] with [||] and assigns a keyword to a property other than
   animation-name wherever that property has not been filled yet, so the first
   [none] of [animation: none none] is the fill mode and the second the name.
   Both are the initial values of their longhands, so every slot written here
   holds one, and what the whole declares is what [animation: none] declares. *)
let animation_drained_shorthand () =
  check_declaration ~expected:"animation:none" ~optimized:"animation:none"
    "animation: none none";
  (* CSS Animations 2 sec. 4.1 makes auto the initial duration, so an explicit
     0s is a value of its own: dropping it says auto, which Chrome resolves to a
     different animation-duration. The delay's initial is still 0s. *)
  check_declaration ~expected:"animation:0s" ~optimized:"animation:0s"
    "animation: 0s ease 0s 1 normal none running none";
  check_declaration ~expected:"animation:spin 0s" "animation: spin 0s";
  check_declaration ~expected:"animation:spin 0s linear"
    "animation: spin 0s linear";
  check_declaration ~expected:"animation:spin" "animation: spin";
  check_declaration ~expected:"animation:spin" "animation: spin auto";
  check_declaration ~expected:"animation:spin 1s" "animation: spin 1s 0s";
  (* Controls: the drop still runs wherever a slot outlives it. *)
  check_declaration ~expected:"animation:1s" ~optimized:"animation:1s"
    "animation: 1s none";
  check_declaration ~expected:"animation:slide" ~optimized:"animation:slide"
    "animation: none slide";
  check_declaration ~expected:"animation:none" ~optimized:"animation:none"
    "animation: none"

let transforms () =
  (* Transform functions *)
  check_declaration ~expected:"transform:none" "transform: none";
  check_declaration ~expected:"transform:translateX(10px)"
    "transform: translateX(10px)";
  check_declaration ~expected:"transform:translateY(20px)"
    "transform: translateY(20px)";
  check_declaration ~expected:"transform:translate(10px,20px)"
    "transform: translate(10px, 20px)";
  check_declaration ~expected:"transform:scale(2)" "transform: scale(2)";
  check_declaration ~expected:"transform:scale(1.5,2)"
    "transform: scale(1.5, 2)";
  check_declaration ~expected:"transform:rotate(45deg)"
    "transform: rotate(45deg)";
  check_declaration ~expected:"transform:skewX(30deg)" "transform: skewX(30deg)";
  check_declaration ~expected:"transform:skewY(15deg)" "transform: skewY(15deg)";
  check_declaration ~expected:"transform:matrix(1,0,0,1,0,0)"
    "transform: matrix(1, 0, 0, 1, 0, 0)";

  (* Multiple transforms. Per CSS Transforms 1 section 8 the printer drops
     whitespace between back-to-back transform functions under minify, matching
     Lightning CSS. *)
  check_declaration ~expected:"transform:translateX(10px)rotate(45deg)"
    "transform: translateX(10px) rotate(45deg)";
  check_declaration ~expected:"transform:scale(2)translateY(20px)rotate(180deg)"
    "transform: scale(2) translateY(20px) rotate(180deg)";

  (* Transform origin *)
  (* Per CSS Transforms 1 sec. 4 [center] is shorthand for [50% 50%] and the
     keyword pair [top left] is [0 0]. A single [0] would mean [0 50%], so the
     two-value form must be preserved. *)
  check_declaration ~expected:"transform-origin:center"
    ~optimized:"transform-origin:50%" "transform-origin: center";
  check_declaration ~expected:"transform-origin:top left"
    ~optimized:"transform-origin:0 0" "transform-origin: top left";
  check_declaration ~expected:"transform-origin:50% 50%"
    ~optimized:"transform-origin:50%" "transform-origin: 50% 50%";
  check_declaration ~expected:"transform-origin:var(--o,center)"
    ~optimized:"transform-origin:var(--o,50%)"
    "transform-origin:var(--o,center)";
  check_declaration ~expected:"transform-origin:10px 20px"
    "transform-origin: 10px 20px"

let grid () =
  (* Grid template columns/rows *)
  check_declaration ~expected:"grid-template-columns:none"
    "grid-template-columns: none";
  check_declaration ~expected:"grid-template-columns:100px 200px"
    "grid-template-columns: 100px 200px";
  check_declaration ~expected:"grid-template-columns:1fr 2fr"
    "grid-template-columns: 1fr 2fr";
  check_declaration ~expected:"grid-template-columns:repeat(3,1fr)"
    "grid-template-columns: repeat(3, 1fr)";
  (* a var() in the repeat count is a pending-substitution count, kept verbatim
     rather than dropped *)
  check_declaration ~expected:"grid-template-columns:repeat(var(--n),1fr)"
    "grid-template-columns: repeat(var(--n), 1fr)";
  check_declaration ~expected:"grid-template-columns:repeat(var(--n,3),1fr)"
    "grid-template-columns: repeat(var(--n, 3), 1fr)";

  (* minmax with fr units *)
  check_declaration ~expected:"grid-template-columns:minmax(100px,1fr)200px"
    "grid-template-columns: minmax(100px, 1fr) 200px";
  check_declaration ~expected:"grid-template-rows:none"
    "grid-template-rows: none";
  check_declaration ~expected:"grid-template-rows:100px auto"
    "grid-template-rows: 100px auto";

  check_declaration ~expected:"grid-template-rows:repeat(2,minmax(0,1fr))"
    "grid-template-rows: repeat(2, minmax(0, 1fr))";
  (* CSS Grid 2 (ED) sec. 7.2 gives the longhands a track-list grammar. The
     slash and string-area forms belong only to grid-template in sec. 7.4. *)
  neg_cursor read_declaration "grid-template-columns: 1px / 2px";
  neg_cursor read_declaration "grid-template-rows: 1px / 2px";
  neg_cursor read_declaration "grid-template-columns: \"a\" 1px";
  neg_cursor read_declaration "grid-template-rows: \"a\" 1px";

  (* Grid areas *)
  check_declaration
    ~expected:"grid-template-areas:\"header header\"\"sidebar main\""
    "grid-template-areas: \"header header\" \"sidebar main\"";
  check_declaration ~expected:"grid-template-areas:\"nav main\"\". foot\""
    "grid-template-areas: \"nav  main\" \".    foot\"";
  check_declaration ~expected:"grid-template-areas:\". .\""
    "grid-template-areas: \".  .\"";
  check_declaration ~expected:"grid-area:header" "grid-area: header";

  (* Grid lines *)
  check_declaration ~expected:"grid-row-start:1" "grid-row-start: 1";
  check_declaration ~expected:"grid-row-start:span 2" "grid-row-start: span 2";
  check_declaration ~expected:"grid-row-end:3" "grid-row-end: 3";
  check_declaration ~expected:"grid-column-start:1" "grid-column-start: 1";
  check_declaration ~expected:"grid-column-end:-1" "grid-column-end: -1";
  (* A grid-line calc carrying a var is kept as a typed calc and round-trips; an
     all-numeric grid-line calc folds to an integer. *)
  check_declaration ~expected:"grid-column-start:calc(var(--n)*-1)"
    "grid-column-start: calc(var(--n) * -1)";
  check_declaration ~expected:"grid-column-end:6" "grid-column-end: calc(3 * 2)";

  (* Grid auto flow *)
  check_declaration ~expected:"grid-auto-flow:row" "grid-auto-flow: row";
  check_declaration ~expected:"grid-auto-flow:column" "grid-auto-flow: column";
  check_declaration ~expected:"grid-auto-flow:row dense"
    "grid-auto-flow: row dense";
  check_declaration ~expected:"grid-auto-flow:column dense"
    "grid-auto-flow: column dense";
  (* CSS Grid 2 sec. 7.7 gives [ row | column ] || dense with [row] as the
     omitted axis, so [row dense] and [dense] are one value and the optimizer
     picks the shorter. The axis is load-bearing on [column dense]. *)
  decl_optimizes_to ~held:"grid-auto-flow:row dense"
    ~into:"grid-auto-flow:dense" "grid-auto-flow: row dense";
  decl_optimizes_to ~held:"grid-auto-flow:row dense"
    ~into:"grid-auto-flow:dense" "grid-auto-flow: dense row";
  decl_optimizes ~prop:"grid-auto-flow" ~into:"column dense" "column dense";

  (* CSS Grid 2 (ED) sec. 7.6 gives grid-auto-columns and grid-auto-rows
     [<track-size>+], so they route to a reader with no [<line-names>] position
     and none of the sec. 7.2 [<track-list>] forms. *)
  check_declaration ~expected:"grid-auto-rows:1px" "grid-auto-rows: 1px";
  check_declaration ~expected:"grid-auto-rows:minmax(1px,2px)"
    "grid-auto-rows: minmax(1px, 2px)";
  check_declaration ~expected:"grid-auto-columns:1fr 2fr"
    "grid-auto-columns: 1fr 2fr";
  check_declaration ~expected:"grid-auto-rows:fit-content(10px)"
    "grid-auto-rows: fit-content(10px)";
  check_declaration ~expected:"grid-auto-rows:auto" "grid-auto-rows: auto";
  check_declaration ~expected:"grid-auto-rows:initial" "grid-auto-rows: initial";
  neg_cursor read_declaration "grid-auto-rows: [a] 1px";
  neg_cursor read_declaration "grid-auto-rows: [a]";
  neg_cursor read_declaration "grid-auto-rows: 1px [a]";
  neg_cursor read_declaration "grid-auto-columns: [a] 1px";
  neg_cursor read_declaration "grid-auto-rows: none";
  neg_cursor read_declaration "grid-auto-rows: subgrid";
  neg_cursor read_declaration "grid-auto-rows: repeat(2, 1px)";
  neg_cursor read_declaration "grid-auto-rows: 1px / 2px";

  (* sec. 7.2 keeps every [<line-names>] next to a track size, in the shorthands
     as well as in grid-template-columns / grid-template-rows. *)
  check_declaration ~expected:"grid-template-columns:[a]1px"
    "grid-template-columns: [a] 1px";
  check_declaration ~expected:"grid-template-columns:1px[a]2px"
    "grid-template-columns: 1px [a] 2px";
  (* CSS Grid 2 sec. 7.2 gives [subgrid] its own [<line-name-list>] grammar.
     Unlike an ordinary track list, that list may contain adjacent line-name
     blocks or a name-only [repeat()]. *)
  check_declaration ~expected:"grid-template-columns:subgrid[a]"
    "grid-template-columns: subgrid [a]";
  check_declaration ~expected:"grid-template-rows:subgrid[a][b]"
    "grid-template-rows: subgrid [a] [b]";
  check_declaration ~expected:"grid-template-columns:subgrid repeat(2,[a])"
    "grid-template-columns: subgrid repeat(2, [a])";
  neg_cursor read_declaration "grid-template-columns: [a]";
  neg_cursor read_declaration "grid-template-rows: [a] [b] 1px";
  neg_cursor read_declaration "grid-template: 1px / [a]";
  neg_cursor read_declaration "grid: auto-flow [a] / 1px";

  (* CSS Grid 2 sec. 7.4 builds the shorthand from [<'grid-template-rows'> /
     <'grid-template-columns'>], and sec. 7.2 gives each of those a [none] arm,
     so [none] is a track list like any other on either side of the slash. *)
  check_declaration ~expected:"grid-template:none/1fr"
    "grid-template: none / 1fr";
  check_declaration ~expected:"grid-template:1fr/none"
    "grid-template: 1fr / none";
  check_declaration ~expected:"grid-template:none/none"
    "grid-template: none / none";
  check_declaration ~expected:"grid-template:none/auto"
    "grid-template: none / auto";
  check_declaration ~expected:"grid:none/200px" "grid: none / 200px";
  check_declaration ~expected:"grid:200px/none" "grid: 200px / none";
  check_declaration ~expected:"grid:none/auto" "grid: none / auto";
  (* sec. 7.8: [auto-flow] with no [<grid-auto-rows>] leaves the row track list
     empty, which is the same declaration as a [none] rows arm. *)
  check_declaration ~expected:"grid:none/200px" "grid: auto-flow / 200px";

  (* CSS Cascade 5 (ED) sec. 7.3: explicit defaulting takes the whole
     declaration, so a CSS-wide keyword is a placement value on its own and
     never one slot of one. *)
  check_declaration ~expected:"grid-column:initial" "grid-column: initial";
  check_declaration ~expected:"grid-area:initial" "grid-area: initial";
  neg_cursor read_declaration "grid-column: 2 / initial";
  neg_cursor read_declaration "grid-row: 2 / initial";
  neg_cursor read_declaration "grid-area: 1 / 2 / 3 / initial";
  neg_cursor read_declaration "grid-column: initial / 2";
  neg_cursor read_declaration "grid-column-start: 2 initial";

  (* Grid gaps *)
  check_declaration ~expected:"gap:10px" "gap: 10px";
  check_declaration ~expected:"gap:10px 20px" "gap: 10px 20px";
  check_declaration ~expected:"column-gap:10px" "column-gap: 10px";
  check_declaration ~expected:"row-gap:20px" "row-gap: 20px"

let misc () =
  (* Opacity *)
  check_declaration ~expected:"opacity:0" "opacity: 0";
  check_declaration ~expected:"opacity:.5" "opacity: 0.5";
  check_declaration ~expected:"opacity:1" "opacity: 1";

  (* Z-index *)
  check_declaration ~expected:"z-index:auto" "z-index: auto";
  check_declaration ~expected:"z-index:0" "z-index: 0";
  check_declaration ~expected:"z-index:10" "z-index: 10";
  check_declaration ~expected:"z-index:-1" "z-index: -1";
  check_declaration ~expected:"z-index:9999" "z-index: 9999";
  (* A calc that cannot const-fold (carries a var) is kept as a typed calc and
     round-trips; an all-numeric calc still folds to an integer. *)
  check_declaration ~expected:"z-index:calc(var(--z)*-1)"
    "z-index: calc(var(--z) * -1)";
  check_declaration ~expected:"z-index:10" "z-index: calc(5 * 2)";

  (* Cursor *)
  check_declaration ~expected:"cursor:auto" "cursor: auto";
  check_declaration ~expected:"cursor:default" "cursor: default";
  check_declaration ~expected:"cursor:pointer" "cursor: pointer";
  check_declaration ~expected:"cursor:move" "cursor: move";
  check_declaration ~expected:"cursor:text" "cursor: text";
  check_declaration ~expected:"cursor:wait" "cursor: wait";
  check_declaration ~expected:"cursor:help" "cursor: help";
  check_declaration ~expected:"cursor:crosshair" "cursor: crosshair";
  check_declaration ~expected:"cursor:not-allowed" "cursor: not-allowed";
  check_declaration ~expected:"cursor:none" "cursor: none";

  (* Visibility *)
  check_declaration ~expected:"visibility:visible" "visibility: visible";
  check_declaration ~expected:"visibility:hidden" "visibility: hidden";
  check_declaration ~expected:"visibility:collapse" "visibility: collapse";

  (* Box sizing *)
  check_declaration ~expected:"box-sizing:content-box" "box-sizing: content-box";
  check_declaration ~expected:"box-sizing:border-box" "box-sizing: border-box";
  (* Vendor-prefixed box-sizing is typed, so it round-trips and canonicalises
     its value (an [Unknown_property] would keep [BORDER-BOX] verbatim). *)
  check_declaration ~expected:"-webkit-box-sizing:border-box"
    "-webkit-box-sizing: border-box";
  check_declaration ~expected:"-moz-box-sizing:border-box"
    "-moz-box-sizing: BORDER-BOX";
  (* The vendor-prefixed transform properties are typed too. *)
  check_declaration ~expected:"-moz-transform:none" "-moz-transform: NONE";
  check_declaration ~expected:"-o-transform:rotate(45deg)"
    "-o-transform: rotate(45deg)";
  check_declaration ~expected:"-ms-user-select:none" "-ms-user-select: NONE";
  check_declaration ~expected:"-webkit-text-fill-color:#fff"
    "-webkit-text-fill-color: #ffffff";

  (* User select *)
  check_declaration ~expected:"user-select:none" "user-select: none";
  check_declaration ~expected:"user-select:auto" "user-select: auto";
  check_declaration ~expected:"user-select:text" "user-select: text";
  check_declaration ~expected:"user-select:all" "user-select: all";

  (* Pointer events *)
  check_declaration ~expected:"pointer-events:none" "pointer-events: none";
  check_declaration ~expected:"pointer-events:auto" "pointer-events: auto";

  (* Resize *)
  check_declaration ~expected:"resize:none" "resize: none";
  check_declaration ~expected:"resize:both" "resize: both";
  check_declaration ~expected:"resize:horizontal" "resize: horizontal";
  check_declaration ~expected:"resize:vertical" "resize: vertical"

let suite =
  ( "declaration_families",
    [
      (* Property value categories *)
      test_case "colors" `Quick colors;
      test_case "lengths" `Quick lengths;
      test_case "display" `Quick display;
      test_case "position" `Quick position;
      test_case "font properties" `Quick font_properties;
      test_case "text properties" `Quick text_properties;
      test_case "flexbox direction" `Quick flexbox_direction;
      test_case "flexbox wrap" `Quick flexbox_wrap;
      test_case "flexbox flex+basis" `Quick flexbox_flex_and_basis;
      test_case "flexbox alignment" `Quick flexbox_alignment;
      test_case "borders" `Quick borders;
      test_case "overflow" `Quick overflow;
      test_case "animations (timing)" `Quick animations_timing;
      test_case "animations (state)" `Quick animations_state;
      test_case "animation drained shorthand" `Quick animation_drained_shorthand;
      test_case "transforms" `Quick transforms;
      test_case "grid" `Quick grid;
      test_case "misc properties" `Quick misc;
    ] )
