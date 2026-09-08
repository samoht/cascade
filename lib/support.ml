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
      key = "css.properties.width.contain";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Sizing 4 sec. 3.2 adds contain to <box-size>, which every sizing \
         property takes; Chrome has not implemented it";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.height.contain";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Sizing 4 sec. 3.2 adds contain to <box-size>, which every sizing \
         property takes; Chrome has not implemented it";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.min-width.contain";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Sizing 4 sec. 3.2 adds contain to <box-size>, which every sizing \
         property takes; Chrome has not implemented it";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.min-height.contain";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Sizing 4 sec. 3.2 adds contain to <box-size>, which every sizing \
         property takes; Chrome has not implemented it";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.max-width.contain";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Sizing 4 sec. 3.2 adds contain to <box-size>, which every sizing \
         property takes; Chrome has not implemented it";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.max-height.contain";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Sizing 4 sec. 3.2 adds contain to <box-size>, which every sizing \
         property takes; Chrome has not implemented it";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.inline-size.contain";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Sizing 4 sec. 3.2 adds contain to <box-size>, which every sizing \
         property takes; Chrome has not implemented it";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.min-inline-size.contain";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Sizing 4 sec. 3.2 adds contain to <box-size>, which every sizing \
         property takes; Chrome has not implemented it";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.max-inline-size.contain";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Sizing 4 sec. 3.2 adds contain to <box-size>, which every sizing \
         property takes; Chrome has not implemented it";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.block-size.contain";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Sizing 4 sec. 3.2 adds contain to <box-size>, which every sizing \
         property takes; Chrome has not implemented it";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.min-block-size.contain";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Sizing 4 sec. 3.2 adds contain to <box-size>, which every sizing \
         property takes; Chrome has not implemented it";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.max-block-size.contain";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Sizing 4 sec. 3.2 adds contain to <box-size>, which every sizing \
         property takes; Chrome has not implemented it";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.flex-basis.contain";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Sizing 4 sec. 3.2 adds contain to <box-size>, which every sizing \
         property takes; Chrome has not implemented it";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.text-box-edge.ideographic-ink";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Inline 3 sec. 4.4: <text-edge> = [ text | ideographic | \
         ideographic-ink ] | [ text | ideographic | ideographic-ink | cap | ex \
         ] [ text | ideographic | ideographic-ink | alphabetic ]";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.dominant-baseline.text_bottom";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Inline 3 sec. 5.2: auto | <baseline-metric>, and sec. 5.1 gives \
         <baseline-metric> = text-bottom | alphabetic | ideographic | middle | \
         central | mathematical | hanging | text-top";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.column-rule.trailing_comma";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Gaps 1 sec. 4 gives these a comma-separated list, one entry per \
         rule line, so a comma between two entries is theirs to read. The list \
         has no empty entry, and Chrome reads a trailing comma and drops it on \
         serialising";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.column-rule-width.trailing_comma";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Gaps 1 sec. 4 gives these a comma-separated list, one entry per \
         rule line, so a comma between two entries is theirs to read. The list \
         has no empty entry, and Chrome reads a trailing comma and drops it on \
         serialising";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.column-rule-style.trailing_comma";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Gaps 1 sec. 4 gives these a comma-separated list, one entry per \
         rule line, so a comma between two entries is theirs to read. The list \
         has no empty entry, and Chrome reads a trailing comma and drops it on \
         serialising";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.column-rule-color.trailing_comma";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Gaps 1 sec. 4 gives these a comma-separated list, one entry per \
         rule line, so a comma between two entries is theirs to read. The list \
         has no empty entry, and Chrome reads a trailing comma and drops it on \
         serialising";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.border.comma";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Backgrounds 3 sec. 4.5 and CSS Logical 1 sec. 4.6 build these \
         from a [||] of a width, a style and a colour, and no arm of either is \
         a comma. Chrome reads one between two components as the whitespace \
         separating them, so the declaration sets what the same value without \
         the comma sets";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.border-block.comma";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Backgrounds 3 sec. 4.5 and CSS Logical 1 sec. 4.6 build these \
         from a [||] of a width, a style and a colour, and no arm of either is \
         a comma. Chrome reads one between two components as the whitespace \
         separating them, so the declaration sets what the same value without \
         the comma sets";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.border-inline.comma";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Backgrounds 3 sec. 4.5 and CSS Logical 1 sec. 4.6 build these \
         from a [||] of a width, a style and a colour, and no arm of either is \
         a comma. Chrome reads one between two components as the whitespace \
         separating them, so the declaration sets what the same value without \
         the comma sets";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.border-block-start.comma";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Backgrounds 3 sec. 4.5 and CSS Logical 1 sec. 4.6 build these \
         from a [||] of a width, a style and a colour, and no arm of either is \
         a comma. Chrome reads one between two components as the whitespace \
         separating them, so the declaration sets what the same value without \
         the comma sets";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.border-block-end.comma";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Backgrounds 3 sec. 4.5 and CSS Logical 1 sec. 4.6 build these \
         from a [||] of a width, a style and a colour, and no arm of either is \
         a comma. Chrome reads one between two components as the whitespace \
         separating them, so the declaration sets what the same value without \
         the comma sets";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.border-inline-start.comma";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Backgrounds 3 sec. 4.5 and CSS Logical 1 sec. 4.6 build these \
         from a [||] of a width, a style and a colour, and no arm of either is \
         a comma. Chrome reads one between two components as the whitespace \
         separating them, so the declaration sets what the same value without \
         the comma sets";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.border-inline-end.comma";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Backgrounds 3 sec. 4.5 and CSS Logical 1 sec. 4.6 build these \
         from a [||] of a width, a style and a colour, and no arm of either is \
         a comma. Chrome reads one between two components as the whitespace \
         separating them, so the declaration sets what the same value without \
         the comma sets";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.border-top.comma";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Backgrounds 3 sec. 4.5 and CSS Logical 1 sec. 4.6 build these \
         from a [||] of a width, a style and a colour, and no arm of either is \
         a comma. Chrome reads one between two components as the whitespace \
         separating them, so the declaration sets what the same value without \
         the comma sets";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.border-right.comma";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Backgrounds 3 sec. 4.5 and CSS Logical 1 sec. 4.6 build these \
         from a [||] of a width, a style and a colour, and no arm of either is \
         a comma. Chrome reads one between two components as the whitespace \
         separating them, so the declaration sets what the same value without \
         the comma sets";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.border-bottom.comma";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Backgrounds 3 sec. 4.5 and CSS Logical 1 sec. 4.6 build these \
         from a [||] of a width, a style and a colour, and no arm of either is \
         a comma. Chrome reads one between two components as the whitespace \
         separating them, so the declaration sets what the same value without \
         the comma sets";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.border-left.comma";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Backgrounds 3 sec. 4.5 and CSS Logical 1 sec. 4.6 build these \
         from a [||] of a width, a style and a colour, and no arm of either is \
         a comma. Chrome reads one between two components as the whitespace \
         separating them, so the declaration sets what the same value without \
         the comma sets";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.baseline-shift.number";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Inline 3 sec. 4.2.3: <length-percentage> | sub | super | top | \
         center | bottom, and no arm is a bare <number>. Chrome reads one as \
         the unitless length SVG presentation attributes take";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.transition.negative";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Transitions 1 sec. 2.5's note assigns the first <time> that \
         parses to transition-duration, which sec. 2.2 ranges [0s,inf], so a \
         lone negative fills no slot. Chrome takes it";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.-webkit-transition.negative";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Transitions 1 sec. 2.5's note assigns the first <time> that \
         parses to transition-duration, which sec. 2.2 ranges [0s,inf], so a \
         lone negative fills no slot. Chrome takes it";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.-moz-transition.negative";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Transitions 1 sec. 2.5's note assigns the first <time> that \
         parses to transition-duration, which sec. 2.2 ranges [0s,inf], so a \
         lone negative fills no slot. Chrome takes it";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.-o-transition.negative";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Transitions 1 sec. 2.5's note assigns the first <time> that \
         parses to transition-duration, which sec. 2.2 ranges [0s,inf], so a \
         lone negative fills no slot. Chrome takes it";
      measured = "Chrome 153";
    };
    {
      key = "css.at-rules.counter-style.symbols.image";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Counter Styles 3 (ED) sec. 3.2 gives <symbol> the arms <string> | \
         \\\n\
        \         <image> | <custom-ident>, so a counter symbol may be an \
         image. Chrome \\\n\
        \         implements the string and ident arms and refuses every image";
      measured = "Chrome 153";
    };
    {
      key = "css.at-rules.counter-style.negative.image";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Counter Styles 3 (ED) sec. 3.2 gives <symbol> the arms <string> | \
         \\\n\
        \         <image> | <custom-ident>, so a counter symbol may be an \
         image. Chrome \\\n\
        \         implements the string and ident arms and refuses every image";
      measured = "Chrome 153";
    };
    {
      key = "css.at-rules.counter-style.prefix.image";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Counter Styles 3 (ED) sec. 3.2 gives <symbol> the arms <string> | \
         \\\n\
        \         <image> | <custom-ident>, so a counter symbol may be an \
         image. Chrome \\\n\
        \         implements the string and ident arms and refuses every image";
      measured = "Chrome 153";
    };
    {
      key = "css.at-rules.counter-style.suffix.image";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Counter Styles 3 (ED) sec. 3.2 gives <symbol> the arms <string> | \
         \\\n\
        \         <image> | <custom-ident>, so a counter symbol may be an \
         image. Chrome \\\n\
        \         implements the string and ident arms and refuses every image";
      measured = "Chrome 153";
    };
    {
      key = "css.at-rules.counter-style.additive-symbols.image";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Counter Styles 3 (ED) sec. 3.2 gives <symbol> the arms <string> | \
         \\\n\
        \         <image> | <custom-ident>, so a counter symbol may be an \
         image. Chrome \\\n\
        \         implements the string and ident arms and refuses every image";
      measured = "Chrome 153";
    };
    {
      key = "css.at-rules.counter-style.pad.image";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Counter Styles 3 (ED) sec. 3.2 gives <symbol> the arms <string> | \
         \\\n\
        \         <image> | <custom-ident>, so a counter symbol may be an \
         image. Chrome \\\n\
        \         implements the string and ident arms and refuses every image";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.font-family.default";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Fonts 4 sec. 2.1.1 gives an unquoted family a <custom-ident>+ and \
         excludes an identifier that could be misinterpreted as a pre-defined \
         keyword or a CSS-wide keyword, and CSS Values 4 sec. 4.2 reserves \
         default. Chrome applies the exclusion to a family of one identifier \
         and reads a reserved word inside a longer name";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.font-family.inherit";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Values 4 sec. 4.2 excludes the CSS-wide keywords from \
         <custom-ident> itself, so no word of a family sequence is one \
         wherever it stands. Chrome reads inherit as a family name once \
         another word stands beside it";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.contain-intrinsic-size.auto_none_auto";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Sizing 4 (ED) sec. 5.2 spells contain-intrinsic-size [ auto? [ \
         none | <length [0,inf]> ] ]{1,2}, so auto none is one whole slot and \
         the auto after it is no slot at all. Chrome takes the trailing bare \
         auto";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.break-before.all";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Break 4 sec. 3.1 writes break-before and break-after as auto | \
         avoid | always | all | avoid-page | page | left | right | recto | \
         verso | avoid-column | column | avoid-region | region, and the \
         module's change list says it adds exactly always and all over CSS \
         Fragmentation 3. web-features records the always key for both \
         properties and none for all";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.break-after.all";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why = "CSS Break 4 sec. 3.1 lists all among break-after's values";
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
        "CSS Transitions 1 sec. 2.5 spells the shorthand \
         <single-transition>#,          and sec. 2.3 gives <single-transition> \
         a [ none |          <single-transition-property> ] slot, so none is \
         one entry of the list          wherever it stands. Chrome reads \
         transition: none alone and refuses          every list holding one. \
         web-features records          css.properties.transition-property.none \
         for the longhand and nothing          for the shorthand's list";
      measured = "Chrome 153";
    };
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
      key = "css.properties.outline.auto";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "The colour slot of the shorthand is the sec. 3.4 <'outline-color'> \
         above, so outline: solid auto fills style and colour and CSS Values 4 \
         sec. 2.2 takes each option of the || once. Chrome reads auto only as \
         the style, so a second style keyword beside it leaves the value with \
         two, and it drops the declaration";
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
          Baseline.chrome = Some (153, 0);
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
          Baseline.chrome = Some (153, 0);
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
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Inline 3 sec. 4.2.2: baseline | <baseline-metric>, and no arm is \
         auto. Chrome accepts it from the SVG 1.1 grammar";
      measured = "Chrome 153";
    };
    {
      key = "css.at-rules.font-face.font-family.default";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Values 4 sec. 4.2 reserves default from <custom-ident> itself, so \
         no word of a family sequence is one wherever it stands. Chrome reads \
         default as a family word once another word stands beside it, at the \
         descriptor as at the property";
      measured = "Chrome 153";
    };
    {
      key = "css.at-rules.font-face.font-family.inherit";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Values 4 sec. 4.2 excludes the CSS-wide keywords from \
         <custom-ident> itself, so no word of a family sequence is one \
         wherever it stands. Chrome reads inherit as a family word once \
         another word stands beside it, at the descriptor as at the property";
      measured = "Chrome 153";
    };
    {
      key = "css.at-rules.font-face.font-weight.two_value_syntax";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Fonts 4 sec. 4.4 writes the descriptor auto | \
         <font-weight-absolute>{1,2}, so a pair of endpoints is the range a \
         variable font is asked for. Chrome takes only one value";
      measured = "Chrome 153";
    };
    {
      key = "css.at-rules.font-face.font-stretch.two_value_syntax";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Fonts 4 sec. 4.5 writes the descriptor auto | \
         <font-stretch-css3>{1,2}, so a pair of endpoints is the range a \
         variable font is asked for. Chrome takes only one value";
      measured = "Chrome 153";
    };
    {
      key = "css.at-rules.font-face.font-variation-settings.string";
      support =
        {
          Baseline.chrome = Some (153, 0);
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Fonts 4 sec. 6.6 writes each entry <opentype-tag> <number>, so a \
         bare tag names an axis and sets it to nothing. Chrome takes the tag \
         alone at the descriptor and gives it 1, where it refuses the same \
         entry at the property";
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

(* The generated table first and the measurements second, as {!implemented}:
   asking about one engine is the same question asked of one column, so a fact
   this project measured has to be visible to both or a caller gets a different
   answer for the same key depending on which it called. *)
let engine_implements engine version key =
  match
    match Hashtbl.find_opt (Lazy.force table) key with
    | Some s -> Some s
    | None -> Hashtbl.find_opt (Lazy.force measured_table) key
  with
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

let last_segment key =
  match String.rindex_opt key '.' with
  | None -> key
  | Some i -> String.sub key (i + 1) (String.length key - i - 1)

let keys_named segment =
  let of_table t =
    Hashtbl.fold
      (fun key _ acc ->
        if String.equal (last_segment key) segment then key :: acc else acc)
      (Lazy.force t) []
  in
  List.sort_uniq String.compare (of_table table @ of_table measured_table)

let keys_under prefix =
  let of_table t =
    Hashtbl.fold
      (fun key _ acc ->
        if String.starts_with ~prefix key then key :: acc else acc)
      (Lazy.force t) []
  in
  List.sort_uniq String.compare (of_table table @ of_table measured_table)

let unimplemented_by targets key =
  match implemented targets key with
  | Some false -> true
  | Some true | None -> false
