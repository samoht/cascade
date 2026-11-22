CLI: a rule whose selector no target browser reads.

`:-o-prefocus` is Presto's, and Opera left Presto for Blink in 2013. MDN's
browser-compat-data records it for no browser, Firefox's selector parser
lists no such pseudo-class, and Chrome 151 and WebKit 26.5 drop
`a:-o-prefocus{color:red}`. Selectors 4 sec. 3.9 then drops the whole
rule in every browser `--minify` targets, so the minifier may drop it too.

  $ cat > prefocus.css <<EOF
  > a:-o-prefocus{color:red}
  > b{color:red}
  > EOF
  $ cascade fmt --minify prefocus.css
  prefocus.css:1:14: warning: no target browser reads :-o-prefocus
    a:-o-prefocus{color:red}
                 ^
  b{color:red}

`--enforce-spec` targets no browser, so an engine that reads the
pseudo-class may be the one the sheet is for: the rule stays as written,
and stays out of `b`'s, since a browser that does not read it would drop
`b` with it.

  $ cascade fmt --minify --enforce-spec prefocus.css
  a:-o-prefocus{color:red}b{color:red}

Canonical diff judges for the targets `--minify` serves, so the dropped
rule is no difference and the two sheets compare equal.

  $ cascade fmt --minify -q prefocus.css > prefocus.min.css
  $ cascade diff --diff=canonical prefocus.css prefocus.min.css
  prefocus.css:1:14: warning: no target browser reads :-o-prefocus
    a:-o-prefocus{color:red}
                 ^
  
  CSS files are identical

Under `--enforce-spec` it judges for every engine, and one side lost a rule
an engine outside the targets reads.

  $ cascade diff --diff=canonical --enforce-spec prefocus.css prefocus.min.css > /dev/null
  [1]

The same holds for each pseudo-class of the other SatCSS sheets that
`--minify` used to leave undecided, none of which a target reads:
`:-moz-any-link` (Firefox 1 to 50 in browser-compat-data), `:-ms-fullscreen`
(Internet Explorer 11 there), and `:-moz-submit-invalid`, `:selected`,
`:-ms-full-screen`, `:-o-full-screen` and `:full-screen`, which it records
for no browser and Firefox's parser does not list, and `:-moz-locale-dir()`,
which Firefox's parser reads in its own sheets alone. Chrome 151 and WebKit
26.5 drop a rule holding any of them.

  $ for pc in -moz-submit-invalid -moz-any-link -ms-fullscreen selected -ms-full-screen -o-full-screen full-screen '-moz-locale-dir(rtl)'; do
  >   printf 'a:%s{color:red}b{color:red}\n' $pc > one.css
  >   printf 'b{color:red}\n' > other.css
  >   cascade diff --diff=canonical one.css other.css > /dev/null 2>&1; echo "$pc $?"
  > done
  -moz-submit-invalid 0
  -moz-any-link 0
  -ms-fullscreen 0
  selected 0
  -ms-full-screen 0
  -o-full-screen 0
  full-screen 0
  -moz-locale-dir(rtl) 0

A vendor-prefixed pseudo-class cascade does not know is not one of these:
nothing says no target reads it, so its rule stays, out of every list,
and a sheet that lost it differs.

  $ printf 'a:-x-future{color:red}b{color:red}\n' > future.css
  $ cascade fmt --minify future.css
  a:-x-future{color:red}b{color:red}
  $ cascade diff --diff=canonical future.css other.css > /dev/null 2>&1
  [1]

Firefox 128 reads several such names in an author sheet (Servo's
non_ts_pseudo_class_list.rs and the aliases in selector_parser.rs, ESR
128), which Chrome 151 and WebKit 26.5 drop: `:-moz-only-whitespace`,
`:-moz-window-inactive`, `:-moz-first-node`, `:-moz-drag-over` and,
with one colon, `:-moz-placeholder`; Chrome 151 and WebKit 26.5 read
`:-webkit-any-link` and `:-webkit-drag`. A target reads each rule below, so each stays as written, at every site
Selectors 4 makes unforgiving and inside a forgiving `:is()`.

  $ cat > vendor.css <<EOF
  > a:-moz-only-whitespace{z-index:1}
  > a:-moz-window-inactive{z-index:2}
  > a:not(:-moz-first-node){z-index:3}
  > a:has(:-moz-only-whitespace){z-index:4}
  > a:-moz-drag-over{z-index:5}
  > :is(a:-webkit-any-link,c){z-index:6}
  > a:-webkit-drag{z-index:7}
  > a:-moz-placeholder{z-index:8}
  > EOF
  $ cascade fmt --minify vendor.css
  a:-moz-only-whitespace{z-index:1}a:-moz-window-inactive{z-index:2}a:not(:-moz-first-node){z-index:3}a:has(:-moz-only-whitespace){z-index:4}a:-moz-drag-over{z-index:5}:is(a:-webkit-any-link,c){z-index:6}a:-webkit-drag{z-index:7}a:-moz-placeholder{z-index:8}
  $ cascade fmt --minify -q vendor.css > vendor.min.css
  $ cascade diff --diff=canonical vendor.css vendor.min.css
  CSS files are identical

A functional pseudo-class written without its argument is read by no
engine, whatever its name: Chrome 151 and WebKit 26.5 drop `a:lang`,
`a:not` and `a:-moz-any`, and Firefox's parser takes each of them only
as a function.

  $ printf 'a:lang{color:red}a:not{color:red}a:-moz-any{color:red}b{color:red}\n' > bare.css
  $ cascade fmt --minify -q bare.css
  b{color:red}

Inside a forgiving `:is()` the pseudo-class costs only its own argument in a
browser, and the rest of the rule stays, so it is not a rule no target
reads.

  $ printf ':is(a:-o-prefocus,c){color:red}b{color:red}\n' > forgiving.css
  $ cascade fmt --minify forgiving.css
  b,c{color:red}

An unprefixed pseudo-class MDN's compatibility data does not record is
one no target ships: every pseudo-class an engine reads without a
prefix is a standard one, which the data records. yahoo-uk-2's
`#Stencil .Td-u:h:hover` is such a rule: Chrome 151 and WebKit 26.5
drop it, and Firefox's selector parser lists no `:h`.

  $ printf '#s .t:h:hover{color:red}b{color:red}\n' > h.css
  $ cascade fmt --minify -q h.css
  b{color:red}
  $ cascade diff --diff=canonical h.css other.css > /dev/null 2>&1; echo $?
  0

A keyframe selector outside CSS Animations 1's `from | to |
<percentage>`, such as YUI's `0` for `0%`, drops its keyframe rule in
every engine: Chrome 151 and WebKit 26.5 keep no keyframe for `0` or
`0,50%,80%,100%`, and Firefox's KeyframePercentage takes `from`, `to`
and a percentage alone. So its loss is no difference either, under
`--enforce-spec` too.

  $ printf '@keyframes a{0{opacity:0}to{opacity:1}}\n' > kf.css
  $ printf '@keyframes a{to{opacity:1}}\n' > kf2.css
  $ cascade diff --diff=canonical kf.css kf2.css > /dev/null 2>&1; echo $?
  0
  $ cascade diff --diff=canonical --enforce-spec kf.css kf2.css > /dev/null 2>&1; echo $?
  0
