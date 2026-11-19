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
for no browser and Firefox's parser does not list. Chrome 151 and WebKit
26.5 drop a rule holding any of them.

  $ for pc in -moz-submit-invalid -moz-any-link -ms-fullscreen selected -ms-full-screen -o-full-screen full-screen; do
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

A pseudo-class cascade does not know is not one of these: nothing says no
target reads it, so a rule lost to it still withholds the verdict.

  $ printf 'a:-x-future{color:red}b{color:red}\n' > future.css
  $ cascade diff --diff=canonical future.css other.css > /dev/null 2>&1
  [2]

Inside a forgiving `:is()` the pseudo-class costs only its own argument in a
browser, and the rest of the rule stays, so it is not a rule no target
reads.

  $ printf ':is(a:-o-prefocus,c){color:red}b{color:red}\n' > forgiving.css
  $ cascade fmt --minify forgiving.css
  b,c{color:red}
