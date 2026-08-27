CLI: --minify keeps the value the author wrote inside an @supports condition.

CSS Conditional 3 sec. 6 gives a declaration feature the grammar
[( <declaration> )], and the UA answers it by running that exact declaration
through its own parser. The value there is not a value, it is the question. Two
spellings the spec calls equal are still two questions, because a feature query
exists to find the UAs whose parser does not match the spec: whether one of them
accepts both spellings is precisely what the author is asking, and a build
cannot know.

That makes a condition the one place in a stylesheet where spec-equivalence does
not license a rewrite. Everywhere else [--minify] picks the shortest equivalent
spelling and that is the whole policy; here the author's spelling is the input to
somebody else's parser, so it is handed back.

The surrounding whitespace is not part of the question. Every guard below is
written with the author's spacing and prints without it, the same elision
[test/cli/supports_guards.t] pins.


# Legacy colour syntax


[rgba()] with comma-separated arguments and [rgb()] with a slash-separated alpha
are two different grammars, shipped years apart. A guard written in the older one
asks about a colour syntax every UA that has ever evaluated a feature query
accepts; rewritten into the newer one it asks about CSS Color 4, which the older
half of that population rejects. The answer flips in exactly the UAs a guard is
written to catch.

  $ cat > rgba.css <<EOF
  > .a { opacity: 1 }
  > @supports (color: rgba(0,0,0,.5)) { .b { color: red } }
  > EOF
  $ cascade --minify rgba.css
  .a{opacity:1}@supports(color:rgba(0,0,0,.5)){.b{color:red}}

The same holds with no alpha at all, where the two spellings differ only in the
separator.

  $ cat > rgb.css <<EOF
  > .a { opacity: 1 }
  > @supports (color: rgb(0,0,0)) { .b { color: red } }
  > EOF
  $ cascade --minify rgb.css
  .a{opacity:1}@supports(color:rgb(0,0,0)){.b{color:red}}

And with [hsl()], which has the same pair of grammars.

  $ cat > hsl.css <<EOF
  > .a { opacity: 1 }
  > @supports (color: hsl(0,0%,0%)) { .b { color: red } }
  > EOF
  $ cascade --minify hsl.css
  .a{opacity:1}@supports(color:hsl(0,0%,0%)){.b{color:red}}


# Hex with an alpha channel


An eight-digit hex colour is its own syntax, and a UA that reads six digits does
not necessarily read eight. Dropping a fully opaque alpha channel turns a probe
for the eight-digit form into a probe for the six-digit form it was written to
tell apart.

  $ cat > hexalpha.css <<EOF
  > .a { opacity: 1 }
  > @supports (color: #ff0000ff) { .b { color: red } }
  > EOF
  $ cascade --minify hexalpha.css
  .a{opacity:1}@supports(color:#ff0000ff){.b{color:red}}


# The same class, with no divergence to name


These three change the question the same way. Unlike the colour rows there is no
UA anyone can point at that answers the two spellings differently: [calc()] was
already in every engine before that engine evaluated its first feature query, and
the other two are token spellings of the same age. They are here because the rule
belongs to the construct. A build does not get to decide which of its rewrites a
foreign parser will overlook.

A [calc()] wrapper around a single term is a probe for [calc()].

  $ cat > calc.css <<EOF
  > .a { opacity: 1 }
  > @supports (width: calc(10px)) { .b { color: red } }
  > EOF
  $ cascade --minify calc.css
  .a{opacity:1}@supports(width:calc(10px)){.b{color:red}}

A trailing zero is the author's spelling of the number.

  $ cat > zero.css <<EOF
  > .a { opacity: 1 }
  > @supports (width: 10.0px) { .b { color: red } }
  > EOF
  $ cascade --minify zero.css
  .a{opacity:1}@supports(width:10.0px){.b{color:red}}

A quoted [url()] argument is a function token over a string, and an unquoted one
is a url token: different tokens, not different spacing.

  $ cat > url.css <<EOF
  > .a { opacity: 1 }
  > @supports (background: url("a.png")) { .b { color: red } }
  > EOF
  $ cascade --minify url.css
  .a{opacity:1}@supports(background:url("a.png")){.b{color:red}}


# Two questions must not become one


Rewriting the value is not only a change of spelling. Two guards that ask
different questions can be rewritten into the same one, and then the blocks
behind them merge: a UA answering yes to one and no to the other loses the
distinction the author built.

  $ cat > merge.css <<EOF
  > .a { opacity: 1 }
  > @supports (width: calc(10px)) { .b { color: red } }
  > @supports (width: 10.0px) { .c { color: red } }
  > EOF
  $ cascade --minify merge.css
  .a{opacity:1}@supports(width:calc(10px)){.b{color:red}}@supports(width:10.0px){.c{color:red}}


# The same guard in prefix position


The [supports()] clause of an [@import] carries the same declaration feature and
decides whether the sheet is fetched, so its value is preserved on the same
grounds.

  $ cat > import.css <<EOF
  > @import url("t.css") supports(color: rgba(0,0,0,.5));
  > .a { opacity: 1 }
  > EOF
  $ cascade --minify import.css
  @import"t.css"supports(color:rgba(0,0,0,.5));.a{opacity:1}


# --enforce-spec is not the flag that buys this back


[--enforce-spec] drops the shortenings that depend on an evergreen target. The
condition is not a target question at all, so it is preserved under both.

  $ for f in rgba.css rgb.css hsl.css hexalpha.css calc.css zero.css url.css import.css; do
  >   cascade --minify --enforce-spec $f
  > done
  .a{opacity:1}@supports(color:rgba(0,0,0,.5)){.b{color:red}}
  .a{opacity:1}@supports(color:rgb(0,0,0)){.b{color:red}}
  .a{opacity:1}@supports(color:hsl(0,0%,0%)){.b{color:red}}
  .a{opacity:1}@supports(color:#ff0000ff){.b{color:red}}
  .a{opacity:1}@supports(width:calc(10px)){.b{color:red}}
  .a{opacity:1}@supports(width:10.0px){.b{color:red}}
  .a{opacity:1}@supports(background:url("a.png")){.b{color:red}}
  @import"t.css"supports(color:rgba(0,0,0,.5));.a{opacity:1}


# Controls


These already hold and a fix must not move them.

A [calc()] carrying real arithmetic is not folded, so the guard reaches the UA as
written. A condition already in the modern colour syntax is not rewritten into
the legacy one, a zero length keeps its unit, and a [scale()] with two equal
arguments keeps both.

  $ cat > kept.css <<EOF
  > .a { opacity: 1 }
  > @supports (width: calc(10px*2)) { .b { color: red } }
  > @supports (color: rgb(0 0 0)) { .c { color: red } }
  > @supports (margin: 0px) { .d { color: red } }
  > @supports (transform: scale(1,1)) { .e { color: red } }
  > EOF
  $ cascade --minify kept.css
  .a{opacity:1}@supports(width:calc(10px*2)){.b{color:red}}@supports(color:rgb(0 0 0)){.c{color:red}}@supports(margin:0px){.d{color:red}}@supports(transform:scale(1,1)){.e{color:red}}

A condition naming a property cascade does not model keeps its value byte for
byte, including the two spellings the rows above lose. It is the control the
guarded rows are measured against: what cascade can take apart must not come out
differently from what it cannot.

  $ cat > unknown.css <<EOF
  > .a { opacity: 1 }
  > @supports (nonsense-prop: 10.0px) { .b { color: red } }
  > @supports (nonsense-prop: rgba(0,0,0,.5)) { .c { color: red } }
  > EOF
  $ cascade --minify unknown.css
  .a{opacity:1}@supports(nonsense-prop:10.0px){.b{color:red}}@supports(nonsense-prop:rgba(0,0,0,.5)){.c{color:red}}
