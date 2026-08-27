CLI: --minify keeps every author-written @supports guard.

CSS Conditional 5 sec. 2 evaluates an [@supports] condition in the UA that
renders the stylesheet. That is the construct's only purpose: the author ships
a fallback and an enhancement in one sheet, and each UA picks. Deciding the
condition at build time against an assumed support table deletes the fallback
path the guard was written for, so the default [--minify] keeps the guard
whatever cascade knows about the feature.

[--enforce-spec] is not the flag that buys this back. It also drops the
vendor-prefix and direction facts, which are sound and which a build that only
wants its feature queries left alone still wants.


# Guards that survive


A guard on a widely-available feature is still a guard: the author asked the
UA, not the build.

  $ cat > grid.css <<EOF
  > .a { opacity: 1 }
  > @supports (display: grid) { .b { color: red } }
  > EOF
  $ cascade --minify grid.css
  .a{opacity:1}@supports(display:grid){.b{color:red}}

A [color-mix()] probe is a feature test, not a colour. The condition asks
whether the UA parses that exact declaration, so the value inside it is not
folded: resolving [color-mix(in lab, red, red)] to [red] would ask a question
every UA answers yes to.

  $ cat > color-mix.css <<EOF
  > .a { opacity: 1 }
  > @supports (color: color-mix(in lab, red, red)) { .b { color: red } }
  > EOF
  $ cascade --minify color-mix.css
  .a{opacity:1}@supports(color:color-mix(in lab,red,red)){.b{color:red}}

A condition naming a property cascade knows nothing about is kept. It is the
control for the rows above and below: what cascade can decide must not differ
from what it cannot.

  $ cat > unknown.css <<EOF
  > .a { opacity: 1 }
  > @supports (nonsense-prop: 1px) { .b { color: red } }
  > EOF
  $ cascade --minify unknown.css
  .a{opacity:1}@supports(nonsense-prop:1px){.b{color:red}}

A negated guard selects exactly the UAs that lack the feature, which is the
whole population a fallback is for, so its block is not dead. The outer parens
of [(not (display: grid))] are redundant per the Conditional 5 grammar, where
[not <supports-in-parens>] is itself a [<supports-condition>], so the shortest
spelling drops them; the space before [(] stays, since [not(] lexes as a
function token.

  $ cat > negated.css <<EOF
  > .a { opacity: 1 }
  > @supports (not (display: grid)) { .b { color: red } }
  > EOF
  $ cascade --minify negated.css
  .a{opacity:1}@supports not (display:grid){.b{color:red}}

The [supports()] clause of an [@import] is the same guard in prefix position:
it decides whether the imported sheet is fetched at all, so dropping it turns
a conditional import into an unconditional one.

  $ cat > import.css <<EOF
  > @import url("grid.css") supports(display: grid);
  > .a { opacity: 1 }
  > EOF
  $ cascade --minify import.css
  @import"grid.css"supports(display:grid);.a{opacity:1}


# The fallback chain the guards protect


Tailwind's preflight writes the placeholder colour twice: [currentColor] for
every UA, then a [color-mix()] override behind a probe for [color-mix()]
itself. A UA that cannot parse the override drops that one declaration and
keeps [currentColor]. Collapse the guards and it is left with no placeholder
colour at all.

  $ cat > preflight.css <<EOF
  > ::placeholder { opacity: 1 }
  > @supports (not ((-webkit-appearance: -apple-pay-button))) or (contain-intrinsic-size: 1px) {
  >   ::placeholder { color: currentColor }
  >   @supports (color: color-mix(in lab, red, red)) {
  >     ::placeholder { color: color-mix(in oklab, currentcolor 50%, transparent) }
  >   }
  > }
  > EOF
  $ cascade --minify preflight.css > out.css

The assertions are structural: what has to hold is that each declaration is
still behind the guard the author put it behind. Whether the optimiser leaves
the guarded rule flat or nests it into the rule above, and how it spells the
[currentColor] keyword, are its own choices.

Both guards survive, the outer probe included.

  $ grep -o "@supports" out.css | wc -l | tr -d ' '
  2
  $ grep -o "contain-intrinsic-size:1px" out.css | wc -l | tr -d ' '
  1

The unguarded fallback survives, so a UA that fails the inner probe still has
a placeholder colour.

  $ grep -oiE "color:currentcolor[;}]" out.css | wc -l | tr -d ' '
  1

The override stays inside the probe rather than being hoisted over it.

  $ grep -oE "@supports\(color:color-mix\(in lab,red,red\)\)\{[^}]*color-mix\(in oklab" out.css | wc -l | tr -d ' '
  1
