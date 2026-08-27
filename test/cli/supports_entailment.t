CLI: --minify simplifies a nested @supports condition against its context.

CSS Conditional Rules 3 sec. 6 evaluates a condition as a two-valued boolean
over feature tests: [not] negates, [and] is true when every term is true, [or]
is false when every term is false, and a [<general-enclosed>] term the UA does
not understand is false rather than unknown. A condition is therefore a
propositional formula over its feature tests, and classical entailment holds
over it.

Inside [@supports (A)] the UA has already answered [A], and one sheet gets the
same answer to the same question everywhere in it. An inner condition can be
simplified against the conjunction [K] of the conditions enclosing it, by
propositional logic alone and against no support table. Writing [C] for the
inner condition: [K and not C] unsatisfiable means [C] is entailed, so the
inner guard goes; [K and C] unsatisfiable means [C] is refuted, so the inner
block is dead; otherwise the guard stays, narrowed to what [K] leaves.

This is the simplification the removal of the baseline fold leaves room for. It
asks the support table nothing, so it holds whatever the UA answers.

Atom identity is syntactic: two feature tests are the same variable when they
are structurally equal after parsing. Nothing below asks cascade to decide that
two different spellings probe the same thing.

Most rows probe properties cascade models nothing about, so that entailment is
the only thing that can decide them. The [display] and [text-wrap] rows repeat
the same shapes with properties cascade knows in full: what it can decide must
not differ from what it cannot.


# The inner condition is entailed


An inner guard repeating its enclosing guard asks a question already answered
yes. Its block applies exactly where the enclosing block applies, so the guard
goes and its contents stay.

  $ cat > same.css <<EOF
  > @supports (nonsense-a: 1px) {
  >   @supports (nonsense-a: 1px) { .r { top: 0 } }
  > }
  > EOF
  $ cascade --minify same.css
  @supports(nonsense-a:1px){.r{top:0}}

A conjunction entails each of its conjuncts, so under [(A) and (B)] the inner
[(A)] is already answered yes.

  $ cat > conjunct.css <<EOF
  > @supports (nonsense-a: 1px) and (nonsense-b: 2px) {
  >   @supports (nonsense-a: 1px) { .r { top: 0 } }
  > }
  > EOF
  $ cascade --minify conjunct.css
  @supports(nonsense-a:1px)and (nonsense-b:2px){.r{top:0}}

A disjunct entails its disjunction, so under [(A)] the inner [(A) or (B)] is
true whatever the UA answers about [B]. [B] does not survive into the output:
nothing left in the sheet depends on it.

  $ cat > weaker.css <<EOF
  > @supports (nonsense-a: 1px) {
  >   @supports (nonsense-a: 1px) or (nonsense-b: 2px) { .r { top: 0 } }
  > }
  > EOF
  $ cascade --minify weaker.css
  @supports(nonsense-a:1px){.r{top:0}}

Operand order is not part of the question. [(A) and (B)] and [(B) and (A)] have
the same truth table, so the inner guard is entailed and the enclosing one
keeps the author's order.

  $ cat > commuted.css <<EOF
  > @supports (nonsense-a: 1px) and (nonsense-b: 2px) {
  >   @supports (nonsense-b: 2px) and (nonsense-a: 1px) { .r { top: 0 } }
  > }
  > EOF
  $ cascade --minify commuted.css
  @supports(nonsense-a:1px)and (nonsense-b:2px){.r{top:0}}

De Morgan is the same truth table too: [not ((A) and (B))] and
[(not (A)) or (not (B))] are false on exactly the same UAs.

  $ cat > demorgan.css <<EOF
  > @supports (not ((nonsense-a: 1px) and (nonsense-b: 2px))) {
  >   @supports ((not (nonsense-a: 1px)) or (not (nonsense-b: 2px))) {
  >     .r { top: 0 }
  >   }
  > }
  > EOF
  $ cascade --minify demorgan.css
  @supports not ((nonsense-a:1px)and (nonsense-b:2px)){.r{top:0}}

A property cascade knows in full behaves the same way. The transform reads the
condition's shape, not a support table, so knowing what [display: grid] means
changes nothing.

  $ cat > same-real.css <<EOF
  > @supports (display: grid) {
  >   @supports (display: grid) { .r { top: 0 } }
  > }
  > EOF
  $ cascade --minify same-real.css
  @supports(display:grid){.r{top:0}}


# The inner condition is refuted


An inner guard negating its enclosing guard applies on no UA at all: [A and not
A] is false everywhere. Its block is dead, and the enclosing block is then
empty.

  $ cat > negated.css <<EOF
  > @supports (nonsense-a: 1px) {
  >   @supports (not (nonsense-a: 1px)) { .r { top: 0 } }
  > }
  > EOF
  $ cascade --minify negated.css

Again with a property cascade knows in full.

  $ cat > negated-real.css <<EOF
  > @supports (display: grid) {
  >   @supports (not (display: grid)) { .r { top: 0 } }
  > }
  > EOF
  $ cascade --minify negated-real.css


# A condition that decides itself


With nothing enclosing it, [K] is the empty conjunction and the same two tests
still apply. [(A) or (not (A))] is true on every UA, so its guard selects the
whole population and carries no information.

  $ cat > tautology.css <<EOF
  > @supports (nonsense-a: 1px) or (not (nonsense-a: 1px)) { .r { top: 0 } }
  > EOF
  $ cascade --minify tautology.css
  .r{top:0}

[(A) and (not (A))] is false on every UA, so its block never applies.

  $ cat > contradiction.css <<EOF
  > @supports (nonsense-a: 1px) and (not (nonsense-a: 1px)) { .r { top: 0 } }
  > EOF
  $ cascade --minify contradiction.css


# What the context leaves of the inner condition


Under [(A)], the inner [(A) and (B)] is neither entailed (a UA can answer yes
to [A] and no to [B]) nor refuted (one can answer yes to both). What is left to
ask is [(B)], so that is what the inner guard becomes. The shape of the result
is the one the unrelated-conditions control below pins: two nested guards.

  $ cat > residual.css <<EOF
  > @supports (nonsense-a: 1px) {
  >   @supports (nonsense-a: 1px) and (nonsense-b: 2px) { .r { top: 0 } }
  > }
  > EOF
  $ cascade --minify residual.css
  @supports(nonsense-a:1px){@supports(nonsense-b:2px){.r{top:0}}}

The residual again with properties cascade knows in full.

  $ cat > residual-real.css <<EOF
  > @supports (display: grid) {
  >   @supports (display: grid) and (text-wrap: balance) { .r { top: 0 } }
  > }
  > EOF
  $ cascade --minify residual-real.css
  @supports(display:grid){@supports(text-wrap:balance){.r{top:0}}}


# The context travels down


Whether the UA supports a declaration does not depend on the medium, so an
[@media] between the two guards does not reopen the question.

  $ cat > through-media.css <<EOF
  > @supports (nonsense-a: 1px) {
  >   @media print {
  >     @supports (nonsense-a: 1px) { .r { top: 0 } }
  >   }
  > }
  > EOF
  $ cascade --minify through-media.css
  @supports(nonsense-a:1px){@media print{.r{top:0}}}

Nor does it depend on which cascade layer the rule lands in, so an [@layer]
between the two guards does not reopen it either.

  $ cat > through-layer.css <<EOF
  > @supports (nonsense-a: 1px) {
  >   @layer x {
  >     @supports (nonsense-a: 1px) { .r { top: 0 } }
  >   }
  > }
  > EOF
  $ cascade --minify through-layer.css
  @supports(nonsense-a:1px){@layer x{.r{top:0}}}

A style rule between the two guards does not reopen it either: it selects
elements, and the UA's answer is the same for all of them. Dropping the inner
guard leaves its declaration in the rule that held it, after the declaration
already there.

  $ cat > through-rule.css <<EOF
  > @supports (nonsense-a: 1px) {
  >   .r {
  >     top: 0;
  >     @supports (nonsense-a: 1px) { left: 0 }
  >   }
  > }
  > EOF
  $ cascade --minify through-rule.css
  @supports(nonsense-a:1px){.r{top:0;left:0}}


# Controls: conditions that must not move


These rows pass before the transform exists. They are the boundary it must not
cross.

Two unrelated conditions say nothing about each other, so both guards stay.
This is also the shape the residual row above is expected to reach.

  $ cat > unrelated.css <<EOF
  > @supports (nonsense-a: 1px) {
  >   @supports (nonsense-b: 2px) { .r { top: 0 } }
  > }
  > EOF
  $ cascade --minify unrelated.css
  @supports(nonsense-a:1px){@supports(nonsense-b:2px){.r{top:0}}}

A disjunction does not entail its disjuncts: [(A) or (B)] is true on a UA that
answers yes to [B] and no to [A], and there the inner [(A)] still has to be
asked.

  $ cat > disjunction.css <<EOF
  > @supports (nonsense-a: 1px) or (nonsense-b: 2px) {
  >   @supports (nonsense-a: 1px) { .r { top: 0 } }
  > }
  > EOF
  $ cascade --minify disjunction.css
  @supports(nonsense-a:1px)or (nonsense-b:2px){@supports(nonsense-a:1px){.r{top:0}}}

Context does not leak sideways. The second block is not inside the first, so a
UA that answered no to the first still has to evaluate the second in full.

  $ cat > siblings.css <<EOF
  > @supports (nonsense-a: 1px) { .x { top: 0 } }
  > @supports (nonsense-a: 1px) and (nonsense-b: 2px) { .y { left: 0 } }
  > EOF
  $ cascade --minify siblings.css
  @supports(nonsense-a:1px){.x{top:0}}@supports(nonsense-a:1px)and (nonsense-b:2px){.y{left:0}}

The same in its sharpest form: a guard and its negation as siblings are the
fallback pattern, and both blocks apply, each on its own half of the
population.

  $ cat > siblings-negated.css <<EOF
  > @supports (nonsense-a: 1px) { .x { top: 0 } }
  > @supports (not (nonsense-a: 1px)) { .y { left: 0 } }
  > EOF
  $ cascade --minify siblings-negated.css
  @supports(nonsense-a:1px){.x{top:0}}@supports not (nonsense-a:1px){.y{left:0}}

Two feature tests on the same property are still two variables. A UA that
accepts [display: grid] need not accept [display: inline-grid], so the inner
guard is neither entailed nor refuted.

  $ cat > same-property.css <<EOF
  > @supports (display: grid) {
  >   @supports (display: inline-grid) { .r { top: 0 } }
  > }
  > EOF
  $ cascade --minify same-property.css
  @supports(display:grid){@supports(display:inline-grid){.r{top:0}}}
