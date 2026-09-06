CLI: [cascade prune] removes the rules the documents cannot match.

A rule is removed only when the matcher has a model for its selector
and every element of every document answers No_match. Selectors 4
describes far more than a matcher with no browser behind it can decide,
which is why resolve.mli splits the negative answer in two: Unsupported
is not "does not match", so a selector with no model is kept and is
never counted as unused.

  $ cat > page.html <<EOF
  > <html><head></head><body>
  > <div class="card"><p>one</p></div>
  > <p>two</p>
  > </body></html>
  > EOF


# What goes and what stays


[.unused] matches no element of the page, so it goes. [:root] matches
the [<html>] element (Selectors 4 sec. 8.1), [p] matches two elements,
and [.card:hover] is a stateful pseudo-class the matcher has no model
for, so it is kept without a verdict rather than ruled out.

  $ cat > basic.css <<EOF
  > :root { --brand: red }
  > .card { color: var(--brand) }
  > .unused { color: blue }
  > p { margin: 0 }
  > .card:hover { color: green }
  > EOF
  $ cascade prune page.html basic.css
  rules: 5 total, 1 removed, 3 kept as used, 1 kept without a verdict
  :root {
    --brand: red;
  }
  .card {
    color: var(--brand);
  }
  p {
    margin: 0;
  }
  .card:hover {
    color: green;
  }

A rule live in any one of the documents survives. [.badge] is nowhere
in the first page and [.card] is nowhere in the second.

  $ cat > other.html <<EOF
  > <html><head></head><body><span class="badge">b</span></body></html>
  > EOF
  $ cat > two.css <<EOF
  > .badge { color: red }
  > .card { color: blue }
  > .neither { color: green }
  > EOF
  $ cascade prune page.html other.html two.css
  rules: 3 total, 1 removed, 2 kept as used, 0 kept without a verdict
  .badge {
    color: red;
  }
  .card {
    color: blue;
  }


# A condition is not a selector


[@media], [@supports] and [@container] ask about the device, the user
agent and a layout container, none of which a document carries. So the
condition is never evaluated: the rules inside are judged by their own
selectors, and [@media print] survives because [.card] matches. A group
block left with nothing goes with its rules; a [@layer] block stays,
emptied or not, because it declares a layer that orders the cascade.

  $ cat > cond.css <<EOF
  > @media print { .card { color: black } }
  > @supports (display: grid) { .nowhere { display: grid } }
  > @layer base { .card { margin: 0 } .nowhere { margin: 1px } }
  > EOF
  $ cascade prune page.html cond.css
  rules: 4 total, 2 removed, 2 kept as used, 0 kept without a verdict
  @media print {
    .card {
      color: black;
    }
  }
  @layer base {
    .card {
      margin: 0;
    }
  }


# A statement with no selector is never unused


[@charset], [@import], [@layer], [@font-face], [@keyframes] and
[@property] name nothing this analysis can rule out: they register a
font, a name or a property, and a document says nothing about whether
they are reached. Only [.gone] is decided here.

  $ cat > atrules.css <<EOF
  > @charset "UTF-8";
  > @import url("theme.css");
  > @layer base, theme;
  > @font-face { font-family: Brand; src: url("brand.woff2") }
  > @keyframes fade { from { opacity: 0 } }
  > @property --gap { syntax: "<length>"; inherits: false; initial-value: 1rem }
  > .gone { color: red }
  > EOF
  $ cascade prune page.html atrules.css
  rules: 1 total, 1 removed, 0 kept as used, 0 kept without a verdict
  @charset "UTF-8";
  @import "theme.css";
  @layer base, theme;
  @font-face {
    font-family: Brand;
    src: url("brand.woff2");
  }
  @keyframes fade {
    from {
      opacity: 0;
    }
  }
  @property --gap {
    syntax: "<length>";
    inherits: false;
    initial-value: 1rem;
  }


# A selector list is only as modelled as its least modelled branch


Each branch of a selector list matches on its own and carries its own
specificity (Selectors 4 sec. 3.3, sec. 17), so a branch that matches
nothing can go while the rest of the rule stays.

  $ cat > list.css <<EOF
  > .card, .nowhere { color: red }
  > EOF
  $ cascade prune page.html list.css
  rules: 1 total, 0 removed, 1 kept as used, 0 kept without a verdict
  .card {
    color: red;
  }

One unmodelled branch leaves the whole list without a verdict, so the
rule is kept as written. Dropping [.nowhere] here would rest on a
No_match the matcher never gave for the list it belongs to.

  $ cat > mixed.css <<EOF
  > .nowhere, .card:hover { color: red }
  > EOF
  $ cascade prune page.html mixed.css
  rules: 1 total, 0 removed, 0 kept as used, 1 kept without a verdict
  .nowhere, .card:hover {
    color: red;
  }


# A custom property a removed rule declares


A custom property reaches an element through a rule that matched it, or
by inheritance from an ancestor a rule matched. [.theme-dark] matched
nothing, so nothing in this document ever carried [--bg] and
[var(--bg)] already resolved to its guaranteed-invalid value: removing
the declaration changes no computed value here. [:root] is the case
that matters in practice, and it survives on [<html>].

  $ cat > vars.css <<EOF
  > :root { --brand: red }
  > .theme-dark { --bg: black }
  > body { background: var(--bg); color: var(--brand) }
  > EOF
  $ cascade prune page.html vars.css
  rules: 3 total, 1 removed, 2 kept as used, 0 kept without a verdict
  :root {
    --brand: red;
  }
  body {
    background: var(--bg);
    color: var(--brand);
  }


# :empty is kept for want of a shipped model


Selectors 4 sec. 13.2 matches an element holding nothing but document
white space, and its own note records that Level 2 and Level 3 did
not. No engine has taken that change, so a verdict here would rest on
what the specification says rather than on what the browser applies,
and removing a rule on it would delete one the page still uses.

  $ cat > empty-page.html <<EOF
  > <html><head></head><body>
  > <p id="a"></p>
  > <p id="b"><!-- c --></p>
  > <p id="c">x</p>
  > </body></html>
  > EOF
  $ cat > empty.css <<EOF
  > p:empty { color: red }
  > EOF
  $ cascade prune --dry-run empty-page.html empty.css
  documents: 1, elements: 6
  
  no verdict, kept: the matcher has no model for the selector.
    p:empty
  
  rules: 1 total, 0 removed, 0 kept as used, 1 kept without a verdict
  
  Unused means unused in these documents: a class a script adds at runtime is not in them.


# The ranked report


[--dry-run] writes the ranking instead of the pruned stylesheet: what
would go, then what stays ordered by how few elements it matched, then
what was kept for want of a model. The last count is the measure of
what the analysis cannot see.

  $ cascade prune --dry-run page.html basic.css
  documents: 1, elements: 6
  
  unused, removed by a run without --dry-run:
    .unused
  
  used, fewest matched elements first:
       1  :root
       1  .card
       2  p
  
  no verdict, kept: the matcher has no model for the selector.
    .card:hover
  
  rules: 5 total, 1 removed, 3 kept as used, 1 kept without a verdict
  
  Unused means unused in these documents: a class a script adds at runtime is not in them.


# Nothing to match against


A document set with no element makes every rule look unused, which is
an answer about the input rather than about the stylesheet.

  $ printf '' > blank.html
  $ cascade prune blank.html basic.css
  Error: the documents hold no element, so every rule would look unused
  [1]
  $ cascade prune --dry-run blank.html basic.css
  Error: the documents hold no element, so every rule would look unused
  [1]
