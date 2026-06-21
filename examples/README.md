# cascade examples

These show cascade used as a renderer-agnostic **cascade engine**: selector
matching plus specificity resolution projected onto a tree, rather than just
parsing and printing CSS text.

## `css-resolve`

Projects a global stylesheet onto each node of a plain OCaml tree (no DOM):
selector match + specificity cascade + last-wins gives every node its resolved
declarations. Pure cascade, no extra dependencies.

```
dune exec examples/css-resolve/main.exe
```

## `css-inline`

A mode where cascade takes an HTML page (plus an optional extra `.css`) and
emits **fully resolved HTML**, with the matched declarations written into each
element's `style=""` attribute (premailer / juice style). cascade is the
cascade engine and also minifies each inline style; [lambdasoup][] only parses
and serialises the HTML, since HTML is outside cascade's scope.

```
dune exec examples/css-inline/main.exe -- page.html [extra.css]
```

It inlines statically-matchable rules (type, class, id, attribute, descendant /
child / sibling combinators, structural pseudo-classes). Rules that cannot be
inlined (`:hover`, media queries, pseudo-elements) are left in the kept
`<style>`. Author inline styles keep their priority. Inheritance and
computed-value resolution (`var()` / `calc()`) are not expanded yet.

[lambdasoup]: https://github.com/aantron/lambdasoup
