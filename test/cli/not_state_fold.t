CLI: --minify keeps :not() over a state pseudo-class it cannot scope.

CSS Selectors 4 sec. 12.1.1 says "In a typical document most elements will be
neither :enabled nor :disabled", so `<p class=c>` matches `.c:not(:enabled)`
and does not match `.c:disabled`. Rewriting one to the other drops the rule on
every element that carries no enabled state.

  $ cat > class.css <<EOF
  > .c:not(:enabled) { color: red }
  > EOF
  $ cascade fmt --minify class.css
  .c:not(:enabled){color:red}

A type selector that names an element HTML gives the state to does scope it.
HTML sec. 4.16.3 makes every `input` either `:enabled` or `:disabled`.

  $ cat > input.css <<EOF
  > input:not(:enabled) { color: red }
  > EOF
  $ cascade fmt --minify input.css
  input:disabled{color:red}

Validity and optionality reach a narrower set. An `input` is `:valid` or
`:invalid` only while it is a candidate for constraint validation, which a
disabled one is not, and `:optional` wants an `input` "to which the required
attribute applies", which `type=hidden` is not.

  $ cat > narrow.css <<EOF
  > input:not(:invalid) { color: red }
  > input:not(:required) { padding: 0 }
  > EOF
  $ cascade fmt --minify narrow.css
  input:not(:invalid){color:red}input:not(:required){padding:0}

Selectors 4 sec. 12.1.1 leaves the enabled and disabled states to the host
language, so it is HTML, not the CSS text, that puts `input` on the list.

  $ cascade fmt --minify --enforce-spec input.css
  input:not(:enabled){color:red}
