CLI: cascade diff and fmt - a bang comment inside a rule.

A `/*! ... */` comment between statements is kept as a statement of its
own. One written inside a rule belongs to that rule's text, and comments
there are discarded like any other, so it never becomes a sibling
statement. @tailwindcss/forms emits exactly this value for
`--tw-ring-inset`.

  $ cat > forms.css <<EOF
  > .a { --x: var(--tw-empty,/*!*/ /*!*/); }
  > EOF
  $ cat > plain.css <<EOF
  > .a { --x: var(--tw-empty,); }
  > EOF
  $ cascade fmt forms.css
  .a {
    --x: var(--tw-empty, );
  }
  $ cascade diff --diff=canonical forms.css plain.css
  CSS files are identical

A bang comment between two rules is still kept, in place.

  $ cat > license.css <<EOF
  > .a { color: red } /*! license */ .b { color: blue }
  > EOF
  $ cascade fmt license.css
  .a {
    color: red;
  }
  /*! license */
  .b {
    color: blue;
  }
