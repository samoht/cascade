CLI: --inline-vars and @property registrations.

CSS Properties and Values API 1 sec. 2 gives a registered property an
[initial-value], used as its computed value wherever no declaration wins,
and an [inherits] descriptor deciding whether it inherits at all. A
registration is therefore dead only once nothing is left for it to govern:
no declaration of the property, and no live var() reading it.

A property the inliner resolved away leaves neither, so its registration
goes with it.

  $ cat > resolved.css <<EOF
  > @property --gap { syntax: "<length>"; inherits: false; initial-value: 16px }
  > .a { padding: var(--gap) }
  > EOF
  $ cascade --minify --inline-vars resolved.css
  .a{padding:16px}

A property the inliner keeps live keeps its registration. Here b declares
no --c: with the registration it computes --c as the initial-value red,
without it var(--c) is invalid at computed-value time and color falls back
to the inherited black, so dropping the registration repaints the page.

  $ cat > kept.css <<EOF
  > @property --c { syntax: "<color>"; inherits: false; initial-value: red }
  > a { --c: #00f }
  > b { color: var(--c) }
  > EOF
  $ cascade --minify --inline-vars kept.css
  Warning: --c is redefined in a different scope; kept live (cannot inline safely)
  @property --c{syntax:"<color>";inherits:false;initial-value:red}a{--c:#00f}b{color:var(--c)}

An inheriting registration is kept on the same terms.

  $ cat > inherit.css <<EOF
  > @property --c { syntax: "<color>"; inherits: true; initial-value: red }
  > a { --c: #00f }
  > b { color: var(--c) }
  > EOF
  $ cascade --minify --inline-vars inherit.css 2> /dev/null
  @property --c{syntax:"<color>";inherits:true;initial-value:red}a{--c:#00f}b{color:var(--c)}

A declaration the inliner cannot prove reaches its consumer keeps the
var() live, so the registration that types that declaration stays too.

  $ cat > scoped.css <<EOF
  > @property --c { syntax: "<color>"; inherits: false; initial-value: red }
  > .theme { --c: #00f }
  > .other { color: var(--c) }
  > EOF
  $ cascade --minify --inline-vars scoped.css 2> /dev/null
  @property --c{syntax:"<color>";inherits:false;initial-value:red}.theme{--c:#00f}.other{color:var(--c)}

A registration nothing declares and nothing reads has nothing to govern in
a closed world, so it goes.

  $ cat > unused.css <<EOF
  > @property --y { syntax: "*"; inherits: false }
  > .b { color: red }
  > EOF
  $ cascade --minify --inline-vars unused.css
  .b{color:red}

Inlining is a rewrite to a fixpoint: a second run over its own output has
to change nothing. Keeping a declaration because a registration made its
property look multi-defined and then deleting that registration breaks
that, since the next run sees a single-definition variable and prunes it.

  $ cat > fix.css <<EOF
  > @property --c { syntax: "<color>"; inherits: false; initial-value: red }
  > a { --c: #00f }
  > EOF
  $ cascade --minify --inline-vars fix.css 2> /dev/null > pass1.css
  $ cat pass1.css
  @property --c{syntax:"<color>";inherits:false;initial-value:red}a{--c:#00f}
  $ cascade --minify --inline-vars pass1.css 2> /dev/null > pass2.css
  $ diff pass1.css pass2.css
