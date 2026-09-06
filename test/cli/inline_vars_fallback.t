CLI: --inline-vars - fallback resolution.

A var() reference whose name cannot resolve drops the wrapper and emits
the fallback. The fallback canonicalizes through the same value pipeline
as a directly-written value.

  $ cat > basic.css <<EOF
  > .a { color: var(--undef, red) }
  > .b { color: var(--undef, #ff0000) }
  > .c { color: var(--undef, transparent) }
  > EOF
  $ cascade --minify --inline-vars basic.css
  .a,.b{color:red}.c{color:#0000}

Nested var() in fallback chains through to the deepest defined value. A
deepest value the property refuses leaves the declaration invalid at
computed-value time, which is neither the refused text nor the
declaration's absence, so the reference stays for the browser to answer.

  $ cat > nested.css <<EOF
  > .a { color: var(--undef, var(--also-undef, blue)) }
  > .b { color: var(--undef, var(--also-undef, var(--third, fallback))) }
  > EOF
  $ cascade --minify --inline-vars nested.css
  .a{color:#00f}.b{color:var(--undef,var(--also-undef,var(--third,fallback)))}

A var() with no fallback that cannot resolve preserves the var()
verbatim - the spec says the declaration is invalid at computed time;
the syntax-layer printer keeps it for the cascade engine to handle.

  $ cat > no-fb.css <<EOF
  > .a { color: var(--undef) }
  > EOF
  $ cascade --minify --inline-vars no-fb.css
  .a{color:var(--undef)}

A fallback containing a calc() reduces after the var() wrapper drops.

  $ cat > calc-fb.css <<EOF
  > .a { width: var(--undef, calc(1px + 2px)) }
  > .b { width: var(--undef, calc(var(--gap, 8px) * 2)) }
  > EOF
  $ cascade --minify --inline-vars calc-fb.css
  .a{width:3px}.b{width:16px}

A multi-comma fallback list (font-family fallback chain) is preserved -
commas inside the fallback are part of the fallback's token stream per
Custom Properties L1 §2. A token stream the destination property refuses
leaves the declaration invalid at computed-value time, so the reference
stays.

  $ cat > list.css <<EOF
  > .a { font-family: var(--font, "Helvetica Neue", sans-serif) }
  > .b { color: var(--undef, red, blue) }
  > EOF
  $ cascade --minify --inline-vars list.css
  .a{font-family:Helvetica Neue,sans-serif}.b{color:var(--undef,red,blue)}

An empty fallback substitutes to an empty value; for a property that
does not accept one the declaration is invalid at computed-value time,
so the reference stays.

  $ cat > empty.css <<EOF
  > .a { color: var(--undef,) }
  > EOF
  $ cascade --minify --inline-vars empty.css 2>&1 | grep -v "warning" || true
  .a{color:var(--undef,)}

A non-trivial fallback is NOT eagerly inlined when the var() name
resolves elsewhere - only an unresolvable reference emits the fallback.

  $ cat > resolved.css <<EOF
  > :root { --gap: 10px }
  > .a { padding: var(--gap, 20px) }
  > EOF
  $ cascade --minify --inline-vars resolved.css
  .a{padding:10px}

Various dimension-typed fallbacks canonicalize after substitution.

  $ cat > dims.css <<EOF
  > .a { transform: rotate(var(--undef, 90deg)) }
  > .b { animation-duration: var(--undef, 0.5s) }
  > .c { width: var(--undef, calc(50% + 10px)) }
  > EOF
  $ cascade --minify --inline-vars dims.css
  .a{transform:rotate(90deg)}.b{animation-duration:.5s}.c{width:calc(50% + 10px)}
