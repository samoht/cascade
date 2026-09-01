Every normal interop suite carries its own pinned provenance and authoritative
license notice. Regeneration is explicit, gated, and promotes declared trace
targets; an ignored local corpus can never silently alter a normal test run.

  $ interop="$PWD/../../../test/interop"
  $ for corpus in "$interop"/*; do
  >   test -d "$corpus" || continue
  >   name=${corpus##*/}
  >   if ! test -f "$corpus/UPSTREAM.md"; then
  >     echo "$name: missing UPSTREAM.md"
  >   fi
  >   license=
  >   for notice in LICENSE LICENSE.md COPYING COPYING.md THIRD_PARTY.md; do
  >     if test -f "$corpus/$notice"; then license="$corpus/$notice"; break; fi
  >   done
  >   if test -z "$license"; then
  >     echo "$name: missing license notice"
  >   fi
  >   regen="REGEN=1 dune build @test/interop/$name/regen-traces"
  >   if test -f "$corpus/UPSTREAM.md" && ! grep -Fq "$regen" "$corpus/UPSTREAM.md"; then
  >     echo "$name: missing exact regeneration command"
  >   fi
  >   if ! grep -Fq 'UPSTREAM.md' "$corpus/dune"; then
  >     echo "$name: provenance absent from test dependencies"
  >   fi
  >   if test -n "$license" && ! grep -Fq "${license##*/}" "$corpus/dune"; then
  >     echo "$name: license absent from test dependencies"
  >   fi
  >   if ! grep -Fq '(alias traces/regen)' "$corpus/dune"; then
  >     echo "$name: regen alias bypasses gated trace rule"
  >   fi
  >   if ! grep -Fq '(targets' "$corpus/traces/dune"; then
  >     echo "$name: regen rule has no declared targets"
  >   fi
  >   if ! grep -Fq '%{env:REGEN=0}' "$corpus/traces/dune"; then
  >     echo "$name: regen rule is not REGEN-gated"
  >   fi
  >   if ! grep -Fq '(mode promote)' "$corpus/traces/dune"; then
  >     echo "$name: regen rule does not promote its targets"
  >   fi
  >   if test -f "$corpus/traces/.gitignore" && grep -Eq '^\*$' "$corpus/traces/.gitignore"; then
  >     echo "$name: normal tests can consume ignored local traces"
  >   fi
  > done

Cached failure diagnostics must not embed the regeneration host's npm path or
Node version.

  $ if grep -Eq '(/Users/|/home/).*(node_modules|node-versions)|Node[.]js v[0-9]' "$interop/lightning/traces/minify.pairs"; then
  >   echo "lightning: trace contains host-specific failure metadata"
  > fi
