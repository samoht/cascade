CLI: --profile without --minify prints only its warning.

`--profile` reports the optimizer's factoring-fixpoint recorder, but that
recorder is created unconditionally, before the code even checks `--minify`.
Without `--minify`, `Css.optimize` (the only thing that ever touches the
recorder) never runs, so the recorder still holds zero counters and an empty
iteration list - and used to be printed anyway, as an empty report sitting
right under a warning that says the flag has no effect. Only one of those two
can be right; the warning is correct, so the empty report must not print.

  $ cat > small.css <<EOF
  > .a { color: red }
  > EOF
  $ cascade --profile small.css > /dev/null 2>profile.txt
  $ cat profile.txt
  cascade: warning: --profile has no effect without --minify

The formatted output on stdout is unaffected: only the stderr report goes
away.

  $ cascade --profile small.css 2>/dev/null
  .a {
    color: red;
  }

`--profile --minify` together must still print the full report: this is the
positive control that must keep working. Two rules sharing a declaration
give the global factoring fixpoint real work, so the report is non-empty and
counts its runs, then gives one row per run: the rules and bytes it started
from, the rules it ended with, what it saved, whether it changed anything and
its time. It opens with where the run spent its time.

  $ cat > dup.css <<EOF
  > .a { color: red }
  > .b { color: red }
  > EOF
  $ cascade --minify --profile dup.css > /dev/null 2>profile2.txt
  $ grep -o "factor fixpoint ([0-9]* runs," profile2.txt
  factor fixpoint (1 runs,
  $ grep "rules-in" profile2.txt
    run  rules-in rules-out  bytes-in     saved changed     time
  $ head -1 profile2.txt | sed -E 's/[0-9]+\.[0-9]+s/Ns/g'
  time: read Ns, optimize Ns, print Ns, total Ns

The footer counts the declaration blocks the nested-merge safety check looked
up. Neither rule above carries a nested block, so none is looked up. Two
`.q` rules, the first carrying a nested block, put the second rule's block to
that nested body once, and merging them leaves no second rule to look up again.

  $ grep "nested-merge probes" profile2.txt
  nested-merge probes: 0
  $ cat > nested.css <<EOF
  > .q { top: 0; &:hover { color: red } }
  > .q { left: 0 }
  > EOF
  $ cascade --minify --profile nested.css > /dev/null 2>profile3.txt
  $ grep "nested-merge probes" profile3.txt
  nested-merge probes: 1
