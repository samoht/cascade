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
  Warning: --profile has no effect without --minify

The formatted output on stdout is unaffected: only the stderr report goes
away.

  $ cascade --profile small.css 2>/dev/null
  .a {
    color: red;
  }

`--profile --minify` together must still print the full report: this is the
positive control that must keep working. Two rules sharing a declaration
give the global factoring fixpoint real work, so the report is non-empty and
names its iterations (the header line's counts, and the per-iteration table).

  $ cat > dup.css <<EOF
  > .a { color: red }
  > .b { color: red }
  > EOF
  $ cascade --minify --profile dup.css > /dev/null 2>profile2.txt
  $ grep -o "factor fixpoint ([0-9]* runs, [0-9]* iterations" profile2.txt
  factor fixpoint (2 runs, 2 iterations
  $ grep -c "rules_in" profile2.txt
  1
