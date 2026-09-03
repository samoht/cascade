The diff command gets option rows from Cmdliner rather than repeating them in
the prose manpage.

  $ cascade diff --help=plain \
  > | grep -E '^       --(diff|limit|lossless)(=|$)' \
  > | sed -E 's/^ +//'
  --diff=MODE (absent=auto)
  --limit=LIMIT (absent=auto)
  --lossless

Command exit statuses have one authoritative entry apiece.

  $ for command in apply diff fmt prune; do
  >   cascade "$command" --help=plain \
  >   | grep -E '^       (0|1|123|124|125) ' \
  >   | sed -E 's/^ +([0-9]+).*/\1/' \
  >   | sort | uniq -d | sed "s/^/$command: /"
  > done
  > echo unique
  unique

Prune help asks the resolver which representative selectors it models, so the
description cannot classify :nth-child() as unsupported again.

  $ cascade prune --help=plain \
  > | tr '\n' ' ' | tr -s ' ' \
  > | grep -o 'modelled examples: [^;]*; unmodelled examples: [^.]*\.'
  modelled examples: .item, :nth-child(2n+1), :has(.child); unmodelled examples: :hover, ::before.
