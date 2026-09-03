CLI: cascade diff - size summary with an empty expected side.

When the first file is empty there is no denominator for a percentage, so the
summary line shows only the two sizes.

  $ printf '' > empty.css
  $ printf '.a{color:red}\n' > a.css
  $ NO_COLOR=1 cascade diff empty.css a.css | head -1
  CSS: 0 chars vs 14 chars

When the second file is empty the denominator is non-empty, so the percentage
is still printed.

  $ NO_COLOR=1 cascade diff a.css empty.css | head -1
  CSS: 14 chars vs 0 chars (100.0% diff)
