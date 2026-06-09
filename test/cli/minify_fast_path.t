CLI: --minify uses a fast large-stylesheet path by default.

Small inputs still run the global optimiser, so existing size-arbitrage
rewrites remain the default for focused stylesheets.

  $ cat > small.css <<EOF
  > .a { color: red }
  > .b { color: red }
  > EOF
  $ cascade --minify small.css
  .a,.b{color:red}

Large inputs stay on the fast minifying serialiser unless an
optimizer-dependent mode is requested. This keeps the binary fast by default:
the output is minified and valid, but repeated equal declarations are not
globally grouped.

  $ for i in $(seq 1 2001); do printf '.x%s{color:red}\n' "$i"; done > big.css
  $ cascade --minify big.css | grep -o "{color:red}" | wc -l | tr -d ' '
  2001

Profiling is an optimizer-dependent mode, so it forces the aggressive
pipeline and reports the factoring fixpoint.

  $ cascade --minify --profile big.css > /dev/null 2>profile.txt
  $ grep "factor fixpoint" profile.txt | sed 's/(.*):/(...):/'
  factor fixpoint (...):
