CLI: --minify uses a fast low-ROI stylesheet path by default.

Small inputs with useful candidates still run the global optimiser, so existing
size-arbitrage rewrites remain the default for focused stylesheets.

  $ cat > small.css <<EOF
  > .a { color: red }
  > .b { color: red }
  > EOF
  $ cascade --minify small.css
  .a,.b{color:red}

Large low-ROI inputs stay on the fast minifying serialiser unless an
optimizer-dependent mode is requested. This keeps the binary fast by default
while still emitting minified, valid CSS.

  $ for i in $(seq 1 2001); do printf '.x%s{width:%spx}\n' "$i" "$i"; done > big.css
  $ cascade --minify big.css | grep -o "{width:" | wc -l | tr -d ' '
  2001

Profiling is an optimizer-dependent mode, so it forces the aggressive
pipeline and reports the factoring fixpoint.

  $ cascade --minify --profile big.css > /dev/null 2>profile.txt
  $ grep "factor fixpoint" profile.txt | sed 's/(.*):/(...):/'
  factor fixpoint (...):
