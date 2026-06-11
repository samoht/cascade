CLI: --minify uses a fast low-ROI stylesheet path by default.

Small inputs with useful candidates still run the global optimiser, so existing
size-arbitrage rewrites remain the default for focused stylesheets.

  $ cat > small.css <<EOF
  > .a { color: red }
  > .b { color: red }
  > EOF
  $ cascade --minify small.css
  .a,.b{color:red}

Large low-ROI inputs still use the single optimizer path, but the global
factoring fixpoint is skipped after local linear rewrites.

  $ for i in $(seq 1 5001); do printf '.x%s{width:%spx}\n' "$i" "$i"; done > big.css
  $ cascade --minify big.css | grep -o "{width:" | wc -l | tr -d ' '
  5001

Profiling reports the incremental factoring decision without changing it.

  $ cascade --minify --profile big.css > /dev/null 2>profile.txt
  $ grep "factor preflight" profile.txt
  factor preflight: 1 skipped, estimated gain 0 bytes
