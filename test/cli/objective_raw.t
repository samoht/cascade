CLI: one size/speed dial. --objective=raw subsumes the old --aggressive flag.

The aggressive global-factoring fixpoint only pays for uncompressed output (the
transfer gate discards its extra raw-byte wins as gzip growth), so it is folded
into --objective=raw rather than exposed as a separate flag.

--aggressive is no longer accepted.

  $ printf '.a{color:red}\n' > style.css
  $ cascade --minify --aggressive style.css >/dev/null 2>&1
  [124]
  $ cascade --minify --aggressive style.css 2>&1 | grep -c aggressive
  1

The default transfer objective skips the low-ROI factoring fixpoint on a large
input the preflight predicts no compressed gain from.

  $ for i in $(seq 1 1100); do printf '.x%s{width:%spx}\n' "$i" "$i"; done > big.css
  $ cascade --minify --profile big.css > /dev/null 2>profile.txt
  $ grep "factor preflight" profile.txt
  factor preflight: 1 skipped, estimated gain 0 bytes

--objective=raw drives that same fixpoint to convergence instead of skipping it.

  $ cascade --minify --objective=raw --profile big.css > /dev/null 2>profile-raw.txt
  $ grep "factor preflight" profile-raw.txt
  factor preflight: 0 skipped, estimated gain 0 bytes

The transfer objective merges rules only within 128 printed rules of each
other; raw merges at any distance. Two identical bodies with 200 unrelated
rules between them stay apart by default and merge under raw.

  $ { printf '.a{color:red;margin:0}'; for i in $(seq 1 200); do printf '.p%s{order:%s}' "$i" "$i"; done; printf '.b{color:red;margin:0}'; echo; } > far.css
  $ cascade --minify far.css | grep -o '[.a-z,]*{color:red;margin:0}'
  .a{color:red;margin:0}
  .b{color:red;margin:0}
  $ cascade --minify --objective=raw far.css | grep -o '[.a-z,]*{color:red;margin:0}'
  .a,.b{color:red;margin:0}
