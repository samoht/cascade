CLI: --memtrace with an unwritable path warns and keeps going.

Memtrace.start_tracing raises Invalid_argument when it cannot open the
trace file (here the parent directory does not exist). The CLI catches
that, warns, and still formats the input rather than aborting with a
backtrace.

  $ cat > in.css <<EOF
  > .a { color: red }
  > EOF
  $ cascade --minify --memtrace nope/trace.ctf in.css 2>&1
  warning: memtrace unavailable on this runtime (Cannot open memtrace file nope/trace.ctf: No such file or directory); skipping
  .a{color:red}
