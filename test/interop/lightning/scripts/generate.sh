#!/usr/bin/env bash
# Regenerate test/interop/lightning/traces/minify.pairs against Lightning CSS.
#
# Strategy: clone Lightning CSS at a pinned commit, patch the body of
# `minify_test_with_options` in src/lib.rs to append every (source,
# expected) pair to the trace, then run `cargo test --lib`. Rust's
# compiler resolves every r#"..."#, indoc!{...}, and format!() into a
# concrete &str at the call site, so the dumper sees the same strings
# the upstream's own assertions see.
#
# Trace format (length-prefixed, robust to any content, no CSV escaping
# headaches): each record is
#
#     >>> <input_len> <expected_len>\n<input bytes><expected bytes>\n
#
# This script does NOT require a personal $LIGHTNINGCSS_REPO. It clones
# the upstream into ./.gen/lightningcss/ and pins a single revision.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TRACE_DIR="$(cd "$SCRIPT_DIR/../traces" && pwd)"

REPO_URL="https://github.com/parcel-bundler/lightningcss"
PIN_REV="df63db2c51c49a6a82f795f3a8988a3cd08ea03a"

command -v cargo >/dev/null 2>&1 || { echo "cargo not on PATH" >&2; exit 1; }
command -v git   >/dev/null 2>&1 || { echo "git not on PATH" >&2; exit 1; }

cd "$SCRIPT_DIR"
mkdir -p .gen

if [ ! -d .gen/lightningcss/.git ]; then
  git clone "$REPO_URL" .gen/lightningcss
fi
git -C .gen/lightningcss fetch --quiet origin "$PIN_REV" || true
git -C .gen/lightningcss checkout --quiet "$PIN_REV"

SRC=.gen/lightningcss/src/lib.rs
BACKUP="$SRC.cascade-orig"
DUMP="$SCRIPT_DIR/.gen/pairs.bin"
rm -f "$DUMP"

cleanup() {
  if [ -f "$BACKUP" ]; then mv "$BACKUP" "$SRC"; fi
}
trap cleanup EXIT

cp "$SRC" "$BACKUP"

# Replace the body of minify_test_with_options with a length-prefixed
# trace emitter. Awk does brace counting so we don't depend on upstream
# line numbers.
awk '
  function emit_body() {
    print "    use std::fs::OpenOptions;"
    print "    use std::io::Write;"
    print "    use std::sync::{Mutex, OnceLock};"
    print "    static M: OnceLock<Mutex<()>> = OnceLock::new();"
    print "    let _g = M.get_or_init(|| Mutex::new(())).lock().unwrap();"
    print "    let path = std::env::var(\"CASCADE_DUMP\")"
    print "        .expect(\"CASCADE_DUMP env var not set\");"
    print "    let mut f = OpenOptions::new().create(true).append(true).open(&path).unwrap();"
    print "    let _ = options;"
    print "    let s = source.as_bytes();"
    print "    let e = expected.as_bytes();"
    print "    write!(f, \">>> {} {}\\n\", s.len(), e.len()).unwrap();"
    print "    f.write_all(s).unwrap();"
    print "    f.write_all(e).unwrap();"
    print "    f.write_all(b\"\\n\").unwrap();"
  }
  BEGIN { state = "scan"; depth = 0 }
  state == "scan" {
    print
    if ($0 ~ /fn minify_test_with_options/) {
      if (index($0, "{") > 0) { depth = 1; emit_body(); state = "skip" }
      else { state = "find_brace" }
    }
    next
  }
  state == "find_brace" {
    print
    if (index($0, "{") > 0) { depth = 1; emit_body(); state = "skip" }
    next
  }
  state == "skip" {
    n = length($0)
    out = ""
    closed = 0
    for (i = 1; i <= n; i++) {
      c = substr($0, i, 1)
      if (c == "{") depth++
      else if (c == "}") {
        depth--
        if (depth == 0) { out = substr($0, i); closed = 1; break }
      }
    }
    if (closed) { print out; state = "tail" }
    next
  }
  state == "tail" { print }
' "$BACKUP" > "$SRC"

(
  cd .gen/lightningcss
  CASCADE_DUMP="$DUMP" cargo test --lib --release -- --test-threads=1
) >&2

mv "$BACKUP" "$SRC"
trap - EXIT

count=$(grep -c '^>>> ' "$DUMP" 2>/dev/null || echo 0)
[ "$count" -gt 0 ] || { echo "no pairs captured" >&2; exit 2; }

mv "$DUMP" "$TRACE_DIR/minify.pairs"
echo "wrote $TRACE_DIR/minify.pairs ($count pairs, lightningcss@$PIN_REV)" >&2
