#!/usr/bin/env bash
# Regenerate the Lightning-derived input corpus in
# test/interop/lightning/traces/minify.pairs.
#
# Strategy: clone Lightning CSS at a pinned commit, patch the body of
# `minify_test_with_options` in src/lib.rs to append every (source,
# expected) pair to a temporary trace, then run `cargo test --lib`. The source
# is the corpus input; the expected value is Lightning CSS' candidate oracle
# answer. This script then runs the configured minifiers over every input and
# writes their outputs or failures into the checked-in trace. Normal dune tests
# only read cached answers and do not call external minifier CLIs. Rust's
# compiler resolves every r#"..."#, indoc!{...}, and format!() into a concrete
# &str at the call site, so the dumper sees the same strings the upstream's own
# assertions see.
#
# Trace format (length-prefixed, robust to any content, no CSV escaping
# headaches): each record is
#
#     >>> <input_len> <candidate_count> <failure_count>\n
#     <input bytes>\n
#     OK <tool_len> <css_len>\n<tool bytes><css bytes>\n
#     FAIL <tool_len> <command_len> <reason_len>\n<tool bytes><command bytes><reason bytes>\n
#
# This script does NOT require a personal $LIGHTNINGCSS_REPO. It clones
# the upstream into ./.gen/lightningcss/ and pins a single revision.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(git -C "$script_dir" rev-parse --show-toplevel 2>/dev/null || true)"
if [ -n "$repo_root" ] && [[ "$script_dir" == "$repo_root"/_build/default/* ]]; then
  SCRIPT_DIR="$repo_root/${script_dir#"$repo_root/_build/default/"}"
else
  SCRIPT_DIR="$script_dir"
fi
TRACE_DIR_ARG="${1:-$SCRIPT_DIR/../traces}"
mkdir -p "$TRACE_DIR_ARG"
TRACE_DIR="$(cd "$TRACE_DIR_ARG" && pwd)"
TRACE_OUT="${CASCADE_TRACE_OUT:-$TRACE_DIR/minify.pairs}"

REPO_URL="https://github.com/parcel-bundler/lightningcss"
PIN_REV="df63db2c51c49a6a82f795f3a8988a3cd08ea03a"

command -v cargo >/dev/null 2>&1 || { echo "cargo not on PATH" >&2; exit 1; }
command -v git   >/dev/null 2>&1 || { echo "git not on PATH" >&2; exit 1; }
command -v python3 >/dev/null 2>&1 || { echo "python3 not on PATH" >&2; exit 1; }

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

TRACE_IN="$DUMP" TRACE_OUT="$TRACE_OUT" python3 - <<'PY'
import os
import re
import subprocess
import sys

trace_in = os.environ["TRACE_IN"]
trace_out = os.environ["TRACE_OUT"]

default_commands = [
    ("esbuild", "esbuild --loader=css --minify"),
    ("cleancss", "cleancss -O2 -"),
    ("csso", "csso"),
    ("cssnano", "cssnano"),
    ("lightningcss-cli", "lightningcss --minify"),
]

def split_commands(value):
    return [part.strip() for part in value.split(";;") if part.strip()]

custom_commands = [
    (f"custom{i + 1}", command)
    for i, command in enumerate(split_commands(os.environ.get("CASCADE_INTEROP_MINIFIERS", "")))
]

def run_command(command, input_bytes):
    try:
        proc = subprocess.run(
            command,
            input=input_bytes,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            shell=True,
        )
    except OSError as exc:
        return False, f"os error: {exc}".encode("utf-8", "replace")

    if proc.returncode == 0:
        return True, proc.stdout.strip()
    if proc.returncode < 0:
        reason = f"signal {-proc.returncode}"
    else:
        stderr = proc.stderr.decode("utf-8", "replace").strip()
        reason = f"exit {proc.returncode}"
        if stderr:
            reason += f": {stderr}"
    return False, reason.encode("utf-8", "replace")

def read_lightning_pairs(path):
    pairs = []
    with open(path, "rb") as f:
        while True:
            header = f.readline()
            if not header:
                break
            match = re.fullmatch(rb">>> ([0-9]+) ([0-9]+)\n", header)
            if match is None:
                raise RuntimeError(f"bad lightning trace header: {header!r}")
            input_len = int(match.group(1))
            expected_len = int(match.group(2))
            source = f.read(input_len)
            expected = f.read(expected_len)
            sep = f.read(1)
            if len(source) != input_len or len(expected) != expected_len or sep != b"\n":
                raise RuntimeError("truncated lightning trace record")
            pairs.append((source, expected))
    return pairs

def write_blob_record(f, header, blobs):
    f.write(header)
    for blob in blobs:
        f.write(blob)
    f.write(b"\n")

pairs = read_lightning_pairs(trace_in)
if not pairs:
    print("no pairs captured", file=sys.stderr)
    sys.exit(2)

commands = custom_commands + default_commands
available = []
for name, command in commands:
    ok, result = run_command(command, b".x{color:red}")
    if ok:
        available.append((name, command))
    else:
        print(f"skipping unavailable oracle {name}: {result.decode('utf-8', 'replace')}", file=sys.stderr)

failure_count = 0
with open(trace_out, "wb") as f:
    for source, expected in pairs:
        candidates = [(b"lightningcss-trace", expected)]
        failures = []
        for name, command in available:
            ok, result = run_command(command, source)
            if ok:
                candidates.append((name.encode("utf-8"), result))
            else:
                failures.append(
                    (
                        name.encode("utf-8"),
                        command.encode("utf-8"),
                        b"command failed: " + result,
                    )
                )
        failure_count += len(failures)
        f.write(f">>> {len(source)} {len(candidates)} {len(failures)}\n".encode("ascii"))
        f.write(source)
        f.write(b"\n")
        for tool, css in candidates:
            header = f"OK {len(tool)} {len(css)}\n".encode("ascii")
            write_blob_record(f, header, [tool, css])
        for tool, command, reason in failures:
            header = f"FAIL {len(tool)} {len(command)} {len(reason)}\n".encode("ascii")
            write_blob_record(f, header, [tool, command, reason])

print(
    "wrote {} ({} pairs, {} cached oracle tools, {} cached oracle failures)".format(
        trace_out,
        len(pairs),
        ", ".join(["lightningcss-trace"] + [name for name, _ in available]),
        failure_count,
    ),
    file=sys.stderr,
)
PY

rm -f "$DUMP"
echo "lightningcss@$PIN_REV" >&2
