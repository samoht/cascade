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
# This script does NOT require a personal $LIGHTNINGCSS_REPO. It clones the
# upstream at a single revision and installs the other oracle CLIs from the
# committed npm lockfile in build-local temporary directories.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
SCRIPT_DIR="$script_dir"
TRACE_DIR_ARG="${1:-$SCRIPT_DIR/../traces}"
mkdir -p "$TRACE_DIR_ARG"
TRACE_DIR="$(cd "$TRACE_DIR_ARG" && pwd)"
TRACE_OUT="${CASCADE_TRACE_OUT:-$TRACE_DIR/minify.pairs}"
GEN_DIR="$TRACE_DIR/.gen"
NPM_DIR="$TRACE_DIR/.npm"

REPO_URL="https://github.com/parcel-bundler/lightningcss"
PIN_REV="df63db2c51c49a6a82f795f3a8988a3cd08ea03a"

command -v cargo >/dev/null 2>&1 || { echo "cargo not on PATH" >&2; exit 1; }
command -v git   >/dev/null 2>&1 || { echo "git not on PATH" >&2; exit 1; }
command -v python3 >/dev/null 2>&1 || { echo "python3 not on PATH" >&2; exit 1; }
command -v npm >/dev/null 2>&1 || { echo "npm not on PATH" >&2; exit 1; }

rm -rf "$GEN_DIR" "$NPM_DIR"
mkdir -p "$GEN_DIR/lightningcss" "$NPM_DIR"
trap 'rm -rf "$GEN_DIR" "$NPM_DIR"' EXIT

git -C "$GEN_DIR/lightningcss" init -q
git -C "$GEN_DIR/lightningcss" remote add origin "$REPO_URL"
git -C "$GEN_DIR/lightningcss" fetch --depth=1 origin "$PIN_REV" -q
git -C "$GEN_DIR/lightningcss" checkout --quiet FETCH_HEAD

cp "$SCRIPT_DIR/package.json" "$SCRIPT_DIR/package-lock.json" "$NPM_DIR/"
npm ci --prefix "$NPM_DIR" >&2
ORACLE_BIN="$NPM_DIR/node_modules/.bin"

SRC="$GEN_DIR/lightningcss/src/lib.rs"
BACKUP="$SRC.cascade-orig"
DUMP="$GEN_DIR/pairs.bin"
rm -f "$DUMP"

cleanup() {
  rm -rf "$GEN_DIR" "$NPM_DIR"
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
  cd "$GEN_DIR/lightningcss"
  CASCADE_DUMP="$DUMP" cargo test --lib --release -- --test-threads=1
) >&2

TRACE_IN="$DUMP" TRACE_OUT="$TRACE_OUT" ORACLE_BIN="$ORACLE_BIN" python3 - <<'PY'
import os
import re
import subprocess
import sys

trace_in = os.environ["TRACE_IN"]
trace_out = os.environ["TRACE_OUT"]

oracle_bin = os.environ["ORACLE_BIN"]
oracle_root = os.path.dirname(os.path.dirname(oracle_bin))


def oracle(name, args):
    return [os.path.join(oracle_bin, name), *args]


commands = [
    (
        "esbuild",
        oracle("esbuild", ["--loader=css", "--minify"]),
        "esbuild --loader=css --minify",
    ),
    ("cleancss", oracle("cleancss", ["-O2", "-"]), "cleancss -O2 -"),
    ("csso", oracle("csso", []), "csso"),
    ("cssnano", oracle("cssnano", []), "cssnano"),
    (
        "lightningcss-cli",
        oracle("lightningcss", ["--minify"]),
        "lightningcss --minify",
    ),
]


def stable_stderr(stderr):
    text = stderr.decode("utf-8", "replace").strip()
    text = text.replace(oracle_root, "<npm>")
    return re.sub(r"Node[.]js v[0-9.]+", "Node.js", text)


def run_command(command, input_bytes):
    try:
        proc = subprocess.run(
            command,
            input=input_bytes,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
    except OSError as exc:
        return False, f"os error: {exc}".encode("utf-8", "replace")

    if proc.returncode == 0:
        return True, proc.stdout.strip()
    if proc.returncode < 0:
        reason = f"signal {-proc.returncode}"
    else:
        stderr = stable_stderr(proc.stderr)
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

available = []
for name, command, display_command in commands:
    ok, result = run_command(command, b".x{color:red}")
    if ok:
        available.append((name, command, display_command))
    else:
        print(f"pinned oracle {name} failed: {result.decode('utf-8', 'replace')}", file=sys.stderr)
        sys.exit(2)

failure_count = 0
with open(trace_out, "wb") as f:
    for source, expected in pairs:
        candidates = [(b"lightningcss-trace", expected)]
        failures = []
        for name, command, display_command in available:
            ok, result = run_command(command, source)
            if ok:
                candidates.append((name.encode("utf-8"), result))
            else:
                failures.append(
                    (
                        name.encode("utf-8"),
                        display_command.encode("utf-8"),
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
        ", ".join(
            ["lightningcss-trace"] + [name for name, _, _ in available]
        ),
        failure_count,
    ),
    file=sys.stderr,
)
PY

rm -f "$DUMP"
echo "lightningcss@$PIN_REV" >&2
