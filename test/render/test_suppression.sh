#!/bin/sh
# A gate silenced by CASCADE_NO_BROWSER must not read as a pass, and the one
# value that exits 0 has to say the run checked nothing. Both legs return
# before the harness looks for a browser, so this control runs on every
# machine, and needs none.
#
# Usage: sh test_suppression.sh <harness.exe>...
set -eu
for exe in "$@"; do
  case $exe in */*) ;; *) exe=./$exe ;; esac
  name=$(basename "$exe")
  if CASCADE_NO_BROWSER=1 "$exe" >/dev/null 2>&1; then
    echo "FAIL: $name exits 0 under CASCADE_NO_BROWSER=1" >&2
    exit 1
  fi
  if ! out=$(CASCADE_NO_BROWSER=unchecked "$exe" 2>&1); then
    echo "FAIL: $name does not exit 0 for the acknowledged value" >&2
    printf '%s\n' "$out" >&2
    exit 1
  fi
  case $out in
    SKIP:*) ;;
    *)
      echo "FAIL: $name skipped without saying so: $out" >&2
      exit 1
      ;;
  esac
done
echo "PASS: a suppressed browser gate fails unless the run is declared unchecked"
