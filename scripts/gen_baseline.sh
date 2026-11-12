#!/bin/sh
# Regenerate lib/baseline.tsv from the web-features Baseline dataset; dune
# builds lib/baseline.ml from it with scripts/gen_baseline_ml.ml.
#
# An unprefixed declaration supersedes its vendor-prefixed twin only once every
# maintained browser reads it. A feature that is Baseline "widely available"
# (in every maintained engine for ~30 months) is safe to assume; anything less,
# "newly available" or "limited", is not. This emits the CSS property names that
# are NOT yet "widely available", so lib/shorthand.ml keeps exactly those
# prefixes.
#
# It also emits the per-engine first versions web-features records for every
# BCD css.* key some target engine still lacks. That is the machine-checkable
# form of "which grammar is implemented where": lib/support.ml answers a
# targets question from it, so a browser gap is looked up rather than asserted
# by hand, and a browser that catches up moves the answer at the next run.
#
# The source (https://github.com/web-platform-dx/web-features) is the machine-
# checkable Baseline dataset. Rerun this when browsers ship new features:
#
#   scripts/gen_baseline.sh
#
# Requires curl and jq.
set -eu

url=https://unpkg.com/web-features
data=$(curl -sfL "$url/data.json")
version=$(curl -sfL "$url/package.json" | jq -r .version)

# Names, under BCD prefix $1, of a single [a-z-] segment (i.e. the property
# itself, not a sub-value like [anchor-name.none]). A name counts as greenfield
# only when it is covered by a not-"high" feature AND by no "high" feature,
# otherwise an established property (e.g. accent-color) is picked up just
# because a newer feature also references it.
greenfield() {
  printf '%s' "$data" | jq -r --arg p "$1" '
    def names(cond):
      [ .features[]
        | select(cond)
        | (.compat_features // [])[]
        | select(startswith($p + ".") and (ltrimstr($p + ".") | test("^[a-z-]+$")))
        | ltrimstr($p + ".") ]
      | unique;
    (names(.status.baseline != "high") - names(.status.baseline == "high"))
    | .[]'
}

# Every css.* compat key web-features records support for, as
#   key<TAB>chrome<TAB>firefox<TAB>safari<TAB>safari_ios
# with "-" for an engine that does not implement it. Keys where every engine has
# it are emitted too: "implemented in these targets" is as much an answer as
# "missing from them", and a table holding only the gaps could never give it. A
# version reads as its leading major.minor; BCD's "<=37" and preview markers
# lose the marker, which only ever makes the recorded support older than the
# real one.
support_rows() {
  printf '%s' "$data" | jq -r '
    def ver(s): if s == null then "-"
                else (s | tostring | sub("^[^0-9]*"; "") | sub("[^0-9.].*$"; ""))
                     | if . == "" then "-" else . end
                end;
    [ .features[]
      | (.status.by_compat_key // {})
      | to_entries[]
      | select(.key | startswith("css."))
      | { key: .key, s: (.value.support // {}) } ]
    | unique_by(.key)
    | .[]
    | [ .key, ver(.s.chrome), ver(.s.firefox), ver(.s.safari), ver(.s.safari_ios) ]
    | @tsv'
}

# One record per key. A version is normalised to major.minor, the precision
# lib/baseline.ml keeps.
support_to_tsv() {
  awk -F'\t' '
    function v(s) {
      if (s == "-") return "-"
      n = split(s, p, ".")
      return (p[1] + 0) "." ((n > 1) ? p[2] + 0 : 0)
    }
    {
      printf "support\t%s\t%s\t%s\t%s\t%s\n", $1, v($2), v($3), v($4), v($5)
    }'
}

{
  printf 'web-features\t%s\n' "$version"
  greenfield css.properties | awk 'NF { printf "greenfield\t%s\n", $0 }'
  support_rows | support_to_tsv
} >lib/baseline.tsv

echo "wrote lib/baseline.tsv from web-features $version"
