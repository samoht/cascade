#!/bin/sh
# Download a few real pages into test/inline/pages/ so run.sh can run the
# differential test against them. Each page is made self-contained (its external
# stylesheets are fetched and inlined into a <style> block) and then frozen
# (scripts, web fonts and remote images removed, see freeze_page.js), so it
# renders offline and its computed styles are reproducible run to run. The
# downloaded files are large and change upstream, so they are gitignored, never
# committed; re-run this script to reproduce them.
#
# Committing them instead would pin the inputs outright, at the price of about
# a megabyte of third-party CSS in every clone forever, kept fresh by hand. The
# need is to notice a changed input, not to prevent one, so MANIFEST records a
# sha256 per stage instead: the page off the wire, the CSS the CDN served, and
# the frozen page run.sh measures. A count that moved because the site moved is
# then a diff in MANIFEST rather than a mystery.
dir=$(CDPATH= cd "$(dirname "$0")" && pwd)
out="$dir/pages"
manifest="$out/MANIFEST"
mkdir -p "$out"

sha() { # file -> sha256, or "-" if nothing on this machine can hash
  if command -v shasum >/dev/null 2>&1; then shasum -a 256 "$1"
  elif command -v sha256sum >/dev/null 2>&1; then sha256sum "$1"
  elif command -v openssl >/dev/null 2>&1; then openssl dgst -sha256 "$1" | sed 's/.*= //'
  else echo -
  fi | cut -d' ' -f1
}

{
  echo "# fetched $(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "# name url raw-sha256 css-sha256 frozen-sha256"
} > "$manifest"

fetch() { # name url
  name="$1"
  url="$2"
  echo "$name <- $url"
  if curl -sL --max-time 60 -A "Mozilla/5.0" "$url" -o "$out/$name.raw.html"; then
    if info=$(node "$dir/inline_css.js" "$url" "$out/$name.raw.html" "$out/$name.in.html")
    then
      printf '%s\n' "$info"
      if node "$dir/freeze_page.js" "$out/$name.in.html" "$out/$name.html"; then
        # The CSS digest covers the stylesheet bytes alone, so a CDN that
        # changed its CSS under an unchanged page is visible on its own line.
        printf '%s %s %s %s %s\n' "$name" "$url" \
          "$(sha "$out/$name.raw.html")" \
          "$(printf '%s\n' "$info" | sed -n 's/^  css-sha256 //p')" \
          "$(sha "$out/$name.html")" >> "$manifest"
      else
        echo "  freeze failed"
      fi
    else
      echo "  inline-css failed"
    fi
  else
    echo "  download failed"
  fi
  rm -f "$out/$name.raw.html" "$out/$name.in.html"
}

fetch wikipedia https://en.wikipedia.org/wiki/Cascading_Style_Sheets
fetch ocaml https://ocaml.org/
fetch bootstrap https://getbootstrap.com/docs/5.3/examples/album/
fetch tailwind https://tailwindcss.com/
