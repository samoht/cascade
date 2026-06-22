#!/bin/sh
# Download a few real pages into test/inline/pages/ so run.sh can run the
# differential test against them. Each page is made self-contained (its external
# stylesheets are fetched and inlined into a <style> block) so the inliner has
# CSS to project and the page renders without network access. The downloaded
# files are large and change upstream, so they are gitignored, never committed.
dir=$(CDPATH= cd "$(dirname "$0")" && pwd)
out="$dir/pages"
mkdir -p "$out"

fetch() { # name url
  name="$1"
  url="$2"
  echo "$name <- $url"
  if curl -sL --max-time 60 -A "Mozilla/5.0" "$url" -o "$out/$name.raw.html"; then
    node "$dir/inline_css.js" "$url" "$out/$name.raw.html" "$out/$name.html" ||
      echo "  inline-css failed"
  else
    echo "  download failed"
  fi
  rm -f "$out/$name.raw.html"
}

fetch wikipedia https://en.wikipedia.org/wiki/Cascading_Style_Sheets
fetch ocaml https://ocaml.org/
fetch bootstrap https://getbootstrap.com/docs/5.3/examples/album/
fetch tailwind https://tailwindcss.com/
