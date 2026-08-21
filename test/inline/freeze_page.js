// Freeze a fetched page so its layout is a pure function of its CSS.
//
// A page straight off the wire has three resolvers that finish at a moment
// nobody controls, and each of them moves computed styles: <script> mutates the
// DOM, an @font-face whose relative .woff2 src cannot be read under file://
// swaps text metrics from the fallback to the webfont mid-render, and an <img>
// with an unreachable src settles on its broken-image box whenever the load
// finally fails. Measured unfrozen, one unchanged pair of pages returned 14,
// 410, 14 and 14 differences on four consecutive runs.
//
// Everything asynchronous is therefore removed, or replaced by something that
// resolves before layout. <noscript> goes too: the browser leaves its content
// as inert text while scripting is on, so an HTML parser that reads it as
// markup styles elements the browser never lays out.
//
// Usage: node freeze_page.js <in.html> <out.html>
const fs = require('fs');

// A 1x1 transparent GIF: an intrinsic size that is known before layout.
const PIXEL =
  'data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7';

const paired = (tag) =>
  new RegExp('<' + tag + '\\b[^>]*>[\\s\\S]*?<\\/' + tag + '\\s*>', 'gi');

function freeze(html) {
  // Self-closing scripts first: to the paired pattern they read as an opener,
  // and it would swallow the document down to the next </script>.
  html = html.replace(/<script\b[^>]*\/>/gi, '');
  for (const tag of ['script', 'noscript', 'iframe', 'video', 'audio'])
    html = html.replace(paired(tag), '');
  html = html.replace(/<source\b[^>]*>/gi, '');
  html = html.replace(/@font-face\s*\{[^}]*\}/gi, '');
  html = html.replace(/<img\b[^>]*>/gi, (tag) => {
    // Leading \s keeps data-src and friends, which are inert without scripts.
    const bare = tag.replace(
      /\s(?:src|srcset|sizes)\s*=\s*(?:"[^"]*"|'[^']*'|[^\s>]*)/gi,
      ''
    );
    return bare.replace(/^<img/i, '<img src="' + PIXEL + '"');
  });
  return html;
}

const [, , inPath, outPath] = process.argv;
fs.writeFileSync(outPath, freeze(fs.readFileSync(inPath, 'utf8')));
