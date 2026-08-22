// Make a fetched page self-contained: download every linked stylesheet and
// inline it into a single <style> block, dropping the <link rel=stylesheet>
// tags. The inliner reads CSS from <style>, and a self-contained page renders
// in the headless browser without network access, so the differential test is
// reproducible. Usage: node inline_css.js <page-url> <raw.html> <out.html>
const { execSync } = require('child_process');
const crypto = require('crypto');
const fs = require('fs');
const [, , pageUrl, rawPath, outPath] = process.argv;

let html = fs.readFileSync(rawPath, 'utf8');
const base = new URL(pageUrl);

const hrefs = [];
html.replace(/<link\b[^>]*>/gi, (tag) => {
  if (/rel\s*=\s*["']?stylesheet/i.test(tag)) {
    const m = tag.match(/href\s*=\s*["']([^"']+)["']/i);
    if (m) hrefs.push(m[1]);
  }
  return tag;
});

let css = '';
for (const href of hrefs) {
  const u = new URL(href, base).toString();
  try {
    css += '\n' + execSync(`curl -sL --max-time 60 -A "Mozilla/5.0" "${u}"`, {
      encoding: 'utf8', maxBuffer: 5e8,
    });
  } catch (e) {
    console.error('  css fetch failed: ' + u);
  }
}

html = html.replace(/<link\b[^>]*rel\s*=\s*["']?stylesheet[^>]*>/gi, '');
const style = '<style>' + css + '</style>';
html = html.includes('</head>') ? html.replace('</head>', style + '</head>') : style + html;
fs.writeFileSync(outPath, html);
console.log('  wrote ' + outPath + ' (' + html.length + ' bytes, ' + hrefs.length + ' stylesheet(s))');
// The stylesheet bytes are whatever the CDN served at this moment, and they
// move independently of the page. Digest them on their own so fetch.sh can
// record which CSS a later measurement was taken against.
console.log('  css-sha256 ' + crypto.createHash('sha256').update(css).digest('hex'));
