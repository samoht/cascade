// Copy an HTML page with every <style> block minified through cascade, so the
// differential test can check that minification preserves the render.
const { execSync } = require('child_process');
const fs = require('fs');
// No fallback to a `cascade` off $PATH: which build minified the page is the
// one thing the measurement is about. run.sh resolves it.
const CASCADE = process.env.CASCADE;
if (!CASCADE) {
  console.error('ERROR: CASCADE is unset; run this through test/inline/run.sh');
  process.exit(1);
}
const src = fs.readFileSync(process.argv[2], 'utf8');
const out = src.replace(/<style([^>]*)>([\s\S]*?)<\/style>/gi, (_m, attrs, css) => {
  const min = execSync(`"${CASCADE}" fmt --minify -`, { input: css, encoding: 'utf8' });
  return `<style${attrs}>${min.trim()}</style>`;
});
process.stdout.write(out);
