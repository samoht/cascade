// Copy an HTML page with every <style> block rewritten through cascade, so the
// differential test can check that the transform preserves the render. Any
// argument after the page is passed on to cascade, so one driver covers
// `fmt --minify` and the closed-world `--inline-vars` cleanup on top of it.
const { execFileSync, execSync } = require('child_process');
const fs = require('fs');
const CASCADE = process.env.CASCADE;
const MINIFIER_CMD = process.env.MINIFIER_CMD;
// No fallback to a minifier off $PATH: which build rewrote the page is the one
// thing the measurement is about. run.sh resolves Cascade; the differential
// suite supplies an explicit external command.
if (!CASCADE && !MINIFIER_CMD) {
  console.error('ERROR: CASCADE and MINIFIER_CMD are unset');
  process.exit(1);
}
const src = fs.readFileSync(process.argv[2], 'utf8');
const flags = process.argv.slice(3);
let blocks = 0;
const out = src.replace(/<style([^>]*)>([\s\S]*?)<\/style>/gi, (_m, attrs, css) => {
  blocks++;
  // maxBuffer: a real page's <style> runs to megabytes, and the default 1MB
  // makes execSync throw rather than return the minified sheet.
  const options = { input: css, encoding: 'utf8', maxBuffer: 1e9 };
  const min = MINIFIER_CMD
    ? execSync(MINIFIER_CMD, options)
    : execFileSync(CASCADE, ['fmt', '--minify', ...flags, '-'], options);
  return `<style${attrs}>${min.trim()}</style>`;
});
if (process.env.REQUIRE_STYLE && blocks === 0) {
  console.error('ERROR: no <style> block in ' + process.argv[2]);
  process.exit(1);
}
process.stdout.write(out);
