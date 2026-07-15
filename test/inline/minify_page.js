// Copy an HTML page with every <style> block minified through cascade, so the
// differential test can check that minification preserves the render.
const { execSync } = require('child_process');
const fs = require('fs');
const CASCADE = process.env.CASCADE || 'cascade';
const src = fs.readFileSync(process.argv[2], 'utf8');
const out = src.replace(/<style([^>]*)>([\s\S]*?)<\/style>/gi, (_m, attrs, css) => {
  const min = execSync(`"${CASCADE}" fmt --minify -`, { input: css, encoding: 'utf8' });
  return `<style${attrs}>${min.trim()}</style>`;
});
process.stdout.write(out);
