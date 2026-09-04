// Ask a headless browser what each shorthand declaration expands to.
//
//   node shorthand.js JOBS.json WORKDIR
//
// JOBS.json is written by shorthand_expand.exe: an array of {id, property,
// value}. Each is set on a detached element's inline style, and the CSSOM
// declaration block that results is the expansion, longhand by longhand, in the
// browser's own order. That block is where the resets live: [background: red]
// leaves nine declarations behind, not one.
//
// Output is TSV on stdout, one record per line:
//   n <job> <number of longhands>
//   l <job> <longhand> <specified value> <initial-substituted value>
//   x <job> <error>
// A tab never appears in a CSSOM value, so the fields are unambiguous.
//
// The two value columns differ only where the browser spells a reset [initial]:
// the second column then carries the computed value of that longhand on an
// untouched element, which is the same declaration written the other way. Both
// come from the browser, so neither is a hand-written expansion.
//
// All the jobs of one chunk share a single browser launch. No layout, style
// recalculation or waiting is involved: the inline style block is filled in by
// setProperty itself.

const { execFileSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const CHROME = process.env.CHROME;
const jobsFile = process.argv[2];
const workDir = process.argv[3];
const CHUNK_BYTES = 512 * 1024;

// Runs inside the page.
const HARNESS = `
function shJob(doc, pristine, job, out) {
  var el = doc.createElement('div');
  try { el.style.setProperty(job.property, job.value); }
  catch (e) {
    out.push(['x', job.id, String((e && e.message) || e)].join('\\t'));
    return;
  }
  var st = el.style, n = st.length;
  out.push(['n', job.id, n].join('\\t'));
  for (var i = 0; i < n; i++) {
    var name = st.item(i);
    var v = st.getPropertyValue(name), sub = v;
    if (v === 'initial') {
      var c = doc.defaultView.getComputedStyle(pristine).getPropertyValue(name);
      if (c !== '') sub = c;
    }
    out.push(['l', job.id, name, v, sub].join('\\t'));
  }
}
function shMain(jobs) {
  var out = [];
  var pristine = document.createElement('div');
  document.body.appendChild(pristine);
  jobs.forEach(function (job) {
    try { shJob(document, pristine, job, out); }
    catch (e) { out.push(['x', job.id, String((e && e.message) || e)].join('\\t')); }
  });
  var text = out.join('\\n');
  document.body.setAttribute('data-sh',
    btoa(unescape(encodeURIComponent(text))));
}
`;

function page(jobs) {
  const payload = JSON.stringify(jobs).replace(/</g, '\\u003c');
  return '<!doctype html><html><head><meta charset="utf-8"></head><body>' +
    '<script id="sh-jobs" type="application/json">' + payload + '</script>' +
    '<script>' + HARNESS +
    'shMain(JSON.parse(document.getElementById("sh-jobs").textContent));</script>' +
    '</body></html>';
}

function chunks(jobs) {
  const out = [];
  let cur = [], size = 0;
  for (const job of jobs) {
    const n = JSON.stringify(job).length;
    if (cur.length && size + n > CHUNK_BYTES) { out.push(cur); cur = []; size = 0; }
    cur.push(job);
    size += n;
  }
  if (cur.length) out.push(cur);
  return out;
}

function run(jobs, index) {
  // Absolute: a file:// URL has no notion of the process's directory.
  const file = path.resolve(workDir, 'shorthand-' + index + '.html');
  fs.writeFileSync(file, page(jobs));
  const dom = execFileSync(CHROME, [
    '--headless', '--disable-gpu', '--no-sandbox',
    '--virtual-time-budget=120000', '--dump-dom', 'file://' + file,
  ], { encoding: 'utf8', maxBuffer: 1 << 28 });
  const m = dom.match(/data-sh="([^"]*)"/);
  if (!m) throw new Error('the page produced no result for chunk ' + index);
  return Buffer.from(m[1], 'base64').toString('utf8');
}

const jobs = JSON.parse(fs.readFileSync(jobsFile, 'utf8'));
const parts = chunks(jobs);
parts.forEach(function (chunk, i) {
  const text = run(chunk, i);
  if (text.length) process.stdout.write(text + '\n');
});
