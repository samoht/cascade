// Ask a headless browser whether each property grammar vector is valid CSS.
//
//   node vectors.js JOBS.json WORKDIR
//
// JOBS.json is written by property_vectors.exe: an array of {id, property,
// value}. Each job is put to two independent oracles inside the page:
//
//   s   el.style.setProperty(property, value) leaves a non-empty block
//   c   CSS.supports(property, value)
//
// Both are asked of every job. The manifest is judged by their agreement, so a
// vector they disagree on is a fact about the browser rather than about the
// manifest, and the run reports it instead of picking a side.
//
// Output is TSV on stdout, one record per line:
//   v <job> <s> <c>      each 0 or 1
//   x <job> <error>
// A tab never appears in a job id or in either verdict, so the fields are
// unambiguous.
//
// All the jobs of one chunk share a single browser launch. No layout or style
// recalculation is involved: setProperty fills the inline block itself.

const { execFileSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const CHROME = process.env.CHROME;
const jobsFile = process.argv[2];
const workDir = process.argv[3];
const CHUNK_BYTES = 512 * 1024;

// Runs inside the page.
const HARNESS = `
function vJob(doc, job, out) {
  var s = 0, c = 0;
  var el = doc.createElement('div');
  try {
    el.style.setProperty(job.property, job.value);
    s = el.style.length > 0 ? 1 : 0;
  } catch (e) {
    out.push(['x', job.id, String((e && e.message) || e)].join('\\t'));
    return;
  }
  try { c = CSS.supports(job.property, job.value) ? 1 : 0; }
  catch (e) {
    out.push(['x', job.id, String((e && e.message) || e)].join('\\t'));
    return;
  }
  out.push(['v', job.id, s, c].join('\\t'));
}
function vMain(jobs) {
  var out = [];
  jobs.forEach(function (job) {
    try { vJob(document, job, out); }
    catch (e) { out.push(['x', job.id, String((e && e.message) || e)].join('\\t')); }
  });
  var text = out.join('\\n');
  document.body.setAttribute('data-v',
    btoa(unescape(encodeURIComponent(text))));
}
`;

function page(jobs) {
  const payload = JSON.stringify(jobs).replace(/</g, '\\u003c');
  return '<!doctype html><html><head><meta charset="utf-8"></head><body>' +
    '<script id="v-jobs" type="application/json">' + payload + '</script>' +
    '<script>' + HARNESS +
    'vMain(JSON.parse(document.getElementById("v-jobs").textContent));</script>' +
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
  const file = path.resolve(workDir, 'vectors-' + index + '.html');
  fs.writeFileSync(file, page(jobs));
  const dom = execFileSync(CHROME, [
    '--headless', '--disable-gpu', '--no-sandbox',
    '--virtual-time-budget=120000', '--dump-dom', 'file://' + file,
  ], { encoding: 'utf8', maxBuffer: 1 << 28 });
  const m = dom.match(/data-v="([^"]*)"/);
  if (!m) throw new Error('the page produced no result for chunk ' + index);
  return Buffer.from(m[1], 'base64').toString('utf8');
}

const jobs = JSON.parse(fs.readFileSync(jobsFile, 'utf8'));
const parts = chunks(jobs);
parts.forEach(function (chunk, i) {
  const text = run(chunk, i);
  if (text.length) process.stdout.write(text + '\n');
});
