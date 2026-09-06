// Render each job's stylesheets in a headless browser and report, for every
// element, every computed-style property the variants disagree on.
//
//   node driver.js JOBS.json WORKDIR
//
// JOBS.json is written by render_diff.exe: an array of jobs, each carrying the
// derived document, the pseudo-elements to sample, the probe selectors, and the
// stylesheet variants (the first is the reference the others are compared to).
//
// Output is TSV on stdout, one record per line:
//   d <job> <variant> <index> <element> <property> <reference> <variant value>
//   c <job> <matched probes> <unmatched probes> <rejected probes>
//   u <job> <probe that matched no element>
//   x <job> <error>
// A tab never appears in a computed-style value, so the fields are unambiguous.
//
// All the jobs of one chunk share a single browser launch: the page reuses one
// iframe, clearing it between jobs. getComputedStyle forces a synchronous style
// recalculation, so no waiting or polling is involved.

const { execFileSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const CHROME = process.env.CHROME;
const jobsFile = process.argv[2];
const workDir = process.argv[3];
const MAX_DIFFS = 200;
const CHUNK_BYTES = 512 * 1024;

const domJs = fs.readFileSync(path.join(__dirname, 'dom.js'), 'utf8');
const frameJs = fs.readFileSync(path.join(__dirname, 'frame.js'), 'utf8');

// Runs inside the page.
const HARNESS = `
function rdReset(doc) {
  var root = doc.documentElement, a = root.attributes;
  for (var i = a.length - 1; i >= 0; i--) root.removeAttribute(a[i].name);
  doc.head.textContent = '';
  doc.body.textContent = '';
  var b = doc.body.attributes;
  for (var i = b.length - 1; i >= 0; i--) doc.body.removeAttribute(b[i].name);
}
function rdProps(cs) {
  var props = [];
  // A custom property reaches the render only through a var() in a real
  // property, and those are resolved in the values compared below.
  for (var i = 0; i < cs.length; i++)
    if (cs[i].indexOf('--') !== 0) props.push(cs[i]);
  return props;
}
function rdSnapshot(view, els, pseudos, props) {
  var snap = [], n = props.length;
  for (var i = 0; i < els.length; i++) {
    for (var p = -1; p < pseudos.length; p++) {
      var cs = p < 0 ? view.getComputedStyle(els[i])
                     : view.getComputedStyle(els[i], pseudos[p]);
      var row = new Array(n);
      for (var j = 0; j < n; j++) row[j] = cs.getPropertyValue(props[j]);
      snap.push(row);
    }
  }
  return snap;
}
function rdJob(doc, job, out) {
  rdReset(doc);
  rdBuild(doc, job);
  var view = doc.defaultView;
  var pseudos = job.pseudos || [];
  var els = [doc.documentElement, doc.body];
  var found = doc.body.querySelectorAll('*');
  for (var i = 0; i < found.length; i++) els.push(found[i]);
  var labels = [];
  for (i = 0; i < els.length; i++)
    for (var p = -1; p < pseudos.length; p++)
      labels.push((els[i] === doc.documentElement ? 'html'
                   : els[i] === doc.body ? 'body' : rdPath(els[i]))
                  + (p < 0 ? '' : pseudos[p]));
  var matched = 0, unmatched = 0, rejected = 0, shown = 0;
  (job.probes || []).forEach(function (sel) {
    var n;
    try { n = doc.querySelectorAll(sel).length; }
    catch (e) { rejected++; return; }
    if (n > 0) matched++;
    else {
      unmatched++;
      if (shown++ < 10) out.push(['u', job.id, sel].join('\\t'));
    }
  });
  out.push(['c', job.id, matched, unmatched, rejected].join('\\t'));
  var style = doc.createElement('style');
  doc.head.appendChild(style);
  var props = null, base = null;
  job.sheets.forEach(function (sheet) {
    style.textContent = sheet.css;
    if (props === null) props = rdProps(view.getComputedStyle(doc.body));
    var snap = rdSnapshot(view, els, pseudos, props);
    if (base === null) { base = snap; return; }
    var diffs = 0;
    for (var e = 0; e < base.length && diffs < MAX_DIFFS; e++)
      for (var j = 0; j < props.length && diffs < MAX_DIFFS; j++)
        if (base[e][j] !== snap[e][j]) {
          diffs++;
          out.push(['d', job.id, sheet.name, e, labels[e], props[j],
                    base[e][j], snap[e][j]].join('\\t'));
        }
  });
}
function rdMain(jobs) {
  var out = [];
  // A frame nobody can sample is every job's failure, and the reader keys an
  // error by the job it belongs to.
  var doc = null, broken = null;
  try { doc = rdStandardsFrame(window, 1024, 768); }
  catch (e) { broken = String((e && e.message) || e); }
  jobs.forEach(function (job) {
    if (broken !== null) {
      out.push(['x', job.id, broken].join('\\t'));
      return;
    }
    try { rdJob(doc, job, out); }
    catch (e) { out.push(['x', job.id, String((e && e.message) || e)].join('\\t')); }
  });
  var text = out.join('\\n');
  document.body.setAttribute('data-rd',
    btoa(unescape(encodeURIComponent(text))));
}
`;

function page(jobs) {
  const payload = JSON.stringify(jobs).replace(/</g, '\\u003c');
  return '<!doctype html><html><head><meta charset="utf-8"></head><body>' +
    '<script id="rd-jobs" type="application/json">' + payload + '</script>' +
    '<script>' + domJs + '</script>' +
    '<script>' + frameJs + '</script>' +
    '<script>var MAX_DIFFS = ' + MAX_DIFFS + ';' + HARNESS +
    'rdMain(JSON.parse(document.getElementById("rd-jobs").textContent));</script>' +
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
  const file = path.resolve(workDir, 'page-' + index + '.html');
  fs.writeFileSync(file, page(jobs));
  const dom = execFileSync(CHROME, [
    '--headless', '--disable-gpu', '--no-sandbox',
    '--virtual-time-budget=120000', '--dump-dom', 'file://' + file,
  ], { encoding: 'utf8', maxBuffer: 1 << 28 });
  const m = dom.match(/data-rd="([^"]*)"/);
  if (!m) throw new Error('the page produced no result for chunk ' + index);
  return Buffer.from(m[1], 'base64').toString('utf8');
}

const jobs = JSON.parse(fs.readFileSync(jobsFile, 'utf8'));
const parts = chunks(jobs);
parts.forEach(function (chunk, i) {
  const text = run(chunk, i);
  if (text.length) process.stdout.write(text + '\n');
});
