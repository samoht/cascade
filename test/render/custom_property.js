// Compute every job's stylesheet variants in a headless browser and report,
// for every element, every computed-style property the variants disagree on.
//
//   node custom_property.js JOBS.json WORKDIR
//
// JOBS.json is written by custom_property.exe: an array of jobs, each carrying
// the document, the custom-property names to sample by name, and the
// stylesheet variants (the first is the reference the others are compared to).
//
// The sampled property list is what getComputedStyle enumerates, plus the
// job's custom-property names. The enumeration answers for registered custom
// properties only, so an unregistered `--x` reaches the comparison only by
// being named, and every other harness here drops `--` entirely.
//
// Output is TSV on stdout, one record per line:
//   d <job> <variant> <element> <property> <reference> <variant value>
//   n <job> <elements> <properties> <variants> <differences>
//   a <job> <adapter fact> <value>
//   x <job> <error>
// A tab never appears in a computed-style value, so the fields are unambiguous.

const { execFileSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const CHROME = process.env.CHROME;
const jobsFile = process.argv[2];
const workDir = process.argv[3];
const MAX_DIFFS = 120;
const CHUNK_BYTES = 512 * 1024;

const frameJs = fs.readFileSync(path.join(__dirname, 'frame.js'), 'utf8');

// Runs inside the page.
const HARNESS = `
function cpReset(doc) {
  doc.head.textContent = '';
  doc.body.textContent = '';
  var a = doc.documentElement.attributes;
  for (var i = a.length - 1; i >= 0; i--)
    doc.documentElement.removeAttribute(a[i].name);
  var b = doc.body.attributes;
  for (var i = b.length - 1; i >= 0; i--) doc.body.removeAttribute(b[i].name);
}
function cpLabel(el) {
  if (el.id) return '#' + el.id;
  return el.tagName.toLowerCase();
}
function cpElements(doc) {
  var els = [doc.documentElement, doc.body];
  var found = doc.body.querySelectorAll('*');
  for (var i = 0; i < found.length; i++) els.push(found[i]);
  return els;
}
// The enumeration plus the names the job declares or references. A custom
// property nothing registered is absent from the enumeration, so naming it is
// the only way its value is ever compared.
function cpProps(cs, names) {
  var props = [], seen = {};
  for (var i = 0; i < cs.length; i++) {
    props.push(cs[i]);
    seen[cs[i]] = true;
  }
  for (var j = 0; j < names.length; j++)
    if (!seen[names[j]]) props.push(names[j]);
  return props;
}
function cpSnapshot(view, els, props) {
  var snap = [], n = props.length;
  for (var i = 0; i < els.length; i++) {
    var cs = view.getComputedStyle(els[i]);
    var row = new Array(n);
    for (var j = 0; j < n; j++) row[j] = cs.getPropertyValue(props[j]);
    snap.push(row);
  }
  return snap;
}
// Whether the document the job asked for is the document the frame built, and
// whether a custom property declared on the root reaches a descendant. A NODE
// that does not report ids, or a frame that lost inheritance, answers every
// comparison out of a document nobody wrote.
function cpAdapter(doc, job, out) {
  if (!job.adapter) return;
  for (var i = 0; i < job.adapter.length; i++) {
    var sel = job.adapter[i], n;
    try { n = doc.querySelectorAll(sel).length; }
    catch (e) { n = -1; }
    out.push(['a', job.id, sel, n].join('\\t'));
  }
}
function cpJob(doc, job, out) {
  cpReset(doc);
  doc.body.innerHTML = job.doc;
  var view = doc.defaultView;
  var els = cpElements(doc);
  var labels = [];
  for (var i = 0; i < els.length; i++) labels.push(cpLabel(els[i]));
  cpAdapter(doc, job, out);
  var style = doc.createElement('style');
  doc.head.appendChild(style);
  var props = null, base = null, diffs = 0, variants = 0;
  job.sheets.forEach(function (sheet) {
    style.textContent = sheet.css;
    if (props === null) props = cpProps(view.getComputedStyle(doc.body), job.names);
    var snap = cpSnapshot(view, els, props);
    if (base === null) { base = snap; return; }
    variants++;
    for (var e = 0; e < base.length && diffs < MAX_DIFFS; e++)
      for (var j = 0; j < props.length && diffs < MAX_DIFFS; j++)
        if (base[e][j] !== snap[e][j]) {
          diffs++;
          out.push(['d', job.id, sheet.name, labels[e], props[j],
                    base[e][j], snap[e][j]].join('\\t'));
        }
  });
  out.push(['n', job.id, els.length, props === null ? 0 : props.length,
            variants, diffs].join('\\t'));
}
function cpMain(jobs) {
  var out = [];
  // A frame nobody can sample is every job's failure, and the reader keys an
  // error by the job it belongs to.
  var doc = null, broken = null;
  try { doc = rdStandardsFrame(window, 800, 600); }
  catch (e) { broken = String((e && e.message) || e); }
  jobs.forEach(function (job) {
    if (broken !== null) {
      out.push(['x', job.id, broken].join('\\t'));
      return;
    }
    try { cpJob(doc, job, out); }
    catch (e) { out.push(['x', job.id, String((e && e.message) || e)].join('\\t')); }
  });
  document.body.setAttribute('data-cp',
    btoa(unescape(encodeURIComponent(out.join('\\n')))));
}
`;

function page(jobs) {
  const payload = JSON.stringify(jobs).replace(/</g, '\\u003c');
  return '<!doctype html><html><head><meta charset="utf-8"></head><body>' +
    '<script id="cp-jobs" type="application/json">' + payload + '</script>' +
    '<script>' + frameJs + '</script>' +
    '<script>var MAX_DIFFS = ' + MAX_DIFFS + ';' + HARNESS +
    'cpMain(JSON.parse(document.getElementById("cp-jobs").textContent));</script>' +
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
  const m = dom.match(/data-cp="([^"]*)"/);
  if (!m) throw new Error('the page produced no result for chunk ' + index);
  return Buffer.from(m[1], 'base64').toString('utf8');
}

const jobs = JSON.parse(fs.readFileSync(jobsFile, 'utf8'));
const parts = chunks(jobs);
parts.forEach(function (chunk, i) {
  const text = run(chunk, i);
  if (text.length) process.stdout.write(text + '\n');
});
