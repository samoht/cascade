// Ask the browser which elements a selector matches, so the library's matcher
// can be compared against it.
//
//   node selector_match.js JOBS.json WORKDIR
//
// JOBS.json is written by selector_match.exe: one job per document, each
// carrying the document to build, the fingerprint the OCaml side read off its
// own copy of that document, and the selectors to run against it.
//
// Each job gets its own standards-mode document: an iframe rewritten with a
// doctype, because attribute-value and class case-sensitivity change between
// the two modes and a quirks-mode document answers a different question. The
// mode is asserted rather than assumed.
//
// Output is TSV on stdout, one record per line:
//   V <user agent>
//   A <job> <browser elements> <cascade elements> <fingerprint mismatches> <mode>
//   P <job> <element index> <hex of the browser's fingerprint>
//   M <job> <selector> <matched element indices, comma separated>
//   E <job> <selector>            the browser refuses the selector
//   X <job> <error>
// Every field is an index, a hex string or a name, so a tab never appears in
// one.

const { execFileSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const CHROME = process.env.CHROME;
const jobsFile = process.argv[2];
const workDir = process.argv[3];
const MAX_FINGERPRINT_REPORTS = 20;

// Runs inside the page.
const HARNESS = `
var SM_TAB = String.fromCharCode(9);
var SM_NL = String.fromCharCode(10);
var SM_US = String.fromCharCode(31);
function smRow(fields) { return fields.join(SM_TAB); }
function smHex(s) {
  var b = unescape(encodeURIComponent(s)), out = '';
  for (var i = 0; i < b.length; i++) {
    var h = b.charCodeAt(i).toString(16);
    out += h.length < 2 ? '0' + h : h;
  }
  return out;
}
// createElement rather than a serialised document: the HTML parser rewrites
// nesting the generator did not ask for, and the comparison needs the tree the
// OCaml side read, not the one a parser is willing to spell.
function smBuild(doc, spec) {
  function node(n) {
    var e = doc.createElement(n.t);
    (n.a || []).forEach(function (a) { e.setAttribute(a[0], a[1]); });
    (n.k || []).forEach(function (k) { e.appendChild(node(k)); });
    (n.x || []).forEach(function (t) { e.appendChild(doc.createTextNode(t)); });
    (n.c || []).forEach(function (t) { e.appendChild(doc.createComment(t)); });
    return e;
  }
  (spec.body || []).forEach(function (k) { doc.body.appendChild(node(k)); });
}
// What the library's NODE accessors report, read off the browser instead. A
// document adapter that answers a different tree makes every later record about
// something the browser never saw.
function smFingerprint(el) {
  var names = [], i;
  for (i = 0; i < el.attributes.length; i++) names.push(el.attributes[i].name);
  names.sort();
  var attrs = names.map(function (n) { return n + '=' + el.getAttribute(n); });
  var classes = [];
  for (i = 0; i < el.classList.length; i++) classes.push(el.classList[i]);
  var text = [];
  for (i = 0; i < el.childNodes.length; i++)
    if (el.childNodes[i].nodeType === 3) text.push(el.childNodes[i].data);
  return [el.tagName.toLowerCase(), attrs.join(SM_US), classes.join(SM_US),
          text.join(SM_US)].join('|');
}
function smJob(frame, job, out) {
  var doc = frame.contentDocument;
  doc.open();
  doc.write('<!doctype html><html><head></head><body></body></html>');
  doc.close();
  if (doc.compatMode !== 'CSS1Compat') {
    out.push(smRow(['X', job.id, 'compatMode is ' + doc.compatMode]));
    return;
  }
  smBuild(doc, job);
  var els = [doc.documentElement], i;
  var found = doc.documentElement.querySelectorAll('*');
  for (i = 0; i < found.length; i++) els.push(found[i]);
  var bad = 0;
  for (i = 0; i < els.length; i++) {
    var fp = smHex(smFingerprint(els[i]));
    if (i >= job.fp.length || fp !== job.fp[i]) {
      bad++;
      if (bad <= ${MAX_FINGERPRINT_REPORTS}) out.push(smRow(['P', job.id, i, fp]));
    }
  }
  out.push(smRow(['A', job.id, els.length, job.fp.length, bad, doc.compatMode]));
  var index = new Map();
  for (i = 0; i < els.length; i++) index.set(els[i], i);
  job.sels.forEach(function (s) {
    var hits;
    try { hits = doc.querySelectorAll(s.t); }
    catch (e) { out.push(smRow(['E', job.id, s.id])); return; }
    var ids = [];
    for (var j = 0; j < hits.length; j++) {
      var k = index.get(hits[j]);
      if (k === undefined) {
        out.push(smRow(['X', job.id,
                        s.id + ' matched an element the enumeration misses']));
        return;
      }
      ids.push(k);
    }
    out.push(smRow(['M', job.id, s.id, ids.join(',')]));
  });
}
function smMain(jobs) {
  var out = [];
  out.push(smRow(['V', navigator.userAgent]));
  if (document.compatMode !== 'CSS1Compat') {
    out.push(smRow(['X', 'page', 'host compatMode is ' + document.compatMode]));
  } else {
    var frame = document.createElement('iframe');
    frame.width = 800;
    frame.height = 600;
    document.body.appendChild(frame);
    jobs.forEach(function (job) {
      try { smJob(frame, job, out); }
      catch (e) {
        out.push(smRow(['X', job.id, String((e && e.message) || e)]));
      }
    });
  }
  document.body.setAttribute('data-sm',
    btoa(unescape(encodeURIComponent(out.join(SM_NL)))));
}
`;

function page(jobs) {
  const payload = JSON.stringify(jobs).replace(/</g, '\\u003c');
  return '<!doctype html><html><head><meta charset="utf-8"></head><body>' +
    '<script id="sm-jobs" type="application/json">' + payload + '</script>' +
    '<script>' + HARNESS +
    'smMain(JSON.parse(document.getElementById("sm-jobs").textContent));' +
    '</script></body></html>';
}

function run(jobs, index) {
  // Absolute: a file:// URL has no notion of the process's directory.
  const file = path.resolve(workDir, 'page-' + index + '.html');
  fs.writeFileSync(file, page(jobs));
  const dom = execFileSync(CHROME, [
    '--headless', '--disable-gpu', '--no-sandbox',
    '--virtual-time-budget=120000', '--dump-dom', 'file://' + file,
  ], { encoding: 'utf8', maxBuffer: 1 << 28 });
  const m = dom.match(/data-sm="([^"]*)"/);
  if (!m) throw new Error('the page produced no result for chunk ' + index);
  return Buffer.from(m[1], 'base64').toString('utf8');
}

// One page per document: the whole run in one page would put a megabyte of
// results in one attribute, and a failure there loses every document at once.
const jobs = JSON.parse(fs.readFileSync(jobsFile, 'utf8'));
jobs.forEach(function (job, i) {
  const text = run([job], i);
  if (text.length) process.stdout.write(text + '\n');
});
