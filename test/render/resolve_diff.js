// Resolve each job's stylesheet in a headless browser twice and report, for
// every element, every computed-style property the two legs disagree on.
//
//   node resolve_diff.js JOBS.json WORKDIR
//
// JOBS.json is written by resolve_diff.exe: an array of jobs, each carrying the
// document, the stylesheet, and the declarations cascade resolved for every
// element of it.
//
// The first leg puts the stylesheet in a <style> element and lets the browser
// cascade it. The second empties that element and writes cascade's answer into
// each element's style attribute instead. Both legs are the same document, the
// same engine and the same user-agent style, so a property the two spell
// differently is a declaration cascade let win that the browser did not, or
// the other way round.
//
// Output is TSV on stdout, one record per line:
//   d <job> <element index> <property> <browser value> <cascade value>
//   n <job> <elements> <properties> <styled elements> <differences>
//   v <user agent>
//   x <job> <error>
// A tab never appears in a computed-style value, so the fields are unambiguous.

const { execFileSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const CHROME = process.env.CHROME;
const jobsFile = process.argv[2];
const workDir = process.argv[3];
const MAX_DIFFS = 60;
const CHUNK_BYTES = 512 * 1024;

const domJs = fs.readFileSync(path.join(__dirname, 'dom.js'), 'utf8');

// Runs inside the page.
const HARNESS = `
function cdReset(doc) {
  doc.head.textContent = '';
  doc.body.textContent = '';
  var b = doc.body.attributes;
  for (var i = b.length - 1; i >= 0; i--) doc.body.removeAttribute(b[i].name);
}
function cdProps(cs) {
  var props = [];
  // A custom property reaches the render only through a var() in a real
  // property, and those are resolved in the values compared below.
  for (var i = 0; i < cs.length; i++)
    if (cs[i].indexOf('--') !== 0) props.push(cs[i]);
  return props;
}
function cdSnapshot(view, els, props) {
  var snap = [], n = props.length;
  for (var i = 0; i < els.length; i++) {
    var cs = view.getComputedStyle(els[i]);
    var row = new Array(n);
    for (var j = 0; j < n; j++) row[j] = cs.getPropertyValue(props[j]);
    snap.push(row);
  }
  return snap;
}
function cdJob(doc, job, out) {
  cdReset(doc);
  rdBuild(doc, job);
  var view = doc.defaultView;
  var els = Array.prototype.slice.call(doc.body.querySelectorAll('*'));
  // The two sides address elements by position, so a document the builder did
  // not build the way the projection read it makes every later record about
  // the wrong element. Say so instead of reporting one.
  if (els.length !== job.tags.length) {
    out.push(['x', job.id, 'the document has ' + els.length +
              ' elements and the projection ' + job.tags.length].join('\\t'));
    return;
  }
  for (var i = 0; i < els.length; i++)
    if (els[i].tagName.toLowerCase() !== job.tags[i]) {
      out.push(['x', job.id, 'element ' + i + ' is a ' +
                els[i].tagName.toLowerCase() + ' and the projection read a ' +
                job.tags[i]].join('\\t'));
      return;
    }
  var style = doc.createElement('style');
  doc.head.appendChild(style);
  style.textContent = job.css;
  var props = cdProps(view.getComputedStyle(doc.body));
  var cascaded = cdSnapshot(view, els, props);
  // The author sheet goes, cascade's answer arrives. The user-agent style
  // stays on both sides, and so does the document.
  style.textContent = '';
  var styled = 0;
  for (i = 0; i < els.length; i++) {
    if (job.styles[i]) styled++;
    els[i].setAttribute('style', job.styles[i]);
  }
  var projected = cdSnapshot(view, els, props);
  var diffs = 0;
  for (var e = 0; e < els.length; e++)
    for (var j = 0; j < props.length; j++)
      if (cascaded[e][j] !== projected[e][j]) {
        diffs++;
        if (diffs <= MAX_DIFFS)
          out.push(['d', job.id, e, props[j], cascaded[e][j],
                    projected[e][j]].join('\\t'));
      }
  out.push(['n', job.id, els.length, props.length, styled, diffs].join('\\t'));
}
function cdMain(jobs) {
  var out = [];
  var frame = document.createElement('iframe');
  frame.width = 1024;
  frame.height = 768;
  frame.style.border = '0';
  document.body.appendChild(frame);
  var doc = frame.contentDocument;
  // Engines do not agree on computed styles, so a count means something only
  // beside the engine that produced it.
  out.push(['v', navigator.userAgent].join('\t'));
  jobs.forEach(function (job) {
    try { cdJob(doc, job, out); }
    catch (e) { out.push(['x', job.id, String((e && e.message) || e)].join('\\t')); }
  });
  var text = out.join('\\n');
  document.body.setAttribute('data-cd',
    btoa(unescape(encodeURIComponent(text))));
}
`;

function page(jobs) {
  const payload = JSON.stringify(jobs).replace(/</g, '\\u003c');
  return '<!doctype html><html><head><meta charset="utf-8"></head><body>' +
    '<script id="cd-jobs" type="application/json">' + payload + '</script>' +
    '<script>' + domJs + '</script>' +
    '<script>var MAX_DIFFS = ' + MAX_DIFFS + ';' + HARNESS +
    'cdMain(JSON.parse(document.getElementById("cd-jobs").textContent));</script>' +
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
  const m = dom.match(/data-cd="([^"]*)"/);
  if (!m) throw new Error('the page produced no result for chunk ' + index);
  return Buffer.from(m[1], 'base64').toString('utf8');
}

const jobs = JSON.parse(fs.readFileSync(jobsFile, 'utf8'));
const parts = chunks(jobs);
parts.forEach(function (chunk, i) {
  const text = run(chunk, i);
  if (text.length) process.stdout.write(text + '\n');
});
