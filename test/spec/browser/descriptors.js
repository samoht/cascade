// Ask a headless browser whether each @font-face descriptor value is valid CSS.
//
//   node descriptors.js JOBS.json WORKDIR
//
// JOBS.json is written by descriptor_set.exe: an array of {id, sheet,
// descriptor}. A descriptor is not a property, so neither oracle the property
// harnesses use can answer here: CSS.supports takes a property name and
// el.style.setProperty fills an inline block, and both refuse a descriptor.
// What CSSOM exposes instead is the parsed rule, so each job is put to two
// independent entry points into the same grammar:
//
//   p   insertRule(sheet) into a <style> element's sheet, and the parsed
//       CSSFontFaceRule still spells the descriptor in its cssText
//   o   replaceSync(sheet) on a constructed CSSStyleSheet, read back the same
//       way
//
// Both are the CSS parser reached by a different entry point, and a job they
// disagree about is a fact about the browser rather than about cascade, so the
// run reports it instead of picking a side. Two things are deliberately not
// the second oracle. rule.style.setProperty is a PROPERTY setter, so it takes
// every CSS-wide keyword whatever the descriptor grammar says, and would call
// [ascent-override: inherit] valid on every descriptor at once. And
// getPropertyValue answers "" for a descriptor that is a shorthand, which
// would call font-variant unimplemented on a browser that reads it, so the
// read-back is of the rule's own text.
//
// Output is TSV on stdout, one record per line:
//   v <job> <p> <o>      each 0 or 1
//   x <job> <error>
// A tab never appears in a job id or in either verdict, so the fields are
// unambiguous.
//
// All the jobs of one chunk share a single browser launch. No layout or style
// recalculation is involved: the descriptors are read straight off the rule.

const { execFileSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const CHROME = process.env.CHROME;
const jobsFile = process.argv[2];
const workDir = process.argv[3];
const CHUNK_BYTES = 512 * 1024;

// Runs inside the page.
const HARNESS = `
function dSheet(doc, text) {
  var el = doc.createElement('style');
  doc.head.appendChild(el);
  var sheet = el.sheet;
  try { sheet.insertRule(text, 0); } catch (e) { return null; }
  return sheet.cssRules.length ? sheet.cssRules[0] : null;
}
function dHolds(rule, descriptor) {
  if (!rule || !rule.cssText) return 0;
  return rule.cssText.indexOf(descriptor + ':') >= 0 ? 1 : 0;
}
function dJob(doc, job, out) {
  var p = 0, o = 0;
  try {
    p = dHolds(dSheet(doc, job.sheet), job.descriptor);
  } catch (e) {
    out.push(['x', job.id, String((e && e.message) || e)].join('\\t'));
    return;
  }
  try {
    var built = new CSSStyleSheet();
    built.replaceSync(job.sheet);
    o = dHolds(built.cssRules.length ? built.cssRules[0] : null, job.descriptor);
  } catch (e) {
    out.push(['x', job.id, String((e && e.message) || e)].join('\\t'));
    return;
  }
  out.push(['v', job.id, p, o].join('\\t'));
}
function dMain(jobs) {
  var out = [];
  jobs.forEach(function (job) {
    try { dJob(document, job, out); }
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
    '<script id="d-jobs" type="application/json">' + payload + '</script>' +
    '<script>' + HARNESS +
    'dMain(JSON.parse(document.getElementById("d-jobs").textContent));</script>' +
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
  const file = path.resolve(workDir, 'descriptors-' + index + '.html');
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
