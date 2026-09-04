// Read every job's two stylesheets through the browser's CSSOM and report the
// rules and declarations each one kept.
//
//   node parse_recovery.js JOBS.json WORKDIR
//
// JOBS.json is written by parse_recovery.exe: an array of jobs, each with an
// id, the malformed input `a`, and cascade's recovery of it `b`. Putting both
// through the same parser is what makes them comparable: whatever the browser
// normalises, it normalises on both sides, so a difference is cascade's.
//
// Output is TSV on stdout, one record per line:
//   r <job> <side> <fact> <fact> ...
//   x <job> <side> <error>
// A fact is a path of `Kind|identity` segments joined by `>`, optionally
// suffixed with `#property`. The three separators and the backslash are
// escaped inside an identity, so the reader can strip identities without
// knowing how a selector is spelled.

const { execFileSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const CHROME = process.env.CHROME;
const jobsFile = process.argv[2];
const workDir = process.argv[3];
const CHUNK_BYTES = 512 * 1024;

// Runs inside the page.
const HARNESS = `
// Descriptors an at-rule exposes as named attributes rather than through a
// style declaration block.
var PR_DESCR = ['syntax', 'inherits', 'initialValue', 'system', 'symbols',
  'additiveSymbols', 'negative', 'prefix', 'suffix', 'range', 'pad',
  'speakAs', 'fallback', 'href', 'layerName', 'nameList', 'supportsText',
  'namespaceURI'];

function prEscape(s) {
  return String(s).replace(/\\\\/g, '\\\\\\\\').replace(/>/g, '\\\\g')
    .replace(/\\|/g, '\\\\p').replace(/#/g, '\\\\h').replace(/\\s+/g, ' ');
}
function prIdentity(r) {
  if (typeof r.selectorText === 'string') return r.selectorText;
  if (typeof r.conditionText === 'string') return r.conditionText;
  if (typeof r.keyText === 'string') return r.keyText;
  if (typeof r.name === 'string') return r.name;
  return '';
}
// A rule that declares nothing, anywhere under it, styles nothing: whether a
// serialiser writes it out or elides it is not a recovery question, and the
// CSSOM keeps every one of them. Reporting them would bury the rules that do
// carry declarations, so an empty subtree contributes no fact and its root
// goes with it. Answers whether anything was contributed.
function prWalk(rules, prefix, out) {
  var any = false;
  for (var i = 0; i < rules.length; i++) {
    var r = rules[i];
    var seg = prefix + r.constructor.name + '|' + prEscape(prIdentity(r));
    var mine = [];
    var st = r.style;
    if (st) for (var j = 0; j < st.length; j++) mine.push(seg + '#' + prEscape(st[j]));
    for (var d = 0; d < PR_DESCR.length; d++) {
      var v = r[PR_DESCR[d]];
      if (v !== undefined && v !== null && v !== '') mine.push(seg + '#' + PR_DESCR[d]);
    }
    var sub = [];
    var kids = r.cssRules ? prWalk(r.cssRules, seg + '>', sub) : false;
    if (mine.length || kids) {
      out.push(seg);
      for (var m = 0; m < mine.length; m++) out.push(mine[m]);
      for (var s = 0; s < sub.length; s++) out.push(sub[s]);
      any = true;
    }
  }
  return any;
}
function prFacts(doc, css) {
  var st = doc.createElement('style');
  st.textContent = css;
  doc.head.appendChild(st);
  var out = [];
  var err = null;
  try { prWalk(st.sheet.cssRules, '', out); }
  catch (e) { err = String((e && e.message) || e); }
  st.parentNode.removeChild(st);
  return { facts: out, error: err };
}
function prMain(jobs) {
  var frame = document.createElement('iframe');
  frame.width = 64;
  frame.height = 64;
  frame.style.border = '0';
  document.body.appendChild(frame);
  // A created iframe's document has no doctype, so it parses CSS in quirks
  // mode, where a unitless length is a length and much else the standards
  // parser refuses is accepted. Writing a doctype into it is what makes the
  // oracle the parser a page gets.
  var doc = frame.contentDocument;
  doc.open();
  doc.write('\u003c!DOCTYPE html>\u003chtml>\u003chead>\u003c/head>' +
    '\u003cbody>\u003c/body>\u003c/html>');
  doc.close();
  doc = frame.contentDocument;
  var out = [];
  if (doc.compatMode !== 'CSS1Compat') {
    out.push(['x', 'page', 'a', 'the oracle is in ' + doc.compatMode].join('\t'));
  }
  jobs.forEach(function (job) {
    ['a', 'b'].forEach(function (side) {
      var got;
      try { got = prFacts(doc, job[side]); }
      catch (e) { got = { facts: [], error: String((e && e.message) || e) }; }
      if (got.error !== null) out.push(['x', job.id, side, got.error].join('\\t'));
      else out.push(['r', job.id, side].concat(got.facts).join('\\t'));
    });
  });
  document.body.setAttribute('data-pr',
    btoa(unescape(encodeURIComponent(out.join('\\n')))));
}
`;

function page(jobs) {
  const payload = JSON.stringify(jobs).replace(/</g, '\\u003c');
  return '<!doctype html><html><head><meta charset="utf-8"></head><body>' +
    '<script id="pr-jobs" type="application/json">' + payload + '</script>' +
    '<script>' + HARNESS +
    'prMain(JSON.parse(document.getElementById("pr-jobs").textContent));</script>' +
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
  const m = dom.match(/data-pr="([^"]*)"/);
  if (!m) throw new Error('the page produced no result for chunk ' + index);
  return Buffer.from(m[1], 'base64').toString('utf8');
}

const jobs = JSON.parse(fs.readFileSync(jobsFile, 'utf8'));
const parts = chunks(jobs);
parts.forEach(function (chunk, i) {
  const text = run(chunk, i);
  if (text.length) process.stdout.write(text + '\n');
});
