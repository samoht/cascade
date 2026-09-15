// Render two stylesheets over one supplied document in a headless browser and
// report every computed-style value the two disagree on.
//
//   CHROME=/path/to/chrome node browser_diff.js JOB.json WORKDIR
//
// JOB.json is written by `cascade diff --browser`: the document's HTML, the
// two sheets, and the sampling policy. The document's own <style> elements and
// stylesheet <link>s are removed before it is written, so each sheet is the
// only one in effect; inline style attributes stay, since they are part of the
// document. A document without a doctype gets one, so the frame renders in
// standards mode, and the report says so.
//
// Every element of the document and the pseudo-elements below are sampled at
// every viewport a media condition in either sheet turns on, plus the default
// one, and once per interaction state either sheet names: a state is written
// as the class rd-s-<state> on every element and the sheets' selectors are
// rewritten in the browser's own CSSOM to read it, so a :hover rule is
// sampled as if everything were hovered. A run that samples nothing fails.
//
// Output is TSV on stdout, one record per line, and a tab never appears in a
// computed-style value:
//   m <key> <value>                                   one metadata line per key
//   d <viewport> <state> <element> <pseudo> <property> <first> <second> <paints>
//   x <error>
// <paints> is 1 when the two values paint the same to a viewer (a colour as
// the pixel it paints, a length within 0.05px, a number within 0.001) and 0
// otherwise; the caller decides what to make of it.

const { execFileSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const CHROME = process.env.CHROME;
const jobFile = process.argv[2];
const workDir = process.argv[3];
const MAX_DIFFS = 5000;

const STATES = [
  'focus-visible', 'focus-within', 'target-within', 'user-valid',
  'user-invalid', '-webkit-autofill', 'autofill', 'popover-open', 'hover',
  'focus', 'active', 'visited', 'target', 'local-link', 'modal', 'fullscreen',
  'indeterminate', 'blank', 'playing', 'paused',
];
const PSEUDOS = ['::before', '::after', '::marker', '::placeholder',
  '::first-letter', '::first-line'];

// Runs inside the page.
const HARNESS = `
function rdPath(el) {
  var parts = [];
  while (el && el.nodeType === 1 && el.tagName !== 'BODY' && el.tagName !== 'HTML') {
    var s = el.tagName.toLowerCase();
    if (el.id) s += '#' + el.id;
    else if (typeof el.className === 'string' && el.className.trim())
      s += '.' + el.className.trim().split(/\\s+/).filter(function (c) {
        return c.indexOf('rd-s-') !== 0;
      }).join('.');
    var p = el.parentNode, i = 1;
    if (p && p.nodeType === 1) {
      for (var c = el.previousElementSibling; c; c = c.previousElementSibling) i++;
      s += ':nth-child(' + i + ')';
    }
    parts.unshift(s);
    el = el.parentNode;
  }
  if (el && el.tagName === 'BODY') parts.unshift('body');
  else if (el && el.tagName === 'HTML') parts.unshift('html');
  return parts.join('>');
}
function rdProps(cs) {
  var props = [];
  for (var i = 0; i < cs.length; i++)
    if (cs[i].indexOf('--') !== 0) props.push(cs[i]);
  return props;
}
function rdStatesIn(rules, found) {
  for (var i = 0; i < rules.length; i++) {
    var r = rules[i];
    if (typeof r.selectorText === 'string')
      STATES.forEach(function (s) {
        if (found.indexOf(s) < 0 &&
            new RegExp('(^|[^:\\\\\\\\]):' + s + '(?![\\\\w-])').test(r.selectorText))
          found.push(s);
      });
    if (r.cssRules) rdStatesIn(r.cssRules, found);
  }
}
function rdRewrite(rules, state) {
  var re = new RegExp('(^|[^:\\\\\\\\]):' + state + '(?![\\\\w-])', 'g');
  for (var i = 0; i < rules.length; i++) {
    var r = rules[i];
    if (typeof r.selectorText === 'string') {
      var s = r.selectorText.replace(re, '$1.rd-s-' + state);
      if (s !== r.selectorText) r.selectorText = s;
    }
    if (r.cssRules) rdRewrite(r.cssRules, state);
  }
}
function rdWidths(rules, px) {
  var re = /\\((?:min-|max-)?width\\s*(?:>=|<=|<|>|:)\\s*([\\d.]+)(rem|px|em)\\)/g;
  for (var i = 0; i < rules.length; i++) {
    var r = rules[i], m;
    if (typeof r.conditionText === 'string')
      while ((m = re.exec(r.conditionText)) !== null) {
        var v = Math.round(parseFloat(m[1]) * (m[2] === 'px' ? 1 : 16));
        if (v > 0 && v <= 2560) { px[v] = 1; px[v - 1] = 1; px[v + 1] = 1; }
      }
    if (r.cssRules) rdWidths(r.cssRules, px);
  }
}
var rdCanvas = null, rdColours = {};
function rdPixel(css) {
  if (rdCanvas === null) {
    var c = document.createElement('canvas');
    c.width = c.height = 1;
    rdCanvas = c.getContext('2d', { willReadFrequently: true });
  }
  var v = rdColours[css];
  if (v === undefined) {
    rdCanvas.clearRect(0, 0, 1, 1);
    rdCanvas.fillStyle = '#000';
    rdCanvas.fillStyle = css;
    rdCanvas.fillRect(0, 0, 1, 1);
    v = 'C(' + Array.prototype.join.call(rdCanvas.getImageData(0, 0, 1, 1).data, ' ') + ')';
    rdColours[css] = v;
  }
  return v;
}
var rdColourRe = /\\b(?:rgba?|hsla?|hwb|lab|lch|oklab|oklch|color)\\((?:[^()]|\\([^()]*\\))*\\)/g;
var rdNumRe = /-?\\d*\\.?\\d+(?:e[-+]?\\d+)?(px|%)?/g;
function rdPaintsSame(a, b) {
  if (a === b) return true;
  var x = a.replace(rdColourRe, rdPixel), y = b.replace(rdColourRe, rdPixel);
  if (x === y) return true;
  var nx = x.match(rdNumRe) || [], ny = y.match(rdNumRe) || [];
  if (nx.length !== ny.length || x.replace(rdNumRe, '#') !== y.replace(rdNumRe, '#'))
    return false;
  for (var i = 0; i < nx.length; i++) {
    var u = parseFloat(nx[i]), v = parseFloat(ny[i]);
    var ua = /px$/.test(nx[i]) ? 'px' : /%$/.test(nx[i]) ? '%' : '';
    var ub = /px$/.test(ny[i]) ? 'px' : /%$/.test(ny[i]) ? '%' : '';
    if (ua !== ub && !(u === 0 && v === 0)) return false;
    if (Math.abs(u - v) > (ua || ub ? 0.05 : 0.001)) return false;
  }
  return true;
}
function rdSnapshot(view, els, props) {
  var snap = [], n = props.length;
  for (var i = 0; i < els.length; i++)
    for (var p = -1; p < PSEUDOS.length; p++) {
      var cs = p < 0 ? view.getComputedStyle(els[i])
                     : view.getComputedStyle(els[i], PSEUDOS[p]);
      var row = new Array(n);
      for (var j = 0; j < n; j++) row[j] = cs.getPropertyValue(props[j]);
      snap.push(row);
    }
  return snap;
}
function rdMain(job) {
  var out = [];
  var frame = document.createElement('iframe');
  frame.width = 1024; frame.height = 768; frame.style.border = '0';
  document.body.appendChild(frame);
  var doc = frame.contentDocument;
  doc.open(); doc.write(job.html); doc.close();
  if (doc.compatMode !== 'CSS1Compat')
    throw new Error('the document renders in ' + doc.compatMode + ', not standards mode');
  var view = doc.defaultView;
  var els = [doc.documentElement];
  var found = doc.documentElement.querySelectorAll('*');
  for (var i = 0; i < found.length; i++) els.push(found[i]);
  if (els.length < 2) throw new Error('the document has no element to sample');
  var labels = [];
  for (i = 0; i < els.length; i++)
    for (var p = -1; p < PSEUDOS.length; p++)
      labels.push([els[i] === doc.documentElement ? 'html' : rdPath(els[i]),
                   p < 0 ? '' : PSEUDOS[p]]);
  var style = doc.createElement('style');
  doc.head.appendChild(style);
  // The states and viewports either sheet names, read off the parsed sheets.
  var states = [], px = {};
  job.sheets.forEach(function (sheet) {
    style.textContent = sheet.css;
    rdStatesIn(style.sheet.cssRules, states);
    rdWidths(style.sheet.cssRules, px);
  });
  var widths = [1024];
  Object.keys(px).forEach(function (w) { widths.push(parseInt(w, 10)); });
  widths = widths.filter(function (w, i, a) { return a.indexOf(w) === i; })
                 .sort(function (a, b) { return a - b; });
  var runs = ['none'].concat(states);
  style.textContent = '';
  var props = rdProps(view.getComputedStyle(doc.body || doc.documentElement));
  out.push(['m', 'elements', String(els.length)].join('\\t'));
  out.push(['m', 'properties', String(props.length)].join('\\t'));
  out.push(['m', 'pseudos', PSEUDOS.join(' ')].join('\\t'));
  out.push(['m', 'states', runs.join(' ')].join('\\t'));
  out.push(['m', 'viewports', widths.map(function (w) { return w + 'x768'; }).join(' ')].join('\\t'));
  var diffs = 0, samples = 0;
  widths.forEach(function (width) {
    frame.width = width;
    doc.body.offsetWidth;
    runs.forEach(function (state) {
      if (state !== 'none')
        for (var e = 0; e < els.length; e++) els[e].classList.add('rd-s-' + state);
      var base = null;
      job.sheets.forEach(function (sheet) {
        style.textContent = sheet.css;
        if (state !== 'none') rdRewrite(style.sheet.cssRules, state);
        doc.body.offsetWidth;
        // Transitions are finished and animations held at their start, so
        // the two sheets are sampled at one point rather than wherever each
        // had got to.
        if (typeof doc.getAnimations === 'function')
          doc.getAnimations().forEach(function (a) {
            if (a.constructor.name === 'CSSTransition') a.finish();
            else { a.pause(); a.currentTime = 0; }
          });
        var snap = rdSnapshot(view, els, props);
        samples += snap.length * props.length;
        if (base === null) { base = snap; return; }
        for (var e = 0; e < base.length; e++)
          for (var j = 0; j < props.length; j++)
            if (base[e][j] !== snap[e][j]) {
              diffs++;
              if (diffs <= MAX_DIFFS)
                out.push(['d', width + 'x768', state, labels[e][0], labels[e][1],
                          props[j], base[e][j], snap[e][j],
                          rdPaintsSame(base[e][j], snap[e][j]) ? '1' : '0'].join('\\t'));
            }
      });
      if (state !== 'none')
        for (var e = 0; e < els.length; e++) els[e].classList.remove('rd-s-' + state);
    });
  });
  out.push(['m', 'samples', String(samples)].join('\\t'));
  out.push(['m', 'differences', String(diffs)].join('\\t'));
  if (diffs > MAX_DIFFS) out.push(['m', 'truncated', String(MAX_DIFFS)].join('\\t'));
  return out.join('\\n');
}
function rdRun(job) {
  var text;
  try { text = rdMain(job); }
  catch (e) { text = ['x', String((e && e.message) || e)].join('\\t'); }
  document.body.setAttribute('data-rd', btoa(unescape(encodeURIComponent(text))));
}
`;

// The document's own styles go, and a doctype comes, before the browser sees
// it: a stylesheet link would be fetched and a missing doctype would put the
// frame in quirks mode, where every sampled value answers for no page.
function prepare(html) {
  let removed = 0;
  html = html.replace(/<link\b[^>]*\brel\s*=\s*["']?\s*stylesheet\b[^>]*>/gi,
    function () { removed++; return ''; });
  html = html.replace(/<style\b[^>]*>[\s\S]*?<\/style\s*>/gi,
    function () { removed++; return ''; });
  let doctype = false;
  if (!/^\s*<!doctype\b/i.test(html)) { html = '<!DOCTYPE html>\n' + html; doctype = true; }
  return { html, removed, doctype };
}

function page(job) {
  const payload = JSON.stringify(job).replace(/</g, '\\u003c');
  return '<!doctype html><html><head><meta charset="utf-8"></head><body>' +
    '<script id="rd-job" type="application/json">' + payload + '</script>' +
    '<script>var STATES = ' + JSON.stringify(STATES) + ', PSEUDOS = ' +
    JSON.stringify(PSEUDOS) + ', MAX_DIFFS = ' + MAX_DIFFS + ';' + HARNESS +
    'rdRun(JSON.parse(document.getElementById("rd-job").textContent));</script>' +
    '</body></html>';
}

const job = JSON.parse(fs.readFileSync(jobFile, 'utf8'));
const prepared = prepare(job.html);
job.html = prepared.html;
const file = path.resolve(workDir, 'page.html');
fs.writeFileSync(file, page(job));
const dom = execFileSync(CHROME, [
  '--headless', '--disable-gpu', '--no-sandbox',
  '--virtual-time-budget=120000', '--dump-dom', 'file://' + file,
], { encoding: 'utf8', maxBuffer: 1 << 28 });
const m = dom.match(/data-rd="([^"]*)"/);
if (!m) throw new Error('the page produced no result');
const lines = [
  ['m', 'document_styles_removed', String(prepared.removed)].join('\t'),
  ['m', 'doctype_added', prepared.doctype ? '1' : '0'].join('\t'),
  Buffer.from(m[1], 'base64').toString('utf8'),
];
process.stdout.write(lines.join('\n') + '\n');
