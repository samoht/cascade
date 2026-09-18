// Render two stylesheets over one supplied document in a headless browser and
// report where the two renders differ.
//
//   CHROME=/path/to/chrome node browser_diff.js JOB.json WORKDIR
//
// JOB.json is written by Browser_compare.run: the document's HTML, the sheets
// rendered, and the sheets sampled for. The document's own <style> elements
// and stylesheet <link>s are removed before it is written, so each sheet is the
// only one in effect; inline style attributes stay, since they are part of the
// document. A document without a doctype gets one, so the page renders in
// standards mode, and the report says so. The caller prepares the document
// itself and the same steps below then find nothing left to do.
//
// The verdict is the raster. The page is loaded afresh under each sheet, at
// every viewport width a media condition in any sampling sheet turns on plus
// the default one, and once per interaction state a sampling sheet names; a
// state is applied by rewriting the sheet's selectors in the browser's own
// CSSOM to match every element, so a :hover rule renders as if everything were
// hovered. Every animation is held at its start and every transition finished
// before the whole page is captured, and the two captures are compared pixel
// for pixel. Equal captures everywhere mean the two sheets render the same,
// and nothing else runs.
//
// Where a capture differs, the computed styles of the elements under the
// differing pixels are read under both sheets and what they disagree on is
// listed, so the report points at a property. A computed value that differs
// over pixels that do not is not reported: what a script reads back through
// getComputedStyle is not what the browser renders.
//
// Output is TSV on stdout, one record per line, and a tab never appears in a
// computed-style value:
//   m <key> <value>                                   one metadata line per key
//   r <viewport> <state> <x> <y> <width> <height> <first> <second>
//                                            a render difference: the box the
//                                            differing pixels span, and each
//                                            capture's size as WxH
//   d <viewport> <state> <element> <pseudo> <property> <first> <second>
//                                            a computed value the elements
//                                            under that box disagree on
//   x <error>
//
// The browser is driven over DevTools Protocol through a pipe, so no port is
// opened and nothing beyond node and the browser is needed.

const { spawn } = require('child_process');
const fs = require('fs');
const path = require('path');
const zlib = require('zlib');

const CHROME = process.env.CHROME;
const jobFile = process.argv[2];
const workDir = process.argv[3];
const MAX_DIFFS = 5000;
const VIEWPORT_HEIGHT = 768;

const STATES = [
  'focus-visible', 'focus-within', 'target-within', 'user-valid',
  'user-invalid', '-webkit-autofill', 'autofill', 'popover-open', 'hover',
  'focus', 'active', 'visited', 'target', 'local-link', 'modal', 'fullscreen',
  'indeterminate', 'blank', 'playing', 'paused',
];
const PSEUDOS = ['::before', '::after', '::marker', '::placeholder',
  '::first-letter', '::first-line'];

// ===== The page =====
//
// Runs inside the page, before its body is parsed: the sheet the URL names is
// installed and its state rewritten while nothing has rendered yet, so no
// transition fires on the way to the sampled state.
const HARNESS = `
(function () {
  var q = {};
  location.search.slice(1).split('&').forEach(function (kv) {
    var i = kv.indexOf('=');
    if (i > 0) q[kv.slice(0, i)] = decodeURIComponent(kv.slice(i + 1));
  });
  RD.query = q;
  var sheets = JSON.parse(document.getElementById('rd-sheets').textContent);
  RD.sheets = sheets;
  if (q.sheet === undefined) return;
  var style = document.createElement('style');
  style.textContent = sheets[parseInt(q.sheet, 10)].css;
  document.head.appendChild(style);
  RD.style = style;
  if (q.state && q.state !== 'none') RD.rewrite(style.sheet.cssRules, q.state);
})();
`;

const HELPERS = `
var RD = {};
// The state is applied by rewriting :state into a pseudo-class every element
// matches at the same specificity, so a :hover rule renders as if everything
// were hovered and :not(:hover) as if nothing were.
RD.rewrite = function (rules, state) {
  var re = new RegExp('(^|[^:\\\\\\\\]):' + state + '(?![\\\\w-])', 'g');
  for (var i = 0; i < rules.length; i++) {
    var r = rules[i];
    if (typeof r.selectorText === 'string') {
      var s = r.selectorText.replace(re, '$1:not(.rd-never)');
      if (s !== r.selectorText) r.selectorText = s;
    }
    if (r.cssRules) RD.rewrite(r.cssRules, state);
  }
};
RD.statesIn = function (rules, found) {
  for (var i = 0; i < rules.length; i++) {
    var r = rules[i];
    if (typeof r.selectorText === 'string')
      STATES.forEach(function (s) {
        if (found.indexOf(s) < 0 &&
            new RegExp('(^|[^:\\\\\\\\]):' + s + '(?![\\\\w-])').test(r.selectorText))
          found.push(s);
      });
    if (r.cssRules) RD.statesIn(r.cssRules, found);
  }
};
// A width a media condition names, and the pixel either side of it, so a
// range is sampled on both sides of its edge.
RD.widths = function (rules, px) {
  var re = /\\((?:min-|max-)?width\\s*(?:>=|<=|<|>|:)\\s*([\\d.]+)(rem|px|em)\\)/g;
  for (var i = 0; i < rules.length; i++) {
    var r = rules[i], m;
    if (typeof r.conditionText === 'string')
      while ((m = re.exec(r.conditionText)) !== null) {
        var v = Math.round(parseFloat(m[1]) * (m[2] === 'px' ? 1 : 16));
        if (v > 0 && v <= 2560) { px[v] = 1; px[v - 1] = 1; px[v + 1] = 1; }
      }
    if (r.cssRules) RD.widths(r.cssRules, px);
  }
};
// The states and viewports the sampling sheets name, read off them parsed.
RD.plan = function () {
  var style = document.createElement('style');
  document.head.appendChild(style);
  var states = [], px = {};
  (RD.sampling || RD.sheets).forEach(function (sheet) {
    style.textContent = sheet.css;
    RD.statesIn(style.sheet.cssRules, states);
    RD.widths(style.sheet.cssRules, px);
  });
  style.remove();
  var widths = [1024];
  Object.keys(px).forEach(function (w) { widths.push(parseInt(w, 10)); });
  widths = widths.filter(function (w, i, a) { return a.indexOf(w) === i; })
                 .sort(function (a, b) { return a - b; });
  var props = [];
  var cs = getComputedStyle(document.body || document.documentElement);
  for (var i = 0; i < cs.length; i++)
    if (cs[i].indexOf('--') !== 0) props.push(cs[i]);
  return { states: ['none'].concat(states), widths: widths, props: props,
           elements: RD.elements().length };
};
RD.elements = function () {
  var els = [document.documentElement];
  var found = document.documentElement.querySelectorAll('*');
  for (var i = 0; i < found.length; i++) els.push(found[i]);
  return els;
};
RD.path = function (el) {
  var parts = [];
  while (el && el.nodeType === 1 && el.tagName !== 'BODY' && el.tagName !== 'HTML') {
    var s = el.tagName.toLowerCase();
    if (el.id) s += '#' + el.id;
    else if (typeof el.className === 'string' && el.className.trim())
      s += '.' + el.className.trim().split(/\\s+/).join('.');
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
};
// Every animation held at its start and every transition finished, so a
// capture is taken at one point rather than wherever each had got to.
RD.settle = function () {
  if (typeof document.getAnimations !== 'function') return;
  document.getAnimations().forEach(function (a) {
    if (a.constructor.name === 'CSSTransition') a.finish();
    else { a.pause(); a.currentTime = 0; }
  });
};
// The document's size and every element's box, in page pixels.
RD.boxes = function () {
  var els = RD.elements(), boxes = new Array(els.length);
  var sx = window.scrollX, sy = window.scrollY;
  for (var i = 0; i < els.length; i++) {
    var r = els[i].getBoundingClientRect();
    boxes[i] = [r.left + sx, r.top + sy, r.width, r.height];
  }
  var de = document.documentElement;
  return { width: Math.max(de.scrollWidth, de.clientWidth),
           height: Math.max(de.scrollHeight, de.clientHeight), boxes: boxes };
};
// The computed styles of the elements at [indexes], each with its
// pseudo-elements, over [props].
RD.snapshot = function (indexes, props) {
  var els = RD.elements(), snap = [];
  indexes.forEach(function (i) {
    var el = els[i];
    for (var p = -1; p < PSEUDOS.length; p++) {
      var cs = p < 0 ? getComputedStyle(el) : getComputedStyle(el, PSEUDOS[p]);
      var row = new Array(props.length);
      for (var j = 0; j < props.length; j++) row[j] = cs.getPropertyValue(props[j]);
      snap.push([i === 0 ? 'html' : RD.path(el), p < 0 ? '' : PSEUDOS[p], row]);
    }
  });
  return snap;
};
`;

// The document's own styles go, and a doctype comes, before the browser sees
// it: a stylesheet link would be fetched and a missing doctype would put the
// page in quirks mode, where every rendered result answers for no page.
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

// The harness goes at the top of <head>, ahead of anything the document
// itself puts there, and the sheets ride along as data.
function page(job) {
  const sheets = JSON.stringify(job.sheets).replace(/</g, '\\u003c');
  const head =
    '<script id="rd-sheets" type="application/json">' + sheets + '</script>' +
    '<script>var STATES = ' + JSON.stringify(STATES) + ', PSEUDOS = ' +
    JSON.stringify(PSEUDOS) + ';' + HELPERS + HARNESS + '</script>';
  const html = job.html;
  const m = html.match(/<head\b[^>]*>/i);
  if (m) return html.slice(0, m.index + m[0].length) + head + html.slice(m.index + m[0].length);
  const h = html.match(/<html\b[^>]*>/i);
  if (h) return html.slice(0, h.index + h[0].length) + '<head>' + head + '</head>' + html.slice(h.index + h[0].length);
  return '<!DOCTYPE html><html><head>' + head + '</head>' + html.replace(/^\s*<!doctype[^>]*>/i, '') + '</html>';
}

// ===== The browser =====

class Session {
  constructor(chrome) {
    this.child = spawn(chrome, [
      '--headless', '--disable-gpu', '--no-sandbox', '--hide-scrollbars',
      '--force-device-scale-factor=1', '--remote-debugging-pipe', 'about:blank',
    ], { stdio: ['ignore', 'ignore', 'pipe', 'pipe', 'pipe'] });
    this.stderr = '';
    this.child.stderr.on('data', (d) => { this.stderr += d; });
    this.inp = this.child.stdio[3];
    this.outp = this.child.stdio[4];
    this.id = 0;
    this.pending = new Map();
    this.waiting = [];
    this.buf = '';
    this.closed = null;
    this.outp.on('data', (d) => this.receive(d));
    this.child.on('exit', (code, signal) => {
      this.closed = new Error('the browser exited (' + (signal || code) + ')' +
        (this.stderr ? ': ' + this.stderr.trim().split('\n').pop() : ''));
      for (const [, reject] of this.pending) reject[1](this.closed);
      this.pending.clear();
      for (const w of this.waiting) w.reject(this.closed);
      this.waiting = [];
    });
  }
  receive(d) {
    this.buf += d.toString('utf8');
    let i;
    while ((i = this.buf.indexOf('\0')) >= 0) {
      const msg = JSON.parse(this.buf.slice(0, i));
      this.buf = this.buf.slice(i + 1);
      if (msg.id !== undefined && this.pending.has(msg.id)) {
        const [resolve, reject] = this.pending.get(msg.id);
        this.pending.delete(msg.id);
        if (msg.error) reject(new Error(msg.error.message));
        else resolve(msg.result);
      } else if (msg.method) {
        const k = this.waiting.findIndex((w) => w.method === msg.method &&
          (!w.sessionId || w.sessionId === msg.sessionId));
        if (k >= 0) this.waiting.splice(k, 1)[0].resolve(msg.params);
      }
    }
  }
  send(method, params, sessionId) {
    if (this.closed) return Promise.reject(this.closed);
    return new Promise((resolve, reject) => {
      const m = { id: ++this.id, method, params: params || {} };
      if (sessionId) m.sessionId = sessionId;
      this.pending.set(m.id, [resolve, reject]);
      this.inp.write(JSON.stringify(m) + '\0');
    });
  }
  event(method, sessionId) {
    if (this.closed) return Promise.reject(this.closed);
    return new Promise((resolve, reject) => {
      this.waiting.push({ method, sessionId, resolve, reject });
    });
  }
  async close() {
    try { await this.send('Browser.close'); } catch (e) { /* already gone */ }
    this.child.kill();
  }
}

class Page {
  constructor(session, sessionId) { this.s = session; this.sid = sessionId; }
  static async open(session) {
    const { targetId } = await session.send('Target.createTarget', { url: 'about:blank' });
    const { sessionId } = await session.send('Target.attachToTarget', { targetId, flatten: true });
    const p = new Page(session, sessionId);
    await p.cmd('Page.enable');
    await p.cmd('Runtime.enable');
    return p;
  }
  cmd(method, params) { return this.s.send(method, params, this.sid); }
  async eval(expression) {
    const r = await this.cmd('Runtime.evaluate', { expression, returnByValue: true, awaitPromise: true });
    if (r.exceptionDetails) {
      const e = r.exceptionDetails.exception;
      throw new Error('in the page: ' + ((e && (e.description || e.value)) || r.exceptionDetails.text));
    }
    return r.result.value;
  }
  async viewport(width) {
    await this.cmd('Emulation.setDeviceMetricsOverride',
      { width, height: VIEWPORT_HEIGHT, deviceScaleFactor: 1, mobile: false });
  }
  // A fresh load of the page under one sheet in one state, settled.
  async load(url, query) {
    const q = Object.keys(query).map((k) => k + '=' + encodeURIComponent(query[k])).join('&');
    const loaded = this.s.event('Page.loadEventFired', this.sid);
    await this.cmd('Page.navigate', { url: url + '?' + q });
    await loaded;
    await this.eval('RD.settle(); document.fonts.ready.then(function () { return true; })');
  }
  async capture() {
    const { width, height, boxes } = await this.eval('JSON.stringify(RD.boxes())').then(JSON.parse);
    const w = Math.max(1, Math.ceil(width)), h = Math.max(1, Math.ceil(height));
    const { data } = await this.cmd('Page.captureScreenshot', {
      format: 'png', captureBeyondViewport: true,
      clip: { x: 0, y: 0, width: w, height: h, scale: 1 },
    });
    return { png: Buffer.from(data, 'base64'), width: w, height: h, boxes };
  }
}

// ===== The raster =====

// A PNG as the browser writes one: 8-bit, non-interlaced, RGB or RGBA. The
// bytes come back one row after another with the filter undone, four channels
// per pixel.
function decodePng(buf) {
  if (buf.readUInt32BE(0) !== 0x89504e47) throw new Error('not a PNG');
  let pos = 8, width = 0, height = 0, channels = 0, depth = 0;
  const idat = [];
  while (pos < buf.length) {
    const len = buf.readUInt32BE(pos), type = buf.toString('ascii', pos + 4, pos + 8);
    const data = buf.subarray(pos + 8, pos + 8 + len);
    if (type === 'IHDR') {
      width = data.readUInt32BE(0); height = data.readUInt32BE(4);
      depth = data[8];
      const colour = data[9];
      channels = { 0: 1, 2: 3, 4: 2, 6: 4 }[colour];
      if (depth !== 8 || channels === undefined || data[12] !== 0)
        throw new Error('unexpected PNG encoding (depth ' + depth + ', colour type ' + colour + ')');
    } else if (type === 'IDAT') idat.push(data);
    else if (type === 'IEND') break;
    pos += 12 + len;
  }
  const raw = zlib.inflateSync(Buffer.concat(idat));
  const bpp = channels, stride = width * bpp;
  const out = Buffer.alloc(width * height * 4);
  let prev = Buffer.alloc(stride);
  for (let y = 0; y < height; y++) {
    const filter = raw[y * (stride + 1)];
    const line = Buffer.from(raw.subarray(y * (stride + 1) + 1, (y + 1) * (stride + 1)));
    for (let i = 0; i < stride; i++) {
      const a = i >= bpp ? line[i - bpp] : 0, b = prev[i], c = i >= bpp ? prev[i - bpp] : 0;
      let x = line[i];
      if (filter === 1) x += a;
      else if (filter === 2) x += b;
      else if (filter === 3) x += (a + b) >> 1;
      else if (filter === 4) {
        const p = a + b - c, pa = Math.abs(p - a), pb = Math.abs(p - b), pc = Math.abs(p - c);
        x += (pa <= pb && pa <= pc) ? a : (pb <= pc ? b : c);
      }
      line[i] = x & 255;
    }
    for (let x = 0; x < width; x++) {
      const o = (y * width + x) * 4, s = x * bpp;
      if (channels === 4) { out[o] = line[s]; out[o + 1] = line[s + 1]; out[o + 2] = line[s + 2]; out[o + 3] = line[s + 3]; }
      else if (channels === 3) { out[o] = line[s]; out[o + 1] = line[s + 1]; out[o + 2] = line[s + 2]; out[o + 3] = 255; }
      else if (channels === 2) { out[o] = out[o + 1] = out[o + 2] = line[s]; out[o + 3] = line[s + 1]; }
      else { out[o] = out[o + 1] = out[o + 2] = line[s]; out[o + 3] = 255; }
    }
    prev = line;
  }
  return { width, height, pixels: out };
}

// The box the differing pixels of two captures span, or null when they are
// the same picture. Two captures of different sizes differ over their whole
// extent: the page laid out differently.
function rasterDifference(a, b) {
  if (a.png.equals(b.png)) return null;
  if (a.width !== b.width || a.height !== b.height)
    return { x: 0, y: 0, width: Math.max(a.width, b.width), height: Math.max(a.height, b.height) };
  const p = decodePng(a.png), q = decodePng(b.png);
  if (p.width !== q.width || p.height !== q.height)
    return { x: 0, y: 0, width: Math.max(p.width, q.width), height: Math.max(p.height, q.height) };
  let x0 = p.width, y0 = p.height, x1 = -1, y1 = -1;
  const pa = p.pixels, qa = q.pixels;
  for (let y = 0; y < p.height; y++) {
    const row = y * p.width * 4;
    if (pa.compare(qa, row, row + p.width * 4, row, row + p.width * 4) === 0) continue;
    for (let x = 0; x < p.width; x++) {
      const o = row + x * 4;
      if (pa[o] !== qa[o] || pa[o + 1] !== qa[o + 1] || pa[o + 2] !== qa[o + 2] || pa[o + 3] !== qa[o + 3]) {
        if (x < x0) x0 = x; if (x > x1) x1 = x; if (y < y0) y0 = y; if (y > y1) y1 = y;
      }
    }
  }
  if (x1 < 0) return null;
  return { x: x0, y: y0, width: x1 - x0 + 1, height: y1 - y0 + 1 };
}

function intersects(box, r) {
  return box[2] > 0 && box[3] > 0 &&
    box[0] < r.x + r.width && box[0] + box[2] > r.x &&
    box[1] < r.y + r.height && box[1] + box[3] > r.y;
}

// ===== The run =====

async function main() {
  const job = JSON.parse(fs.readFileSync(jobFile, 'utf8'));
  const prepared = prepare(job.html);
  job.html = prepared.html;
  const file = path.resolve(workDir, 'page.html');
  fs.writeFileSync(file, page(job));
  const url = 'file://' + file;
  const out = [];
  const meta = (k, v) => out.push(['m', k, String(v)].join('\t'));
  meta('document_styles_removed', prepared.removed);
  meta('doctype_added', prepared.doctype ? '1' : '0');
  const session = new Session(CHROME);
  try {
    const pg = await Page.open(session);
    await pg.viewport(1024);
    await pg.load(url, {});
    if (job.sampling)
      await pg.eval('RD.sampling = ' + JSON.stringify(job.sampling) + '; true');
    const plan = JSON.parse(await pg.eval('JSON.stringify(RD.plan())'));
    if (plan.elements < 2) throw new Error('the document has no element to sample');
    const readyState = await pg.eval('document.compatMode');
    if (readyState !== 'CSS1Compat')
      throw new Error('the document renders in ' + readyState + ', not standards mode');
    meta('elements', plan.elements);
    meta('properties', plan.props.length);
    meta('pseudos', PSEUDOS.join(' '));
    meta('states', plan.states.join(' '));
    meta('viewports', plan.widths.map((w) => w + 'x' + VIEWPORT_HEIGHT).join(' '));
    let captures = 0, renders = 0, diffs = 0, samples = 0;
    for (const width of plan.widths) {
      await pg.viewport(width);
      const viewport = width + 'x' + VIEWPORT_HEIGHT;
      for (const state of plan.states) {
        const shots = [];
        for (let i = 0; i < job.sheets.length; i++) {
          await pg.load(url, { sheet: i, state });
          shots.push(await pg.capture());
          captures++;
        }
        const region = rasterDifference(shots[0], shots[1]);
        if (!region) continue;
        renders++;
        out.push(['r', viewport, state, region.x, region.y, region.width, region.height,
          shots[0].width + 'x' + shots[0].height, shots[1].width + 'x' + shots[1].height].join('\t'));
        // The elements under the box, on either side, and what they compute.
        const under = new Set();
        shots.forEach((s) => s.boxes.forEach((b, i) => { if (intersects(b, region)) under.add(i); }));
        const indexes = Array.from(under).sort((a, b) => a - b);
        if (indexes.length === 0) continue;
        const snaps = [];
        for (let i = 0; i < job.sheets.length; i++) {
          await pg.load(url, { sheet: i, state });
          snaps.push(JSON.parse(await pg.eval(
            'JSON.stringify(RD.snapshot(' + JSON.stringify(indexes) + ', ' + JSON.stringify(plan.props) + '))')));
        }
        const [base, other] = snaps;
        samples += base.length * plan.props.length;
        for (let e = 0; e < base.length; e++)
          for (let j = 0; j < plan.props.length; j++)
            if (base[e][2][j] !== other[e][2][j]) {
              diffs++;
              if (diffs <= MAX_DIFFS)
                out.push(['d', viewport, state, base[e][0], base[e][1], plan.props[j],
                  base[e][2][j], other[e][2][j]].join('\t'));
            }
      }
    }
    meta('captures', captures);
    meta('renders', renders);
    meta('samples', samples);
    meta('differences', diffs);
    if (diffs > MAX_DIFFS) meta('truncated', MAX_DIFFS);
  } catch (e) {
    out.push(['x', String((e && e.message) || e)].join('\t'));
  } finally {
    await session.close();
  }
  process.stdout.write(out.join('\n') + '\n');
}

main().catch((e) => { process.stderr.write(String((e && e.stack) || e) + '\n'); process.exit(1); });
