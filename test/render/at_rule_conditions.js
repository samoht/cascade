// Ask a headless browser which at-rule conditions apply, and in which
// environment.
//
//   node at_rule_conditions.js JOBS.json WORKDIR
//
// JOBS.json is written by at_rule_conditions.exe:
//
//   { "envs":      [{ "id", "flags": [...] }],
//     "media":     [{ "id", "text" }],
//     "nested":    [{ "id", "supports", "media" }],
//     "supports":  [{ "id", "text" }],
//     "hosts":     [{ "id", "style" }],
//     "container": [{ "id", "text" }] }
//
// One browser launch per environment: an environment is a set of command-line
// flags, so it cannot be changed from inside the page. Every environment
// answers the media and nested jobs. The supports and container jobs are
// answered once, in the first environment, because no flag in this list moves
// a feature query or a container's own size.
//
// Each environment is reported before it is used. The page does not assume the
// flags took: it reads back what the browser says the environment is, and the
// caller builds the query context cascade is asked about out of that reading.
//
// Output is TSV on stdout, one record per line:
//   e <env> <feature> <value>    what the browser reports, "?" for a feature it
//                                does not recognise and "-" for one it does
//                                recognise and exposes no value of
//   m <env> <id> <matchMedia> <applied>
//   n <env> <id> <applied>
//   s <id> <CSS.supports> <applied>
//   c <host> <id> <applied>
//   x <scope> <message>
//
// A tab never appears in a condition, a feature name or a verdict, so the
// fields are unambiguous.

const { execFileSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const CHROME = process.env.CHROME;
const jobsFile = process.argv[2];
const workDir = process.argv[3];

// Runs inside the page.
const HARNESS = `
// Media Queries 4 sec. 2.4: a discrete feature is a keyword out of a fixed
// list, so its value is found by asking about every keyword. A feature the
// browser does not recognise is a <general-enclosed> and answers false to all
// of them, which is why a negation is asked too: only an implemented feature
// makes [not (f: k)] true.
var AC_KEYWORDS = {
  'orientation': ['portrait', 'landscape'],
  'scan': ['interlace', 'progressive'],
  'hover': ['none', 'hover'],
  'any-hover': ['none', 'hover'],
  'pointer': ['none', 'coarse', 'fine'],
  'any-pointer': ['none', 'coarse', 'fine'],
  'update': ['none', 'slow', 'fast'],
  'overflow-block': ['none', 'scroll', 'optional-paged', 'paged'],
  'overflow-inline': ['none', 'scroll'],
  'color-gamut': ['srgb', 'p3', 'rec2020'],
  'dynamic-range': ['standard', 'high'],
  'video-dynamic-range': ['standard', 'high'],
  'display-mode': ['fullscreen', 'standalone', 'minimal-ui', 'browser',
                   'picture-in-picture'],
  'environment-blending': ['opaque', 'additive', 'subtractive'],
  'prefers-color-scheme': ['light', 'dark'],
  'prefers-reduced-motion': ['no-preference', 'reduce'],
  'prefers-reduced-transparency': ['no-preference', 'reduce'],
  'prefers-reduced-data': ['no-preference', 'reduce'],
  'prefers-contrast': ['no-preference', 'less', 'more', 'custom'],
  'forced-colors': ['none', 'active'],
  'inverted-colors': ['none', 'inverted'],
  'nav-controls': ['none', 'back'],
  'scripting': ['none', 'initial-only', 'enabled']
};

var AC_INTEGERS = ['color', 'color-index', 'monochrome', 'grid',
                   'horizontal-viewport-segments',
                   'vertical-viewport-segments'];

function acMatch(query) {
  try { return window.matchMedia(query).matches; } catch (e) { return false; }
}

function acKeyword(name) {
  var keywords = AC_KEYWORDS[name], hits = [], known = false, i;
  for (i = 0; i < keywords.length; i++) {
    if (acMatch('(' + name + ': ' + keywords[i] + ')')) hits.push(keywords[i]);
    else if (acMatch('not (' + name + ': ' + keywords[i] + ')')) known = true;
  }
  if (hits.length) return hits.join(',');
  return known ? '-' : '?';
}

// The plain form answers for every implemented feature, so the small values
// are read off directly; the [min-] search is what reaches a large one, and a
// feature added after Level 3 need not take the prefix at all.
function acInteger(name) {
  if (!acMatch('(' + name + ': 0)') && !acMatch('not (' + name + ': 0)'))
    return '?';
  for (var n = 0; n <= 8; n++)
    if (acMatch('(' + name + ': ' + n + ')')) return String(n);
  var low = 0, high = 1;
  while (high < 16777216 && acMatch('(min-' + name + ': ' + high + ')'))
    high = high * 2;
  while (low + 1 < high) {
    var mid = Math.floor((low + high) / 2);
    if (acMatch('(min-' + name + ': ' + mid + ')')) low = mid; else high = mid;
  }
  return acMatch('(' + name + ': ' + low + ')') ? String(low) : '-';
}

function acDiscover(env, out) {
  var width = window.innerWidth, height = window.innerHeight;
  var dpr = window.devicePixelRatio, name;
  out.push(['e', env, 'width',
            acMatch('(width: ' + width + 'px)') ? width + 'px' : '?'].join('\\t'));
  out.push(['e', env, 'height',
            acMatch('(height: ' + height + 'px)') ? height + 'px' : '?'].join('\\t'));
  out.push(['e', env, 'resolution',
            acMatch('(resolution: ' + dpr + 'dppx)') ? dpr + 'dppx' : '?'].join('\\t'));
  out.push(['e', env, 'aspect-ratio',
            acMatch('(aspect-ratio: ' + width + '/' + height + ')')
              ? width + '/' + height : '?'].join('\\t'));
  for (name in AC_KEYWORDS)
    out.push(['e', env, name, acKeyword(name)].join('\\t'));
  for (var i = 0; i < AC_INTEGERS.length; i++)
    out.push(['e', env, AC_INTEGERS[i], acInteger(AC_INTEGERS[i])].join('\\t'));
}

// A custom property is the cheapest thing a rule can set that survives to
// getComputedStyle without a layout of its own, and one probe element carries
// as many of them as the sheet has rules.
function acMedia(env, jobs, style, probe, out) {
  var rules = [], ids = [], i;
  for (i = 0; i < jobs.length; i++) {
    rules.push('@media ' + jobs[i].text + '{#ac-probe{--m' + jobs[i].id + ':1}}');
    ids.push(jobs[i].id);
  }
  style.textContent = rules.join('');
  var computed = window.getComputedStyle(probe);
  for (i = 0; i < jobs.length; i++) {
    var applied =
      computed.getPropertyValue('--m' + jobs[i].id).trim() === '1' ? 1 : 0;
    var matches = acMatch(jobs[i].text) ? 1 : 0;
    out.push(['m', env, jobs[i].id, matches, applied].join('\\t'));
  }
}

function acNested(env, jobs, style, probe, out) {
  var rules = [], i;
  for (i = 0; i < jobs.length; i++)
    rules.push('@supports ' + jobs[i].supports + '{@media ' + jobs[i].media +
               '{#ac-probe{--n' + jobs[i].id + ':1}}}');
  style.textContent = rules.join('');
  var computed = window.getComputedStyle(probe);
  for (i = 0; i < jobs.length; i++)
    out.push(['n', env, jobs[i].id,
              computed.getPropertyValue('--n' + jobs[i].id).trim() === '1' ? 1 : 0]
             .join('\\t'));
}

function acSupports(jobs, style, probe, out) {
  var rules = [], i;
  for (i = 0; i < jobs.length; i++)
    rules.push('@supports ' + jobs[i].text +
               '{#ac-probe{--s' + jobs[i].id + ':1}}');
  style.textContent = rules.join('');
  var computed = window.getComputedStyle(probe);
  for (i = 0; i < jobs.length; i++) {
    var applied =
      computed.getPropertyValue('--s' + jobs[i].id).trim() === '1' ? 1 : 0;
    var supported = 0;
    try { supported = CSS.supports(jobs[i].text) ? 1 : 0; }
    catch (e) { supported = 0; }
    out.push(['s', jobs[i].id, supported, applied].join('\\t'));
  }
}

// A container query resolves against the queried element's own container, so
// every host carries its own probe and the same condition is asked once per
// host.
function acContainer(hosts, jobs, style, out) {
  var rules = [], h, i;
  for (h = 0; h < hosts.length; h++) {
    var container = document.createElement('div');
    container.setAttribute('style', hosts[h].style);
    var probe = document.createElement('div');
    probe.id = 'ac-host-' + hosts[h].id;
    container.appendChild(probe);
    document.body.appendChild(container);
    for (i = 0; i < jobs.length; i++)
      rules.push('@container ' + jobs[i].text + '{#ac-host-' + hosts[h].id +
                 '{--q' + jobs[i].id + ':1}}');
  }
  style.textContent = rules.join('');
  for (h = 0; h < hosts.length; h++) {
    var computed =
      window.getComputedStyle(document.getElementById('ac-host-' + hosts[h].id));
    for (i = 0; i < jobs.length; i++)
      out.push(['c', hosts[h].id, jobs[i].id,
                computed.getPropertyValue('--q' + jobs[i].id).trim() === '1'
                  ? 1 : 0].join('\\t'));
  }
}

function acMain(payload) {
  var out = [];
  var style = document.getElementById('ac-sheet');
  var probe = document.getElementById('ac-probe');
  try { acDiscover(payload.env, out); }
  catch (e) { out.push(['x', 'discover', String((e && e.message) || e)].join('\\t')); }
  try { acMedia(payload.env, payload.media, style, probe, out); }
  catch (e) { out.push(['x', 'media', String((e && e.message) || e)].join('\\t')); }
  try { acNested(payload.env, payload.nested, style, probe, out); }
  catch (e) { out.push(['x', 'nested', String((e && e.message) || e)].join('\\t')); }
  if (payload.supports.length) {
    try { acSupports(payload.supports, style, probe, out); }
    catch (e) { out.push(['x', 'supports', String((e && e.message) || e)].join('\\t')); }
  }
  if (payload.container.length) {
    try { acContainer(payload.hosts, payload.container, style, out); }
    catch (e) { out.push(['x', 'container', String((e && e.message) || e)].join('\\t')); }
  }
  document.body.setAttribute('data-v',
    btoa(unescape(encodeURIComponent(out.join('\\n')))));
}
`;

function page(payload) {
  const json = JSON.stringify(payload).replace(/</g, '\\u003c');
  return '<!doctype html><html><head><meta charset="utf-8">' +
    '<style id="ac-sheet"></style></head><body><div id="ac-probe"></div>' +
    '<script id="ac-jobs" type="application/json">' + json + '</script>' +
    '<script>' + HARNESS +
    'acMain(JSON.parse(document.getElementById("ac-jobs").textContent));</script>' +
    '</body></html>';
}

function run(payload, flags, index) {
  // Absolute: a file:// URL has no notion of the process's directory.
  const file = path.resolve(workDir, 'conditions-' + index + '.html');
  fs.writeFileSync(file, page(payload));
  const args = ['--headless', '--disable-gpu', '--no-sandbox', '--hide-scrollbars']
    .concat(flags)
    .concat(['--virtual-time-budget=120000', '--dump-dom', 'file://' + file]);
  const dom = execFileSync(CHROME, args, { encoding: 'utf8', maxBuffer: 1 << 28 });
  const m = dom.match(/data-v="([^"]*)"/);
  if (!m) throw new Error('the page produced no result for environment ' + index);
  return Buffer.from(m[1], 'base64').toString('utf8');
}

const jobs = JSON.parse(fs.readFileSync(jobsFile, 'utf8'));
jobs.envs.forEach(function (env, i) {
  const payload = {
    env: env.id,
    media: jobs.media,
    nested: jobs.nested,
    supports: i === 0 ? jobs.supports : [],
    hosts: i === 0 ? jobs.hosts : [],
    container: i === 0 ? jobs.container : [],
  };
  const text = run(payload, env.flags, i);
  if (text.length) process.stdout.write(text + '\n');
});
