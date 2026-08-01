// Build the document the OCaml side derived from a stylesheet's selectors.
//
// The tree is built with createElement rather than serialised to HTML text: the
// HTML parser rewrites nesting the selectors asked for (a <div> inside a <p>, a
// <li> outside a <ul>), and the differential test needs the tree it derived,
// not the one a parser is willing to spell. The same builder runs in the driver
// and in the repro pages written next to a failure, so an artefact reproduces
// the run exactly.
function rdBuild(doc, spec) {
  var html = doc.documentElement;
  (spec.htmlClasses || []).forEach(function (c) { html.classList.add(c); });
  (spec.htmlAttrs || []).forEach(function (a) { html.setAttribute(a[0], a[1]); });
  function node(n) {
    var e = doc.createElement(n.t);
    if (n.i) e.id = n.i;
    (n.c || []).forEach(function (c) { e.classList.add(c); });
    (n.a || []).forEach(function (a) { e.setAttribute(a[0], a[1]); });
    (n.k || []).forEach(function (k) { e.appendChild(node(k)); });
    return e;
  }
  (spec.dom.k || []).forEach(function (k) { doc.body.appendChild(node(k)); });
}

// A label a human can find the element by in the repro page.
function rdPath(el) {
  var parts = [];
  while (el && el.nodeType === 1 && el.tagName !== 'BODY') {
    var s = el.tagName.toLowerCase();
    if (el.id) s += '#' + el.id;
    else if (el.className && el.className.baseVal === undefined && el.className)
      s += '.' + String(el.className).trim().split(/\s+/).join('.');
    var p = el.parentNode, i = 1;
    if (p) {
      for (var c = el.previousElementSibling; c; c = c.previousElementSibling) i++;
      s += ':nth-child(' + i + ')';
    }
    parts.unshift(s);
    el = el.parentNode;
  }
  return parts.join('>');
}

if (typeof module !== 'undefined') module.exports = { rdBuild: rdBuild, rdPath: rdPath };
