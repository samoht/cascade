// The frame the page-side harnesses read computed style out of.
//
// A created iframe's document has no doctype, so it is in quirks mode: a
// unitless length is a length, a percentage height resolves against the
// viewport instead of an auto-height parent, and a table does not inherit
// font-size. Every value sampled there answers for a rendering mode no page
// uses, so a clean run in it says nothing about a run under a doctype.
//
// The doctype is written before the frame has parsed anything: a document that
// has already styled something in quirks mode goes on rendering that way after
// compatMode says otherwise. The check is what holds the frame in standards
// mode across a later edit, and every caller reports a failed one as its own
// kind of error.
function rdAssertStandards(doc, what) {
  if (doc.compatMode !== 'CSS1Compat')
    throw new Error(what + ' is in ' + doc.compatMode + ', not CSS1Compat: ' +
      'the reading is a quirks-mode one and answers for no page');
  return doc;
}

function rdStandardsFrame(win, width, height) {
  var frame = win.document.createElement('iframe');
  frame.width = width;
  frame.height = height;
  frame.style.border = '0';
  win.document.body.appendChild(frame);
  var doc = frame.contentDocument;
  doc.open();
  doc.write('<!DOCTYPE html><html><head></head><body></body></html>');
  doc.close();
  return rdAssertStandards(frame.contentDocument, 'the sampling frame');
}
