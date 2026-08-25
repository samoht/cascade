const { execSync } = require('child_process');
const fs = require('fs');
const CHROME = process.env.CHROME;
const SKIP = { STYLE:1, SCRIPT:1, HEAD:1, META:1, TITLE:1, BASE:1, LINK:1, HTML:1 };
const EXTRACTOR = '<script>(function(){var s=' + JSON.stringify(SKIP) +
  ';var e=document.querySelectorAll("*"),o=[];for(var i=0;i<e.length;i++){var x=e[i];if(s[x.tagName])continue;' +
  // skip custom properties (--*): they affect rendering only through var() in
  // real properties, which are already resolved and compared below.
  'var c=getComputedStyle(x),r={_tag:x.tagName};for(var j=0;j<c.length;j++){var p=c[j];if(p.indexOf("--")===0)continue;r[p]=c.getPropertyValue(p);}o.push(r);}' +
  'document.body.setAttribute("data-xtest",btoa(unescape(encodeURIComponent(JSON.stringify(o)))));})();</script>';
// Rendering has to be a pure function of the page: a computed style that
// depends on when the network gave up, how wide the window is or what the
// display scale is makes the difference list change run to run. The pages
// themselves are frozen by freeze_page.js at fetch time; these pin the browser.
//   host-resolver-rules   nothing resolves, so a stray remote URL fails at once
//   window-size, scale    viewport units and device pixels are fixed
//   hide-scrollbars       a scrollbar does not eat viewport width
const budget = process.env.VIRTUAL_TIME_BUDGET || '4000';
if (!/^\d+$/.test(budget)) throw new Error('invalid VIRTUAL_TIME_BUDGET: '+budget);
const FLAGS = '--headless --disable-gpu --no-sandbox' +
  ' --host-resolver-rules="MAP * ~NOTFOUND" --window-size=1280,900' +
  ' --force-device-scale-factor=1 --hide-scrollbars --virtual-time-budget='+budget;
function computed(path){
  // Appended, not spliced in at </body>: a page can carry that text inside an
  // attribute value (wikipedia's CSS article quotes a whole HTML document in
  // one), and splicing there puts the extractor somewhere it never runs. The
  // parser reparents trailing content into the body, so the end of the file
  // reaches the same place.
  const h = fs.readFileSync(path,'utf8') + EXTRACTOR;
  const tmp = '/tmp/xt_'+process.pid+'_'+path.replace(/\W/g,'_')+'.html';
  fs.writeFileSync(tmp,h);
  // Chrome's stderr is console chatter from the page (a blocked preload, a
  // CORS notice); dropping it keeps a caller's failure report readable. A
  // browser that fails outright still shows up: there is no data-xtest.
  const dom = execSync(`"${CHROME}" ${FLAGS} --dump-dom "file://${tmp}"`,{encoding:'utf8',maxBuffer:1e8,stdio:['ignore','pipe','ignore']});
  const m = dom.match(/data-xtest="([^"]*)"/);
  if(!m) throw new Error('no data-xtest for '+path);
  return JSON.parse(Buffer.from(m[1],'base64').toString('utf8'));
}
// --self renders one page twice and compares it with itself. Nothing
// transformed it in between, so a difference here is the page's own, and every
// later count taken from it reads as a defect in whichever transform happened
// to be measured. run.sh settles that before it believes any count.
const self = process.argv[2] === '--self';
const [pathA, pathB] = self
  ? [process.argv[3], process.argv[3]]
  : [process.argv[2], process.argv[3]];
const a = computed(pathA), b = computed(pathB);
// The two lists line up by position, so the comparison is only meaningful up to
// the first place the trees disagree. Past a dropped or added element every
// later index compares two different elements, and one structural change
// reports as thousands, which makes the total say nothing about the transform.
// Stop there and name the position instead.
let n = Math.min(a.length,b.length), split = null;
for (let i=0;i<n;i++) if (a[i]._tag !== b[i]._tag) {
  split = 'element trees diverge at index '+i+': '+a[i]._tag+' vs '+b[i]._tag+
    ' ('+(Math.max(a.length,b.length)-i)+' element(s) past it not compared)';
  n = i; break;
}
if (split === null && a.length !== b.length)
  split = 'element count '+a.length+' vs '+b.length;
// Candidate differences: any property whose getComputedStyle string differs.
const cand = [];
for (let i=0;i<n;i++) for (const p in a[i]) if (a[i][p]!==b[i][p])
  cand.push(i+'\t'+a[i]._tag+'\t'+p+'\t'+a[i][p]+'\t'+b[i][p]);
// Reduce to render-real differences: a pair like "0% 0%" vs "0px 0px" or "red"
// vs "rgb(255, 0, 0)" is the same render, so canon_filter (Css_compare
// ~mode:Canonical) drops it; only an actual render change survives.
let lines = cand;
const CANON = process.env.CANON_FILTER;
if (CANON && cand.length) {
  const tmp = '/tmp/canon_'+process.pid+'.tsv';
  fs.writeFileSync(tmp, cand.join('\n'));
  lines = execSync(`"${CANON}" < "${tmp}"`,{encoding:'utf8',maxBuffer:1e8}).split('\n').filter(Boolean);
  fs.unlinkSync(tmp);
}
if (process.env.RAW_OUT) fs.writeFileSync(process.env.RAW_OUT, cand.map(x=>x+'\n').join(''));
if (process.env.FILT_OUT) fs.writeFileSync(process.env.FILT_OUT, lines.map(x=>x+'\n').join(''));
if (process.env.REPORT_FORMAT === 'tsv') {
  const label = process.env.LABEL || '';
  console.log('LABEL\t'+label+'\tELEMS\t'+n+'\tRAW\t'+cand.length+
              '\tFILTERED\t'+lines.length+'\tSPLIT\t'+(split === null ? '-' : split));
  process.exit(lines.length || split !== null ? 1 : 0);
}
const diffs = lines.map(l=>{const f=l.split('\t');return '['+f[0]+' '+f[1]+'] '+f[2]+': "'+f[3]+'" vs "'+f[4]+'"';});
if (split !== null) diffs.unshift(split);
// Say which comparison produced the count. Without the filter every
// equivalent spelling counts, so the total is inflated and is not the number
// run.sh reports: label it loudly rather than let it pass for the real one.
const how = CANON ? ' (canonical compare)'
  : ' (RAW COMPARE, no CANON_FILTER: equivalent spellings counted, total inflated)';
console.log('visual elements: '+n+', computed props/elem: ~'+Object.keys(a[0]||{}).length+how);
const verdict = self
  ? { same: 'RESULT: SELF-STABLE (the page renders the same twice)',
      differ: 'RESULT: NOT SELF-STABLE, '+diffs.length+' difference(s) between two renders of it' }
  : { same: 'RESULT: IDENTICAL computed styles (inlining preserves the render)',
      differ: 'RESULT: '+diffs.length+' difference(s)'+(CANON?'':' [RAW, INFLATED]') };
if (!diffs.length) console.log(verdict.same);
// The whole list, not a sample: it is the artifact you diff between runs.
else { console.log(verdict.differ+':'); diffs.forEach(d=>console.log('  '+d)); }
