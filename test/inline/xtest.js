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
function computed(path){
  let h = fs.readFileSync(path,'utf8');
  h = h.includes('</body>') ? h.replace('</body>', EXTRACTOR+'</body>') : h+EXTRACTOR;
  const tmp = '/tmp/xt_'+path.replace(/\W/g,'_')+'.html';
  fs.writeFileSync(tmp,h);
  const dom = execSync(`"${CHROME}" --headless --disable-gpu --no-sandbox --virtual-time-budget=2000 --dump-dom "file://${tmp}"`,{encoding:'utf8',maxBuffer:1e8});
  const m = dom.match(/data-xtest="([^"]*)"/);
  if(!m) throw new Error('no data-xtest for '+path);
  return JSON.parse(Buffer.from(m[1],'base64').toString('utf8'));
}
const a = computed(process.argv[2]), b = computed(process.argv[3]);
const n = Math.min(a.length,b.length);
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
}
const diffs = lines.map(l=>{const f=l.split('\t');return '['+f[0]+' '+f[1]+'] '+f[2]+': "'+f[3]+'" vs "'+f[4]+'"';});
if (a.length !== b.length) diffs.unshift('element count '+a.length+' vs '+b.length);
console.log('visual elements: '+n+', computed props/elem: ~'+Object.keys(a[0]||{}).length+(CANON?' (canonical compare)':''));
if (!diffs.length) console.log('RESULT: IDENTICAL computed styles (inlining preserves the render)');
else { console.log('RESULT: '+diffs.length+' difference(s):'); diffs.slice(0,40).forEach(d=>console.log('  '+d)); }
