// Lists analytics/tracking requests a page makes (script loads and hits), per site.
const { chromium } = require('playwright');
const UA = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 14_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0 Safari/537.36';
(async () => {
  const b = await chromium.launch();
  for (const url of process.argv.slice(2)) {
    const p = await b.newPage({ userAgent: UA }); const seen = {};
    p.on('request', r => {
      const u = r.url(); let m;
      if (/analytics\.js/.test(u)) m = 'UA analytics.js loaded';
      else if ((m = u.match(/[?&]tid=(UA-[\d-]+|G-[A-Z0-9]+|AW-\d+)/))) m = 'hit ' + m[1];
      else if (/googletagmanager\.com\/gtag\/js/.test(u)) m = 'gtag.js loaded (' + (u.match(/id=([^&]+)/) || [])[1] + ')';
      else if (/facebook\.(net|com)/.test(u)) m = 'facebook';
      if (m) seen[m] = (seen[m] || 0) + 1;
    });
    await p.goto(url, { waitUntil: 'networkidle' }); await p.waitForTimeout(2000);
    console.log(url.padEnd(48), JSON.stringify(seen)); await p.close();
  }
  await b.close();
})();
