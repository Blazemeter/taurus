// Loads each page in a fresh browser context and lists cookies (by domain) and embedded player frames.
const { chromium } = require('playwright');
const UA = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 14_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0 Safari/537.36';
(async () => {
  const b = await chromium.launch();
  for (const url of process.argv.slice(2)) {
    const ctx = await b.newContext({ userAgent: UA }); const p = await ctx.newPage();
    const frames = {};
    p.on('response', r => { const m = r.url().match(/^https:\/\/www\.(youtube(-nocookie)?\.com)\/embed\//); if (m) frames[m[1] + ' ' + r.status()] = (frames[m[1] + ' ' + r.status()] || 0) + 1; });
    await p.goto(url, { waitUntil: 'networkidle', timeout: 90000 }); await p.waitForTimeout(3000);
    const cookies = {};
    (await ctx.cookies()).forEach(c => cookies[c.domain] = (cookies[c.domain] || []).concat(c.name));
    console.log(url.replace('http://host.docker.internal:', ':'));
    console.log('   cookies:', JSON.stringify(cookies), '\n   player frames:', JSON.stringify(frames));
    await ctx.close();
  }
  await b.close();
})();
