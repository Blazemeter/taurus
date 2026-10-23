// Compares what the usage-stats chart plots on legacy (Highcharts) and dev (Chart.js) for Day/Week/Month,
// and saves screenshots. Usage: node chart-compare.js <outdir> <legacy-url> <dev-url>
const fs = require('fs');
const { chromium } = require('playwright');
const [out, legacyUrl, devUrl] = process.argv.slice(2);
fs.mkdirSync(out, { recursive: true });
const UA = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 14_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0 Safari/537.36';

// normalized {title, labels, series: {name: [values]}} from whichever library rendered the chart
const extract = () => {
  if (window.Highcharts && Highcharts.charts.filter(Boolean).length) {
    const c = Highcharts.charts.filter(Boolean).pop();
    const s = {};
    c.series.forEach(x => s[x.name + ':' + x.type] = x.points.map(p => p.y));
    return { lib: 'Highcharts ' + Highcharts.version, title: c.title.textStr, labels: c.xAxis[0].categories, series: s };
  }
  const canvas = document.querySelector('#out canvas');
  const c = canvas && window.Chart && Chart.getChart(canvas);
  if (!c) return { lib: 'none', noData: !!document.querySelector('.no-data') };
  const s = {};
  c.data.datasets.forEach(x => s[x.label + ':' + ({ bar: 'column', line: 'spline' }[x.type] || x.type)] = x.data);
  return { lib: 'Chart.js ' + Chart.version, title: c.options.plugins.title.text, labels: c.data.labels, series: s };
};

(async () => {
  const browser = await chromium.launch();
  const res = {};
  for (const [tag, url] of [['legacy', legacyUrl], ['dev', devUrl]]) {
    const page = await browser.newPage({ userAgent: UA, viewport: { width: 1280, height: 700 } });
    const errors = [];
    page.on('console', m => m.type() === 'error' && errors.push(m.text()));
    page.on('pageerror', e => errors.push(e.message));
    await page.goto(url, { waitUntil: 'networkidle' });
    res[tag] = { errors };
    for (const btn of ['day', 'week', 'month']) {
      await page.click(`button[name=${btn}]`);
      await page.waitForTimeout(1500);   // let chart animations finish
      res[tag][btn] = await page.evaluate(extract);
      await page.screenshot({ path: `${out}/${tag}-${btn}.png` });
    }
    await page.close();
  }
  await browser.close();
  for (const btn of ['day', 'week', 'month']) {
    const a = res.legacy[btn], b = res.dev[btn];
    const same = JSON.stringify([a.title, a.labels, a.series]) === JSON.stringify([b.title, b.labels, b.series]);
    console.log(`${btn.padEnd(6)} ${same ? 'SAME DATA' : 'DIFFERENT'} | ${a.lib} -> ${b.lib} | "${b.title}" ${(b.labels || []).length} labels, series: ${Object.keys(b.series || {}).join(', ')}`);
    if (!same) console.log('   legacy:', JSON.stringify(a).slice(0, 300), '\n   dev:   ', JSON.stringify(b).slice(0, 300));
  }
  for (const tag of ['legacy', 'dev']) res[tag].errors.forEach(e => console.log(`${tag} console error: ${e.slice(0, 200)}`));
})();
