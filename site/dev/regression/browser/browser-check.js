// Loads pages in headless Chromium on two sites, reports console errors / failed requests,
// exercises the YAML/JSON switch and compares screenshots pixel by pixel.
// Usage: node browser-check.js <outdir> <old-base> <new-base> <path>...
const fs = require('fs');
const path = require('path');
const { chromium } = require('playwright');
const { PNG } = require('pngjs');
const pixelmatch = require('pixelmatch');

const [out, oldBase, newBase, ...paths] = process.argv.slice(2);
fs.mkdirSync(out, { recursive: true });

async function visit(browser, base, p, tag) {
  const page = await browser.newPage({ viewport: { width: 1280, height: 900 } });
  const errors = [], failed = [];
  page.on('console', m => { if (m.type() === 'error') errors.push(m.text()); });
  page.on('pageerror', e => errors.push('pageerror: ' + e.message));
  page.on('requestfailed', r => failed.push(r.url() + ' (' + (r.failure() || {}).errorText + ')'));
  page.on('response', r => { if (r.status() >= 400) failed.push(r.status() + ' ' + r.url()); });
  await page.goto(base + p, { waitUntil: 'networkidle', timeout: 90000 });
  const sw = await page.evaluate(() => {
    const s = document.querySelector('.yaml-json-switch');
    if (!s) return 'n/a';
    const id = s.dataset.id, spans = s.querySelectorAll('span');
    if (spans.length !== 2) return 'broken: ' + spans.length + ' buttons';
    spans[1].click();
    const json = getComputedStyle(document.querySelector('.json.id-' + id)).display !== 'none';
    spans[0].click();
    const yaml = getComputedStyle(document.querySelector('.yaml.id-' + id)).display !== 'none';
    return json && yaml ? 'works' : 'broken: toggle';
  });
  const libs = await page.evaluate(() => (window.jQuery ? "jq " + jQuery.fn.jquery : "no jq") + (window.jQuery && jQuery.fn.modal ? " +bs " + (jQuery.fn.modal.Constructor || {}).VERSION : " no bs"));
  const shot = path.join(out, tag + p.replace(/\//g, '_') + '.png');
  await page.screenshot({ path: shot, fullPage: true });
  await page.close();
  return { errors, failed, sw, shot, libs };
}

(async () => {
  const browser = await chromium.launch();
  for (const p of paths) {
    const o = await visit(browser, oldBase, p, 'old');
    const n = await visit(browser, newBase, p, 'new');
    const a = PNG.sync.read(fs.readFileSync(o.shot)), b = PNG.sync.read(fs.readFileSync(n.shot));
    let pix;
    if (a.width !== b.width || a.height !== b.height) {
      pix = `size differs ${a.width}x${a.height} vs ${b.width}x${b.height}`;
    } else {
      const diff = new PNG({ width: a.width, height: a.height });
      const cnt = pixelmatch(a.data, b.data, diff.data, a.width, a.height, { threshold: 0 });
      if (cnt) fs.writeFileSync(path.join(out, 'diff' + p.replace(/\//g, '_') + '.png'), PNG.sync.write(diff));
      pix = cnt ? `${cnt} pixels differ` : 'identical';
    }
    console.log(`${p.padEnd(28)} screenshots: ${pix} | switch: ${o.sw}/${n.sw} | libs: ${o.libs} -> ${n.libs}`);
    for (const [tag, r] of [['old', o], ['new', n]]) {
      r.errors.forEach(e => console.log(`    ${tag} console: ${e.slice(0, 160)}`));
      r.failed.forEach(f => console.log(`    ${tag} failed:  ${f.slice(0, 160)}`));
    }
  }
  await browser.close();
})();
