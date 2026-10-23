# Site regression checks

Compares the local **legacy** stack (`site/legacy`, frozen production release 1.17.1, http://localhost:8080)
with the **dev** stack (`site/dev`, PHP 8.5 with the local `pwe` clone, http://localhost:8081), and dev with
itself before/after a change. Used to verify the PHP 8 / PWE / CDN work without changing what visitors see.

## Prerequisites
- both stacks running: `docker compose up -d` in `site/legacy` and in `site/dev` (`site/legacy/fetch-learn.sh` once before)
- Docker image `mcr.microsoft.com/playwright:v1.55.1-noble` for the browser checks (pulled on first use, ~3.5 GB),
  Node packages from `package.json` are installed into `.deps/` on first use

## Run everything
```
./run-all.sh                          # first run
./run-all.sh out/crawl-<timestamp>    # next runs: also compares dev pages with the previous run
```
Typical use: run once before a change, make the change (rebuild dev if needed), run again with the crawl
directory of the first run. Results are written to `out/` (screenshots, chart screenshots, crawled HTML).

## Checks
| Script | What it checks |
|---|---|
| `crawl.sh` | every page (`urls-pages.txt`) on legacy and dev: status codes and HTML |
| `snapshot-diff.sh` | dev HTML of two crawls, before/after a change (`-v` prints the changed lines) |
| `status-compare.sh` | status codes and redirect targets of page variants and edge cases (`urls-variants.txt`) |
| `template-check.sh` | compiles every PWE and site Smarty template in the dev container |
| `browser.sh browser-check` | headless Chromium: pixel comparison of screenshots, console errors, failed requests, YAML/JSON switch, loaded jQuery/Bootstrap |
| `browser.sh chart-compare` | usage statistics page plots the same data on legacy (Highcharts) and dev (Chart.js) |
| `browser.sh tracking-check` / `cookie-check` | tracking requests and cookies set on page load |
| `seed-db.sh` | loads identical test data into both stats databases (`seed-stats.sql`) |
| `log-check.sh <since>` | PHP errors, warnings and deprecations logged by dev |
| `make-urls.sh` | regenerates the URL lists from `site/dat`, run after adding or removing docs pages |

HTML is compared after normalizing random ids of the YAML/JSON switch, `&#039;` escaping and trailing whitespace.

## Expected differences between legacy and dev
These are intended changes, anything else is a regression:
- **HTML of all pages using `taurus.tpl`**: CDN tags (jQuery 3.7.1, Bootstrap 3.4.1, Font Awesome via jsDelivr),
  removed IE8 scripts, removed and disabled trackers
- **`/cfg.php`**: 200 on legacy, 403 on dev (internal files are blocked)
- **home page `/`**: ~245 pixels in the YouTube player (privacy-enhanced `youtube-nocookie.com` player icon)
- **`/install/Installation/`**: taller on dev, the Windows tools image is displayed (404 on legacy)
- **error pages**: ~1000 pixels, the broken PWE logo is removed from the footer
- **usage statistics**: rendered by Chart.js instead of Highcharts, same data
- **tracking and cookies**: none on dev

## Notes
- calls to `/updates/` add rows to the stats databases, `run-all.sh` reseeds them before comparing charts
- the dev stack serves the `pwe` clone live: switching branches in `pwe` while checks run changes the results
