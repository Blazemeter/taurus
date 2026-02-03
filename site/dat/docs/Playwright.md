# Playwright Executor
Allows running tests written in TypeScript based on Playwright.

Taurus can loop test suite execution in a loop until desired number of `iterations` will complete (for each concurrent user)
or hold-for time will be exceeded. 
Also number of concurrent users (aka workers in Playwright) can be specified using `concurrency`.

*NOTE*: As Playwright does not support infinite iterations if `hold-for` is used without `iterations` Taurus will
set playwright's repeat-each to _1000_.

You may specify `BASE_URL` in `settings` section to be pass to your test as an environment variable.

It is possible to override the browser settings from your Playwright test configuration by using the `browser` option, e.g.,
you may enter one of these: `chromium`, `firefox`, `webkit`.

A single test from the suite may be selected to be executed using the `test` field.

## Usage
```yaml
execution:
- executor: playwright
  scenario: playright_test
  iterations: 10
  hold-for: 1m
  concurrency: 5

settings:
  env:
    BASE_URL: https://blazedemo.com/

scenarios:
    playwright_test:
        script: example.spec.ts # points to the file with tests
        browser: firefox
        test: has title # can be omitted to run all tests
```

## Playwright project

Your project will need to consist of at least these files:
* package.json - lists the dependencies of the project, these are then installed automatically when executing it using the Taurus Playwright executor
* playwright.config.ts - Playwright configuration file
* actual file or files with tests, see example below

Example of Playwright configuration file:
```javascript
import { defineConfig, devices } from '@playwright/test';

/**
 * See https://playwright.dev/docs/test-configuration.
 */
export default defineConfig({
  testDir: '.',
  /* Run tests in files in parallel */
  fullyParallel: true,
  /* Fail the build on CI if you accidentally left test.only in the source code. */
  forbidOnly: !!process.env.CI,
  /* Retry on CI only */
  retries: process.env.CI ? 2 : 0,
  /* Opt out of parallel tests on CI. */
  workers: process.env.CI ? 1 : undefined,
  /* Reporter to use. See https://playwright.dev/docs/test-reporters */
  reporter: 'json',
  /* Shared settings for all the projects below. See https://playwright.dev/docs/api/class-testoptions. */
  use: {
    /* Base URL to use in actions like `await page.goto('/')`. */
    // baseURL: 'http://127.0.0.1:3000',
    baseURL: process.env.BASE_URL,
    /* Collect trace when retrying the failed test. See https://playwright.dev/docs/trace-viewer */
    trace: 'on-first-retry',
    video: {
      mode: 'on',
      size: { width: 640, height: 480 }
    }
  },

  /* Configure projects for major browsers */
  projects: [
    {
      name: 'chromium',
      use: { ...devices['Desktop Chrome'] },
    },

    {
      name: 'firefox',
      use: { ...devices['Desktop Firefox'] },
    },

    {
      name: 'webkit',
      use: { ...devices['Desktop Safari'] },
    }]
});
```


Basic example of Playwright test:
```javascript
import { test, expect } from '@playwright/test';

test('has title', async ({ page }) => {
    await page.goto("/");   // uses the BASE_URL

    // Expect a title "to contain" a substring.
    await expect(page).toHaveTitle(/BlazeDemo/);
});

```

You can also find an example of the complete Playwright-based test suite and Taurus config to run it with
in [examples/playwright](https://github.com/Blazemeter/taurus/tree/master/examples/playrwright)
folder of Taurus's repo.

