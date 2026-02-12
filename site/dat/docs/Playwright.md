# Playwright Executor
Use this executor to run tests written in TypeScript based on Playwright.

Taurus can repeat test suite executions in a loop until the desired number of `iterations` is complete (for each concurrent user),
or until the hold-for time is exceeded. 
To specifiy the number of concurrent users (also known as workers in Playwright), define the `concurrency` option.

*NOTE*: As Playwright does not support infinite iterations, if `hold-for` is used without `iterations`, Taurus will
set playwright's repeat-each to _1000_.

To pass the base URL to your test as an environment variable, define the `BASE_URL` option in the `settings` section. 

To override the browser settings from your Playwright test configuration, define the `browser` option.
For example, enter one of these values: `chromium`, `firefox`, `webkit`.

To select a single test from the suite to be executed, define the `test` field.

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

Your Playwright project must consist of at least these files:
* package.json - Lists the dependencies of the project, these are then installed automatically when executing it using the Taurus Playwright executor
* playwright.config.ts - The Playwright configuration file
* example.spec.ts - The actual file or files with tests, see example below

Example package.json:
https://github.com/Blazemeter/taurus/blob/master/examples/playwright/package.json

Example playwright.config.ts, the Playwright configuration file:
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
    /* Base URL to use in test actions like `await page.goto('/')`. */
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
    }
  ],
});
```

Example example.spec.ts, a basic Playwright test for https://blazedemo.com/:
```javascript
import { test, expect } from '@playwright/test';

test('has title', async ({ page }) => {
  await page.goto("/");

  // Expect a title "to contain" a substring.
  await expect(page).toHaveTitle(/BlazeDemo/);
});

test('reserve flight', async ({ page }) => {
  await page.goto('/');

  await page.getByRole('button', { name: 'Find Flights' }).click();

  await expect(page).toHaveTitle(/reserve/);

  await page.getByRole('button', { name: 'Choose This Flight' }).nth(1).click();

  await expect(page).toHaveTitle(/Purchase/);

});

```

You can find this complete Playwright-based test suite and Taurus config to run it with
in the [examples/playwright](https://github.com/Blazemeter/taurus/tree/master/examples/playwright)
folder of Taurus's repo.

