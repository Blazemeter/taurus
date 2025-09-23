# Playwright Executor

Allows running tests written in TypeScript based on Playwright.

Taurus can repeat test suite execution in a loop until the desired number of `iterations` has completed. 
Specify the number of concurrent users (known as workers in Playwright) using the `concurrency` option. 

Optionally, specify the `BASE_URL` in the `settings` section to be passed to your test as an environment variable.

It is possible to override the browser settings from your Playwright test configuration by using the `browser` option, for example,
enter one of these: `chromium`, `firefox`, `webkit`.

To select a single test from the suite  to be executed, use the `test` field.

## Usage
```yaml
execution:
- executor: playwright
  scenario: playright_test
  iterations: 10
  concurrency: 5

settings:
  env:
    BASE_URL: https://blazedemo.com/

scenarios:
    playwright_test:
        script: example.spec.ts # points to the TypeScript file with tests
        browser: firefox
        test: has title # Omit the test name to run all tests. Here, "has title" is the name of the test.
```

## Playwright project

Your project must consist of at least these files:
* package.json - lists the dependencies of the project, these are then installed automatically when executing the test using the Taurus Playwright executor
* playwright.config.ts - Playwright configuration file
* actual file or files with tests, see example below

The following code snippet is an example of a Playwright configuration file:

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


The following code snippet is an example of a basic Playwright test "has title" that tests whether a web page title contains a substring:

```javascript
import { test, expect } from '@playwright/test';

test('has title', async ({ page }) => {
    await page.goto("/");   // uses the BASE_URL

    // Expect a title "to contain" a substring.
    await expect(page).toHaveTitle(/BlazeDemo/);
});

```

You can also find an example of a complete Playwright-based test suite, and the Taurus config to run it with,
in the [examples/playwright](https://github.com/Blazemeter/taurus/tree/master/examples/playrwright)
folder of Taurus's repo.

