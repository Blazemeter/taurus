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
