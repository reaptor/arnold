// Takes the README's screenshots (docs/screenshots/) from the browser preview
// and its stand-in backend. Start `npm run dev` first; needs Google Chrome.
//
//   npm run screenshots            all of them
//   npm run screenshots -- review  only review.png

import { chromium } from "playwright-core";

const out = new URL("../docs/screenshots/", import.meta.url).pathname;
const only = process.argv[2];
const base = process.env.ARNOLD_URL ?? "http://localhost:1420/";
const browser = await chromium.launch({ channel: "chrome" });

/** A fresh window (nothing remembered) on the preview, settled. */
async function page(query) {
  const context = await browser.newContext({
    viewport: { width: 1440, height: 860 },
    deviceScaleFactor: 2,
    colorScheme: "dark",
  });
  const p = await context.newPage();
  await p.goto(base + query);
  await p.waitForTimeout(2500);
  return p;
}

/** Opens the stand-in Azure DevOps pull request !1287. */
async function pullRequest(p) {
  await openPullRequestList(p);
  await p.click(".pr-panel li:has-text('!1287')");
  await p.waitForTimeout(3000);
}

async function openPullRequestList(p) {
  await p.click(".source-switch button:has-text('Pull request')");
  await p.waitForTimeout(1500);
}

/** `box` with `margin` around it. */
const around = (box, margin) => ({
  x: box.x - margin,
  y: box.y - margin,
  width: box.width + 2 * margin,
  height: box.height + 2 * margin,
});

const shots = {
  async commit() {
    const p = await page("?uncommitted&devops=empty&lsp");
    await p.click(".dock li.file:has-text('Orders.fs')");
    // Until the F# projects have loaded.
    await p.waitForTimeout(4000);
    await p.locator(".commit-message").fill("Add the loyalty total for returning customers");
    await p.waitForTimeout(300);
    await p.screenshot({ path: `${out}/commit.png` });
  },
  async fantomas() {
    const p = await page("?uncommitted&unformatted");
    await p.locator(".commit-message").fill("Add the loyalty total for returning customers");
    await p.click(".commit-bar button[type=submit]");
    await p.waitForTimeout(1500);
    await p.screenshot({ path: `${out}/fantomas.png`, clip: around(await p.locator(".unformatted-dialog").boundingBox(), 40) });
  },
  async "create-pr"() {
    const p = await page("?devops=empty&lsp");
    await p.click(".toolbar .create-pr-button");
    await p.waitForTimeout(1500);
    await p.locator(".create-dialog input[type=text]").nth(1).fill("1234");
    await p.waitForTimeout(1200);
    await p.screenshot({ path: `${out}/create-pr.png`, clip: around(await p.locator(".create-dialog").boundingBox(), 40) });
  },
  async fsharp() {
    const p = await page("?lsp");
    await p.click("li.file:has-text('Orders.fs')");
    await p.waitForTimeout(2000);
    const error = p.locator(".cm-lintRange-error").first();
    await error.scrollIntoViewIfNeeded();
    await error.hover();
    await p.waitForTimeout(800);
    const box = await error.boundingBox();
    // The new side, where the language server works.
    const editor = await p.locator(".cm-editor:has(.cm-lintRange-error)").first().boundingBox();
    await p.screenshot({
      path: `${out}/fsharp.png`,
      clip: { x: editor.x, y: Math.max(40, box.y - 260), width: editor.width, height: 420 },
    });
  },
  async review() {
    const p = await page("?devops&lsp");
    await pullRequest(p);
    // Until the F# projects have loaded.
    await p.waitForTimeout(3500);
    await p.screenshot({ path: `${out}/review.png` });
  },
  async "pull-requests"() {
    const p = await page("?devops&lsp");
    await openPullRequestList(p);
    await p.screenshot({ path: `${out}/pull-requests.png`, clip: { x: 330, y: 52, width: 780, height: 590 } });
  },
  async comment() {
    const p = await page("?devops&lsp");
    await pullRequest(p);
    await p.locator(".cm-line:has-text('WHERE o.CustomerId')").last().click({ button: "right" });
    await p.waitForTimeout(300);
    await p.click(".menu li:has-text('Add comment')");
    await p.waitForTimeout(500);
    await p.locator(".composer-text").fill("Could `N'Open'` be a constant? Other procedures use `@OpenStatus`. :thinking:");
    await p.waitForTimeout(300);
    const section = await p.locator(".file-section").first().boundingBox();
    await p.screenshot({
      path: `${out}/comment.png`,
      clip: { x: section.x, y: section.y, width: section.width, height: Math.min(section.height, 800 - section.y) },
    });
  },
  async complete() {
    const p = await page("?devops&lsp");
    await pullRequest(p);
    await p.click(".pr-actions .pr-button .split-more");
    await p.waitForTimeout(300);
    await p.screenshot({ path: `${out}/vote.png`, clip: { x: 760, y: 0, width: 680, height: 260 } });
    await p.keyboard.press("Escape");
    await p.mouse.click(700, 500);
    await p.click(".pr-actions .pr-button .split-more");
    await p.click(".action-menu li:has-text('Complete')");
    // Until the branch policies are read.
    await p.waitForTimeout(1200);
    await p.screenshot({ path: `${out}/complete.png`, clip: around(await p.locator(".complete-dialog").boundingBox(), 40) });
  },
  async commits() {
    const p = await page("?lsp");
    await p.click(".commit-picker .picker-button");
    await p.waitForTimeout(500);
    await p.screenshot({ path: `${out}/commits.png`, clip: { x: 0, y: 0, width: 900, height: 330 } });
  },
  async language() {
    const p = await page("?lsp");
    // Part-way through loading the projects.
    await p.waitForTimeout(1500);
    await p.click(".language-button");
    await p.waitForTimeout(400);
    const box = await p.locator(".language-popup").boundingBox();
    await p.screenshot({
      path: `${out}/language.png`,
      clip: { x: box.x - 16, y: box.y - 10, width: box.width + 32, height: 860 - box.y + 10 },
    });
  },
};

let failed = false;
for (const [name, shoot] of Object.entries(shots)) {
  if (only && only !== name) continue;
  try {
    await shoot();
    console.log("took", name);
  } catch (error) {
    failed = true;
    console.error("failed", name, error.message.split("\n")[0]);
  }
}
await browser.close();
process.exit(failed ? 1 : 0);
