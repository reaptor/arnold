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
  await p.click(".source-switch button:nth-child(2)");
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
  async review() {
    const p = await page("?devops&lsp");
    await pullRequest(p);
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
    await p.click(".pr-actions .vote-button .split-more");
    await p.waitForTimeout(300);
    await p.screenshot({ path: `${out}/vote.png`, clip: { x: 760, y: 0, width: 680, height: 260 } });
    await p.keyboard.press("Escape");
    await p.mouse.click(700, 500);
    await p.click(".pr-actions .complete-button .split-main");
    await p.waitForTimeout(500);
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
