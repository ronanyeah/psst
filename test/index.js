const puppeteer = require("puppeteer");
const assert = require("assert");

const getText = async el =>
  (await (await el).getProperty("innerText")).jsonValue();

(async () => {
  // sandbox issues: https://github.com/GoogleChrome/puppeteer/issues/290#issuecomment-322851507
  const browser = await puppeteer.launch({
    args: ["--no-sandbox", "--disable-setuid-sandbox"]
  });
  const a = await browser.newPage();

  await a.goto("http://localhost:8080/");

  await a.waitFor("#start-circle");

  await a.click("#start-circle");

  const chatLink = await getText(a.waitForSelector("#chat-link"));

  const b = await browser.newPage();
  await b.goto(chatLink);

  await b.waitFor("#messages");

  await a.waitFor("#messages");

  const testText = "How do you capture a very dangerous animal?";

  await a.type("#message-input", testText, { delay: 10 });

  await b.waitFor("#typing");

  await a.click("#send-message");

  const txt = await getText(b.waitForSelector("#message-1"));

  assert(txt === testText);

  await b.close();

  await a.type("#message-input", "x");

  await a.click("#send-message");

  await a.waitFor("#conn-lost");

  return browser.close();
})().catch(err => {
  console.error(err);
  return process.exit(1);
});
