const puppeteer = require('puppeteer')
const assert = require('assert')

;(async () => {
  // sandbox issues: https://github.com/GoogleChrome/puppeteer/issues/290#issuecomment-322851507
  const browser = await puppeteer.launch({ args: ['--no-sandbox'] })
  const a = await browser.newPage()

  await a.goto('http://localhost:8080/')
  await a.waitFor('.start-circle')

  await a.click('.start-circle')

  await a.waitFor('.room-link')

  const link = await a.$eval('.room-link', x => x.innerText)

  const b = await browser.newPage()
  await b.goto(link)

  await b.waitFor('#messages')
  await a.waitFor('#messages')

  await a.focus('.message-input')
  await a.type('ronan ☘️')

  await b.waitFor('.typing')

  await a.click('.send-message')

  await b.waitFor('.message')

  assert(await b.$eval('.message', x => x.innerText === 'ronan ☘️'))

  await b.close()

  await a.focus('.message-input')
  await a.type('x')
  await a.click('.send-message')

  await a.waitFor('.conn-lost')

  return browser.close()
})()
.catch(err => {
  console.error(err)
  process.exit(1)
})
