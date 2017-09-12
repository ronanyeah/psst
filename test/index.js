const puppeteer = require('puppeteer')

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

  return browser.close()
})()
.catch(console.error)
