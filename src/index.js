// No ES6, because UglifyJS doesn't support it yet.
// https://stackoverflow.com/a/45088328/4224679

if (window.navigator.serviceWorker) {
  window.navigator.serviceWorker.register('/sw.js')
  .then(console.log)
  .catch(console.error)
}

var Clipboard = require('clipboard')
var Elm = require('./Main.elm')
var helpers = require('./helpers.js')
var crypto = window.crypto.subtle || window.crypto.webkitSubtle || window.crypto.msSubtle

var flags = {
  origin: window.location.origin,
  wsUrl: WS_API,
  maybeRoomId: window.location.hash ? window.location.hash.substring(1) : null,
  shareEnabled: typeof window.navigator.share === 'function',
  copyEnabled: Clipboard.isSupported()
}

var cryptoEnabled = crypto && crypto.generateKey && crypto.encrypt && crypto.decrypt && crypto.importKey && crypto.exportKey

;(function () {
  if (!cryptoEnabled) return Elm.Main.fullscreen([null, flags])

  new Clipboard('.copy-button')

  return crypto.generateKey(
    {
      name: 'RSA-OAEP',
      modulusLength: 2048,
      publicExponent: new Uint8Array([0x01, 0x00, 0x01]),
      hash: 'SHA-256'
    },
    true,
    ['encrypt', 'decrypt']
  )
  .then(function (keys) {
    return crypto.exportKey('jwk', keys.publicKey)
    .then(function (jwk) {

      var app = Elm.Main.fullscreen([jwk, flags])

      app.ports.share.subscribe(function (url) {
        return window.navigator.share({
          title: 'Psst',
          url: url
        })
        .catch(alert)
      })

      app.ports.decrypt.subscribe(function (encryptedText) {
        return crypto.decrypt(
          { name: 'RSA-OAEP' },
          keys.privateKey,
          helpers.stringToArrayBuffer(encryptedText)
        )
        .then(helpers.arrayBufferToString)
        .then(decodeURI)
        .then(app.ports.cbDecrypt.send)
        .catch(console.error)
      })

      app.ports.loadPublicKey.subscribe(function (jwk) {
        return crypto.importKey(
          'jwk',
          jwk,
          { name: 'RSA-OAEP', hash: 'SHA-256' },
          true,
          ['encrypt']
        )
        .then(function (publicKey) {

          app.ports.encrypt.subscribe(function (plaintext) {
            return crypto.encrypt(
              { name: 'RSA-OAEP' },
              publicKey,
              helpers.stringToArrayBuffer(encodeURI(plaintext))
            )
            .then(helpers.arrayBufferToString)
            .then(app.ports.cbEncrypt.send)
            .catch(console.error)
          })

          app.ports.cbLoadPublicKey.send(null)
        })
        .catch(console.error)
      })
    })
  })
  .catch(alert)
})()
