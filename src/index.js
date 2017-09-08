if (navigator.serviceWorker) {
  navigator.serviceWorker.register('/sw.js')
  .then(console.log)
  .catch(console.error)
}

var crypto = window.crypto.subtle

var Elm = require('./Main.elm')

// String -> ArrayBuffer
function stringToArrayBuffer (string) {
  return new Uint8Array(
    string.split('')
    .map(function (x) { return x.charCodeAt(0) })
  )
}

// ArrayBuffer -> String
function arrayBufferToText (buffer) {
  return new Uint8Array(buffer).reduce(
    function (acc, val) {
      return acc + String.fromCharCode(val)
    },
    ''
  )
}

crypto.generateKey(
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

    var roomId = new URLSearchParams(window.location.search).get('room-id')

    var app = Elm.Main.fullscreen([jwk, roomId, WS_API])

    app.ports.decrypt.subscribe(function (encryptedText) {
      return crypto.decrypt(
        { name: 'RSA-OAEP' },
        keys.privateKey,
        stringToArrayBuffer(encryptedText)
      )
      .then(function (buffer) {
        return decodeURI(arrayBufferToText(buffer))
      })
      .then(function (plaintext) {
        return app.ports.cbDecrypt.send(plaintext)
      })
      .catch(console.error)
    })

    app.ports.encrypt.subscribe(function ([plaintext, pk]) {
      return crypto.importKey(
        'jwk',
        JSON.parse(pk),
        { name: 'RSA-OAEP', hash: 'SHA-256' },
        true,
        ['encrypt']
      )
      .then(function (publicKey) {
        return crypto.encrypt(
          { name: 'RSA-OAEP' },
          publicKey,
          stringToArrayBuffer(encodeURI(plaintext))
        )
      })
      .then(arrayBufferToText)
      .then(function (encryptedText) {
        return app.ports.cbEncrypt.send(encryptedText)
      })
      .catch(console.error)
    })

  })
})
.catch(console.error)
