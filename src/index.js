if (navigator.serviceWorker) {
  navigator.serviceWorker.register('/sw.js')
  .then(console.log)
  .catch(alert)
}

var crypto = window.crypto.subtle

var Elm = require('./Main.elm')

// String -> ArrayBuffer
const stringToArrayBuffer = string =>
  new Uint8Array(
    string.split('')
    .map( x => x.charCodeAt(0) )
  )

// ArrayBuffer -> String
const arrayBufferToText = buffer =>
  new Uint8Array(buffer).reduce(
    (acc, val) =>
      acc + String.fromCharCode(val),
    ''
  )

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
.then(({ publicKey, privateKey }) => {
  crypto.exportKey('jwk', publicKey)
  .then( jwk => {

    var roomId = new URLSearchParams(window.location.search).get('room-id')

    var app = Elm.Main.fullscreen([jwk, roomId])

    app.ports.decrypt.subscribe(function (encryptedText) {
      crypto.decrypt(
        { name: 'RSA-OAEP' },
        privateKey,
        stringToArrayBuffer(encryptedText)
      )
      .then(
        buffer =>
          decodeURI(arrayBufferToText(buffer))
      )
      .then(
        encryptedText =>
          app.ports.cbDecrypt.send(plaintext)
      )
      .catch(console.error)
    })

    app.ports.encrypt.subscribe(function (plaintext) {
      crypto.encrypt(
        { name: 'RSA-OAEP' },
        publicKey,
        stringToArrayBuffer(encodeURI(plaintext))
      )
      .then(arrayBufferToText)
      .then(
        encryptedText =>
          app.ports.cbEncrypt.send(encryptedText)
      )
      .catch(console.error)
    })

  })
  .catch(console.error)
})
.catch(console.error)
