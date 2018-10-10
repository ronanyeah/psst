const Clipboard = require("clipboard");
const { Elm } = require("./Main.elm");
const { arrayBufferToString, stringToArrayBuffer } = require("./helpers.js");

const crypto =
  window.crypto.subtle || window.crypto.webkitSubtle || window.crypto.msSubtle;

const cryptoEnabled =
  crypto &&
  crypto.generateKey &&
  crypto.encrypt &&
  crypto.decrypt &&
  crypto.importKey &&
  crypto.exportKey;

(async () => {
  if (!cryptoEnabled) {
    return Elm.Main.embed(document.body, null);
  }

  new Clipboard("#copy-button");

  const { publicKey, privateKey } = await crypto.generateKey(
    {
      name: "RSA-OAEP",
      modulusLength: 2048,
      publicExponent: new Uint8Array([0x01, 0x00, 0x01]),
      hash: "SHA-256"
    },
    true,
    ["encrypt", "decrypt"]
  );

  const jwk = await crypto.exportKey("jwk", publicKey);

  const flags = {
    origin: window.location.origin,
    maybeChatId: window.location.hash
      ? window.location.hash.substring(1)
      : null,
    shareEnabled: typeof window.navigator.share === "function",
    copyEnabled: Clipboard.isSupported(),
    publicKey: jwk
  };

  // Clear the hash
  window.history.replaceState({}, document.title, "/");

  const ws = new WebSocket(WS_URL);

  ws.onclose = console.warn;

  ws.onerror = console.error;

  await new Promise(resolve => (ws.onopen = resolve));

  const app = Elm.Main.init({ flags });

  ws.onmessage = e => app.ports.wsReceive.send(e.data);

  app.ports.share.subscribe(url =>
    window.navigator.share({
      title: "Psst",
      url
    })
  );

  app.ports.wsSend.subscribe(txt => ws.send(txt));

  app.ports.decrypt.subscribe(encryptedText =>
    crypto
      .decrypt(
        { name: "RSA-OAEP" },
        privateKey,
        stringToArrayBuffer(encryptedText)
      )
      .then(arrayBufferToString)
      .then(decodeURI)
      .then(app.ports.cbDecrypt.send)
  );

  app.ports.loadPublicKey.subscribe(async jwk => {
    const publicKey = await crypto.importKey(
      "jwk",
      jwk,
      { name: "RSA-OAEP", hash: "SHA-256" },
      true,
      ["encrypt"]
    );

    app.ports.cbLoadPublicKey.send(publicKey);
  });

  app.ports.encrypt.subscribe(({ publicKey, plaintext }) =>
    crypto
      .encrypt(
        { name: "RSA-OAEP" },
        publicKey,
        stringToArrayBuffer(encodeURI(plaintext))
      )
      .then(arrayBufferToString)
      .then(app.ports.cbEncrypt.send)
  );

  app.ports.log.subscribe(console.log);
})().catch(console.error);
