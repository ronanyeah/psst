// ArrayBuffer -> String
const arrayBufferToString = buffer =>
  new Uint8Array(buffer).reduce(
    (acc, val) => acc + String.fromCharCode(val),
    ""
  );

// String -> ArrayBuffer
const stringToArrayBuffer = string =>
  new Uint8Array(string.split("").map(x => x.charCodeAt(0)));

export { arrayBufferToString, stringToArrayBuffer };
