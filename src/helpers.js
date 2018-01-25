// ArrayBuffer -> String
function arrayBufferToString(buffer) {
  return new Uint8Array(buffer).reduce(function(acc, val) {
    return acc + String.fromCharCode(val);
  }, "");
}

// String -> ArrayBuffer
function stringToArrayBuffer(string) {
  return new Uint8Array(
    string.split("").map(function(x) {
      return x.charCodeAt(0);
    })
  );
}

module.exports = {
  arrayBufferToString: arrayBufferToString,
  stringToArrayBuffer: stringToArrayBuffer
};
