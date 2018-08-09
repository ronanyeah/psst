const WebSocket = require("ws");

exports.transmit = function(message) {
  return function(sender) {
    return function(recipient) {
      return function() {
        return recipient.send(message, function(error) {
          if (error) {
            sender.send(JSON.stringify("DEAD"));
          }
        });
      };
    };
  };
};

exports.sendMessage = function(message) {
  return function(socket) {
    return function() {
      return socket.send(message);
    };
  };
};

exports.registerWsServer = function(server) {
  const wsServer = new WebSocket.Server({ server: server });
  return function(callback) {
    return function() {
      wsServer.on("connection", function(ws) {
        ws.on("message", function(message) {
          callback({ message: message, ws: ws })();
        });
      });
    };
  };
};
