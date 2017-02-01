
exports.connectImpl = function (url) {
  return function() {
    const s = require('socket.io-client');
    return s(url);
  };
}

exports.emitImpl = function(socket, eventName, data) {
  return function() {
    socket.emit(eventName, data);
  };
}

exports.onImpl = function(socket, eventName, callback) {
  return function() {
    socket.on(eventName, function(data) {
      console.log("onImpl", data);
      console.log("onImpl type ", typeof data);
      callback(data)();
    });
  };
}
