
exports.connectImpl = function (url) {
  return function() {
    return require('socket.io-client')(url);
  };
}

exports.emitImpl = function(socket, eventName, data) {
  return function() {
    socket.emit(eventName, JSON.stringify(data));
  };
}

exports.onImpl = function(socket, eventName, callback) {
  return function() {
    socket.on(eventName, function(data) {
      callback(data)();
    });
  };
}
