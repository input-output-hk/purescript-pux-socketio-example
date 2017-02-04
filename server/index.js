// Setup basic express server
var express = require('express');
var app = express();
var server = require('http').createServer(app);
var io = require('socket.io')(server);
var port = process.env.PORT || 3000;

server.listen(port, function () {
  console.log('Server listening at port %d', port);
});

// Routing
app.use(express.static(__dirname + '/public'));

// Chatroom

var numUsers = 0;

io.on('connection', function (socket) {
  var addedUser = false;

  // when the client emits 'new message', this listens and executes
  socket.on('new message', function (msg) {
    // we tell the client to execute 'new message'
    socket.broadcast.emit('new message', msg);
  });

// when the client emits 'add user', this listens and executes
  socket.on('add user', function (username) {
    if (addedUser) return;
    // we store the username in the socket session for this client
    socket.username = username;
    ++numUsers;
    addedUser = true;
    var loginData = {
      numUsers: numUsers
    }
    var joinedData = {
      username: socket.username,
      numUsers: numUsers
    }
    socket.emit('login', loginData);
    // echo globally (all clients) that a person has connected
    socket.broadcast.emit('user joined', joinedData);
  });

  socket.on('custom data', function (data) {
    var customDataMsg = {
      from: data.username || 'unknown',
      payload: data
    }
    socket.broadcast.emit('custom data', customDataMsg);
  });

});
