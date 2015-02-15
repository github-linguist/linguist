var net = require("net");
var sys = require("sys");
var EventEmitter = require("events").EventEmitter;

/*******************************************************************************
 * ChatServer
 *
 * Manages connections, users, and chat messages.
 ******************************************************************************/

function ChatServer() {
  this.chatters = {};
  this.server   = net.createServer(this.handleConnection.bind(this));
  this.server.listen(1212, "localhost");
}

ChatServer.prototype.isNicknameLegal = function(nickname) {
  // A nickname may contain letters or numbers only,
  // and may only be used once.
  if(nickname.replace(/[A-Za-z0-9]*/, '') != "") {
    return false
  }
  for(used_nick in this.chatters) {
    if(used_nick == nickname) {
      return false;
    }
  }
  return true;
};

ChatServer.prototype.handleConnection = function(connection) {
  console.log("Incoming connection from " + connection.remoteAddress);
  connection.setEncoding("utf8");

  var chatter = new Chatter(connection, this);
  chatter.on("chat", this.handleChat.bind(this));
  chatter.on("join", this.handleJoin.bind(this));
  chatter.on("leave", this.handleLeave.bind(this));
};

ChatServer.prototype.handleChat = function(chatter, message) {
  this.sendToEveryChatterExcept(chatter, chatter.nickname + ": " + message);
};

ChatServer.prototype.handleJoin = function(chatter) {
  console.log(chatter.nickname + " has joined the chat.");
  this.sendToEveryChatter(chatter.nickname + " has joined the chat.");
  this.addChatter(chatter);
};

ChatServer.prototype.handleLeave = function(chatter) {
  console.log(chatter.nickname + " has left the chat.");
  this.removeChatter(chatter);
  this.sendToEveryChatter(chatter.nickname + " has left the chat.");
};

ChatServer.prototype.addChatter = function(chatter) {
  this.chatters[chatter.nickname] = chatter;
};

ChatServer.prototype.removeChatter = function(chatter) {
  delete this.chatters[chatter.nickname];
};

ChatServer.prototype.sendToEveryChatter = function(data) {
  for(nickname in this.chatters) {
    this.chatters[nickname].send(data);
  }
};

ChatServer.prototype.sendToEveryChatterExcept = function(chatter, data) {
  for(nickname in this.chatters) {
    if(nickname != chatter.nickname) {
      this.chatters[nickname].send(data);
    }
  }
};

/*******************************************************************************
 * Chatter
 *
 * Represents a single user/connection in the chat server.
 ******************************************************************************/

function Chatter(socket, server) {
  EventEmitter.call(this);

  this.socket     = socket;
  this.server     = server;
  this.nickname   = "";
  this.lineBuffer = new SocketLineBuffer(socket);

  this.lineBuffer.on("line", this.handleNickname.bind(this));
  this.socket.on("close", this.handleDisconnect.bind(this));

  this.send("Welcome! What is your nickname?");
};

sys.inherits(Chatter, EventEmitter);

Chatter.prototype.handleNickname = function(nickname) {
  if(server.isNicknameLegal(nickname)) {
    this.nickname = nickname;
    this.lineBuffer.removeAllListeners("line");
    this.lineBuffer.on("line", this.handleChat.bind(this));
    this.send("Welcome to the chat, " + nickname + "!");
    this.emit("join", this);
  } else {
    this.send("Sorry, but that nickname is not legal or is already in use!");
    this.send("What is your nickname?");
  }
};

Chatter.prototype.handleChat = function(line) {
  this.emit("chat", this, line);
};

Chatter.prototype.handleDisconnect = function() {
  this.emit("leave", this);
};

Chatter.prototype.send = function(data) {
  this.socket.write(data + "\r\n");
};

/*******************************************************************************
 * SocketLineBuffer
 *
 * Listens for and buffers incoming data on a socket and emits a 'line' event
 * whenever a complete line is detected.
 ******************************************************************************/

function SocketLineBuffer(socket) {
  EventEmitter.call(this);

  this.socket = socket;
  this.buffer = "";

  this.socket.on("data", this.handleData.bind(this));
};

sys.inherits(SocketLineBuffer, EventEmitter);

SocketLineBuffer.prototype.handleData = function(data) {
  for(var i = 0; i < data.length; i++) {
    var char = data.charAt(i);
    this.buffer += char;
    if(char == "\n") {
      this.buffer = this.buffer.replace("\r\n", "");
      this.buffer = this.buffer.replace("\n", "");
      this.emit("line", this.buffer);
      this.buffer = "";
    }
  }
};

// Start the server!
server = new ChatServer();
