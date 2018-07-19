net = require("net")
sys = require("sys")
EventEmitter = require("events").EventEmitter

isNicknameLegal = (nickname) ->
  return false unless nickname.replace(/[A-Za-z0-9]*/, "") is ""
  for used_nick of @chatters
    return false if used_nick is nickname
  true

class ChatServer
  constructor: ->
    @chatters = {}
    @server = net.createServer @handleConnection
    @server.listen 1212, "localhost"

  handleConnection: (connection) =>
    console.log "Incoming connection from " + connection.remoteAddress
    connection.setEncoding "utf8"
    chatter = new Chatter(connection, this)
    chatter.on "chat", @handleChat
    chatter.on "join", @handleJoin
    chatter.on "leave", @handleLeave

  handleChat: (chatter, message) =>
    @sendToEveryChatterExcept chatter, chatter.nickname + ": " + message

  handleJoin: (chatter) =>
    console.log chatter.nickname + " has joined the chat."
    @sendToEveryChatter chatter.nickname + " has joined the chat."
    @addChatter chatter

  handleLeave: (chatter) =>
    console.log chatter.nickname + " has left the chat."
    @removeChatter chatter
    @sendToEveryChatter chatter.nickname + " has left the chat."

  addChatter: (chatter) =>
    @chatters[chatter.nickname] = chatter

  removeChatter: (chatter) =>
    delete @chatters[chatter.nickname]

  sendToEveryChatter: (data) =>
    for nickname of @chatters
      @chatters[nickname].send data

  sendToEveryChatterExcept: (chatter, data) =>
    for nickname of @chatters
      @chatters[nickname].send data  unless nickname is chatter.nickname


class Chatter extends EventEmitter
  constructor: (socket, server) ->
    EventEmitter.call this
    @socket = socket
    @server = server
    @nickname = ""
    @lineBuffer = new SocketLineBuffer(socket)
    @lineBuffer.on "line", @handleNickname
    @socket.on "close", @handleDisconnect
    @send "Welcome! What is your nickname?"

  handleNickname: (nickname) =>
    if isNicknameLegal(nickname)
      @nickname = nickname
      @lineBuffer.removeAllListeners "line"
      @lineBuffer.on "line", @handleChat
      @send "Welcome to the chat, " + nickname + "!"
      @emit "join", this
    else
      @send "Sorry, but that nickname is not legal or is already in use!"
      @send "What is your nickname?"

  handleChat: (line) =>
    @emit "chat", this, line

  handleDisconnect: =>
    @emit "leave", this

  send: (data) =>
    @socket.write data + "\r\n"


class SocketLineBuffer extends EventEmitter
  constructor: (socket) ->
    EventEmitter.call this
    @socket = socket
    @buffer = ""
    @socket.on "data", @handleData

  handleData: (data) =>
    console.log "Handling data", data
    i = 0

    while i < data.length
      char = data.charAt(i)
      @buffer += char
      if char is "\n"
        @buffer = @buffer.replace("\r\n", "")
        @buffer = @buffer.replace("\n", "")
        @emit "line", @buffer
        console.log "incoming line: #{@buffer}"
        @buffer = ""
      i++

server = new ChatServer()
