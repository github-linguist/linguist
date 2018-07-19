require 'gserver'

class ChatServer < GServer
  def initialize *args
    super

    #Keep a list for broadcasting messages
    @chatters = []

    #We'll need this for thread safety
    @mutex = Mutex.new
  end

  #Send message out to everyone but sender
  def broadcast message, sender = nil
    #Need to use \r\n for our Windows friends
    message = message.strip << "\r\n"

    #Mutex for safety - GServer uses threads
    @mutex.synchronize do
      @chatters.each do |chatter|
        begin
          chatter.print message unless chatter == sender
        rescue
          @chatters.delete chatter
        end
      end
    end
  end

  #Handle each connection
  def serve io
    io.print 'Name: '
    name = io.gets

    #They might disconnect
    return if name.nil?

    name.strip!

    broadcast "--+ #{name} has joined +--"

    #Add to our list of connections
    @mutex.synchronize do
      @chatters << io
    end

    #Get and broadcast input until connection returns nil
    loop do
      message = io.gets

      if message
        broadcast "#{name}> #{message}", io
      else
        break
      end
    end

    broadcast "--+ #{name} has left +--"
  end
end

#Start up the server on port 7000
#Accept connections for any IP address
#Allow up to 100 connections
#Send information to stderr
#Turn on informational messages
ChatServer.new(7000, '0.0.0.0', 100, $stderr, true).start.join
