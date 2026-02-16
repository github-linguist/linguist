require 'yajl/yajl'

# = Extras
# We're not going to load these automatically, because you might not need them ;)
#
# require 'yajl/http_stream.rb' unless defined?(Yajl::HttpStream)
# require 'yajl/gzip.rb' unless defined?(Yajl::Gzip)
# require 'yajl/deflate.rb' unless defined?(Yajl::Deflate)
# require 'yajl/bzip2.rb' unless defined?(Yajl::Bzip2)

# = Yajl
#
# Ruby bindings to the excellent Yajl (Yet Another JSON Parser) ANSI C library.
module Yajl

  # For compatibility, has the same signature of Yajl::Parser.parse
  def self.load(str_or_io, options={}, read_bufsize=nil, &block)
    Parser.parse(str_or_io, options, read_bufsize, &block)
  end

  # For compatibility, has the same signature of Yajl::Encoder.encode
  def self.dump(obj, *args, &block)
    Encoder.encode(obj, args, &block)
  end

  class Projector
    def initialize(stream, read_bufsize=4096)
      @stream = stream
      @buffer_size = read_bufsize
    end
  end

  class Parser
    # A helper method for parse-and-forget use-cases
    #
    # +io+ is the stream to parse JSON from
    #
    # The +options+ hash allows you to set two parsing options - :allow_comments and :check_utf8
    #
    # :allow_comments accepts a boolean will enable/disable checks for in-line comments in the JSON stream
    #
    # :check_utf8 accepts a boolean will enable/disable UTF8 validation for the JSON stream
    def self.parse(str_or_io, options={}, read_bufsize=nil, &block)
      new(options).parse(str_or_io, read_bufsize, &block)
    end
  end

  class Encoder
    # A helper method for encode-and-forget use-cases
    #
    # Examples:
    #   Yajl::Encoder.encode(obj[, io, :pretty => true, :indent => "\t", &block])
    #
    #   output = Yajl::Encoder.encode(obj[, :pretty => true, :indent => "\t", &block])
    #
    # +obj+ is a ruby object to encode to JSON format
    #
    # +io+ is the optional IO stream to encode the ruby object to.
    # If +io+ isn't passed, the resulting JSON string is returned. If +io+ is passed, nil is returned.
    #
    # The +options+ hash allows you to set two encoding options - :pretty and :indent
    #
    # :pretty accepts a boolean and will enable/disable "pretty printing" the resulting output
    #
    # :indent accepts a string and will be used as the indent character(s) during the pretty print process
    #
    # If a block is passed, it will be used as (and work the same as) the +on_progress+ callback
    def self.encode(obj, *args, &block)
      # TODO: this code smells, any ideas?
      args.flatten!
      options = {}
      io = nil
      args.each do |arg|
        if arg.is_a?(Hash)
          options = arg
        elsif arg.respond_to?(:write)
          io = arg
        end
      end if args.any?
      new(options).encode(obj, io, &block)
    end
  end
end
