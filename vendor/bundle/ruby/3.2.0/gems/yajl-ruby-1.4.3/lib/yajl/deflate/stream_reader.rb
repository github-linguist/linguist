module Yajl
  module Deflate
    # This is a wrapper around Zlib::Inflate, creating a #read method that adheres
    # to the IO spec, allowing for two parameters (length, and buffer)
    class StreamReader < ::Zlib::Inflate

      # Wrapper to the initialize method so we can set the initial IO to parse from.
      def initialize(io, options)
        @io = io
        super(options)
      end

      # A helper method to allow use similar to IO#read
      def read(len=nil, buffer=nil)
        if val = @io.read(len)
          unless buffer.nil?
            buffer.replace(inflate(val))
            return buffer
          end
          inflate(@io.read(len))
        else
          nil
        end
      end

      # Helper method for one-off parsing from a deflate-compressed stream
      #
      # See Yajl::Parser#parse for parameter documentation
      def self.parse(input, options={}, buffer_size=nil, &block)
        if input.is_a?(String)
          input = StringIO.new(input)
        end

        if options.is_a?(Hash)
          deflate_options = options.delete(:deflate_options)
          Yajl::Parser.new(options).parse(new(input, deflate_options), buffer_size, &block)
        elsif options.is_a?(Fixnum)
          Yajl::Parser.new.parse(new(input, options), buffer_size, &block)
        end
      end
    end
  end
end