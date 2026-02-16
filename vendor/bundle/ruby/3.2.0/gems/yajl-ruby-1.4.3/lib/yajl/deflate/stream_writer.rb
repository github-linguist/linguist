module Yajl
  module Deflate
    # A wrapper around the Zlib::Deflate class for easier JSON stream parsing
    class StreamWriter < ::Zlib::Deflate

      # A helper method to allow use similar to IO#write
      def write(str)
        deflate(str)
        str.size unless str.nil?
      end

      # A helper method for one-off encoding to a deflate-compressed stream
      #
      # Look up Yajl::Encoder#encode for parameter documentation
      def self.encode(obj, io)
        Yajl::Encoder.new.encode(obj, new(io))
      end
    end
  end
end