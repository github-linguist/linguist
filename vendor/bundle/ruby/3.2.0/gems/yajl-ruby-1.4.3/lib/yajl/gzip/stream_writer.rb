module Yajl
  module Gzip
    # Wraper around the Zlib::GzipWriter class
    class StreamWriter < ::Zlib::GzipWriter
      # A helper method for one-off encoding to a gzip-compressed stream
      #
      # Look up Yajl::Encoder#encode for parameter documentation
      def self.encode(obj, io)
        Yajl::Encoder.new.encode(obj, new(io))
      end
    end
  end
end