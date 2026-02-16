module Yajl
  module Bzip2
    # A wrapper around the Bzip2::Writer class for easier JSON stream encoding
    class StreamWriter < ::Bzip2::Writer

      # A helper method for encoding to a bzip2-compressed stream
      #
      # Look up Yajl::Encoder#encode for parameter documentation
      def self.encode(obj, io)
        Yajl::Encoder.new.encode(obj, new(io))
      end
    end
  end
end