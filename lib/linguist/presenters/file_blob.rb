module Linguist
  module Presenters
    class FileBlob
      def initialize(path, io = $stdout)
        @path = path 
        @io = io
      end

      def present
        @io.puts "#{blob.name}: #{blob.loc} lines (#{blob.sloc} sloc)"
        @io.puts "  type:      #{type}"
        @io.puts "  mime type: #{blob.mime_type}"
        @io.puts "  language:  #{blob.language}"

        if blob.large?
          @io.puts "  blob is too large to be shown"
        end

        if blob.generated?
          @io.puts "  appears to be generated source code"
        end

        if blob.vendored?
          @io.puts "  appears to be a vendored file"
        end

      end

      private

      def blob
        @blob ||= Linguist::FileBlob.new(@path, Dir.pwd)
      end

      def type
        @type ||= if blob.text?
                 'Text'
               elsif blob.image?
                 'Image'
               else
                 'Binary'
               end
      end
    end
  end
end
