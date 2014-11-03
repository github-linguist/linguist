module Linguist
  module Strategy
    class EmptyBlob
      def self.call(blob, langauges)
        # Don't bother with binary contents or an empty file
        [] if blob.data.nil? || blob.data == ""
      end
    end
  end
end
