module Linguist
  module Strategy
    # Stops detection if the blob contents are empty
    class EmptyBlob
      def self.call(blob, langauges)
        # Return empty array to stop detection
        [] if blob.data.nil? || blob.data == ""
      end
    end
  end
end
