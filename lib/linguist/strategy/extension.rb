module Linguist
  module Strategy
    # Detects language based on extension
    class Extension
      def self.call(blob, _)
        Language.find_by_extension(blob.name.to_s)
      end
    end
  end
end
