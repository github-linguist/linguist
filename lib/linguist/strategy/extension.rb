module Linguist
  module Strategy
    # Detects language based on extension
    class Extension
      def self.call(blob, candidates)
        languages = Language.find_by_extension(blob.name.to_s)
        if candidates.any? then languages & candidates else languages end
      end
    end
  end
end
