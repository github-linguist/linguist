module Linguist
  module Strategy
    # Detects language based on filename
    class Filename
      def self.call(blob, candidates)
        name = blob.name.to_s
        languages = Language.find_by_filename(name)
        if candidates.any? then candidates & languages else languages end
      end
    end
  end
end
