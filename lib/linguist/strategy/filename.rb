module Linguist
  module Strategy
    # Detects language based on filename
    class Filename
      def self.call(blob, _)
        name = blob.name.to_s
        Language.find_by_filename(name)
      end
    end
  end
end
