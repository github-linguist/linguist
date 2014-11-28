module Linguist
  module Strategy
    # Detects language based on filename and/or extension
    class Filename
      def self.call(blob, _)
        Language.find_by_filename(blob.name.to_s)
      end
    end
  end
end
