module Linguist
  module Strategy
    # Check if there's a shebang line and use that as authoritative
    class Shebang
      def self.call(blob, _)
        Language.find_by_shebang(blob.data)
      end
    end
  end
end
