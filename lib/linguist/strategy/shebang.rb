module Linguist
  module Strategy
    class Shebang
      def self.call(blob, _)
        Language.find_by_shebang(blob.data)
      end
    end
  end
end
