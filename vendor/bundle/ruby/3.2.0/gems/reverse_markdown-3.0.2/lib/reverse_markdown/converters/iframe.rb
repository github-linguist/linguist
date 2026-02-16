module ReverseMarkdown
  module Converters
    class Iframe < Base
      def convert(node, state = {})
        extract_src(node)
      end
    end

    register :iframe, Iframe.new
  end
end
