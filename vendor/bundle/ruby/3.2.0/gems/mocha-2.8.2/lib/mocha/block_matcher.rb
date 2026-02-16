module Mocha
  module BlockMatchers
    class OptionalBlock
      def match?(_actual_block)
        true
      end

      def mocha_inspect; end
    end

    class BlockGiven
      def match?(actual_block)
        !actual_block.nil?
      end

      def mocha_inspect
        'with block given'
      end
    end

    class NoBlockGiven
      def match?(actual_block)
        actual_block.nil?
      end

      def mocha_inspect
        'with no block given'
      end
    end
  end
end
