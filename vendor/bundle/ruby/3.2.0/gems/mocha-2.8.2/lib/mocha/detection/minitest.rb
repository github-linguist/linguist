module Mocha
  module Detection
    module Minitest
      def self.testcase
        if defined?(::Minitest::Test)
          ::Minitest::Test
        elsif defined?(::Minitest::Unit::TestCase)
          ::Minitest::Unit::TestCase
        end
      end

      def self.version
        if defined?(::Minitest::Unit::VERSION)
          ::Minitest::Unit::VERSION
        elsif defined?(::Minitest::VERSION)
          ::Minitest::VERSION
        else
          '0.0.0'
        end
      end
    end
  end
end
