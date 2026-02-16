module Mocha
  module Detection
    module TestUnit
      def self.testcase
        if defined?(::Test::Unit::TestCase) &&
           !(defined?(::Minitest::Unit::TestCase) && (::Test::Unit::TestCase < ::Minitest::Unit::TestCase)) &&
           !(defined?(::Minitest::Spec) && (::Test::Unit::TestCase < ::Minitest::Spec))
          ::Test::Unit::TestCase
        end
      end

      def self.version
        version = '1.0.0'
        if testcase
          begin
            require 'test/unit/version'
          rescue LoadError # rubocop:disable Lint/HandleExceptions
          end
          if defined?(::Test::Unit::VERSION)
            version = ::Test::Unit::VERSION
          end
        end
        version
      end
    end
  end
end
