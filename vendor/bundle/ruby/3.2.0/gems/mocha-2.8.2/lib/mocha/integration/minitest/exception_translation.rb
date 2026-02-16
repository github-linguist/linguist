require 'mocha/expectation_error'

module Mocha
  module Integration
    module Minitest
      def self.translate(exception)
        return exception unless exception.is_a?(::Mocha::ExpectationError)
        translated_exception = ::Minitest::Assertion.new(exception.message)
        translated_exception.set_backtrace(exception.backtrace)
        translated_exception
      end
    end
  end
end
