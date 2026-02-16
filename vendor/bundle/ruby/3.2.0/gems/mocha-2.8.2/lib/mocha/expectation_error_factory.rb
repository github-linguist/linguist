require 'mocha/backtrace_filter'
require 'mocha/expectation_error'

module Mocha
  # This factory determines what class of exception should be raised when Mocha detects a test failure.
  #
  # This class should only be used by authors of test libraries and not by typical "users" of Mocha.
  #
  # For example, it is used by +Mocha::Integration::Minitest::Adapter+ in order to have Mocha raise a +Minitest::Assertion+ which can then be sensibly handled by +Minitest::Unit::TestCase+.
  #
  # @see Mocha::Integration::Minitest::Adapter
  class ExpectationErrorFactory
    class << self
      # @!attribute exception_class
      #   Determines what class of exception should be raised when Mocha detects a test failure.
      #
      #   This attribute may be set by authors of test libraries in order to have Mocha raise exceptions of a specific class when there is an unexpected invocation or an unsatisfied expectation.
      #
      #   By default a +Mocha::ExpectationError+ will be raised.
      #
      #   @return [Exception] class of exception to be raised when an expectation error occurs
      #   @see Mocha::ExpectationError
      attr_accessor :exception_class

      # @private
      def build(message = nil, backtrace = [])
        exception = exception_class.new(message)
        filter = BacktraceFilter.new
        exception.set_backtrace(filter.filtered(backtrace))
        exception
      end
    end
    self.exception_class = ExpectationError
  end
end
