module Mocha
  # Default exception class raised when an unexpected invocation or an unsatisfied expectation occurs.
  #
  # Authors of test libraries may use +Mocha::ExpectationErrorFactory+ to have Mocha raise a different exception.
  #
  # @see Mocha::ExpectationErrorFactory
  class ExpectationError < Exception; end # rubocop:disable Lint/InheritException
end
