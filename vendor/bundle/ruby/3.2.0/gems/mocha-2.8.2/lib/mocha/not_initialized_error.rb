require 'mocha/error_with_filtered_backtrace'

module Mocha
  # Exception raised when Mocha has not been initialized, e.g. outside the
  # context of a test.
  class NotInitializedError < ErrorWithFilteredBacktrace; end
end
