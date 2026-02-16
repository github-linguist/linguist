require 'mocha/error_with_filtered_backtrace'

module Mocha
  # Exception raised when stubbing a particular method is not allowed.
  #
  # @see Configuration.prevent
  class StubbingError < ErrorWithFilteredBacktrace; end
end
