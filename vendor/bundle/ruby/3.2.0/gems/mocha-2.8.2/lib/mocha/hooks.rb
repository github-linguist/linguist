require 'mocha/mockery'

module Mocha
  # Integration hooks for test library authors.
  #
  # The methods in this module should be called from test libraries wishing to integrate with Mocha.
  #
  # This module is provided as part of the +Mocha::API+ module and is therefore part of the public API, but should only be used by authors of test libraries and not by typical "users" of Mocha.
  #
  # Integration with Test::Unit and Minitest are provided as part of Mocha, because they are (or were once) part of the Ruby standard library. Integration with other test libraries is not provided as *part* of Mocha, but is supported by means of the methods in this module.
  #
  # See the code in the +Adapter+ modules for examples of how to use the methods in this module. +Mocha::ExpectationErrorFactory+ may be used if you want +Mocha+ to raise a different type of exception.
  #
  # @see Mocha::Integration::TestUnit::Adapter
  # @see Mocha::Integration::Minitest::Adapter
  # @see Mocha::ExpectationErrorFactory
  # @see Mocha::API
  module Hooks
    # Prepares Mocha before a test (only for use by authors of test libraries).
    #
    # This method should be called before each individual test starts (including before any "setup" code).
    def mocha_setup
      Mockery.setup
    end

    # Verifies that all mock expectations have been met (only for use by authors of test libraries).
    #
    # This is equivalent to a series of "assertions".
    #
    # This method should be called at the end of each individual test, before it has been determined whether or not the test has passed.
    def mocha_verify(assertion_counter = nil)
      Mockery.verify(assertion_counter)
    end

    # Resets Mocha after a test (only for use by authors of test libraries).
    #
    # This method should be called after each individual test has finished (including after any "teardown" code).
    def mocha_teardown(origin = mocha_test_name)
      Mockery.teardown(origin)
    end

    # Returns a string representing the unit test name, to be included in some Mocha
    # to help track down potential bugs.
    def mocha_test_name
      nil
    end
  end
end
