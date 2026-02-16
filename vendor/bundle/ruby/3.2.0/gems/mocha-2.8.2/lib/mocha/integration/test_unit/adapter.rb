require 'mocha/api'
require 'mocha/integration/assertion_counter'
require 'mocha/expectation_error'

module Mocha
  module Integration
    module TestUnit
      # Integrates Mocha into recent versions of Test::Unit.
      #
      # See the source code for an example of how to integrate Mocha into a test library.
      module Adapter
        include Mocha::API

        # @private
        def self.applicable_to?(test_unit_version)
          Gem::Requirement.new('>= 2.5.1').satisfied_by?(test_unit_version)
        end

        # @private
        def self.description
          'adapter for Test::Unit gem >= v2.5.1'
        end

        # @private
        def self.included(mod)
          mod.setup :mocha_setup, before: :prepend

          mod.exception_handler(:handle_mocha_expectation_error)

          mod.cleanup after: :append do
            assertion_counter = Integration::AssertionCounter.new(self)
            mocha_verify(assertion_counter)
          end

          mod.teardown :mocha_teardown, after: :append
        end

        private

        # @private
        def mocha_test_name
          name
        end

        # @private
        def handle_mocha_expectation_error(exception)
          return false unless exception.is_a?(Mocha::ExpectationError)
          problem_occurred
          add_failure(exception.message, exception.backtrace)
          true
        end
      end
    end
  end
end
