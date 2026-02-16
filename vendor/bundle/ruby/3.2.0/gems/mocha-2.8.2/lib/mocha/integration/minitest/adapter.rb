require 'mocha/api'
require 'mocha/integration/assertion_counter'
require 'mocha/expectation_error_factory'
require 'mocha/parameter_matchers/deprecations'

module Mocha
  module Integration
    module Minitest
      # Integrates Mocha into recent versions of Minitest.
      #
      # See the source code for an example of how to integrate Mocha into a test library.
      module Adapter
        include Mocha::API

        # @private
        def self.applicable_to?(minitest_version)
          Gem::Requirement.new('>= 3.3.0').satisfied_by?(minitest_version)
        end

        # @private
        def self.description
          'adapter for Minitest gem >= v3.3.0'
        end

        # @private
        def self.included(mod)
          mod.extend(Mocha::ParameterMatchers::Deprecations)
          Mocha::ExpectationErrorFactory.exception_class = ::Minitest::Assertion
        end

        # @private
        def before_setup
          mocha_setup
          super
        end

        # @private
        def before_teardown
          return unless passed?
          assertion_counter = Integration::AssertionCounter.new(self)
          mocha_verify(assertion_counter)
        ensure
          super
        end

        # @private
        def after_teardown
          super
          mocha_teardown
        end

        # @private
        def mocha_test_name
          if respond_to?(:name)
            test_name = name
          elsif respond_to?(:__name__) # Older minitest
            test_name = __name__
          end

          if test_name
            "#{self.class.name}##{test_name}"
          else
            self.class.name
          end
        end
      end
    end
  end
end
