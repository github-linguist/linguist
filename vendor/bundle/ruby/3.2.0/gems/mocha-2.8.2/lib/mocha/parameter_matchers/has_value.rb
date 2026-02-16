require 'mocha/parameter_matchers/base'
require 'mocha/parameter_matchers/deprecations'

module Mocha
  module ParameterMatchers
    module Methods
      # Matches +Hash+ containing +value+.
      #
      # @param [Object] value expected value.
      # @return [HasValue] parameter matcher.
      #
      # @see Expectation#with
      #
      # @example Actual parameter contains entry with expected value.
      #   object = mock()
      #   object.expects(:method_1).with(has_value(1))
      #   object.method_1('key_1' => 1, 'key_2' => 2)
      #   # no error raised
      #
      # @example Actual parameter does not contain entry with expected value.
      #   object = mock()
      #   object.expects(:method_1).with(has_value(1))
      #   object.method_1('key_2' => 2)
      #   # error raised, because method_1 was not called with Hash containing value: 1
      #
      def has_value(value) # rubocop:disable Naming/PredicateName
        HasValue.new(value)
      end
    end

    define_deprecated_matcher_method(:has_value)

    # Parameter matcher which matches when actual parameter contains +Hash+ entry with expected value.
    class HasValue
      include BaseMethods

      # @private
      def initialize(value)
        @value = value
      end

      # @private
      def matches?(available_parameters)
        parameter = available_parameters.shift
        return false unless parameter.respond_to?(:values)
        parameter.values.any? { |value| @value.to_matcher.matches?([value]) }
      end

      # @private
      def mocha_inspect
        "has_value(#{@value.mocha_inspect})"
      end
    end

    provide_deprecated_access_to(:HasValue)
  end
end
