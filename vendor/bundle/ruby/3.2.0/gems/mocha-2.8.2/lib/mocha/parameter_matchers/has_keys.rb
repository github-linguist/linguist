require 'mocha/parameter_matchers/base'
require 'mocha/parameter_matchers/deprecations'

module Mocha
  module ParameterMatchers
    module Methods
      # Matches +Hash+ containing +keys+.
      #
      # @param [*Array<Object>] keys expected keys.
      # @return [HasKeys] parameter matcher.
      #
      # @see Expectation#with
      #
      # @example Actual parameter contains entry with expected keys.
      #   object = mock()
      #   object.expects(:method_1).with(has_keys(:key_1, :key_2))
      #   object.method_1(:key_1 => 1, :key_2 => 2, :key_3 => 3)
      #   # no error raised
      #
      # @example Actual parameter does not contain all expected keys.
      #   object = mock()
      #   object.expects(:method_1).with(has_keys(:key_1, :key_2))
      #   object.method_1(:key_2 => 2)
      #   # error raised, because method_1 was not called with Hash containing key: :key_1
      #
      def has_keys(*keys) # rubocop:disable Naming/PredicateName
        HasKeys.new(*keys)
      end
    end

    define_deprecated_matcher_method(:has_keys)

    # Parameter matcher which matches when actual parameter contains +Hash+ with all expected keys.
    class HasKeys
      include BaseMethods

      # @private
      def initialize(*keys)
        raise ArgumentError, 'No arguments. Expecting at least one.' if keys.empty?

        @keys = keys
      end

      # @private
      def matches?(available_parameters)
        parameter = available_parameters.shift
        return false unless parameter.respond_to?(:keys)

        @keys.map(&:to_matcher).all? do |matcher|
          parameter.keys.any? { |key| matcher.matches?([key]) }
        end
      end

      # @private
      def mocha_inspect
        "has_keys(#{@keys.mocha_inspect(false)})"
      end
    end

    provide_deprecated_access_to(:HasKeys)
  end
end
