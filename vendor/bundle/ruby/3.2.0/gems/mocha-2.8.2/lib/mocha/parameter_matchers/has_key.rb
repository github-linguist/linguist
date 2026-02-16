require 'mocha/parameter_matchers/base'
require 'mocha/parameter_matchers/deprecations'

module Mocha
  module ParameterMatchers
    module Methods
      # Matches +Hash+ containing +key+.
      #
      # @param [Object] key expected key.
      # @return [HasKey] parameter matcher.
      #
      # @see Expectation#with
      #
      # @example Actual parameter contains entry with expected key.
      #   object = mock()
      #   object.expects(:method_1).with(has_key('key_1'))
      #   object.method_1('key_1' => 1, 'key_2' => 2)
      #   # no error raised
      #
      # @example Actual parameter does not contain entry with expected key.
      #   object = mock()
      #   object.expects(:method_1).with(has_key('key_1'))
      #   object.method_1('key_2' => 2)
      #   # error raised, because method_1 was not called with Hash containing key: 'key_1'
      #
      def has_key(key) # rubocop:disable Naming/PredicateName
        HasKey.new(key)
      end
    end

    define_deprecated_matcher_method(:has_key)

    # Parameter matcher which matches when actual parameter contains +Hash+ entry with expected key.
    class HasKey
      include BaseMethods

      # @private
      def initialize(key)
        @key = key
      end

      # @private
      def matches?(available_parameters)
        parameter = available_parameters.shift
        return false unless parameter.respond_to?(:keys)
        parameter.keys.any? { |key| @key.to_matcher.matches?([key]) }
      end

      # @private
      def mocha_inspect
        "has_key(#{@key.mocha_inspect})"
      end
    end

    provide_deprecated_access_to(:HasKey)
  end
end
