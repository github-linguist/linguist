require 'mocha/parameter_matchers/base'
require 'mocha/parameter_matchers/deprecations'

module Mocha
  module ParameterMatchers
    module Methods
      # Matches any +Object+ equalling +value+.
      #
      # @param [Object] value expected value.
      # @return [Equals] parameter matcher.
      #
      # @see Expectation#with
      # @see Object#==
      #
      # @example Actual parameter equals expected parameter.
      #   object = mock()
      #   object.expects(:method_1).with(equals(2))
      #   object.method_1(2)
      #   # no error raised
      #
      # @example Actual parameter does not equal expected parameter.
      #   object = mock()
      #   object.expects(:method_1).with(equals(2))
      #   object.method_1(3)
      #   # error raised, because method_1 was not called with an +Object+ that equals 2
      def equals(value)
        Equals.new(value)
      end
    end

    define_deprecated_matcher_method(:equals)

    # Parameter matcher which matches when actual parameter equals expected value.
    class Equals
      include BaseMethods

      # @private
      def initialize(value)
        @value = value
      end

      # @private
      def matches?(available_parameters)
        parameter = available_parameters.shift
        parameter == @value
      end

      # @private
      def mocha_inspect
        @value.mocha_inspect
      end
    end

    provide_deprecated_access_to(:Equals)
  end
end
