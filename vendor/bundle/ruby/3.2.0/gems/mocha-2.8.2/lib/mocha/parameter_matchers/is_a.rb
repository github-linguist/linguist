require 'mocha/parameter_matchers/base'
require 'mocha/parameter_matchers/deprecations'

module Mocha
  module ParameterMatchers
    module Methods
      # Matches any object that is a +klass+.
      #
      # @param [Class] klass expected class.
      # @return [IsA] parameter matcher.
      #
      # @see Expectation#with
      # @see Kernel#is_a?
      #
      # @example Actual parameter is a +Integer+.
      #   object = mock()
      #   object.expects(:method_1).with(is_a(Integer))
      #   object.method_1(99)
      #   # no error raised
      #
      # @example Actual parameter is not a +Integer+.
      #   object = mock()
      #   object.expects(:method_1).with(is_a(Integer))
      #   object.method_1('string')
      #   # error raised, because method_1 was not called with an Integer
      #
      def is_a(klass) # rubocop:disable Naming/PredicateName
        IsA.new(klass)
      end
    end

    define_deprecated_matcher_method(:is_a)

    # Parameter matcher which matches when actual parameter is a specific class.
    class IsA
      include BaseMethods

      # @private
      def initialize(klass)
        @klass = klass
      end

      # @private
      def matches?(available_parameters)
        parameter = available_parameters.shift
        parameter.is_a?(@klass)
      end

      # @private
      def mocha_inspect
        "is_a(#{@klass.mocha_inspect})"
      end
    end

    provide_deprecated_access_to(:IsA)
  end
end
