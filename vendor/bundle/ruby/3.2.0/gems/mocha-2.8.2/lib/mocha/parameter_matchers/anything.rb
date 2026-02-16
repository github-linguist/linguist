require 'mocha/parameter_matchers/base'
require 'mocha/parameter_matchers/deprecations'

module Mocha
  module ParameterMatchers
    module Methods
      # Matches any object.
      #
      # @return [Anything] parameter matcher.
      #
      # @see Expectation#with
      #
      # @example Any object will match.
      #   object = mock()
      #   object.expects(:method_1).with(anything)
      #   object.method_1('foo')
      #   object.method_1(789)
      #   object.method_1(:bar)
      #   # no error raised
      def anything
        Anything.new
      end
    end

    define_deprecated_matcher_method(:anything)

    # Parameter matcher which always matches a single parameter.
    class Anything
      include BaseMethods

      # @private
      def matches?(available_parameters)
        available_parameters.shift
        true
      end

      # @private
      def mocha_inspect
        'anything'
      end
    end

    provide_deprecated_access_to(:Anything)
  end
end
