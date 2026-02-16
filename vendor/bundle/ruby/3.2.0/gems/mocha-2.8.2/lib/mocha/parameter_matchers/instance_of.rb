require 'mocha/parameter_matchers/base'
require 'mocha/parameter_matchers/deprecations'

module Mocha
  module ParameterMatchers
    module Methods
      # Matches any object that is an instance of +klass+
      #
      # @param [Class] klass expected class.
      # @return [InstanceOf] parameter matcher.
      #
      # @see Expectation#with
      # @see Kernel#instance_of?
      #
      # @example Actual parameter is an instance of +String+.
      #   object = mock()
      #   object.expects(:method_1).with(instance_of(String))
      #   object.method_1('string')
      #   # no error raised
      #
      # @example Actual parameter is not an instance of +String+.
      #   object = mock()
      #   object.expects(:method_1).with(instance_of(String))
      #   object.method_1(99)
      #   # error raised, because method_1 was not called with an instance of String
      def instance_of(klass)
        InstanceOf.new(klass)
      end
    end

    define_deprecated_matcher_method(:instance_of)

    # Parameter matcher which matches when actual parameter is an instance of the specified class.
    class InstanceOf
      include BaseMethods

      # @private
      def initialize(klass)
        @klass = klass
      end

      # @private
      def matches?(available_parameters)
        parameter = available_parameters.shift
        parameter.instance_of?(@klass)
      end

      # @private
      def mocha_inspect
        "instance_of(#{@klass.mocha_inspect})"
      end
    end

    provide_deprecated_access_to(:InstanceOf)
  end
end
