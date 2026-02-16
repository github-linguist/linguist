require 'mocha/parameter_matchers/base'
require 'mocha/parameter_matchers/deprecations'

module Mocha
  module ParameterMatchers
    module Methods
      # Matches any +Object+ that is a kind of +klass+.
      #
      # @param [Class] klass expected class.
      # @return [KindOf] parameter matcher.
      #
      # @see Expectation#with
      # @see Kernel#kind_of?
      #
      # @example Actual parameter is a kind of +Integer+.
      #   object = mock()
      #   object.expects(:method_1).with(kind_of(Integer))
      #   object.method_1(99)
      #   # no error raised
      #
      # @example Actual parameter is not a kind of +Integer+.
      #   object = mock()
      #   object.expects(:method_1).with(kind_of(Integer))
      #   object.method_1('string')
      #   # error raised, because method_1 was not called with a kind of Integer
      def kind_of(klass)
        KindOf.new(klass)
      end
    end

    define_deprecated_matcher_method(:kind_of)

    # Parameter matcher which matches when actual parameter is a kind of specified class.
    class KindOf
      include BaseMethods

      # @private
      def initialize(klass)
        @klass = klass
      end

      # @private
      def matches?(available_parameters)
        parameter = available_parameters.shift
        # rubocop:disable Style/ClassCheck
        parameter.kind_of?(@klass)
        # rubocop:enable Style/ClassCheck
      end

      # @private
      def mocha_inspect
        "kind_of(#{@klass.mocha_inspect})"
      end
    end

    provide_deprecated_access_to(:KindOf)
  end
end
