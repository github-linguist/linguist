require 'mocha/parameter_matchers/base'
require 'mocha/parameter_matchers/deprecations'

module Mocha
  module ParameterMatchers
    module Methods
      # Matches any parameters. This is used as the default for a newly built expectation.
      #
      # @return [AnyParameters] parameter matcher.
      #
      # @see Expectation#with
      #
      # @example Any parameters will match.
      #   object = mock()
      #   object.expects(:method_1).with(any_parameters)
      #   object.method_1(1, 2, 3, 4)
      #   # no error raised
      #
      #   object = mock()
      #   object.expects(:method_1).with(any_parameters)
      #   object.method_1(5, 6, 7, 8, 9, 0)
      #   # no error raised
      def any_parameters
        AnyParameters.new
      end
    end

    define_deprecated_matcher_method(:any_parameters)

    # Parameter matcher which always matches whatever the parameters.
    class AnyParameters
      include BaseMethods

      # @private
      def matches?(available_parameters)
        until available_parameters.empty?
          available_parameters.shift
        end
        true
      end

      # @private
      def mocha_inspect
        'any_parameters'
      end
    end

    provide_deprecated_access_to(:AnyParameters)
  end
end
