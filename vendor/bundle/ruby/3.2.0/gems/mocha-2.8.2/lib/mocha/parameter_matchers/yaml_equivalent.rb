require 'mocha/parameter_matchers/base'
require 'mocha/parameter_matchers/deprecations'
require 'yaml'

module Mocha
  module ParameterMatchers
    module Methods
      # Matches any YAML that represents the specified +object+
      #
      # @param [Object] object object whose YAML to compare.
      # @return [YamlEquivalent] parameter matcher.
      #
      # @see Expectation#with
      #
      # @example Actual parameter is YAML equivalent of specified +object+.
      #   object = mock()
      #   object.expects(:method_1).with(yaml_equivalent(1, 2, 3))
      #   object.method_1("--- \n- 1\n- 2\n- 3\n")
      #   # no error raised
      #
      # @example Actual parameter is not YAML equivalent of specified +object+.
      #   object = mock()
      #   object.expects(:method_1).with(yaml_equivalent(1, 2, 3))
      #   object.method_1("--- \n- 1\n- 2\n")
      #   # error raised, because method_1 was not called with YAML representing the specified Array
      def yaml_equivalent(object)
        YamlEquivalent.new(object)
      end
    end

    define_deprecated_matcher_method(:yaml_equivalent)

    # Parameter matcher which matches if actual parameter is YAML equivalent of specified object.
    class YamlEquivalent
      include BaseMethods

      # @private
      def initialize(object)
        @object = object
      end

      # @private
      def matches?(available_parameters)
        parameter = available_parameters.shift
        # rubocop:disable Security/YAMLLoad
        @object == YAML.load(parameter)
        # rubocop:enable Security/YAMLLoad
      end

      # @private
      def mocha_inspect
        "yaml_equivalent(#{@object.mocha_inspect})"
      end
    end

    provide_deprecated_access_to(:YamlEquivalent)
  end
end
