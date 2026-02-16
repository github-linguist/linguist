require 'mocha/parameter_matchers/base'
require 'mocha/parameter_matchers/all_of'
require 'mocha/parameter_matchers/deprecations'
require 'yaml'

module Mocha
  module ParameterMatchers
    module Methods
      # @overload def responds_with(message, result)
      #   Matches any object that responds to +message+ with +result+. To put it another way, it tests the quack, not the duck.
      #   @param [Symbol] message method to invoke.
      #   @param [Object] result expected result of sending +message+.
      # @overload def responds_with(messages_vs_results)
      #   Matches any object that responds to all the messages with the corresponding results as specified by +messages_vs_results+.
      #   @param [Hash<Symbol,Object>] messages_vs_results +Hash+ of messages vs results.
      #   @raise [ArgumentError] if +messages_vs_results+ does not contain at least one entry.
      #
      # @return [RespondsWith] parameter matcher.
      #
      # @see Expectation#with
      #
      # @example Actual parameter responds with "FOO" when :upcase is invoked.
      #   object = mock()
      #   object.expects(:method_1).with(responds_with(:upcase, "FOO"))
      #   object.method_1("foo")
      #   # no error raised, because "foo".upcase == "FOO"
      #
      # @example Actual parameter does not respond with "FOO" when :upcase is invoked.
      #   object = mock()
      #   object.expects(:method_1).with(responds_with(:upcase, "BAR"))
      #   object.method_1("foo")
      #   # error raised, because "foo".upcase != "BAR"
      #
      # @example Actual parameter responds with "FOO" when :upcase is invoked and "oof" when :reverse is invoked.
      #   object = mock()
      #   object.expects(:method_1).with(responds_with(upcase: "FOO", reverse: "oof"))
      #   object.method_1("foo")
      #   # no error raised, because "foo".upcase == "FOO" and "foo".reverse == "oof"
      def responds_with(*options)
        case options.length
        when 0
          raise ArgumentError, 'No arguments. Expecting at least one.'
        when 1
          option = options.first
          raise ArgumentError, 'Argument is not a Hash.' unless option.is_a?(Hash)
          raise ArgumentError, 'Argument has no entries.' if option.empty?

          matchers = option.map { |message, result| RespondsWith.new(message, result) }
          AllOf.new(*matchers)
        when 2
          message, result = options
          RespondsWith.new(message, result)
        else
          raise ArgumentError, 'Too many arguments; use either a single argument (must be a Hash) or two arguments (a message and a result).'
        end
      end
    end

    define_deprecated_matcher_method(:responds_with)

    # Parameter matcher which matches if actual parameter returns expected result when specified method is invoked.
    class RespondsWith
      include BaseMethods

      # @private
      def initialize(message, result)
        @message = message
        @result = result
      end

      # @private
      def matches?(available_parameters)
        parameter = available_parameters.shift
        @result.to_matcher.matches?([parameter.__send__(@message)])
      end

      # @private
      def mocha_inspect
        "responds_with(#{@message.mocha_inspect}, #{@result.mocha_inspect})"
      end
    end

    provide_deprecated_access_to(:RespondsWith)
  end
end
