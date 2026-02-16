require 'mocha/deprecation'
require 'mocha/parameter_matchers/deprecations'

module Mocha
  module ParameterMatchers
    # @abstract Include and implement +#matches?+ and +#mocha_inspect+ to define a custom matcher. Also add a suitably named instance method to {Methods} to build an instance of the new matcher c.f. {Methods#equals}.
    module BaseMethods
      # A shorthand way of combining two matchers when both must match.
      #
      # Returns a new {AllOf} parameter matcher combining two matchers using a logical AND.
      #
      # This shorthand will not work with an implicit equals match. Instead, an explicit {Equals} matcher should be used.
      #
      # @param [BaseMethods] other parameter matcher.
      # @return [AllOf] parameter matcher.
      #
      # @see Expectation#with
      #
      # @example Alternative ways to combine matchers with a logical AND.
      #   object = mock()
      #   object.expects(:run).with(all_of(has_key(:foo), has_key(:bar)))
      #   object.run(foo: 'foovalue', bar: 'barvalue')
      #
      #   # is exactly equivalent to
      #
      #   object.expects(:run).with(has_key(:foo) & has_key(:bar))
      #   object.run(foo: 'foovalue', bar: 'barvalue)
      def &(other)
        AllOf.new(self, other)
      end

      # A shorthand way of combining two matchers when at least one must match.
      #
      # Returns a new +AnyOf+ parameter matcher combining two matchers using a logical OR.
      #
      # This shorthand will not work with an implicit equals match. Instead, an explicit {Equals} matcher should be used.
      #
      # @param [BaseMethods] other parameter matcher.
      # @return [AnyOf] parameter matcher.
      #
      # @see Expectation#with
      #
      # @example Alternative ways to combine matchers with a logical OR.
      #   object = mock()
      #   object.expects(:run).with(any_of(has_key(:foo), has_key(:bar)))
      #   object.run(foo: 'foovalue')
      #
      #   # is exactly equivalent to
      #
      #   object.expects(:run).with(has_key(:foo) | has_key(:bar))
      #   object.run(foo: 'foovalue')
      #
      # @example Using an explicit {Equals} matcher in combination with {#|}.
      #   object.expects(:run).with(equals(1) | equals(2))
      #   object.run(1) # passes
      #   object.run(2) # passes
      #   object.run(3) # fails
      def |(other)
        AnyOf.new(self, other)
      end
    end

    provide_deprecated_access_to(:BaseMethods)

    # @deprecated Include +BaseMethods+ module instead.
    class Base
      include BaseMethods

      # @private
      def self.inherited(subclass)
        super
        Deprecation.warning(
          "Include #{BaseMethods} module into #{subclass} instead of inheriting from #{self}."
        )
      end
    end

    provide_deprecated_access_to(:Base)
  end
end
