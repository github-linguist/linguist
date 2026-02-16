require 'mocha/parameter_matchers/all_of'
require 'mocha/parameter_matchers/base'
require 'mocha/parameter_matchers/deprecations'

module Mocha
  module ParameterMatchers
    module Methods
      # Matches any object that responds with +true+ to +include?(item)+
      # for all items.
      #
      # @param [*Array] items expected items.
      # @return [Includes] parameter matcher.
      #
      # @see Expectation#with
      #
      # @example Actual parameter includes all items.
      #   object = mock()
      #   object.expects(:method_1).with(includes('foo', 'bar'))
      #   object.method_1(['foo', 'bar', 'baz'])
      #   # no error raised
      #
      # @example Actual parameter does not include all items.
      #   object.method_1(['foo', 'baz'])
      #   # error raised, because ['foo', 'baz'] does not include 'bar'.
      #
      # @example Actual parameter includes item which matches nested matcher.
      #   object = mock()
      #   object.expects(:method_1).with(includes(has_key(:key)))
      #   object.method_1(['foo', 'bar', {key: 'baz'}])
      #   # no error raised
      #
      # @example Actual parameter does not include item matching nested matcher.
      #   object.method_1(['foo', 'bar', {:other_key => 'baz'}])
      #   # error raised, because no element matches `has_key(:key)` matcher
      #
      # @example Actual parameter is a String including substring.
      #   object = mock()
      #   object.expects(:method_1).with(includes('bar'))
      #   object.method_1('foobarbaz')
      #   # no error raised
      #
      # @example Actual parameter is a String not including substring.
      #   object.method_1('foobaz')
      #   # error raised, because 'foobaz' does not include 'bar'
      #
      # @example Actual parameter is a Hash including the given key.
      #   object = mock()
      #   object.expects(:method_1).with(includes(:bar))
      #   object.method_1({foo: 1, bar: 2})
      #   # no error raised
      #
      # @example Actual parameter is a Hash without the given key.
      #   object.method_1({foo: 1, baz: 2})
      #   # error raised, because hash does not include key 'bar'
      #
      # @example Actual parameter is a Hash with a key matching the given matcher.
      #   object = mock()
      #   object.expects(:method_1).with(includes(regexp_matches(/ar/)))
      #   object.method_1({'foo' => 1, 'bar' => 2})
      #   # no error raised
      #
      # @example Actual parameter is a Hash no key matching the given matcher.
      #   object.method_1({'foo' => 1, 'baz' => 3})
      #   # error raised, because hash does not include a key matching /ar/
      def includes(*items)
        Includes.new(*items)
      end
    end

    define_deprecated_matcher_method(:includes)

    # Parameter matcher which matches when actual parameter includes expected values.
    class Includes
      include BaseMethods

      # @private
      def initialize(*items)
        @items = items
      end

      # @private
      # rubocop:disable Metrics/PerceivedComplexity
      def matches?(available_parameters)
        parameter = available_parameters.shift
        return false unless parameter.respond_to?(:include?)
        if @items.size == 1
          # rubocop:disable Style/GuardClause
          if parameter.respond_to?(:any?) && !parameter.is_a?(String)
            parameter = parameter.keys if parameter.is_a?(Hash)
            return parameter.any? { |p| @items.first.to_matcher.matches?([p]) }
          else
            return parameter.include?(@items.first)
          end
          # rubocop:enable Style/GuardClause
        else
          includes_matchers = @items.map { |item| Includes.new(item) }
          AllOf.new(*includes_matchers).matches?([parameter])
        end
      end
      # rubocop:enable Metrics/PerceivedComplexity

      # @private
      def mocha_inspect
        item_descriptions = @items.map(&:mocha_inspect)
        "includes(#{item_descriptions.join(', ')})"
      end
    end

    provide_deprecated_access_to(:Includes)
  end
end
