require 'mocha/parameter_matchers/base'
require 'mocha/parameter_matchers/deprecations'

module Mocha
  module ParameterMatchers
    module Methods
      # Matches +Hash+ containing entry with +key+ and +value+.
      #
      # @overload def has_entry(key, value)
      #   @param [Object] key key for entry.
      #   @param [Object] value value for entry.
      # @overload def has_entry(single_entry_hash)
      #   @param [Hash] single_entry_hash +Hash+ with single entry.
      #   @raise [ArgumentError] if +single_entry_hash+ does not contain exactly one entry.
      #
      # @return [HasEntry] parameter matcher.
      #
      # @see Expectation#with
      #
      # @example Actual parameter contains expected entry supplied as key and value.
      #   object = mock()
      #   object.expects(:method_1).with(has_entry('key_1', 1))
      #   object.method_1('key_1' => 1, 'key_2' => 2)
      #   # no error raised
      #
      # @example Actual parameter contains expected entry supplied as +Hash+ entry.
      #   object = mock()
      #   object.expects(:method_1).with(has_entry('key_1' => 1))
      #   object.method_1('key_1' => 1, 'key_2' => 2)
      #   # no error raised
      #
      # @example Actual parameter does not contain expected entry supplied as key and value.
      #   object = mock()
      #   object.expects(:method_1).with(has_entry('key_1', 1))
      #   object.method_1('key_1' => 2, 'key_2' => 1)
      #   # error raised, because method_1 was not called with Hash containing entry: 'key_1' => 1
      #
      # @example Actual parameter does not contain expected entry supplied as +Hash+ entry.
      #
      #   object = mock()
      #   object.expects(:method_1).with(has_entry('key_1' => 1))
      #   object.method_1('key_1' => 2, 'key_2' => 1)
      #   # error raised, because method_1 was not called with Hash containing entry: 'key_1' => 1
      #
      def has_entry(*options) # rubocop:disable Naming/PredicateName
        case options.length
        when 0
          raise ArgumentError, 'No arguments. Expecting at least one.'
        when 1
          key, value = HasEntry.parse_option(options[0])
        when 2
          key, value = options
        else
          raise ArgumentError, 'Too many arguments; use either a single argument (must be a Hash) or two arguments (a key and a value).'
        end
        HasEntry.new(key, value)
      end
    end

    define_deprecated_matcher_method(:has_entry)

    # Parameter matcher which matches when actual parameter contains expected +Hash+ entry.
    class HasEntry
      include BaseMethods

      # @private
      def initialize(key, value)
        @key = key
        @value = value
      end

      # @private
      def matches?(available_parameters)
        parameter = available_parameters.shift
        return false unless parameter.respond_to?(:keys) && parameter.respond_to?(:[])
        matching_keys = parameter.keys.select { |key| @key.to_matcher.matches?([key]) }
        matching_keys.any? { |key| @value.to_matcher.matches?([parameter[key]]) }
      end

      # @private
      def mocha_inspect
        "has_entry(#{@key.mocha_inspect} => #{@value.mocha_inspect})"
      end

      # @private
      def self.parse_option(option)
        case option
        when Hash
          case option.length
          when 0
            raise ArgumentError, 'Argument has no entries.'
          when 1
            option.first
          else
            raise ArgumentError, 'Argument has multiple entries. Use Mocha::ParameterMatchers#has_entries instead.'
          end
        else
          raise ArgumentError, 'Argument is not a Hash.'
        end
      end
    end

    provide_deprecated_access_to(:HasEntry)
  end
end
