require 'mocha/ruby_version'
require 'mocha/deprecation'

module Mocha
  # Allows setting of configuration options. See {Configuration} for the available options.
  #
  # Typically the configuration is set globally in a +test_helper.rb+ or +spec_helper.rb+ file.
  #
  # @see Configuration
  #
  # @yieldparam configuration [Configuration] the configuration for modification
  #
  # @example Setting multiple configuration options
  #   Mocha.configure do |c|
  #     c.stubbing_method_unnecessarily = :prevent
  #     c.stubbing_method_on_non_mock_object = :warn
  #     c.stubbing_method_on_nil = :allow
  #   end
  #
  def self.configure
    yield configuration
  end

  # @private
  def self.configuration
    Configuration.configuration
  end

  # This class provides a number of ways to configure the library.
  #
  # Typically the configuration is set globally in a +test_helper.rb+ or +spec_helper.rb+ file.
  #
  # @example Setting multiple configuration options
  #   Mocha.configure do |c|
  #     c.stubbing_method_unnecessarily = :prevent
  #     c.stubbing_method_on_non_mock_object = :warn
  #     c.stubbing_method_on_nil = :allow
  #   end
  #
  class Configuration
    # @private
    DEFAULTS = {
      stubbing_method_unnecessarily: :allow,
      stubbing_method_on_non_mock_object: :allow,
      stubbing_non_existent_method: :allow,
      stubbing_non_public_method: :allow,
      stubbing_method_on_nil: :prevent,
      display_matching_invocations_on_failure: false,
      strict_keyword_argument_matching: false
    }.freeze

    attr_reader :options
    protected :options

    # @private
    def initialize(options = {})
      @options = DEFAULTS.merge(options)
    end

    # @private
    def initialize_copy(other)
      @options = other.options.dup
    end

    # @private
    def merge(other)
      self.class.new(@options.merge(other.options))
    end

    # Configure whether stubbing methods unnecessarily is allowed.
    #
    # This is useful for identifying unused stubs. Unused stubs are often accidentally introduced when code is {http://martinfowler.com/bliki/DefinitionOfRefactoring.html refactored}.
    #
    # When +value+ is +:allow+, do nothing. This is the default.
    # When +value+ is +:warn+, display a warning.
    # When +value+ is +:prevent+, raise a {StubbingError}.
    #
    # @param [Symbol] value one of +:allow+, +:warn+, +:prevent+.
    #
    # @example Preventing unnecessary stubbing of a method
    #   Mocha.configure do |c|
    #     c.stubbing_method_unnecessarily = :prevent
    #   end
    #
    #   example = mock('example')
    #   example.stubs(:unused_stub)
    #   # => Mocha::StubbingError: stubbing method unnecessarily:
    #   # =>   #<Mock:example>.unused_stub(any_parameters)
    #
    def stubbing_method_unnecessarily=(value)
      @options[:stubbing_method_unnecessarily] = value
    end

    # @private
    def stubbing_method_unnecessarily
      @options[:stubbing_method_unnecessarily]
    end

    # Configure whether stubbing methods on non-mock objects is allowed.
    #
    # If you like the idea of {http://www.jmock.org/oopsla2004.pdf mocking roles not objects} and {http://www.mockobjects.com/2007/04/test-smell-mocking-concrete-classes.html you don't like stubbing concrete classes}, this is the setting for you. However, while this restriction makes a lot of sense in Java with its {http://java.sun.com/docs/books/tutorial/java/concepts/interface.html explicit interfaces}, it may be moot in Ruby where roles are probably best represented as Modules.
    #
    # When +value+ is +:allow+, do nothing. This is the default.
    # When +value+ is +:warn+, display a warning.
    # When +value+ is +:prevent+, raise a {StubbingError}.
    #
    # @param [Symbol] value one of +:allow+, +:warn+, +:prevent+.
    #
    # @example Preventing stubbing of a method on a non-mock object
    #   Mocha.configure do |c|
    #     c.stubbing_method_on_non_mock_object = :prevent
    #   end
    #
    #   class Example
    #     def example_method; end
    #   end
    #
    #   example = Example.new
    #   example.stubs(:example_method)
    #   # => Mocha::StubbingError: stubbing method on non-mock object:
    #   # =>   #<Example:0x593620>.example_method
    #
    def stubbing_method_on_non_mock_object=(value)
      @options[:stubbing_method_on_non_mock_object] = value
    end

    # @private
    def stubbing_method_on_non_mock_object
      @options[:stubbing_method_on_non_mock_object]
    end

    # Configure whether stubbing of non-existent methods is allowed.
    #
    # This is useful if you want to ensure that methods you're mocking really exist. A common criticism of unit tests with mock objects is that such a test may (incorrectly) pass when an equivalent non-mocking test would (correctly) fail. While you should always have some integration tests, particularly for critical business functionality, this Mocha configuration setting should catch scenarios when mocked methods and real methods have become misaligned.
    #
    # When +value+ is +:allow+, do nothing. This is the default.
    # When +value+ is +:warn+, display a warning.
    # When +value+ is +:prevent+, raise a {StubbingError}.
    #
    # @param [Symbol] value one of +:allow+, +:warn+, +:prevent+.
    #
    # @example Preventing stubbing of a non-existent method
    #
    #   Mocha.configure do |c|
    #     c.stubbing_non_existent_method = :prevent
    #   end
    #
    #   class Example
    #   end
    #
    #   example = Example.new
    #   example.stubs(:method_that_doesnt_exist)
    #   # => Mocha::StubbingError: stubbing non-existent method:
    #   # =>   #<Example:0x593760>.method_that_doesnt_exist
    #
    def stubbing_non_existent_method=(value)
      @options[:stubbing_non_existent_method] = value
    end

    # @private
    def stubbing_non_existent_method
      @options[:stubbing_non_existent_method]
    end

    # Configure whether stubbing of non-public methods is allowed.
    #
    # Many people think that it's good practice only to mock public methods. This is one way to prevent your tests being too tightly coupled to the internal implementation of a class. Such tests tend to be very brittle and not much use when refactoring.
    #
    # When +value+ is +:allow+, do nothing. This is the default.
    # When +value+ is +:warn+, display a warning.
    # When +value+ is +:prevent+, raise a {StubbingError}.
    #
    # @param [Symbol] value one of +:allow+, +:warn+, +:prevent+.
    #
    # @example Preventing stubbing of a non-public method
    #   Mocha.configure do |c|
    #     c.stubbing_non_public_method = :prevent
    #   end
    #
    #   class Example
    #     def internal_method; end
    #     private :internal_method
    #   end
    #
    #   example = Example.new
    #   example.stubs(:internal_method)
    #   # => Mocha::StubbingError: stubbing non-public method:
    #   # =>   #<Example:0x593530>.internal_method
    #
    def stubbing_non_public_method=(value)
      @options[:stubbing_non_public_method] = value
    end

    # @private
    def stubbing_non_public_method
      @options[:stubbing_non_public_method]
    end

    # Configure whether stubbing methods on the +nil+ object is allowed.
    #
    # This is usually done accidentally, but there might be rare cases where it is intended.
    #
    # This option only works for Ruby < v2.2.0. In later versions of Ruby +nil+ is frozen and so a {StubbingError} will be raised if you attempt to stub a method on +nil+.
    #
    # When +value+ is +:allow+, do nothing.
    # When +value+ is +:warn+, display a warning.
    # When +value+ is +:prevent+, raise a {StubbingError}. This is the default.
    #
    # @param [Symbol] value one of +:allow+, +:warn+, +:prevent+.
    # @deprecated This method is deprecated and will be removed in a future release. +nil+ is frozen in Ruby >= v2.2 and Mocha will be dropping support for Ruby v2.1. At that point it won't be possible to stub methods on +nil+ any more.
    #
    def stubbing_method_on_nil=(value)
      Deprecation.warning([
        '`Mocha::Configuration#stubbing_method_on_nil=` is deprecated and will be removed in a future release.',
        '`nil` is frozen in Ruby >= v2.2 and Mocha will be dropping support for Ruby v2.1.',
        "At that point it won't be possible to stub methods on `nil` any more."
      ].join(' '))
      @options[:stubbing_method_on_nil] = value
    end

    # @private
    def stubbing_method_on_nil
      @options[:stubbing_method_on_nil]
    end

    # Display matching invocations alongside expectations on Mocha-related test failure.
    #
    # @param [Boolean] value +true+ to enable display of matching invocations; disabled by default.
    #
    # @example Enable display of matching invocations
    #   Mocha.configure do |c|
    #     c.display_matching_invocations_on_failure = true
    #   end
    #
    #   foo = mock('foo')
    #   foo.expects(:bar)
    #   foo.stubs(:baz).returns('baz').raises(RuntimeError).throws(:tag, 'value')
    #
    #   foo.baz(1, 2)
    #   assert_raises(RuntimeError) { foo.baz(3, 4) }
    #   assert_throws(:tag) { foo.baz(5, 6) }
    #
    #   not all expectations were satisfied
    #   unsatisfied expectations:
    #   - expected exactly once, invoked never: #<Mock:foo>.bar
    #   satisfied expectations:
    #   - allowed any number of times, invoked 3 times: #<Mock:foo>.baz(any_parameters)
    #     - #<Mock:foo>.baz(1, 2) # => "baz"
    #     - #<Mock:foo>.baz(3, 4) # => raised RuntimeError
    #     - #<Mock:foo>.baz(5, 6) # => threw (:tag, "value")
    def display_matching_invocations_on_failure=(value)
      @options[:display_matching_invocations_on_failure] = value
    end

    # @private
    def display_matching_invocations_on_failure?
      @options[:display_matching_invocations_on_failure]
    end

    # Perform strict keyword argument comparison. Only supported in Ruby >= v2.7.
    #
    # When this option is set to +false+ a positional +Hash+ and a set of keyword arguments are treated the same during comparison, which can lead to misleading passing tests in Ruby >= v3.0 (see examples below). However, a deprecation warning will be displayed if a positional +Hash+ matches a set of keyword arguments or vice versa. This is because {#strict_keyword_argument_matching=} will default to +true+ in the future.
    #
    # For more details on keyword arguments in Ruby v3, refer to {https://www.ruby-lang.org/en/news/2019/12/12/separation-of-positional-and-keyword-arguments-in-ruby-3-0 this article}.
    #
    # Note that +Hash+-related matchers such as {ParameterMatchers::Methods#has_value} or {ParameterMatchers::Methods#has_key} will still treat a positional +Hash+ and a set of keyword arguments the same, so misleading passing tests are still possible when they are used.
    #
    # This configuration option is +false+ by default to enable gradual adoption, but will be +true+ by default in the future.
    #
    # @param [Boolean] value +true+ to enable strict keyword argument matching; +false+ by default.
    #
    # @example Loose keyword argument matching (default)
    #
    #   class Example
    #     def foo(a, bar:); end
    #   end
    #
    #   example = Example.new
    #   example.expects(:foo).with('a', bar: 'b')
    #   example.foo('a', { bar: 'b' })
    #   # This passes the test, but would result in an ArgumentError in practice
    #
    # @example Strict keyword argument matching
    #
    #   Mocha.configure do |c|
    #     c.strict_keyword_argument_matching = true
    #   end
    #
    #   class Example
    #     def foo(a, bar:); end
    #   end
    #
    #   example = Example.new
    #   example.expects(:foo).with('a', bar: 'b')
    #   example.foo('a', { bar: 'b' })
    #   # This now fails as expected
    def strict_keyword_argument_matching=(value)
      raise 'Strict keyword argument matching requires Ruby 2.7 and above.' unless Mocha::RUBY_V27_PLUS
      @options[:strict_keyword_argument_matching] = value
    end

    # @private
    def strict_keyword_argument_matching?
      @options[:strict_keyword_argument_matching]
    end

    class << self
      # @private
      def reset_configuration
        @configuration = nil
      end

      # Temporarily modify {Configuration} options.
      #
      # The supplied +temporary_options+ will override the current configuration for the duration of the supplied block.
      # The configuration will be returned to its original state when the block returns.
      #
      # @param [Hash] temporary_options the configuration options to apply for the duration of the block.
      # @yield block during which the configuration change will be in force.
      #
      # @example Temporarily allow stubbing of +nil+
      #   Mocha::Configuration.override(stubbing_method_on_nil: :allow) do
      #     nil.stubs(:foo)
      #   end
      def override(temporary_options)
        original_configuration = configuration
        @configuration = configuration.merge(new(temporary_options))
        yield
      ensure
        @configuration = original_configuration
      end

      # @private
      def configuration
        @configuration ||= new
      end

      private

      # @private
      def change_config(action, new_value, &block)
        if block_given?
          temporarily_change_config action, new_value, &block
        else
          configuration.send("#{action}=".to_sym, new_value)
        end
      end

      # @private
      def temporarily_change_config(action, new_value)
        original_configuration = configuration
        new_configuration = configuration.dup
        new_configuration.send("#{action}=".to_sym, new_value)
        @configuration = new_configuration
        yield
      ensure
        @configuration = original_configuration
      end
    end
  end
end
