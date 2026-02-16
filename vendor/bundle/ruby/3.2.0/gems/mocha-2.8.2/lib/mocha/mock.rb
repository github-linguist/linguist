require 'ruby2_keywords'
require 'mocha/expectation'
require 'mocha/expectation_list'
require 'mocha/invocation'
require 'mocha/names'
require 'mocha/receivers'
require 'mocha/method_matcher'
require 'mocha/parameters_matcher'
require 'mocha/argument_iterator'
require 'mocha/expectation_error_factory'
require 'mocha/deprecation'

module Mocha
  # Traditional mock object.
  #
  # {expects} and {stubs} return an {Expectation} which can be further modified
  # by methods on {Expectation}.
  #
  # {responds_like} and {responds_like_instance_of} both return a {Mock}, and
  # can therefore, be chained to the original creation methods in {API}.
  # They force the mock to indicate what it is supposed to be mocking, thus
  # making it a safer verifying mock. They check that the underlying +responder+
  # will actually respond to the methods being stubbed, throwing a
  # +NoMethodError+ upon invocation otherwise.
  #
  # Stubs and expectations are basically the same thing. A stub is just an
  # expectation of zero or more invocations. The {#stubs} method is syntactic
  # sugar to make the intent of the test more explicit.
  #
  # When a method is invoked on a mock object, the mock object searches through
  # its expectations from newest to oldest to find one that matches the
  # invocation. After the invocation, the matching expectation might stop
  # matching further invocations. For example, an +expects(:foo).once+
  # expectation only matches once and will be ignored on future invocations
  # while an +expects(:foo).at_least_once+ expectation will always be matched
  # against invocations.
  #
  # However, note that if the expectation that matches the invocation has a
  # cardinality of "never", then an unexpected invocation error is reported.
  #
  # This scheme allows you to:
  #
  # - Set up default stubs in your the +setup+ method of your test class and
  #   override some of those stubs in individual tests.
  # - Set up different +once+ expectations for the same method with different
  #   action per invocation. However, it's better to use the
  #   {Expectation#returns} method with multiple arguments to do this, as
  #   described below.
  #
  # However, there are some possible "gotchas" caused by this scheme:
  #
  # - if you create an expectation and then a stub for the same method, the
  #   stub will always override the expectation and the expectation will never
  #   be met.
  # - if you create a stub and then an expectation for the same method, the
  #   expectation will match, and when it stops matching the stub will be used
  #   instead, possibly masking test failures.
  # - if you create different expectations for the same method, they will be
  #   invoked in the opposite order than that in which they were specified,
  #   rather than the same order.
  #
  # The best thing to do is not set up multiple expectations and stubs for the
  # same method with exactly the same matchers. Instead, use the
  # {Expectation#returns} method with multiple arguments to create multiple
  # actions for a method. You can also chain multiple calls to
  # {Expectation#returns} and {Expectation#raises} (along with syntactic sugar
  # {Expectation#then} if desired).
  #
  # @example
  #   object = mock()
  #   object.stubs(:expected_method).returns(1, 2).then.raises(Exception)
  #   object.expected_method # => 1
  #   object.expected_method # => 2
  #   object.expected_method # => raises exception of class Exception1
  #
  # If you want to specify more complex ordering or order invocations across
  # different mock objects, use the {Expectation#in_sequence} method to
  # explicitly define a total or partial ordering of invocations.
  class Mock
    # Adds an expectation that the specified method must be called exactly once with any parameters.
    #
    # @return [Expectation] last-built expectation which can be further modified by methods on {Expectation}.
    #
    # @overload def expects(method_name)
    #   @param [Symbol,String] method_name name of expected method
    # @overload def expects(expected_methods_vs_return_values)
    #   @param [Hash] expected_methods_vs_return_values expected method name symbols as keys and corresponding return values as values - these expectations are setup as if {#expects} were called multiple times.
    #
    # @example Expected method invoked once so no error raised
    #   object = mock()
    #   object.expects(:expected_method)
    #   object.expected_method
    #
    # @example Expected method not invoked so error raised
    #   object = mock()
    #   object.expects(:expected_method)
    #   # error raised when test completes, because expected_method not called exactly once
    #
    # @example Expected method invoked twice so error raised
    #   object = mock()
    #   object.expects(:expected_method)
    #   object.expected_method
    #   object.expected_method # => error raised when expected method invoked second time
    #
    # @example Setup multiple expectations using +expected_methods_vs_return_values+.
    #   object = mock()
    #   object.expects(expected_method_one: :result_one, expected_method_two: :result_two)
    #
    #   # is exactly equivalent to
    #
    #   object = mock()
    #   object.expects(:expected_method_one).returns(:result_one)
    #   object.expects(:expected_method_two).returns(:result_two)
    def expects(method_name_or_hash, backtrace = nil)
      expectation = nil
      iterator = ArgumentIterator.new(method_name_or_hash)
      iterator.each do |*args|
        method_name = args.shift
        ensure_method_not_already_defined(method_name)
        expectation = Expectation.new(self, method_name, backtrace)
        expectation.in_sequence(@mockery.sequences.last) if @mockery.sequences.any?
        expectation.returns(args.shift) unless args.empty?
        @expectations.add(expectation)
      end
      expectation
    end

    # Adds an expectation that the specified method may be called any number of times with any parameters.
    #
    # @return [Expectation] last-built expectation which can be further modified by methods on {Expectation}.
    #
    # @overload def stubs(method_name)
    #   @param [Symbol,String] method_name name of stubbed method
    # @overload def stubs(stubbed_methods_vs_return_values)
    #   @param [Hash] stubbed_methods_vs_return_values stubbed method name symbols as keys and corresponding return values as values - these stubbed methods are setup as if {#stubs} were called multiple times.
    #
    # @example No error raised however many times stubbed method is invoked
    #   object = mock()
    #   object.stubs(:stubbed_method)
    #   object.stubbed_method
    #   object.stubbed_method
    #   # no error raised
    #
    # @example Setup multiple expectations using +stubbed_methods_vs_return_values+.
    #   object = mock()
    #   object.stubs(stubbed_method_one: :result_one, stubbed_method_two: :result_two)
    #
    #   # is exactly equivalent to
    #
    #   object = mock()
    #   object.stubs(:stubbed_method_one).returns(:result_one)
    #   object.stubs(:stubbed_method_two).returns(:result_two)
    def stubs(method_name_or_hash, backtrace = nil)
      expectation = nil
      iterator = ArgumentIterator.new(method_name_or_hash)
      iterator.each do |*args|
        method_name = args.shift
        ensure_method_not_already_defined(method_name)
        expectation = Expectation.new(self, method_name, backtrace)
        expectation.at_least(0)
        expectation.in_sequence(@mockery.sequences.last) if @mockery.sequences.any?
        expectation.returns(args.shift) unless args.empty?
        @expectations.add(expectation)
      end
      expectation
    end

    # Removes the specified stubbed methods (added by calls to {#expects} or {#stubs}) and all expectations associated with them.
    #
    # @param [Array<Symbol>] method_names names of methods to unstub.
    #
    # @example Invoking an unstubbed method causes error to be raised
    #   object = mock('mock')
    #   object.stubs(:stubbed_method).returns(:result_one)
    #   object.stubbed_method # => :result_one
    #   object.unstub(:stubbed_method)
    #   object.stubbed_method # => unexpected invocation: #<Mock:mock>.stubbed_method()
    #
    # @example Unstubbing multiple methods.
    #   multiplier.unstub(:double, :triple)
    #
    #   # exactly equivalent to
    #
    #   multiplier.unstub(:double)
    #   multiplier.unstub(:triple)
    def unstub(*method_names)
      method_names.each do |method_name|
        @expectations.remove_all_matching_method(method_name)
      end
    end

    # Constrains the {Mock} instance so that it can only expect or stub methods to which +responder+ responds publicly. The constraint is only applied at method invocation time.
    #
    # A +NoMethodError+ will be raised if the +responder+ does not publicly +#respond_to?+ the invoked method (even if the method has been expected or stubbed).
    #
    # The {Mock} instance will delegate its +#respond_to?+ method to the +responder+. However, the +include_all+ parameter is not passed through, so only public methods on the +responder+ will be considered.
    #
    # Note that the methods on +responder+ are never actually invoked.
    #
    # @param [Object, #respond_to?] responder an object used to determine whether {Mock} instance should +#respond_to?+ to an invocation.
    # @return [Mock] the same {Mock} instance, thereby allowing invocations of other {Mock} methods to be chained.
    # @see #responds_like_instance_of
    #
    # @example Normal mocking
    #   sheep = mock('sheep')
    #   sheep.expects(:chew)
    #   sheep.expects(:foo)
    #   sheep.respond_to?(:chew) # => true
    #   sheep.respond_to?(:foo) # => true
    #   sheep.chew
    #   sheep.foo
    #   # no error raised
    #
    # @example Using {#responds_like} with an instance method
    #   class Sheep
    #     def chew(grass); end
    #   end
    #
    #   sheep = mock('sheep')
    #   sheep.responds_like(Sheep.new)
    #   sheep.expects(:chew)
    #   sheep.expects(:foo)
    #   sheep.respond_to?(:chew) # => true
    #   sheep.respond_to?(:foo) # => false
    #   sheep.chew
    #   sheep.foo # => raises NoMethodError exception
    #
    # @example Using {#responds_like} with a class method
    #   class Sheep
    #     def self.number_of_legs; end
    #   end
    #
    #   sheep_class = mock('sheep_class')
    #   sheep_class.responds_like(Sheep)
    #   sheep_class.stubs(:number_of_legs).returns(4)
    #   sheep_class.expects(:foo)
    #   sheep_class.respond_to?(:number_of_legs) # => true
    #   sheep_class.respond_to?(:foo) # => false
    #   sheep_class.number_of_legs # => 4
    #   sheep_class.foo # => raises NoMethodError exception
    def responds_like(responder)
      @responder = responder
      self
    end

    # Constrains the {Mock} instance so that it can only expect or stub methods to which an instance of the +responder_class+ responds publicly. The constraint is only applied at method invocation time. Note that the responder instance is instantiated using +Class#allocate+.
    #
    # A +NoMethodError+ will be raised if the responder instance does not publicly +#respond_to?+ the invoked method (even if the method has been expected or stubbed).
    #
    # The {Mock} instance will delegate its +#respond_to?+ method to the responder instance. However, the +include_all+ parameter is not passed through, so only public methods on the +responder+  will be considered.
    #
    # Note that the methods on the responder instance are never actually invoked.
    #
    # @param [Class] responder_class a class used to determine whether {Mock} instance should +#respond_to?+ to an invocation.
    # @return [Mock] the same {Mock} instance, thereby allowing invocations of other {Mock} methods to be chained.
    # @see #responds_like
    #
    # @example Using {#responds_like_instance_of}
    #   class Sheep
    #     def initialize
    #       raise "some awkward code we don't want to call"
    #     end
    #     def chew(grass); end
    #   end
    #
    #   sheep = mock('sheep')
    #   sheep.responds_like_instance_of(Sheep)
    #   sheep.expects(:chew)
    #   sheep.expects(:foo)
    #   sheep.respond_to?(:chew) # => true
    #   sheep.respond_to?(:foo) # => false
    #   sheep.chew
    #   sheep.foo # => raises NoMethodError exception
    def responds_like_instance_of(responder_class)
      responds_like(responder_class.allocate)
    end

    # @private
    def initialize(mockery, name = nil, receiver = nil)
      @mockery = mockery
      @name = name || DefaultName.new(self)
      @receiver = receiver || DefaultReceiver.new(self)
      @expectations = ExpectationList.new
      @everything_stubbed = false
      @responder = nil
      @unexpected_invocation = nil
      @expired = false
    end

    # @private
    attr_reader :everything_stubbed

    alias_method :__expects__, :expects

    alias_method :__stubs__, :stubs

    alias_method :__singleton_class__, :singleton_class

    alias_method :quacks_like, :responds_like
    alias_method :quacks_like_instance_of, :responds_like_instance_of

    # @private
    def __expectations__
      @expectations
    end

    # @private
    def stub_everything
      @everything_stubbed = true
    end

    # @private
    def all_expectations
      @receiver.mocks.inject(ExpectationList.new) { |e, m| e + m.__expectations__ }
    end

    # @private
    def method_missing(symbol, *arguments, &block) # rubocop:disable Style/MethodMissingSuper
      handle_method_call(symbol, arguments, block)
    end
    ruby2_keywords(:method_missing)

    # @private
    def handle_method_call(symbol, arguments, block) # rubocop:disable Metrics/CyclomaticComplexity, Metrics/PerceivedComplexity
      check_expiry
      check_responder_responds_to(symbol)
      invocation = Invocation.new(self, symbol, arguments, block)

      matching_expectations = all_expectations.matching_expectations(invocation)

      index = 0
      while index < matching_expectations.length
        matching_expectation = matching_expectations[index]
        if matching_expectation.invocations_never_allowed?
          raise_unexpected_invocation_error(invocation, matching_expectation)
        elsif matching_expectation.invocations_allowed?
          return matching_expectation.invoke(invocation)
        end
        index += 1
      end

      matching_expectation_ignoring_order = all_expectations.match(invocation, ignoring_order: true)
      if matching_expectation_ignoring_order || (!matching_expectation_ignoring_order && !@everything_stubbed) # rubocop:disable Style/GuardClause
        raise_unexpected_invocation_error(invocation, matching_expectation_ignoring_order)
      end
    end

    # @private
    def respond_to_missing?(symbol, _include_all)
      if @responder
        @responder.respond_to?(symbol)
      else
        @everything_stubbed || all_expectations.matches_method?(symbol)
      end
    end

    # @private
    def __verified__?(assertion_counter = nil)
      @expectations.verified?(assertion_counter)
    end

    # @private
    def __expire__(origin)
      @expired = origin || true
    end

    # @private
    def mocha_inspect
      @name.mocha_inspect
    end

    # @private
    def inspect
      mocha_inspect
    end

    # @private
    def ensure_method_not_already_defined(method_name)
      __singleton_class__.send(:undef_method, method_name) if __singleton_class__.method_defined?(method_name) || __singleton_class__.private_method_defined?(method_name)
    end

    # @private
    def any_expectations?
      @expectations.any?
    end

    private

    def raise_unexpected_invocation_error(invocation, matching_expectation)
      if @unexpected_invocation.nil?
        @unexpected_invocation = invocation
        matching_expectation.invoke(invocation) if matching_expectation
        call_description = @unexpected_invocation.call_description
        if matching_expectation && !matching_expectation.in_correct_order?
          call_description += ' invoked out of order'
        end
        message = "#{call_description}\n#{@mockery.mocha_inspect}"
      else
        message = @unexpected_invocation.short_call_description
      end
      raise ExpectationErrorFactory.build("unexpected invocation: #{message}", caller)
    end

    def check_responder_responds_to(symbol)
      if @responder && !@responder.respond_to?(symbol) # rubocop:disable Style/GuardClause
        raise NoMethodError, "undefined method `#{symbol}' for #{mocha_inspect} which responds like #{@responder.mocha_inspect}"
      end
    end

    def check_expiry
      return unless @expired

      origin = @expired == true ? 'one test' : @expired

      sentences = [
        "#{mocha_inspect} was instantiated in #{origin} but it is receiving invocations within another test.",
        'This can lead to unintended interactions between tests and hence unexpected test failures.',
        'Ensure that every test correctly cleans up any state that it introduces.'
      ]
      raise StubbingError.new(sentences.join(' '), caller)
    end
  end
end
