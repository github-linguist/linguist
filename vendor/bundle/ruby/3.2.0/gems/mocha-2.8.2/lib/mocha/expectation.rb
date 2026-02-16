require 'ruby2_keywords'
require 'mocha/method_matcher'
require 'mocha/parameters_matcher'
require 'mocha/expectation_error'
require 'mocha/return_values'
require 'mocha/exception_raiser'
require 'mocha/thrower'
require 'mocha/yield_parameters'
require 'mocha/is_a'
require 'mocha/in_state_ordering_constraint'
require 'mocha/change_state_side_effect'
require 'mocha/cardinality'
require 'mocha/configuration'
require 'mocha/block_matcher'
require 'mocha/backtrace_filter'

module Mocha
  # Methods on expectations returned from {Mock#expects}, {Mock#stubs}, {ObjectMethods#expects} and {ObjectMethods#stubs}.
  class Expectation
    # Modifies expectation so that the number of calls to the expected method must be within a specific +range+.
    #
    # @param [Range,Integer] range specifies the allowable range in the number of expected invocations.
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @example Specifying a specific number of expected invocations.
    #   object = mock()
    #   object.expects(:expected_method).times(3)
    #   3.times { object.expected_method }
    #   # => verify succeeds
    #
    #   object = mock()
    #   object.expects(:expected_method).times(3)
    #   2.times { object.expected_method }
    #   # => verify fails
    #
    # @example Specifying a range in the number of expected invocations.
    #   object = mock()
    #   object.expects(:expected_method).times(2..4)
    #   3.times { object.expected_method }
    #   # => verify succeeds
    #
    #   object = mock()
    #   object.expects(:expected_method).times(2..4)
    #   object.expected_method
    #   # => verify fails
    def times(range)
      @cardinality.times(range)
      self
    end

    # Modifies expectation so that the expected method must be called exactly twice.
    #
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @example Expected method must be invoked exactly twice.
    #   object = mock()
    #   object.expects(:expected_method).twice
    #   object.expected_method
    #   object.expected_method
    #   # => verify succeeds
    #
    #   object = mock()
    #   object.expects(:expected_method).twice
    #   object.expected_method
    #   object.expected_method
    #   object.expected_method # => unexpected invocation
    #
    #   object = mock()
    #   object.expects(:expected_method).twice
    #   object.expected_method
    #   # => verify fails
    def twice
      @cardinality.exactly(2)
      self
    end

    # Modifies expectation so that the expected method must be called exactly once.
    #
    # Note that this is the default behaviour for an expectation, but you may wish to use it for clarity/emphasis.
    #
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @example Expected method must be invoked exactly once.
    #   object = mock()
    #   object.expects(:expected_method).once
    #   object.expected_method
    #   # => verify succeeds
    #
    #   object = mock()
    #   object.expects(:expected_method).once
    #   object.expected_method
    #   object.expected_method # => unexpected invocation
    #
    #   object = mock()
    #   object.expects(:expected_method).once
    #   # => verify fails
    def once
      @cardinality.exactly(1)
      self
    end

    # Modifies expectation so that the expected method must never be called.
    #
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @example Expected method must never be called.
    #   object = mock()
    #   object.expects(:expected_method).never
    #   object.expected_method # => unexpected invocation
    #
    #   object = mock()
    #   object.expects(:expected_method).never
    #   # => verify succeeds
    def never
      @cardinality.exactly(0)
      self
    end

    # Modifies expectation so that the expected method must be called at least a +minimum_number_of_times+.
    #
    # @param [Integer] minimum_number_of_times minimum number of expected invocations.
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @example Expected method must be called at least twice.
    #   object = mock()
    #   object.expects(:expected_method).at_least(2)
    #   3.times { object.expected_method }
    #   # => verify succeeds
    #
    #   object = mock()
    #   object.expects(:expected_method).at_least(2)
    #   object.expected_method
    #   # => verify fails
    def at_least(minimum_number_of_times)
      @cardinality.at_least(minimum_number_of_times)
      self
    end

    # Modifies expectation so that the expected method must be called at least once.
    #
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @example Expected method must be called at least once.
    #   object = mock()
    #   object.expects(:expected_method).at_least_once
    #   object.expected_method
    #   # => verify succeeds
    #
    #   object = mock()
    #   object.expects(:expected_method).at_least_once
    #   # => verify fails
    def at_least_once
      at_least(1)
    end

    # Modifies expectation so that the expected method must be called at most a +maximum_number_of_times+.
    #
    # @param [Integer] maximum_number_of_times maximum number of expected invocations.
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @example Expected method must be called at most twice.
    #   object = mock()
    #   object.expects(:expected_method).at_most(2)
    #   2.times { object.expected_method }
    #   # => verify succeeds
    #
    #   object = mock()
    #   object.expects(:expected_method).at_most(2)
    #   3.times { object.expected_method } # => unexpected invocation
    def at_most(maximum_number_of_times)
      @cardinality.at_most(maximum_number_of_times)
      self
    end

    # Modifies expectation so that the expected method must be called at most once.
    #
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @example Expected method must be called at most once.
    #   object = mock()
    #   object.expects(:expected_method).at_most_once
    #   object.expected_method
    #   # => verify succeeds
    #
    #   object = mock()
    #   object.expects(:expected_method).at_most_once
    #   2.times { object.expected_method } # => unexpected invocation
    def at_most_once
      at_most(1)
    end

    # Modifies expectation so that the expected method must be called with +expected_parameters_or_matchers+.
    #
    # May be used with Ruby literals or variables for exact matching or with parameter matchers for less-specific matching, e.g. {ParameterMatchers::Methods#includes}, {ParameterMatchers::Methods#has_key}, etc. See {ParameterMatchers} for a list of all available parameter matchers.
    #
    # Alternatively a block argument can be passed to {#with} to implement custom parameter matching. The block receives the +*actual_parameters+ as its arguments and should return +true+ if they are acceptable or +false+ otherwise. See the example below where a method is expected to be called with a value divisible by 4.
    # The block argument takes precedence over +expected_parameters_or_matchers+. The block may be called multiple times per invocation of the expected method and so it should be idempotent.
    #
    # Note that if {#with} is called multiple times on the same expectation, the last call takes precedence; other calls are ignored.
    #
    # Positional arguments were separated from keyword arguments in Ruby v3 (see {https://www.ruby-lang.org/en/news/2019/12/12/separation-of-positional-and-keyword-arguments-in-ruby-3-0 this article}). In relation to this a new configuration option ({Configuration#strict_keyword_argument_matching=}) is available in Ruby >= 2.7.
    #
    # When {Configuration#strict_keyword_argument_matching=} is set to +false+ (which is currently the default), a positional +Hash+ and a set of keyword arguments passed to {#with} are treated the same for the purposes of parameter matching. However, a deprecation warning will be displayed if a positional +Hash+ matches a set of keyword arguments or vice versa. This is because {Configuration#strict_keyword_argument_matching=} will default to +true+ in the future.
    #
    # When {Configuration#strict_keyword_argument_matching=} is set to +true+, an actual positional +Hash+ will not match an expected set of keyword arguments; and vice versa, an actual set of keyword arguments will not match an expected positional +Hash+, i.e. the parameter matching is stricter.
    #
    # @see ParameterMatchers
    # @see Configuration#strict_keyword_argument_matching=
    #
    # @param [Array<Object,ParameterMatchers::BaseMethods>] expected_parameters_or_matchers expected parameter values or parameter matchers.
    # @yield optional block specifying custom matching.
    # @yieldparam [Array<Object>] actual_parameters parameters with which expected method was invoked.
    # @yieldreturn [Boolean] +true+ if +actual_parameters+ are acceptable; +false+ otherwise.
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @example Expected method must be called with exact parameter values.
    #   object = mock()
    #   object.expects(:expected_method).with(:param1, :param2)
    #   object.expected_method(:param1, :param2)
    #   # => verify succeeds
    #
    #   object = mock()
    #   object.expects(:expected_method).with(:param1, :param2)
    #   object.expected_method(:param3)
    #   # => verify fails
    #
    # @example Expected method must be called with parameters matching parameter matchers.
    #   object = mock()
    #   object.expects(:expected_method).with(includes('string2'), anything)
    #   object.expected_method(['string1', 'string2'], 'any-old-value')
    #   # => verify succeeds
    #
    #   object = mock()
    #   object.expects(:expected_method).with(includes('string2'), anything)
    #   object.expected_method(['string1'], 'any-old-value')
    #   # => verify fails
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
    #
    # @example Using a block argument to expect the method to be called with a value divisible by 4.
    #   object = mock()
    #   object.expects(:expected_method).with() { |value| value % 4 == 0 }
    #   object.expected_method(16)
    #   # => verify succeeds
    #
    #   object = mock()
    #   object.expects(:expected_method).with() { |value| value % 4 == 0 }
    #   object.expected_method(17)
    #   # => verify fails
    #
    # @example Extracting a custom matcher into an instance method on the test class.
    #   class MyTest < Minitest::Test
    #     def test_expected_method_is_called_with_a_value_divisible_by_4
    #       object = mock()
    #       object.expects(:expected_method).with(&method(:divisible_by_4))
    #       object.expected_method(16)
    #       # => verify succeeds
    #     end
    #
    #     private
    #
    #     def divisible_by_4(value)
    #       value % 4 == 0
    #     end
    #   end
    def with(*expected_parameters_or_matchers, &matching_block)
      @parameters_matcher = ParametersMatcher.new(expected_parameters_or_matchers, self, &matching_block)
      self
    end
    ruby2_keywords(:with)

    # Modifies expectation so that the expected method must be called with a block.
    #
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @example Expected method must be called with a block.
    #   object = mock()
    #   object.expects(:expected_method).with_block_given
    #   object.expected_method { 1 + 1 }
    #   # => verify succeeds
    #
    #   object = mock()
    #   object.expects(:expected_method).with_block_given
    #   object.expected_method
    #   # => verify fails
    def with_block_given
      @block_matcher = BlockMatchers::BlockGiven.new
      self
    end

    # Modifies expectation so that the expected method must be called without a block.
    #
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @example Expected method must be called without a block.
    #   object = mock()
    #   object.expects(:expected_method).with_no_block_given
    #   object.expected_method
    #   # => verify succeeds
    #
    #   object = mock()
    #   object.expects(:expected_method).with_block_given
    #   object.expected_method { 1 + 1 }
    #   # => verify fails
    def with_no_block_given
      @block_matcher = BlockMatchers::NoBlockGiven.new
      self
    end

    # Modifies expectation so that when the expected method is called, it yields to the block with the specified +parameters+.
    #
    # If no +parameters+ are specified, it yields to the block without any parameters.
    #
    # If no block is provided, the method will still attempt to yield resulting in a +LocalJumpError+. Note that this is what would happen if a "real" (non-mock) method implementation tried to yield to a non-existent block.
    #
    # May be called multiple times on the same expectation for consecutive invocations.
    #
    # @param [*Array] parameters parameters to be yielded.
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    # @see #then
    #
    # @example Yield when expected method is invoked.
    #   benchmark = mock()
    #   benchmark.expects(:measure).yields
    #   yielded = false
    #   benchmark.measure { yielded = true }
    #   yielded # => true
    #
    # @example Yield parameters when expected method is invoked.
    #   fibonacci = mock()
    #   fibonacci.expects(:next_pair).yields(0, 1)
    #   sum = 0
    #   fibonacci.next_pair { |first, second| sum = first + second }
    #   sum # => 1
    #
    # @example Yield different parameters on different invocations of the expected method.
    #   fibonacci = mock()
    #   fibonacci.expects(:next_pair).yields(0, 1).then.yields(1, 1)
    #   sum = 0
    #   fibonacci.next_pair { |first, second| sum = first + second }
    #   sum # => 1
    #   fibonacci.next_pair { |first, second| sum = first + second }
    #   sum # => 2
    def yields(*parameters)
      multiple_yields(parameters)
    end

    # Modifies expectation so that when the expected method is called, it yields multiple times per invocation with the specified +parameter_groups+.
    #
    # If no block is provided, the method will still attempt to yield resulting in a +LocalJumpError+. Note that this is what would happen if a "real" (non-mock) method implementation tried to yield to a non-existent block.
    #
    # @param [*Array<Array>] parameter_groups each element of +parameter_groups+ should iself be an +Array+ representing the parameters to be passed to the block for a single yield. Any element of +parameter_groups+ that is not an +Array+ is wrapped in an +Array+.
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    # @see #then
    #
    # @example When +foreach+ is called, the stub will invoke the block twice, the first time it passes ['row1_col1', 'row1_col2'] as the parameters, and the second time it passes ['row2_col1', ''] as the parameters.
    #   csv = mock()
    #   csv.expects(:foreach).with("path/to/file.csv").multiple_yields(['row1_col1', 'row1_col2'], ['row2_col1', ''])
    #   rows = []
    #   csv.foreach { |row| rows << row }
    #   rows # => [['row1_col1', 'row1_col2'], ['row2_col1', '']]
    #
    # @example Yield different groups of parameters on different invocations of the expected method. Simulating a situation where the CSV file at 'path/to/file.csv' has been modified between the two calls to +foreach+.
    #   csv = mock()
    #   csv.stubs(:foreach).with("path/to/file.csv").multiple_yields(['old_row1_col1', 'old_row1_col2'], ['old_row2_col1', '']).then.multiple_yields(['new_row1_col1', ''], ['new_row2_col1', 'new_row2_col2'])
    #   rows_from_first_invocation = []
    #   rows_from_second_invocation = []
    #   csv.foreach { |row| rows_from_first_invocation << row } # first invocation
    #   csv.foreach { |row| rows_from_second_invocation << row } # second invocation
    #   rows_from_first_invocation # => [['old_row1_col1', 'old_row1_col2'], ['old_row2_col1', '']]
    #   rows_from_second_invocation # => [['new_row1_col1', ''], ['new_row2_col1', 'new_row2_col2']]
    def multiple_yields(*parameter_groups)
      @yield_parameters.add(*parameter_groups)
      self
    end

    # Modifies expectation so that when the expected method is called, it returns the specified +value+.
    #
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    # @see #then
    #
    # @overload def returns(value)
    #   @param [Object] value value to return on invocation of expected method.
    # @overload def returns(*values)
    #   @param [*Array] values values to return on consecutive invocations of expected method.
    #
    # @example Return the same value on every invocation.
    #   object = mock()
    #   object.stubs(:stubbed_method).returns('result')
    #   object.stubbed_method # => 'result'
    #   object.stubbed_method # => 'result'
    #
    # @example Return a different value on consecutive invocations.
    #   object = mock()
    #   object.stubs(:stubbed_method).returns(1, 2)
    #   object.stubbed_method # => 1
    #   object.stubbed_method # => 2
    #
    # @example Alternative way to return a different value on consecutive invocations.
    #   object = mock()
    #   object.stubs(:expected_method).returns(1, 2).then.returns(3)
    #   object.expected_method # => 1
    #   object.expected_method # => 2
    #   object.expected_method # => 3
    #
    # @example May be called in conjunction with {#raises} on the same expectation.
    #   object = mock()
    #   object.stubs(:expected_method).returns(1, 2).then.raises(Exception)
    #   object.expected_method # => 1
    #   object.expected_method # => 2
    #   object.expected_method # => raises exception of class Exception1
    #
    # @example Note that in Ruby a method returning multiple values is exactly equivalent to a method returning an +Array+ of those values.
    #   object = mock()
    #   object.stubs(:expected_method).returns([1, 2])
    #   x, y = object.expected_method
    #   x # => 1
    #   y # => 2
    def returns(*values)
      @return_values += ReturnValues.build(*values)
      self
    end

    # Modifies expectation so that when the expected method is called, it raises the specified +exception+ with the specified +message+ i.e. calls +Kernel#raise(exception, message)+.
    #
    # @param [Class,Exception,String,#exception] exception exception to be raised or message to be passed to RuntimeError.
    # @param [String] message exception message.
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @see Kernel#raise
    # @see #then
    #
    # @overload def raises
    # @overload def raises(exception)
    # @overload def raises(exception, message)
    #
    # @example Raise specified exception if expected method is invoked.
    #   object = stub()
    #   object.stubs(:expected_method).raises(Exception, 'message')
    #   object.expected_method # => raises exception of class Exception and with message 'message'
    #
    # @example Raise custom exception with extra constructor parameters by passing in an instance of the exception.
    #   object = stub()
    #   object.stubs(:expected_method).raises(MyException.new('message', 1, 2, 3))
    #   object.expected_method # => raises the specified instance of MyException
    #
    # @example Raise different exceptions on consecutive invocations of the expected method.
    #   object = stub()
    #   object.stubs(:expected_method).raises(Exception1).then.raises(Exception2)
    #   object.expected_method # => raises exception of class Exception1
    #   object.expected_method # => raises exception of class Exception2
    #
    # @example Raise an exception on first invocation of expected method and then return values on subsequent invocations.
    #   object = stub()
    #   object.stubs(:expected_method).raises(Exception).then.returns(2, 3)
    #   object.expected_method # => raises exception of class Exception1
    #   object.expected_method # => 2
    #   object.expected_method # => 3
    def raises(exception = RuntimeError, message = nil)
      @return_values += ReturnValues.new(ExceptionRaiser.new(exception, message))
      self
    end

    # Modifies expectation so that when the expected method is called, it throws the specified +tag+ with the specific return value +object+ i.e. calls +Kernel#throw(tag, object)+.
    #
    # @param [Symbol,String] tag tag to throw to transfer control to the active catch block.
    # @param [Object] object return value for the catch block.
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @see Kernel#throw
    # @see #then
    #
    # @overload def throw(tag)
    # @overload def throw(tag, object)
    #
    # @example Throw tag when expected method is invoked.
    #   object = stub()
    #   object.stubs(:expected_method).throws(:done)
    #   object.expected_method # => throws tag :done
    #
    # @example Throw tag with return value +object+ c.f. +Kernel#throw+.
    #   object = stub()
    #   object.stubs(:expected_method).throws(:done, 'result')
    #   object.expected_method # => throws tag :done and causes catch block to return 'result'
    #
    # @example Throw different tags on consecutive invocations of the expected method.
    #   object = stub()
    #   object.stubs(:expected_method).throws(:done).then.throws(:continue)
    #   object.expected_method # => throws :done
    #   object.expected_method # => throws :continue
    #
    # @example Throw tag on first invocation of expected method and then return values for subsequent invocations.
    #   object = stub()
    #   object.stubs(:expected_method).throws(:done).then.returns(2, 3)
    #   object.expected_method # => throws :done
    #   object.expected_method # => 2
    #   object.expected_method # => 3
    def throws(tag, object = nil)
      @return_values += ReturnValues.new(Thrower.new(tag, object))
      self
    end

    # @overload def then
    #   Used as syntactic sugar to improve readability. It has no effect on state of the expectation.
    # @overload def then(state)
    #   Used to change the +state_machine+ to the specified state when the expected invocation occurs.
    #   @param [StateMachine::State] state state_machine.is(state_name) provides a mechanism to change the +state_machine+ into the state specified by +state_name+ when the expected method is invoked.
    #
    #   @see API#states
    #   @see StateMachine
    #   @see #when
    #
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @example Using {#then} as syntactic sugar when specifying values to be returned and exceptions to be raised on consecutive invocations of the expected method.
    #   object = mock()
    #   object.stubs(:expected_method).returns(1, 2).then.raises(Exception).then.returns(4)
    #   object.expected_method # => 1
    #   object.expected_method # => 2
    #   object.expected_method # => raises exception of class Exception
    #   object.expected_method # => 4
    #
    # @example Using {#then} to change the +state+ of a +state_machine+ on the invocation of an expected method.
    #   power = states('power').starts_as('off')
    #
    #   radio = mock('radio')
    #   radio.expects(:switch_on).then(power.is('on'))
    #   radio.expects(:select_channel).with('BBC Radio 4').when(power.is('on'))
    #   radio.expects(:adjust_volume).with(+5).when(power.is('on'))
    #   radio.expects(:select_channel).with('BBC World Service').when(power.is('on'))
    #   radio.expects(:adjust_volume).with(-5).when(power.is('on'))
    #   radio.expects(:switch_off).then(power.is('off'))
    def then(state = nil)
      add_side_effect(ChangeStateSideEffect.new(state)) if state
      self
    end

    # Constrains the expectation to occur only when the +state_machine+ is in the state specified by +state_predicate+.
    #
    # @param [StateMachine::StatePredicate] state_predicate +state_machine.is(state_name)+ provides a mechanism to determine whether the +state_machine+ is in the state specified by +state_predicate+ when the expected method is invoked.
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @see API#states
    # @see StateMachine
    # @see #then
    #
    # @example Using {#when} to only allow invocation of methods when "power" state machine is in the "on" state.
    #   power = states('power').starts_as('off')
    #
    #   radio = mock('radio')
    #   radio.expects(:switch_on).then(power.is('on'))
    #   radio.expects(:select_channel).with('BBC Radio 4').when(power.is('on'))
    #   radio.expects(:adjust_volume).with(+5).when(power.is('on'))
    #   radio.expects(:select_channel).with('BBC World Service').when(power.is('on'))
    #   radio.expects(:adjust_volume).with(-5).when(power.is('on'))
    #   radio.expects(:switch_off).then(power.is('off'))
    def when(state_predicate)
      add_ordering_constraint(InStateOrderingConstraint.new(state_predicate))
      self
    end

    # Constrains the expectation so that it must be invoked at the current point in the +sequence+.
    #
    # To expect a sequence of invocations, write the expectations in order and add the +in_sequence(sequence)+ clause to each one.
    #
    # Expectations in a +sequence+ can have any invocation count.
    #
    # If an expectation in a sequence is stubbed, rather than expected, it can be skipped in the +sequence+.
    #
    # An expected method can appear in multiple sequences.
    #
    # @param [Sequence] sequence sequence in which expected method should appear.
    # @param [*Array<Sequence>] sequences more sequences in which expected method should appear.
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @see API#sequence
    #
    # @example Ensure methods are invoked in a specified order.
    #   breakfast = sequence('breakfast')
    #
    #   egg = mock('egg')
    #   egg.expects(:crack).in_sequence(breakfast)
    #   egg.expects(:fry).in_sequence(breakfast)
    #   egg.expects(:eat).in_sequence(breakfast)
    def in_sequence(sequence, *sequences)
      sequences.unshift(sequence).each { |seq| add_in_sequence_ordering_constraint(seq) }
      self
    end

    # @private
    attr_reader :backtrace

    # @private
    def initialize(mock, expected_method_name, backtrace = nil)
      @mock = mock
      @method_matcher = MethodMatcher.new(expected_method_name.to_sym)
      @parameters_matcher = ParametersMatcher.new
      @block_matcher = BlockMatchers::OptionalBlock.new
      @ordering_constraints = []
      @side_effects = []
      @cardinality = Cardinality.new.exactly(1)
      @return_values = ReturnValues.new
      @yield_parameters = YieldParameters.new
      @backtrace = backtrace || caller
    end

    # @private
    def add_ordering_constraint(ordering_constraint)
      @ordering_constraints << ordering_constraint
    end

    # @private
    def add_in_sequence_ordering_constraint(sequence)
      sequence.constrain_as_next_in_sequence(self)
    end

    # @private
    def add_side_effect(side_effect)
      @side_effects << side_effect
    end

    # @private
    def perform_side_effects
      @side_effects.each(&:perform)
    end

    # @private
    def in_correct_order?
      @ordering_constraints.all?(&:allows_invocation_now?)
    end

    # @private
    def ordering_constraints_not_allowing_invocation_now
      @ordering_constraints.reject(&:allows_invocation_now?)
    end

    # @private
    def matches_method?(method_name)
      @method_matcher.match?(method_name)
    end

    # @private
    def match?(invocation, ignoring_order: false)
      order_independent_match = @method_matcher.match?(invocation.method_name) && @parameters_matcher.match?(invocation.arguments) && @block_matcher.match?(invocation.block)
      ignoring_order ? order_independent_match : order_independent_match && in_correct_order?
    end

    # @private
    def invocations_allowed?
      @cardinality.invocations_allowed?
    end

    # @private
    def invocations_never_allowed?
      @cardinality.invocations_never_allowed?
    end

    # @private
    def satisfied?
      @cardinality.satisfied?
    end

    # @private
    def invoke(invocation)
      perform_side_effects
      @cardinality << invocation
      invocation.call(@yield_parameters, @return_values)
    end

    # @private
    def verified?(assertion_counter = nil)
      assertion_counter.increment if assertion_counter && @cardinality.needs_verifying?
      @cardinality.verified?
    end

    # @private
    def used?
      @cardinality.used?
    end

    # @private
    def inspect
      address = __id__ * 2
      address += 0x100000000 if address < 0
      "#<Expectation:0x#{format('%<address>x', address: address)} #{mocha_inspect} >"
    end

    # @private
    def mocha_inspect
      message = "#{@cardinality.anticipated_times}, #{@cardinality.invoked_times}: #{method_signature}"
      message << "; #{@ordering_constraints.map(&:mocha_inspect).join('; ')}" unless @ordering_constraints.empty?
      if Mocha.configuration.display_matching_invocations_on_failure?
        message << @cardinality.actual_invocations
      end
      message
    end

    # @private
    def method_signature
      signature = "#{@mock.mocha_inspect}.#{@method_matcher.mocha_inspect}#{@parameters_matcher.mocha_inspect}"
      signature << " #{@block_matcher.mocha_inspect}" if @block_matcher.mocha_inspect
      signature
    end

    # @private
    def definition_location
      filter = BacktraceFilter.new
      filter.filtered(backtrace)[0]
    end
  end
end
