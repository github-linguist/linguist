require 'mocha/ruby_version'
require 'mocha/parameter_matchers'
require 'mocha/hooks'
require 'mocha/mockery'
require 'mocha/sequence'
require 'mocha/object_methods'
require 'mocha/class_methods'

module Mocha
  # Methods added to +Test::Unit::TestCase+, +Minitest::Unit::TestCase+ or equivalent.
  # The mock creation methods are {#mock}, {#stub} and {#stub_everything}, all of which return a #{Mock}
  # which can be further modified by {Mock#responds_like} and {Mock#responds_like_instance_of} methods,
  # both of which return a {Mock}, too, and can therefore, be chained to the original creation methods.
  #
  # {Mock#responds_like} and {Mock#responds_like_instance_of} force the mock to indicate what it is
  # supposed to be mocking, thus making it a safer verifying mock. They check that the underlying +responder+
  # will actually respond to the methods being stubbed, throwing a +NoMethodError+ upon invocation otherwise.
  #
  # @example Verifying mock using {Mock#responds_like_instance_of}
  #   class Sheep
  #     def initialize
  #       raise "some awkward code we don't want to call"
  #     end
  #     def chew(grass); end
  #   end
  #
  #   sheep = mock('sheep').responds_like_instance_of(Sheep)
  #   sheep.expects(:chew)
  #   sheep.expects(:foo)
  #   sheep.respond_to?(:chew) # => true
  #   sheep.respond_to?(:foo) # => false
  #   sheep.chew
  #   sheep.foo # => raises NoMethodError exception
  module API
    include ParameterMatchers::Methods
    include Hooks

    # @private
    def self.included(_mod)
      Object.send(:include, Mocha::ObjectMethods)
      Class.send(:include, Mocha::ClassMethods)
    end

    # @private
    def self.extended(mod)
      included(mod)
    end

    # Builds a new mock object
    #
    # @return [Mock] a new mock object
    #
    # @overload def mock(name)
    #   @param [String, Symbol] name identifies mock object in error messages.
    # @overload def mock(expected_methods_vs_return_values = {})
    #   @param [Hash] expected_methods_vs_return_values expected method name symbols as keys and corresponding return values as values - these expectations are setup as if {Mock#expects} were called multiple times.
    # @overload def mock(name, expected_methods_vs_return_values = {})
    #   @param [String, Symbol] name identifies mock object in error messages.
    #   @param [Hash] expected_methods_vs_return_values expected method name symbols as keys and corresponding return values as values - these expectations are setup as if {Mock#expects} were called multiple times.
    #
    # @example Using expected_methods_vs_return_values Hash to setup expectations.
    #   def test_motor_starts_and_stops
    #     motor = mock('motor', start: true, stop: true)
    #     assert motor.start
    #     assert motor.stop
    #     # an error will be raised unless both Motor#start and Motor#stop have been called
    #   end
    #
    def mock(*arguments)
      name = arguments.shift.to_s if arguments.first.is_a?(String) || arguments.first.is_a?(Symbol)
      expectations = arguments.shift || {}
      mock = name ? Mockery.instance.named_mock(name) : Mockery.instance.unnamed_mock
      mock.expects(expectations)
      mock
    end

    # Builds a new mock object
    #
    # @return [Mock] a new mock object
    #
    # @overload def stub(name)
    #   @param [String, Symbol] name identifies mock object in error messages.
    # @overload def stub(stubbed_methods_vs_return_values = {})
    #   @param [Hash] stubbed_methods_vs_return_values stubbed method name symbols as keys and corresponding return values as values - these stubbed methods are setup as if {Mock#stubs} were called multiple times.
    # @overload def stub(name, stubbed_methods_vs_return_values = {})
    #   @param [String, Symbol] name identifies mock object in error messages.
    #   @param [Hash] stubbed_methods_vs_return_values stubbed method name symbols as keys and corresponding return values as values - these stubbed methods are setup as if {Mock#stubs} were called multiple times.
    #
    # @example Using stubbed_methods_vs_return_values Hash to setup stubbed methods.
    #   def test_motor_starts_and_stops
    #     motor = stub('motor', start: true, stop: true)
    #     assert motor.start
    #     assert motor.stop
    #     # an error will not be raised even if either Motor#start or Motor#stop has not been called
    #   end
    def stub(*arguments)
      name = arguments.shift.to_s if arguments.first.is_a?(String) || arguments.first.is_a?(Symbol)
      expectations = arguments.shift || {}
      stub = name ? Mockery.instance.named_mock(name) : Mockery.instance.unnamed_mock
      stub.stubs(expectations)
      stub
    end

    # Builds a mock object that accepts calls to any method. By default it will return +nil+ for any method call.
    #
    # @return [Mock] a new mock object
    #
    # @overload def stub_everything(name)
    #   @param [String, Symbol] name identifies mock object in error messages.
    # @overload def stub_everything(stubbed_methods_vs_return_values = {})
    #   @param [Hash] stubbed_methods_vs_return_values stubbed method name symbols as keys and corresponding return values as values - these stubbed methods are setup as if {Mock#stubs} were called multiple times.
    # @overload def stub_everything(name, stubbed_methods_vs_return_values = {})
    #   @param [String, Symbol] name identifies mock object in error messages.
    #   @param [Hash] stubbed_methods_vs_return_values stubbed method name symbols as keys and corresponding return values as values - these stubbed methods are setup as if {Mock#stubs} were called multiple times.
    #
    # @example Ignore invocations of irrelevant methods.
    #   def test_motor_stops
    #     motor = stub_everything('motor', stop: true)
    #     assert_nil motor.irrelevant_method_1 # => no error raised
    #     assert_nil motor.irrelevant_method_2 # => no error raised
    #     assert motor.stop
    #   end
    def stub_everything(*arguments)
      name = arguments.shift if arguments.first.is_a?(String) || arguments.first.is_a?(Symbol)
      expectations = arguments.shift || {}
      stub = name ? Mockery.instance.named_mock(name) : Mockery.instance.unnamed_mock
      stub.stub_everything
      stub.stubs(expectations)
      stub
    end

    # Builds a new sequence which can be used to constrain the order in which expectations can occur.
    #
    # Specify that an expected invocation must occur within a named {Sequence} by calling {Expectation#in_sequence}
    # on each expectation or by passing a block within which all expectations should be constrained by the {Sequence}.
    #
    # @param [String] name name of sequence
    # @yield optional block within which expectations should be constrained by the sequence
    # @return [Sequence] a new sequence
    #
    # @see Expectation#in_sequence
    #
    # @example Ensure methods on egg are invoked in correct order.
    #   breakfast = sequence('breakfast')
    #
    #   egg = mock('egg')
    #   egg.expects(:crack).in_sequence(breakfast)
    #   egg.expects(:fry).in_sequence(breakfast)
    #   egg.expects(:eat).in_sequence(breakfast)
    #
    # @example Ensure methods across multiple objects are invoked in correct order.
    #   sequence = sequence(:task_order)
    #
    #   task_one = mock("task_one")
    #   task_two = mock("task_two")
    #
    #   task_one.expects(:execute).in_sequence(sequence)
    #   task_two.expects(:execute).in_sequence(sequence)
    #
    #   task_one.execute
    #   task_two.execute
    #
    # @example Ensure methods on egg are invoked in the correct order using a block.
    #   egg = mock('egg')
    #   sequence('breakfast') do
    #     egg.expects(:crack)
    #     egg.expects(:fry)
    #     egg.expects(:eat)
    #   end
    def sequence(name)
      Sequence.new(name).tap do |seq|
        Mockery.instance.sequences.push(seq)
        begin
          yield if block_given?
        ensure
          Mockery.instance.sequences.pop
        end
      end
    end

    # Builds a new state machine which can be used to constrain the order in which expectations can occur.
    #
    # Specify the initial state of the state machine by using {StateMachine#starts_as}.
    #
    # Specify that an expected invocation should change the state of the state machine by using {Expectation#then}.
    #
    # Specify that an expected invocation should be constrained to occur within a particular +state+ by using {Expectation#when}.
    #
    # A test can contain multiple state machines.
    #
    # @param [String] name name of state machine
    # @return [StateMachine] a new state machine
    #
    # @see Expectation#then
    # @see Expectation#when
    # @see StateMachine
    # @example Constrain expected invocations to occur in particular states.
    #   power = states('power').starts_as('off')
    #
    #   radio = mock('radio')
    #   radio.expects(:switch_on).then(power.is('on'))
    #   radio.expects(:select_channel).with('BBC Radio 4').when(power.is('on'))
    #   radio.expects(:adjust_volume).with(+5).when(power.is('on'))
    #   radio.expects(:select_channel).with('BBC World Service').when(power.is('on'))
    #   radio.expects(:adjust_volume).with(-5).when(power.is('on'))
    #   radio.expects(:switch_off).then(power.is('off'))
    def states(name)
      Mockery.instance.new_state_machine(name)
    end
  end
end
