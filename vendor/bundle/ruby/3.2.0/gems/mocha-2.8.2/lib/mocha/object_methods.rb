require 'mocha/mockery'
require 'mocha/instance_method'
require 'mocha/argument_iterator'
require 'mocha/expectation_error_factory'

module Mocha
  # Methods added to all objects to allow mocking and stubbing on real (i.e. non-mock) objects.
  #
  # Both {#expects} and {#stubs} return an {Expectation} which can be further modified by methods on {Expectation}.
  module ObjectMethods
    # @private
    alias_method :_method, :method

    # @private
    def mocha(instantiate = true)
      if instantiate
        @mocha ||= Mocha::Mockery.instance.mock_impersonating(self)
      else
        defined?(@mocha) ? @mocha : nil
      end
    end

    # @private
    def reset_mocha
      @mocha = nil
    end

    # @private
    def stubba_method
      Mocha::InstanceMethod
    end

    # @private
    def stubba_object
      self
    end

    # @private
    def stubba_class
      singleton_class
    end

    # Adds an expectation that the specified method must be called exactly once with any parameters.
    #
    # The original implementation of the method is replaced during the test and then restored at the end of the test. The temporary replacement method has the same visibility as the original method.
    #
    # @return [Expectation] last-built expectation which can be further modified by methods on {Expectation}.
    # @raise [StubbingError] if attempting to stub method which is not allowed.
    #
    # @overload def expects(method_name)
    #   @param [Symbol,String] method_name name of expected method
    # @overload def expects(expected_methods_vs_return_values)
    #   @param [Hash] expected_methods_vs_return_values expected method name symbols as keys and corresponding return values as values - these expectations are setup as if {#expects} were called multiple times.
    #
    # @example Setting up an expectation on a non-mock object.
    #   product = Product.new
    #   product.expects(:save).returns(true)
    #   assert_equal true, product.save
    #
    # @example Setting up multiple expectations on a non-mock object.
    #   product = Product.new
    #   product.expects(valid?: true, save: true)
    #
    #   # exactly equivalent to
    #
    #   product = Product.new
    #   product.expects(:valid?).returns(true)
    #   product.expects(:save).returns(true)
    #
    # @see Mock#expects
    def expects(expected_methods_vs_return_values)
      if expected_methods_vs_return_values.to_s =~ /the[^a-z]*spanish[^a-z]*inquisition/i
        raise ExpectationErrorFactory.build('NOBODY EXPECTS THE SPANISH INQUISITION!')
      end
      if frozen?
        raise StubbingError.new("can't stub method on frozen object: #{mocha_inspect}", caller)
      end
      expectation = nil
      mockery = Mocha::Mockery.instance
      iterator = ArgumentIterator.new(expected_methods_vs_return_values)
      iterator.each do |*args|
        method_name = args.shift
        mockery.on_stubbing(self, method_name)
        method = stubba_method.new(stubba_object, method_name)
        mockery.stubba.stub(method)
        expectation = mocha.expects(method_name, caller)
        expectation.returns(args.shift) unless args.empty?
      end
      expectation
    end

    # Adds an expectation that the specified method may be called any number of times with any parameters.
    #
    # The original implementation of the method is replaced during the test and then restored at the end of the test. The temporary replacement method has the same visibility as the original method.
    #
    # @return [Expectation] last-built expectation which can be further modified by methods on {Expectation}.
    # @raise [StubbingError] if attempting to stub method which is not allowed.
    #
    # @overload def stubs(method_name)
    #   @param [Symbol,String] method_name name of stubbed method
    # @overload def stubs(stubbed_methods_vs_return_values)
    #   @param [Hash] stubbed_methods_vs_return_values stubbed method name symbols as keys and corresponding return values as values - these stubbed methods are setup as if {#stubs} were called multiple times.
    #
    # @example Setting up a stubbed methods on a non-mock object.
    #   product = Product.new
    #   product.stubs(:save).returns(true)
    #   assert_equal true, product.save
    #
    # @example Setting up multiple stubbed methods on a non-mock object.
    #   product = Product.new
    #   product.stubs(valid?: true, save: true)
    #
    #   # exactly equivalent to
    #
    #   product = Product.new
    #   product.stubs(:valid?).returns(true)
    #   product.stubs(:save).returns(true)
    #
    # @see Mock#stubs
    def stubs(stubbed_methods_vs_return_values)
      if frozen?
        raise StubbingError.new("can't stub method on frozen object: #{mocha_inspect}", caller)
      end
      expectation = nil
      mockery = Mocha::Mockery.instance
      iterator = ArgumentIterator.new(stubbed_methods_vs_return_values)
      iterator.each do |*args|
        method_name = args.shift
        mockery.on_stubbing(self, method_name)
        method = stubba_method.new(stubba_object, method_name)
        mockery.stubba.stub(method)
        expectation = mocha.stubs(method_name, caller)
        expectation.returns(args.shift) unless args.empty?
      end
      expectation
    end

    # Removes the specified stubbed methods (added by calls to {#expects} or {#stubs}) and all expectations associated with them.
    #
    # Restores the original behaviour of the methods before they were stubbed. This is normally done automatically at the end of each test, but in some circumstances you may want to do it *before* the end of the test.
    #
    # WARNING: If you {#unstub} a method which still has unsatisfied expectations, you may be removing the only way those expectations can be satisfied. Use {#unstub} with care.
    #
    # @param [Array<Symbol>] method_names names of methods to unstub.
    #
    # @example Stubbing and unstubbing a method on a real (non-mock) object.
    #   multiplier = Multiplier.new
    #   multiplier.double(2) # => 4
    #   multiplier.stubs(:double).raises # new behaviour defined
    #   multiplier.double(2) # => raises exception
    #   multiplier.unstub(:double) # original behaviour restored
    #   multiplier.double(2) # => 4
    #
    # @example Unstubbing multiple methods on a real (non-mock) object.
    #   multiplier.unstub(:double, :triple)
    #
    #   # exactly equivalent to
    #
    #   multiplier.unstub(:double)
    #   multiplier.unstub(:triple)
    def unstub(*method_names)
      mockery = Mocha::Mockery.instance
      method_names.each do |method_name|
        method = stubba_method.new(stubba_object, method_name)
        mockery.stubba.unstub(method)
      end
    end
  end
end
