require 'ruby2_keywords'
require 'mocha/ruby_version'

module Mocha
  class StubbedMethod
    PrependedModule = Class.new(Module)

    attr_reader :stubbee, :method_name

    def initialize(stubbee, method_name)
      @stubbee = stubbee
      @original_method = nil
      @original_visibility = nil
      @method_name = method_name.to_sym
    end

    def stub
      hide_original_method
      define_new_method
    end

    def unstub
      remove_new_method
      mock.unstub(method_name.to_sym)
      return if mock.any_expectations?
      reset_mocha
    end

    def mock
      mock_owner.mocha
    end

    def reset_mocha
      mock_owner.reset_mocha
    end

    def hide_original_method
      return unless original_method_owner.__method_exists__?(method_name)
      store_original_method_visibility
      use_prepended_module_for_stub_method
    end

    def define_new_method
      self_in_scope = self
      method_name_in_scope = method_name
      stub_method_owner.send(:define_method, method_name) do |*args, &block|
        self_in_scope.mock.handle_method_call(method_name_in_scope, args, block)
      end
      stub_method_owner.send(:ruby2_keywords, method_name)
      retain_original_visibility(stub_method_owner)
    end

    def remove_new_method
      stub_method_owner.send(:remove_method, method_name)
    end

    def matches?(other)
      return false unless other.class == self.class
      (stubbee.object_id == other.stubbee.object_id) && (method_name == other.method_name)
    end

    alias_method :==, :eql?

    def to_s
      "#{stubbee}.#{method_name}"
    end

    private

    def retain_original_visibility(method_owner)
      return unless @original_visibility
      Module.instance_method(@original_visibility).bind(method_owner).call(method_name)
    end

    def store_original_method_visibility
      @original_visibility = original_method_owner.__method_visibility__(method_name)
    end

    def use_prepended_module_for_stub_method
      @stub_method_owner = PrependedModule.new
      original_method_owner.__send__ :prepend, @stub_method_owner
    end

    def stub_method_owner
      @stub_method_owner ||= original_method_owner
    end
  end
end
