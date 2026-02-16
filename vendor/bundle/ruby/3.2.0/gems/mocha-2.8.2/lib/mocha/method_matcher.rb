module Mocha
  class MethodMatcher
    attr_reader :expected_method_name

    def initialize(expected_method_name)
      @expected_method_name = expected_method_name
    end

    def match?(actual_method_name)
      @expected_method_name == actual_method_name.to_sym
    end

    def mocha_inspect
      @expected_method_name.to_s
    end
  end
end
