module Mocha
  class ExpectationList
    def initialize(expectations = [])
      @expectations = expectations
    end

    def add(expectation)
      @expectations.unshift(expectation)
      expectation
    end

    def remove_all_matching_method(method_name)
      @expectations.reject! { |expectation| expectation.matches_method?(method_name) }
    end

    def matches_method?(method_name)
      @expectations.any? { |expectation| expectation.matches_method?(method_name) }
    end

    def match(invocation, ignoring_order: false)
      matching_expectations(invocation, ignoring_order: ignoring_order).first
    end

    def match_allowing_invocation(invocation)
      matching_expectations(invocation).detect(&:invocations_allowed?)
    end

    def match_never_allowing_invocation(invocation)
      matching_expectations(invocation).detect(&:invocations_never_allowed?)
    end

    def verified?(assertion_counter = nil)
      @expectations.all? { |expectation| expectation.verified?(assertion_counter) }
    end

    def to_a
      @expectations
    end

    def to_set
      @expectations.to_set
    end

    def length
      @expectations.length
    end

    def any?
      @expectations.any?
    end

    def +(other)
      self.class.new(to_a + other.to_a)
    end

    def matching_expectations(invocation, ignoring_order: false)
      @expectations.select { |e| e.match?(invocation, ignoring_order: ignoring_order) }
    end
  end
end
