require 'mocha/is_a'

module Mocha
  class SingleReturnValue
    def initialize(value)
      @value = value
    end

    def evaluate(invocation)
      invocation.returned(@value)
      @value
    end
  end
end
