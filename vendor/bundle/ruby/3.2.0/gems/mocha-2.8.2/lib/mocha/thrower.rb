module Mocha
  class Thrower
    def initialize(tag, object = nil)
      @tag = tag
      @object = object
    end

    def evaluate(invocation)
      invocation.threw(@tag, @object)
      throw @tag, @object
    end
  end
end
