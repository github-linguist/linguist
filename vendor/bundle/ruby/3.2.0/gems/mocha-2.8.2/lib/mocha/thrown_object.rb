module Mocha
  class ThrownObject
    def initialize(tag, value = nil)
      @tag = tag
      @value = value
    end

    def mocha_inspect
      "threw (#{@tag.mocha_inspect}, #{@value.mocha_inspect})"
    end
  end
end
