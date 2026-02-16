require 'mocha/single_return_value'

module Mocha
  class ReturnValues
    def self.build(*values)
      new(*values.map { |value| SingleReturnValue.new(value) })
    end

    attr_accessor :values

    def initialize(*values)
      @values = values
    end

    def next(invocation)
      case @values.length
      when 0 then nil
      when 1 then @values.first.evaluate(invocation)
      else @values.shift.evaluate(invocation)
      end
    end

    def +(other)
      self.class.new(*(@values + other.values))
    end
  end
end
