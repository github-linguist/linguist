require 'test/unit'
include Test::Unit::Assertions

class MyInt
  @@min = 1
  @@max = 10

  attr_reader :value
  private :value

  def initialize(val)
    begin
      v = Integer(val)
    rescue ArgumentError
      raise ArgumentError, "invalid value '#{val}', must be an integer"
    end

    unless v.between?(@@min, @@max)
      raise ArgumentError, "invalid value '#{v}', must be between #{@@min} and #{@@max}"
    end

    @value = v
  end

  def method_missing(m, *args)
    super unless @value.respond_to?(m)
    myint_args = args.collect do |arg|
      arg.kind_of?(self.class) ? arg.to_int : arg
    end
    result = @value.send(m, *myint_args)
    return result if m == :coerce
    case result
    when Integer
      MyInt.new(result)
    when Array
      result.collect do |element|
        element.kind_of?(Integer) ? MyInt.new(element) : element
      end
    else
      result
    end
  end

  def respond_to?(method)
    super or @value.respond_to? method
  end

  def to_int
    @value
  end
  def to_f
    Float(@value)
  end
  def to_s
    @value.to_s
  end
  def inspect
    to_s
  end
end


assert_raise(ArgumentError) { MyInt.new("foo") }    # => invalid value 'foo', must be an integer
assert_raise(ArgumentError) { MyInt.new(11) }       # => invalid value '11', must be an integer

a = MyInt.new(7)
b = MyInt.new(5)

c = 5 + a
assert_kind_of(Fixnum, c)
assert_equal(12, c)

c = a + 2
assert_kind_of(MyInt, c)
assert_equal(9, c.to_int)

c = a + 2.8
assert_kind_of(Float, c)
assert_equal(9.8, c)

c = a - b
assert_kind_of(MyInt, c)
assert_equal(2, c.to_int)

assert_raise(ArgumentError) { c = a + b }    # => invalid value '12', must be an integer
assert_raise(ArgumentError) { c = b - a }    # => invalid value '-2', must be an integer
