class NumberWithUncertainty
  def initialize(number, error)
    @num = number
    @err = error.abs
  end
  attr_reader :num, :err

  def +(other)
    if other.kind_of?(self.class)
      self.class.new(num + other.num, Math::hypot(err, other.err))
    else
      self.class.new(num + other, err)
    end
  end

  def -(other)
    if other.kind_of?(self.class)
      self.class.new(num - other.num, Math::hypot(err, other.err))
    else
      self.class.new(num - other, err)
    end
  end

  def *(other)
    if other.kind_of?(self.class)
      prod = num * other.num
      e = Math::hypot((prod * err / num), (prod * other.err / other.num))
      self.class.new(prod, e)
    else
      self.class.new(num * other, (err * other).abs)
    end
  end

  def /(other)
    if other.kind_of?(self.class)
      quo = num / other.num
      e = Math::hypot((quo * err / num), (quo * other.err / other.num))
      self.class.new(quo, e)
    else
      self.class.new(num / other, (err * other).abs)
    end
  end

  def **(exponent)
    Float(exponent) rescue raise ArgumentError, "not an number: #{exponent}"
    prod = num ** exponent
    self.class.new(prod, (prod * exponent * err / num).abs)
  end

  def sqrt
    self ** 0.5
  end

  def to_s
    "#{num} \u00b1 #{err}"
  end
end

x1 = NumberWithUncertainty.new(100, 1.1)
y1 = NumberWithUncertainty.new( 50, 1.2)
x2 = NumberWithUncertainty.new(200, 2.2)
y2 = NumberWithUncertainty.new(100, 2.3)

puts ((x1 - x2) ** 2 + (y1 - y2) ** 2).sqrt
