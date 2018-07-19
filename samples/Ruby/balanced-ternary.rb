class BalancedTernary
  include Comparable
  def initialize(str = "")
    if str =~ /[^-+0]+/
      raise ArgumentError, "invalid BalancedTernary number: #{str}"
    end
    @digits = trim0(str)
  end

  I2BT = {0 => ["0",0], 1 => ["+",0], 2 => ["-",1]}
  def self.from_int(value)
    n = value.to_i
    digits = ""
    while n != 0
      quo, rem = n.divmod(3)
      bt, carry = I2BT[rem]
      digits = bt + digits
      n = quo + carry
    end
    new(digits)
  end

  BT2I = {"-" => -1, "0" => 0, "+" => 1}
  def to_int
    @digits.chars.inject(0) do |sum, char|
      sum = 3 * sum + BT2I[char]
    end
  end
  alias :to_i :to_int

  def to_s
    @digits.dup                 # String is mutable
  end
  alias :inspect :to_s

  def <=>(other)
    to_i <=> other.to_i
  end

  ADDITION_TABLE = {
    "---" => ["-","0"], "--0" => ["-","+"], "--+" => ["0","-"],
    "-0-" => ["-","+"], "-00" => ["0","-"], "-0+" => ["0","0"],
    "-+-" => ["0","-"], "-+0" => ["0","0"], "-++" => ["0","+"],
    "0--" => ["-","+"], "0-0" => ["0","-"], "0-+" => ["0","0"],
    "00-" => ["0","-"], "000" => ["0","0"], "00+" => ["0","+"],
    "0+-" => ["0","0"], "0+0" => ["0","+"], "0++" => ["+","-"],
    "+--" => ["0","-"], "+-0" => ["0","0"], "+-+" => ["0","+"],
    "+0-" => ["0","0"], "+00" => ["0","+"], "+0+" => ["+","-"],
    "++-" => ["0","+"], "++0" => ["+","-"], "+++" => ["+","0"],
  }

  def +(other)
    maxl = [to_s.length, other.to_s.length].max
    a = pad0_reverse(to_s, maxl)
    b = pad0_reverse(other.to_s, maxl)
    carry = "0"
    sum = a.zip( b ).inject("") do |sum, (c1, c2)|
      carry, digit = ADDITION_TABLE[carry + c1 + c2]
      sum = digit + sum
    end
    self.class.new(carry + sum)
  end

  MULTIPLICATION_TABLE = {
    "-" => "+0-",
    "0" => "000",
    "+" => "-0+",
  }

  def *(other)
    product = self.class.new
    other.to_s.each_char do |bdigit|
      row = to_s.tr("-0+", MULTIPLICATION_TABLE[bdigit])
      product += self.class.new(row)
      product << 1
    end
    product >> 1
  end

  # negation
  def -@()
    self.class.new(@digits.tr('-+','+-'))
  end

  # subtraction
  def -(other)
    self + (-other)
  end

  # shift left
  def <<(count)
    @digits = trim0(@digits + "0"*count)
    self
  end

  # shift right
  def >>(count)
    @digits[-count..-1] = "" if count > 0
    @digits = trim0(@digits)
    self
  end

  private

  def trim0(str)
    str = str.sub(/^0+/, "")
    str = "0" if str.empty?
    str
  end

  def pad0_reverse(str, len)
    str.rjust(len, "0").reverse.chars
  end
end

a = BalancedTernary.new("+-0++0+")
b = BalancedTernary.from_int(-436)
c = BalancedTernary.new("+-++-")

%w[a b c a*(b-c)].each do |exp|
  val = eval(exp)
  puts "%8s :%13s,%8d" % [exp, val, val.to_i]
end
