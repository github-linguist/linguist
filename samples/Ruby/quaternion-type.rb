class Quaternion
  def initialize(*parts)
    raise ArgumentError, "wrong number of arguments (#{parts.size} for 4)" unless parts.size == 4
    raise ArgumentError, "invalid value of quaternion parts #{parts}" unless parts.all? {|x| x.is_a?(Numeric)}
    @parts = parts
  end

  def to_a;          @parts;                                       end
  def to_s;          "Quaternion#{@parts.to_s}"                    end
  alias inspect to_s
  def complex_parts; [Complex(*to_a[0..1]), Complex(*to_a[2..3])]; end

  def real;          @parts.first;                                 end
  def imag;          @parts[1..3];                                 end
  def conj;          Quaternion.new(real, *imag.map(&:-@));        end
  def norm;          Math.sqrt(to_a.reduce(0){|sum,e| sum + e**2}) end # In Rails: Math.sqrt(to_a.sum { e**2 })

  def ==(other)
    case other
    when Quaternion; to_a == other.to_a
    when Numeric;    to_a == [other, 0, 0, 0]
    else             false
    end
  end
  def -@;            Quaternion.new(*to_a.map(&:-@));              end
  def -(other);      self + -other;                                end

  def +(other)
    case other
    when Numeric
      Quaternion.new(real + other, *imag)
    when Quaternion
      Quaternion.new(*to_a.zip(other.to_a).map { |x,y| x + y }) # In Rails: zip(other).map(&:sum)
    end
  end

  def *(other)
    case other
    when Numeric
      Quaternion.new(*to_a.map { |x| x * other })
    when Quaternion
      # Multiplication of quaternions in C x C space. See "Cayley-Dickson construction".
      a, b, c, d = *complex_parts, *other.complex_parts
      x, y = a*c - d.conj*b, a*d + b*c.conj
      Quaternion.new(x.real, x.imag, y.real, y.imag)
    end
  end

  # Coerce is called by Ruby to return a compatible type/receiver when the called method/operation does not accept a Quaternion
  def coerce(other)
    case other
    when Numeric then [Scalar.new(other), self]
    else raise TypeError, "#{other.class} can't be coerced into #{self.class}"
    end
  end

  class Scalar
    def initialize(val); @val = val;                            end
    def +(other);        other + @val;                          end
    def *(other);        other * @val;                          end
    def -(other);        Quaternion.new(@val, 0, 0, 0) - other; end
  end
end

if __FILE__ == $0
  q  = Quaternion.new(1,2,3,4)
  q1 = Quaternion.new(2,3,4,5)
  q2 = Quaternion.new(3,4,5,6)
  r  = 7
  expressions = ["q", "q1", "q2",
                 "q.norm", "-q", "q.conj", "q + r", "r + q","q1 + q2", "q2 + q1",
                 "q * r", "r * q", "q1 * q2", "q2 * q1", "(q1 * q2 != q2 * q1)",
                 "q - r", "r - q"]
  expressions.each do |exp|
    puts "%20s = %s" % [exp, eval(exp)]
  end
end
