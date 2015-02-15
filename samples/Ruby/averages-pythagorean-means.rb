class Array
  def arithmetic_mean
    inject(:+).to_f / length
  end

  def geometric_mean
    inject(:*) ** (1.0 / length)
  end

  def harmonic_mean
    length.to_f / inject(0) {|s, m| s += 1.0/m}
  end
end

class Range
  def method_missing(m, *args)
    case m
    when /_mean$/ then to_a.send(m)
    else super
    end
  end
end

p a = (1..10).arithmetic_mean
p g = (1..10).geometric_mean
p h = (1..10).harmonic_mean
# is h < g < a ??
p g.between?(h, a)
