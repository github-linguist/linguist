def accumulator(sum)
  lambda {|n| sum += n}
end

# mixing Integer and Float
x = accumulator(1)
x.call(5)
p accumulator(3)  # add some output to show what it returns
puts x.call(2.3)  # prints 8.3

# mixing Rational and Complex
require 'rational'
require 'complex'
y = accumulator(Rational(2, 3))
y.call(Rational(1, 2))
puts y.call(4)
puts y.call(Complex(0, 1))
puts y.call(Complex.polar(6, 5 * Math::PI / 4))
puts x.call(0)    # again prints 8.3

# using other things that have a + method
t = accumulator(Time.utc(1999, 8, 7, 6, 5))
puts t.call(4)    # prints 1999-08-07 06:05:04 UTC

require 'matrix'
m = accumulator(Matrix[[1, 2], [3, 4]])
puts m.call(Matrix[[5, 6], [7, 8]])
puts t.call(-12 * 60 * 60)  # subtracts 12 hours
puts y.call(1e200)
puts x.call(0)    # again prints 8.3
