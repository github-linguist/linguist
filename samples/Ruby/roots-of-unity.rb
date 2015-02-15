require 'complex'

for n in 2..10
  printf "%2d ", n
  puts (0..n-1).map { |k| Complex.polar(1, 2 * Math::PI * k / n) }.join(" ")
end
