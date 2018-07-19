def leftrect(f, left, right)
  f.call(left)
end

def midrect(f, left, right)
  f.call((left+right)/2.0)
end

def rightrect(f, left, right)
  f.call(right)
end

def trapezium(f, left, right)
  (f.call(left) + f.call(right)) / 2.0
end

def simpson(f, left, right)
  (f.call(left) + 4*f.call((left+right)/2.0) + f.call(right)) / 6.0
end

def integrate(f, a, b, steps, method)
  delta = 1.0 * (b - a) / steps
  total = 0.0
  steps.times do |i|
    left = a + i*delta
    right = left + delta
    total += delta * send(method, f, left, right)
  end
  total
end

def square(x)
  x**2
end

def def_int(f, a, b)
  l = case f.to_s
      when /sin>/
        lambda {|x| -Math.cos(x)}
      when /square>/
        lambda {|x| (x**3)/3.0}
      end
  l.call(b) - l.call(a)
end

a = 0
b = Math::PI
steps = 10

for func in [method(:square), Math.method(:sin)]
  puts "integral of #{func} from #{a} to #{b} in #{steps} steps"
  actual = def_int(func, a, b)
  for method in [:leftrect, :midrect, :rightrect, :trapezium, :simpson]
    int = integrate(func, a, b, steps, method)
    diff = (int - actual) * 100.0 / actual
    printf "   %-10s  %s\t(%.1f%%)\n", method, int, diff
  end
end
