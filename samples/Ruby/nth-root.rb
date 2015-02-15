def nthroot(n, a, precision = 1e-5)
  x = Float(a)
  begin
    prev = x
    x = ((n - 1) * prev + a / (prev ** (n - 1))) / n
  end while (prev - x).abs > precision
  x
end

p nthroot(5,34)  # => 2.02439745849989
