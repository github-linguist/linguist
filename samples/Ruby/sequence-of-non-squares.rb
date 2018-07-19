def f(n); n + (0.5 + Math.sqrt(n)).floor; end

(1..22).each { |n| p "#{n} #{f(n)}" }

non_squares = (1..1_000_000).map { |n| f(n) }
squares = (1..1001).map { |n| n**2 } # Note: 1001*1001 = 1_002_001 > 1_001_000 = f(1_000_000)
(squares & non_squares).each do |n|
  fail "Oops, found a square f(#{non_squares.index(n)}) = #{n}"
end
