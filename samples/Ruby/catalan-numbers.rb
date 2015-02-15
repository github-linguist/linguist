# direct

def factorial(n)
  (1..n).reduce(:*)
end

def catalan_direct(n)
  factorial(2*n) / (factorial(n+1) * factorial(n))
end

# recursive

def catalan_rec1(n)
  return 1 if n == 0
  (0..n-1).inject(0) {|sum, i| sum + catalan_rec1(i) * catalan_rec1(n-1-i)}
end

def catalan_rec2(n)
  return 1 if n == 0
  2*(2*n - 1) * catalan_rec2(n-1) /(n+1)
end

# performance and results

require 'benchmark'
require 'memoize'
include Memoize

Benchmark.bm(10) do |b|
  b.report('forget') {
    16.times {|n| [n, catalan_direct(n), catalan_rec1(n), catalan_rec2(n)]}
  }
  b.report('memoized') {
    memoize :factorial
    memoize :catalan_direct
    memoize :catalan_rec1
    memoize :catalan_rec2
    16.times {|n| [n, catalan_direct(n), catalan_rec1(n), catalan_rec2(n)]}
  }
end

16.times {|n| p [n, catalan_direct(n), catalan_rec1(n), catalan_rec2(n)]}
