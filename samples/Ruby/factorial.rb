# Recursive
def factorial_recursive(n)
  n.zero? ? 1 : n * factorial_recursive(n - 1)
end

# Tail-recursive
def factorial_tail_recursive(n, prod = 1)
  n.zero? ? prod : factorial_tail_recursive(n - 1, prod * n)
end

# Iterative with Range#each
def factorial_iterative(n)
  (2 .. n - 1).each {|i| n *= i}
  n
end

# Iterative with Range#inject
def factorial_inject(n)
  (1..n).inject {|prod, i| prod * i}
end

# Iterative with Range#reduce, requires Ruby 1.8.7
def factorial_reduce(n)
  (1..n).reduce(:*)
end


require 'benchmark'

n = 400
m = 10000

Benchmark.bm(16) do |b|
  b.report('recursive:')       {m.times {factorial_recursive(n)}}
  b.report('tail recursive:')  {m.times {factorial_tail_recursive(n)}}
  b.report('iterative:')       {m.times {factorial_iterative(n)}}
  b.report('inject:')          {m.times {factorial_inject(n)}}
  b.report('reduce:')          {m.times {factorial_reduce(n)}}
end
