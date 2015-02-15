def F(n)
  n == 0 ? 1 : n - M(F(n-1))
end
def M(n)
  n == 0 ? 0 : n - F(M(n-1))
end

p (Array.new(20) {|n| F(n) })
p (Array.new(20) {|n| M(n) })
