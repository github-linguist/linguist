function isperfect(n)
  n == sum([n % i == 0 ? i : 0 for i = 1:n-1])
end
