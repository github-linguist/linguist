def perms(n)
  p = Array.new(n+1){|i| -i}
  s = 1
  loop do
    yield p[1..-1].map(&:abs), s
    k = 0
    for i in 2..n
      k = i if p[i] < 0 and p[i].abs > p[i-1].abs and p[i].abs > p[k].abs
    end
    for i in 1...n
      k = i if p[i] > 0 and p[i].abs > p[i+1].abs and  p[i].abs > p[k].abs
    end
    break if k.zero?
    for i in 1..n
      p[i] *= -1 if p[i].abs > p[k].abs
    end
    i = k + (p[k] <=> 0)
    p[k], p[i] = p[i], p[k]
    s = -s
  end
end

for i in 3..4
  perms(i){|perm, sign| puts "Perm: #{perm}  Sign: #{sign}"}
  puts
end
