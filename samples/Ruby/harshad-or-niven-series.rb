def digsum n
  n.to_s.chars.map(&:to_i).reduce(:+)
end

harshad = (1..Float::INFINITY).lazy.select { |n| n % digsum(n) == 0 }

p harshad.take(20).to_a
p harshad.find { |n| n > 1000 }
