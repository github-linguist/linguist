def factor_pairs n
  first = 10 ** ((n.to_s.size / 2) - 1)
  (first .. n ** 0.5).map { |i| [i, n / i] if n % i == 0 }.compact
end

def vampire_factors n
  return [] if n.to_s.size.odd?
  half = n.to_s.size / 2
  factor_pairs(n).select do |a, b|
    a.to_s.size == half && b.to_s.size == half &&
    [a, b].count {|x| x%10 == 0} != 2          &&
    "#{a}#{b}".chars.sort == n.to_s.chars.sort
  end
end

i = vamps = 0
until vamps == 25
  vf = vampire_factors(i += 1)
  unless vf.empty?
    puts "#{i}:\t#{vf}"
    vamps += 1
  end
end

[16758243290880, 24959017348650, 14593825548650].each do |n|
  if (vf = vampire_factors n).empty?
    puts "#{n} is not a vampire number!"
  else
    puts "#{n}:\t#{vf}" unless (vf = vampire_factors n).empty?
  end
end
