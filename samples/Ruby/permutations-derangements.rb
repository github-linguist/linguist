def derangements(n)
  ary = (1 .. n).to_a
  ary.permutation.select do |perm|
    ary.zip(perm).all? {|a,b| a != b}
  end
end

def subfact(n)
  case n
  when 0 then 1
  when 1 then 0
  else (n-1)*(subfact(n-1) + subfact(n-2))
  end
end

puts "derangements for n = 4"
derangements(4).each{|d|p d}

puts "\n n   derange  subfact"
(0..9).each do |n|
  puts "%2d :%8d,%8d" % [n, derangements(n).size, subfact(n)]
end

puts "\nNumber of derangements"
(10..20).each do |n|
  puts "#{n} : #{subfact(n)}"
end
