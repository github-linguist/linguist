for n in 0..33
  puts " %6b %3o %2d %2X" % [n, n, n, n]
end
puts
[2,8,10,16,36].each {|i| puts " 100.to_s(#{i}) => #{100.to_s(i)}"}
