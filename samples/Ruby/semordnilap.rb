DICT=File.readlines("unixdict.txt").collect &:chomp
i = 0
res = DICT.collect(&:reverse).sort.select {|z|
  i+=1 while z > DICT[i] and i < DICT.length-1
  z == DICT[i] and z < z.reverse
}
puts "There are #{res.length} semordnilaps, of which the following are 5:"
res.sample(5).each {|z| puts "#{z}   #{z.reverse}"}
