srand
ary = (1..20).to_a.shuffle.each_slice(4).to_a
p ary

catch :found_it do
  for row in ary
    for element in row
      print "%2d " % element
      throw :found_it if element == 20
    end
    puts ","
  end
end

puts "done"
