nums = [];

puts "Please enter 11 numbers:"
11.times{nums << gets.chomp.to_f}

nums.reverse.each do |n|
  res = n.abs ** 0.5 + 5 * n ** 3
  if res > 400
    puts "Overflow!"
  else
    puts res
  end
end
