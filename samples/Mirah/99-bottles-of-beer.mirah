plural = 's'
99.downto(1) do |i|
  puts "#{i} bottle#{plural} of beer on the wall,"
  puts "#{i} bottle#{plural} of beer"
  puts "Take one down, pass it around!"
  plural = '' if i - 1 == 1
  if i > 1
    puts "#{i-1} bottle#{plural} of beer on the wall!"
    puts
  else
    puts "No more bottles of beer on the wall!"
  end
end
