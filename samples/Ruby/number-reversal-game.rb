ary = (1..9).to_a
ary.shuffle! while ary == ary.sort
score = 0
until ary == ary.sort
  print "#{ary.inspect} -- How many digits to reverse? "
  num = gets.to_i  # should validate input
  ary[0, num] = ary[0, num].reverse
  score += 1
end
p ary
puts "Your score: #{score}"
