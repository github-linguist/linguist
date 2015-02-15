def middle_three_digits(n)
  # minus sign doesn't factor into digit count,
  # and calling #abs acts as a duck-type assertion
  n = n.abs

  # convert to string and find length
  l = (s = n.to_s).length

  # check validity
  raise ArgumentError, "Number must have at least three digits" if l < 3
  raise ArgumentError, "Number must have an odd number of digits" if l.even?

  return s[l/2-1,3].to_i
end

samples = [
  123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345,
  1, 2, -1, -10, 2002, -2002, 0
]

width = samples.map { |n| n.to_s.length }.max

samples.each do |n|
  print "%#{width}d: " % n
  begin
    puts "%03d" % middle_three_digits(n)
  rescue ArgumentError => e
    puts e
  end
end
