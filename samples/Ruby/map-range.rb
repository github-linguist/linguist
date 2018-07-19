def map_range(a, b, s)
  af, al, bf, bl = a.first, a.last, b.first, b.last
  bf + (s - af).*(bl - bf).quo(al - af)
end

11.times do |s|
  s *= 1.0  # force floating point
  print s, " maps to ", map_range((0..10), (-1..0), s), "\n"
end
