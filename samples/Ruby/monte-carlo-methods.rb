def approx_pi(throws)
  times_inside = throws.times.count {Math.hypot(rand, rand) <= 1.0}
  # in pre-Ruby-1.8.7: times_inside = throws.times.select {Math.hypot(rand, rand) <= 1.0}.length
  4.0 * times_inside / throws
end

[1000, 10_000, 100_000, 1_000_000, 10_000_000].each do |n|
      puts "%8d samples: PI = %s" % [n, approx_pi(n)]
end
