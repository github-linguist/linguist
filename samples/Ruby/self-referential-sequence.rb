$cache = {}
def selfReferentialSequence_cached(n, seen = [])
  return $cache[n] if $cache.include? n
  return [] if seen.include? n

  digit_count = Array.new(10, 0)
  n.to_s.chars.collect {|char| digit_count[char.to_i] += 1}
  term = ''
  9.downto(0).each do |d|
    if digit_count[d] > 0
      term += digit_count[d].to_s + d.to_s
    end
  end
  term = term.to_i
  $cache[n] = [n] + selfReferentialSequence_cached(term, [n] + seen)
end

limit = 1_000_000
max_len = 0
max_vals = []

1.upto(limit - 1) do |n|
  seq = selfReferentialSequence_cached(n)
  if seq.length > max_len
    max_len = seq.length
    max_vals = [n]
  elsif seq.length == max_len
    max_vals << n
  end
end

puts "values: #{max_vals.inspect}"
puts "iterations: #{max_len}"
puts "sequence:"
selfReferentialSequence_cached(max_vals[0]).each_with_index do |val, idx|
  puts "%2d %d" % [idx + 1, val]
end
