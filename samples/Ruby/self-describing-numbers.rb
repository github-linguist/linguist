def is_self_describing?(n)
  digits = n.to_s.chars.collect {|digit| digit.to_i}
  len = digits.length
  count = Array.new(len, 0)

  digits.each do |digit|
    return false if digit >= len
    count[digit] = count[digit] + 1
  end

  digits.eql?(count)
end

3_300_000.times {|n| puts n if is_self_describing?(n)}
