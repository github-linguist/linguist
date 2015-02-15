def kaprekar(n, base = 10)
  n = n.to_s
  return [1, 1, 1, ""] if n == "1"
  sqr = (n.to_i(base) ** 2).to_s(base)
  0.upto(sqr.length - 1) do |i|
    a = sqr[0 .. i]
    b = sqr[i+1 .. -1]
    break if b.delete("0").empty?
    sum = (a.to_i(base) + b.to_i(base)).to_s(base)
    return [n, sqr, a, b] if sum == n
  end
  nil
end

count = 0
1.upto(10_000 - 1) do |i|
  if result = kaprekar(i)
    puts "%4d  %8d  %s + %s" % result
    count += 1
  end
end

10_000.upto(1_000_000 - 1) {|i| count += 1 if kaprekar(i)}
puts "#{count} kaprekar numbers under 1,000,000"

puts "\nbase17 kaprekar numbers under (base10)1,000,000"
base = 17
1.upto(1_000_000) do |decimal|
  if result = kaprekar(decimal.to_s(base), base)
    puts "%7s  %5s  %9s  %s + %s\n" % [decimal, *result]
  end
end
