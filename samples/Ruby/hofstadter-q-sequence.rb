@cache = []
def Q(n)
  if @cache[n].nil?
    case n
    when 1, 2 then @cache[n] = 1
    else @cache[n] = Q(n - Q(n-1)) + Q(n - Q(n-2))
    end
  end
  @cache[n]
end

puts "first 10 numbers in the sequence: #{(1..10).map {|n| Q(n)}}"
puts "1000'th term: #{Q(1000)}"

prev = Q(1)
count = 0
2.upto(100_000) do |n|
  q = Q(n)
  count += 1 if q < prev
  prev = q
end
puts "number of times in the first 100,000 terms where Q(i)<Q(i-1): #{count}"
