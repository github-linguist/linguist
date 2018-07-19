def s_of_n_creator(n)
  sample = []
  i = 0
  Proc.new do |item|
    i += 1
    if i <= n
      sample << item
    elsif rand(i) < n
      sample[rand(n)] = item
    end
    sample
  end
end

frequency = Array.new(10,0)
100_000.times do
  s_of_n = s_of_n_creator(3)
  sample = nil
  (0..9).each {|digit| sample = s_of_n[digit]}
  sample.each {|digit| frequency[digit] += 1}
end

(0..9).each {|digit| puts "#{digit}\t#{frequency[digit]}"}
