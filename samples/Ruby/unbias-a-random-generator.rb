def rand_n(bias)
  rand(bias) == 0 ? 1 : 0
end

def unbiased(bias)
  a, b = rand_n(bias), rand_n(bias) until a != b #loop until a and b are 0,1 or 1,0
  a
end

runs = 1_000_000
keys = %i(bias biased unbiased) #use [:bias,:biased,:unbiased] in Ruby < 2.0
puts keys.join("\t")

(3..6).each do |bias|
  counter = Hash.new(0) # counter will respond with 0 when key is not known
  runs.times do
    counter[:biased] += 1 if rand_n(bias) == 1 #the first time, counter has no key for :biased, so it will respond 0
    counter[:unbiased] += 1 if unbiased(bias) == 1
  end
  counter[:bias] = bias
  puts counter.values_at(*keys).join("\t")
end
