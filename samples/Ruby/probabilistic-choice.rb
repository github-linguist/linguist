probabilities = {
  "aleph"  => 1/5.0,
  "beth"   => 1/6.0,
  "gimel"  => 1/7.0,
  "daleth" => 1/8.0,
  "he"     => 1/9.0,
  "waw"    => 1/10.0,
  "zayin"  => 1/11.0,
}
probabilities["heth"] = 1.0 - probabilities.each_value.inject(:+)
ordered_keys = probabilities.keys

sum, sums = 0.0, {}
ordered_keys.each do |key|
  sum += probabilities[key]
  sums[key] = sum
end

actual = Hash.new(0)

samples = 1_000_000
samples.times do
  r = rand
  for k in ordered_keys
    if r < sums[k]
      actual[k] += 1
      break
    end
  end
end

puts  "key     expected    actual        diff"
for k in ordered_keys
  act = Float(actual[k]) / samples
  val = probabilities[k]
  printf "%-8s%.8f  %.8f  %6.3f %%\n", k, val, act, 100*(act-val)/val
end
