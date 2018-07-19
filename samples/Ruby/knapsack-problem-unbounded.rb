KnapsackItem = Struct.new(:volume, :weight, :value)
panacea = KnapsackItem.new(0.025, 0.3, 3000)
ichor   = KnapsackItem.new(0.015, 0.2, 1800)
gold    = KnapsackItem.new(0.002, 2.0, 2500)
maximum = KnapsackItem.new(0.25,  25,  0)

max_items = {}
for item in [panacea, ichor, gold]
  max_items[item] = [(maximum.volume/item.volume).to_i, (maximum.weight/item.weight).to_i].min
end

maxval = 0
solutions = []

0.upto(max_items[ichor]) do |i|
  0.upto(max_items[panacea]) do |p|
    0.upto(max_items[gold]) do |g|
      next if i*ichor.weight + p*panacea.weight + g*gold.weight > maximum.weight
      next if i*ichor.volume + p*panacea.volume + g*gold.volume > maximum.volume
      val = i*ichor.value + p*panacea.value + g*gold.value
      if val > maxval
        maxval = val
        solutions = [[i, p, g]]
      elsif val == maxval
        solutions << [i, p, g]
      end
    end
  end
end

puts "The maximal solution has value #{maxval}"
solutions.each do |i, p, g|
  printf "  ichor=%2d, panacea=%2d, gold=%2d -- weight:%.1f, volume=%.3f\n",
    i, p, g,
    i*ichor.weight + p*panacea.weight + g*gold.weight,
    i*ichor.volume + p*panacea.volume + g*gold.volume
end
