class Array
  def radix_sort(base=10)
    ary = dup
    rounds = (Math.log(ary.minmax.map(&:abs).max)/Math.log(base)).ceil
    rounds.times do |i|
      buckets = Array.new(2*base){[]}
      base_i = base**i
      ary.each do |n|
        digit = (n/base_i) % base
        digit += base if 0<=n
        buckets[digit] << n
      end
      ary = buckets.flatten
      p [i, ary] if $DEBUG
    end
    ary
  end
  def radix_sort!(base=10)
    replace radix_sort(base)
  end
end

p [1, 3, 8, 9, 0, 0, 8, 7, 1, 6].radix_sort
p [170, 45, 75, 90, 2, 24, 802, 66].radix_sort
p [170, 45, 75, 90, 2, 24, -802, -66].radix_sort
