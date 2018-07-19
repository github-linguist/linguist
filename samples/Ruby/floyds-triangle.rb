def floyd(rows)
  max = (rows * (rows + 1)) / 2
  widths = ((max - rows + 1)..max).map {|n| n.to_s.length + 1}
  n = 0
  rows.times do |r|
    puts (0..r).map {|i| n += 1; "%#{widths[i]}d" % n}.join
  end
end

floyd(5)
floyd(14)
