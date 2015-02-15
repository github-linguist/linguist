def range_extract(l)
  sorted = l.sort
  range = []
  start = sorted.first
  # pad the list with a big value, so that the last loop iteration will
  # appended something to the range
  sorted.concat([Float::MAX]).each_cons(2) do |prev,n|
    if prev.succ < n
      if start == prev
        range << start.to_s
      else
        range << "%d%s%d" % [start, (start.succ == prev ? "," : "-"), prev]
      end
      start = n
    end
  end
  range.join(',')
end

lst = [
    0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
   15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
   25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
   37, 38, 39
]

p rng = range_extract(lst)
