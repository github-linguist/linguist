max_sum_seq = (sequence) ->
  # This runs in linear time.
  [sum_start, sum, max_sum, max_start, max_end] = [0, 0, 0, 0, 0]
  for n, i in sequence
    sum += n
    if sum > max_sum
      max_sum = sum
      max_start = sum_start
      max_end = i + 1
    if sum < 0 # start new sequence
      sum = 0
      sum_start = i + 1
  sequence[max_start...max_end]

# tests
console.log max_sum_seq [-1, 0, 15, 3, -9, 12, -4]
console.log max_sum_seq [-1]
console.log max_sum_seq [4, -10, 3]
