biased_rand_function = (n) ->
  # return a function that returns 0/1  with
  # 1 appearing only 1/Nth of the time
  cap = 1/n
  ->
    if Math.random() < cap
      1
    else
      0

unbiased_function = (f) ->
  ->
    while true
      [n1, n2] = [f(), f()]
      return n1 if n1 + n2 == 1

stats = (label, f) ->
  cnt = 0
  sample_size = 10000000
  for i in [1...sample_size]
    cnt += 1 if f() == 1
  console.log "ratio of 1s: #{cnt / sample_size} [#{label}]"

for n in [3..6]
  console.log "\n---------- n = #{n}"
  f_biased = biased_rand_function(n)
  f_unbiased = unbiased_function f_biased
  stats "biased", f_biased
  stats "unbiased", f_unbiased
