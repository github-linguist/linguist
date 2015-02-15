accumulator = (sum) ->
  (n) -> sum += n

f = accumulator(1)
console.log f(5)
console.log f(2.3)
