knuth_shuffle = (a) ->
  n = a.length
  while n > 1
    r = Math.floor(n * Math.random())
    n -= 1
    [a[n], a[r]] = [a[r], a[n]]
  a

counts =
  "1,2,3": 0
  "1,3,2": 0
  "2,1,3": 0
  "2,3,1": 0
  "3,1,2": 0
  "3,2,1": 0

for i in [1..100000]
  counts[knuth_shuffle([ 1, 2, 3 ]).join(",")] += 1

for key, val of counts
  console.log "#{key}: #{val}"
