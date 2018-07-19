from strutils import format

proc check_doors() =
  const n = 100
  var is_open : array[1..n, bool] # auto-initialized to false
  # pass over the doors n times
  for pass in 1..n:
    var i = pass
    while i <= n:
      is_open[i] = not is_open[i]
      i += pass
  # print the result
  for door in 1..n:
    echo format("door $1 is $2.", door, (if is_open[door]: "open" else: "closed"))

check_doors()
