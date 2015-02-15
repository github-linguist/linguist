quicksort = ([x, xs...]) ->
  return [] unless x?
  smallerOrEqual = (a for a in xs when a <= x)
  larger = (a for a in xs when a > x)
  (quicksort smallerOrEqual).concat(x).concat(quicksort larger)
