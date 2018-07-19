mean = (array) ->
 return 0 if array.length is 0
 sum = array.reduce (s,i,0) -> s += i
 sum / array.length


alert mean [1]
