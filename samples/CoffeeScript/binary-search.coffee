binsearch = (arr, k) ->
  low = 0
  high = arr.length - 1
  while low <= high
    mid = Math.floor (low + high) / 2
    return mid if arr[mid] == k
    if arr[mid] < k
      low = mid + 1
    else
      high = mid - 1
  null

arr = [1,3,5,7,9,11]
for i in [0..12]
  pos = binsearch arr, i
  console.log "found #{i} at pos #{pos}" if pos?
