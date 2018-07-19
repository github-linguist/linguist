# This is a simple version of mergesort that returns brand-new arrays.
# A more sophisticated version would do more in-place optimizations.
merge_sort = (arr) ->
  if arr.length <= 1
    return (elem for elem in arr)
  m = Math.floor(arr.length / 2)
  arr1 = merge_sort(arr.slice 0, m)
  arr2 = merge_sort(arr.slice m)
  result = []
  p1 = p2 = 0
  while true
    if p1 >= arr1.length
      if p2 >= arr2.length
        return result
      result.push arr2[p2]
      p2 += 1
    else if p2 >= arr2.length or arr1[p1] < arr2[p2]
      result.push arr1[p1]
      p1 += 1
    else
      result.push arr2[p2]
      p2 += 1

do ->
  console.log merge_sort [2,4,6,8,1,3,5,7,9,10,11,0,13,12]
