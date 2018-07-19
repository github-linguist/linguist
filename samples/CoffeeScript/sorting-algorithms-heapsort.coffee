# Do an in-place heap sort.
heap_sort = (arr) ->
  put_array_in_heap_order(arr)
  end = arr.length - 1
  while end > 0
    [arr[0], arr[end]] = [arr[end], arr[0]]
    sift_element_down_heap arr, 0, end
    end -= 1

put_array_in_heap_order = (arr) ->
  i = arr.length / 2 - 1
  i = Math.floor i
  while i >= 0
    sift_element_down_heap arr, i, arr.length
    i -= 1

sift_element_down_heap = (heap, i, max) ->
  while i < max
    i_big = i
    c1 = 2*i + 1
    c2 = c1 + 1
    if c1 < max and heap[c1] > heap[i_big]
      i_big = c1
    if c2 < max and heap[c2] > heap[i_big]
      i_big = c2
    return if i_big is i
    [heap[i], heap[i_big]] = [heap[i_big], heap[i]]
    i = i_big

do ->
  arr = [12, 11, 15, 10, 9, 1, 2, 3, 13, 14, 4, 5, 6, 7, 8]
  heap_sort arr
  console.log arr
