forward_difference = (arr, n) ->
  # Find the n-th order forward difference for arr using
  # a straightforward recursive algorithm.
  # Assume arr is integers and n <= arr.length.
  return arr if n == 0
  arr = forward_difference(arr, n-1)
  (arr[i+1] - arr[i] for i in [0...arr.length - 1])

arr = [-1, 0, 1, 8, 27, 64, 125, 216]
for n in [0..arr.length]
  console.log n, forward_difference arr, n
