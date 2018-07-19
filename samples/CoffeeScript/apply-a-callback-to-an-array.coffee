map = (arr, f) -> (f(e) for e in arr)
arr = [1, 2, 3, 4, 5]
f = (x) -> x * x
console.log map arr, f # prints [1, 4, 9, 16, 25]
