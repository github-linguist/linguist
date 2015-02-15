counting_sort <- function(arr, minval, maxval) {
  r <- arr
  z <- 1
  for(i in minval:maxval) {
    cnt = sum(arr == i)
    while(cnt > 0) {
      r[z] = i
      z <- z + 1
      cnt <- cnt - 1
    }
  }
  r
}

# 140+1 instead of 140, since random numbers generated
# by runif are always less than the given maximum;
# floor(a number at most 140.9999...) is 140
ages <- floor(runif(100, 0, 140+1))
sorted <- counting_sort(ages, 0, 140)
print(sorted)
