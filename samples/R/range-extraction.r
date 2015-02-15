extract.range = function(v)
   {r = c(1, which(diff(v) != 1) + 1, length(v) + 1)
      # 'r' holds the index of the start of each run of sequential
      # elements.
    paste0(collapse = ",", v[head(r, -1)], ifelse(diff(r) == 1, "",
        paste0(
            ifelse(diff(r) == 2, ",", "-"),
            v[r[-1] - 1])))}

print(extract.range(c(
    -6, -3, -2, -1, 0, 1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20)))
print(extract.range(c(
    0, 1, 2, 4, 6, 7, 8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22,
    23, 24, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39)))
