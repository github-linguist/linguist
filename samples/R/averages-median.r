omedian <- function(v) {
  if ( length(v) < 1 )
    NA
  else {
    sv <- sort(v)
    l <- length(sv)
    if ( l %% 2 == 0 )
      (sv[floor(l/2)+1] + sv[floor(l/2)])/2
    else
      sv[floor(l/2)+1]
  }
}

a <- c(4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2)
b <- c(4.1, 7.2, 1.7, 9.3, 4.4, 3.2)

print(median(a))   # 4.4
print(omedian(a))
print(median(b))   # 4.25
print(omedian(b))
