statmode <- function(v) {
  a <- sort(table(v), decreasing=TRUE)
  r <- c()
  for(i in 1:length(a)) {
    if ( a[[1]] == a[[i]] ) {
      r <- c(r, as.integer(names(a)[i]))
    } else break; # since it's sorted, once we find
                  # a different value, we can stop
  }
  r
}

print(statmode(c(1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17)))
print(statmode(c(1, 1, 2, 4, 4)))
