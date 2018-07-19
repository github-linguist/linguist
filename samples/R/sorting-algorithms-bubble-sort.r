bubblesort <- function(v) {
  itemCount <- length(v)
  repeat {
    hasChanged <- FALSE
    itemCount <- itemCount - 1
    for(i in 1:itemCount) {
      if ( v[i] > v[i+1] ) {
        t <- v[i]
        v[i] <- v[i+1]
        v[i+1] <- t
        hasChanged <- TRUE
      }
    }
    if ( !hasChanged ) break;
  }
  v
}

v <- c(9,8,7,3,1,100)
print(bubblesort(v))
