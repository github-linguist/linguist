hanoimove <- function(ndisks, from, to, via) {
  if ( ndisks == 1 )
    cat("move disk from", from, "to", to, "\n")
  else {
    hanoimove(ndisks-1, from, via, to)
    hanoimove(1, from, to, via)
    hanoimove(ndisks-1, via, to, from)
  }
}

hanoimove(4,1,2,3)
