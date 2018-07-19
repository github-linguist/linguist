for(j in 2:10) {
  r <- sprintf("%d: ", j)
  for(n in 1:j) {
    r <- paste(r, format(exp(2i*pi*n/j), digits=4), ifelse(n<j, ",", ""))
  }
  print(r)
}
