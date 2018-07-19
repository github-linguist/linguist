for(i in 0:4) {
  s <- ""
  for(j in 0:i) {
    s <- paste(s, "*", sep="")
  }
  print(s)
}
