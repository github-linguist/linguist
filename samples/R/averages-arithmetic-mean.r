omean <- function(v) {
  m <- mean(v)
  ifelse(is.na(m), 0, m)
}
