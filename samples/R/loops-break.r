sample0to19 <- function() sample(0L:19L, 1,replace=TRUE)
repeat
{
  result1 <- sample0to19()
  if (result1 == 10L)
  {
    print(result1)
    break
  }
  result2 <- sample0to19()
  cat(result1, result2, "\n")
}
