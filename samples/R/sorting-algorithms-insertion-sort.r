insertionsort <- function(x)
{
   for(i in 2:(length(x)))
   {
      value <- x[i]
      j <- i - 1
      while(j >= 1 && x[j] > value)
      {
         x[j+1] <- x[j]
         j <- j-1
      }
      x[j+1] <- value
   }
   x
}
insertionsort(c(4, 65, 2, -31, 0, 99, 83, 782, 1)) # -31   0   1   2   4  65  83  99 782
