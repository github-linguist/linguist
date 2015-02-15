gnomesort <- function(x)
{
   i <- 1
   j <- 1
   while(i < length(x))
   {
      if(x[i] <= x[i+1])
      {
         i <- j
         j <- j+1
      } else
      {
         temp <- x[i]
         x[i] <- x[i+1]
         x[i+1]  <- temp
         i <- i - 1
         if(i == 0)
         {
            i <- j
            j <- j+1
         }
      }
   }
   x
}
gnomesort(c(4, 65, 2, -31, 0, 99, 83, 782, 1)) # -31   0   1   2   4  65  83  99 782
