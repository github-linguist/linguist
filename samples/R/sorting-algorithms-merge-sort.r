mergesort <- function(m)
{
   merge_ <- function(left, right)
   {
      result <- c()
      while(length(left) > 0 && length(right) > 0)
      {
         if(left[1] <= right[1])
         {
            result <- c(result, left[1])
            left <- left[-1]
         } else
         {
            result <- c(result, right[1])
            right <- right[-1]
         }
      }
      if(length(left) > 0) result <- c(result, left)
      if(length(right) > 0) result <- c(result, right)
      result
   }

   len <- length(m)
   if(len <= 1) m else
   {
      middle <- length(m) / 2
      left <- m[1:floor(middle)]
      right <- m[floor(middle+1):len]
      left <- mergesort(left)
      right <- mergesort(right)
      if(left[length(left)] <= right[1])
      {
         c(left, right)
      } else
      {
         merge_(left, right)
      }
   }
}
mergesort(c(4, 65, 2, -31, 0, 99, 83, 782, 1)) # -31   0   1   2   4  65  83  99 782
