cocktailsort <- function(x)
{
   lenx <- length(x)
   repeat
   {
      swapped <- FALSE
      for(i in 1:(lenx-1))
      {
         if(x[i] > x[i+1])
         {
            temp <- x[i]
            x[i] <- x[i+1]
            x[i+1] <- temp
            swapped <- TRUE
         }
      }
      if(!swapped) break

      swapped <- FALSE
      for(i in (lenx-1):1)
      {
         if(x[i] > x[i+1])
         {
            temp <- x[i]
            x[i] <- x[i+1]
            x[i+1] <- temp
            swapped <- TRUE
         }
      }
      if(!swapped) break
   }
   x
}

print(cocktailsort(c(5, -1, 101, -4, 0, 1, 8,    6,  2, 3)))
