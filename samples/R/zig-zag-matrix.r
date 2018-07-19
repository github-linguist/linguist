zigzag <- function(size)
{
   digits <- seq_len(size^2) - 1
   mat <- matrix(0, nrow = size, ncol=size)
   i <- 1
   j <- 1
   for(element in digits)
   {
      mat[i,j] <- element
      if((i + j) %% 2 == 0)
      {
         # Even stripes
         if(j < size) j <- j + 1 else i <- i + 2
         if(i > 1) i <- i - 1
      } else
      {
         # Odd stripes
         if(i < size) i <- i + 1 else j <- j + 2
         if(j > 1) j <- j - 1
      }
   }
   mat
}

zigzag(5)
