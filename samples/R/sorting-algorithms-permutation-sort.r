permutationsort <- function(x)
{
   if(!require(e1071) stop("the package e1071 is required")
   is.sorted <- function(x) all(diff(x) >= 0)

   perms <- permutations(length(x))
   i <- 1
   while(!is.sorted(x))
   {
      x <- x[perms[i,]]
      i <- i + 1
   }
   x
}
permutationsort(c(1, 10, 9, 7, 3, 0))
