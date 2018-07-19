bogosort <- function(x)
{
   is.sorted <- function(x) all(diff(x) >= 0)
   while(!is.sorted(x)) x <- sample(x)
   x
}

n <- c(1, 10, 9, 7, 3, 0)
print(bogosort(n))
