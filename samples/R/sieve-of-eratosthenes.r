sieve <- function(n)
{
   n <- as.integer(n)
   if(n > 1e6) stop("n too large")
   primes <- rep(TRUE, n)
   primes[1] <- FALSE
   last.prime <- 2L
   for(i in last.prime:floor(sqrt(n)))
   {
      primes[seq.int(2L*last.prime, n, last.prime)] <- FALSE
      last.prime <- last.prime + min(which(primes[(last.prime+1):n]))
   }
   which(primes)
}

sieve(1000)
