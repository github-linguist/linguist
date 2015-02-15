stirling <- function(z) sqrt(2*pi/z) * (exp(-1)*z)^z

nemes <- function(z) sqrt(2*pi/z) * (exp(-1)*(z + (12*z - (10*z)^-1)^-1))^z

lanczos <- function(z)
{
   if(length(z) > 1)
   {
      sapply(z, lanczos)
   } else
   {
     g <- 7
      p <- c(0.99999999999980993, 676.5203681218851, -1259.1392167224028,
        771.32342877765313, -176.61502916214059, 12.507343278686905,
        -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7)
      z <- as.complex(z)
      if(Re(z) < 0.5)
      {
         pi / (sin(pi*z) * lanczos(1-z))
      } else
      {
         z <- z - 1
         x <- p[1] + sum(p[-1]/seq.int(z+1, z+g+1))
         tt <- z + g + 0.5
         sqrt(2*pi) * tt^(z+0.5) * exp(-tt) * x
      }
   }
}

spouge <- function(z, a=49)
{
   if(length(z) > 1)
   {
      sapply(z, spouge)
   } else
   {
      z <- z-1
      k <- seq.int(1, a-1)
      ck <- rep(c(1, -1), len=a-1) / factorial(k-1) * (a-k)^(k-0.5) * exp(a-k)
      (z + a)^(z+0.5) * exp(-z - a) * (sqrt(2*pi) + sum(ck/(z+k)))
   }
}

# Checks
z <- (1:10)/3
all.equal(gamma(z), stirling(z))             # Mean relative difference: 0.07181942
all.equal(gamma(z), nemes(z))                # Mean relative difference: 0.003460549
all.equal(as.complex(gamma(z)), lanczos(z))  # TRUE
all.equal(gamma(z), spouge(z))               # TRUE
data.frame(z=z, stirling=stirling(z), nemes=nemes(z), lanczos=lanczos(z), spouge=spouge(z), builtin=gamma(z))
