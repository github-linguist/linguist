iterate.until.escape <- function(z, c, trans, cond, max=50, response=dwell) {
  #we iterate all active points in the same array operation,
  #and keeping track of which points are still iterating.
  active <- seq_along(z)
  dwell <- z
  dwell[] <- 0
  for (i in 1:max) {
    z[active] <- trans(z[active], c[active]);
    survived <- cond(z[active])
    dwell[active[!survived]] <- i
    active <- active[survived]
    if (length(active) == 0) break
  }
  eval(substitute(response))
}

re = seq(-2, 1, len=500)
im = seq(-1.5, 1.5, len=500)
c <- outer(re, im, function(x,y) complex(real=x, imaginary=y))
x <- iterate.until.escape(array(0, dim(c)), c,
                          function(z,c)z^2+c, function(z)abs(z) <= 2,
                          max=100)
image(x)
