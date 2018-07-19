sum <- function(var, lo, hi, term)
  eval(substitute({
    .temp <- 0;
    for (var in lo:hi) {
      .temp <- .temp + term
    }
    .temp
  }, as.list(match.call()[-1])),
  enclos=parent.frame())

sum(i, 1, 100, 1/i) #prints 5.187378

##and because of enclos=parent.frame(), the term can involve variables in the caller's scope:
x <- -1
sum(i, 1, 100, i^x) #5.187378
