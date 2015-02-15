randN = function(N) sample.int(N, 1) == 1

unbiased = function(f)
   {while ((x <- f()) == f()) {}
    x}

samples = 10000
print(t(round(d = 2, sapply(3:6, function(N) c(
    N = N,
    biased = mean(replicate(samples, randN(N))),
    unbiased = mean(replicate(samples, unbiased(function() randN(N)))))))))
