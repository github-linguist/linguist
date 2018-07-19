code <- quote(
          if (n == 1) n else {
            count <- count + 1;
            n <- if (n %% 2 == 1) 3 * n + 1 else n/2
          })

eprint <- function(envs, var="n")
  cat(paste(sprintf("%4d", sapply(envs, `[[`, var)), collapse=" "), "\n")

envs <- mapply(function(...) list2env(list(...)), n=1:12, count=0)

while (any(sapply(envs, eval, expr=code) > 1)) {eprint(envs)}
eprint(envs)

cat("\nCounts:\n")
eprint(envs, "count")
