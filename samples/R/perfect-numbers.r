is.perf <- function(n){
	if (n==0|n==1) return(FALSE)
	s <- seq (1,n-1)
	x <- n %% s
	m <- data.frame(s,x)
	out <- with(m, s[x==0])
	return(sum(out)==n)	
}
# Usage - Warning High Memory Usage
is.perf(28)
sapply(c(6,28,496,8128,33550336),is.perf)
