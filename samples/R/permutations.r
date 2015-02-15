next.perm <- function(p) {
	n <- length(p)
	i <- n - 1
	r = TRUE
	for(i in (n-1):1) {
		if(p[i] < p[i+1]) {
			r = FALSE
			break
		}
	}
	
	j <- i + 1
	k <- n
	while(j < k) {
		x <- p[j]
		p[j] <- p[k]
		p[k] <- x
		j <- j + 1
		k <- k - 1
	}
	
	if(r) return(NULL)
	
	j <- n
	while(p[j] > p[i]) j <- j - 1
	j <- j + 1
	
	x <- p[i]
	p[i] <- p[j]
	p[j] <- x
	return(p)
}

print.perms <- function(n) {
	p <- 1:n
	while(!is.null(p)) {
		cat(p,"\n")
		p <- next.perm(p)
	}
}

print.perms(3)
# 1 2 3
# 1 3 2
# 2 1 3
# 2 3 1
# 3 1 2
# 3 2 1
