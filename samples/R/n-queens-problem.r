# Brute force, see the "Permutations" page for the next.perm function
safe <- function(p) {
	n <- length(p)
	for(i in 1:(n-1)) {
		for(j in (i+1):n) {
			if(abs(p[j] - p[i]) == abs(j - i)) return(FALSE)
		}
	}
	return(TRUE)
}

queens <- function(n) {
	p <- 1:n
	k <- 0
	while(!is.null(p)) {
		if(safe(p)) {
			cat(p,"\n")
			k <- k + 1
		}
		p <- next.perm(p)
	}
	return(k)
}

queens(8)
# 1 5 8 6 3 7 2 4
# ...
# 92
