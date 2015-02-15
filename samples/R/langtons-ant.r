langton.ant = function(n = 100) {
	map = matrix(data = 0, nrow = n, ncol = n)
	p = floor(c(n/2, n/2))
	d = sample(1:4, 1)
	i = 1
	while(p[1] > 0 & p[1] <= n & p[2] > 0 & p[2] <= n) {
		if(map[p[1], p[2]] == 1) {
			map[p[1], p[2]] = 0
			p = p + switch(d, c(0, 1), c(-1, 0), c(0, -1), c(1, 0))
			d = ifelse(d == 4, 1, d + 1)
		} else {
			map[p[1], p[2]] = 1
			p = p + switch(d, c(0, -1), c(1, 0), c(0, 1), c(-1, 0))
			d = ifelse(d == 1, 4, d - 1)
		}
	}
	return(map)
}

image(langton.ant(), xaxt = "n", yaxt = "n", bty = "n")
