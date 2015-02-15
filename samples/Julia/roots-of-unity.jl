nthroots(n::Integer) = [ cospi(2k/n)+sinpi(2k/n)im for k = 0:n-1 ]
