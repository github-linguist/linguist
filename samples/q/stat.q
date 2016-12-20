pi:acos -1

/ normal from x
nx:{abs(x>0)-(exp[-.5*x*x]%sqrt 2*pi)*t*.31938153+t*-.356563782+t*1.781477937+t*-1.821255978+1.330274429*t:1%1+.2316419*abs x}

/ x from normal  (chebychev near 0.5 and log for the tails)
xn:{$[.5>x;0-.z.s 1-x;.92>x;
 (x*2.50662823884+l*-18.61500062529+l*41.39119773534+l*-25.44106049637)%1+l*-8.47351093090+l*23.08336743743+l*-21.06224101826+3.13082909833*l:x*x-:.5;
 0.3374754822726147+l*0.9761690190917186+l*0.1607979714918209+l*0.0276438810333863+l*0.0038405729373609+l*0.0003951896511919+l*0.0000321767881768+l*0.0000002888167364+0.0000003960315187*l:log 0-log 1-x]}

/ random normal distribution, e.g. nor 10
nor:{$[x=2*n:x div 2;raze sqrt[-2*log n?1f]*/:(sin;cos)@\:(2*pi)*n?1f;-1_.z.s 1+x]}

/ builtins: avg var dev med wavg cov cor avgs
mode:{where g=max g:count each group x}

/ covariance matrix (8 times faster than x cov/:\:x)
cvm:{(x+flip(not n=\:n)*x:(n#'0.0),'(x$/:'(n:til count x)_\:x)%count first x)-a*\:a:avg each x}

/ correlation matrix
crm:{cvm[x]%u*/:u:dev each x}

/ quantile of y using method z with percents x
/ qtln[0.9 0.99;til 10; 8]
/ implements method 4 to 9 from https://stat.ethz.ch/R-manual/R-devel/library/stats/html/quantile.html
qtln:{[x;y;z]cf:(0 1;1%2 2;0 0;1 1;1%3 3;3%8 8) z-4;n:count y:asc y;
    ?[hf<1;first y;last y]^y[hf-1]+(h-hf)*y[hf]-y -1+hf:floor h:cf[0]+x*n+1f-sum cf}

/ default quantile using method 8
qtl:qtln[;;8]

/ interquartile range using method 8 for approximation (n.b. R uses method 7 by default)
iqr:{last deltas qtl[0.25 0.75;x]}
