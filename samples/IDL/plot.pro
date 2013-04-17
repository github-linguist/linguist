; IDL code to plot graph given on Wikipedia at
; http://en.wikipedia.org/wiki/File:Random-data-plus-trend-r2.png
; Written by William M. Connolley and released under the GNU FDL
n=1000

data=10*randomn(seed,n)+indgen(n)/100.
y=indgen(n)
y1=y(indgen(n/10)*10+5)
y2=y(indgen(n/100)*100+5*10)

ret=pp_regress(y,data)
print,reg_explain(ret)

data1=reform(data,10,n/10)
data1=avg(data1,0)

ret1=pp_regress(y1,data1)
print,reg_explain(ret1)

data2=reform(data,100,n/100)
data2=avg(data2,0)

ret2=pp_regress(y2,data2)
print,reg_explain(ret2)

plot,y,data,yr=[-20,30]
pp_regress_plot,ret,th=3

oplot,y1,data1,col=2,th=3
oplot,y2,data2,col=3,th=3