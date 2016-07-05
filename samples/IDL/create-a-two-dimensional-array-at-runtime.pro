read, x, prompt='Enter x size:'
read, y, prompt='Enter y size:'
d = fltarr(x,y)

d[3,4] = 5.6
print,d[3,4]
;==> outputs  5.6

delvar, d
