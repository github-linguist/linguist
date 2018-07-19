compile_OPT IDL2

print, "input a, press enter, input b, press enter, input c, press enter"
read,a,b,c
Promt='Enter values of a,b,c and hit enter'

a0=0.0
b0=0.0
c0=0.0   ;make them floating point variables

x=-b+sqrt((b^2)-4*a*c)
y=-b-sqrt((b^2)-4*a*c)
z=2*a
d= x/z
e= y/z

print, d,e
