# set terminal pngcairo  transparent enhanced font "arial,10" fontscale 1.0 size 500, 350 
# set output 'surface1.16.png'
set dummy u,v
set label 1 "increasing v" at 6, 0, -1 left norotate back nopoint offset character 0, 0, 0
set label 2 "u=0" at 5, 6.5, -1 left norotate back nopoint offset character 0, 0, 0
set label 3 "u=1" at 5, 6.5, 0.100248 left norotate back nopoint offset character 0, 0, 0
set arrow 1 from 5, -5, -1.2 to 5, 5, -1.2 head back nofilled linetype -1 linewidth 1.000
set arrow 2 from 5, 6, -1 to 5, 5, -1 head back nofilled linetype -1 linewidth 1.000
set arrow 3 from 5, 6, 0.100248 to 5, 5, 0.100248 head back nofilled linetype -1 linewidth 1.000
set parametric
set view 70, 20, 1, 1
set samples 51, 51
set isosamples 2, 33
set hidden3d back offset 1 trianglepattern 3 undefined 1 altdiagonal bentover
set ztics -1.00000,0.25,1.00000 norangelimit
set title "\"fence plot\" using separate parametric surfaces" 
set xlabel "X axis" 
set xlabel  offset character -3, -2, 0 font "" textcolor lt -1 norotate
set xrange [ -5.00000 : 5.00000 ] noreverse nowriteback
set ylabel "Y axis" 
set ylabel  offset character 3, -2, 0 font "" textcolor lt -1 rotate by -270
set yrange [ -5.00000 : 5.00000 ] noreverse nowriteback
set zlabel "Z axis" 
set zlabel  offset character -5, 0, 0 font "" textcolor lt -1 norotate
set zrange [ -1.00000 : 1.00000 ] noreverse nowriteback
sinc(u,v) = sin(sqrt(u**2+v**2)) / sqrt(u**2+v**2)
GPFUN_sinc = "sinc(u,v) = sin(sqrt(u**2+v**2)) / sqrt(u**2+v**2)"
xx = 6.08888888888889
dx = 1.10888888888889
x0 = -5
x1 = -3.89111111111111
x2 = -2.78222222222222
x3 = -1.67333333333333
x4 = -0.564444444444444
x5 = 0.544444444444445
x6 = 1.65333333333333
x7 = 2.76222222222222
x8 = 3.87111111111111
x9 = 4.98
splot [u=0:1][v=-4.99:4.99] 	x0, v, (u<0.5) ? -1 : sinc(x0,v) notitle, 	x1, v, (u<0.5) ? -1 : sinc(x1,v) notitle, 	x2, v, (u<0.5) ? -1 : sinc(x2,v) notitle, 	x3, v, (u<0.5) ? -1 : sinc(x3,v) notitle, 	x4, v, (u<0.5) ? -1 : sinc(x4,v) notitle, 	x5, v, (u<0.5) ? -1 : sinc(x5,v) notitle, 	x6, v, (u<0.5) ? -1 : sinc(x6,v) notitle, 	x7, v, (u<0.5) ? -1 : sinc(x7,v) notitle, 	x8, v, (u<0.5) ? -1 : sinc(x8,v) notitle, 	x9, v, (u<0.5) ? -1 : sinc(x9,v) notitle
