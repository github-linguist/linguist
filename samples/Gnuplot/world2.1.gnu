# set terminal pngcairo  transparent enhanced font "arial,10" fontscale 1.0 size 500, 350 
# set output 'world2.1.png'
unset border
set dummy u,v
set angles degrees
set parametric
set view 60, 136, 1.22, 1.26
set samples 64, 64
set isosamples 13, 13
set mapping spherical
set noxtics
set noytics
set noztics
set title "Labels colored by GeV plotted in spherical coordinate system" 
set urange [ -90.0000 : 90.0000 ] noreverse nowriteback
set vrange [ 0.00000 : 360.000 ] noreverse nowriteback
set cblabel "GeV" 
set cbrange [ 0.00000 : 8.00000 ] noreverse nowriteback
set colorbox user
set colorbox vertical origin screen 0.9, 0.2, 0 size screen 0.02, 0.75, 0 front bdefault
splot cos(u)*cos(v),cos(u)*sin(v),sin(u) notitle with lines lt 5,       'world.dat' notitle with lines lt 2,       'srl.dat' using 3:2:(1):1:4 with labels notitle point pt 6 lw .1 left offset 1,0 font "Helvetica,7" tc pal
