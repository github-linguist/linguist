#!/usr/bin/env gnuplot

reset

set terminal png
set output 'rates100.png'

set xlabel "A2A price"
set ylabel "Response Rate"

#set xr [0:5]
#set yr [0:6]

plot 'rates100.dat' pt 7 notitle
