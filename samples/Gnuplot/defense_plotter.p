#
# defense_plotter.p
#
# This is a gnuplot script that creates the graphs for defense values.
#
# Defense values are from:
#
#   http://wiki.starsautohost.org/wiki/Guts_of_bombing
#   Author:  Leonard Dickens
#   Date:  1998/07/17
#   Forums:  rec.games.computer.stars
#
# :author: Brandon Arrendondo
# :license: MIT
#
set terminal pngcairo transparent truecolor font "Arial Bold,10" size 350, 250
set output "out.png"

set title "Shield Coverage vs. Defense Quantity"
set grid
set xrange [0:100]
set xlabel "Number of Defenses"

set format y '%2.0f%%'
set yrange [0:100]

set key right bottom

sdi = 0.0099
missile_battery = 0.0199
laser_battery = 0.0239
planetary_shield = 0.0299
neutron_shield = 0.0379

val = neutron_shield

f(x) = (1.0 - ((1.0 - (val)) ** x)) * 100.0
g(x) = (1.0 - ((1.0 - (val/2.0)) ** x)) * 100.0

plot f(x) title "Standard" lt rgb "#000080", g(x) title "Smart" lt rgb "#000000"
