#!/usr/bin/env python
#four gray scaled stripes 8:16:32:64 in Python 2.7.1

from livewires import *

horiz=640; vert=480; pruh=vert/4; dpp=255.0
begin_graphics(width=horiz,height=vert,title="Gray stripes",background=Colour.black)

def ty_pruhy(each):
	hiy=each[0]*pruh; loy=hiy-pruh
	krok=horiz/each[1]; piecol=255.0/(each[1]-1)
	for x in xrange(0,each[1]):
		barva=Colour(piecol*x/dpp,piecol*x/dpp,piecol*x/dpp ); set_colour(barva)
		if each[2]:
			box(x*krok,hiy,x*krok+krok,loy,filled=1)
		else:
			box(horiz-x*krok,hiy,horiz-((x+1)*krok),loy,filled=1)

# main
source=[[4,8,True],[3,16,False],[2,32,True],[1,64,False]]
for each in source:
	ty_pruhy(each)

while keys_pressed() != [' ']: # press spacebar to close window
	pass
