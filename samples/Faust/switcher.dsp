declare name 		"switcher";
declare version 	"1.0";
declare author 		"Grame";
declare license 	"BSD";
declare copyright 	"(c)GRAME 2007";

//-----------------------------------------------
// Switch between two stereo sources. 
// Useful to compare these two sources
// The parameter c\in{0,1} indicates the 
// channels to select
//-----------------------------------------------

switch(c,x0,x1,y0,y1) = sel(c,x0,y0), sel(c,x1,y1)
							with { 
								sel(c,x,y) = (1-c)*x + c*y; 
							};
	
process = switch(hslider("source 0 <-> source 1",0,0,1,1));

				 

