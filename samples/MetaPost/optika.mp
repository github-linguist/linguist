prologues      := 3;

color yellowColor;
yellowColor := (0.4, 0.8, 0.6);

primarydef pct along pat = 
  (arctime (pct * (arclength pat)) of pat) of pat
enddef;

primarydef len on pat =
  (arctime len of pat) of pat 
enddef;

beginfig(1);

path p;
p = (0,0)--(0,1in)--(1in,1in)--(1in,0)--cycle;
fill p withcolor yellowColor;

z0=(2.5in,1in);
z1=(-5in,2in);
z9=(0.5in,10in);

z2=(0.7in,0.1in);
z3=point (arctime 0.4in of (z2--z1)) of (z2--z1);
% temporary:
z4=point (arctime 0.2in of (z2--z1)) of (z2--z1);
z5=point (arctime 0.4in of (z4--z9)) of (z4--z9);

draw z2--z3--z5--cycle;

z6=point (arctime 0.2in of (z2--z0)) of (z2--z0);
z7=point (arctime 0.2in of (z3--z0)) of (z3--z0);
z8=point (arctime 0.2in of (z5--z0)) of (z5--z0);

draw z6--z7--z8--cycle;

draw z2--z6;
draw z3--z7;
draw z5--z8;

endfig;

end
