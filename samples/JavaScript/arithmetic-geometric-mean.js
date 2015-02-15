function agm(a0,g0){
var an=(a0+g0)/2,gn=Math.sqrt(a0*g0);
while(Math.abs(an-gn)>tolerance){
an=(an+gn)/2,gn=Math.sqrt(an*gn)
}
return an;
}

agm(1,1/Math.sqrt(2));
