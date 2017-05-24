/day:(day;year)
dy:{"D"$string[y],x}

/residue
r:{y-x*y div x}

/adjust sat/sun
a:{d+0^(x,1)r[7]d:dy[y]z}

/goto dayofweek
b:{d+r[7]x-d:dy[y]z}

/good friday(1900-2099)
g:{d+:e:r[7](6*d:r[30]24+19*a:r[19]x)+5+2*r[4;x]+2*r[7]x;dy["0320";x]+d-7*(d=35)|(d=34)&(e=6)&a>10}

/nyse holidays
nyse:(a[2]"0101";b[2]"0115";b[2]"0215";g;b[2]"0525";a[-1]"0704";b[2]"0901";b[5]"1122";a[-1]"1225")

nyse@/:\:2007 2008