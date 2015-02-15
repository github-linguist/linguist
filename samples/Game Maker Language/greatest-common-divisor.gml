 var n,m,r;
 n = max(argument0,argument1);
 m = min(argument0,argument1);
 while (m != 0)
 {
  r = n mod m;
  n = m;
  m = r;
 }
 return a;
