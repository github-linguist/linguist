function sum(a,b,fn) {
   var s = 0;
   for ( ; a <= b; a++) s += fn(a);
   return s;
}

 sum(1,1000, function(x) { return 1/(x*x) } )  // 1.64393456668156
