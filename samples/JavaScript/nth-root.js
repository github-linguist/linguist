function nthRoot(num, nArg, precArg) {
  var n = nArg || 2;
  var prec = precArg || 12;

  var x = 1; // Initial guess.
  for (var i=0; i<prec; i++) {
    x = 1/n * ((n-1)*x + (num / Math.pow(x, n-1)));
  }

  return x;
}
