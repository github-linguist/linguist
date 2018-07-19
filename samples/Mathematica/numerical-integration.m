leftRect[f_, a_Real, b_Real, N_Integer] :=
 Module[{sum = 0, dx = (b - a)/N, x = a, n = N} ,
  For[n = N, n > 0, n--, x += dx; sum += f[x];];
  Return [ sum*dx ]]

rightRect[f_, a_Real, b_Real, N_Integer] :=
 Module[{sum = 0, dx = (b - a)/N, x = a + (b - a)/N, n = N} ,
  For[n = N, n > 0, n--, x += dx; sum += f[x];];
  Return [ sum*dx ]]

midRect[f_, a_Real, b_Real, N_Integer] :=
 Module[{sum = 0, dx = (b - a)/N, x = a + (b - a)/(2 N), n = N} ,
  For[n = N, n > 0, n--, x += dx; sum += f[x];];
  Return [ sum*dx ]]

trapezium[f_, a_Real, b_Real, N_Integer] :=
 Module[{sum = f[a], dx = (b - a)/N, x = a, n = N} ,
  For[n = 1, n < N, n++, x += dx; sum += 2 f[x];];
  sum += f[b];
  Return [ 0.5*sum*dx ]]

simpson[f_, a_Real, b_Real, N_Integer] :=
 Module[{sum1 = f[a + (b - a)/(2 N)], sum2 = 0, dx = (b - a)/N, x = a, n = N} ,
  For[n = 1, n < N, n++, sum1 += f[a + dx*n + dx/2];
   sum2 += f[a + dx*n];];
  Return [(dx/6)*(f[a] + f[b] + 4*sum1 + 2*sum2)]]
