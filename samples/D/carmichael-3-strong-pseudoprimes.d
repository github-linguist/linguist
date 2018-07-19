enum mod = (in int n, in int m) pure nothrow => ((n % m) + m) % m;

bool isPrime(in uint n) pure nothrow {
  if (n == 2 || n == 3)
    return true;
  else if (n < 2 || n % 2 == 0 || n % 3 == 0)
    return false;
  for (uint div = 5, inc = 2; div ^^ 2 <= n;
     div += inc, inc = 6 - inc)
    if (n % div == 0)
      return false;
  return true;
}

void main() {
  import std.stdio;

  foreach (immutable p; 2 .. 62) {
    if (!p.isPrime) continue;
    foreach (immutable h3; 2 .. p) {
      immutable g = h3 + p;
      foreach (immutable d; 1 .. g) {
        if ((g * (p - 1)) % d != 0 || mod(-p * p, h3) != d % h3)
          continue;
        immutable q = 1 + (p - 1) * g / d;
        if (!q.isPrime) continue;
        immutable r = 1 + (p * q / h3);
        if (!r.isPrime || (q * r) % (p - 1) != 1) continue;
        writeln(p, " x ", q, " x ", r);
      }
    }
  }
}
