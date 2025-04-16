// RUN: %boogie "%s" > "%t"
// RUN: %diff "%s.expect" "%t"
// A Boogie version of Turing's additive factorial program, from "Checking a large routine"
// published in the "Report of a Conference of High Speed Automatic Calculating Machines",
// pp. 67-69, 1949.

procedure ComputeFactorial(n: int) returns (u: int)
  requires 1 <= n;
  ensures u == Factorial(n);
{
  var r, v, s: int;
  r, u := 1, 1;
TOP:  // B
  assert 1 <= r && r <= n;
  assert 1 <= u;
  assert u == Factorial(r);
  v := u;
  if (n <= r) { return; }
  s := 1;
INNER:  // E
  assert s <= r;
  assert v == Factorial(r) && u == s * Factorial(r);
  u := u + v;
  s := s + 1;
  assert s - 1 <= r;
  if (s <= r) { goto INNER; }
  r := r + 1;
  goto TOP;
}

function Factorial(int): int;
axiom Factorial(0) == 1;
axiom (forall n: int :: {Factorial(n)}  1 <= n ==> Factorial(n) == n * Factorial_Aux(n-1));

function Factorial_Aux(int): int;
axiom (forall n: int :: {Factorial(n)} Factorial(n) == Factorial_Aux(n));
