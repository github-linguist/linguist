import std.stdio, std.algorithm, std.range, std.typecons, std.conv,
       std.string, permutations2, arithmetic_rational;

string solve(in int target, in int[] problem) {
  static struct ComputeAllOperations {
    //static struct T { Rational r; string e; }
    alias T = Tuple!(Rational,"r", string,"e");
    Rational[] L;

    int opApply(in int delegate(ref T) dg) {
      int result;

      if (!L.empty) {
        auto x = L[0];
        auto xs = L[1 .. $];
        if (L.length == 1) {
          T aux = T(x, text(x));
          result = dg(aux);
        } else {
          OUTER: foreach (o; ComputeAllOperations(xs)) {
            auto y = o.r;
            auto sub = [T(x * y, "*"), T(x + y, "+"), T(x - y, "-")];
            if (y) sub ~= [T(x/y, "/")];
            foreach (e; sub) {
              auto aux = T(e.r, format("(%s%s%s)", x, e.e, o.e));
              result = dg(aux); if (result) break OUTER;
            }
          }
        }
      }

      return result;
    }
  }


  foreach (p; problem.map!Rational.array.permutations)
    foreach (sol; ComputeAllOperations(p))
      if (sol.r == target)
        return sol.e;
  return "No solution";
}

void main() {
  foreach (prob; [[6, 7, 9, 5], [3, 3, 8, 8], [1, 1, 1, 1]])
    writeln(prob, ": ", solve(24, prob));
}
