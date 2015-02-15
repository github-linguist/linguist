void main() {
  import std.stdio, std.random, std.algorithm, std.range, std.ascii;

  auto d9 = "123456789"d.dup;
  auto choices = cartesianProduct(d9, d9, d9, d9).map!(t => [t[]])
                 .filter!(a => a.sort().uniq.count == 4).array;

   do {
      const ans = choices[uniform(0, $)];
      writef("My guess is %s. How many bulls and cows? ", ans);
      immutable score = readln.filter!isDigit.map!q{ a - '0' }.array;
      choices = choices.remove!(c => score !=
        [c.zip(ans).count!(p => p[0] == p[1]),
         c.zip(ans).count!(p => p[0] != p[1] && ans.canFind(p[0]))]);
   } while (choices.length > 1);

   if (choices.empty)
      return "Nothing fits the scores you gave.".writeln;
   writeln("Solution found: ", choices[0]);
}
