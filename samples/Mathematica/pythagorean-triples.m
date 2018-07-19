pythag[n_] := Block[{soln = Solve[{a^2 + b^2 == c^2, a + b + c <= n, 0 < a < b < c}, {a, b, c}, Integers]},
        {Length[soln], Count[GCD[a, b] == GCD[b, c] == GCD[c, a] == 1 /. soln, True]}
      ]
