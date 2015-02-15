euler[step_, val_] := NDSolve[{T'[t] == -0.07 (T[t] - 20), T[0] == 100}, T, {t, 0, 100}, Method -> "ExplicitEuler", StartingStepSize -> step][[1, 1, 2]][val]
