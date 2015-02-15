Y = fun(M) -> (fun(X) -> X(X) end)(fun (F) -> M(fun(A) -> (F(F))(A) end) end) end.

Fac = fun (F) ->
          fun (0) -> 1;
              (N) -> N * F(N-1)
          end
      end.
Fib = fun(F) ->
          fun(0) -> 0;
             (1) -> 1;
             (N) -> F(N-1) + F(N-2)
          end
      end.
(Y(Fac))(5). %% 120
(Y(Fib))(8). %% 21
