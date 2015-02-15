declare
  %% Lazy list of indices of Y in Xs.
  fun {Indices Y Xs}
     for
        X in Xs
        I in 1;I+1
        yield:Yield
     do
        if Y == X then {Yield I} end
     end
  end

  fun {Index Y Xs}
     case {Indices Y Xs} of X|_ then X
     else raise index(elementNotFound Y) end
     end
  end

  Haystack = ["Zig" "Zag" "Wally" "Ronald" "Bush" "Krusty" "Charlie" "Bush" "Bozo"]
in
  {Show {Index "Bush" Haystack}}
  {Show {List.last {Indices "Bush" Haystack}}}

  {Show {Index "Washington" Haystack}} %% throws
