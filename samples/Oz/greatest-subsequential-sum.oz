declare
  fun {MaxSubSeq Xs}

     fun {Step [Sum0 Seq0 MaxSum MaxSeq] X}
        Sum = Sum0 + X
        Seq = X|Seq0
     in
        if Sum > MaxSum then
           %% found new maximum
           [Sum Seq Sum Seq]
        elseif Sum < 0 then
           %% discard negative subseqs
           [0 nil MaxSum MaxSeq]
        else
           [Sum Seq MaxSum MaxSeq]
        end
     end

     [_ _ _ MaxSeq] = {FoldL Xs Step [0 nil 0 nil]}
  in
     {Reverse MaxSeq}
  end
in
  {Show {MaxSubSeq [~1 ~2 3 5 6 ~2 ~1 4 ~4 2 1]}}
