declare
  fun {BinomialCoeff N K}
     {List.foldL {List.number 1 K 1}
      fun {$ Z I}
         Z * (N-I+1) div I
      end
      1}
  end
in
  {Show {BinomialCoeff 5 3}}
