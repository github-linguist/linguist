declare
  fun {Horner Coeffs X}
     {FoldL1 {Reverse Coeffs}
      fun {$ Acc Coeff}
         Acc*X + Coeff
      end}
  end

  fun {FoldL1 X|Xr Fun}
     {FoldL Xr Fun X}
  end
in
  {Show {Horner [~19 7 ~4 6] 3}}
