declare
  fun {UnsafeGCD A B}
     if B == 0 then
        A
     else
        {UnsafeGCD B A mod B}
     end
  end

  fun {GCD A B}
     if A == 0 andthen B == 0 then
        raise undefined(gcd 0 0) end
     else
        {UnsafeGCD {Abs A} {Abs B}}
     end
  end
in
  {Show {GCD 456 ~632}}
