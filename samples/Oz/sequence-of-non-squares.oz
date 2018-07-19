declare
  fun {NonSqr N}
     N + {Float.toInt {Floor 0.5 + {Sqrt {Int.toFloat N}}}}
  end

  fun {SqrtInt N}
     {Float.toInt {Sqrt {Int.toFloat N}}}
  end

  fun {IsSquare N}
     {Pow {SqrtInt N} 2} == N
  end

  Ns = {Map {List.number 1 999999 1} NonSqr}
in
  {Show {List.take Ns 22}}
  {Show {Some Ns IsSquare}}
