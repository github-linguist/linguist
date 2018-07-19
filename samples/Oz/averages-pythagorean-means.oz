declare
  %% helpers
  fun {Sum Xs} {FoldL Xs Number.'+' 0.0} end
  fun {Product Xs} {FoldL Xs Number.'*' 1.0} end
  fun {Len Xs} {Int.toFloat {Length Xs}} end

  fun {AMean Xs}
     {Sum Xs}
     /
     {Len Xs}
  end

  fun {GMean Xs}
     {Pow
      {Product Xs}
      1.0/{Len Xs}}
  end

  fun {HMean Xs}
     {Len Xs}
     /
     {Sum {Map Xs fun {$ X} 1.0 / X end}}
  end

  Numbers = {Map {List.number 1 10 1} Int.toFloat}

  [A G H] = [{AMean Numbers} {GMean Numbers} {HMean Numbers}]
in
  {Show [A G H]}
  A >= G = true
  G >= H = true
