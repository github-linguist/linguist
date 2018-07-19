declare
  fun {Distance X1#Y1 X2#Y2}
     {Sqrt {Pow X2-X1 2.0} + {Pow Y2-Y1 2.0}}
  end

  %% brute force
  fun {BFClosestPair Points=P1|P2|_}
     Ps = {List.toTuple unit Points} %% for efficient random access
     N = {Width Ps}
     MinDist = {NewCell {Distance P1 P2}}
     MinPoints = {NewCell P1#P2}
  in
     for I in 1..N-1 do
        for J in I+1..N do
           IJDist = {Distance Ps.I Ps.J}
        in
           if IJDist < @MinDist then
              MinDist := IJDist
              MinPoints := Ps.I#Ps.J
           end
        end
     end
     @MinPoints
  end

  %% divide and conquer
  fun {ClosestPair Points}
     case {ClosestPair2
           {Sort Points {LessThanBy X}}
           {Sort Points {LessThanBy Y}}}
     of Distance#Pair then
        Pair
     end
  end

  %% XP: points sorted by X, YP: sorted by Y
  %% returns a pair Distance#Pair
  fun {ClosestPair2 XP YP}
     N = {Length XP} = {Length YP}
  in
     if N =< 3 then
        P = {BFClosestPair XP}
     in
        {Distance P.1 P.2}#P
     else
        XL XR
        {List.takeDrop XP (N div 2) ?XL ?XR}
        XM = {Nth XP (N div 2)}.X
        YL YR
        {List.partition YP fun {$ P} P.X =< XM end ?YL ?YR}
        DL#PairL = {ClosestPair2 XL YL}
        DR#PairR = {ClosestPair2 XR YR}
        DMin#PairMin = if DL < DR then DL#PairL else DR#PairR end
        YSList = {Filter YP fun {$ P} {Abs XM-P.X} < DMin end}
        YS = {List.toTuple unit YSList} %% for efficient random access
        NS = {Width YS}
        Closest = {NewCell DMin}
        ClosestPair = {NewCell PairMin}
     in
        for I in 1..NS-1 do
           for K in I+1..NS while:YS.K.Y - YS.I.Y < DMin do
              DistKI = {Distance YS.K YS.I}
           in
              if DistKI < @Closest then
                 Closest := DistKI
                 ClosestPair := YS.K#YS.I
              end
           end
        end
        @Closest#@ClosestPair
     end
  end

  %% To access components when points are represented as pairs
  X = 1
  Y = 2

  %% returns a less-than predicate that accesses feature F
  fun {LessThanBy F}
     fun {$ A B}
        A.F < B.F
     end
  end

  fun {Random Min Max}
     Min +
     {Int.toFloat {OS.rand}} * (Max-Min)
     / {Int.toFloat {OS.randLimits _}}
  end

  fun {RandomPoint}
     {Random 0.0 100.0}#{Random 0.0 100.0}
  end

  Points = {MakeList 5}
in
  {ForAll Points RandomPoint}
  {Show Points}
  {Show {ClosestPair Points}}
