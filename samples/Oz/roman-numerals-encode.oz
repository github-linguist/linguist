declare
  fun {Digit X Y Z K}
     unit([X] [X X] [X X X] [X Y] [Y] [Y X] [Y X X] [Y X X X] [X Z])
     .K
  end

  fun {ToRoman X}
     if     X == 0    then ""
     elseif X < 0     then raise toRoman(negativeInput X) end
     elseif X >= 1000 then "M"#{ToRoman X-1000}
     elseif X >= 100  then {Digit &C &D &M  X div 100}#{ToRoman X mod 100}
     elseif X >= 10   then {Digit &X &L &C  X div 10}#{ToRoman X mod 10}
     else                  {Digit &I &V &X  X}
     end
  end
in
  {ForAll {Map [1999 25 944] ToRoman} System.showInfo}
