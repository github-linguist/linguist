USING: kernel sequences math math.combinatorics formatting io locals ;
IN: queens

: /=  ( x y -- ? )   = not ; inline

:: safe?  ( board q -- ? )
    [let  q board nth :> x
      q iota [
         x swap
         [ board nth ] keep
         q swap -
           [ + /= ]
           [ - /= ] 3bi and
      ] all?
    ] ;

: solution? ( board -- ? )
    dup length iota [ dupd safe? ] all? nip ;

: queens ( n -- l )
    iota all-permutations [ solution? ] filter ;

: .queens ( n -- )
    queens
    [
      [ 1 + "%d " printf ] each nl
    ] each ;
