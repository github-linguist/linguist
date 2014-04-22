-- (c) 2011 Katerina Bohmova under LGPL

resource ResCze = open Prelude in {
  flags 
    coding = utf8 ;
  param
    Number = Sg | Pl ;
    Gender = Masc | Fem | Neutr;
  oper
    NounPhrase : Type = 
      {s : Str ; g : Gender ; n : Number} ; 
    Noun : Type = {s : Number => Str ; g : Gender} ;
    Adjective : Type = {s : Gender => Number => Str} ;

    det : Number -> Str -> Str -> Str -> Noun -> NounPhrase =
      \n,m,f,ne,cn -> {
        s = table {Masc => m ; Fem => f; Neutr => ne} ! cn.g ++ 
            cn.s ! n ;
        g = cn.g ;
        n = n
      } ;
    noun : Str -> Str -> Gender -> Noun =
      \muz,muzi,g -> {
        s = table {Sg => muz ; Pl => muzi} ;
        g = g
      } ;
    adjective : (msg,fsg,nsg,mpl,fpl,npl : Str) -> Adjective =
      \msg,fsg,nsg,mpl,fpl,npl -> {
        s = table {
          Masc => table {Sg => msg ; Pl => mpl} ; 
          Fem => table {Sg => fsg ; Pl => fpl}  ;
          Neutr => table {Sg => nsg ; Pl => npl}
          }
        } ;
    regAdj : Str -> Adjective = 
      \mlad ->
      adjective (mlad+"ý") (mlad+"á") (mlad+"é")
		 (mlad+"é") (mlad+"é") (mlad+"á") ;
    regnfAdj : Str -> Adjective = 
      \vynikajici ->
      adjective vynikajici vynikajici vynikajici 
		vynikajici vynikajici vynikajici;
    copula : Number => Str = 
      table {Sg => "je" ; Pl => "jsou"} ;
}

