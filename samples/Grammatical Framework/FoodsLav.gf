--# -path=.:prelude

-- (c) 2009 Inese Bernsone under LGPL

concrete FoodsLav of Foods = open Prelude in {

  flags
    coding=utf8 ;
    
  lincat
    Comment = SS ; 
    Quality = {s : Q => Gender => Number => Defin => Str } ; 
    Kind = {s : Number => Str ; g : Gender} ; 
    Item = {s : Str ; g : Gender ; n : Number } ; 

  lin
    Pred item quality = ss (item.s ++ {- copula item.n -} "ir" ++ quality.s ! Q1 ! item.g ! item.n ! Ind ) ;
      This  = det Sg "šis" "šī" ;
      That  = det Sg "tas" "tā" ;
      These = det Pl "šie" "šīs" ;
      Those = det Pl "tie" "tās" ;
    Mod quality kind = {s = \\n => quality.s ! Q1 ! kind.g ! n ! Def ++ kind.s ! n ; g = kind.g } ;
      Wine = noun "vīns" "vīni" Masc ;
      Cheese = noun "siers" "sieri" Masc ;
      Fish = noun "zivs" "zivis" Fem ;
      Pizza = noun "pica" "picas" Fem ;
    Very qual = {s = \\q,g,n,spec => "ļoti" ++ qual.s ! Q2 ! g ! n ! spec }; 
 
      Fresh = adjective "svaigs" "svaiga" "svaigi" "svaigas" "svaigais" "svaigā" "svaigie" "svaigās" ;
      Warm = regAdj "silts" ;
      Italian = specAdj "itāļu" (regAdj "itālisks") ;
      Expensive = regAdj "dārgs" ;
      Delicious = regAdj "garšīgs" ;
      Boring = regAdj "garlaicīgs" ;

  param
    Number = Sg | Pl ;
    Gender = Masc | Fem ;
    Defin = Ind | Def ;
    Q = Q1 | Q2 ;

  oper
   det : Number -> Str -> Str -> {s : Number => Str ; g : Gender} -> 
        {s : Str ; g : Gender ; n : Number} = 
      \n,m,f,cn -> {
        s = case cn.g of {Masc => m ; Fem => f} ++ cn.s ! n ;
        g = cn.g ;
        n = n
      } ;
    noun : Str -> Str -> Gender -> {s : Number => Str ; g : Gender} = 
      \man,men,g -> {
        s = table {
          Sg => man ;
          Pl => men 
          } ;
        g = g
      } ;
    adjective : (_,_,_,_,_,_,_,_ : Str) -> {s : Q => Gender => Number => Defin => Str} = 
      \skaists,skaista,skaisti,skaistas,skaistais,skaistaa,skaistie,skaistaas -> {
        s = table {
		  _ => table {
            Masc => table {
              Sg => table {Ind => skaists ; Def => skaistais} ;
              Pl => table {Ind => skaisti ; Def => skaistie}
              } ; 
            Fem => table {
              Sg => table {Ind => skaista ; Def => skaistaa} ;
              Pl => table {Ind => skaistas ; Def => skaistaas} 
              }
            } 
		  }
        } ;
      
 {-   irregAdj : Str -> {s : Gender => Number => Defin => Str} = \itaalju ->
     let itaalju = itaalju
     in adjective itaalju (itaalju) (itaalju) (itaalju) (itaalju) (itaalju) (itaalju) (itaalju) ; -}
     
    regAdj : Str -> {s : Q => Gender => Number => Defin => Str} = \skaists ->
      let skaist = init skaists 
      in adjective skaists (skaist + "a") (skaist + "i") (skaist + "as") (skaist + "ais") (skaist + "ā") (skaist + "ie") (skaist + "ās");

    Adjective : Type = {s : Q => Gender => Number => Defin => Str} ;

	specAdj : Str -> Adjective -> Adjective = \s,a -> {
      s = table {
        Q2 => a.s ! Q1 ;
        Q1 => \\_,_,_ => s
        }		
	  } ;
	
	}
