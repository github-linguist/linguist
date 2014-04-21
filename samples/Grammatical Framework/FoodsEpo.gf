-- (c) 2009 Julia Hammar under LGPL

concrete FoodsEpo of Foods = open Prelude in {

  flags coding =utf8 ;

  lincat
    Comment = SS ; 
    Kind, Quality = {s : Number => Str} ; 
    Item = {s : Str ; n : Number} ; 

  lin
    Pred item quality = ss (item.s ++ copula ! item.n ++ quality.s ! item.n) ;
    This = det Sg "ĉi tiu" ;
    That  = det Sg "tiu" ;
    These = det Pl "ĉi tiuj" ;
    Those = det Pl "tiuj" ;
    Mod quality kind = {s = \\n => quality.s ! n ++ kind.s ! n} ;
    Wine = regNoun "vino" ;
    Cheese = regNoun "fromaĝo" ;
    Fish = regNoun "fiŝo" ;
    Pizza = regNoun "pico" ;
    Very quality = {s = \\n => "tre" ++ quality.s ! n} ;
    Fresh = regAdj "freŝa" ;
    Warm = regAdj "varma" ;
    Italian = regAdj "itala" ;
    Expensive = regAdj "altekosta" ;
    Delicious = regAdj "bongusta" ;
    Boring = regAdj "enuiga" ;

  param
    Number = Sg | Pl ;

  oper
    det : Number -> Str -> {s : Number => Str} -> {s : Str ; n : Number} = 
      \n,d,cn -> {
        s = d ++ cn.s ! n ;
        n = n
      } ;
    regNoun : Str -> {s : Number => Str} = 
      \vino -> {s = table {Sg => vino ; Pl => vino + "j"}
	} ;
    regAdj : Str -> {s : Number => Str} =
      \nova -> {s = table {Sg => nova ; Pl => nova + "j"}
      } ;
    copula : Number => Str = \\_ => "estas" ;
}
    
