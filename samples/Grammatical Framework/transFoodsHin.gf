-- (c) 2009 Aarne Ranta under LGPL

concrete FoodsHin of Foods = {

  flags coding=utf8 ;

  param
    Gender = Masc | Fem ;
    Number = Sg | Pl ;
  lincat
    Comment = {s : Str} ;
    Item    = {s : Str ; g : Gender ; n : Number} ;
    Kind    = {s : Number => Str ; g : Gender} ;
    Quality = {s : Gender => Number => Str} ;
  lin
    Pred item quality = {
      s = item.s ++ quality.s ! item.g ! item.n ++ copula item.n
      } ;
    This kind = {s = "yah" ++ kind.s ! Sg ; g = kind.g ; n = Sg} ;
    That kind = {s = "vah" ++ kind.s ! Sg ; g = kind.g ; n = Sg} ;
    These kind = {s = "ye" ++ kind.s ! Pl ; g = kind.g ; n = Pl} ;
    Those kind = {s = "ve" ++ kind.s ! Pl ; g = kind.g ; n = Pl} ;
    Mod quality kind = {
      s = \\n => quality.s ! kind.g ! n ++ kind.s ! n ; 
      g = kind.g
      } ;
    Wine = regN "madirA" ;
    Cheese = regN "panIr" ;
    Fish = regN "maClI" ;
    Pizza = regN "pijjA" ;
    Very quality = {s = \\g,n => "bahut" ++ quality.s ! g ! n} ;
    Fresh = regAdj "tAzA" ;
    Warm = regAdj "garam" ;
    Italian = regAdj "i-t.alI" ; 
    Expensive = regAdj "mahaNgA" ;
    Delicious = regAdj "rucikar" ;
    Boring = regAdj "pEriyA" ;

  oper
    mkN : Str -> Str -> Gender -> {s : Number => Str ; g : Gender} = 
      \s,p,g -> {
        s = table {
          Sg => s ;
          Pl => p
          } ;
        g = g
      } ;

    regN : Str -> {s : Number => Str ; g : Gender} = \s -> case s of {
      lark + "A" => mkN s (lark + "e") Masc ;
      lark + "I" => mkN s (lark + "iyaM") Fem ;
      _           => mkN s s Masc 
      } ;

    mkAdj : Str -> Str -> Str -> {s : Gender => Number => Str} = \ms,mp,f -> {
      s = table {
        Masc => table {
          Sg => ms ;
          Pl => mp
          } ;
        Fem  => \\_ => f
        }
      } ;

    regAdj : Str -> {s : Gender => Number => Str} = \a -> case a of {
      acch + "A" => mkAdj a (acch + "e") (acch + "I") ;
      _          => mkAdj a a a
      } ;

    copula : Number -> Str = \n -> case n of {
      Sg => "hE" ;
      Pl => "hEN"
      } ;

  }
