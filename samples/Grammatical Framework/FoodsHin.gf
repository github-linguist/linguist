-- (c) 2010 Vikash Rauniyar under LGPL

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
    This kind = {s = "यह" ++ kind.s ! Sg ; g = kind.g ; n = Sg} ;
    That kind = {s = "वह" ++ kind.s ! Sg ; g = kind.g ; n = Sg} ;
    These kind = {s = "ये" ++ kind.s ! Pl ; g = kind.g ; n = Pl} ;
    Those kind = {s = "वे" ++ kind.s ! Pl ; g = kind.g ; n = Pl} ;
    Mod quality kind = {
      s = \\n => quality.s ! kind.g ! n ++ kind.s ! n ; 
      g = kind.g
      } ;
    Wine = regN "मदिरा" ;
    Cheese = regN "पनीर" ;
    Fish = regN "मछली" ;
    Pizza = regN "पिज़्ज़ा" ;
    Very quality = {s = \\g,n => "अति" ++ quality.s ! g ! n} ;
    Fresh = regAdj "ताज़ा" ;
    Warm = regAdj "गरम" ;
    Italian = regAdj "इटली" ; 
    Expensive = regAdj "बहुमूल्य" ;
    Delicious = regAdj "स्वादिष्ट" ;
    Boring = regAdj "अरुचिकर" ;

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
      lark + "ा" => mkN s (lark + "े") Masc ;
      lark + "ी" => mkN s (lark + "ीयँा") Fem ;
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
      acch + "ा" => mkAdj a (acch + "े") (acch + "ी") ;
      _          => mkAdj a a a
      } ;

    copula : Number -> Str = \n -> case n of {
      Sg => "है" ;
      Pl => "हैं"
      } ;

  }
