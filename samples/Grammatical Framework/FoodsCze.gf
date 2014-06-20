-- (c) 2011 Katerina Bohmova under LGPL

concrete FoodsCze of Foods = open ResCze in {
  flags 
    coding = utf8 ;
  lincat
    Comment = {s : Str} ; 
    Quality = Adjective ; 
    Kind = Noun ; 
    Item = NounPhrase ;
  lin
    Pred item quality = 
      {s = item.s ++ copula ! item.n ++ 
           quality.s ! item.g ! item.n} ;
    This  = det Sg "tento" "tato" "toto" ;
    That  = det Sg "tamten" "tamta" "tamto" ;
    These = det Pl "tyto" "tyto" "tato" ;
    Those = det Pl "tamty" "tamty" "tamta" ;
    Mod quality kind = {
      s = \\n => quality.s ! kind.g ! n ++ kind.s ! n ;
      g = kind.g
      } ;
    Wine = noun "víno" "vína" Neutr ;
    Cheese = noun "sýr" "sýry" Masc ;
    Fish = noun "ryba" "ryby" Fem ;
    Pizza = noun "pizza" "pizzy" Fem ;
    Very qual = {s = \\g,n => "velmi" ++ qual.s ! g ! n} ;
    Fresh = regAdj "čerstv" ;
    Warm = regAdj "tepl" ;
    Italian = regAdj "italsk" ;
    Expensive = regAdj "drah" ;
    Delicious = regnfAdj "vynikající" ;
    Boring = regAdj "nudn" ;
}

