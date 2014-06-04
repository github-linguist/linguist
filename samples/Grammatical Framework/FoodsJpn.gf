--# -path=.:../lib/src/prelude

-- (c) 2009 Zofia Stankiewicz under LGPL

concrete FoodsJpn of Foods = open Prelude in {

flags coding=utf8 ;

  lincat
    Comment = {s: Style => Str};
    Quality = {s: AdjUse => Str ; t: AdjType} ;
    Kind = {s : Number => Str} ; 
    Item = {s : Str ; n : Number} ; 

  lin
    Pred item quality = {s = case quality.t of {
      IAdj => table {Plain => item.s ++ quality.s ! APred ; Polite => item.s ++ quality.s ! APred ++ copula ! Polite ! item.n } ;
      NaAdj => \\p => item.s ++ quality.s ! APred ++ copula ! p ! item.n } 
      } ;
    This  = det Sg "この" ;
    That  = det Sg "その" ;
    These = det Pl "この" ;
    Those = det Pl "その" ;
    Mod quality kind = {s = \\n => quality.s ! Attr ++ kind.s ! n} ;
    Wine = regNoun "ワインは" ;
    Cheese = regNoun "チーズは" ;
    Fish = regNoun "魚は" ;
    Pizza = regNoun "ピザは" ;
    Very quality =  {s = \\a => "とても" ++ quality.s ! a ; t = quality.t } ;
    Fresh = adj "新鮮な" "新鮮";
    Warm = regAdj "あたたかい" ;
    Italian = adj "イタリアの" "イタリアのもの";
    Expensive = regAdj "たかい" ;
    Delicious = regAdj "おいしい" ;
    Boring = regAdj "つまらない" ;

  param
    Number = Sg | Pl ;
    AdjUse = Attr | APred ;        -- na-adjectives have different forms as noun attributes and predicates
    Style = Plain | Polite ;      -- for phrase types
    AdjType = IAdj | NaAdj ;      -- IAdj can form predicates without the copula, NaAdj cannot

  oper
    det : Number -> Str -> {s : Number => Str} -> {s : Str ; n : Number} = 
      \n,d,cn -> {
        s = d ++ cn.s ! n ;
        n = n
      } ;
   noun : Str -> Str -> {s : Number => Str} = 
      \sakana,sakana -> {s = \\_ => sakana } ;

   regNoun : Str -> {s : Number => Str} = 
      \sakana -> noun sakana sakana ;

   adj : Str -> Str -> {s : AdjUse => Str ; t : AdjType} =
      \chosenna, chosen -> {
       s = table { 
         Attr => chosenna ;
         APred => chosen
       } ;
       t = NaAdj 
      } ;
   
   regAdj : Str -> {s: AdjUse => Str ; t : AdjType} =\akai -> {
     s = \\_ => akai ; t = IAdj} ;

    copula : Style => Number => Str = 
      table { 
        Plain => \\_ => "だ" ;
        Polite => \\_ => "です" } ;

}   
