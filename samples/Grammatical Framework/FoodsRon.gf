-- (c) 2009 Ramona Enache under LGPL

concrete FoodsRon of Foods = 
{
flags coding=utf8 ;

param Number = Sg | Pl ;
      Gender = Masc | Fem ;
      NGender = NMasc | NFem | NNeut ;
lincat 
Comment = {s : Str};
Quality = {s : Number => Gender => Str};
Kind = {s : Number => Str; g : NGender};
Item = {s : Str ; n : Number; g : Gender};

lin

This = det Sg (mkTab "acest" "această");
That = det Sg (mkTab "acel" "acea");
These = det Pl (mkTab "acești" "aceste");
Those = det Pl (mkTab "acei" "acele");

Wine = mkNoun "vin" "vinuri" NNeut ;
Cheese = mkNoun "brânză" "brânzeturi" NFem ;
Fish = mkNoun "peşte" "peşti" NMasc ;
Pizza = mkNoun "pizza" "pizze" NFem;

Very a = {s = \\n,g => "foarte" ++ a.s ! n ! g};

Fresh = mkAdj "proaspăt" "proaspătă" "proaspeţi" "proaspete" ;
Warm = mkAdj "cald" "caldă" "calzi" "calde" ;
Italian = mkAdj "italian" "italiană" "italieni" "italiene" ;
Expensive = mkAdj "scump" "scumpă" "scumpi" "scumpe" ;
Delicious = mkAdj "delicios" "delcioasă" "delicioşi" "delicioase" ;
Boring = mkAdj "plictisitor" "plictisitoare" "plictisitori" "plictisitoare" ;

Pred item quality = {s = item.s ++ copula ! item.n ++ quality.s ! item.n ! item.g} ;

Mod quality kind = {s = \\n => kind.s ! n ++ quality.s ! n ! (getAgrGender kind.g n) ; g = kind.g};

oper 

mkTab : Str -> Str -> {s : Gender => Str} = \acesta, aceasta ->
{s = table{Masc => acesta;
      Fem  => aceasta}};

det : Number -> {s : Gender => Str} -> {s : Number => Str ; g : NGender} -> {s : Str; n : Number; g : Gender} =
\n,det,noun -> let gg = getAgrGender noun.g n 
  in
   {s =  det.s ! gg  ++ noun.s ! n  ; n = n ; g = gg};

mkNoun : Str -> Str -> NGender -> {s : Number => Str; g : NGender} = \peste, pesti,g ->
{s = table {Sg => peste;
            Pl => pesti};
 g = g
};

oper mkAdj : (x1,_,_,x4 : Str) -> {s : Number => Gender => Str} = \scump, scumpa, scumpi, scumpe ->
{s = \\n,g => case <n,g> of
{<Sg,Masc> => scump ; <Sg,Fem> => scumpa;
<Pl,Masc> => scumpi ; <Pl,Fem> => scumpe
}};

copula : Number => Str = table {Sg => "este" ; Pl => "sunt"};

getAgrGender : NGender -> Number -> Gender = \ng,n ->
case <ng,n> of
{<NMasc,_> => Masc ; <NFem,_> => Fem;
<NNeut,Sg> => Masc ; <NNeut,Pl> => Fem
};

}
