-- (c) 2009 Shafqat Virk under LGPL

concrete FoodsUrd of Foods = {

  flags coding=utf8 ;

  
	  param Number = Sg | Pl ;
        param Gender = Masc | Fem;
	  
	 oper coupla : Number -> Str =\n -> case n of {Sg => "ہے" ; Pl => "ہیں"};
	  
	 	  
      lincat
        Comment = {s : Str} ; 
	  Item = {s: Str ; n: Number ; g:Gender};
	  Kind = {s: Number => Str ; g:Gender};
        Quality = {s: Gender => Number => Str};
  
   lin
    Pred item quality = {s = item.s ++ quality.s ! item.g ! item.n ++  coupla item.n} ;
	This kind = {s = "یھ" ++ kind.s ! Sg; n= Sg ; g = kind.g } ;
	These kind = {s = "یھ" ++ kind.s ! Pl; n = Pl ; g = kind.g} ;
    That kind = {s = "وہ"  ++ kind.s ! Sg; n= Sg ; g = kind.g} ;
	Those kind = {s = "وہ" ++ kind.s ! Pl; n=Pl ; g = kind.g} ;
    Mod quality kind = {s = \\n => quality.s ! kind.g ! n  ++ kind.s ! n ;  g = kind.g};
	Wine = {s = table { Sg => "شراب" ; Pl => "شرابیں"} ; g = Fem};
    Cheese = {s = table { Sg => "پنیر" ; Pl => "پنیریں"} ; g = Fem};
    Fish = {s = table { Sg => "مچھلی" ; Pl => "مچھلیاں"} ; g = Fem};
    Pizza = {s = table { Sg => "پیزہ" ; Pl => "پیزے"} ; g = Masc};
	Very quality = {s = \\g,n => "بہت" ++ quality.s ! g ! n} ;
    Fresh = regAdj "تازہ" ;
    Warm = regAdj "گرم" ;
    Italian = regAdj "اٹا لوی" ;
    Expensive = regAdj "مہنگا" ;
    Delicious = regAdj "مزیدار" ;
    Boring = regAdj "فضول" ;
   
   oper
    regAdj : Str -> {s: Gender => Number => Str} = \a -> case a of {
        x + "ا" => mkAdj a (x+"ے") (x+"ی");
        _      => mkAdj a a a
       };
    mkAdj : Str -> Str -> Str -> {s: Gender => Number => Str} = \s,p,f -> {
      s = table {
           Masc => table {
              Sg => s;
              Pl => p
              };
           Fem => \\_ => f
         }
       };
	}  