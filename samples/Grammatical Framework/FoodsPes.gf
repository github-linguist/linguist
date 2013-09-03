concrete FoodsPes of Foods = {

      flags optimize=noexpand ; coding=utf8 ;
	  
      lincat
        Comment = {s : Str} ;
		Quality = {s : Add => Str; prep : Str} ;
        Kind = {s : Add => Number => Str ; prep : Str};
        Item = {s : Str ; n : Number};
      lin
        Pred item quality = {s = item.s ++ quality.s ! Indep ++ copula ! item.n} ;
        This = det Sg "این" ;
        That = det Sg "آن" ;
        These = det Pl "این" ;
        Those = det Pl "آن" ;
        
        Mod quality kind = {s = \\a,n =>  kind.s ! Attr ! n ++ kind.prep ++ quality.s ! a ;                                  
                            prep = quality.prep             
                            };
        Wine = regN "شراب" ; 
        Cheese = regN "پنیر" ;
        Fish = regN "ماهى" ;
        Pizza = regN "پیتزا" ;
        Very a = {s = \\at => "خیلی" ++ a.s ! at ; prep = a.prep} ;
        Fresh = adj "تازه" ;
        Warm = adj "گرم" ;
        Italian = adj "ایتالیایی" ;
        Expensive = adj "گران" ;
        Delicious = adj "لذىذ" ;
        Boring = adj "ملال آور" ;  -- it must be written as ملال آور. 
     
     param
        Number = Sg | Pl ;
		Add = Indep | Attr ;
     oper
        det : Number -> Str -> {s: Add => Number => Str ; prep : Str} -> {s : Str ; n: Number} =
           \n,det,noun -> {s = det ++ noun.s ! Indep ! n ; n = n };
           
        noun : (x1,_,_,x4 : Str) -> {s : Add => Number => Str ; prep : Str} = \pytzA, pytzAy, pytzAhA,pr -> 
         {s = \\a,n => case <a,n> of
		        {<Indep,Sg> => pytzA ; <Indep,Pl> => pytzAhA ;
  				 <Attr,Sg>  =>pytzA ; <Attr,Pl>  => pytzAhA + "ى" };
		 prep = pr
		 };		 
         
        regN : Str -> {s: Add => Number => Str ; prep : Str} = \mrd -> 
		case mrd of 
		{ _ + ("ا"|"ه"|"ى"|"و"|"") => noun mrd (mrd+"ى") (mrd + "ها") "";
		  _                        => noun mrd mrd (mrd + "ها") "e"
		};
        
        adj : Str -> {s : Add => Str; prep : Str} = \tAzh -> 
		case tAzh of 
		{ _ + ("ا"|"ه"|"ى"|"و"|"") => mkAdj tAzh (tAzh ++ "ى") "" ;
		  _                        => mkAdj tAzh tAzh "ه"
        };
		
		mkAdj : Str -> Str -> Str -> {s : Add => Str; prep : Str} = \tAzh, tAzhy, pr  ->
		{s = table {Indep => tAzh;
		            Attr => tAzhy};
		 prep = pr 			
		};
        copula : Number => Str = table {Sg => "است"; Pl => "هستند"};
      
}