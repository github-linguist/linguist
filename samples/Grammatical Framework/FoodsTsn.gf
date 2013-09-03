--# -path=alltenses

-- (c) 2009 Laurette Pretorius Sr & Jr and Ansu Berg under LGPL

concrete FoodsTsn of Foods = open Prelude, Predef in {
	flags coding = utf8;
	lincat
		Comment = {s:Str};
		Item = {s:Str; c:NounClass; n:Number};
		Kind = {w: Number => Str; r: Str; c: NounClass; q: Number => Str; b: Bool};
		Quality = {s: NounClass => Number => Str; p_form: Str; t: TType};
	lin	
		Pred item quality = {s = item.s ++ ((mkPredDescrCop quality.t) ! item.c ! item.n) ++ quality.p_form};

		This kind = {s = (kind.w ! Sg) ++ (mkDemPron1 ! kind.c ! Sg) ++ (kind.q ! Sg); c = kind.c; n = Sg};
		That kind = {s = (kind.w ! Sg) ++ (mkDemPron2 ! kind.c ! Sg) ++ (kind.q ! Sg); c = kind.c; n = Sg};
		These kind = {s = (kind.w ! Pl) ++ (mkDemPron1 ! kind.c ! Pl) ++ (kind.q ! Pl); c = kind.c; n = Pl};
		Those kind = {s = (kind.w ! Pl) ++ (mkDemPron2 ! kind.c ! Pl) ++ (kind.q ! Pl); c = kind.c; n = Pl};	
		
		Mod quality kind = mkMod quality kind;

	-- Lexicon
		Wine = mkNounNC14_6 "jalwa";
		Cheese = mkNounNC9_10 "kase";
		Fish = mkNounNC9_10 "thlapi";
		Pizza = mkNounNC9_10 "pizza";
		Very quality = smartVery quality; 
		Fresh = mkVarAdj "ntsha";
		Warm = mkOrdAdj "bothitho";
		Italian = mkPerAdj "Itali";
		Expensive = mkVerbRel "tura"; 
		Delicious = mkOrdAdj "monate";
		Boring = mkOrdAdj "bosula";
		
	param
		NounClass = NC9_10 | NC14_6;
		Number = Sg | Pl;
		TType = P | V | ModV | R ;				
	oper
		mkMod : {s: NounClass => Number => Str; p_form: Str; t: TType} -> {w: Number => Str; r: Str; c: NounClass; q: Number => Str; b: Bool} -> {w: Number => Str; r: Str; c: NounClass; q: Number => Str; 
		b: Bool} = \x,y -> case y.b of
		{
			True => {w = y.w; r = y.r; c = y.c; 
				q = table {
				Sg => ((y.q ! Sg) ++ "le" ++ ((smartQualRelPart (x.t)) ! y.c ! Sg) ++ ((smartDescrCop (x.t)) ! 					y.c ! Sg) ++ (x.s ! y.c ! Sg)); 
				Pl => ((y.q ! Pl) ++ "le" ++ ((smartQualRelPart (x.t))! y.c ! Pl) ++ ((smartDescrCop (x.t)) ! 					y.c ! Pl) ++(x.s ! y.c ! Pl))
				  }; b = True
				};
			False => {w = y.w; r = y.r; c = y.c; 
				q = table {
				Sg => ((y.q ! Sg) ++ ((smartQualRelPart (x.t)) ! y.c ! Sg) ++ ((smartDescrCop (x.t)) ! y.c ! Sg) 					++ (x.s ! y.c ! Sg)); 
				Pl => ((y.q ! Pl) ++ ((smartQualRelPart (x.t)) ! y.c ! Pl) ++ ((smartDescrCop (x.t)) ! y.c ! Pl) 					++(x.s ! y.c ! Pl))
				  }; b = True
				}
		};

		mkNounNC14_6 : Str -> {w: Number => Str; r: Str; c: NounClass; q: Number => Str; b: Bool} = \x -> {w = table {Sg => "bo" + x; Pl => "ma" + x}; r = x; c = NC14_6; 
		q = table {Sg => ""; Pl => ""}; b = False};
		
		mkNounNC9_10 : Str -> {w: Number => Str; r: Str; c: NounClass; q: Number => Str; b: Bool} = \x -> {w = table {Sg => "" + x; Pl => "di" + x}; r = x; c = NC9_10; 
		q = table {Sg => ""; Pl => ""}; b = False};

		mkVarAdj : Str -> {s: NounClass => Number => Str; p_form: Str; t: TType} = \x -> 
		{
			s = table {
				NC9_10 => table {Sg => "" + x; Pl => "di" + x};
				NC14_6 => table {Sg => "bo" + x; Pl => "ma" + x}
				};
			p_form = x;
			t = R;
		};

		mkOrdAdj : Str -> {s: NounClass => Number => Str; p_form: Str; t: TType} = \x -> 
		{
			s = table {
				NC9_10 => table {Sg => "" + x; Pl => "" + x};
				NC14_6 => table {Sg => "" + x; Pl => "" + x}
				};
			p_form = x;
			t = R;
		};

		mkVerbRel : Str -> {s: NounClass => Number => Str; p_form: Str; t: TType} = \x -> 
		{
			s = table {
				NC9_10 => table {Sg => x + "ng"; Pl => x + "ng"};
				NC14_6 => table {Sg => x + "ng"; Pl => x + "ng"}
				};
			p_form = x;
			t = V;
		};

		mkPerAdj : Str -> {s: NounClass => Number => Str; p_form: Str; t: TType} = \x -> 
		{
			s = table {
				NC9_10 => table {Sg => "" + x; Pl => "" + x};
				NC14_6 => table {Sg => "" + x; Pl => "" + x}
				};
			p_form = "mo" ++ x;
			t = P;
		};

		mkVeryAdj : {s: NounClass => Number => Str; p_form: Str; t: TType} -> {s: NounClass => Number => Str; p_form: Str; t: TType} = \x ->
		{
			s = table{c => table{n => (x.s!c!n) ++ "thata"}}; p_form = x.p_form ++ "thata"; t = x.t
		};

		mkVeryVerb : {s: NounClass => Number => Str; p_form: Str; t: TType} -> {s: NounClass => Number => Str; p_form: Str; t: TType} = \x ->
		{
			s = table{c => table{n => (x.s!c!n) ++ "thata"}}; p_form = x.p_form ++ "thata"; t = ModV
		};

		smartVery : {s: NounClass => Number => Str; p_form: Str; t: TType} -> {s: NounClass => Number => Str; p_form: Str; t: TType} = 
\x -> case x.t of --(x.s!c!n)
		{
			(V | ModV)	=>	mkVeryVerb x;
			--ModV	=>	mkVeryVerb x;
			_	=>	mkVeryAdj x
		};

		mkDemPron1 : NounClass => Number => Str = table
			{
				NC9_10 => table {Sg => "e"; Pl => "tse"};
				NC14_6 => table {Sg => "bo"; Pl => "a"}
			};
		
		mkDemPron2 : NounClass => Number => Str = table
			{
				NC9_10 => table {Sg => "eo"; Pl => "tseo"};
				NC14_6 => table {Sg => "boo"; Pl => "ao"}
			};

		smartQualRelPart : TType -> (NounClass => Number => Str) = \x -> case x of 
		{
			P	=> mkQualRelPart_PName;
			_	=> mkQualRelPart
		};

		mkQualRelPart : NounClass => Number => Str = table
			{
				NC9_10 => table {Sg => "e"; Pl => "tse"};
				NC14_6 => table {Sg => "bo"; Pl => "a"}
			};

		mkQualRelPart_PName : NounClass => Number => Str = table
			{
				NC9_10 => table {Sg => "ya"; Pl => "tsa"};
				NC14_6 => table {Sg => "ba"; Pl => "a"}
			};

		smartDescrCop : TType -> (NounClass => Number => Str) = \x -> case x of
		{
			P	=> mkDescrCop_PName;
			_	=> mkDescrCop
		};

		mkDescrCop : NounClass => Number => Str = table
			{
				NC9_10 => table {Sg => "e"; Pl => "di"};
				NC14_6 => table {Sg => "bo"; Pl => "a"}
			};

		mkDescrCop_PName : NounClass => Number => Str = table
			{
				NC9_10 => table {Sg => "ga"; Pl => "ga"};
				NC14_6 => table {Sg => "ga"; Pl => "ga"}
			};

		mkPredDescrCop : TType -> (NounClass => Number => Str) = \x -> case x of
			{
				V => table 	{NC9_10 => table {Sg => "e" ++ "a"; Pl => "di" ++ "a"};
						NC14_6 => table {Sg => "bo" ++ "a"; Pl => "a" ++ "a"}};

				_ => table	{NC9_10 => table {Sg => "e"; Pl => "di"};
						NC14_6 => table {Sg => "bo"; Pl => "a"}}
			};

}
