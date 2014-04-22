--# -path=.:present

-- (c) 2009 Aarne Ranta under LGPL

concrete FoodsSwe of Foods = FoodsI with 
  (Syntax = SyntaxSwe),
  (LexFoods = LexFoodsSwe) ** {flags language = sv_SE;} ;
