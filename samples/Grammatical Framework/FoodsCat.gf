--# -path=.:present

-- (c) 2009 Jordi Saludes under LGPL

concrete FoodsCat of Foods = FoodsI with 
  (Syntax = SyntaxCat),
  (LexFoods = LexFoodsCat) ;
