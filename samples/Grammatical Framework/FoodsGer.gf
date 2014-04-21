--# -path=.:present

-- (c) 2009 Aarne Ranta under LGPL

concrete FoodsGer of Foods = FoodsI with 
  (Syntax = SyntaxGer),
  (LexFoods = LexFoodsGer) ;
