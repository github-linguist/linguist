   cfrac=: +`% / NB. Evaluate a list as a continued fraction

   sqrt2=: cfrac 1 1,200$2 1x
   pi=:cfrac 3, , ,&6"0 *:<:+:>:i.100x
   e=: cfrac 2 1, , ,~"0 >:i.100x

   NB. translate from fraction to decimal string
   NB. translated from factor
   dec =: (-@:[ (}.,'.',{.) ":@:<.@:(* 10x&^)~)"0

   100 10 100 dec sqrt2, pi, e
1.4142135623730950488016887242096980785696718753769480731766797379907324784621205551109457595775322165
3.1415924109
2.7182818284590452353602874713526624977572470936999595749669676277240766303535475945713821785251664274
