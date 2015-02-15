functor
import
   LazyList
   Application
   System
define

   fun {Pyth N}
      <<list [X Y Z] with
	 X <- {List.number 1 N 1}
	 Y <- {List.number X N 1}
	 Z <- {List.number Y N 1}
	 where X*X + Y*Y == Z*Z
      >>
   end

   {ForAll {Pyth 20} System.show}

   {Application.exit 0}
end
