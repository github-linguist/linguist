functor
import
   System Application
prepare
   fun {Fac N}
      {FoldL {List.number 1 N 1} Number.'*' 1}
   end
   Fac10 = {Fac 10}
define
   {System.showInfo "10! = "#Fac10}
   {Application.exit 0}
end
