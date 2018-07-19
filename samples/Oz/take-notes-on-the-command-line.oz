functor
import
   Application
   Open
   OS
   System
define
   fun {TimeStamp}
      N = {OS.localTime}
   in
      (1900+N.year)#"-"#(1+N.mon)#"-"#N.mDay#", "#N.hour#":"#N.min#":"#N.sec
   end

   fun {Join X|Xr Sep}
      {FoldL Xr fun {$ Z X} Z#Sep#X end X}
   end

   case {Application.getArgs plain}
   of nil then
      try
         F = {New Open.file init(name:"notes.txt")}
      in
         {System.printInfo {F read(list:$ size:all)}}
         {F close}
      catch _ then skip end
   [] Args then
      F = {New Open.file init(name:"notes.txt" flags:[write text create append])}
   in
      {F write(vs:{TimeStamp}#"\n")}
      {F write(vs:"\t"#{Join Args " "}#"\n")}
      {F close}
   end
   {Application.exit 0}
end
