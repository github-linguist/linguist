functor
import Application Open System
define
   fun {IsAlreadyRunning}
      try
	 S = {New Open.socket init}
      in
	 {S bind(takePort:12345)}
	 false
      catch system(os(os "bind" ...) ...) then
	 true
      end
   end

   if {IsAlreadyRunning} then
      {System.showInfo "Exiting because already running."}
      {Application.exit 1}
   end
   {System.showInfo "Press enter to exit."}
   {{New Open.file init(name:stdin)} read(list:_ size:1)}
   {Application.exit 0}
end
