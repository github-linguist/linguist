declare X in

thread
   if {IsFree X} then {System.showInfo "X is unbound."} end
   {Wait X}
   {System.showInfo "Now X is determined."}
end

{System.showInfo "Sleeping..."}
{Delay 1000}
{System.showInfo "Setting X."}
X = 42
