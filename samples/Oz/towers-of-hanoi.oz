declare
  proc {TowersOfHanoi N From To Via}
     if N > 0 then
        {TowersOfHanoi N-1 From Via To}
        {System.showInfo "Move from "#From#" to "#To}
        {TowersOfHanoi N-1 Via To From}
     end
  end
in
  {TowersOfHanoi 4 left middle right}
