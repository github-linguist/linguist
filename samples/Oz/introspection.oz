declare
  Version = {Property.get 'oz.version'}
  %% Version is an atom like '1.4.0'. So we can not compare it directly.
  %% Extract the version components:
  [Major Minor Release] = {Map {String.tokens {Atom.toString Version} &.} String.toInt}
in
  if Major >= 1 andthen  Minor >= 4 then
     %% check whether module Number exports a value 'abs':
     if {HasFeature Number abs} then
        {Show {Number.abs ~42}}
     end
  else
     {System.showInfo "Your Mozart version is too old."}
  end
