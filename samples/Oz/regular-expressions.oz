declare
  [Regex] = {Module.link ['x-oz://contrib/regex']}
  String = "This is a string"
in
  if {Regex.search "string$" String} \= false then
     {System.showInfo "Ends with string."}
  end
  {System.showInfo {Regex.replace String " a " fun {$ _ _} " another " end}}
