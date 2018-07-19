# As is, will not work if val is a String
Assign := function(var, val)
	Read(InputTextString(Concatenation(var, " := ", String(val), ";")));
end;
