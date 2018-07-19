varname = InputString["Enter a variable name"];
varvalue = InputString["Enter a value"];
ReleaseHold[ Hold[Set["nameholder", "value"]] /. {"nameholder" -> Symbol[varname], "value" -> varvalue}];
Print[varname, " is now set to ", Symbol[varname]]
