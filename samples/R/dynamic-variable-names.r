# Read the name in from a command prompt
varname <- readline("Please name your variable >")
# Make sure the name is valid for a variable
varname <- make.names(varname)
message(paste("The variable being assigned is '", varname, "'"))
# Assign the variable (with value 42) into the user workspace (global environment)
assign(varname, 42)
#Check that the value has been assigned ok
ls(pattern=varname)
get(varname)
