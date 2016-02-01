C = terralib.includecstring [[
    #include <stdio.h>
]]
C.fopen("broken.t","r")
