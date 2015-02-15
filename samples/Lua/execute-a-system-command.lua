-- just executing the command
os.execute("ls")

-- to execute and capture the output, use io.popen
local f = io.popen("ls") -- store the output in a "file"
print( f:read("*a") )    -- print out the "file"'s content
