inFile  = io.open("input.txt", "r")
data = inFile:read("*all") -- may be abbreviated to "*a";
                           -- other options are "*line",
                           -- or the number of characters to read.
inFile:close()

outFile = io.open("output.txt", "w")
outfile:write(data)
outfile:close()

-- Oneliner version:
io.open("output.txt", "w"):write(io.open("input.txt", "r"):read("*a"))
