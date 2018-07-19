require "lfs"
directorypath = "." -- current working directory
for filename in lfs.dir(directorypath) do
    if filename:match("%.lua$") then -- "%." is an escaped ".", "$" is end of string
        print(filename)
    end
end
