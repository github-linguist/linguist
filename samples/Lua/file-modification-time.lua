require "lfs"
local attributes = lfs.attributes("input.txt")
if attributes then
    print(path .. " was last modified " .. os.date("%c", attributes.modification) .. ".")

    -- set access and modification time to now ...
    lfs.touch("input.txt")

    -- ... or set modification time to now, keep original access time
    lfs.touch("input.txt", attributes.access, os.time())
else
    print(path .. " does not exist.")
end
