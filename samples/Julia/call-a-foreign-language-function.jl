julia> p = ccall(:strdup, Ptr{Uint8}, (Ptr{Uint8},), "Hello world")
Ptr{Uint8} @0x000000011731bde0

julia> bytestring(p) # convert pointer back to a native Julia string
"Hello world"

julia> ccall(:free, Void, (Ptr{Uint8},), p)
