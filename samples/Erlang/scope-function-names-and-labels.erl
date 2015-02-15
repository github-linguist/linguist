-module( a_module ).

-export( [exported_function/0] ).

exported_function() -> 1 + local_function().

local_function() -> 2.
