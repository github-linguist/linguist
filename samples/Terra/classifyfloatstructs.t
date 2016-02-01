-- reading and returning by reference produces far more compact assembly
-- in terra than reading and returning by value, even when a by-value function
-- is embedded in a by-reference wrapper. Also, optimization behavior varies
-- wildly. Here's a demonstration:
 
local float4 = tuple(float,float,float,float)
 
-- first, comparing assembly code size of stand-alone functions which all do 
-- the same operation, in different styles. This is after aggressive 
-- optimization by the LLVM backend
--------------------------------------------------------------------------------
 
-- read by value, return by value
-- 26 instructions, 120 bytes
local terra add4_val_val(a : float4, b : float4) : float4
    return {
        a._0+b._0,
        a._1+b._1,
        a._2+b._2,
        a._3+b._3
    }
end
 
-- read by value, return by reference
-- 24 instructions, 103 bytes
local terra add4_val_ref(c : &float4, a : float4, b: float4) : {}
    c._0 = a._0 + b._0
    c._1 = a._1 + b._1
    c._2 = a._2 + b._2
    c._3 = a._3 + b._3
end
 
-- read by reference, return by value
-- 14 instructions, 74 bytes
local terra add4_ref_val(a : &float4, b : &float4) : float4
    return {
        a._0+b._0,
        a._1+b._1,
        a._2+b._2,
        a._3+b._3
    }
end
 
-- read by reference, return by reference
-- 12 instructions, 57 bytes
local terra add4_ref_ref(c : &float4, a : &float4, b: &float4) : {}
    c._0 = a._0 + b._0
    c._1 = a._1 + b._1
    c._2 = a._2 + b._2
    c._3 = a._3 + b._3
end
 
-- read by reference, return by reference, BUT use temporary variables
-- 4 instructions, 12 bytes (!!!)
-- what happens here is that the tempvars are understood and treated
-- as a single SIMD register, and so only one addition is executed.
-- this is already reflected in the bytecode passed to LLVM, so I suppose 
-- terra does this optimization.
local terra add4_ref_ref_tempvar(c : &float4, a : &float4, b: &float4) : {}
    var x = a._0 + b._0
    var y = a._1 + b._1
    var z = a._2 + b._2
    var w = a._3 + b._3
    c._0 = x
    c._1 = y
    c._2 = z
    c._3 = w
end
 
-- turn on always-inline for later
add4_val_val:setinlined(true)
add4_val_ref:setinlined(true)
add4_ref_val:setinlined(true)
add4_ref_ref:setinlined(true)
 
-- uncomment to look at individual LLVM bytecode & disassembly
add4_val_val:disas()
-- add4_val_ref:disas()
-- add4_ref_val:disas()
-- add4_ref_ref:disas()
-- add4_ref_ref_tempvar:disas()
 

if terralib.lookupsymbol and require("ffi").os ~= "Windows" then
    terra sizecheck() 
     var si : terralib.SymbolInfo
     terralib.lookupsymbol(add4_val_val,&si)
     return si.size
    end
    assert(sizecheck() < 16)
end
--------------------------------------------------------------------------------
 
-- up to this point, one could argue that functions are always inlined into 
-- other functions, and that the instructions dissolve in the greater scheme
-- of things.
 
-- if that is true, let's attempt to convert one style into another and see
-- if the optimizations catch up.
 
-- read by value, return by value -> read by reference, return by reference
-- the clunky solution to the slim interface
-- 38 instructions (!), 193 bytes (!!), so it gets *worse*
local terra add4_val_val_to_ref_ref(c : &float4, a : &float4, b: &float4) : {}
    @c = add4_val_val(@a, @b)
end
 
-- read by reference, return by reference -> read by value, return by value
-- the slim solution to the clunky interface
-- 13 instructions, 61 bytes with tempvar -- wow, that's better than
-- the original function with the actual code in it!
local terra add4_ref_ref_to_val_val(a : float4, b : float4) : float4
    var c : float4
    add4_ref_ref_tempvar(&c, &a, &b)
    return c
end
 
-- so what happens if we do a conversion back to the by-reference interface?
-- 41 instructions, 194 bytes
local terra add4_ref_ref_to_val_val_to_ref_ref(c : &float4, a : &float4, b: &float4) : {}
    @c = add4_ref_ref_to_val_val(@a, @b)
end
 
-- and nest it once more, back to the by-value interface
-- 47 instructions, 208 bytes
-- so once we pass structs by-value, we'll never recover.
local terra add4_ref_ref_to_val_val_to_ref_ref_to_val_val(a : float4, b : float4) : float4
    var c : float4
    add4_ref_ref_to_val_val_to_ref_ref(&c, &a, &b)
    return c
end
 
-- uncomment to look at individual disassembly
-- add4_val_val_to_ref_ref:disas()
-- add4_ref_ref_to_val_val:disas()
-- add4_ref_ref_to_val_val_to_ref_ref:disas()
-- add4_ref_ref_to_val_val_to_ref_ref_to_val_val:disas()