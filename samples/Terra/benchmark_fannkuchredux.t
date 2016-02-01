--[[
  The Computer Language Benchmarks Game
  http://shootout.alioth.debian.org/
 
  contributed by Ledrug Katz
 
 ]]


local C = {
    printf = terralib.externfunction("printf", terralib.types.funcpointer(rawstring,int,true)),
    exit = terralib.externfunction("exit", int -> {}),
    atoi = terralib.externfunction("atoi", rawstring -> int)
}

-- this depends highly on the platform.  It might be faster to use
-- char type on 32-bit systems; it might be faster to use unsigned.

elem = int

s = global(elem[16])
t = global(elem[16])

maxflips = global(int)
max_n = global(int)
odd = global(bool)
checksum = global(int)

terra flip()
    var i = max_n
    var x : &elem = t
    var y : &elem = s
    var c : elem
    
    while i > 0 do
        i = i - 1
        @x = @y
        x = x + 1
        y = y + 1
    end
    
    i = 1
    
    repeat
        x = t
        y = t + t[0]
        while x < y do
            c = @x
            @x = @y
            x = x + 1
            @y = c
            y = y - 1
        end
        i = i + 1
    until t[t[0]] == 0
    --C.printf("flip %d\n",i);
    return i
end


terra rotate(n : int)
    var c = s[0]
    for i = 0,n do
        s[i] = s[i+1] 
    end
    s[n] = c
    --C.printf("rotate(%d) %d\n",n,c);
end


terra tk(n : int)
    var i = 0
    var f : int
    var c : elem[16]
    
    for i = 0,16 do
        c[i] = 0
    end
    
    while i < n do
        rotate(i)
        if c[i] >= i then
            c[i] = 0
            i = i + 1
            goto continue
        end
        
        c[i] = c[i] + 1
        i = 1
        odd = not odd
        if s[0] ~= 0 then
            if s[s[0]] ~= 0 then
                f = flip()
            else
                f = 1
            end
            if f > maxflips then
                maxflips = f
            end
            --C.printf("f = %d\n",f)
            if odd then
                checksum = checksum - f
            else
                checksum = checksum + f
            end
        end
        ::continue::
    end
   
end


terra doit(N : int)
    maxflips = 0
    odd = false
    checksum = 0
    max_n = N
    if max_n < 3 or max_n > 15 then
      C.printf("range: must be 3 <= n <= 12\n")
      C.exit(1)
    end
    
    for i = 0,max_n do
        s[i] = i
    end
    tk(max_n)
    C.printf("%d\nPfannkuchen(%d) = %d\n", checksum, max_n, maxflips)
    return checksum
end

terra main(argc : int, v : &&int8)
    if argc < 2 then
      C.printf("usage: %s number\n", v[0])
      C.exit(1);
    end
    
    doit(C.atoi(v[1]))
    return 0
end

local test = require("test")

doit:compile()
print(test.time(function()
    test.eq(doit(10),73196)
end))

terralib.saveobj("benchmark_fannkuchredux", { main = main } )
