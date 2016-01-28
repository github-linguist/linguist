terra foo() 
    var c = 3 
    escape 
        local a = 1
        for i = 1,10 do
            emit quote c = c + a end 
        end
    end 
    return c 
end

assert(foo() == 13)

terra foo2()
    return escape emit(1) end
end

assert(1 == foo2())

a = terralib.newlist()
a:insert quote
    foo()
end