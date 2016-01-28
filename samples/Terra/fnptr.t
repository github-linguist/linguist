terra baz() return 1 end
bp = baz:getpointer()
terra foo()
    return bp()
end

assert(foo() == 1)