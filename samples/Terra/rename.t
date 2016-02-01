terralib.includecstring [[
    int foo() { return 3; }
]]

terra what() return 4 end


terralib.saveobj("foo.o",{foo = what})