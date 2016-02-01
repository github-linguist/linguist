
terra foo()
end

foo:compile()
terralib.dumpmodule()

terralib.includecstring [[
    int foo() {
        return 4;
    }
]]