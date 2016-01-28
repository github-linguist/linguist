C = terralib.includec("stdio.h")
terra main()
    C.printf("hello world\n")
end
local m = { main = main }
terralib.saveobj("output.o",m)
local a = terralib.saveobj(nil,"object",m)
terralib.saveobj("output2.bc",m)
local b = terralib.saveobj(nil,"bitcode",m)
terralib.saveobj("output.ll",m)
local c = terralib.saveobj(nil,"llvmir",m)
terralib.saveobj("output",m)
terralib.saveobj("output2","executable",m)

assert(a:match("hello world"))
assert(c:match("hello world"))