local f = assert(io.popen("uname", 'r'))
local s = assert(f:read('*a'))
f:close()

if s~="Darwin\n" then
  print("Warning, not running test b/c this isn't a mac")
  return
end

C = terralib.includecstring [[
    #include <stdio.h>
    #include <stdlib.h>
]]
local arraytypes = {}
function Array(T)
    if arraytypes[T] then return arraytypes[T] end
    local struct ArrayImpl {
        data : &T;
        N : int;
    }
    arraytypes[T] = ArrayImpl
    terra ArrayImpl:init(N : int)
        self.data = [&T](C.malloc(N*sizeof(T)))
        self.N = N
    end
    terra ArrayImpl:free()
        C.free(self.data)
    end
    ArrayImpl.metamethods.__apply = macro(function(self,idx)
        return `self.data[idx]
    end)
    ArrayImpl.metamethods.__methodmissing = macro(function(methodname,selfexp,...)
        local args = terralib.newlist {...}
        local params = args:map(function(a) return symbol(a:gettype()) end)
        local terra elemfn(a : &T, [params])
            return a:[methodname](params)
        end
        local RT = elemfn:gettype().returntype
        return quote
            var self = selfexp
            var r : Array(RT)
            r:init(self.N)
            for i = 0,r.N do
                r.data[i] = elemfn(&self.data[i],args)
            end
        in
            r
        end
    end)
    return ArrayImpl
end

local OC = require("lib/objc")
local IO = terralib.includec("stdio.h")

struct Rect {
    a : double,
    b : double,
    c : double,
    d : double
}

terra str(data : &uint8)
    return OC.NSString:stringWithUTF8String(data)
end

terra main()
    OC.NSAutoreleasePool:new()
    var app = OC.NSApplication:sharedApplication()
    var rec = Rect {0,0,200,200}
    var windows : Array(OC.ID)
    windows:init(2)
    windows(0) = OC.NSWindow
    windows(1) = OC.NSWindow
    windows = windows:alloc():initWithContentRect_styleMask_backing_defer(rec,1,2,false)
    windows:makeKeyAndOrderFront(nil)
    IO.printf("entering run loop\n")
    app:run()
end

terralib.linklibrary("/System/Library/Frameworks/Cocoa.framework/Cocoa")
main:compile()
