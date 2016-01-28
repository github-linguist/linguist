local C = terralib.includecstring [[
    typedef union {
            float v[2];
            struct {
                float x;
                float y;
            };
    } float2;
    static float2 a;
    void doit() {
        a.x = 4;
        a.y = 5;
    }
    float2* what() { return &a; }
]]

C.float2:printpretty()

local anonstructgetter = macro(function(name,self)
    for i,e in pairs(self:gettype():getfields()) do
        if e.key:match("_%d+") and e.type:getfield(name) then
            return `self.[e.key].[name]
        end
    end 
    error("no field "..name.." in struct of type "..tostring(T))
end)

C.float2.metamethods.__entrymissing = anonstructgetter

terra foo(pa : &C.float2)
    var a = @C.what()
    return a.v[0],a.v[1],a.x,a.y
end

C.doit()

local a = foo(C.what())
assert(4 == a._0)
assert(5 == a._1)
assert(4 == a._2)
assert(5 == a._3)