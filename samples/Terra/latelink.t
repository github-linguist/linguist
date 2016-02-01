C = terralib.includecstring [[
    typedef struct {
        int a;
    } S;
    typedef struct {
        S a;
    } A;
]]
C2 = terralib.includecstring [[
    typedef struct {
        int a;
    } S;
    typedef struct {
        S a;
    } B;
]]

terra second()
    var a : C2.B
    var b : C.A
    
    a.a = b.a
end
second:disas()