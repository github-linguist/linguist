

C = terralib.includecstring [[
    typedef struct { int x; int y; } Point;
    Point mkpoint() { Point p; p.x = p.y = 3; return p; }
]]

assert(C.mkpoint().x == 3)