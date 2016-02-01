import "lib.exampleparser"

P exp -1 + 1 * (2 - 3)
P exp not true or false and 3
P exp a.b.c[d]()(a,b,c)
P exp (a + 3)(4)
P exp {a,3,4, d = 4, [1+3] = 3}
P exp [ 3 + 4 ]
P statement var a,b,c:int = 3
P statement do
    if a then b else c end
    if a then b end
    if a then b elseif c then d end
    while 3 + 3 do end
    repeat
    until a + b
    for a,b,c:int = 1,3 do end
    for a,b,c:int in 1,3 do end
    a = b
    a
    a,c = d,e
end